
	#' List BuildingAccountDistributions
	#'
	#' This function returns a dataframe or json object of BuildingAccountDistributions
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given BuildingAccountDistributions. Defaults to FALSE for all return fields which, for convenience, returns all fields for the BuildingAccountDistributions.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object listed as text. Run \code{\link{getSchemaForObjects}}('BuildingAccountDistribution') to get more field paths.
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
	#' @concept Substitute Tracking
	#' @return A list of BuildingAccountDistributions
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	listBuildingAccountDistributions <- function(searchConditionsList = NULL, BuildingAccountDistributionID = F, BuildingID = F, AccountID = F, DistributionPercent = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, fieldPaths = NULL, searchConditionsGroupType = "And", searchSortFieldNamesList = NULL, searchSortFieldNamesDescendingList = NULL, entityId = 1, schoolYearId = NULL, page = 1, pageSize = 100000, flatten = T, returnResponse = F){

		params <- as.list(environment())

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		listSkyObjects(module = "SubstituteTracking", objectName = "BuildingAccountDistribution", searchFields = searchFields %>% append(fieldPaths), page = page, pageSize = pageSize, SearchConditionsList = searchConditionsList, SearchConditionsGroupType = searchConditionsGroupType, SearchSortFieldNamesList = searchSortFieldNamesList, SearchSortFieldNamesDescendingList = searchSortFieldNamesDescendingList, entityId = entityId, schoolYearId = schoolYearId, flatten = flatten, returnResponse = returnResponse)
	}

	#' Get a BuildingAccountDistribution
	#'
	#' This function returns a dataframe or json object of a BuildingAccountDistribution
	#' @param BuildingAccountDistributionID The ID of the BuildingAccountDistribution to return.
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given BuildingAccountDistribution. Defaults to FALSE for all return fields which, for convenience, returns all fields for the BuildingAccountDistribution.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object listed as text. Run \code{\link{getSchemaForObjects}}('BuildingAccountDistribution') to get more field paths.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{listSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Substitute Tracking
	#' @return A dataframe or of BuildingAccountDistribution
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	getBuildingAccountDistribution <- function(BuildingAccountDistributionID, BuildingID = F, AccountID = F, DistributionPercent = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, fieldPaths = NULL, entityId = 1, schoolYearId = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment()) %>% purrr::keep(names(.) != "BuildingAccountDistributionID")

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		getSkyObject(module = "SubstituteTracking", objectName = "BuildingAccountDistribution", objectId = BuildingAccountDistributionID, searchFields = searchFields, entityId = entityId, flatten = flatten, returnResponse = returnResponse)
	}

	#' Delete a BuildingAccountDistribution
	#'
	#' This function deletes a BuildingAccountDistribution
	#' @param BuildingAccountDistributionID The ID of the BuildingAccountDistribution to delete
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{listSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Substitute Tracking
	#' @return The BuildingAccountDistributionID of the deleted BuildingAccountDistribution.
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	deleteBuildingAccountDistribution <- function(BuildingAccountDistributionID, ignoreWarnings = F, entityId = 1, schoolYearId = NULL, flatten = T, returnResponse = F){

		deleteSkyObject(module = "SubstituteTracking", objectName = "BuildingAccountDistribution", objectId = BuildingAccountDistributionID, ignoreWarnings = ignoreWarnings, entityId = entityId, flatten = flatten, returnResponse = returnResponse)
	}

	#' Create a BuildingAccountDistribution
	#'
	#' This function creates a BuildingAccountDistribution
	#' @param fieldNames The field values to give the created BuildingAccountDistribution. Each defaults to NULL.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{listSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Substitute Tracking
	#' @return A newly created BuildingAccountDistribution
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	createBuildingAccountDistribution <- function(BuildingID = NULL, AccountID = NULL, DistributionPercent = NULL, entityId = 1, schoolYearId = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment())

		body <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper()) %>% purrr::compact()

		createSkyObject(module = "SubstituteTracking", objectName = "BuildingAccountDistribution", body = list(DataObject = body), searchFields = append("BuildingAccountDistributionID", body %>% names()), entityId = entityId, schoolYearId = schoolYearId, flatten = flatten, returnResponse = returnResponse)
	}

	#' Modify a BuildingAccountDistribution
	#'
	#' This function modifies a BuildingAccountDistribution
	#' @param fieldNames The field values to give the modified BuildingAccountDistribution. Each defaults to NULL.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{listSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Substitute Tracking
	#' @return The modified BuildingAccountDistribution
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	modifyBuildingAccountDistribution <- function(BuildingAccountDistributionID, BuildingID = NULL, AccountID = NULL, DistributionPercent = NULL, entityId = 1, schoolYearId = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment())

		body <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper()) %>% purrr::compact()

		modifySkyObject(module = "SubstituteTracking", objectName = "BuildingAccountDistribution", objectId = BuildingAccountDistributionID, body = list(DataObject = body), searchFields = append("BuildingAccountDistributionID", body %>% names()), entityId = entityId, schoolYearId = schoolYearId, flatten = flatten, returnResponse = returnResponse)
	}

	#' List SubstituteTrackingConfigDistricts
	#'
	#' This function returns a dataframe or json object of SubstituteTrackingConfigDistricts
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given SubstituteTrackingConfigDistricts. Defaults to FALSE for all return fields which, for convenience, returns all fields for the SubstituteTrackingConfigDistricts.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object listed as text. Run \code{\link{getSchemaForObjects}}('SubstituteTrackingConfigDistrict') to get more field paths.
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
	#' @concept Substitute Tracking
	#' @return A list of SubstituteTrackingConfigDistricts
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	listSubstituteTrackingConfigDistricts <- function(searchConditionsList = NULL, ConfigDistrictID = F, DistrictID = F, PriorHistoryEndDate = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, ThirdPartyFormatID = F, fieldPaths = NULL, searchConditionsGroupType = "And", searchSortFieldNamesList = NULL, searchSortFieldNamesDescendingList = NULL, entityId = 1, schoolYearId = NULL, page = 1, pageSize = 100000, flatten = T, returnResponse = F){

		params <- as.list(environment())

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		listSkyObjects(module = "SubstituteTracking", objectName = "ConfigDistrict", searchFields = searchFields %>% append(fieldPaths), page = page, pageSize = pageSize, SearchConditionsList = searchConditionsList, SearchConditionsGroupType = searchConditionsGroupType, SearchSortFieldNamesList = searchSortFieldNamesList, SearchSortFieldNamesDescendingList = searchSortFieldNamesDescendingList, entityId = entityId, schoolYearId = schoolYearId, flatten = flatten, returnResponse = returnResponse)
	}

	#' Get a SubstituteTrackingConfigDistrict
	#'
	#' This function returns a dataframe or json object of a SubstituteTrackingConfigDistrict
	#' @param SubstituteTrackingConfigDistrictID The ID of the SubstituteTrackingConfigDistrict to return.
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given SubstituteTrackingConfigDistrict. Defaults to FALSE for all return fields which, for convenience, returns all fields for the SubstituteTrackingConfigDistrict.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object listed as text. Run \code{\link{getSchemaForObjects}}('SubstituteTrackingConfigDistrict') to get more field paths.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{listSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Substitute Tracking
	#' @return A dataframe or of SubstituteTrackingConfigDistrict
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	getSubstituteTrackingConfigDistrict <- function(SubstituteTrackingConfigDistrictID, ConfigDistrictID = F, DistrictID = F, PriorHistoryEndDate = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, ThirdPartyFormatID = F, fieldPaths = NULL, entityId = 1, schoolYearId = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment()) %>% purrr::keep(names(.) != "SubstituteTrackingConfigDistrictID")

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		getSkyObject(module = "SubstituteTracking", objectName = "ConfigDistrict", objectId = SubstituteTrackingConfigDistrictID, searchFields = searchFields, entityId = entityId, flatten = flatten, returnResponse = returnResponse)
	}

	#' Delete a SubstituteTrackingConfigDistrict
	#'
	#' This function deletes a SubstituteTrackingConfigDistrict
	#' @param SubstituteTrackingConfigDistrictID The ID of the SubstituteTrackingConfigDistrict to delete
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{listSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Substitute Tracking
	#' @return The SubstituteTrackingConfigDistrictID of the deleted SubstituteTrackingConfigDistrict.
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	deleteSubstituteTrackingConfigDistrict <- function(SubstituteTrackingConfigDistrictID, ignoreWarnings = F, entityId = 1, schoolYearId = NULL, flatten = T, returnResponse = F){

		deleteSkyObject(module = "SubstituteTracking", objectName = "ConfigDistrict", objectId = SubstituteTrackingConfigDistrictID, ignoreWarnings = ignoreWarnings, entityId = entityId, flatten = flatten, returnResponse = returnResponse)
	}

	#' Create a SubstituteTrackingConfigDistrict
	#'
	#' This function creates a SubstituteTrackingConfigDistrict
	#' @param fieldNames The field values to give the created SubstituteTrackingConfigDistrict. Each defaults to NULL.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{listSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Substitute Tracking
	#' @return A newly created SubstituteTrackingConfigDistrict
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	createSubstituteTrackingConfigDistrict <- function(DistrictID = NULL, PriorHistoryEndDate = NULL, ThirdPartyFormatID = NULL, entityId = 1, schoolYearId = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment())

		body <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper()) %>% purrr::compact()

		createSkyObject(module = "SubstituteTracking", objectName = "ConfigDistrict", body = list(DataObject = body), searchFields = append("ConfigDistrictID", body %>% names()), entityId = entityId, schoolYearId = schoolYearId, flatten = flatten, returnResponse = returnResponse)
	}

	#' Modify a SubstituteTrackingConfigDistrict
	#'
	#' This function modifies a SubstituteTrackingConfigDistrict
	#' @param fieldNames The field values to give the modified SubstituteTrackingConfigDistrict. Each defaults to NULL.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{listSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Substitute Tracking
	#' @return The modified SubstituteTrackingConfigDistrict
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	modifySubstituteTrackingConfigDistrict <- function(ConfigDistrictID, DistrictID = NULL, PriorHistoryEndDate = NULL, ThirdPartyFormatID = NULL, entityId = 1, schoolYearId = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment())

		body <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper()) %>% purrr::compact()

		modifySkyObject(module = "SubstituteTracking", objectName = "ConfigDistrict", objectId = ConfigDistrictID, body = list(DataObject = body), searchFields = append("ConfigDistrictID", body %>% names()), entityId = entityId, schoolYearId = schoolYearId, flatten = flatten, returnResponse = returnResponse)
	}

	#' List SubstituteTrackingCSVFileFormats
	#'
	#' This function returns a dataframe or json object of SubstituteTrackingCSVFileFormats
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given SubstituteTrackingCSVFileFormats. Defaults to FALSE for all return fields which, for convenience, returns all fields for the SubstituteTrackingCSVFileFormats.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object listed as text. Run \code{\link{getSchemaForObjects}}('SubstituteTrackingCSVFileFormat') to get more field paths.
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
	#' @concept Substitute Tracking
	#' @return A list of SubstituteTrackingCSVFileFormats
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	listSubstituteTrackingCSVFileFormats <- function(searchConditionsList = NULL, CSVFileFormatID = F, SkywardID = F, ThirdPartyFormatID = F, NumberOfHeaderRows = F, SubstituteDurationColumnNumber = F, AbsentDurationColumnNumber = F, SubstituteEmployeeColumnNumber = F, AbsentEmployeeColumnNumber = F, TimeOffTypeColumnNumber = F, BuildingColumnNumber = F, DateColumnNumber = F, AccountColumnNumber = F, DelimiterType = F, OtherDelimiter = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, SkywardHash = F, TimeOffDescriptionColumnNumber = F, ExceptionHandlingType = F, ExceptionExportType = F, fieldPaths = NULL, searchConditionsGroupType = "And", searchSortFieldNamesList = NULL, searchSortFieldNamesDescendingList = NULL, entityId = 1, schoolYearId = NULL, page = 1, pageSize = 100000, flatten = T, returnResponse = F){

		params <- as.list(environment())

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		listSkyObjects(module = "SubstituteTracking", objectName = "CSVFileFormat", searchFields = searchFields %>% append(fieldPaths), page = page, pageSize = pageSize, SearchConditionsList = searchConditionsList, SearchConditionsGroupType = searchConditionsGroupType, SearchSortFieldNamesList = searchSortFieldNamesList, SearchSortFieldNamesDescendingList = searchSortFieldNamesDescendingList, entityId = entityId, schoolYearId = schoolYearId, flatten = flatten, returnResponse = returnResponse)
	}

	#' Get a SubstituteTrackingCSVFileFormat
	#'
	#' This function returns a dataframe or json object of a SubstituteTrackingCSVFileFormat
	#' @param SubstituteTrackingCSVFileFormatID The ID of the SubstituteTrackingCSVFileFormat to return.
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given SubstituteTrackingCSVFileFormat. Defaults to FALSE for all return fields which, for convenience, returns all fields for the SubstituteTrackingCSVFileFormat.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object listed as text. Run \code{\link{getSchemaForObjects}}('SubstituteTrackingCSVFileFormat') to get more field paths.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{listSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Substitute Tracking
	#' @return A dataframe or of SubstituteTrackingCSVFileFormat
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	getSubstituteTrackingCSVFileFormat <- function(SubstituteTrackingCSVFileFormatID, CSVFileFormatID = F, SkywardID = F, ThirdPartyFormatID = F, NumberOfHeaderRows = F, SubstituteDurationColumnNumber = F, AbsentDurationColumnNumber = F, SubstituteEmployeeColumnNumber = F, AbsentEmployeeColumnNumber = F, TimeOffTypeColumnNumber = F, BuildingColumnNumber = F, DateColumnNumber = F, AccountColumnNumber = F, DelimiterType = F, OtherDelimiter = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, SkywardHash = F, TimeOffDescriptionColumnNumber = F, ExceptionHandlingType = F, ExceptionExportType = F, fieldPaths = NULL, entityId = 1, schoolYearId = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment()) %>% purrr::keep(names(.) != "SubstituteTrackingCSVFileFormatID")

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		getSkyObject(module = "SubstituteTracking", objectName = "CSVFileFormat", objectId = SubstituteTrackingCSVFileFormatID, searchFields = searchFields, entityId = entityId, flatten = flatten, returnResponse = returnResponse)
	}

	#' Delete a SubstituteTrackingCSVFileFormat
	#'
	#' This function deletes a SubstituteTrackingCSVFileFormat
	#' @param SubstituteTrackingCSVFileFormatID The ID of the SubstituteTrackingCSVFileFormat to delete
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{listSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Substitute Tracking
	#' @return The SubstituteTrackingCSVFileFormatID of the deleted SubstituteTrackingCSVFileFormat.
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	deleteSubstituteTrackingCSVFileFormat <- function(SubstituteTrackingCSVFileFormatID, ignoreWarnings = F, entityId = 1, schoolYearId = NULL, flatten = T, returnResponse = F){

		deleteSkyObject(module = "SubstituteTracking", objectName = "CSVFileFormat", objectId = SubstituteTrackingCSVFileFormatID, ignoreWarnings = ignoreWarnings, entityId = entityId, flatten = flatten, returnResponse = returnResponse)
	}

	#' Create a SubstituteTrackingCSVFileFormat
	#'
	#' This function creates a SubstituteTrackingCSVFileFormat
	#' @param fieldNames The field values to give the created SubstituteTrackingCSVFileFormat. Each defaults to NULL.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{listSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Substitute Tracking
	#' @return A newly created SubstituteTrackingCSVFileFormat
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	createSubstituteTrackingCSVFileFormat <- function(ThirdPartyFormatID = NULL, NumberOfHeaderRows = NULL, SubstituteDurationColumnNumber = NULL, AbsentDurationColumnNumber = NULL, SubstituteEmployeeColumnNumber = NULL, AbsentEmployeeColumnNumber = NULL, TimeOffTypeColumnNumber = NULL, BuildingColumnNumber = NULL, DateColumnNumber = NULL, AccountColumnNumber = NULL, DelimiterType = NULL, OtherDelimiter = NULL, TimeOffDescriptionColumnNumber = NULL, ExceptionHandlingType = NULL, ExceptionExportType = NULL, entityId = 1, schoolYearId = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment())

		body <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper()) %>% purrr::compact()

		createSkyObject(module = "SubstituteTracking", objectName = "CSVFileFormat", body = list(DataObject = body), searchFields = append("CSVFileFormatID", body %>% names()), entityId = entityId, schoolYearId = schoolYearId, flatten = flatten, returnResponse = returnResponse)
	}

	#' Modify a SubstituteTrackingCSVFileFormat
	#'
	#' This function modifies a SubstituteTrackingCSVFileFormat
	#' @param fieldNames The field values to give the modified SubstituteTrackingCSVFileFormat. Each defaults to NULL.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{listSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Substitute Tracking
	#' @return The modified SubstituteTrackingCSVFileFormat
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	modifySubstituteTrackingCSVFileFormat <- function(CSVFileFormatID, ThirdPartyFormatID = NULL, NumberOfHeaderRows = NULL, SubstituteDurationColumnNumber = NULL, AbsentDurationColumnNumber = NULL, SubstituteEmployeeColumnNumber = NULL, AbsentEmployeeColumnNumber = NULL, TimeOffTypeColumnNumber = NULL, BuildingColumnNumber = NULL, DateColumnNumber = NULL, AccountColumnNumber = NULL, DelimiterType = NULL, OtherDelimiter = NULL, TimeOffDescriptionColumnNumber = NULL, ExceptionHandlingType = NULL, ExceptionExportType = NULL, entityId = 1, schoolYearId = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment())

		body <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper()) %>% purrr::compact()

		modifySkyObject(module = "SubstituteTracking", objectName = "CSVFileFormat", objectId = CSVFileFormatID, body = list(DataObject = body), searchFields = append("CSVFileFormatID", body %>% names()), entityId = entityId, schoolYearId = schoolYearId, flatten = flatten, returnResponse = returnResponse)
	}

	#' List AesopWebFormats
	#'
	#' This function returns a dataframe or json object of AesopWebFormats
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given AesopWebFormats. Defaults to FALSE for all return fields which, for convenience, returns all fields for the AesopWebFormats.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object listed as text. Run \code{\link{getSchemaForObjects}}('AesopWebFormat') to get more field paths.
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
	#' @concept Substitute Tracking
	#' @return A list of AesopWebFormats
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	listAesopWebFormats <- function(searchConditionsList = NULL, AesopWebFormatID = F, SkywardID = F, ThirdPartyFormatID = F, PostURL = F, WebServiceKey = F, TemplateIdentifier = F, RootTag = F, RecordTag = F, UniqueRecordIdentifier = F, SubstituteDurationIdentifier = F, AbsentDurationIdentifier = F, SubstituteEmployeeNumberIdentifier = F, AbsentEmployeeNumberIdentifier = F, TimeOffTypeIdentifier = F, BuildingIdentifier = F, DateIdentifier = F, AccountIdentifier = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, SkywardHash = F, TimeOffDescriptionIdentifier = F, fieldPaths = NULL, searchConditionsGroupType = "And", searchSortFieldNamesList = NULL, searchSortFieldNamesDescendingList = NULL, entityId = 1, schoolYearId = NULL, page = 1, pageSize = 100000, flatten = T, returnResponse = F){

		params <- as.list(environment())

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		listSkyObjects(module = "SubstituteTracking", objectName = "AesopWebFormat", searchFields = searchFields %>% append(fieldPaths), page = page, pageSize = pageSize, SearchConditionsList = searchConditionsList, SearchConditionsGroupType = searchConditionsGroupType, SearchSortFieldNamesList = searchSortFieldNamesList, SearchSortFieldNamesDescendingList = searchSortFieldNamesDescendingList, entityId = entityId, schoolYearId = schoolYearId, flatten = flatten, returnResponse = returnResponse)
	}

	#' Get an AesopWebFormat
	#'
	#' This function returns a dataframe or json object of an AesopWebFormat
	#' @param AesopWebFormatID The ID of the AesopWebFormat to return.
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given AesopWebFormat. Defaults to FALSE for all return fields which, for convenience, returns all fields for the AesopWebFormat.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object listed as text. Run \code{\link{getSchemaForObjects}}('AesopWebFormat') to get more field paths.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{listSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Substitute Tracking
	#' @return A dataframe or of AesopWebFormat
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	getAesopWebFormat <- function(AesopWebFormatID, SkywardID = F, ThirdPartyFormatID = F, PostURL = F, WebServiceKey = F, TemplateIdentifier = F, RootTag = F, RecordTag = F, UniqueRecordIdentifier = F, SubstituteDurationIdentifier = F, AbsentDurationIdentifier = F, SubstituteEmployeeNumberIdentifier = F, AbsentEmployeeNumberIdentifier = F, TimeOffTypeIdentifier = F, BuildingIdentifier = F, DateIdentifier = F, AccountIdentifier = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, SkywardHash = F, TimeOffDescriptionIdentifier = F, fieldPaths = NULL, entityId = 1, schoolYearId = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment()) %>% purrr::keep(names(.) != "AesopWebFormatID")

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		getSkyObject(module = "SubstituteTracking", objectName = "AesopWebFormat", objectId = AesopWebFormatID, searchFields = searchFields, entityId = entityId, flatten = flatten, returnResponse = returnResponse)
	}

	#' Delete an AesopWebFormat
	#'
	#' This function deletes an AesopWebFormat
	#' @param AesopWebFormatID The ID of the AesopWebFormat to delete
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{listSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Substitute Tracking
	#' @return The AesopWebFormatID of the deleted AesopWebFormat.
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	deleteAesopWebFormat <- function(AesopWebFormatID, ignoreWarnings = F, entityId = 1, schoolYearId = NULL, flatten = T, returnResponse = F){

		deleteSkyObject(module = "SubstituteTracking", objectName = "AesopWebFormat", objectId = AesopWebFormatID, ignoreWarnings = ignoreWarnings, entityId = entityId, flatten = flatten, returnResponse = returnResponse)
	}

	#' Create an AesopWebFormat
	#'
	#' This function creates an AesopWebFormat
	#' @param fieldNames The field values to give the created AesopWebFormat. Each defaults to NULL.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{listSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Substitute Tracking
	#' @return A newly created AesopWebFormat
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	createAesopWebFormat <- function(ThirdPartyFormatID = NULL, PostURL = NULL, WebServiceKey = NULL, TemplateIdentifier = NULL, RootTag = NULL, RecordTag = NULL, UniqueRecordIdentifier = NULL, SubstituteDurationIdentifier = NULL, AbsentDurationIdentifier = NULL, SubstituteEmployeeNumberIdentifier = NULL, AbsentEmployeeNumberIdentifier = NULL, TimeOffTypeIdentifier = NULL, BuildingIdentifier = NULL, DateIdentifier = NULL, AccountIdentifier = NULL, TimeOffDescriptionIdentifier = NULL, entityId = 1, schoolYearId = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment())

		body <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper()) %>% purrr::compact()

		createSkyObject(module = "SubstituteTracking", objectName = "AesopWebFormat", body = list(DataObject = body), searchFields = append("AesopWebFormatID", body %>% names()), entityId = entityId, schoolYearId = schoolYearId, flatten = flatten, returnResponse = returnResponse)
	}

	#' Modify an AesopWebFormat
	#'
	#' This function modifies an AesopWebFormat
	#' @param fieldNames The field values to give the modified AesopWebFormat. Each defaults to NULL.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{listSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Substitute Tracking
	#' @return The modified AesopWebFormat
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	modifyAesopWebFormat <- function(AesopWebFormatID, ThirdPartyFormatID = NULL, PostURL = NULL, WebServiceKey = NULL, TemplateIdentifier = NULL, RootTag = NULL, RecordTag = NULL, UniqueRecordIdentifier = NULL, SubstituteDurationIdentifier = NULL, AbsentDurationIdentifier = NULL, SubstituteEmployeeNumberIdentifier = NULL, AbsentEmployeeNumberIdentifier = NULL, TimeOffTypeIdentifier = NULL, BuildingIdentifier = NULL, DateIdentifier = NULL, AccountIdentifier = NULL, TimeOffDescriptionIdentifier = NULL, entityId = 1, schoolYearId = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment())

		body <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper()) %>% purrr::compact()

		modifySkyObject(module = "SubstituteTracking", objectName = "AesopWebFormat", objectId = AesopWebFormatID, body = list(DataObject = body), searchFields = append("AesopWebFormatID", body %>% names()), entityId = entityId, schoolYearId = schoolYearId, flatten = flatten, returnResponse = returnResponse)
	}

	#' List SubstituteTransactionTimeOffTransactions
	#'
	#' This function returns a dataframe or json object of SubstituteTransactionTimeOffTransactions
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given SubstituteTransactionTimeOffTransactions. Defaults to FALSE for all return fields which, for convenience, returns all fields for the SubstituteTransactionTimeOffTransactions.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object listed as text. Run \code{\link{getSchemaForObjects}}('SubstituteTransactionTimeOffTransaction') to get more field paths.
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
	#' @concept Substitute Tracking
	#' @return A list of SubstituteTransactionTimeOffTransactions
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	listSubstituteTransactionTimeOffTransactions <- function(searchConditionsList = NULL, SubstituteTransactionTimeOffTransactionID = F, SubstituteTransactionID = F, TimeOffTransactionID = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, fieldPaths = NULL, searchConditionsGroupType = "And", searchSortFieldNamesList = NULL, searchSortFieldNamesDescendingList = NULL, entityId = 1, schoolYearId = NULL, page = 1, pageSize = 100000, flatten = T, returnResponse = F){

		params <- as.list(environment())

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		listSkyObjects(module = "SubstituteTracking", objectName = "SubstituteTransactionTimeOffTransaction", searchFields = searchFields %>% append(fieldPaths), page = page, pageSize = pageSize, SearchConditionsList = searchConditionsList, SearchConditionsGroupType = searchConditionsGroupType, SearchSortFieldNamesList = searchSortFieldNamesList, SearchSortFieldNamesDescendingList = searchSortFieldNamesDescendingList, entityId = entityId, schoolYearId = schoolYearId, flatten = flatten, returnResponse = returnResponse)
	}

	#' Get a SubstituteTransactionTimeOffTransaction
	#'
	#' This function returns a dataframe or json object of a SubstituteTransactionTimeOffTransaction
	#' @param SubstituteTransactionTimeOffTransactionID The ID of the SubstituteTransactionTimeOffTransaction to return.
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given SubstituteTransactionTimeOffTransaction. Defaults to FALSE for all return fields which, for convenience, returns all fields for the SubstituteTransactionTimeOffTransaction.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object listed as text. Run \code{\link{getSchemaForObjects}}('SubstituteTransactionTimeOffTransaction') to get more field paths.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{listSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Substitute Tracking
	#' @return A dataframe or of SubstituteTransactionTimeOffTransaction
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	getSubstituteTransactionTimeOffTransaction <- function(SubstituteTransactionTimeOffTransactionID, SubstituteTransactionID = F, TimeOffTransactionID = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, fieldPaths = NULL, entityId = 1, schoolYearId = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment()) %>% purrr::keep(names(.) != "SubstituteTransactionTimeOffTransactionID")

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		getSkyObject(module = "SubstituteTracking", objectName = "SubstituteTransactionTimeOffTransaction", objectId = SubstituteTransactionTimeOffTransactionID, searchFields = searchFields, entityId = entityId, flatten = flatten, returnResponse = returnResponse)
	}

	#' Delete a SubstituteTransactionTimeOffTransaction
	#'
	#' This function deletes a SubstituteTransactionTimeOffTransaction
	#' @param SubstituteTransactionTimeOffTransactionID The ID of the SubstituteTransactionTimeOffTransaction to delete
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{listSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Substitute Tracking
	#' @return The SubstituteTransactionTimeOffTransactionID of the deleted SubstituteTransactionTimeOffTransaction.
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	deleteSubstituteTransactionTimeOffTransaction <- function(SubstituteTransactionTimeOffTransactionID, ignoreWarnings = F, entityId = 1, schoolYearId = NULL, flatten = T, returnResponse = F){

		deleteSkyObject(module = "SubstituteTracking", objectName = "SubstituteTransactionTimeOffTransaction", objectId = SubstituteTransactionTimeOffTransactionID, ignoreWarnings = ignoreWarnings, entityId = entityId, flatten = flatten, returnResponse = returnResponse)
	}

	#' Create a SubstituteTransactionTimeOffTransaction
	#'
	#' This function creates a SubstituteTransactionTimeOffTransaction
	#' @param fieldNames The field values to give the created SubstituteTransactionTimeOffTransaction. Each defaults to NULL.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{listSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Substitute Tracking
	#' @return A newly created SubstituteTransactionTimeOffTransaction
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	createSubstituteTransactionTimeOffTransaction <- function(SubstituteTransactionID = NULL, TimeOffTransactionID = NULL, entityId = 1, schoolYearId = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment())

		body <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper()) %>% purrr::compact()

		createSkyObject(module = "SubstituteTracking", objectName = "SubstituteTransactionTimeOffTransaction", body = list(DataObject = body), searchFields = append("SubstituteTransactionTimeOffTransactionID", body %>% names()), entityId = entityId, schoolYearId = schoolYearId, flatten = flatten, returnResponse = returnResponse)
	}

	#' Modify a SubstituteTransactionTimeOffTransaction
	#'
	#' This function modifies a SubstituteTransactionTimeOffTransaction
	#' @param fieldNames The field values to give the modified SubstituteTransactionTimeOffTransaction. Each defaults to NULL.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{listSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Substitute Tracking
	#' @return The modified SubstituteTransactionTimeOffTransaction
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	modifySubstituteTransactionTimeOffTransaction <- function(SubstituteTransactionTimeOffTransactionID, SubstituteTransactionID = NULL, TimeOffTransactionID = NULL, entityId = 1, schoolYearId = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment())

		body <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper()) %>% purrr::compact()

		modifySkyObject(module = "SubstituteTracking", objectName = "SubstituteTransactionTimeOffTransaction", objectId = SubstituteTransactionTimeOffTransactionID, body = list(DataObject = body), searchFields = append("SubstituteTransactionTimeOffTransactionID", body %>% names()), entityId = entityId, schoolYearId = schoolYearId, flatten = flatten, returnResponse = returnResponse)
	}

	#' List TempSubstituteTransactionAccountDistributions
	#'
	#' This function returns a dataframe or json object of TempSubstituteTransactionAccountDistributions
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given TempSubstituteTransactionAccountDistributions. Defaults to FALSE for all return fields which, for convenience, returns all fields for the TempSubstituteTransactionAccountDistributions.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object listed as text. Run \code{\link{getSchemaForObjects}}('TempSubstituteTransactionAccountDistribution') to get more field paths.
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
	#' @concept Substitute Tracking
	#' @return A list of TempSubstituteTransactionAccountDistributions
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	listTempSubstituteTransactionAccountDistributions <- function(searchConditionsList = NULL, TempSubstituteTransactionAccountDistributionID = F, TempSubstituteTransactionID = F, AccountID = F, Percent = F, StateConcordDepartmentTNID = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, fieldPaths = NULL, searchConditionsGroupType = "And", searchSortFieldNamesList = NULL, searchSortFieldNamesDescendingList = NULL, entityId = 1, schoolYearId = NULL, page = 1, pageSize = 100000, flatten = T, returnResponse = F){

		params <- as.list(environment())

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		listSkyObjects(module = "SubstituteTracking", objectName = "TempSubstituteTransactionAccountDistribution", searchFields = searchFields %>% append(fieldPaths), page = page, pageSize = pageSize, SearchConditionsList = searchConditionsList, SearchConditionsGroupType = searchConditionsGroupType, SearchSortFieldNamesList = searchSortFieldNamesList, SearchSortFieldNamesDescendingList = searchSortFieldNamesDescendingList, entityId = entityId, schoolYearId = schoolYearId, flatten = flatten, returnResponse = returnResponse)
	}

	#' Get a TempSubstituteTransactionAccountDistribution
	#'
	#' This function returns a dataframe or json object of a TempSubstituteTransactionAccountDistribution
	#' @param TempSubstituteTransactionAccountDistributionID The ID of the TempSubstituteTransactionAccountDistribution to return.
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given TempSubstituteTransactionAccountDistribution. Defaults to FALSE for all return fields which, for convenience, returns all fields for the TempSubstituteTransactionAccountDistribution.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object listed as text. Run \code{\link{getSchemaForObjects}}('TempSubstituteTransactionAccountDistribution') to get more field paths.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{listSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Substitute Tracking
	#' @return A dataframe or of TempSubstituteTransactionAccountDistribution
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	getTempSubstituteTransactionAccountDistribution <- function(TempSubstituteTransactionAccountDistributionID, TempSubstituteTransactionID = F, AccountID = F, Percent = F, StateConcordDepartmentTNID = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, fieldPaths = NULL, entityId = 1, schoolYearId = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment()) %>% purrr::keep(names(.) != "TempSubstituteTransactionAccountDistributionID")

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		getSkyObject(module = "SubstituteTracking", objectName = "TempSubstituteTransactionAccountDistribution", objectId = TempSubstituteTransactionAccountDistributionID, searchFields = searchFields, entityId = entityId, flatten = flatten, returnResponse = returnResponse)
	}

	#' Delete a TempSubstituteTransactionAccountDistribution
	#'
	#' This function deletes a TempSubstituteTransactionAccountDistribution
	#' @param TempSubstituteTransactionAccountDistributionID The ID of the TempSubstituteTransactionAccountDistribution to delete
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{listSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Substitute Tracking
	#' @return The TempSubstituteTransactionAccountDistributionID of the deleted TempSubstituteTransactionAccountDistribution.
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	deleteTempSubstituteTransactionAccountDistribution <- function(TempSubstituteTransactionAccountDistributionID, ignoreWarnings = F, entityId = 1, schoolYearId = NULL, flatten = T, returnResponse = F){

		deleteSkyObject(module = "SubstituteTracking", objectName = "TempSubstituteTransactionAccountDistribution", objectId = TempSubstituteTransactionAccountDistributionID, ignoreWarnings = ignoreWarnings, entityId = entityId, flatten = flatten, returnResponse = returnResponse)
	}

	#' Create a TempSubstituteTransactionAccountDistribution
	#'
	#' This function creates a TempSubstituteTransactionAccountDistribution
	#' @param fieldNames The field values to give the created TempSubstituteTransactionAccountDistribution. Each defaults to NULL.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{listSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Substitute Tracking
	#' @return A newly created TempSubstituteTransactionAccountDistribution
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	createTempSubstituteTransactionAccountDistribution <- function(TempSubstituteTransactionID = NULL, AccountID = NULL, Percent = NULL, StateConcordDepartmentTNID = NULL, entityId = 1, schoolYearId = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment())

		body <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper()) %>% purrr::compact()

		createSkyObject(module = "SubstituteTracking", objectName = "TempSubstituteTransactionAccountDistribution", body = list(DataObject = body), searchFields = append("TempSubstituteTransactionAccountDistributionID", body %>% names()), entityId = entityId, schoolYearId = schoolYearId, flatten = flatten, returnResponse = returnResponse)
	}

	#' Modify a TempSubstituteTransactionAccountDistribution
	#'
	#' This function modifies a TempSubstituteTransactionAccountDistribution
	#' @param fieldNames The field values to give the modified TempSubstituteTransactionAccountDistribution. Each defaults to NULL.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{listSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Substitute Tracking
	#' @return The modified TempSubstituteTransactionAccountDistribution
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	modifyTempSubstituteTransactionAccountDistribution <- function(TempSubstituteTransactionAccountDistributionID, TempSubstituteTransactionID = NULL, AccountID = NULL, Percent = NULL, StateConcordDepartmentTNID = NULL, entityId = 1, schoolYearId = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment())

		body <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper()) %>% purrr::compact()

		modifySkyObject(module = "SubstituteTracking", objectName = "TempSubstituteTransactionAccountDistribution", objectId = TempSubstituteTransactionAccountDistributionID, body = list(DataObject = body), searchFields = append("TempSubstituteTransactionAccountDistributionID", body %>% names()), entityId = entityId, schoolYearId = schoolYearId, flatten = flatten, returnResponse = returnResponse)
	}

	#' List TempSubstituteTransactions
	#'
	#' This function returns a dataframe or json object of TempSubstituteTransactions
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given TempSubstituteTransactions. Defaults to FALSE for all return fields which, for convenience, returns all fields for the TempSubstituteTransactions.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object listed as text. Run \code{\link{getSchemaForObjects}}('TempSubstituteTransaction') to get more field paths.
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
	#' @concept Substitute Tracking
	#' @return A list of TempSubstituteTransactions
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	listTempSubstituteTransactions <- function(searchConditionsList = NULL, TempSubstituteTransactionID = F, SubstitutePayScaleID = F, StartDateTime = F, Duration = F, FormattedDuration = F, AbsenceType = F, SubstituteEmployee = F, AbsentEmployee = F, AssignmentID = F, BuildingID = F, PositionID = F, UniqueImportRecordIdentifier = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, SubstituteTransactionID = F, EmployeeNameLFM = F, EmployeeAbsent = F, EmployeeID = F, StartDate = F, StartTime = F, PayAmount = F, UnroundedWorkAmount = F, FormattedUnroundedWorkAmount = F, LineNumber = F, SubstituteEmployeeNumber = F, AbsentEmployeeNumber = F, SubstitutePayScaleIDClonedFrom = F, ThirdPartyImportAccountSource = F, TempSubstitutePayScaleID = F, fieldPaths = NULL, searchConditionsGroupType = "And", searchSortFieldNamesList = NULL, searchSortFieldNamesDescendingList = NULL, entityId = 1, schoolYearId = NULL, page = 1, pageSize = 100000, flatten = T, returnResponse = F){

		params <- as.list(environment())

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		listSkyObjects(module = "SubstituteTracking", objectName = "TempSubstituteTransaction", searchFields = searchFields %>% append(fieldPaths), page = page, pageSize = pageSize, SearchConditionsList = searchConditionsList, SearchConditionsGroupType = searchConditionsGroupType, SearchSortFieldNamesList = searchSortFieldNamesList, SearchSortFieldNamesDescendingList = searchSortFieldNamesDescendingList, entityId = entityId, schoolYearId = schoolYearId, flatten = flatten, returnResponse = returnResponse)
	}

	#' Get a TempSubstituteTransaction
	#'
	#' This function returns a dataframe or json object of a TempSubstituteTransaction
	#' @param TempSubstituteTransactionID The ID of the TempSubstituteTransaction to return.
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given TempSubstituteTransaction. Defaults to FALSE for all return fields which, for convenience, returns all fields for the TempSubstituteTransaction.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object listed as text. Run \code{\link{getSchemaForObjects}}('TempSubstituteTransaction') to get more field paths.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{listSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Substitute Tracking
	#' @return A dataframe or of TempSubstituteTransaction
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	getTempSubstituteTransaction <- function(TempSubstituteTransactionID, SubstitutePayScaleID = F, StartDateTime = F, Duration = F, FormattedDuration = F, AbsenceType = F, SubstituteEmployee = F, AbsentEmployee = F, AssignmentID = F, BuildingID = F, PositionID = F, UniqueImportRecordIdentifier = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, SubstituteTransactionID = F, EmployeeNameLFM = F, EmployeeAbsent = F, EmployeeID = F, StartDate = F, StartTime = F, PayAmount = F, UnroundedWorkAmount = F, FormattedUnroundedWorkAmount = F, LineNumber = F, SubstituteEmployeeNumber = F, AbsentEmployeeNumber = F, SubstitutePayScaleIDClonedFrom = F, ThirdPartyImportAccountSource = F, TempSubstitutePayScaleID = F, fieldPaths = NULL, entityId = 1, schoolYearId = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment()) %>% purrr::keep(names(.) != "TempSubstituteTransactionID")

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		getSkyObject(module = "SubstituteTracking", objectName = "TempSubstituteTransaction", objectId = TempSubstituteTransactionID, searchFields = searchFields, entityId = entityId, flatten = flatten, returnResponse = returnResponse)
	}

	#' Delete a TempSubstituteTransaction
	#'
	#' This function deletes a TempSubstituteTransaction
	#' @param TempSubstituteTransactionID The ID of the TempSubstituteTransaction to delete
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{listSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Substitute Tracking
	#' @return The TempSubstituteTransactionID of the deleted TempSubstituteTransaction.
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	deleteTempSubstituteTransaction <- function(TempSubstituteTransactionID, ignoreWarnings = F, entityId = 1, schoolYearId = NULL, flatten = T, returnResponse = F){

		deleteSkyObject(module = "SubstituteTracking", objectName = "TempSubstituteTransaction", objectId = TempSubstituteTransactionID, ignoreWarnings = ignoreWarnings, entityId = entityId, flatten = flatten, returnResponse = returnResponse)
	}

	#' Create a TempSubstituteTransaction
	#'
	#' This function creates a TempSubstituteTransaction
	#' @param fieldNames The field values to give the created TempSubstituteTransaction. Each defaults to NULL.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{listSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Substitute Tracking
	#' @return A newly created TempSubstituteTransaction
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	createTempSubstituteTransaction <- function(SubstitutePayScaleID = NULL, StartDateTime = NULL, Duration = NULL, AbsenceType = NULL, SubstituteEmployee = NULL, AbsentEmployee = NULL, AssignmentID = NULL, BuildingID = NULL, PositionID = NULL, UniqueImportRecordIdentifier = NULL, SubstituteTransactionID = NULL, EmployeeNameLFM = NULL, EmployeeAbsent = NULL, PayAmount = NULL, UnroundedWorkAmount = NULL, LineNumber = NULL, SubstituteEmployeeNumber = NULL, AbsentEmployeeNumber = NULL, SubstitutePayScaleIDClonedFrom = NULL, ThirdPartyImportAccountSource = NULL, TempSubstitutePayScaleID = NULL, entityId = 1, schoolYearId = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment())

		body <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper()) %>% purrr::compact()

		createSkyObject(module = "SubstituteTracking", objectName = "TempSubstituteTransaction", body = list(DataObject = body), searchFields = append("TempSubstituteTransactionID", body %>% names()), entityId = entityId, schoolYearId = schoolYearId, flatten = flatten, returnResponse = returnResponse)
	}

	#' Modify a TempSubstituteTransaction
	#'
	#' This function modifies a TempSubstituteTransaction
	#' @param fieldNames The field values to give the modified TempSubstituteTransaction. Each defaults to NULL.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{listSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Substitute Tracking
	#' @return The modified TempSubstituteTransaction
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	modifyTempSubstituteTransaction <- function(TempSubstituteTransactionID, SubstitutePayScaleID = NULL, StartDateTime = NULL, Duration = NULL, AbsenceType = NULL, SubstituteEmployee = NULL, AbsentEmployee = NULL, AssignmentID = NULL, BuildingID = NULL, PositionID = NULL, UniqueImportRecordIdentifier = NULL, SubstituteTransactionID = NULL, EmployeeNameLFM = NULL, EmployeeAbsent = NULL, PayAmount = NULL, UnroundedWorkAmount = NULL, LineNumber = NULL, SubstituteEmployeeNumber = NULL, AbsentEmployeeNumber = NULL, SubstitutePayScaleIDClonedFrom = NULL, ThirdPartyImportAccountSource = NULL, TempSubstitutePayScaleID = NULL, entityId = 1, schoolYearId = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment())

		body <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper()) %>% purrr::compact()

		modifySkyObject(module = "SubstituteTracking", objectName = "TempSubstituteTransaction", objectId = TempSubstituteTransactionID, body = list(DataObject = body), searchFields = append("TempSubstituteTransactionID", body %>% names()), entityId = entityId, schoolYearId = schoolYearId, flatten = flatten, returnResponse = returnResponse)
	}

	#' List SubstituteTrackingTempExceptions
	#'
	#' This function returns a dataframe or json object of SubstituteTrackingTempExceptions
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given SubstituteTrackingTempExceptions. Defaults to FALSE for all return fields which, for convenience, returns all fields for the SubstituteTrackingTempExceptions.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object listed as text. Run \code{\link{getSchemaForObjects}}('SubstituteTrackingTempException') to get more field paths.
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
	#' @concept Substitute Tracking
	#' @return A list of SubstituteTrackingTempExceptions
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	listSubstituteTrackingTempExceptions <- function(searchConditionsList = NULL, TempExceptionID = F, SubstituteTransactionID = F, SubstituteName = F, Message = F, LineNumber = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, IsFatal = F, SubstituteEmployeeNumber = F, fieldPaths = NULL, searchConditionsGroupType = "And", searchSortFieldNamesList = NULL, searchSortFieldNamesDescendingList = NULL, entityId = 1, schoolYearId = NULL, page = 1, pageSize = 100000, flatten = T, returnResponse = F){

		params <- as.list(environment())

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		listSkyObjects(module = "SubstituteTracking", objectName = "TempException", searchFields = searchFields %>% append(fieldPaths), page = page, pageSize = pageSize, SearchConditionsList = searchConditionsList, SearchConditionsGroupType = searchConditionsGroupType, SearchSortFieldNamesList = searchSortFieldNamesList, SearchSortFieldNamesDescendingList = searchSortFieldNamesDescendingList, entityId = entityId, schoolYearId = schoolYearId, flatten = flatten, returnResponse = returnResponse)
	}

	#' Get a SubstituteTrackingTempException
	#'
	#' This function returns a dataframe or json object of a SubstituteTrackingTempException
	#' @param SubstituteTrackingTempExceptionID The ID of the SubstituteTrackingTempException to return.
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given SubstituteTrackingTempException. Defaults to FALSE for all return fields which, for convenience, returns all fields for the SubstituteTrackingTempException.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object listed as text. Run \code{\link{getSchemaForObjects}}('SubstituteTrackingTempException') to get more field paths.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{listSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Substitute Tracking
	#' @return A dataframe or of SubstituteTrackingTempException
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	getSubstituteTrackingTempException <- function(SubstituteTrackingTempExceptionID, TempExceptionID = F, SubstituteTransactionID = F, SubstituteName = F, Message = F, LineNumber = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, IsFatal = F, SubstituteEmployeeNumber = F, fieldPaths = NULL, entityId = 1, schoolYearId = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment()) %>% purrr::keep(names(.) != "SubstituteTrackingTempExceptionID")

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		getSkyObject(module = "SubstituteTracking", objectName = "TempException", objectId = SubstituteTrackingTempExceptionID, searchFields = searchFields, entityId = entityId, flatten = flatten, returnResponse = returnResponse)
	}

	#' Delete a SubstituteTrackingTempException
	#'
	#' This function deletes a SubstituteTrackingTempException
	#' @param SubstituteTrackingTempExceptionID The ID of the SubstituteTrackingTempException to delete
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{listSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Substitute Tracking
	#' @return The SubstituteTrackingTempExceptionID of the deleted SubstituteTrackingTempException.
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	deleteSubstituteTrackingTempException <- function(SubstituteTrackingTempExceptionID, ignoreWarnings = F, entityId = 1, schoolYearId = NULL, flatten = T, returnResponse = F){

		deleteSkyObject(module = "SubstituteTracking", objectName = "TempException", objectId = SubstituteTrackingTempExceptionID, ignoreWarnings = ignoreWarnings, entityId = entityId, flatten = flatten, returnResponse = returnResponse)
	}

	#' Create a SubstituteTrackingTempException
	#'
	#' This function creates a SubstituteTrackingTempException
	#' @param fieldNames The field values to give the created SubstituteTrackingTempException. Each defaults to NULL.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{listSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Substitute Tracking
	#' @return A newly created SubstituteTrackingTempException
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	createSubstituteTrackingTempException <- function(SubstituteTransactionID = NULL, SubstituteName = NULL, Message = NULL, LineNumber = NULL, IsFatal = NULL, SubstituteEmployeeNumber = NULL, entityId = 1, schoolYearId = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment())

		body <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper()) %>% purrr::compact()

		createSkyObject(module = "SubstituteTracking", objectName = "TempException", body = list(DataObject = body), searchFields = append("TempExceptionID", body %>% names()), entityId = entityId, schoolYearId = schoolYearId, flatten = flatten, returnResponse = returnResponse)
	}

	#' Modify a SubstituteTrackingTempException
	#'
	#' This function modifies a SubstituteTrackingTempException
	#' @param fieldNames The field values to give the modified SubstituteTrackingTempException. Each defaults to NULL.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{listSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Substitute Tracking
	#' @return The modified SubstituteTrackingTempException
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	modifySubstituteTrackingTempException <- function(TempExceptionID, SubstituteTransactionID = NULL, SubstituteName = NULL, Message = NULL, LineNumber = NULL, IsFatal = NULL, SubstituteEmployeeNumber = NULL, entityId = 1, schoolYearId = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment())

		body <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper()) %>% purrr::compact()

		modifySkyObject(module = "SubstituteTracking", objectName = "TempException", objectId = TempExceptionID, body = list(DataObject = body), searchFields = append("TempExceptionID", body %>% names()), entityId = entityId, schoolYearId = schoolYearId, flatten = flatten, returnResponse = returnResponse)
	}

	#' List ThirdPartyFormatBuildings
	#'
	#' This function returns a dataframe or json object of ThirdPartyFormatBuildings
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given ThirdPartyFormatBuildings. Defaults to FALSE for all return fields which, for convenience, returns all fields for the ThirdPartyFormatBuildings.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object listed as text. Run \code{\link{getSchemaForObjects}}('ThirdPartyFormatBuilding') to get more field paths.
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
	#' @concept Substitute Tracking
	#' @return A list of ThirdPartyFormatBuildings
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	listThirdPartyFormatBuildings <- function(searchConditionsList = NULL, ThirdPartyFormatBuildingID = F, ThirdPartyFormatID = F, BuildingID = F, ImportValue = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, fieldPaths = NULL, searchConditionsGroupType = "And", searchSortFieldNamesList = NULL, searchSortFieldNamesDescendingList = NULL, entityId = 1, schoolYearId = NULL, page = 1, pageSize = 100000, flatten = T, returnResponse = F){

		params <- as.list(environment())

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		listSkyObjects(module = "SubstituteTracking", objectName = "ThirdPartyFormatBuilding", searchFields = searchFields %>% append(fieldPaths), page = page, pageSize = pageSize, SearchConditionsList = searchConditionsList, SearchConditionsGroupType = searchConditionsGroupType, SearchSortFieldNamesList = searchSortFieldNamesList, SearchSortFieldNamesDescendingList = searchSortFieldNamesDescendingList, entityId = entityId, schoolYearId = schoolYearId, flatten = flatten, returnResponse = returnResponse)
	}

	#' Get a ThirdPartyFormatBuilding
	#'
	#' This function returns a dataframe or json object of a ThirdPartyFormatBuilding
	#' @param ThirdPartyFormatBuildingID The ID of the ThirdPartyFormatBuilding to return.
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given ThirdPartyFormatBuilding. Defaults to FALSE for all return fields which, for convenience, returns all fields for the ThirdPartyFormatBuilding.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object listed as text. Run \code{\link{getSchemaForObjects}}('ThirdPartyFormatBuilding') to get more field paths.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{listSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Substitute Tracking
	#' @return A dataframe or of ThirdPartyFormatBuilding
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	getThirdPartyFormatBuilding <- function(ThirdPartyFormatBuildingID, ThirdPartyFormatID = F, BuildingID = F, ImportValue = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, fieldPaths = NULL, entityId = 1, schoolYearId = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment()) %>% purrr::keep(names(.) != "ThirdPartyFormatBuildingID")

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		getSkyObject(module = "SubstituteTracking", objectName = "ThirdPartyFormatBuilding", objectId = ThirdPartyFormatBuildingID, searchFields = searchFields, entityId = entityId, flatten = flatten, returnResponse = returnResponse)
	}

	#' Delete a ThirdPartyFormatBuilding
	#'
	#' This function deletes a ThirdPartyFormatBuilding
	#' @param ThirdPartyFormatBuildingID The ID of the ThirdPartyFormatBuilding to delete
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{listSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Substitute Tracking
	#' @return The ThirdPartyFormatBuildingID of the deleted ThirdPartyFormatBuilding.
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	deleteThirdPartyFormatBuilding <- function(ThirdPartyFormatBuildingID, ignoreWarnings = F, entityId = 1, schoolYearId = NULL, flatten = T, returnResponse = F){

		deleteSkyObject(module = "SubstituteTracking", objectName = "ThirdPartyFormatBuilding", objectId = ThirdPartyFormatBuildingID, ignoreWarnings = ignoreWarnings, entityId = entityId, flatten = flatten, returnResponse = returnResponse)
	}

	#' Create a ThirdPartyFormatBuilding
	#'
	#' This function creates a ThirdPartyFormatBuilding
	#' @param fieldNames The field values to give the created ThirdPartyFormatBuilding. Each defaults to NULL.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{listSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Substitute Tracking
	#' @return A newly created ThirdPartyFormatBuilding
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	createThirdPartyFormatBuilding <- function(ThirdPartyFormatID = NULL, BuildingID = NULL, ImportValue = NULL, entityId = 1, schoolYearId = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment())

		body <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper()) %>% purrr::compact()

		createSkyObject(module = "SubstituteTracking", objectName = "ThirdPartyFormatBuilding", body = list(DataObject = body), searchFields = append("ThirdPartyFormatBuildingID", body %>% names()), entityId = entityId, schoolYearId = schoolYearId, flatten = flatten, returnResponse = returnResponse)
	}

	#' Modify a ThirdPartyFormatBuilding
	#'
	#' This function modifies a ThirdPartyFormatBuilding
	#' @param fieldNames The field values to give the modified ThirdPartyFormatBuilding. Each defaults to NULL.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{listSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Substitute Tracking
	#' @return The modified ThirdPartyFormatBuilding
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	modifyThirdPartyFormatBuilding <- function(ThirdPartyFormatBuildingID, ThirdPartyFormatID = NULL, BuildingID = NULL, ImportValue = NULL, entityId = 1, schoolYearId = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment())

		body <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper()) %>% purrr::compact()

		modifySkyObject(module = "SubstituteTracking", objectName = "ThirdPartyFormatBuilding", objectId = ThirdPartyFormatBuildingID, body = list(DataObject = body), searchFields = append("ThirdPartyFormatBuildingID", body %>% names()), entityId = entityId, schoolYearId = schoolYearId, flatten = flatten, returnResponse = returnResponse)
	}

	#' List ThirdPartyFormatTimeOffs
	#'
	#' This function returns a dataframe or json object of ThirdPartyFormatTimeOffs
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given ThirdPartyFormatTimeOffs. Defaults to FALSE for all return fields which, for convenience, returns all fields for the ThirdPartyFormatTimeOffs.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object listed as text. Run \code{\link{getSchemaForObjects}}('ThirdPartyFormatTimeOff') to get more field paths.
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
	#' @concept Substitute Tracking
	#' @return A list of ThirdPartyFormatTimeOffs
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	listThirdPartyFormatTimeOffs <- function(searchConditionsList = NULL, ThirdPartyFormatTimeOffID = F, ThirdPartyFormatID = F, ImportValue = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, SkipTimeOffOnlyImport = F, fieldPaths = NULL, searchConditionsGroupType = "And", searchSortFieldNamesList = NULL, searchSortFieldNamesDescendingList = NULL, entityId = 1, schoolYearId = NULL, page = 1, pageSize = 100000, flatten = T, returnResponse = F){

		params <- as.list(environment())

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		listSkyObjects(module = "SubstituteTracking", objectName = "ThirdPartyFormatTimeOff", searchFields = searchFields %>% append(fieldPaths), page = page, pageSize = pageSize, SearchConditionsList = searchConditionsList, SearchConditionsGroupType = searchConditionsGroupType, SearchSortFieldNamesList = searchSortFieldNamesList, SearchSortFieldNamesDescendingList = searchSortFieldNamesDescendingList, entityId = entityId, schoolYearId = schoolYearId, flatten = flatten, returnResponse = returnResponse)
	}

	#' Get a ThirdPartyFormatTimeOff
	#'
	#' This function returns a dataframe or json object of a ThirdPartyFormatTimeOff
	#' @param ThirdPartyFormatTimeOffID The ID of the ThirdPartyFormatTimeOff to return.
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given ThirdPartyFormatTimeOff. Defaults to FALSE for all return fields which, for convenience, returns all fields for the ThirdPartyFormatTimeOff.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object listed as text. Run \code{\link{getSchemaForObjects}}('ThirdPartyFormatTimeOff') to get more field paths.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{listSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Substitute Tracking
	#' @return A dataframe or of ThirdPartyFormatTimeOff
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	getThirdPartyFormatTimeOff <- function(ThirdPartyFormatTimeOffID, ThirdPartyFormatID = F, ImportValue = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, SkipTimeOffOnlyImport = F, fieldPaths = NULL, entityId = 1, schoolYearId = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment()) %>% purrr::keep(names(.) != "ThirdPartyFormatTimeOffID")

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		getSkyObject(module = "SubstituteTracking", objectName = "ThirdPartyFormatTimeOff", objectId = ThirdPartyFormatTimeOffID, searchFields = searchFields, entityId = entityId, flatten = flatten, returnResponse = returnResponse)
	}

	#' Delete a ThirdPartyFormatTimeOff
	#'
	#' This function deletes a ThirdPartyFormatTimeOff
	#' @param ThirdPartyFormatTimeOffID The ID of the ThirdPartyFormatTimeOff to delete
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{listSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Substitute Tracking
	#' @return The ThirdPartyFormatTimeOffID of the deleted ThirdPartyFormatTimeOff.
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	deleteThirdPartyFormatTimeOff <- function(ThirdPartyFormatTimeOffID, ignoreWarnings = F, entityId = 1, schoolYearId = NULL, flatten = T, returnResponse = F){

		deleteSkyObject(module = "SubstituteTracking", objectName = "ThirdPartyFormatTimeOff", objectId = ThirdPartyFormatTimeOffID, ignoreWarnings = ignoreWarnings, entityId = entityId, flatten = flatten, returnResponse = returnResponse)
	}

	#' Create a ThirdPartyFormatTimeOff
	#'
	#' This function creates a ThirdPartyFormatTimeOff
	#' @param fieldNames The field values to give the created ThirdPartyFormatTimeOff. Each defaults to NULL.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{listSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Substitute Tracking
	#' @return A newly created ThirdPartyFormatTimeOff
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	createThirdPartyFormatTimeOff <- function(ThirdPartyFormatID = NULL, ImportValue = NULL, SkipTimeOffOnlyImport = NULL, entityId = 1, schoolYearId = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment())

		body <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper()) %>% purrr::compact()

		createSkyObject(module = "SubstituteTracking", objectName = "ThirdPartyFormatTimeOff", body = list(DataObject = body), searchFields = append("ThirdPartyFormatTimeOffID", body %>% names()), entityId = entityId, schoolYearId = schoolYearId, flatten = flatten, returnResponse = returnResponse)
	}

	#' Modify a ThirdPartyFormatTimeOff
	#'
	#' This function modifies a ThirdPartyFormatTimeOff
	#' @param fieldNames The field values to give the modified ThirdPartyFormatTimeOff. Each defaults to NULL.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{listSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Substitute Tracking
	#' @return The modified ThirdPartyFormatTimeOff
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	modifyThirdPartyFormatTimeOff <- function(ThirdPartyFormatTimeOffID, ThirdPartyFormatID = NULL, ImportValue = NULL, SkipTimeOffOnlyImport = NULL, entityId = 1, schoolYearId = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment())

		body <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper()) %>% purrr::compact()

		modifySkyObject(module = "SubstituteTracking", objectName = "ThirdPartyFormatTimeOff", objectId = ThirdPartyFormatTimeOffID, body = list(DataObject = body), searchFields = append("ThirdPartyFormatTimeOffID", body %>% names()), entityId = entityId, schoolYearId = schoolYearId, flatten = flatten, returnResponse = returnResponse)
	}

	#' List ThirdPartyFormatTimeOffTypeReasons
	#'
	#' This function returns a dataframe or json object of ThirdPartyFormatTimeOffTypeReasons
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given ThirdPartyFormatTimeOffTypeReasons. Defaults to FALSE for all return fields which, for convenience, returns all fields for the ThirdPartyFormatTimeOffTypeReasons.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object listed as text. Run \code{\link{getSchemaForObjects}}('ThirdPartyFormatTimeOffTypeReason') to get more field paths.
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
	#' @concept Substitute Tracking
	#' @return A list of ThirdPartyFormatTimeOffTypeReasons
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	listThirdPartyFormatTimeOffTypeReasons <- function(searchConditionsList = NULL, ThirdPartyFormatTimeOffTypeReasonID = F, ThirdPartyFormatTimeOffID = F, TimeOffTypeReasonID = F, Rank = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, ImportAsUnpaid = F, fieldPaths = NULL, searchConditionsGroupType = "And", searchSortFieldNamesList = NULL, searchSortFieldNamesDescendingList = NULL, entityId = 1, schoolYearId = NULL, page = 1, pageSize = 100000, flatten = T, returnResponse = F){

		params <- as.list(environment())

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		listSkyObjects(module = "SubstituteTracking", objectName = "ThirdPartyFormatTimeOffTypeReason", searchFields = searchFields %>% append(fieldPaths), page = page, pageSize = pageSize, SearchConditionsList = searchConditionsList, SearchConditionsGroupType = searchConditionsGroupType, SearchSortFieldNamesList = searchSortFieldNamesList, SearchSortFieldNamesDescendingList = searchSortFieldNamesDescendingList, entityId = entityId, schoolYearId = schoolYearId, flatten = flatten, returnResponse = returnResponse)
	}

	#' Get a ThirdPartyFormatTimeOffTypeReason
	#'
	#' This function returns a dataframe or json object of a ThirdPartyFormatTimeOffTypeReason
	#' @param ThirdPartyFormatTimeOffTypeReasonID The ID of the ThirdPartyFormatTimeOffTypeReason to return.
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given ThirdPartyFormatTimeOffTypeReason. Defaults to FALSE for all return fields which, for convenience, returns all fields for the ThirdPartyFormatTimeOffTypeReason.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object listed as text. Run \code{\link{getSchemaForObjects}}('ThirdPartyFormatTimeOffTypeReason') to get more field paths.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{listSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Substitute Tracking
	#' @return A dataframe or of ThirdPartyFormatTimeOffTypeReason
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	getThirdPartyFormatTimeOffTypeReason <- function(ThirdPartyFormatTimeOffTypeReasonID, ThirdPartyFormatTimeOffID = F, TimeOffTypeReasonID = F, Rank = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, ImportAsUnpaid = F, fieldPaths = NULL, entityId = 1, schoolYearId = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment()) %>% purrr::keep(names(.) != "ThirdPartyFormatTimeOffTypeReasonID")

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		getSkyObject(module = "SubstituteTracking", objectName = "ThirdPartyFormatTimeOffTypeReason", objectId = ThirdPartyFormatTimeOffTypeReasonID, searchFields = searchFields, entityId = entityId, flatten = flatten, returnResponse = returnResponse)
	}

	#' Delete a ThirdPartyFormatTimeOffTypeReason
	#'
	#' This function deletes a ThirdPartyFormatTimeOffTypeReason
	#' @param ThirdPartyFormatTimeOffTypeReasonID The ID of the ThirdPartyFormatTimeOffTypeReason to delete
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{listSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Substitute Tracking
	#' @return The ThirdPartyFormatTimeOffTypeReasonID of the deleted ThirdPartyFormatTimeOffTypeReason.
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	deleteThirdPartyFormatTimeOffTypeReason <- function(ThirdPartyFormatTimeOffTypeReasonID, ignoreWarnings = F, entityId = 1, schoolYearId = NULL, flatten = T, returnResponse = F){

		deleteSkyObject(module = "SubstituteTracking", objectName = "ThirdPartyFormatTimeOffTypeReason", objectId = ThirdPartyFormatTimeOffTypeReasonID, ignoreWarnings = ignoreWarnings, entityId = entityId, flatten = flatten, returnResponse = returnResponse)
	}

	#' Create a ThirdPartyFormatTimeOffTypeReason
	#'
	#' This function creates a ThirdPartyFormatTimeOffTypeReason
	#' @param fieldNames The field values to give the created ThirdPartyFormatTimeOffTypeReason. Each defaults to NULL.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{listSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Substitute Tracking
	#' @return A newly created ThirdPartyFormatTimeOffTypeReason
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	createThirdPartyFormatTimeOffTypeReason <- function(ThirdPartyFormatTimeOffID = NULL, TimeOffTypeReasonID = NULL, Rank = NULL, entityId = 1, schoolYearId = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment())

		body <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper()) %>% purrr::compact()

		createSkyObject(module = "SubstituteTracking", objectName = "ThirdPartyFormatTimeOffTypeReason", body = list(DataObject = body), searchFields = append("ThirdPartyFormatTimeOffTypeReasonID", body %>% names()), entityId = entityId, schoolYearId = schoolYearId, flatten = flatten, returnResponse = returnResponse)
	}

	#' Modify a ThirdPartyFormatTimeOffTypeReason
	#'
	#' This function modifies a ThirdPartyFormatTimeOffTypeReason
	#' @param fieldNames The field values to give the modified ThirdPartyFormatTimeOffTypeReason. Each defaults to NULL.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{listSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Substitute Tracking
	#' @return The modified ThirdPartyFormatTimeOffTypeReason
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	modifyThirdPartyFormatTimeOffTypeReason <- function(ThirdPartyFormatTimeOffTypeReasonID, ThirdPartyFormatTimeOffID = NULL, TimeOffTypeReasonID = NULL, Rank = NULL, entityId = 1, schoolYearId = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment())

		body <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper()) %>% purrr::compact()

		modifySkyObject(module = "SubstituteTracking", objectName = "ThirdPartyFormatTimeOffTypeReason", objectId = ThirdPartyFormatTimeOffTypeReasonID, body = list(DataObject = body), searchFields = append("ThirdPartyFormatTimeOffTypeReasonID", body %>% names()), entityId = entityId, schoolYearId = schoolYearId, flatten = flatten, returnResponse = returnResponse)
	}

	#' List ThirdPartyFormatVacancies
	#'
	#' This function returns a dataframe or json object of ThirdPartyFormatVacancies
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given ThirdPartyFormatVacancies. Defaults to FALSE for all return fields which, for convenience, returns all fields for the ThirdPartyFormatVacancies.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object listed as text. Run \code{\link{getSchemaForObjects}}('ThirdPartyFormatVacancy') to get more field paths.
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
	#' @concept Substitute Tracking
	#' @return A list of ThirdPartyFormatVacancies
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	listThirdPartyFormatVacancies <- function(searchConditionsList = NULL, ThirdPartyFormatVacancyID = F, ThirdPartyFormatID = F, ImportValue = F, PayScaleID = F, PositionNumberID = F, VacancyType = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, ThirdPartyFormatVacancyIDClonedFrom = F, fieldPaths = NULL, searchConditionsGroupType = "And", searchSortFieldNamesList = NULL, searchSortFieldNamesDescendingList = NULL, entityId = 1, schoolYearId = NULL, page = 1, pageSize = 100000, flatten = T, returnResponse = F){

		params <- as.list(environment())

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		listSkyObjects(module = "SubstituteTracking", objectName = "ThirdPartyFormatVacancy", searchFields = searchFields %>% append(fieldPaths), page = page, pageSize = pageSize, SearchConditionsList = searchConditionsList, SearchConditionsGroupType = searchConditionsGroupType, SearchSortFieldNamesList = searchSortFieldNamesList, SearchSortFieldNamesDescendingList = searchSortFieldNamesDescendingList, entityId = entityId, schoolYearId = schoolYearId, flatten = flatten, returnResponse = returnResponse)
	}

	#' Get a ThirdPartyFormatVacancy
	#'
	#' This function returns a dataframe or json object of a ThirdPartyFormatVacancy
	#' @param ThirdPartyFormatVacancyID The ID of the ThirdPartyFormatVacancy to return.
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given ThirdPartyFormatVacancy. Defaults to FALSE for all return fields which, for convenience, returns all fields for the ThirdPartyFormatVacancy.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object listed as text. Run \code{\link{getSchemaForObjects}}('ThirdPartyFormatVacancy') to get more field paths.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{listSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Substitute Tracking
	#' @return A dataframe or of ThirdPartyFormatVacancy
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	getThirdPartyFormatVacancy <- function(ThirdPartyFormatVacancyID, ThirdPartyFormatID = F, ImportValue = F, PayScaleID = F, PositionNumberID = F, VacancyType = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, ThirdPartyFormatVacancyIDClonedFrom = F, fieldPaths = NULL, entityId = 1, schoolYearId = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment()) %>% purrr::keep(names(.) != "ThirdPartyFormatVacancyID")

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		getSkyObject(module = "SubstituteTracking", objectName = "ThirdPartyFormatVacancy", objectId = ThirdPartyFormatVacancyID, searchFields = searchFields, entityId = entityId, flatten = flatten, returnResponse = returnResponse)
	}

	#' Delete a ThirdPartyFormatVacancy
	#'
	#' This function deletes a ThirdPartyFormatVacancy
	#' @param ThirdPartyFormatVacancyID The ID of the ThirdPartyFormatVacancy to delete
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{listSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Substitute Tracking
	#' @return The ThirdPartyFormatVacancyID of the deleted ThirdPartyFormatVacancy.
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	deleteThirdPartyFormatVacancy <- function(ThirdPartyFormatVacancyID, ignoreWarnings = F, entityId = 1, schoolYearId = NULL, flatten = T, returnResponse = F){

		deleteSkyObject(module = "SubstituteTracking", objectName = "ThirdPartyFormatVacancy", objectId = ThirdPartyFormatVacancyID, ignoreWarnings = ignoreWarnings, entityId = entityId, flatten = flatten, returnResponse = returnResponse)
	}

	#' Create a ThirdPartyFormatVacancy
	#'
	#' This function creates a ThirdPartyFormatVacancy
	#' @param fieldNames The field values to give the created ThirdPartyFormatVacancy. Each defaults to NULL.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{listSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Substitute Tracking
	#' @return A newly created ThirdPartyFormatVacancy
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	createThirdPartyFormatVacancy <- function(ThirdPartyFormatID = NULL, ImportValue = NULL, PayScaleID = NULL, PositionNumberID = NULL, VacancyType = NULL, ThirdPartyFormatVacancyIDClonedFrom = NULL, entityId = 1, schoolYearId = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment())

		body <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper()) %>% purrr::compact()

		createSkyObject(module = "SubstituteTracking", objectName = "ThirdPartyFormatVacancy", body = list(DataObject = body), searchFields = append("ThirdPartyFormatVacancyID", body %>% names()), entityId = entityId, schoolYearId = schoolYearId, flatten = flatten, returnResponse = returnResponse)
	}

	#' Modify a ThirdPartyFormatVacancy
	#'
	#' This function modifies a ThirdPartyFormatVacancy
	#' @param fieldNames The field values to give the modified ThirdPartyFormatVacancy. Each defaults to NULL.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{listSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Substitute Tracking
	#' @return The modified ThirdPartyFormatVacancy
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	modifyThirdPartyFormatVacancy <- function(ThirdPartyFormatVacancyID, ThirdPartyFormatID = NULL, ImportValue = NULL, PayScaleID = NULL, PositionNumberID = NULL, VacancyType = NULL, ThirdPartyFormatVacancyIDClonedFrom = NULL, entityId = 1, schoolYearId = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment())

		body <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper()) %>% purrr::compact()

		modifySkyObject(module = "SubstituteTracking", objectName = "ThirdPartyFormatVacancy", objectId = ThirdPartyFormatVacancyID, body = list(DataObject = body), searchFields = append("ThirdPartyFormatVacancyID", body %>% names()), entityId = entityId, schoolYearId = schoolYearId, flatten = flatten, returnResponse = returnResponse)
	}

	#' List ThirdPartyImports
	#'
	#' This function returns a dataframe or json object of ThirdPartyImports
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given ThirdPartyImports. Defaults to FALSE for all return fields which, for convenience, returns all fields for the ThirdPartyImports.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object listed as text. Run \code{\link{getSchemaForObjects}}('ThirdPartyImport') to get more field paths.
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
	#' @concept Substitute Tracking
	#' @return A list of ThirdPartyImports
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	listThirdPartyImports <- function(searchConditionsList = NULL, ThirdPartyImportID = F, ThirdPartyFormatID = F, Request = F, ImportTime = F, ImportData = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, MediaID = F, MediaIDFailedResult = F, ProcessedSubstituteTransactions = F, ProcessedTimeOffTransactions = F, fieldPaths = NULL, searchConditionsGroupType = "And", searchSortFieldNamesList = NULL, searchSortFieldNamesDescendingList = NULL, entityId = 1, schoolYearId = NULL, page = 1, pageSize = 100000, flatten = T, returnResponse = F){

		params <- as.list(environment())

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		listSkyObjects(module = "SubstituteTracking", objectName = "ThirdPartyImport", searchFields = searchFields %>% append(fieldPaths), page = page, pageSize = pageSize, SearchConditionsList = searchConditionsList, SearchConditionsGroupType = searchConditionsGroupType, SearchSortFieldNamesList = searchSortFieldNamesList, SearchSortFieldNamesDescendingList = searchSortFieldNamesDescendingList, entityId = entityId, schoolYearId = schoolYearId, flatten = flatten, returnResponse = returnResponse)
	}

	#' Get a ThirdPartyImport
	#'
	#' This function returns a dataframe or json object of a ThirdPartyImport
	#' @param ThirdPartyImportID The ID of the ThirdPartyImport to return.
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given ThirdPartyImport. Defaults to FALSE for all return fields which, for convenience, returns all fields for the ThirdPartyImport.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object listed as text. Run \code{\link{getSchemaForObjects}}('ThirdPartyImport') to get more field paths.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{listSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Substitute Tracking
	#' @return A dataframe or of ThirdPartyImport
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	getThirdPartyImport <- function(ThirdPartyImportID, ThirdPartyFormatID = F, Request = F, ImportTime = F, ImportData = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, MediaID = F, MediaIDFailedResult = F, ProcessedSubstituteTransactions = F, ProcessedTimeOffTransactions = F, fieldPaths = NULL, entityId = 1, schoolYearId = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment()) %>% purrr::keep(names(.) != "ThirdPartyImportID")

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		getSkyObject(module = "SubstituteTracking", objectName = "ThirdPartyImport", objectId = ThirdPartyImportID, searchFields = searchFields, entityId = entityId, flatten = flatten, returnResponse = returnResponse)
	}

	#' Delete a ThirdPartyImport
	#'
	#' This function deletes a ThirdPartyImport
	#' @param ThirdPartyImportID The ID of the ThirdPartyImport to delete
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{listSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Substitute Tracking
	#' @return The ThirdPartyImportID of the deleted ThirdPartyImport.
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	deleteThirdPartyImport <- function(ThirdPartyImportID, ignoreWarnings = F, entityId = 1, schoolYearId = NULL, flatten = T, returnResponse = F){

		deleteSkyObject(module = "SubstituteTracking", objectName = "ThirdPartyImport", objectId = ThirdPartyImportID, ignoreWarnings = ignoreWarnings, entityId = entityId, flatten = flatten, returnResponse = returnResponse)
	}

	#' Create a ThirdPartyImport
	#'
	#' This function creates a ThirdPartyImport
	#' @param fieldNames The field values to give the created ThirdPartyImport. Each defaults to NULL.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{listSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Substitute Tracking
	#' @return A newly created ThirdPartyImport
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	createThirdPartyImport <- function(ThirdPartyFormatID = NULL, ImportTime = NULL, MediaID = NULL, MediaIDFailedResult = NULL, ProcessedSubstituteTransactions = NULL, ProcessedTimeOffTransactions = NULL, entityId = 1, schoolYearId = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment())

		body <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper()) %>% purrr::compact()

		createSkyObject(module = "SubstituteTracking", objectName = "ThirdPartyImport", body = list(DataObject = body), searchFields = append("ThirdPartyImportID", body %>% names()), entityId = entityId, schoolYearId = schoolYearId, flatten = flatten, returnResponse = returnResponse)
	}

	#' Modify a ThirdPartyImport
	#'
	#' This function modifies a ThirdPartyImport
	#' @param fieldNames The field values to give the modified ThirdPartyImport. Each defaults to NULL.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{listSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Substitute Tracking
	#' @return The modified ThirdPartyImport
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	modifyThirdPartyImport <- function(ThirdPartyImportID, ThirdPartyFormatID = NULL, ImportTime = NULL, MediaID = NULL, MediaIDFailedResult = NULL, ProcessedSubstituteTransactions = NULL, ProcessedTimeOffTransactions = NULL, entityId = 1, schoolYearId = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment())

		body <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper()) %>% purrr::compact()

		modifySkyObject(module = "SubstituteTracking", objectName = "ThirdPartyImport", objectId = ThirdPartyImportID, body = list(DataObject = body), searchFields = append("ThirdPartyImportID", body %>% names()), entityId = entityId, schoolYearId = schoolYearId, flatten = flatten, returnResponse = returnResponse)
	}

	#' List SubstituteTrackingTempAccountDistributions
	#'
	#' This function returns a dataframe or json object of SubstituteTrackingTempAccountDistributions
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given SubstituteTrackingTempAccountDistributions. Defaults to FALSE for all return fields which, for convenience, returns all fields for the SubstituteTrackingTempAccountDistributions.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object listed as text. Run \code{\link{getSchemaForObjects}}('SubstituteTrackingTempAccountDistribution') to get more field paths.
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
	#' @concept Substitute Tracking
	#' @return A list of SubstituteTrackingTempAccountDistributions
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	listSubstituteTrackingTempAccountDistributions <- function(searchConditionsList = NULL, TempAccountDistributionID = F, SubstituteTransactionID = F, AccountID = F, DistributionPercent = F, StateConcordDepartmentTNID = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, fieldPaths = NULL, searchConditionsGroupType = "And", searchSortFieldNamesList = NULL, searchSortFieldNamesDescendingList = NULL, entityId = 1, schoolYearId = NULL, page = 1, pageSize = 100000, flatten = T, returnResponse = F){

		params <- as.list(environment())

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		listSkyObjects(module = "SubstituteTracking", objectName = "TempAccountDistribution", searchFields = searchFields %>% append(fieldPaths), page = page, pageSize = pageSize, SearchConditionsList = searchConditionsList, SearchConditionsGroupType = searchConditionsGroupType, SearchSortFieldNamesList = searchSortFieldNamesList, SearchSortFieldNamesDescendingList = searchSortFieldNamesDescendingList, entityId = entityId, schoolYearId = schoolYearId, flatten = flatten, returnResponse = returnResponse)
	}

	#' Get a SubstituteTrackingTempAccountDistribution
	#'
	#' This function returns a dataframe or json object of a SubstituteTrackingTempAccountDistribution
	#' @param SubstituteTrackingTempAccountDistributionID The ID of the SubstituteTrackingTempAccountDistribution to return.
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given SubstituteTrackingTempAccountDistribution. Defaults to FALSE for all return fields which, for convenience, returns all fields for the SubstituteTrackingTempAccountDistribution.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object listed as text. Run \code{\link{getSchemaForObjects}}('SubstituteTrackingTempAccountDistribution') to get more field paths.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{listSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Substitute Tracking
	#' @return A dataframe or of SubstituteTrackingTempAccountDistribution
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	getSubstituteTrackingTempAccountDistribution <- function(SubstituteTrackingTempAccountDistributionID, TempAccountDistributionID = F, SubstituteTransactionID = F, AccountID = F, DistributionPercent = F, StateConcordDepartmentTNID = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, fieldPaths = NULL, entityId = 1, schoolYearId = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment()) %>% purrr::keep(names(.) != "SubstituteTrackingTempAccountDistributionID")

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		getSkyObject(module = "SubstituteTracking", objectName = "TempAccountDistribution", objectId = SubstituteTrackingTempAccountDistributionID, searchFields = searchFields, entityId = entityId, flatten = flatten, returnResponse = returnResponse)
	}

	#' Delete a SubstituteTrackingTempAccountDistribution
	#'
	#' This function deletes a SubstituteTrackingTempAccountDistribution
	#' @param SubstituteTrackingTempAccountDistributionID The ID of the SubstituteTrackingTempAccountDistribution to delete
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{listSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Substitute Tracking
	#' @return The SubstituteTrackingTempAccountDistributionID of the deleted SubstituteTrackingTempAccountDistribution.
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	deleteSubstituteTrackingTempAccountDistribution <- function(SubstituteTrackingTempAccountDistributionID, ignoreWarnings = F, entityId = 1, schoolYearId = NULL, flatten = T, returnResponse = F){

		deleteSkyObject(module = "SubstituteTracking", objectName = "TempAccountDistribution", objectId = SubstituteTrackingTempAccountDistributionID, ignoreWarnings = ignoreWarnings, entityId = entityId, flatten = flatten, returnResponse = returnResponse)
	}

	#' Create a SubstituteTrackingTempAccountDistribution
	#'
	#' This function creates a SubstituteTrackingTempAccountDistribution
	#' @param fieldNames The field values to give the created SubstituteTrackingTempAccountDistribution. Each defaults to NULL.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{listSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Substitute Tracking
	#' @return A newly created SubstituteTrackingTempAccountDistribution
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	createSubstituteTrackingTempAccountDistribution <- function(SubstituteTransactionID = NULL, AccountID = NULL, DistributionPercent = NULL, StateConcordDepartmentTNID = NULL, entityId = 1, schoolYearId = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment())

		body <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper()) %>% purrr::compact()

		createSkyObject(module = "SubstituteTracking", objectName = "TempAccountDistribution", body = list(DataObject = body), searchFields = append("TempAccountDistributionID", body %>% names()), entityId = entityId, schoolYearId = schoolYearId, flatten = flatten, returnResponse = returnResponse)
	}

	#' Modify a SubstituteTrackingTempAccountDistribution
	#'
	#' This function modifies a SubstituteTrackingTempAccountDistribution
	#' @param fieldNames The field values to give the modified SubstituteTrackingTempAccountDistribution. Each defaults to NULL.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{listSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Substitute Tracking
	#' @return The modified SubstituteTrackingTempAccountDistribution
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	modifySubstituteTrackingTempAccountDistribution <- function(TempAccountDistributionID, SubstituteTransactionID = NULL, AccountID = NULL, DistributionPercent = NULL, StateConcordDepartmentTNID = NULL, entityId = 1, schoolYearId = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment())

		body <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper()) %>% purrr::compact()

		modifySkyObject(module = "SubstituteTracking", objectName = "TempAccountDistribution", objectId = TempAccountDistributionID, body = list(DataObject = body), searchFields = append("TempAccountDistributionID", body %>% names()), entityId = entityId, schoolYearId = schoolYearId, flatten = flatten, returnResponse = returnResponse)
	}

	#' List TempSubstituteTransactionPays
	#'
	#' This function returns a dataframe or json object of TempSubstituteTransactionPays
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given TempSubstituteTransactionPays. Defaults to FALSE for all return fields which, for convenience, returns all fields for the TempSubstituteTransactionPays.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object listed as text. Run \code{\link{getSchemaForObjects}}('TempSubstituteTransactionPay') to get more field paths.
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
	#' @concept Substitute Tracking
	#' @return A list of TempSubstituteTransactionPays
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	listTempSubstituteTransactionPays <- function(searchConditionsList = NULL, TempSubstituteTransactionPayID = F, SubstituteTransactionPayID = F, SubstituteTransactionID = F, PayScaleClassID = F, SubstituteName = F, PayScaleCodeDescription = F, PayScaleClassDescription = F, StartDateTime = F, Duration = F, FormattedDuration = F, AbsentEmployeeName = F, LifeTermUnitCounter = F, LongTermUnitCounter = F, Rate = F, Factor = F, PayAmount = F, Type = F, OverrideLifeTermUnitCounter = F, OverrideLongTermUnitCounter = F, OverridePayScaleClass = F, OverrideRate = F, OverrideFactor = F, TimesheetAssignmentPayTypeID = F, TimesheetAssignmentDetailID = F, TimesheetFactor = F, TimesheetRate = F, TimesheetHoursWorked = F, TimesheetWorkStartDate = F, TimesheetWorkEndDate = F, TimesheetCorrectiveAssignmentPayTypeID = F, TimesheetCorrectiveAssignmentDetailID = F, TimesheetCorrectiveFactor = F, TimesheetCorrectiveRate = F, TimesheetCorrectiveHoursWorked = F, TimesheetCorrectiveWorkStartDate = F, TimesheetCorrectiveWorkEndDate = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, IsRetro = F, RetroAmount = F, MovedToHistoryWithoutPayroll = F, SubstituteEmployeeNumber = F, AbsentEmployeeNumber = F, MovedToHistoryFromNetZeroCorrection = F, fieldPaths = NULL, searchConditionsGroupType = "And", searchSortFieldNamesList = NULL, searchSortFieldNamesDescendingList = NULL, entityId = 1, schoolYearId = NULL, page = 1, pageSize = 100000, flatten = T, returnResponse = F){

		params <- as.list(environment())

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		listSkyObjects(module = "SubstituteTracking", objectName = "TempSubstituteTransactionPay", searchFields = searchFields %>% append(fieldPaths), page = page, pageSize = pageSize, SearchConditionsList = searchConditionsList, SearchConditionsGroupType = searchConditionsGroupType, SearchSortFieldNamesList = searchSortFieldNamesList, SearchSortFieldNamesDescendingList = searchSortFieldNamesDescendingList, entityId = entityId, schoolYearId = schoolYearId, flatten = flatten, returnResponse = returnResponse)
	}

	#' Get a TempSubstituteTransactionPay
	#'
	#' This function returns a dataframe or json object of a TempSubstituteTransactionPay
	#' @param TempSubstituteTransactionPayID The ID of the TempSubstituteTransactionPay to return.
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given TempSubstituteTransactionPay. Defaults to FALSE for all return fields which, for convenience, returns all fields for the TempSubstituteTransactionPay.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object listed as text. Run \code{\link{getSchemaForObjects}}('TempSubstituteTransactionPay') to get more field paths.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{listSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Substitute Tracking
	#' @return A dataframe or of TempSubstituteTransactionPay
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	getTempSubstituteTransactionPay <- function(TempSubstituteTransactionPayID, SubstituteTransactionPayID = F, SubstituteTransactionID = F, PayScaleClassID = F, SubstituteName = F, PayScaleCodeDescription = F, PayScaleClassDescription = F, StartDateTime = F, Duration = F, FormattedDuration = F, AbsentEmployeeName = F, LifeTermUnitCounter = F, LongTermUnitCounter = F, Rate = F, Factor = F, PayAmount = F, Type = F, OverrideLifeTermUnitCounter = F, OverrideLongTermUnitCounter = F, OverridePayScaleClass = F, OverrideRate = F, OverrideFactor = F, TimesheetAssignmentPayTypeID = F, TimesheetAssignmentDetailID = F, TimesheetFactor = F, TimesheetRate = F, TimesheetHoursWorked = F, TimesheetWorkStartDate = F, TimesheetWorkEndDate = F, TimesheetCorrectiveAssignmentPayTypeID = F, TimesheetCorrectiveAssignmentDetailID = F, TimesheetCorrectiveFactor = F, TimesheetCorrectiveRate = F, TimesheetCorrectiveHoursWorked = F, TimesheetCorrectiveWorkStartDate = F, TimesheetCorrectiveWorkEndDate = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, IsRetro = F, RetroAmount = F, MovedToHistoryWithoutPayroll = F, SubstituteEmployeeNumber = F, AbsentEmployeeNumber = F, MovedToHistoryFromNetZeroCorrection = F, fieldPaths = NULL, entityId = 1, schoolYearId = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment()) %>% purrr::keep(names(.) != "TempSubstituteTransactionPayID")

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		getSkyObject(module = "SubstituteTracking", objectName = "TempSubstituteTransactionPay", objectId = TempSubstituteTransactionPayID, searchFields = searchFields, entityId = entityId, flatten = flatten, returnResponse = returnResponse)
	}

	#' Delete a TempSubstituteTransactionPay
	#'
	#' This function deletes a TempSubstituteTransactionPay
	#' @param TempSubstituteTransactionPayID The ID of the TempSubstituteTransactionPay to delete
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{listSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Substitute Tracking
	#' @return The TempSubstituteTransactionPayID of the deleted TempSubstituteTransactionPay.
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	deleteTempSubstituteTransactionPay <- function(TempSubstituteTransactionPayID, ignoreWarnings = F, entityId = 1, schoolYearId = NULL, flatten = T, returnResponse = F){

		deleteSkyObject(module = "SubstituteTracking", objectName = "TempSubstituteTransactionPay", objectId = TempSubstituteTransactionPayID, ignoreWarnings = ignoreWarnings, entityId = entityId, flatten = flatten, returnResponse = returnResponse)
	}

	#' Create a TempSubstituteTransactionPay
	#'
	#' This function creates a TempSubstituteTransactionPay
	#' @param fieldNames The field values to give the created TempSubstituteTransactionPay. Each defaults to NULL.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{listSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Substitute Tracking
	#' @return A newly created TempSubstituteTransactionPay
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	createTempSubstituteTransactionPay <- function(SubstituteTransactionPayID = NULL, SubstituteTransactionID = NULL, PayScaleClassID = NULL, SubstituteName = NULL, PayScaleCodeDescription = NULL, PayScaleClassDescription = NULL, StartDateTime = NULL, AbsentEmployeeName = NULL, LifeTermUnitCounter = NULL, LongTermUnitCounter = NULL, Rate = NULL, Factor = NULL, Type = NULL, OverrideLifeTermUnitCounter = NULL, OverrideLongTermUnitCounter = NULL, OverridePayScaleClass = NULL, OverrideRate = NULL, OverrideFactor = NULL, TimesheetAssignmentPayTypeID = NULL, TimesheetAssignmentDetailID = NULL, TimesheetFactor = NULL, TimesheetRate = NULL, TimesheetHoursWorked = NULL, TimesheetWorkStartDate = NULL, TimesheetWorkEndDate = NULL, TimesheetCorrectiveAssignmentPayTypeID = NULL, TimesheetCorrectiveAssignmentDetailID = NULL, TimesheetCorrectiveFactor = NULL, TimesheetCorrectiveRate = NULL, TimesheetCorrectiveHoursWorked = NULL, TimesheetCorrectiveWorkStartDate = NULL, TimesheetCorrectiveWorkEndDate = NULL, IsRetro = NULL, RetroAmount = NULL, MovedToHistoryWithoutPayroll = NULL, SubstituteEmployeeNumber = NULL, AbsentEmployeeNumber = NULL, MovedToHistoryFromNetZeroCorrection = NULL, entityId = 1, schoolYearId = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment())

		body <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper()) %>% purrr::compact()

		createSkyObject(module = "SubstituteTracking", objectName = "TempSubstituteTransactionPay", body = list(DataObject = body), searchFields = append("TempSubstituteTransactionPayID", body %>% names()), entityId = entityId, schoolYearId = schoolYearId, flatten = flatten, returnResponse = returnResponse)
	}

	#' Modify a TempSubstituteTransactionPay
	#'
	#' This function modifies a TempSubstituteTransactionPay
	#' @param fieldNames The field values to give the modified TempSubstituteTransactionPay. Each defaults to NULL.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{listSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Substitute Tracking
	#' @return The modified TempSubstituteTransactionPay
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	modifyTempSubstituteTransactionPay <- function(TempSubstituteTransactionPayID, SubstituteTransactionPayID = NULL, SubstituteTransactionID = NULL, PayScaleClassID = NULL, SubstituteName = NULL, PayScaleCodeDescription = NULL, PayScaleClassDescription = NULL, StartDateTime = NULL, AbsentEmployeeName = NULL, LifeTermUnitCounter = NULL, LongTermUnitCounter = NULL, Rate = NULL, Factor = NULL, Type = NULL, OverrideLifeTermUnitCounter = NULL, OverrideLongTermUnitCounter = NULL, OverridePayScaleClass = NULL, OverrideRate = NULL, OverrideFactor = NULL, TimesheetAssignmentPayTypeID = NULL, TimesheetAssignmentDetailID = NULL, TimesheetFactor = NULL, TimesheetRate = NULL, TimesheetHoursWorked = NULL, TimesheetWorkStartDate = NULL, TimesheetWorkEndDate = NULL, TimesheetCorrectiveAssignmentPayTypeID = NULL, TimesheetCorrectiveAssignmentDetailID = NULL, TimesheetCorrectiveFactor = NULL, TimesheetCorrectiveRate = NULL, TimesheetCorrectiveHoursWorked = NULL, TimesheetCorrectiveWorkStartDate = NULL, TimesheetCorrectiveWorkEndDate = NULL, IsRetro = NULL, RetroAmount = NULL, MovedToHistoryWithoutPayroll = NULL, SubstituteEmployeeNumber = NULL, AbsentEmployeeNumber = NULL, MovedToHistoryFromNetZeroCorrection = NULL, entityId = 1, schoolYearId = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment())

		body <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper()) %>% purrr::compact()

		modifySkyObject(module = "SubstituteTracking", objectName = "TempSubstituteTransactionPay", objectId = TempSubstituteTransactionPayID, body = list(DataObject = body), searchFields = append("TempSubstituteTransactionPayID", body %>% names()), entityId = entityId, schoolYearId = schoolYearId, flatten = flatten, returnResponse = returnResponse)
	}

	#' List ThirdPartyFormats
	#'
	#' This function returns a dataframe or json object of ThirdPartyFormats
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given ThirdPartyFormats. Defaults to FALSE for all return fields which, for convenience, returns all fields for the ThirdPartyFormats.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object listed as text. Run \code{\link{getSchemaForObjects}}('ThirdPartyFormat') to get more field paths.
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
	#' @concept Substitute Tracking
	#' @return A list of ThirdPartyFormats
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	listThirdPartyFormats <- function(searchConditionsList = NULL, ThirdPartyFormatID = F, SkywardID = F, DistrictID = F, Description = F, EmployeeIdentification = F, ImportType = F, ProcessSubstituteTransactions = F, ProcessTimeOffTransactions = F, IsSystemLoaded = F, SkywardIDClonedFrom = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, SkywardHash = F, IsActive = F, DateFormat = F, AbsentDurationType = F, fieldPaths = NULL, searchConditionsGroupType = "And", searchSortFieldNamesList = NULL, searchSortFieldNamesDescendingList = NULL, entityId = 1, schoolYearId = NULL, page = 1, pageSize = 100000, flatten = T, returnResponse = F){

		params <- as.list(environment())

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		listSkyObjects(module = "SubstituteTracking", objectName = "ThirdPartyFormat", searchFields = searchFields %>% append(fieldPaths), page = page, pageSize = pageSize, SearchConditionsList = searchConditionsList, SearchConditionsGroupType = searchConditionsGroupType, SearchSortFieldNamesList = searchSortFieldNamesList, SearchSortFieldNamesDescendingList = searchSortFieldNamesDescendingList, entityId = entityId, schoolYearId = schoolYearId, flatten = flatten, returnResponse = returnResponse)
	}

	#' Get a ThirdPartyFormat
	#'
	#' This function returns a dataframe or json object of a ThirdPartyFormat
	#' @param ThirdPartyFormatID The ID of the ThirdPartyFormat to return.
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given ThirdPartyFormat. Defaults to FALSE for all return fields which, for convenience, returns all fields for the ThirdPartyFormat.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object listed as text. Run \code{\link{getSchemaForObjects}}('ThirdPartyFormat') to get more field paths.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{listSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Substitute Tracking
	#' @return A dataframe or of ThirdPartyFormat
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	getThirdPartyFormat <- function(ThirdPartyFormatID, SkywardID = F, DistrictID = F, Description = F, EmployeeIdentification = F, ImportType = F, ProcessSubstituteTransactions = F, ProcessTimeOffTransactions = F, IsSystemLoaded = F, SkywardIDClonedFrom = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, SkywardHash = F, IsActive = F, DateFormat = F, AbsentDurationType = F, fieldPaths = NULL, entityId = 1, schoolYearId = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment()) %>% purrr::keep(names(.) != "ThirdPartyFormatID")

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		getSkyObject(module = "SubstituteTracking", objectName = "ThirdPartyFormat", objectId = ThirdPartyFormatID, searchFields = searchFields, entityId = entityId, flatten = flatten, returnResponse = returnResponse)
	}

	#' Delete a ThirdPartyFormat
	#'
	#' This function deletes a ThirdPartyFormat
	#' @param ThirdPartyFormatID The ID of the ThirdPartyFormat to delete
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{listSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Substitute Tracking
	#' @return The ThirdPartyFormatID of the deleted ThirdPartyFormat.
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	deleteThirdPartyFormat <- function(ThirdPartyFormatID, ignoreWarnings = F, entityId = 1, schoolYearId = NULL, flatten = T, returnResponse = F){

		deleteSkyObject(module = "SubstituteTracking", objectName = "ThirdPartyFormat", objectId = ThirdPartyFormatID, ignoreWarnings = ignoreWarnings, entityId = entityId, flatten = flatten, returnResponse = returnResponse)
	}

	#' Create a ThirdPartyFormat
	#'
	#' This function creates a ThirdPartyFormat
	#' @param fieldNames The field values to give the created ThirdPartyFormat. Each defaults to NULL.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{listSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Substitute Tracking
	#' @return A newly created ThirdPartyFormat
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	createThirdPartyFormat <- function(DistrictID = NULL, Description = NULL, EmployeeIdentification = NULL, ImportType = NULL, ProcessSubstituteTransactions = NULL, ProcessTimeOffTransactions = NULL, SkywardIDClonedFrom = NULL, IsActive = NULL, DateFormat = NULL, AbsentDurationType = NULL, entityId = 1, schoolYearId = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment())

		body <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper()) %>% purrr::compact()

		createSkyObject(module = "SubstituteTracking", objectName = "ThirdPartyFormat", body = list(DataObject = body), searchFields = append("ThirdPartyFormatID", body %>% names()), entityId = entityId, schoolYearId = schoolYearId, flatten = flatten, returnResponse = returnResponse)
	}

	#' Modify a ThirdPartyFormat
	#'
	#' This function modifies a ThirdPartyFormat
	#' @param fieldNames The field values to give the modified ThirdPartyFormat. Each defaults to NULL.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{listSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Substitute Tracking
	#' @return The modified ThirdPartyFormat
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	modifyThirdPartyFormat <- function(ThirdPartyFormatID, DistrictID = NULL, Description = NULL, EmployeeIdentification = NULL, ImportType = NULL, ProcessSubstituteTransactions = NULL, ProcessTimeOffTransactions = NULL, SkywardIDClonedFrom = NULL, IsActive = NULL, DateFormat = NULL, AbsentDurationType = NULL, entityId = 1, schoolYearId = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment())

		body <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper()) %>% purrr::compact()

		modifySkyObject(module = "SubstituteTracking", objectName = "ThirdPartyFormat", objectId = ThirdPartyFormatID, body = list(DataObject = body), searchFields = append("ThirdPartyFormatID", body %>% names()), entityId = entityId, schoolYearId = schoolYearId, flatten = flatten, returnResponse = returnResponse)
	}

	#' List SubstitutePayScaleClasses
	#'
	#' This function returns a dataframe or json object of SubstitutePayScaleClasses
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given SubstitutePayScaleClasses. Defaults to FALSE for all return fields which, for convenience, returns all fields for the SubstitutePayScaleClasses.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object listed as text. Run \code{\link{getSchemaForObjects}}('SubstitutePayScaleClass') to get more field paths.
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
	#' @concept Substitute Tracking
	#' @return A list of SubstitutePayScaleClasses
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	listSubstitutePayScaleClasses <- function(searchConditionsList = NULL, SubstitutePayScaleClassID = F, SubstitutePayScaleID = F, PayScaleClassID = F, StartDate = F, EndDate = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, SubstitutePayScaleClassIDClonedFrom = F, fieldPaths = NULL, searchConditionsGroupType = "And", searchSortFieldNamesList = NULL, searchSortFieldNamesDescendingList = NULL, entityId = 1, schoolYearId = NULL, page = 1, pageSize = 100000, flatten = T, returnResponse = F){

		params <- as.list(environment())

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		listSkyObjects(module = "SubstituteTracking", objectName = "SubstitutePayScaleClass", searchFields = searchFields %>% append(fieldPaths), page = page, pageSize = pageSize, SearchConditionsList = searchConditionsList, SearchConditionsGroupType = searchConditionsGroupType, SearchSortFieldNamesList = searchSortFieldNamesList, SearchSortFieldNamesDescendingList = searchSortFieldNamesDescendingList, entityId = entityId, schoolYearId = schoolYearId, flatten = flatten, returnResponse = returnResponse)
	}

	#' Get a SubstitutePayScaleClass
	#'
	#' This function returns a dataframe or json object of a SubstitutePayScaleClass
	#' @param SubstitutePayScaleClassID The ID of the SubstitutePayScaleClass to return.
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given SubstitutePayScaleClass. Defaults to FALSE for all return fields which, for convenience, returns all fields for the SubstitutePayScaleClass.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object listed as text. Run \code{\link{getSchemaForObjects}}('SubstitutePayScaleClass') to get more field paths.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{listSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Substitute Tracking
	#' @return A dataframe or of SubstitutePayScaleClass
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	getSubstitutePayScaleClass <- function(SubstitutePayScaleClassID, SubstitutePayScaleID = F, PayScaleClassID = F, StartDate = F, EndDate = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, SubstitutePayScaleClassIDClonedFrom = F, fieldPaths = NULL, entityId = 1, schoolYearId = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment()) %>% purrr::keep(names(.) != "SubstitutePayScaleClassID")

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		getSkyObject(module = "SubstituteTracking", objectName = "SubstitutePayScaleClass", objectId = SubstitutePayScaleClassID, searchFields = searchFields, entityId = entityId, flatten = flatten, returnResponse = returnResponse)
	}

	#' Delete a SubstitutePayScaleClass
	#'
	#' This function deletes a SubstitutePayScaleClass
	#' @param SubstitutePayScaleClassID The ID of the SubstitutePayScaleClass to delete
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{listSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Substitute Tracking
	#' @return The SubstitutePayScaleClassID of the deleted SubstitutePayScaleClass.
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	deleteSubstitutePayScaleClass <- function(SubstitutePayScaleClassID, ignoreWarnings = F, entityId = 1, schoolYearId = NULL, flatten = T, returnResponse = F){

		deleteSkyObject(module = "SubstituteTracking", objectName = "SubstitutePayScaleClass", objectId = SubstitutePayScaleClassID, ignoreWarnings = ignoreWarnings, entityId = entityId, flatten = flatten, returnResponse = returnResponse)
	}

	#' Create a SubstitutePayScaleClass
	#'
	#' This function creates a SubstitutePayScaleClass
	#' @param fieldNames The field values to give the created SubstitutePayScaleClass. Each defaults to NULL.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{listSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Substitute Tracking
	#' @return A newly created SubstitutePayScaleClass
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	createSubstitutePayScaleClass <- function(SubstitutePayScaleID = NULL, PayScaleClassID = NULL, StartDate = NULL, EndDate = NULL, SubstitutePayScaleClassIDClonedFrom = NULL, entityId = 1, schoolYearId = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment())

		body <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper()) %>% purrr::compact()

		createSkyObject(module = "SubstituteTracking", objectName = "SubstitutePayScaleClass", body = list(DataObject = body), searchFields = append("SubstitutePayScaleClassID", body %>% names()), entityId = entityId, schoolYearId = schoolYearId, flatten = flatten, returnResponse = returnResponse)
	}

	#' Modify a SubstitutePayScaleClass
	#'
	#' This function modifies a SubstitutePayScaleClass
	#' @param fieldNames The field values to give the modified SubstitutePayScaleClass. Each defaults to NULL.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{listSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Substitute Tracking
	#' @return The modified SubstitutePayScaleClass
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	modifySubstitutePayScaleClass <- function(SubstitutePayScaleClassID, SubstitutePayScaleID = NULL, PayScaleClassID = NULL, StartDate = NULL, EndDate = NULL, SubstitutePayScaleClassIDClonedFrom = NULL, entityId = 1, schoolYearId = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment())

		body <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper()) %>% purrr::compact()

		modifySkyObject(module = "SubstituteTracking", objectName = "SubstitutePayScaleClass", objectId = SubstitutePayScaleClassID, body = list(DataObject = body), searchFields = append("SubstitutePayScaleClassID", body %>% names()), entityId = entityId, schoolYearId = schoolYearId, flatten = flatten, returnResponse = returnResponse)
	}

	#' List SubstitutePayScales
	#'
	#' This function returns a dataframe or json object of SubstitutePayScales
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given SubstitutePayScales. Defaults to FALSE for all return fields which, for convenience, returns all fields for the SubstitutePayScales.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object listed as text. Run \code{\link{getSchemaForObjects}}('SubstitutePayScale') to get more field paths.
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
	#' @concept Substitute Tracking
	#' @return A list of SubstitutePayScales
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	listSubstitutePayScales <- function(searchConditionsList = NULL, SubstitutePayScaleID = F, EmployeeID = F, PayScaleID = F, AssignmentPayTypeID = F, SubstitutePayScaleIdentifier = F, AttachmentCount = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, SkipImport = F, StartingLifeTermUnitCounter = F, SubstitutePayScaleIDClonedFrom = F, fieldPaths = NULL, searchConditionsGroupType = "And", searchSortFieldNamesList = NULL, searchSortFieldNamesDescendingList = NULL, entityId = 1, schoolYearId = NULL, page = 1, pageSize = 100000, flatten = T, returnResponse = F){

		params <- as.list(environment())

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		listSkyObjects(module = "SubstituteTracking", objectName = "SubstitutePayScale", searchFields = searchFields %>% append(fieldPaths), page = page, pageSize = pageSize, SearchConditionsList = searchConditionsList, SearchConditionsGroupType = searchConditionsGroupType, SearchSortFieldNamesList = searchSortFieldNamesList, SearchSortFieldNamesDescendingList = searchSortFieldNamesDescendingList, entityId = entityId, schoolYearId = schoolYearId, flatten = flatten, returnResponse = returnResponse)
	}

	#' Get a SubstitutePayScale
	#'
	#' This function returns a dataframe or json object of a SubstitutePayScale
	#' @param SubstitutePayScaleID The ID of the SubstitutePayScale to return.
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given SubstitutePayScale. Defaults to FALSE for all return fields which, for convenience, returns all fields for the SubstitutePayScale.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object listed as text. Run \code{\link{getSchemaForObjects}}('SubstitutePayScale') to get more field paths.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{listSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Substitute Tracking
	#' @return A dataframe or of SubstitutePayScale
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	getSubstitutePayScale <- function(SubstitutePayScaleID, EmployeeID = F, PayScaleID = F, AssignmentPayTypeID = F, SubstitutePayScaleIdentifier = F, AttachmentCount = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, SkipImport = F, StartingLifeTermUnitCounter = F, SubstitutePayScaleIDClonedFrom = F, fieldPaths = NULL, entityId = 1, schoolYearId = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment()) %>% purrr::keep(names(.) != "SubstitutePayScaleID")

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		getSkyObject(module = "SubstituteTracking", objectName = "SubstitutePayScale", objectId = SubstitutePayScaleID, searchFields = searchFields, entityId = entityId, flatten = flatten, returnResponse = returnResponse)
	}

	#' Delete a SubstitutePayScale
	#'
	#' This function deletes a SubstitutePayScale
	#' @param SubstitutePayScaleID The ID of the SubstitutePayScale to delete
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{listSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Substitute Tracking
	#' @return The SubstitutePayScaleID of the deleted SubstitutePayScale.
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	deleteSubstitutePayScale <- function(SubstitutePayScaleID, ignoreWarnings = F, entityId = 1, schoolYearId = NULL, flatten = T, returnResponse = F){

		deleteSkyObject(module = "SubstituteTracking", objectName = "SubstitutePayScale", objectId = SubstitutePayScaleID, ignoreWarnings = ignoreWarnings, entityId = entityId, flatten = flatten, returnResponse = returnResponse)
	}

	#' Create a SubstitutePayScale
	#'
	#' This function creates a SubstitutePayScale
	#' @param fieldNames The field values to give the created SubstitutePayScale. Each defaults to NULL.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{listSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Substitute Tracking
	#' @return A newly created SubstitutePayScale
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	createSubstitutePayScale <- function(EmployeeID = NULL, PayScaleID = NULL, AssignmentPayTypeID = NULL, SkipImport = NULL, StartingLifeTermUnitCounter = NULL, SubstitutePayScaleIDClonedFrom = NULL, entityId = 1, schoolYearId = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment())

		body <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper()) %>% purrr::compact()

		createSkyObject(module = "SubstituteTracking", objectName = "SubstitutePayScale", body = list(DataObject = body), searchFields = append("SubstitutePayScaleID", body %>% names()), entityId = entityId, schoolYearId = schoolYearId, flatten = flatten, returnResponse = returnResponse)
	}

	#' Modify a SubstitutePayScale
	#'
	#' This function modifies a SubstitutePayScale
	#' @param fieldNames The field values to give the modified SubstitutePayScale. Each defaults to NULL.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{listSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Substitute Tracking
	#' @return The modified SubstitutePayScale
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	modifySubstitutePayScale <- function(SubstitutePayScaleID, EmployeeID = NULL, PayScaleID = NULL, AssignmentPayTypeID = NULL, SkipImport = NULL, StartingLifeTermUnitCounter = NULL, SubstitutePayScaleIDClonedFrom = NULL, entityId = 1, schoolYearId = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment())

		body <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper()) %>% purrr::compact()

		modifySkyObject(module = "SubstituteTracking", objectName = "SubstitutePayScale", objectId = SubstitutePayScaleID, body = list(DataObject = body), searchFields = append("SubstitutePayScaleID", body %>% names()), entityId = entityId, schoolYearId = schoolYearId, flatten = flatten, returnResponse = returnResponse)
	}

	#' List PayScales
	#'
	#' This function returns a dataframe or json object of PayScales
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given PayScales. Defaults to FALSE for all return fields which, for convenience, returns all fields for the PayScales.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object listed as text. Run \code{\link{getSchemaForObjects}}('PayScale') to get more field paths.
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
	#' @concept Substitute Tracking
	#' @return A list of PayScales
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	listPayScales <- function(searchConditionsList = NULL, PayScaleID = F, DistrictID = F, FiscalYearID = F, PayScheduleID = F, Code = F, Description = F, DailyCounterType = F, RateType = F, CodeDescription = F, PayScaleIDClonedFrom = F, SecondsPerDay = F, CalendarID = F, AttachmentCount = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, fieldPaths = NULL, searchConditionsGroupType = "And", searchSortFieldNamesList = NULL, searchSortFieldNamesDescendingList = NULL, entityId = 1, schoolYearId = NULL, page = 1, pageSize = 100000, flatten = T, returnResponse = F){

		params <- as.list(environment())

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		listSkyObjects(module = "SubstituteTracking", objectName = "PayScale", searchFields = searchFields %>% append(fieldPaths), page = page, pageSize = pageSize, SearchConditionsList = searchConditionsList, SearchConditionsGroupType = searchConditionsGroupType, SearchSortFieldNamesList = searchSortFieldNamesList, SearchSortFieldNamesDescendingList = searchSortFieldNamesDescendingList, entityId = entityId, schoolYearId = schoolYearId, flatten = flatten, returnResponse = returnResponse)
	}

	#' Get a PayScale
	#'
	#' This function returns a dataframe or json object of a PayScale
	#' @param PayScaleID The ID of the PayScale to return.
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given PayScale. Defaults to FALSE for all return fields which, for convenience, returns all fields for the PayScale.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object listed as text. Run \code{\link{getSchemaForObjects}}('PayScale') to get more field paths.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{listSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Substitute Tracking
	#' @return A dataframe or of PayScale
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	getPayScale <- function(PayScaleID, DistrictID = F, FiscalYearID = F, PayScheduleID = F, Code = F, Description = F, DailyCounterType = F, RateType = F, CodeDescription = F, PayScaleIDClonedFrom = F, SecondsPerDay = F, CalendarID = F, AttachmentCount = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, fieldPaths = NULL, entityId = 1, schoolYearId = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment()) %>% purrr::keep(names(.) != "PayScaleID")

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		getSkyObject(module = "SubstituteTracking", objectName = "PayScale", objectId = PayScaleID, searchFields = searchFields, entityId = entityId, flatten = flatten, returnResponse = returnResponse)
	}

	#' Delete a PayScale
	#'
	#' This function deletes a PayScale
	#' @param PayScaleID The ID of the PayScale to delete
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{listSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Substitute Tracking
	#' @return The PayScaleID of the deleted PayScale.
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	deletePayScale <- function(PayScaleID, ignoreWarnings = F, entityId = 1, schoolYearId = NULL, flatten = T, returnResponse = F){

		deleteSkyObject(module = "SubstituteTracking", objectName = "PayScale", objectId = PayScaleID, ignoreWarnings = ignoreWarnings, entityId = entityId, flatten = flatten, returnResponse = returnResponse)
	}

	#' Create a PayScale
	#'
	#' This function creates a PayScale
	#' @param fieldNames The field values to give the created PayScale. Each defaults to NULL.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{listSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Substitute Tracking
	#' @return A newly created PayScale
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	createPayScale <- function(DistrictID = NULL, FiscalYearID = NULL, PayScheduleID = NULL, Code = NULL, Description = NULL, DailyCounterType = NULL, RateType = NULL, PayScaleIDClonedFrom = NULL, SecondsPerDay = NULL, CalendarID = NULL, entityId = 1, schoolYearId = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment())

		body <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper()) %>% purrr::compact()

		createSkyObject(module = "SubstituteTracking", objectName = "PayScale", body = list(DataObject = body), searchFields = append("PayScaleID", body %>% names()), entityId = entityId, schoolYearId = schoolYearId, flatten = flatten, returnResponse = returnResponse)
	}

	#' Modify a PayScale
	#'
	#' This function modifies a PayScale
	#' @param fieldNames The field values to give the modified PayScale. Each defaults to NULL.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{listSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Substitute Tracking
	#' @return The modified PayScale
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	modifyPayScale <- function(PayScaleID, DistrictID = NULL, FiscalYearID = NULL, PayScheduleID = NULL, Code = NULL, Description = NULL, DailyCounterType = NULL, RateType = NULL, PayScaleIDClonedFrom = NULL, SecondsPerDay = NULL, CalendarID = NULL, entityId = 1, schoolYearId = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment())

		body <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper()) %>% purrr::compact()

		modifySkyObject(module = "SubstituteTracking", objectName = "PayScale", objectId = PayScaleID, body = list(DataObject = body), searchFields = append("PayScaleID", body %>% names()), entityId = entityId, schoolYearId = schoolYearId, flatten = flatten, returnResponse = returnResponse)
	}

	#' List PayScaleClasses
	#'
	#' This function returns a dataframe or json object of PayScaleClasses
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given PayScaleClasses. Defaults to FALSE for all return fields which, for convenience, returns all fields for the PayScaleClasses.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object listed as text. Run \code{\link{getSchemaForObjects}}('PayScaleClass') to get more field paths.
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
	#' @concept Substitute Tracking
	#' @return A list of PayScaleClasses
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	listPayScaleClasses <- function(searchConditionsList = NULL, PayScaleClassID = F, PayScaleID = F, Description = F, PayScaleClassIDClonedFrom = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, MaskIDExpense = F, UseLongTermRetroBasePay = F, UseLifeTermRetroBasePay = F, ApplyMaskImportFile = F, ApplyMaskAbsentPay = F, ApplyMaskAbsentPosition = F, ApplyMaskBuilding = F, ApplyMaskSubstituteAssignmentPayType = F, fieldPaths = NULL, searchConditionsGroupType = "And", searchSortFieldNamesList = NULL, searchSortFieldNamesDescendingList = NULL, entityId = 1, schoolYearId = NULL, page = 1, pageSize = 100000, flatten = T, returnResponse = F){

		params <- as.list(environment())

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		listSkyObjects(module = "SubstituteTracking", objectName = "PayScaleClass", searchFields = searchFields %>% append(fieldPaths), page = page, pageSize = pageSize, SearchConditionsList = searchConditionsList, SearchConditionsGroupType = searchConditionsGroupType, SearchSortFieldNamesList = searchSortFieldNamesList, SearchSortFieldNamesDescendingList = searchSortFieldNamesDescendingList, entityId = entityId, schoolYearId = schoolYearId, flatten = flatten, returnResponse = returnResponse)
	}

	#' Get a PayScaleClass
	#'
	#' This function returns a dataframe or json object of a PayScaleClass
	#' @param PayScaleClassID The ID of the PayScaleClass to return.
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given PayScaleClass. Defaults to FALSE for all return fields which, for convenience, returns all fields for the PayScaleClass.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object listed as text. Run \code{\link{getSchemaForObjects}}('PayScaleClass') to get more field paths.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{listSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Substitute Tracking
	#' @return A dataframe or of PayScaleClass
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	getPayScaleClass <- function(PayScaleClassID, PayScaleID = F, Description = F, PayScaleClassIDClonedFrom = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, MaskIDExpense = F, UseLongTermRetroBasePay = F, UseLifeTermRetroBasePay = F, ApplyMaskImportFile = F, ApplyMaskAbsentPay = F, ApplyMaskAbsentPosition = F, ApplyMaskBuilding = F, ApplyMaskSubstituteAssignmentPayType = F, fieldPaths = NULL, entityId = 1, schoolYearId = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment()) %>% purrr::keep(names(.) != "PayScaleClassID")

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		getSkyObject(module = "SubstituteTracking", objectName = "PayScaleClass", objectId = PayScaleClassID, searchFields = searchFields, entityId = entityId, flatten = flatten, returnResponse = returnResponse)
	}

	#' Delete a PayScaleClass
	#'
	#' This function deletes a PayScaleClass
	#' @param PayScaleClassID The ID of the PayScaleClass to delete
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{listSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Substitute Tracking
	#' @return The PayScaleClassID of the deleted PayScaleClass.
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	deletePayScaleClass <- function(PayScaleClassID, ignoreWarnings = F, entityId = 1, schoolYearId = NULL, flatten = T, returnResponse = F){

		deleteSkyObject(module = "SubstituteTracking", objectName = "PayScaleClass", objectId = PayScaleClassID, ignoreWarnings = ignoreWarnings, entityId = entityId, flatten = flatten, returnResponse = returnResponse)
	}

	#' Create a PayScaleClass
	#'
	#' This function creates a PayScaleClass
	#' @param fieldNames The field values to give the created PayScaleClass. Each defaults to NULL.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{listSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Substitute Tracking
	#' @return A newly created PayScaleClass
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	createPayScaleClass <- function(PayScaleID = NULL, Description = NULL, PayScaleClassIDClonedFrom = NULL, MaskIDExpense = NULL, UseLongTermRetroBasePay = NULL, UseLifeTermRetroBasePay = NULL, ApplyMaskImportFile = NULL, ApplyMaskAbsentPay = NULL, ApplyMaskAbsentPosition = NULL, ApplyMaskBuilding = NULL, ApplyMaskSubstituteAssignmentPayType = NULL, entityId = 1, schoolYearId = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment())

		body <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper()) %>% purrr::compact()

		createSkyObject(module = "SubstituteTracking", objectName = "PayScaleClass", body = list(DataObject = body), searchFields = append("PayScaleClassID", body %>% names()), entityId = entityId, schoolYearId = schoolYearId, flatten = flatten, returnResponse = returnResponse)
	}

	#' Modify a PayScaleClass
	#'
	#' This function modifies a PayScaleClass
	#' @param fieldNames The field values to give the modified PayScaleClass. Each defaults to NULL.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{listSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Substitute Tracking
	#' @return The modified PayScaleClass
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	modifyPayScaleClass <- function(PayScaleClassID, PayScaleID = NULL, Description = NULL, PayScaleClassIDClonedFrom = NULL, MaskIDExpense = NULL, UseLongTermRetroBasePay = NULL, UseLifeTermRetroBasePay = NULL, ApplyMaskImportFile = NULL, ApplyMaskAbsentPay = NULL, ApplyMaskAbsentPosition = NULL, ApplyMaskBuilding = NULL, ApplyMaskSubstituteAssignmentPayType = NULL, entityId = 1, schoolYearId = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment())

		body <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper()) %>% purrr::compact()

		modifySkyObject(module = "SubstituteTracking", objectName = "PayScaleClass", objectId = PayScaleClassID, body = list(DataObject = body), searchFields = append("PayScaleClassID", body %>% names()), entityId = entityId, schoolYearId = schoolYearId, flatten = flatten, returnResponse = returnResponse)
	}

	#' List LifeTerms
	#'
	#' This function returns a dataframe or json object of LifeTerms
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given LifeTerms. Defaults to FALSE for all return fields which, for convenience, returns all fields for the LifeTerms.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object listed as text. Run \code{\link{getSchemaForObjects}}('LifeTerm') to get more field paths.
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
	#' @concept Substitute Tracking
	#' @return A list of LifeTerms
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	listLifeTerms <- function(searchConditionsList = NULL, LifeTermID = F, PayScaleClassID = F, FirstLifeTermUnit = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, fieldPaths = NULL, searchConditionsGroupType = "And", searchSortFieldNamesList = NULL, searchSortFieldNamesDescendingList = NULL, entityId = 1, schoolYearId = NULL, page = 1, pageSize = 100000, flatten = T, returnResponse = F){

		params <- as.list(environment())

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		listSkyObjects(module = "SubstituteTracking", objectName = "LifeTerm", searchFields = searchFields %>% append(fieldPaths), page = page, pageSize = pageSize, SearchConditionsList = searchConditionsList, SearchConditionsGroupType = searchConditionsGroupType, SearchSortFieldNamesList = searchSortFieldNamesList, SearchSortFieldNamesDescendingList = searchSortFieldNamesDescendingList, entityId = entityId, schoolYearId = schoolYearId, flatten = flatten, returnResponse = returnResponse)
	}

	#' Get a LifeTerm
	#'
	#' This function returns a dataframe or json object of a LifeTerm
	#' @param LifeTermID The ID of the LifeTerm to return.
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given LifeTerm. Defaults to FALSE for all return fields which, for convenience, returns all fields for the LifeTerm.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object listed as text. Run \code{\link{getSchemaForObjects}}('LifeTerm') to get more field paths.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{listSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Substitute Tracking
	#' @return A dataframe or of LifeTerm
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	getLifeTerm <- function(LifeTermID, PayScaleClassID = F, FirstLifeTermUnit = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, fieldPaths = NULL, entityId = 1, schoolYearId = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment()) %>% purrr::keep(names(.) != "LifeTermID")

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		getSkyObject(module = "SubstituteTracking", objectName = "LifeTerm", objectId = LifeTermID, searchFields = searchFields, entityId = entityId, flatten = flatten, returnResponse = returnResponse)
	}

	#' Delete a LifeTerm
	#'
	#' This function deletes a LifeTerm
	#' @param LifeTermID The ID of the LifeTerm to delete
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{listSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Substitute Tracking
	#' @return The LifeTermID of the deleted LifeTerm.
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	deleteLifeTerm <- function(LifeTermID, ignoreWarnings = F, entityId = 1, schoolYearId = NULL, flatten = T, returnResponse = F){

		deleteSkyObject(module = "SubstituteTracking", objectName = "LifeTerm", objectId = LifeTermID, ignoreWarnings = ignoreWarnings, entityId = entityId, flatten = flatten, returnResponse = returnResponse)
	}

	#' Create a LifeTerm
	#'
	#' This function creates a LifeTerm
	#' @param fieldNames The field values to give the created LifeTerm. Each defaults to NULL.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{listSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Substitute Tracking
	#' @return A newly created LifeTerm
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	createLifeTerm <- function(PayScaleClassID = NULL, FirstLifeTermUnit = NULL, entityId = 1, schoolYearId = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment())

		body <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper()) %>% purrr::compact()

		createSkyObject(module = "SubstituteTracking", objectName = "LifeTerm", body = list(DataObject = body), searchFields = append("LifeTermID", body %>% names()), entityId = entityId, schoolYearId = schoolYearId, flatten = flatten, returnResponse = returnResponse)
	}

	#' Modify a LifeTerm
	#'
	#' This function modifies a LifeTerm
	#' @param fieldNames The field values to give the modified LifeTerm. Each defaults to NULL.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{listSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Substitute Tracking
	#' @return The modified LifeTerm
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	modifyLifeTerm <- function(LifeTermID, PayScaleClassID = NULL, FirstLifeTermUnit = NULL, entityId = 1, schoolYearId = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment())

		body <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper()) %>% purrr::compact()

		modifySkyObject(module = "SubstituteTracking", objectName = "LifeTerm", objectId = LifeTermID, body = list(DataObject = body), searchFields = append("LifeTermID", body %>% names()), entityId = entityId, schoolYearId = schoolYearId, flatten = flatten, returnResponse = returnResponse)
	}

	#' List LongTerms
	#'
	#' This function returns a dataframe or json object of LongTerms
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given LongTerms. Defaults to FALSE for all return fields which, for convenience, returns all fields for the LongTerms.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object listed as text. Run \code{\link{getSchemaForObjects}}('LongTerm') to get more field paths.
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
	#' @concept Substitute Tracking
	#' @return A list of LongTerms
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	listLongTerms <- function(searchConditionsList = NULL, LongTermID = F, LifeTermID = F, FirstLongTermUnit = F, Rate = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, fieldPaths = NULL, searchConditionsGroupType = "And", searchSortFieldNamesList = NULL, searchSortFieldNamesDescendingList = NULL, entityId = 1, schoolYearId = NULL, page = 1, pageSize = 100000, flatten = T, returnResponse = F){

		params <- as.list(environment())

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		listSkyObjects(module = "SubstituteTracking", objectName = "LongTerm", searchFields = searchFields %>% append(fieldPaths), page = page, pageSize = pageSize, SearchConditionsList = searchConditionsList, SearchConditionsGroupType = searchConditionsGroupType, SearchSortFieldNamesList = searchSortFieldNamesList, SearchSortFieldNamesDescendingList = searchSortFieldNamesDescendingList, entityId = entityId, schoolYearId = schoolYearId, flatten = flatten, returnResponse = returnResponse)
	}

	#' Get a LongTerm
	#'
	#' This function returns a dataframe or json object of a LongTerm
	#' @param LongTermID The ID of the LongTerm to return.
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given LongTerm. Defaults to FALSE for all return fields which, for convenience, returns all fields for the LongTerm.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object listed as text. Run \code{\link{getSchemaForObjects}}('LongTerm') to get more field paths.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{listSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Substitute Tracking
	#' @return A dataframe or of LongTerm
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	getLongTerm <- function(LongTermID, LifeTermID = F, FirstLongTermUnit = F, Rate = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, fieldPaths = NULL, entityId = 1, schoolYearId = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment()) %>% purrr::keep(names(.) != "LongTermID")

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		getSkyObject(module = "SubstituteTracking", objectName = "LongTerm", objectId = LongTermID, searchFields = searchFields, entityId = entityId, flatten = flatten, returnResponse = returnResponse)
	}

	#' Delete a LongTerm
	#'
	#' This function deletes a LongTerm
	#' @param LongTermID The ID of the LongTerm to delete
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{listSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Substitute Tracking
	#' @return The LongTermID of the deleted LongTerm.
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	deleteLongTerm <- function(LongTermID, ignoreWarnings = F, entityId = 1, schoolYearId = NULL, flatten = T, returnResponse = F){

		deleteSkyObject(module = "SubstituteTracking", objectName = "LongTerm", objectId = LongTermID, ignoreWarnings = ignoreWarnings, entityId = entityId, flatten = flatten, returnResponse = returnResponse)
	}

	#' Create a LongTerm
	#'
	#' This function creates a LongTerm
	#' @param fieldNames The field values to give the created LongTerm. Each defaults to NULL.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{listSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Substitute Tracking
	#' @return A newly created LongTerm
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	createLongTerm <- function(LifeTermID = NULL, FirstLongTermUnit = NULL, Rate = NULL, entityId = 1, schoolYearId = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment())

		body <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper()) %>% purrr::compact()

		createSkyObject(module = "SubstituteTracking", objectName = "LongTerm", body = list(DataObject = body), searchFields = append("LongTermID", body %>% names()), entityId = entityId, schoolYearId = schoolYearId, flatten = flatten, returnResponse = returnResponse)
	}

	#' Modify a LongTerm
	#'
	#' This function modifies a LongTerm
	#' @param fieldNames The field values to give the modified LongTerm. Each defaults to NULL.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{listSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Substitute Tracking
	#' @return The modified LongTerm
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	modifyLongTerm <- function(LongTermID, LifeTermID = NULL, FirstLongTermUnit = NULL, Rate = NULL, entityId = 1, schoolYearId = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment())

		body <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper()) %>% purrr::compact()

		modifySkyObject(module = "SubstituteTracking", objectName = "LongTerm", objectId = LongTermID, body = list(DataObject = body), searchFields = append("LongTermID", body %>% names()), entityId = entityId, schoolYearId = schoolYearId, flatten = flatten, returnResponse = returnResponse)
	}

	#' List SubstituteTransactions
	#'
	#' This function returns a dataframe or json object of SubstituteTransactions
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given SubstituteTransactions. Defaults to FALSE for all return fields which, for convenience, returns all fields for the SubstituteTransactions.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object listed as text. Run \code{\link{getSchemaForObjects}}('SubstituteTransaction') to get more field paths.
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
	#' @concept Substitute Tracking
	#' @return A list of SubstituteTransactions
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	listSubstituteTransactions <- function(searchConditionsList = NULL, SubstituteTransactionID = F, SubstitutePayScaleID = F, StartDateTime = F, StartDate = F, StartTime = F, Duration = F, FormattedDuration = F, EndDateTime = F, EndDate = F, EndTime = F, AbsenceType = F, AssignmentIDAbsent = F, PositionIDAbsent = F, BuildingIDAbsent = F, TransactionIdentifier = F, AccountDistributionString = F, PayAmount = F, AttachmentCount = F, Status = F, ThirdPartyImportID = F, UniqueImportRecordIdentifier = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, UnroundedWorkAmount = F, FormattedUnroundedWorkAmount = F, RenderMoveToHistoryWithoutPayroll = F, ThirdPartyImportAccountSource = F, RenderRestoreToUncalculated = F, fieldPaths = NULL, searchConditionsGroupType = "And", searchSortFieldNamesList = NULL, searchSortFieldNamesDescendingList = NULL, entityId = 1, schoolYearId = NULL, page = 1, pageSize = 100000, flatten = T, returnResponse = F){

		params <- as.list(environment())

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		listSkyObjects(module = "SubstituteTracking", objectName = "SubstituteTransaction", searchFields = searchFields %>% append(fieldPaths), page = page, pageSize = pageSize, SearchConditionsList = searchConditionsList, SearchConditionsGroupType = searchConditionsGroupType, SearchSortFieldNamesList = searchSortFieldNamesList, SearchSortFieldNamesDescendingList = searchSortFieldNamesDescendingList, entityId = entityId, schoolYearId = schoolYearId, flatten = flatten, returnResponse = returnResponse)
	}

	#' Get a SubstituteTransaction
	#'
	#' This function returns a dataframe or json object of a SubstituteTransaction
	#' @param SubstituteTransactionID The ID of the SubstituteTransaction to return.
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given SubstituteTransaction. Defaults to FALSE for all return fields which, for convenience, returns all fields for the SubstituteTransaction.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object listed as text. Run \code{\link{getSchemaForObjects}}('SubstituteTransaction') to get more field paths.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{listSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Substitute Tracking
	#' @return A dataframe or of SubstituteTransaction
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	getSubstituteTransaction <- function(SubstituteTransactionID, SubstitutePayScaleID = F, StartDateTime = F, StartDate = F, StartTime = F, Duration = F, FormattedDuration = F, EndDateTime = F, EndDate = F, EndTime = F, AbsenceType = F, AssignmentIDAbsent = F, PositionIDAbsent = F, BuildingIDAbsent = F, TransactionIdentifier = F, AccountDistributionString = F, PayAmount = F, AttachmentCount = F, Status = F, ThirdPartyImportID = F, UniqueImportRecordIdentifier = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, UnroundedWorkAmount = F, FormattedUnroundedWorkAmount = F, RenderMoveToHistoryWithoutPayroll = F, ThirdPartyImportAccountSource = F, RenderRestoreToUncalculated = F, fieldPaths = NULL, entityId = 1, schoolYearId = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment()) %>% purrr::keep(names(.) != "SubstituteTransactionID")

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		getSkyObject(module = "SubstituteTracking", objectName = "SubstituteTransaction", objectId = SubstituteTransactionID, searchFields = searchFields, entityId = entityId, flatten = flatten, returnResponse = returnResponse)
	}

	#' Delete a SubstituteTransaction
	#'
	#' This function deletes a SubstituteTransaction
	#' @param SubstituteTransactionID The ID of the SubstituteTransaction to delete
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{listSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Substitute Tracking
	#' @return The SubstituteTransactionID of the deleted SubstituteTransaction.
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	deleteSubstituteTransaction <- function(SubstituteTransactionID, ignoreWarnings = F, entityId = 1, schoolYearId = NULL, flatten = T, returnResponse = F){

		deleteSkyObject(module = "SubstituteTracking", objectName = "SubstituteTransaction", objectId = SubstituteTransactionID, ignoreWarnings = ignoreWarnings, entityId = entityId, flatten = flatten, returnResponse = returnResponse)
	}

	#' Create a SubstituteTransaction
	#'
	#' This function creates a SubstituteTransaction
	#' @param fieldNames The field values to give the created SubstituteTransaction. Each defaults to NULL.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{listSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Substitute Tracking
	#' @return A newly created SubstituteTransaction
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	createSubstituteTransaction <- function(SubstitutePayScaleID = NULL, StartDateTime = NULL, EndDateTime = NULL, AbsenceType = NULL, AssignmentIDAbsent = NULL, PositionIDAbsent = NULL, BuildingIDAbsent = NULL, AccountDistributionString = NULL, ThirdPartyImportID = NULL, UniqueImportRecordIdentifier = NULL, ThirdPartyImportAccountSource = NULL, entityId = 1, schoolYearId = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment())

		body <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper()) %>% purrr::compact()

		createSkyObject(module = "SubstituteTracking", objectName = "SubstituteTransaction", body = list(DataObject = body), searchFields = append("SubstituteTransactionID", body %>% names()), entityId = entityId, schoolYearId = schoolYearId, flatten = flatten, returnResponse = returnResponse)
	}

	#' Modify a SubstituteTransaction
	#'
	#' This function modifies a SubstituteTransaction
	#' @param fieldNames The field values to give the modified SubstituteTransaction. Each defaults to NULL.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{listSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Substitute Tracking
	#' @return The modified SubstituteTransaction
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	modifySubstituteTransaction <- function(SubstituteTransactionID, SubstitutePayScaleID = NULL, StartDateTime = NULL, EndDateTime = NULL, AbsenceType = NULL, AssignmentIDAbsent = NULL, PositionIDAbsent = NULL, BuildingIDAbsent = NULL, AccountDistributionString = NULL, ThirdPartyImportID = NULL, UniqueImportRecordIdentifier = NULL, ThirdPartyImportAccountSource = NULL, entityId = 1, schoolYearId = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment())

		body <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper()) %>% purrr::compact()

		modifySkyObject(module = "SubstituteTracking", objectName = "SubstituteTransaction", objectId = SubstituteTransactionID, body = list(DataObject = body), searchFields = append("SubstituteTransactionID", body %>% names()), entityId = entityId, schoolYearId = schoolYearId, flatten = flatten, returnResponse = returnResponse)
	}

	#' List SubstituteTransactionPays
	#'
	#' This function returns a dataframe or json object of SubstituteTransactionPays
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given SubstituteTransactionPays. Defaults to FALSE for all return fields which, for convenience, returns all fields for the SubstituteTransactionPays.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object listed as text. Run \code{\link{getSchemaForObjects}}('SubstituteTransactionPay') to get more field paths.
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
	#' @concept Substitute Tracking
	#' @return A list of SubstituteTransactionPays
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	listSubstituteTransactionPays <- function(searchConditionsList = NULL, SubstituteTransactionPayID = F, SubstituteTransactionID = F, LifeTermUnitCounter = F, LongTermUnitCounter = F, PayScaleClassID = F, Rate = F, Factor = F, PayAmount = F, TimesheetTotal = F, OverrideLifeTermUnitCounter = F, OverrideLongTermUnitCounter = F, OverridePayScaleClass = F, OverrideRate = F, OverrideFactor = F, Type = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, IsRetro = F, RetroAmount = F, MovedToHistoryWithoutPayroll = F, RenderMoveToHistoryWithoutPayroll = F, RenderRestoreToUncalculated = F, MovedToHistoryFromNetZeroCorrection = F, fieldPaths = NULL, searchConditionsGroupType = "And", searchSortFieldNamesList = NULL, searchSortFieldNamesDescendingList = NULL, entityId = 1, schoolYearId = NULL, page = 1, pageSize = 100000, flatten = T, returnResponse = F){

		params <- as.list(environment())

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		listSkyObjects(module = "SubstituteTracking", objectName = "SubstituteTransactionPay", searchFields = searchFields %>% append(fieldPaths), page = page, pageSize = pageSize, SearchConditionsList = searchConditionsList, SearchConditionsGroupType = searchConditionsGroupType, SearchSortFieldNamesList = searchSortFieldNamesList, SearchSortFieldNamesDescendingList = searchSortFieldNamesDescendingList, entityId = entityId, schoolYearId = schoolYearId, flatten = flatten, returnResponse = returnResponse)
	}

	#' Get a SubstituteTransactionPay
	#'
	#' This function returns a dataframe or json object of a SubstituteTransactionPay
	#' @param SubstituteTransactionPayID The ID of the SubstituteTransactionPay to return.
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given SubstituteTransactionPay. Defaults to FALSE for all return fields which, for convenience, returns all fields for the SubstituteTransactionPay.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object listed as text. Run \code{\link{getSchemaForObjects}}('SubstituteTransactionPay') to get more field paths.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{listSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Substitute Tracking
	#' @return A dataframe or of SubstituteTransactionPay
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	getSubstituteTransactionPay <- function(SubstituteTransactionPayID, SubstituteTransactionID = F, LifeTermUnitCounter = F, LongTermUnitCounter = F, PayScaleClassID = F, Rate = F, Factor = F, PayAmount = F, TimesheetTotal = F, OverrideLifeTermUnitCounter = F, OverrideLongTermUnitCounter = F, OverridePayScaleClass = F, OverrideRate = F, OverrideFactor = F, Type = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, IsRetro = F, RetroAmount = F, MovedToHistoryWithoutPayroll = F, RenderMoveToHistoryWithoutPayroll = F, RenderRestoreToUncalculated = F, MovedToHistoryFromNetZeroCorrection = F, fieldPaths = NULL, entityId = 1, schoolYearId = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment()) %>% purrr::keep(names(.) != "SubstituteTransactionPayID")

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		getSkyObject(module = "SubstituteTracking", objectName = "SubstituteTransactionPay", objectId = SubstituteTransactionPayID, searchFields = searchFields, entityId = entityId, flatten = flatten, returnResponse = returnResponse)
	}

	#' Delete a SubstituteTransactionPay
	#'
	#' This function deletes a SubstituteTransactionPay
	#' @param SubstituteTransactionPayID The ID of the SubstituteTransactionPay to delete
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{listSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Substitute Tracking
	#' @return The SubstituteTransactionPayID of the deleted SubstituteTransactionPay.
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	deleteSubstituteTransactionPay <- function(SubstituteTransactionPayID, ignoreWarnings = F, entityId = 1, schoolYearId = NULL, flatten = T, returnResponse = F){

		deleteSkyObject(module = "SubstituteTracking", objectName = "SubstituteTransactionPay", objectId = SubstituteTransactionPayID, ignoreWarnings = ignoreWarnings, entityId = entityId, flatten = flatten, returnResponse = returnResponse)
	}

	#' Create a SubstituteTransactionPay
	#'
	#' This function creates a SubstituteTransactionPay
	#' @param fieldNames The field values to give the created SubstituteTransactionPay. Each defaults to NULL.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{listSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Substitute Tracking
	#' @return A newly created SubstituteTransactionPay
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	createSubstituteTransactionPay <- function(SubstituteTransactionID = NULL, LifeTermUnitCounter = NULL, LongTermUnitCounter = NULL, PayScaleClassID = NULL, Rate = NULL, Factor = NULL, OverrideLifeTermUnitCounter = NULL, OverrideLongTermUnitCounter = NULL, OverridePayScaleClass = NULL, OverrideRate = NULL, OverrideFactor = NULL, Type = NULL, IsRetro = NULL, RetroAmount = NULL, MovedToHistoryWithoutPayroll = NULL, MovedToHistoryFromNetZeroCorrection = NULL, entityId = 1, schoolYearId = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment())

		body <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper()) %>% purrr::compact()

		createSkyObject(module = "SubstituteTracking", objectName = "SubstituteTransactionPay", body = list(DataObject = body), searchFields = append("SubstituteTransactionPayID", body %>% names()), entityId = entityId, schoolYearId = schoolYearId, flatten = flatten, returnResponse = returnResponse)
	}

	#' Modify a SubstituteTransactionPay
	#'
	#' This function modifies a SubstituteTransactionPay
	#' @param fieldNames The field values to give the modified SubstituteTransactionPay. Each defaults to NULL.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{listSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Substitute Tracking
	#' @return The modified SubstituteTransactionPay
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	modifySubstituteTransactionPay <- function(SubstituteTransactionPayID, SubstituteTransactionID = NULL, LifeTermUnitCounter = NULL, LongTermUnitCounter = NULL, PayScaleClassID = NULL, Rate = NULL, Factor = NULL, OverrideLifeTermUnitCounter = NULL, OverrideLongTermUnitCounter = NULL, OverridePayScaleClass = NULL, OverrideRate = NULL, OverrideFactor = NULL, Type = NULL, IsRetro = NULL, RetroAmount = NULL, MovedToHistoryWithoutPayroll = NULL, MovedToHistoryFromNetZeroCorrection = NULL, entityId = 1, schoolYearId = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment())

		body <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper()) %>% purrr::compact()

		modifySkyObject(module = "SubstituteTracking", objectName = "SubstituteTransactionPay", objectId = SubstituteTransactionPayID, body = list(DataObject = body), searchFields = append("SubstituteTransactionPayID", body %>% names()), entityId = entityId, schoolYearId = schoolYearId, flatten = flatten, returnResponse = returnResponse)
	}

	#' List SubstituteTransactionAccountDistributions
	#'
	#' This function returns a dataframe or json object of SubstituteTransactionAccountDistributions
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given SubstituteTransactionAccountDistributions. Defaults to FALSE for all return fields which, for convenience, returns all fields for the SubstituteTransactionAccountDistributions.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object listed as text. Run \code{\link{getSchemaForObjects}}('SubstituteTransactionAccountDistribution') to get more field paths.
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
	#' @concept Substitute Tracking
	#' @return A list of SubstituteTransactionAccountDistributions
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	listSubstituteTransactionAccountDistributions <- function(searchConditionsList = NULL, SubstituteTransactionAccountDistributionID = F, SubstituteTransactionID = F, AccountID = F, Percent = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, fieldPaths = NULL, searchConditionsGroupType = "And", searchSortFieldNamesList = NULL, searchSortFieldNamesDescendingList = NULL, entityId = 1, schoolYearId = NULL, page = 1, pageSize = 100000, flatten = T, returnResponse = F){

		params <- as.list(environment())

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		listSkyObjects(module = "SubstituteTracking", objectName = "SubstituteTransactionAccountDistribution", searchFields = searchFields %>% append(fieldPaths), page = page, pageSize = pageSize, SearchConditionsList = searchConditionsList, SearchConditionsGroupType = searchConditionsGroupType, SearchSortFieldNamesList = searchSortFieldNamesList, SearchSortFieldNamesDescendingList = searchSortFieldNamesDescendingList, entityId = entityId, schoolYearId = schoolYearId, flatten = flatten, returnResponse = returnResponse)
	}

	#' Get a SubstituteTransactionAccountDistribution
	#'
	#' This function returns a dataframe or json object of a SubstituteTransactionAccountDistribution
	#' @param SubstituteTransactionAccountDistributionID The ID of the SubstituteTransactionAccountDistribution to return.
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given SubstituteTransactionAccountDistribution. Defaults to FALSE for all return fields which, for convenience, returns all fields for the SubstituteTransactionAccountDistribution.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object listed as text. Run \code{\link{getSchemaForObjects}}('SubstituteTransactionAccountDistribution') to get more field paths.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{listSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Substitute Tracking
	#' @return A dataframe or of SubstituteTransactionAccountDistribution
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	getSubstituteTransactionAccountDistribution <- function(SubstituteTransactionAccountDistributionID, SubstituteTransactionID = F, AccountID = F, Percent = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, fieldPaths = NULL, entityId = 1, schoolYearId = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment()) %>% purrr::keep(names(.) != "SubstituteTransactionAccountDistributionID")

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		getSkyObject(module = "SubstituteTracking", objectName = "SubstituteTransactionAccountDistribution", objectId = SubstituteTransactionAccountDistributionID, searchFields = searchFields, entityId = entityId, flatten = flatten, returnResponse = returnResponse)
	}

	#' Delete a SubstituteTransactionAccountDistribution
	#'
	#' This function deletes a SubstituteTransactionAccountDistribution
	#' @param SubstituteTransactionAccountDistributionID The ID of the SubstituteTransactionAccountDistribution to delete
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{listSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Substitute Tracking
	#' @return The SubstituteTransactionAccountDistributionID of the deleted SubstituteTransactionAccountDistribution.
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	deleteSubstituteTransactionAccountDistribution <- function(SubstituteTransactionAccountDistributionID, ignoreWarnings = F, entityId = 1, schoolYearId = NULL, flatten = T, returnResponse = F){

		deleteSkyObject(module = "SubstituteTracking", objectName = "SubstituteTransactionAccountDistribution", objectId = SubstituteTransactionAccountDistributionID, ignoreWarnings = ignoreWarnings, entityId = entityId, flatten = flatten, returnResponse = returnResponse)
	}

	#' Create a SubstituteTransactionAccountDistribution
	#'
	#' This function creates a SubstituteTransactionAccountDistribution
	#' @param fieldNames The field values to give the created SubstituteTransactionAccountDistribution. Each defaults to NULL.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{listSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Substitute Tracking
	#' @return A newly created SubstituteTransactionAccountDistribution
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	createSubstituteTransactionAccountDistribution <- function(SubstituteTransactionID = NULL, AccountID = NULL, Percent = NULL, entityId = 1, schoolYearId = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment())

		body <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper()) %>% purrr::compact()

		createSkyObject(module = "SubstituteTracking", objectName = "SubstituteTransactionAccountDistribution", body = list(DataObject = body), searchFields = append("SubstituteTransactionAccountDistributionID", body %>% names()), entityId = entityId, schoolYearId = schoolYearId, flatten = flatten, returnResponse = returnResponse)
	}

	#' Modify a SubstituteTransactionAccountDistribution
	#'
	#' This function modifies a SubstituteTransactionAccountDistribution
	#' @param fieldNames The field values to give the modified SubstituteTransactionAccountDistribution. Each defaults to NULL.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{listSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Substitute Tracking
	#' @return The modified SubstituteTransactionAccountDistribution
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	modifySubstituteTransactionAccountDistribution <- function(SubstituteTransactionAccountDistributionID, SubstituteTransactionID = NULL, AccountID = NULL, Percent = NULL, entityId = 1, schoolYearId = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment())

		body <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper()) %>% purrr::compact()

		modifySkyObject(module = "SubstituteTracking", objectName = "SubstituteTransactionAccountDistribution", objectId = SubstituteTransactionAccountDistributionID, body = list(DataObject = body), searchFields = append("SubstituteTransactionAccountDistributionID", body %>% names()), entityId = entityId, schoolYearId = schoolYearId, flatten = flatten, returnResponse = returnResponse)
	}

	#' List TempSubstitutePayScales
	#'
	#' This function returns a dataframe or json object of TempSubstitutePayScales
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given TempSubstitutePayScales. Defaults to FALSE for all return fields which, for convenience, returns all fields for the TempSubstitutePayScales.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object listed as text. Run \code{\link{getSchemaForObjects}}('TempSubstitutePayScale') to get more field paths.
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
	#' @concept Substitute Tracking
	#' @return A list of TempSubstitutePayScales
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	listTempSubstitutePayScales <- function(searchConditionsList = NULL, TempSubstitutePayScaleID = F, EmployeeID = F, EmployeeNumber = F, EmployeeNameLFM = F, PayScaleID = F, SubstitutePayScaleID = F, PayScaleCodeDescription = F, AssignmentPayTypeID = F, PayTypeCodeDescription = F, AssignmentIdentifier = F, PayScaleClassID = F, PayScaleClassDescription = F, StartDate = F, EndDate = F, ErrorCount = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, SubstitutePayScaleIDClonedFrom = F, SkipImport = F, PreviousYearLifeTermUnitCounter = F, ErrorDetail = F, LifeTermUnitCounter = F, fieldPaths = NULL, searchConditionsGroupType = "And", searchSortFieldNamesList = NULL, searchSortFieldNamesDescendingList = NULL, entityId = 1, schoolYearId = NULL, page = 1, pageSize = 100000, flatten = T, returnResponse = F){

		params <- as.list(environment())

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		listSkyObjects(module = "SubstituteTracking", objectName = "TempSubstitutePayScale", searchFields = searchFields %>% append(fieldPaths), page = page, pageSize = pageSize, SearchConditionsList = searchConditionsList, SearchConditionsGroupType = searchConditionsGroupType, SearchSortFieldNamesList = searchSortFieldNamesList, SearchSortFieldNamesDescendingList = searchSortFieldNamesDescendingList, entityId = entityId, schoolYearId = schoolYearId, flatten = flatten, returnResponse = returnResponse)
	}

	#' Get a TempSubstitutePayScale
	#'
	#' This function returns a dataframe or json object of a TempSubstitutePayScale
	#' @param TempSubstitutePayScaleID The ID of the TempSubstitutePayScale to return.
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given TempSubstitutePayScale. Defaults to FALSE for all return fields which, for convenience, returns all fields for the TempSubstitutePayScale.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object listed as text. Run \code{\link{getSchemaForObjects}}('TempSubstitutePayScale') to get more field paths.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{listSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Substitute Tracking
	#' @return A dataframe or of TempSubstitutePayScale
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	getTempSubstitutePayScale <- function(TempSubstitutePayScaleID, EmployeeID = F, EmployeeNumber = F, EmployeeNameLFM = F, PayScaleID = F, SubstitutePayScaleID = F, PayScaleCodeDescription = F, AssignmentPayTypeID = F, PayTypeCodeDescription = F, AssignmentIdentifier = F, PayScaleClassID = F, PayScaleClassDescription = F, StartDate = F, EndDate = F, ErrorCount = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, SubstitutePayScaleIDClonedFrom = F, SkipImport = F, PreviousYearLifeTermUnitCounter = F, ErrorDetail = F, LifeTermUnitCounter = F, fieldPaths = NULL, entityId = 1, schoolYearId = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment()) %>% purrr::keep(names(.) != "TempSubstitutePayScaleID")

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		getSkyObject(module = "SubstituteTracking", objectName = "TempSubstitutePayScale", objectId = TempSubstitutePayScaleID, searchFields = searchFields, entityId = entityId, flatten = flatten, returnResponse = returnResponse)
	}

	#' Delete a TempSubstitutePayScale
	#'
	#' This function deletes a TempSubstitutePayScale
	#' @param TempSubstitutePayScaleID The ID of the TempSubstitutePayScale to delete
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{listSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Substitute Tracking
	#' @return The TempSubstitutePayScaleID of the deleted TempSubstitutePayScale.
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	deleteTempSubstitutePayScale <- function(TempSubstitutePayScaleID, ignoreWarnings = F, entityId = 1, schoolYearId = NULL, flatten = T, returnResponse = F){

		deleteSkyObject(module = "SubstituteTracking", objectName = "TempSubstitutePayScale", objectId = TempSubstitutePayScaleID, ignoreWarnings = ignoreWarnings, entityId = entityId, flatten = flatten, returnResponse = returnResponse)
	}

	#' Create a TempSubstitutePayScale
	#'
	#' This function creates a TempSubstitutePayScale
	#' @param fieldNames The field values to give the created TempSubstitutePayScale. Each defaults to NULL.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{listSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Substitute Tracking
	#' @return A newly created TempSubstitutePayScale
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	createTempSubstitutePayScale <- function(EmployeeID = NULL, EmployeeNumber = NULL, EmployeeNameLFM = NULL, PayScaleID = NULL, SubstitutePayScaleID = NULL, PayScaleCodeDescription = NULL, AssignmentPayTypeID = NULL, PayTypeCodeDescription = NULL, AssignmentIdentifier = NULL, PayScaleClassID = NULL, PayScaleClassDescription = NULL, StartDate = NULL, EndDate = NULL, ErrorCount = NULL, SubstitutePayScaleIDClonedFrom = NULL, SkipImport = NULL, PreviousYearLifeTermUnitCounter = NULL, ErrorDetail = NULL, LifeTermUnitCounter = NULL, entityId = 1, schoolYearId = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment())

		body <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper()) %>% purrr::compact()

		createSkyObject(module = "SubstituteTracking", objectName = "TempSubstitutePayScale", body = list(DataObject = body), searchFields = append("TempSubstitutePayScaleID", body %>% names()), entityId = entityId, schoolYearId = schoolYearId, flatten = flatten, returnResponse = returnResponse)
	}

	#' Modify a TempSubstitutePayScale
	#'
	#' This function modifies a TempSubstitutePayScale
	#' @param fieldNames The field values to give the modified TempSubstitutePayScale. Each defaults to NULL.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{listSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Substitute Tracking
	#' @return The modified TempSubstitutePayScale
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	modifyTempSubstitutePayScale <- function(TempSubstitutePayScaleID, EmployeeID = NULL, EmployeeNumber = NULL, EmployeeNameLFM = NULL, PayScaleID = NULL, SubstitutePayScaleID = NULL, PayScaleCodeDescription = NULL, AssignmentPayTypeID = NULL, PayTypeCodeDescription = NULL, AssignmentIdentifier = NULL, PayScaleClassID = NULL, PayScaleClassDescription = NULL, StartDate = NULL, EndDate = NULL, ErrorCount = NULL, SubstitutePayScaleIDClonedFrom = NULL, SkipImport = NULL, PreviousYearLifeTermUnitCounter = NULL, ErrorDetail = NULL, LifeTermUnitCounter = NULL, entityId = 1, schoolYearId = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment())

		body <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper()) %>% purrr::compact()

		modifySkyObject(module = "SubstituteTracking", objectName = "TempSubstitutePayScale", objectId = TempSubstitutePayScaleID, body = list(DataObject = body), searchFields = append("TempSubstitutePayScaleID", body %>% names()), entityId = entityId, schoolYearId = schoolYearId, flatten = flatten, returnResponse = returnResponse)
	}

	#' List TempSubstitutePayScaleExceptionMessages
	#'
	#' This function returns a dataframe or json object of TempSubstitutePayScaleExceptionMessages
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given TempSubstitutePayScaleExceptionMessages. Defaults to FALSE for all return fields which, for convenience, returns all fields for the TempSubstitutePayScaleExceptionMessages.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object listed as text. Run \code{\link{getSchemaForObjects}}('TempSubstitutePayScaleExceptionMessage') to get more field paths.
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
	#' @concept Substitute Tracking
	#' @return A list of TempSubstitutePayScaleExceptionMessages
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	listTempSubstitutePayScaleExceptionMessages <- function(searchConditionsList = NULL, TempSubstitutePayScaleExceptionMessageID = F, TempSubstitutePayScaleID = F, EmployeeNameLFM = F, Error = F, ErrorDetail = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, EmployeeNumber = F, fieldPaths = NULL, searchConditionsGroupType = "And", searchSortFieldNamesList = NULL, searchSortFieldNamesDescendingList = NULL, entityId = 1, schoolYearId = NULL, page = 1, pageSize = 100000, flatten = T, returnResponse = F){

		params <- as.list(environment())

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		listSkyObjects(module = "SubstituteTracking", objectName = "TempSubstitutePayScaleExceptionMessage", searchFields = searchFields %>% append(fieldPaths), page = page, pageSize = pageSize, SearchConditionsList = searchConditionsList, SearchConditionsGroupType = searchConditionsGroupType, SearchSortFieldNamesList = searchSortFieldNamesList, SearchSortFieldNamesDescendingList = searchSortFieldNamesDescendingList, entityId = entityId, schoolYearId = schoolYearId, flatten = flatten, returnResponse = returnResponse)
	}

	#' Get a TempSubstitutePayScaleExceptionMessage
	#'
	#' This function returns a dataframe or json object of a TempSubstitutePayScaleExceptionMessage
	#' @param TempSubstitutePayScaleExceptionMessageID The ID of the TempSubstitutePayScaleExceptionMessage to return.
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given TempSubstitutePayScaleExceptionMessage. Defaults to FALSE for all return fields which, for convenience, returns all fields for the TempSubstitutePayScaleExceptionMessage.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object listed as text. Run \code{\link{getSchemaForObjects}}('TempSubstitutePayScaleExceptionMessage') to get more field paths.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{listSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Substitute Tracking
	#' @return A dataframe or of TempSubstitutePayScaleExceptionMessage
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	getTempSubstitutePayScaleExceptionMessage <- function(TempSubstitutePayScaleExceptionMessageID, TempSubstitutePayScaleID = F, EmployeeNameLFM = F, Error = F, ErrorDetail = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, EmployeeNumber = F, fieldPaths = NULL, entityId = 1, schoolYearId = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment()) %>% purrr::keep(names(.) != "TempSubstitutePayScaleExceptionMessageID")

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		getSkyObject(module = "SubstituteTracking", objectName = "TempSubstitutePayScaleExceptionMessage", objectId = TempSubstitutePayScaleExceptionMessageID, searchFields = searchFields, entityId = entityId, flatten = flatten, returnResponse = returnResponse)
	}

	#' Delete a TempSubstitutePayScaleExceptionMessage
	#'
	#' This function deletes a TempSubstitutePayScaleExceptionMessage
	#' @param TempSubstitutePayScaleExceptionMessageID The ID of the TempSubstitutePayScaleExceptionMessage to delete
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{listSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Substitute Tracking
	#' @return The TempSubstitutePayScaleExceptionMessageID of the deleted TempSubstitutePayScaleExceptionMessage.
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	deleteTempSubstitutePayScaleExceptionMessage <- function(TempSubstitutePayScaleExceptionMessageID, ignoreWarnings = F, entityId = 1, schoolYearId = NULL, flatten = T, returnResponse = F){

		deleteSkyObject(module = "SubstituteTracking", objectName = "TempSubstitutePayScaleExceptionMessage", objectId = TempSubstitutePayScaleExceptionMessageID, ignoreWarnings = ignoreWarnings, entityId = entityId, flatten = flatten, returnResponse = returnResponse)
	}

	#' Create a TempSubstitutePayScaleExceptionMessage
	#'
	#' This function creates a TempSubstitutePayScaleExceptionMessage
	#' @param fieldNames The field values to give the created TempSubstitutePayScaleExceptionMessage. Each defaults to NULL.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{listSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Substitute Tracking
	#' @return A newly created TempSubstitutePayScaleExceptionMessage
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	createTempSubstitutePayScaleExceptionMessage <- function(TempSubstitutePayScaleID = NULL, EmployeeNameLFM = NULL, Error = NULL, ErrorDetail = NULL, EmployeeNumber = NULL, entityId = 1, schoolYearId = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment())

		body <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper()) %>% purrr::compact()

		createSkyObject(module = "SubstituteTracking", objectName = "TempSubstitutePayScaleExceptionMessage", body = list(DataObject = body), searchFields = append("TempSubstitutePayScaleExceptionMessageID", body %>% names()), entityId = entityId, schoolYearId = schoolYearId, flatten = flatten, returnResponse = returnResponse)
	}

	#' Modify a TempSubstitutePayScaleExceptionMessage
	#'
	#' This function modifies a TempSubstitutePayScaleExceptionMessage
	#' @param fieldNames The field values to give the modified TempSubstitutePayScaleExceptionMessage. Each defaults to NULL.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{listSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Substitute Tracking
	#' @return The modified TempSubstitutePayScaleExceptionMessage
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	modifyTempSubstitutePayScaleExceptionMessage <- function(TempSubstitutePayScaleExceptionMessageID, TempSubstitutePayScaleID = NULL, EmployeeNameLFM = NULL, Error = NULL, ErrorDetail = NULL, EmployeeNumber = NULL, entityId = 1, schoolYearId = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment())

		body <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper()) %>% purrr::compact()

		modifySkyObject(module = "SubstituteTracking", objectName = "TempSubstitutePayScaleExceptionMessage", objectId = TempSubstitutePayScaleExceptionMessageID, body = list(DataObject = body), searchFields = append("TempSubstitutePayScaleExceptionMessageID", body %>% names()), entityId = entityId, schoolYearId = schoolYearId, flatten = flatten, returnResponse = returnResponse)
	}

	#' List TempSubstituteTransactionErrors
	#'
	#' This function returns a dataframe or json object of TempSubstituteTransactionErrors
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given TempSubstituteTransactionErrors. Defaults to FALSE for all return fields which, for convenience, returns all fields for the TempSubstituteTransactionErrors.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object listed as text. Run \code{\link{getSchemaForObjects}}('TempSubstituteTransactionError') to get more field paths.
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
	#' @concept Substitute Tracking
	#' @return A list of TempSubstituteTransactionErrors
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	listTempSubstituteTransactionErrors <- function(searchConditionsList = NULL, TempSubstituteTransactionErrorID = F, SubstituteTransactionID = F, SubstitutePayScaleID = F, EmployeeNameLFM = F, EmployeeAbsent = F, EmployeeID = F, StartDateTime = F, StartDate = F, StartTime = F, Duration = F, FormattedDuration = F, AbsenceType = F, SubstituteEmployee = F, AbsentEmployee = F, AssignmentID = F, BuildingID = F, PositionID = F, UniqueImportRecordIdentifier = F, PayAmount = F, ErrorReason = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, EmployeeNumber = F, AbsentEmployeeNumber = F, fieldPaths = NULL, searchConditionsGroupType = "And", searchSortFieldNamesList = NULL, searchSortFieldNamesDescendingList = NULL, entityId = 1, schoolYearId = NULL, page = 1, pageSize = 100000, flatten = T, returnResponse = F){

		params <- as.list(environment())

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		listSkyObjects(module = "SubstituteTracking", objectName = "TempSubstituteTransactionError", searchFields = searchFields %>% append(fieldPaths), page = page, pageSize = pageSize, SearchConditionsList = searchConditionsList, SearchConditionsGroupType = searchConditionsGroupType, SearchSortFieldNamesList = searchSortFieldNamesList, SearchSortFieldNamesDescendingList = searchSortFieldNamesDescendingList, entityId = entityId, schoolYearId = schoolYearId, flatten = flatten, returnResponse = returnResponse)
	}

	#' Get a TempSubstituteTransactionError
	#'
	#' This function returns a dataframe or json object of a TempSubstituteTransactionError
	#' @param TempSubstituteTransactionErrorID The ID of the TempSubstituteTransactionError to return.
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given TempSubstituteTransactionError. Defaults to FALSE for all return fields which, for convenience, returns all fields for the TempSubstituteTransactionError.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object listed as text. Run \code{\link{getSchemaForObjects}}('TempSubstituteTransactionError') to get more field paths.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{listSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Substitute Tracking
	#' @return A dataframe or of TempSubstituteTransactionError
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	getTempSubstituteTransactionError <- function(TempSubstituteTransactionErrorID, SubstituteTransactionID = F, SubstitutePayScaleID = F, EmployeeNameLFM = F, EmployeeAbsent = F, EmployeeID = F, StartDateTime = F, StartDate = F, StartTime = F, Duration = F, FormattedDuration = F, AbsenceType = F, SubstituteEmployee = F, AbsentEmployee = F, AssignmentID = F, BuildingID = F, PositionID = F, UniqueImportRecordIdentifier = F, PayAmount = F, ErrorReason = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, EmployeeNumber = F, AbsentEmployeeNumber = F, fieldPaths = NULL, entityId = 1, schoolYearId = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment()) %>% purrr::keep(names(.) != "TempSubstituteTransactionErrorID")

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		getSkyObject(module = "SubstituteTracking", objectName = "TempSubstituteTransactionError", objectId = TempSubstituteTransactionErrorID, searchFields = searchFields, entityId = entityId, flatten = flatten, returnResponse = returnResponse)
	}

	#' Delete a TempSubstituteTransactionError
	#'
	#' This function deletes a TempSubstituteTransactionError
	#' @param TempSubstituteTransactionErrorID The ID of the TempSubstituteTransactionError to delete
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{listSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Substitute Tracking
	#' @return The TempSubstituteTransactionErrorID of the deleted TempSubstituteTransactionError.
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	deleteTempSubstituteTransactionError <- function(TempSubstituteTransactionErrorID, ignoreWarnings = F, entityId = 1, schoolYearId = NULL, flatten = T, returnResponse = F){

		deleteSkyObject(module = "SubstituteTracking", objectName = "TempSubstituteTransactionError", objectId = TempSubstituteTransactionErrorID, ignoreWarnings = ignoreWarnings, entityId = entityId, flatten = flatten, returnResponse = returnResponse)
	}

	#' Create a TempSubstituteTransactionError
	#'
	#' This function creates a TempSubstituteTransactionError
	#' @param fieldNames The field values to give the created TempSubstituteTransactionError. Each defaults to NULL.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{listSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Substitute Tracking
	#' @return A newly created TempSubstituteTransactionError
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	createTempSubstituteTransactionError <- function(SubstituteTransactionID = NULL, SubstitutePayScaleID = NULL, EmployeeNameLFM = NULL, EmployeeAbsent = NULL, StartDateTime = NULL, Duration = NULL, AbsenceType = NULL, SubstituteEmployee = NULL, AbsentEmployee = NULL, AssignmentID = NULL, BuildingID = NULL, PositionID = NULL, UniqueImportRecordIdentifier = NULL, PayAmount = NULL, ErrorReason = NULL, EmployeeNumber = NULL, AbsentEmployeeNumber = NULL, entityId = 1, schoolYearId = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment())

		body <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper()) %>% purrr::compact()

		createSkyObject(module = "SubstituteTracking", objectName = "TempSubstituteTransactionError", body = list(DataObject = body), searchFields = append("TempSubstituteTransactionErrorID", body %>% names()), entityId = entityId, schoolYearId = schoolYearId, flatten = flatten, returnResponse = returnResponse)
	}

	#' Modify a TempSubstituteTransactionError
	#'
	#' This function modifies a TempSubstituteTransactionError
	#' @param fieldNames The field values to give the modified TempSubstituteTransactionError. Each defaults to NULL.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{listSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Substitute Tracking
	#' @return The modified TempSubstituteTransactionError
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	modifyTempSubstituteTransactionError <- function(TempSubstituteTransactionErrorID, SubstituteTransactionID = NULL, SubstitutePayScaleID = NULL, EmployeeNameLFM = NULL, EmployeeAbsent = NULL, StartDateTime = NULL, Duration = NULL, AbsenceType = NULL, SubstituteEmployee = NULL, AbsentEmployee = NULL, AssignmentID = NULL, BuildingID = NULL, PositionID = NULL, UniqueImportRecordIdentifier = NULL, PayAmount = NULL, ErrorReason = NULL, EmployeeNumber = NULL, AbsentEmployeeNumber = NULL, entityId = 1, schoolYearId = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment())

		body <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper()) %>% purrr::compact()

		modifySkyObject(module = "SubstituteTracking", objectName = "TempSubstituteTransactionError", objectId = TempSubstituteTransactionErrorID, body = list(DataObject = body), searchFields = append("TempSubstituteTransactionErrorID", body %>% names()), entityId = entityId, schoolYearId = schoolYearId, flatten = flatten, returnResponse = returnResponse)
	}

	#' List TempSubstituteTransactionErrorTableNames
	#'
	#' This function returns a dataframe or json object of TempSubstituteTransactionErrorTableNames
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given TempSubstituteTransactionErrorTableNames. Defaults to FALSE for all return fields which, for convenience, returns all fields for the TempSubstituteTransactionErrorTableNames.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object listed as text. Run \code{\link{getSchemaForObjects}}('TempSubstituteTransactionErrorTableName') to get more field paths.
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
	#' @concept Substitute Tracking
	#' @return A list of TempSubstituteTransactionErrorTableNames
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	listTempSubstituteTransactionErrorTableNames <- function(searchConditionsList = NULL, TempSubstituteTransactionErrorTableNameID = F, SubstituteTransactionID = F, SubstitutePayScaleID = F, StartDateTime = F, Duration = F, FormattedDuration = F, AbsenceType = F, SubstituteEmployee = F, AbsentEmployee = F, AssignmentID = F, BuildingID = F, PositionID = F, UniqueImportRecordIdentifier = F, PayAmount = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, fieldPaths = NULL, searchConditionsGroupType = "And", searchSortFieldNamesList = NULL, searchSortFieldNamesDescendingList = NULL, entityId = 1, schoolYearId = NULL, page = 1, pageSize = 100000, flatten = T, returnResponse = F){

		params <- as.list(environment())

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		listSkyObjects(module = "SubstituteTracking", objectName = "TempSubstituteTransactionErrorTableName", searchFields = searchFields %>% append(fieldPaths), page = page, pageSize = pageSize, SearchConditionsList = searchConditionsList, SearchConditionsGroupType = searchConditionsGroupType, SearchSortFieldNamesList = searchSortFieldNamesList, SearchSortFieldNamesDescendingList = searchSortFieldNamesDescendingList, entityId = entityId, schoolYearId = schoolYearId, flatten = flatten, returnResponse = returnResponse)
	}

	#' Get a TempSubstituteTransactionErrorTableName
	#'
	#' This function returns a dataframe or json object of a TempSubstituteTransactionErrorTableName
	#' @param TempSubstituteTransactionErrorTableNameID The ID of the TempSubstituteTransactionErrorTableName to return.
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given TempSubstituteTransactionErrorTableName. Defaults to FALSE for all return fields which, for convenience, returns all fields for the TempSubstituteTransactionErrorTableName.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object listed as text. Run \code{\link{getSchemaForObjects}}('TempSubstituteTransactionErrorTableName') to get more field paths.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{listSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Substitute Tracking
	#' @return A dataframe or of TempSubstituteTransactionErrorTableName
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	getTempSubstituteTransactionErrorTableName <- function(TempSubstituteTransactionErrorTableNameID, SubstituteTransactionID = F, SubstitutePayScaleID = F, StartDateTime = F, Duration = F, FormattedDuration = F, AbsenceType = F, SubstituteEmployee = F, AbsentEmployee = F, AssignmentID = F, BuildingID = F, PositionID = F, UniqueImportRecordIdentifier = F, PayAmount = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, fieldPaths = NULL, entityId = 1, schoolYearId = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment()) %>% purrr::keep(names(.) != "TempSubstituteTransactionErrorTableNameID")

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		getSkyObject(module = "SubstituteTracking", objectName = "TempSubstituteTransactionErrorTableName", objectId = TempSubstituteTransactionErrorTableNameID, searchFields = searchFields, entityId = entityId, flatten = flatten, returnResponse = returnResponse)
	}

	#' Delete a TempSubstituteTransactionErrorTableName
	#'
	#' This function deletes a TempSubstituteTransactionErrorTableName
	#' @param TempSubstituteTransactionErrorTableNameID The ID of the TempSubstituteTransactionErrorTableName to delete
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{listSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Substitute Tracking
	#' @return The TempSubstituteTransactionErrorTableNameID of the deleted TempSubstituteTransactionErrorTableName.
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	deleteTempSubstituteTransactionErrorTableName <- function(TempSubstituteTransactionErrorTableNameID, ignoreWarnings = F, entityId = 1, schoolYearId = NULL, flatten = T, returnResponse = F){

		deleteSkyObject(module = "SubstituteTracking", objectName = "TempSubstituteTransactionErrorTableName", objectId = TempSubstituteTransactionErrorTableNameID, ignoreWarnings = ignoreWarnings, entityId = entityId, flatten = flatten, returnResponse = returnResponse)
	}

	#' Create a TempSubstituteTransactionErrorTableName
	#'
	#' This function creates a TempSubstituteTransactionErrorTableName
	#' @param fieldNames The field values to give the created TempSubstituteTransactionErrorTableName. Each defaults to NULL.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{listSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Substitute Tracking
	#' @return A newly created TempSubstituteTransactionErrorTableName
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	createTempSubstituteTransactionErrorTableName <- function(SubstituteTransactionID = NULL, SubstitutePayScaleID = NULL, StartDateTime = NULL, AbsenceType = NULL, SubstituteEmployee = NULL, AbsentEmployee = NULL, AssignmentID = NULL, BuildingID = NULL, PositionID = NULL, UniqueImportRecordIdentifier = NULL, entityId = 1, schoolYearId = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment())

		body <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper()) %>% purrr::compact()

		createSkyObject(module = "SubstituteTracking", objectName = "TempSubstituteTransactionErrorTableName", body = list(DataObject = body), searchFields = append("TempSubstituteTransactionErrorTableNameID", body %>% names()), entityId = entityId, schoolYearId = schoolYearId, flatten = flatten, returnResponse = returnResponse)
	}

	#' Modify a TempSubstituteTransactionErrorTableName
	#'
	#' This function modifies a TempSubstituteTransactionErrorTableName
	#' @param fieldNames The field values to give the modified TempSubstituteTransactionErrorTableName. Each defaults to NULL.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{listSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Substitute Tracking
	#' @return The modified TempSubstituteTransactionErrorTableName
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	modifyTempSubstituteTransactionErrorTableName <- function(TempSubstituteTransactionErrorTableNameID, SubstituteTransactionID = NULL, SubstitutePayScaleID = NULL, StartDateTime = NULL, AbsenceType = NULL, SubstituteEmployee = NULL, AbsentEmployee = NULL, AssignmentID = NULL, BuildingID = NULL, PositionID = NULL, UniqueImportRecordIdentifier = NULL, entityId = 1, schoolYearId = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment())

		body <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper()) %>% purrr::compact()

		modifySkyObject(module = "SubstituteTracking", objectName = "TempSubstituteTransactionErrorTableName", objectId = TempSubstituteTransactionErrorTableNameID, body = list(DataObject = body), searchFields = append("TempSubstituteTransactionErrorTableNameID", body %>% names()), entityId = entityId, schoolYearId = schoolYearId, flatten = flatten, returnResponse = returnResponse)
	}

	#' List TempThirdPartyFormatData
	#'
	#' This function returns a dataframe or json object of TempThirdPartyFormatData
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given TempThirdPartyFormatData. Defaults to FALSE for all return fields which, for convenience, returns all fields for the TempThirdPartyFormatData.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object listed as text. Run \code{\link{getSchemaForObjects}}('TempThirdPartyFormatData') to get more field paths.
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
	#' @concept Substitute Tracking
	#' @return A list of TempThirdPartyFormatData
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	listTempThirdPartyFormatData <- function(searchConditionsList = NULL, TempThirdPartyFormatDataID = F, UniqueRecordIdentifier = F, SubstituteDuration = F, AbsentDuration = F, SubstituteEmployeeNumber = F, AbsentEmployeeNumber = F, PositionType = F, TimeOffType = F, Building = F, Date = F, Account = F, Data = F, LineNumber = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, TimeOffDescription = F, HasFatalException = F, fieldPaths = NULL, searchConditionsGroupType = "And", searchSortFieldNamesList = NULL, searchSortFieldNamesDescendingList = NULL, entityId = 1, schoolYearId = NULL, page = 1, pageSize = 100000, flatten = T, returnResponse = F){

		params <- as.list(environment())

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		listSkyObjects(module = "SubstituteTracking", objectName = "TempThirdPartyFormatData", searchFields = searchFields %>% append(fieldPaths), page = page, pageSize = pageSize, SearchConditionsList = searchConditionsList, SearchConditionsGroupType = searchConditionsGroupType, SearchSortFieldNamesList = searchSortFieldNamesList, SearchSortFieldNamesDescendingList = searchSortFieldNamesDescendingList, entityId = entityId, schoolYearId = schoolYearId, flatten = flatten, returnResponse = returnResponse)
	}

	#' Get a TempThirdPartyFormatData
	#'
	#' This function returns a dataframe or json object of a TempThirdPartyFormatData
	#' @param TempThirdPartyFormatDataID The ID of the TempThirdPartyFormatData to return.
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given TempThirdPartyFormatData. Defaults to FALSE for all return fields which, for convenience, returns all fields for the TempThirdPartyFormatData.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object listed as text. Run \code{\link{getSchemaForObjects}}('TempThirdPartyFormatData') to get more field paths.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{listSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Substitute Tracking
	#' @return A dataframe or of TempThirdPartyFormatData
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	getTempThirdPartyFormatData <- function(TempThirdPartyFormatDataID, UniqueRecordIdentifier = F, SubstituteDuration = F, AbsentDuration = F, SubstituteEmployeeNumber = F, AbsentEmployeeNumber = F, PositionType = F, TimeOffType = F, Building = F, Date = F, Account = F, Data = F, LineNumber = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, TimeOffDescription = F, HasFatalException = F, fieldPaths = NULL, entityId = 1, schoolYearId = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment()) %>% purrr::keep(names(.) != "TempThirdPartyFormatDataID")

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		getSkyObject(module = "SubstituteTracking", objectName = "TempThirdPartyFormatData", objectId = TempThirdPartyFormatDataID, searchFields = searchFields, entityId = entityId, flatten = flatten, returnResponse = returnResponse)
	}

	#' Delete a TempThirdPartyFormatData
	#'
	#' This function deletes a TempThirdPartyFormatData
	#' @param TempThirdPartyFormatDataID The ID of the TempThirdPartyFormatData to delete
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{listSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Substitute Tracking
	#' @return The TempThirdPartyFormatDataID of the deleted TempThirdPartyFormatData.
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	deleteTempThirdPartyFormatData <- function(TempThirdPartyFormatDataID, ignoreWarnings = F, entityId = 1, schoolYearId = NULL, flatten = T, returnResponse = F){

		deleteSkyObject(module = "SubstituteTracking", objectName = "TempThirdPartyFormatData", objectId = TempThirdPartyFormatDataID, ignoreWarnings = ignoreWarnings, entityId = entityId, flatten = flatten, returnResponse = returnResponse)
	}

	#' Create a TempThirdPartyFormatData
	#'
	#' This function creates a TempThirdPartyFormatData
	#' @param fieldNames The field values to give the created TempThirdPartyFormatData. Each defaults to NULL.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{listSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Substitute Tracking
	#' @return A newly created TempThirdPartyFormatData
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	createTempThirdPartyFormatData <- function(UniqueRecordIdentifier = NULL, SubstituteDuration = NULL, AbsentDuration = NULL, SubstituteEmployeeNumber = NULL, AbsentEmployeeNumber = NULL, PositionType = NULL, TimeOffType = NULL, Building = NULL, Date = NULL, Account = NULL, Data = NULL, LineNumber = NULL, TimeOffDescription = NULL, HasFatalException = NULL, entityId = 1, schoolYearId = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment())

		body <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper()) %>% purrr::compact()

		createSkyObject(module = "SubstituteTracking", objectName = "TempThirdPartyFormatData", body = list(DataObject = body), searchFields = append("TempThirdPartyFormatDataID", body %>% names()), entityId = entityId, schoolYearId = schoolYearId, flatten = flatten, returnResponse = returnResponse)
	}

	#' Modify a TempThirdPartyFormatData
	#'
	#' This function modifies a TempThirdPartyFormatData
	#' @param fieldNames The field values to give the modified TempThirdPartyFormatData. Each defaults to NULL.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{listSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Substitute Tracking
	#' @return The modified TempThirdPartyFormatData
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	modifyTempThirdPartyFormatData <- function(TempThirdPartyFormatDataID, UniqueRecordIdentifier = NULL, SubstituteDuration = NULL, AbsentDuration = NULL, SubstituteEmployeeNumber = NULL, AbsentEmployeeNumber = NULL, PositionType = NULL, TimeOffType = NULL, Building = NULL, Date = NULL, Account = NULL, Data = NULL, LineNumber = NULL, TimeOffDescription = NULL, HasFatalException = NULL, entityId = 1, schoolYearId = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment())

		body <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper()) %>% purrr::compact()

		modifySkyObject(module = "SubstituteTracking", objectName = "TempThirdPartyFormatData", objectId = TempThirdPartyFormatDataID, body = list(DataObject = body), searchFields = append("TempThirdPartyFormatDataID", body %>% names()), entityId = entityId, schoolYearId = schoolYearId, flatten = flatten, returnResponse = returnResponse)
	}

	#' List WorkAmounts
	#'
	#' This function returns a dataframe or json object of WorkAmounts
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given WorkAmounts. Defaults to FALSE for all return fields which, for convenience, returns all fields for the WorkAmounts.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object listed as text. Run \code{\link{getSchemaForObjects}}('WorkAmount') to get more field paths.
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
	#' @concept Substitute Tracking
	#' @return A list of WorkAmounts
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	listWorkAmounts <- function(searchConditionsList = NULL, WorkAmountID = F, PayScaleID = F, LowValueRange = F, FormattedLowValueRange = F, HighValueRange = F, FormattedHighValueRange = F, Value = F, FormattedValue = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, fieldPaths = NULL, searchConditionsGroupType = "And", searchSortFieldNamesList = NULL, searchSortFieldNamesDescendingList = NULL, entityId = 1, schoolYearId = NULL, page = 1, pageSize = 100000, flatten = T, returnResponse = F){

		params <- as.list(environment())

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		listSkyObjects(module = "SubstituteTracking", objectName = "WorkAmount", searchFields = searchFields %>% append(fieldPaths), page = page, pageSize = pageSize, SearchConditionsList = searchConditionsList, SearchConditionsGroupType = searchConditionsGroupType, SearchSortFieldNamesList = searchSortFieldNamesList, SearchSortFieldNamesDescendingList = searchSortFieldNamesDescendingList, entityId = entityId, schoolYearId = schoolYearId, flatten = flatten, returnResponse = returnResponse)
	}

	#' Get a WorkAmount
	#'
	#' This function returns a dataframe or json object of a WorkAmount
	#' @param WorkAmountID The ID of the WorkAmount to return.
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given WorkAmount. Defaults to FALSE for all return fields which, for convenience, returns all fields for the WorkAmount.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object listed as text. Run \code{\link{getSchemaForObjects}}('WorkAmount') to get more field paths.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{listSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Substitute Tracking
	#' @return A dataframe or of WorkAmount
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	getWorkAmount <- function(WorkAmountID, PayScaleID = F, LowValueRange = F, FormattedLowValueRange = F, HighValueRange = F, FormattedHighValueRange = F, Value = F, FormattedValue = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, fieldPaths = NULL, entityId = 1, schoolYearId = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment()) %>% purrr::keep(names(.) != "WorkAmountID")

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		getSkyObject(module = "SubstituteTracking", objectName = "WorkAmount", objectId = WorkAmountID, searchFields = searchFields, entityId = entityId, flatten = flatten, returnResponse = returnResponse)
	}

	#' Delete a WorkAmount
	#'
	#' This function deletes a WorkAmount
	#' @param WorkAmountID The ID of the WorkAmount to delete
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{listSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Substitute Tracking
	#' @return The WorkAmountID of the deleted WorkAmount.
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	deleteWorkAmount <- function(WorkAmountID, ignoreWarnings = F, entityId = 1, schoolYearId = NULL, flatten = T, returnResponse = F){

		deleteSkyObject(module = "SubstituteTracking", objectName = "WorkAmount", objectId = WorkAmountID, ignoreWarnings = ignoreWarnings, entityId = entityId, flatten = flatten, returnResponse = returnResponse)
	}

	#' Create a WorkAmount
	#'
	#' This function creates a WorkAmount
	#' @param fieldNames The field values to give the created WorkAmount. Each defaults to NULL.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{listSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Substitute Tracking
	#' @return A newly created WorkAmount
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	createWorkAmount <- function(PayScaleID = NULL, entityId = 1, schoolYearId = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment())

		body <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper()) %>% purrr::compact()

		createSkyObject(module = "SubstituteTracking", objectName = "WorkAmount", body = list(DataObject = body), searchFields = append("WorkAmountID", body %>% names()), entityId = entityId, schoolYearId = schoolYearId, flatten = flatten, returnResponse = returnResponse)
	}

	#' Modify a WorkAmount
	#'
	#' This function modifies a WorkAmount
	#' @param fieldNames The field values to give the modified WorkAmount. Each defaults to NULL.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{listSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Substitute Tracking
	#' @return The modified WorkAmount
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	modifyWorkAmount <- function(WorkAmountID, PayScaleID = NULL, entityId = 1, schoolYearId = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment())

		body <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper()) %>% purrr::compact()

		modifySkyObject(module = "SubstituteTracking", objectName = "WorkAmount", objectId = WorkAmountID, body = list(DataObject = body), searchFields = append("WorkAmountID", body %>% names()), entityId = entityId, schoolYearId = schoolYearId, flatten = flatten, returnResponse = returnResponse)
	}

	#' List TempImportExceptions
	#'
	#' This function returns a dataframe or json object of TempImportExceptions
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given TempImportExceptions. Defaults to FALSE for all return fields which, for convenience, returns all fields for the TempImportExceptions.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object listed as text. Run \code{\link{getSchemaForObjects}}('TempImportException') to get more field paths.
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
	#' @concept Substitute Tracking
	#' @return A list of TempImportExceptions
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	listTempImportExceptions <- function(searchConditionsList = NULL, TempImportExceptionID = F, LineNumber = F, Employee = F, Message = F, Date = F, RecordType = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, EmployeeNumber = F, fieldPaths = NULL, searchConditionsGroupType = "And", searchSortFieldNamesList = NULL, searchSortFieldNamesDescendingList = NULL, entityId = 1, schoolYearId = NULL, page = 1, pageSize = 100000, flatten = T, returnResponse = F){

		params <- as.list(environment())

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		listSkyObjects(module = "SubstituteTracking", objectName = "TempImportException", searchFields = searchFields %>% append(fieldPaths), page = page, pageSize = pageSize, SearchConditionsList = searchConditionsList, SearchConditionsGroupType = searchConditionsGroupType, SearchSortFieldNamesList = searchSortFieldNamesList, SearchSortFieldNamesDescendingList = searchSortFieldNamesDescendingList, entityId = entityId, schoolYearId = schoolYearId, flatten = flatten, returnResponse = returnResponse)
	}

	#' Get a TempImportException
	#'
	#' This function returns a dataframe or json object of a TempImportException
	#' @param TempImportExceptionID The ID of the TempImportException to return.
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given TempImportException. Defaults to FALSE for all return fields which, for convenience, returns all fields for the TempImportException.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object listed as text. Run \code{\link{getSchemaForObjects}}('TempImportException') to get more field paths.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{listSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Substitute Tracking
	#' @return A dataframe or of TempImportException
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	getTempImportException <- function(TempImportExceptionID, LineNumber = F, Employee = F, Message = F, Date = F, RecordType = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, EmployeeNumber = F, fieldPaths = NULL, entityId = 1, schoolYearId = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment()) %>% purrr::keep(names(.) != "TempImportExceptionID")

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		getSkyObject(module = "SubstituteTracking", objectName = "TempImportException", objectId = TempImportExceptionID, searchFields = searchFields, entityId = entityId, flatten = flatten, returnResponse = returnResponse)
	}

	#' Delete a TempImportException
	#'
	#' This function deletes a TempImportException
	#' @param TempImportExceptionID The ID of the TempImportException to delete
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{listSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Substitute Tracking
	#' @return The TempImportExceptionID of the deleted TempImportException.
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	deleteTempImportException <- function(TempImportExceptionID, ignoreWarnings = F, entityId = 1, schoolYearId = NULL, flatten = T, returnResponse = F){

		deleteSkyObject(module = "SubstituteTracking", objectName = "TempImportException", objectId = TempImportExceptionID, ignoreWarnings = ignoreWarnings, entityId = entityId, flatten = flatten, returnResponse = returnResponse)
	}

	#' Create a TempImportException
	#'
	#' This function creates a TempImportException
	#' @param fieldNames The field values to give the created TempImportException. Each defaults to NULL.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{listSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Substitute Tracking
	#' @return A newly created TempImportException
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	createTempImportException <- function(LineNumber = NULL, Employee = NULL, Message = NULL, Date = NULL, RecordType = NULL, EmployeeNumber = NULL, entityId = 1, schoolYearId = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment())

		body <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper()) %>% purrr::compact()

		createSkyObject(module = "SubstituteTracking", objectName = "TempImportException", body = list(DataObject = body), searchFields = append("TempImportExceptionID", body %>% names()), entityId = entityId, schoolYearId = schoolYearId, flatten = flatten, returnResponse = returnResponse)
	}

	#' Modify a TempImportException
	#'
	#' This function modifies a TempImportException
	#' @param fieldNames The field values to give the modified TempImportException. Each defaults to NULL.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{listSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Substitute Tracking
	#' @return The modified TempImportException
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	modifyTempImportException <- function(TempImportExceptionID, LineNumber = NULL, Employee = NULL, Message = NULL, Date = NULL, RecordType = NULL, EmployeeNumber = NULL, entityId = 1, schoolYearId = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment())

		body <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper()) %>% purrr::compact()

		modifySkyObject(module = "SubstituteTracking", objectName = "TempImportException", objectId = TempImportExceptionID, body = list(DataObject = body), searchFields = append("TempImportExceptionID", body %>% names()), entityId = entityId, schoolYearId = schoolYearId, flatten = flatten, returnResponse = returnResponse)
	}

	#' List PayScaleAbsentPayTypes
	#'
	#' This function returns a dataframe or json object of PayScaleAbsentPayTypes
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given PayScaleAbsentPayTypes. Defaults to FALSE for all return fields which, for convenience, returns all fields for the PayScaleAbsentPayTypes.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object listed as text. Run \code{\link{getSchemaForObjects}}('PayScaleAbsentPayType') to get more field paths.
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
	#' @concept Substitute Tracking
	#' @return A list of PayScaleAbsentPayTypes
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	listPayScaleAbsentPayTypes <- function(searchConditionsList = NULL, PayScaleAbsentPayTypeID = F, PayScaleID = F, PayTypeID = F, Rank = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, fieldPaths = NULL, searchConditionsGroupType = "And", searchSortFieldNamesList = NULL, searchSortFieldNamesDescendingList = NULL, entityId = 1, schoolYearId = NULL, page = 1, pageSize = 100000, flatten = T, returnResponse = F){

		params <- as.list(environment())

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		listSkyObjects(module = "SubstituteTracking", objectName = "PayScaleAbsentPayType", searchFields = searchFields %>% append(fieldPaths), page = page, pageSize = pageSize, SearchConditionsList = searchConditionsList, SearchConditionsGroupType = searchConditionsGroupType, SearchSortFieldNamesList = searchSortFieldNamesList, SearchSortFieldNamesDescendingList = searchSortFieldNamesDescendingList, entityId = entityId, schoolYearId = schoolYearId, flatten = flatten, returnResponse = returnResponse)
	}

	#' Get a PayScaleAbsentPayType
	#'
	#' This function returns a dataframe or json object of a PayScaleAbsentPayType
	#' @param PayScaleAbsentPayTypeID The ID of the PayScaleAbsentPayType to return.
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given PayScaleAbsentPayType. Defaults to FALSE for all return fields which, for convenience, returns all fields for the PayScaleAbsentPayType.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object listed as text. Run \code{\link{getSchemaForObjects}}('PayScaleAbsentPayType') to get more field paths.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{listSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Substitute Tracking
	#' @return A dataframe or of PayScaleAbsentPayType
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	getPayScaleAbsentPayType <- function(PayScaleAbsentPayTypeID, PayScaleID = F, PayTypeID = F, Rank = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, fieldPaths = NULL, entityId = 1, schoolYearId = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment()) %>% purrr::keep(names(.) != "PayScaleAbsentPayTypeID")

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		getSkyObject(module = "SubstituteTracking", objectName = "PayScaleAbsentPayType", objectId = PayScaleAbsentPayTypeID, searchFields = searchFields, entityId = entityId, flatten = flatten, returnResponse = returnResponse)
	}

	#' Delete a PayScaleAbsentPayType
	#'
	#' This function deletes a PayScaleAbsentPayType
	#' @param PayScaleAbsentPayTypeID The ID of the PayScaleAbsentPayType to delete
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{listSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Substitute Tracking
	#' @return The PayScaleAbsentPayTypeID of the deleted PayScaleAbsentPayType.
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	deletePayScaleAbsentPayType <- function(PayScaleAbsentPayTypeID, ignoreWarnings = F, entityId = 1, schoolYearId = NULL, flatten = T, returnResponse = F){

		deleteSkyObject(module = "SubstituteTracking", objectName = "PayScaleAbsentPayType", objectId = PayScaleAbsentPayTypeID, ignoreWarnings = ignoreWarnings, entityId = entityId, flatten = flatten, returnResponse = returnResponse)
	}

	#' Create a PayScaleAbsentPayType
	#'
	#' This function creates a PayScaleAbsentPayType
	#' @param fieldNames The field values to give the created PayScaleAbsentPayType. Each defaults to NULL.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{listSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Substitute Tracking
	#' @return A newly created PayScaleAbsentPayType
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	createPayScaleAbsentPayType <- function(PayScaleID = NULL, PayTypeID = NULL, Rank = NULL, entityId = 1, schoolYearId = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment())

		body <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper()) %>% purrr::compact()

		createSkyObject(module = "SubstituteTracking", objectName = "PayScaleAbsentPayType", body = list(DataObject = body), searchFields = append("PayScaleAbsentPayTypeID", body %>% names()), entityId = entityId, schoolYearId = schoolYearId, flatten = flatten, returnResponse = returnResponse)
	}

	#' Modify a PayScaleAbsentPayType
	#'
	#' This function modifies a PayScaleAbsentPayType
	#' @param fieldNames The field values to give the modified PayScaleAbsentPayType. Each defaults to NULL.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{listSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Substitute Tracking
	#' @return The modified PayScaleAbsentPayType
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	modifyPayScaleAbsentPayType <- function(PayScaleAbsentPayTypeID, PayScaleID = NULL, PayTypeID = NULL, Rank = NULL, entityId = 1, schoolYearId = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment())

		body <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper()) %>% purrr::compact()

		modifySkyObject(module = "SubstituteTracking", objectName = "PayScaleAbsentPayType", objectId = PayScaleAbsentPayTypeID, body = list(DataObject = body), searchFields = append("PayScaleAbsentPayTypeID", body %>% names()), entityId = entityId, schoolYearId = schoolYearId, flatten = flatten, returnResponse = returnResponse)
	}

	#' List ThirdPartyFormatTimeOffPositionTypes
	#'
	#' This function returns a dataframe or json object of ThirdPartyFormatTimeOffPositionTypes
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given ThirdPartyFormatTimeOffPositionTypes. Defaults to FALSE for all return fields which, for convenience, returns all fields for the ThirdPartyFormatTimeOffPositionTypes.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object listed as text. Run \code{\link{getSchemaForObjects}}('ThirdPartyFormatTimeOffPositionType') to get more field paths.
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
	#' @concept Substitute Tracking
	#' @return A list of ThirdPartyFormatTimeOffPositionTypes
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	listThirdPartyFormatTimeOffPositionTypes <- function(searchConditionsList = NULL, ThirdPartyFormatTimeOffPositionTypeID = F, ThirdPartyFormatID = F, PositionTypeID = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, fieldPaths = NULL, searchConditionsGroupType = "And", searchSortFieldNamesList = NULL, searchSortFieldNamesDescendingList = NULL, entityId = 1, schoolYearId = NULL, page = 1, pageSize = 100000, flatten = T, returnResponse = F){

		params <- as.list(environment())

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		listSkyObjects(module = "SubstituteTracking", objectName = "ThirdPartyFormatTimeOffPositionType", searchFields = searchFields %>% append(fieldPaths), page = page, pageSize = pageSize, SearchConditionsList = searchConditionsList, SearchConditionsGroupType = searchConditionsGroupType, SearchSortFieldNamesList = searchSortFieldNamesList, SearchSortFieldNamesDescendingList = searchSortFieldNamesDescendingList, entityId = entityId, schoolYearId = schoolYearId, flatten = flatten, returnResponse = returnResponse)
	}

	#' Get a ThirdPartyFormatTimeOffPositionType
	#'
	#' This function returns a dataframe or json object of a ThirdPartyFormatTimeOffPositionType
	#' @param ThirdPartyFormatTimeOffPositionTypeID The ID of the ThirdPartyFormatTimeOffPositionType to return.
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given ThirdPartyFormatTimeOffPositionType. Defaults to FALSE for all return fields which, for convenience, returns all fields for the ThirdPartyFormatTimeOffPositionType.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object listed as text. Run \code{\link{getSchemaForObjects}}('ThirdPartyFormatTimeOffPositionType') to get more field paths.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{listSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Substitute Tracking
	#' @return A dataframe or of ThirdPartyFormatTimeOffPositionType
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	getThirdPartyFormatTimeOffPositionType <- function(ThirdPartyFormatTimeOffPositionTypeID, ThirdPartyFormatID = F, PositionTypeID = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, fieldPaths = NULL, entityId = 1, schoolYearId = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment()) %>% purrr::keep(names(.) != "ThirdPartyFormatTimeOffPositionTypeID")

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		getSkyObject(module = "SubstituteTracking", objectName = "ThirdPartyFormatTimeOffPositionType", objectId = ThirdPartyFormatTimeOffPositionTypeID, searchFields = searchFields, entityId = entityId, flatten = flatten, returnResponse = returnResponse)
	}

	#' Delete a ThirdPartyFormatTimeOffPositionType
	#'
	#' This function deletes a ThirdPartyFormatTimeOffPositionType
	#' @param ThirdPartyFormatTimeOffPositionTypeID The ID of the ThirdPartyFormatTimeOffPositionType to delete
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{listSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Substitute Tracking
	#' @return The ThirdPartyFormatTimeOffPositionTypeID of the deleted ThirdPartyFormatTimeOffPositionType.
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	deleteThirdPartyFormatTimeOffPositionType <- function(ThirdPartyFormatTimeOffPositionTypeID, ignoreWarnings = F, entityId = 1, schoolYearId = NULL, flatten = T, returnResponse = F){

		deleteSkyObject(module = "SubstituteTracking", objectName = "ThirdPartyFormatTimeOffPositionType", objectId = ThirdPartyFormatTimeOffPositionTypeID, ignoreWarnings = ignoreWarnings, entityId = entityId, flatten = flatten, returnResponse = returnResponse)
	}

	#' Create a ThirdPartyFormatTimeOffPositionType
	#'
	#' This function creates a ThirdPartyFormatTimeOffPositionType
	#' @param fieldNames The field values to give the created ThirdPartyFormatTimeOffPositionType. Each defaults to NULL.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{listSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Substitute Tracking
	#' @return A newly created ThirdPartyFormatTimeOffPositionType
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	createThirdPartyFormatTimeOffPositionType <- function(ThirdPartyFormatID = NULL, PositionTypeID = NULL, entityId = 1, schoolYearId = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment())

		body <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper()) %>% purrr::compact()

		createSkyObject(module = "SubstituteTracking", objectName = "ThirdPartyFormatTimeOffPositionType", body = list(DataObject = body), searchFields = append("ThirdPartyFormatTimeOffPositionTypeID", body %>% names()), entityId = entityId, schoolYearId = schoolYearId, flatten = flatten, returnResponse = returnResponse)
	}

	#' Modify a ThirdPartyFormatTimeOffPositionType
	#'
	#' This function modifies a ThirdPartyFormatTimeOffPositionType
	#' @param fieldNames The field values to give the modified ThirdPartyFormatTimeOffPositionType. Each defaults to NULL.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{listSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Substitute Tracking
	#' @return The modified ThirdPartyFormatTimeOffPositionType
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	modifyThirdPartyFormatTimeOffPositionType <- function(ThirdPartyFormatTimeOffPositionTypeID, ThirdPartyFormatID = NULL, PositionTypeID = NULL, entityId = 1, schoolYearId = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment())

		body <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper()) %>% purrr::compact()

		modifySkyObject(module = "SubstituteTracking", objectName = "ThirdPartyFormatTimeOffPositionType", objectId = ThirdPartyFormatTimeOffPositionTypeID, body = list(DataObject = body), searchFields = append("ThirdPartyFormatTimeOffPositionTypeID", body %>% names()), entityId = entityId, schoolYearId = schoolYearId, flatten = flatten, returnResponse = returnResponse)
	}

	#' List ThirdPartyFormatTimeOffPositionTypeAmounts
	#'
	#' This function returns a dataframe or json object of ThirdPartyFormatTimeOffPositionTypeAmounts
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given ThirdPartyFormatTimeOffPositionTypeAmounts. Defaults to FALSE for all return fields which, for convenience, returns all fields for the ThirdPartyFormatTimeOffPositionTypeAmounts.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object listed as text. Run \code{\link{getSchemaForObjects}}('ThirdPartyFormatTimeOffPositionTypeAmount') to get more field paths.
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
	#' @concept Substitute Tracking
	#' @return A list of ThirdPartyFormatTimeOffPositionTypeAmounts
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	listThirdPartyFormatTimeOffPositionTypeAmounts <- function(searchConditionsList = NULL, ThirdPartyFormatTimeOffPositionTypeAmountID = F, ThirdPartyFormatTimeOffPositionTypeID = F, LowValueRange = F, FormattedLowValueRange = F, HighValueRange = F, FormattedHighValueRange = F, Value = F, FormattedValue = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, LowDayValueRange = F, HighDayValueRange = F, DayValue = F, fieldPaths = NULL, searchConditionsGroupType = "And", searchSortFieldNamesList = NULL, searchSortFieldNamesDescendingList = NULL, entityId = 1, schoolYearId = NULL, page = 1, pageSize = 100000, flatten = T, returnResponse = F){

		params <- as.list(environment())

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		listSkyObjects(module = "SubstituteTracking", objectName = "ThirdPartyFormatTimeOffPositionTypeAmount", searchFields = searchFields %>% append(fieldPaths), page = page, pageSize = pageSize, SearchConditionsList = searchConditionsList, SearchConditionsGroupType = searchConditionsGroupType, SearchSortFieldNamesList = searchSortFieldNamesList, SearchSortFieldNamesDescendingList = searchSortFieldNamesDescendingList, entityId = entityId, schoolYearId = schoolYearId, flatten = flatten, returnResponse = returnResponse)
	}

	#' Get a ThirdPartyFormatTimeOffPositionTypeAmount
	#'
	#' This function returns a dataframe or json object of a ThirdPartyFormatTimeOffPositionTypeAmount
	#' @param ThirdPartyFormatTimeOffPositionTypeAmountID The ID of the ThirdPartyFormatTimeOffPositionTypeAmount to return.
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given ThirdPartyFormatTimeOffPositionTypeAmount. Defaults to FALSE for all return fields which, for convenience, returns all fields for the ThirdPartyFormatTimeOffPositionTypeAmount.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object listed as text. Run \code{\link{getSchemaForObjects}}('ThirdPartyFormatTimeOffPositionTypeAmount') to get more field paths.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{listSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Substitute Tracking
	#' @return A dataframe or of ThirdPartyFormatTimeOffPositionTypeAmount
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	getThirdPartyFormatTimeOffPositionTypeAmount <- function(ThirdPartyFormatTimeOffPositionTypeAmountID, ThirdPartyFormatTimeOffPositionTypeID = F, LowValueRange = F, FormattedLowValueRange = F, HighValueRange = F, FormattedHighValueRange = F, Value = F, FormattedValue = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, LowDayValueRange = F, HighDayValueRange = F, DayValue = F, fieldPaths = NULL, entityId = 1, schoolYearId = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment()) %>% purrr::keep(names(.) != "ThirdPartyFormatTimeOffPositionTypeAmountID")

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		getSkyObject(module = "SubstituteTracking", objectName = "ThirdPartyFormatTimeOffPositionTypeAmount", objectId = ThirdPartyFormatTimeOffPositionTypeAmountID, searchFields = searchFields, entityId = entityId, flatten = flatten, returnResponse = returnResponse)
	}

	#' Delete a ThirdPartyFormatTimeOffPositionTypeAmount
	#'
	#' This function deletes a ThirdPartyFormatTimeOffPositionTypeAmount
	#' @param ThirdPartyFormatTimeOffPositionTypeAmountID The ID of the ThirdPartyFormatTimeOffPositionTypeAmount to delete
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{listSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Substitute Tracking
	#' @return The ThirdPartyFormatTimeOffPositionTypeAmountID of the deleted ThirdPartyFormatTimeOffPositionTypeAmount.
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	deleteThirdPartyFormatTimeOffPositionTypeAmount <- function(ThirdPartyFormatTimeOffPositionTypeAmountID, ignoreWarnings = F, entityId = 1, schoolYearId = NULL, flatten = T, returnResponse = F){

		deleteSkyObject(module = "SubstituteTracking", objectName = "ThirdPartyFormatTimeOffPositionTypeAmount", objectId = ThirdPartyFormatTimeOffPositionTypeAmountID, ignoreWarnings = ignoreWarnings, entityId = entityId, flatten = flatten, returnResponse = returnResponse)
	}

	#' Create a ThirdPartyFormatTimeOffPositionTypeAmount
	#'
	#' This function creates a ThirdPartyFormatTimeOffPositionTypeAmount
	#' @param fieldNames The field values to give the created ThirdPartyFormatTimeOffPositionTypeAmount. Each defaults to NULL.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{listSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Substitute Tracking
	#' @return A newly created ThirdPartyFormatTimeOffPositionTypeAmount
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	createThirdPartyFormatTimeOffPositionTypeAmount <- function(ThirdPartyFormatTimeOffPositionTypeID = NULL, LowDayValueRange = NULL, HighDayValueRange = NULL, DayValue = NULL, entityId = 1, schoolYearId = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment())

		body <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper()) %>% purrr::compact()

		createSkyObject(module = "SubstituteTracking", objectName = "ThirdPartyFormatTimeOffPositionTypeAmount", body = list(DataObject = body), searchFields = append("ThirdPartyFormatTimeOffPositionTypeAmountID", body %>% names()), entityId = entityId, schoolYearId = schoolYearId, flatten = flatten, returnResponse = returnResponse)
	}

	#' Modify a ThirdPartyFormatTimeOffPositionTypeAmount
	#'
	#' This function modifies a ThirdPartyFormatTimeOffPositionTypeAmount
	#' @param fieldNames The field values to give the modified ThirdPartyFormatTimeOffPositionTypeAmount. Each defaults to NULL.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{listSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Substitute Tracking
	#' @return The modified ThirdPartyFormatTimeOffPositionTypeAmount
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	modifyThirdPartyFormatTimeOffPositionTypeAmount <- function(ThirdPartyFormatTimeOffPositionTypeAmountID, ThirdPartyFormatTimeOffPositionTypeID = NULL, LowDayValueRange = NULL, HighDayValueRange = NULL, DayValue = NULL, entityId = 1, schoolYearId = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment())

		body <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper()) %>% purrr::compact()

		modifySkyObject(module = "SubstituteTracking", objectName = "ThirdPartyFormatTimeOffPositionTypeAmount", objectId = ThirdPartyFormatTimeOffPositionTypeAmountID, body = list(DataObject = body), searchFields = append("ThirdPartyFormatTimeOffPositionTypeAmountID", body %>% names()), entityId = entityId, schoolYearId = schoolYearId, flatten = flatten, returnResponse = returnResponse)
	}

	#' List TempSubstitutePayScaleClasses
	#'
	#' This function returns a dataframe or json object of TempSubstitutePayScaleClasses
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given TempSubstitutePayScaleClasses. Defaults to FALSE for all return fields which, for convenience, returns all fields for the TempSubstitutePayScaleClasses.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object listed as text. Run \code{\link{getSchemaForObjects}}('TempSubstitutePayScaleClass') to get more field paths.
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
	#' @concept Substitute Tracking
	#' @return A list of TempSubstitutePayScaleClasses
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	listTempSubstitutePayScaleClasses <- function(searchConditionsList = NULL, TempSubstitutePayScaleClassID = F, TempSubstitutePayScaleID = F, SubstitutePayScaleClassIDClonedFrom = F, PayScaleClassID = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, StartDate = F, EndDate = F, fieldPaths = NULL, searchConditionsGroupType = "And", searchSortFieldNamesList = NULL, searchSortFieldNamesDescendingList = NULL, entityId = 1, schoolYearId = NULL, page = 1, pageSize = 100000, flatten = T, returnResponse = F){

		params <- as.list(environment())

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		listSkyObjects(module = "SubstituteTracking", objectName = "TempSubstitutePayScaleClass", searchFields = searchFields %>% append(fieldPaths), page = page, pageSize = pageSize, SearchConditionsList = searchConditionsList, SearchConditionsGroupType = searchConditionsGroupType, SearchSortFieldNamesList = searchSortFieldNamesList, SearchSortFieldNamesDescendingList = searchSortFieldNamesDescendingList, entityId = entityId, schoolYearId = schoolYearId, flatten = flatten, returnResponse = returnResponse)
	}

	#' Get a TempSubstitutePayScaleClass
	#'
	#' This function returns a dataframe or json object of a TempSubstitutePayScaleClass
	#' @param TempSubstitutePayScaleClassID The ID of the TempSubstitutePayScaleClass to return.
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given TempSubstitutePayScaleClass. Defaults to FALSE for all return fields which, for convenience, returns all fields for the TempSubstitutePayScaleClass.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object listed as text. Run \code{\link{getSchemaForObjects}}('TempSubstitutePayScaleClass') to get more field paths.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{listSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Substitute Tracking
	#' @return A dataframe or of TempSubstitutePayScaleClass
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	getTempSubstitutePayScaleClass <- function(TempSubstitutePayScaleClassID, TempSubstitutePayScaleID = F, SubstitutePayScaleClassIDClonedFrom = F, PayScaleClassID = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, StartDate = F, EndDate = F, fieldPaths = NULL, entityId = 1, schoolYearId = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment()) %>% purrr::keep(names(.) != "TempSubstitutePayScaleClassID")

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		getSkyObject(module = "SubstituteTracking", objectName = "TempSubstitutePayScaleClass", objectId = TempSubstitutePayScaleClassID, searchFields = searchFields, entityId = entityId, flatten = flatten, returnResponse = returnResponse)
	}

	#' Delete a TempSubstitutePayScaleClass
	#'
	#' This function deletes a TempSubstitutePayScaleClass
	#' @param TempSubstitutePayScaleClassID The ID of the TempSubstitutePayScaleClass to delete
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{listSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Substitute Tracking
	#' @return The TempSubstitutePayScaleClassID of the deleted TempSubstitutePayScaleClass.
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	deleteTempSubstitutePayScaleClass <- function(TempSubstitutePayScaleClassID, ignoreWarnings = F, entityId = 1, schoolYearId = NULL, flatten = T, returnResponse = F){

		deleteSkyObject(module = "SubstituteTracking", objectName = "TempSubstitutePayScaleClass", objectId = TempSubstitutePayScaleClassID, ignoreWarnings = ignoreWarnings, entityId = entityId, flatten = flatten, returnResponse = returnResponse)
	}

	#' Create a TempSubstitutePayScaleClass
	#'
	#' This function creates a TempSubstitutePayScaleClass
	#' @param fieldNames The field values to give the created TempSubstitutePayScaleClass. Each defaults to NULL.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{listSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Substitute Tracking
	#' @return A newly created TempSubstitutePayScaleClass
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	createTempSubstitutePayScaleClass <- function(TempSubstitutePayScaleID = NULL, SubstitutePayScaleClassIDClonedFrom = NULL, PayScaleClassID = NULL, StartDate = NULL, EndDate = NULL, entityId = 1, schoolYearId = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment())

		body <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper()) %>% purrr::compact()

		createSkyObject(module = "SubstituteTracking", objectName = "TempSubstitutePayScaleClass", body = list(DataObject = body), searchFields = append("TempSubstitutePayScaleClassID", body %>% names()), entityId = entityId, schoolYearId = schoolYearId, flatten = flatten, returnResponse = returnResponse)
	}

	#' Modify a TempSubstitutePayScaleClass
	#'
	#' This function modifies a TempSubstitutePayScaleClass
	#' @param fieldNames The field values to give the modified TempSubstitutePayScaleClass. Each defaults to NULL.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{listSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Substitute Tracking
	#' @return The modified TempSubstitutePayScaleClass
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	modifyTempSubstitutePayScaleClass <- function(TempSubstitutePayScaleClassID, TempSubstitutePayScaleID = NULL, SubstitutePayScaleClassIDClonedFrom = NULL, PayScaleClassID = NULL, StartDate = NULL, EndDate = NULL, entityId = 1, schoolYearId = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment())

		body <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper()) %>% purrr::compact()

		modifySkyObject(module = "SubstituteTracking", objectName = "TempSubstitutePayScaleClass", objectId = TempSubstitutePayScaleClassID, body = list(DataObject = body), searchFields = append("TempSubstitutePayScaleClassID", body %>% names()), entityId = entityId, schoolYearId = schoolYearId, flatten = flatten, returnResponse = returnResponse)
	}

	#' List ThirdPartyFormatPositionTypes
	#'
	#' This function returns a dataframe or json object of ThirdPartyFormatPositionTypes
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given ThirdPartyFormatPositionTypes. Defaults to FALSE for all return fields which, for convenience, returns all fields for the ThirdPartyFormatPositionTypes.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object listed as text. Run \code{\link{getSchemaForObjects}}('ThirdPartyFormatPositionType') to get more field paths.
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
	#' @concept Substitute Tracking
	#' @return A list of ThirdPartyFormatPositionTypes
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	listThirdPartyFormatPositionTypes <- function(searchConditionsList = NULL, ThirdPartyFormatPositionTypeID = F, ThirdPartyFormatID = F, PositionTypeID = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, fieldPaths = NULL, searchConditionsGroupType = "And", searchSortFieldNamesList = NULL, searchSortFieldNamesDescendingList = NULL, entityId = 1, schoolYearId = NULL, page = 1, pageSize = 100000, flatten = T, returnResponse = F){

		params <- as.list(environment())

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		listSkyObjects(module = "SubstituteTracking", objectName = "ThirdPartyFormatPositionType", searchFields = searchFields %>% append(fieldPaths), page = page, pageSize = pageSize, SearchConditionsList = searchConditionsList, SearchConditionsGroupType = searchConditionsGroupType, SearchSortFieldNamesList = searchSortFieldNamesList, SearchSortFieldNamesDescendingList = searchSortFieldNamesDescendingList, entityId = entityId, schoolYearId = schoolYearId, flatten = flatten, returnResponse = returnResponse)
	}

	#' Get a ThirdPartyFormatPositionType
	#'
	#' This function returns a dataframe or json object of a ThirdPartyFormatPositionType
	#' @param ThirdPartyFormatPositionTypeID The ID of the ThirdPartyFormatPositionType to return.
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given ThirdPartyFormatPositionType. Defaults to FALSE for all return fields which, for convenience, returns all fields for the ThirdPartyFormatPositionType.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object listed as text. Run \code{\link{getSchemaForObjects}}('ThirdPartyFormatPositionType') to get more field paths.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{listSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Substitute Tracking
	#' @return A dataframe or of ThirdPartyFormatPositionType
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	getThirdPartyFormatPositionType <- function(ThirdPartyFormatPositionTypeID, ThirdPartyFormatID = F, PositionTypeID = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, fieldPaths = NULL, entityId = 1, schoolYearId = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment()) %>% purrr::keep(names(.) != "ThirdPartyFormatPositionTypeID")

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		getSkyObject(module = "SubstituteTracking", objectName = "ThirdPartyFormatPositionType", objectId = ThirdPartyFormatPositionTypeID, searchFields = searchFields, entityId = entityId, flatten = flatten, returnResponse = returnResponse)
	}

	#' Delete a ThirdPartyFormatPositionType
	#'
	#' This function deletes a ThirdPartyFormatPositionType
	#' @param ThirdPartyFormatPositionTypeID The ID of the ThirdPartyFormatPositionType to delete
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{listSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Substitute Tracking
	#' @return The ThirdPartyFormatPositionTypeID of the deleted ThirdPartyFormatPositionType.
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	deleteThirdPartyFormatPositionType <- function(ThirdPartyFormatPositionTypeID, ignoreWarnings = F, entityId = 1, schoolYearId = NULL, flatten = T, returnResponse = F){

		deleteSkyObject(module = "SubstituteTracking", objectName = "ThirdPartyFormatPositionType", objectId = ThirdPartyFormatPositionTypeID, ignoreWarnings = ignoreWarnings, entityId = entityId, flatten = flatten, returnResponse = returnResponse)
	}

	#' Create a ThirdPartyFormatPositionType
	#'
	#' This function creates a ThirdPartyFormatPositionType
	#' @param fieldNames The field values to give the created ThirdPartyFormatPositionType. Each defaults to NULL.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{listSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Substitute Tracking
	#' @return A newly created ThirdPartyFormatPositionType
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	createThirdPartyFormatPositionType <- function(ThirdPartyFormatID = NULL, PositionTypeID = NULL, entityId = 1, schoolYearId = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment())

		body <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper()) %>% purrr::compact()

		createSkyObject(module = "SubstituteTracking", objectName = "ThirdPartyFormatPositionType", body = list(DataObject = body), searchFields = append("ThirdPartyFormatPositionTypeID", body %>% names()), entityId = entityId, schoolYearId = schoolYearId, flatten = flatten, returnResponse = returnResponse)
	}

	#' Modify a ThirdPartyFormatPositionType
	#'
	#' This function modifies a ThirdPartyFormatPositionType
	#' @param fieldNames The field values to give the modified ThirdPartyFormatPositionType. Each defaults to NULL.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{listSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Substitute Tracking
	#' @return The modified ThirdPartyFormatPositionType
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	modifyThirdPartyFormatPositionType <- function(ThirdPartyFormatPositionTypeID, ThirdPartyFormatID = NULL, PositionTypeID = NULL, entityId = 1, schoolYearId = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment())

		body <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper()) %>% purrr::compact()

		modifySkyObject(module = "SubstituteTracking", objectName = "ThirdPartyFormatPositionType", objectId = ThirdPartyFormatPositionTypeID, body = list(DataObject = body), searchFields = append("ThirdPartyFormatPositionTypeID", body %>% names()), entityId = entityId, schoolYearId = schoolYearId, flatten = flatten, returnResponse = returnResponse)
	}

	#' List ThirdPartyFormatAccounts
	#'
	#' This function returns a dataframe or json object of ThirdPartyFormatAccounts
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given ThirdPartyFormatAccounts. Defaults to FALSE for all return fields which, for convenience, returns all fields for the ThirdPartyFormatAccounts.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object listed as text. Run \code{\link{getSchemaForObjects}}('ThirdPartyFormatAccount') to get more field paths.
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
	#' @concept Substitute Tracking
	#' @return A list of ThirdPartyFormatAccounts
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	listThirdPartyFormatAccounts <- function(searchConditionsList = NULL, ThirdPartyFormatAccountID = F, ThirdPartyFormatID = F, ImportValue = F, AccountDistributionString = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, fieldPaths = NULL, searchConditionsGroupType = "And", searchSortFieldNamesList = NULL, searchSortFieldNamesDescendingList = NULL, entityId = 1, schoolYearId = NULL, page = 1, pageSize = 100000, flatten = T, returnResponse = F){

		params <- as.list(environment())

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		listSkyObjects(module = "SubstituteTracking", objectName = "ThirdPartyFormatAccount", searchFields = searchFields %>% append(fieldPaths), page = page, pageSize = pageSize, SearchConditionsList = searchConditionsList, SearchConditionsGroupType = searchConditionsGroupType, SearchSortFieldNamesList = searchSortFieldNamesList, SearchSortFieldNamesDescendingList = searchSortFieldNamesDescendingList, entityId = entityId, schoolYearId = schoolYearId, flatten = flatten, returnResponse = returnResponse)
	}

	#' Get a ThirdPartyFormatAccount
	#'
	#' This function returns a dataframe or json object of a ThirdPartyFormatAccount
	#' @param ThirdPartyFormatAccountID The ID of the ThirdPartyFormatAccount to return.
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given ThirdPartyFormatAccount. Defaults to FALSE for all return fields which, for convenience, returns all fields for the ThirdPartyFormatAccount.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object listed as text. Run \code{\link{getSchemaForObjects}}('ThirdPartyFormatAccount') to get more field paths.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{listSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Substitute Tracking
	#' @return A dataframe or of ThirdPartyFormatAccount
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	getThirdPartyFormatAccount <- function(ThirdPartyFormatAccountID, ThirdPartyFormatID = F, ImportValue = F, AccountDistributionString = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, fieldPaths = NULL, entityId = 1, schoolYearId = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment()) %>% purrr::keep(names(.) != "ThirdPartyFormatAccountID")

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		getSkyObject(module = "SubstituteTracking", objectName = "ThirdPartyFormatAccount", objectId = ThirdPartyFormatAccountID, searchFields = searchFields, entityId = entityId, flatten = flatten, returnResponse = returnResponse)
	}

	#' Delete a ThirdPartyFormatAccount
	#'
	#' This function deletes a ThirdPartyFormatAccount
	#' @param ThirdPartyFormatAccountID The ID of the ThirdPartyFormatAccount to delete
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{listSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Substitute Tracking
	#' @return The ThirdPartyFormatAccountID of the deleted ThirdPartyFormatAccount.
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	deleteThirdPartyFormatAccount <- function(ThirdPartyFormatAccountID, ignoreWarnings = F, entityId = 1, schoolYearId = NULL, flatten = T, returnResponse = F){

		deleteSkyObject(module = "SubstituteTracking", objectName = "ThirdPartyFormatAccount", objectId = ThirdPartyFormatAccountID, ignoreWarnings = ignoreWarnings, entityId = entityId, flatten = flatten, returnResponse = returnResponse)
	}

	#' Create a ThirdPartyFormatAccount
	#'
	#' This function creates a ThirdPartyFormatAccount
	#' @param fieldNames The field values to give the created ThirdPartyFormatAccount. Each defaults to NULL.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{listSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Substitute Tracking
	#' @return A newly created ThirdPartyFormatAccount
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	createThirdPartyFormatAccount <- function(ThirdPartyFormatID = NULL, ImportValue = NULL, AccountDistributionString = NULL, entityId = 1, schoolYearId = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment())

		body <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper()) %>% purrr::compact()

		createSkyObject(module = "SubstituteTracking", objectName = "ThirdPartyFormatAccount", body = list(DataObject = body), searchFields = append("ThirdPartyFormatAccountID", body %>% names()), entityId = entityId, schoolYearId = schoolYearId, flatten = flatten, returnResponse = returnResponse)
	}

	#' Modify a ThirdPartyFormatAccount
	#'
	#' This function modifies a ThirdPartyFormatAccount
	#' @param fieldNames The field values to give the modified ThirdPartyFormatAccount. Each defaults to NULL.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{listSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Substitute Tracking
	#' @return The modified ThirdPartyFormatAccount
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	modifyThirdPartyFormatAccount <- function(ThirdPartyFormatAccountID, ThirdPartyFormatID = NULL, ImportValue = NULL, AccountDistributionString = NULL, entityId = 1, schoolYearId = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment())

		body <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper()) %>% purrr::compact()

		modifySkyObject(module = "SubstituteTracking", objectName = "ThirdPartyFormatAccount", objectId = ThirdPartyFormatAccountID, body = list(DataObject = body), searchFields = append("ThirdPartyFormatAccountID", body %>% names()), entityId = entityId, schoolYearId = schoolYearId, flatten = flatten, returnResponse = returnResponse)
	}

	#' List ThirdPartyFormatAccountDistributions
	#'
	#' This function returns a dataframe or json object of ThirdPartyFormatAccountDistributions
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given ThirdPartyFormatAccountDistributions. Defaults to FALSE for all return fields which, for convenience, returns all fields for the ThirdPartyFormatAccountDistributions.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object listed as text. Run \code{\link{getSchemaForObjects}}('ThirdPartyFormatAccountDistribution') to get more field paths.
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
	#' @concept Substitute Tracking
	#' @return A list of ThirdPartyFormatAccountDistributions
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	listThirdPartyFormatAccountDistributions <- function(searchConditionsList = NULL, ThirdPartyFormatAccountDistributionID = F, ThirdPartyFormatAccountID = F, AccountID = F, DistributionPercent = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, fieldPaths = NULL, searchConditionsGroupType = "And", searchSortFieldNamesList = NULL, searchSortFieldNamesDescendingList = NULL, entityId = 1, schoolYearId = NULL, page = 1, pageSize = 100000, flatten = T, returnResponse = F){

		params <- as.list(environment())

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		listSkyObjects(module = "SubstituteTracking", objectName = "ThirdPartyFormatAccountDistribution", searchFields = searchFields %>% append(fieldPaths), page = page, pageSize = pageSize, SearchConditionsList = searchConditionsList, SearchConditionsGroupType = searchConditionsGroupType, SearchSortFieldNamesList = searchSortFieldNamesList, SearchSortFieldNamesDescendingList = searchSortFieldNamesDescendingList, entityId = entityId, schoolYearId = schoolYearId, flatten = flatten, returnResponse = returnResponse)
	}

	#' Get a ThirdPartyFormatAccountDistribution
	#'
	#' This function returns a dataframe or json object of a ThirdPartyFormatAccountDistribution
	#' @param ThirdPartyFormatAccountDistributionID The ID of the ThirdPartyFormatAccountDistribution to return.
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given ThirdPartyFormatAccountDistribution. Defaults to FALSE for all return fields which, for convenience, returns all fields for the ThirdPartyFormatAccountDistribution.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object listed as text. Run \code{\link{getSchemaForObjects}}('ThirdPartyFormatAccountDistribution') to get more field paths.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{listSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Substitute Tracking
	#' @return A dataframe or of ThirdPartyFormatAccountDistribution
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	getThirdPartyFormatAccountDistribution <- function(ThirdPartyFormatAccountDistributionID, ThirdPartyFormatAccountID = F, AccountID = F, DistributionPercent = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, fieldPaths = NULL, entityId = 1, schoolYearId = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment()) %>% purrr::keep(names(.) != "ThirdPartyFormatAccountDistributionID")

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		getSkyObject(module = "SubstituteTracking", objectName = "ThirdPartyFormatAccountDistribution", objectId = ThirdPartyFormatAccountDistributionID, searchFields = searchFields, entityId = entityId, flatten = flatten, returnResponse = returnResponse)
	}

	#' Delete a ThirdPartyFormatAccountDistribution
	#'
	#' This function deletes a ThirdPartyFormatAccountDistribution
	#' @param ThirdPartyFormatAccountDistributionID The ID of the ThirdPartyFormatAccountDistribution to delete
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{listSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Substitute Tracking
	#' @return The ThirdPartyFormatAccountDistributionID of the deleted ThirdPartyFormatAccountDistribution.
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	deleteThirdPartyFormatAccountDistribution <- function(ThirdPartyFormatAccountDistributionID, ignoreWarnings = F, entityId = 1, schoolYearId = NULL, flatten = T, returnResponse = F){

		deleteSkyObject(module = "SubstituteTracking", objectName = "ThirdPartyFormatAccountDistribution", objectId = ThirdPartyFormatAccountDistributionID, ignoreWarnings = ignoreWarnings, entityId = entityId, flatten = flatten, returnResponse = returnResponse)
	}

	#' Create a ThirdPartyFormatAccountDistribution
	#'
	#' This function creates a ThirdPartyFormatAccountDistribution
	#' @param fieldNames The field values to give the created ThirdPartyFormatAccountDistribution. Each defaults to NULL.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{listSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Substitute Tracking
	#' @return A newly created ThirdPartyFormatAccountDistribution
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	createThirdPartyFormatAccountDistribution <- function(ThirdPartyFormatAccountID = NULL, AccountID = NULL, DistributionPercent = NULL, entityId = 1, schoolYearId = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment())

		body <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper()) %>% purrr::compact()

		createSkyObject(module = "SubstituteTracking", objectName = "ThirdPartyFormatAccountDistribution", body = list(DataObject = body), searchFields = append("ThirdPartyFormatAccountDistributionID", body %>% names()), entityId = entityId, schoolYearId = schoolYearId, flatten = flatten, returnResponse = returnResponse)
	}

	#' Modify a ThirdPartyFormatAccountDistribution
	#'
	#' This function modifies a ThirdPartyFormatAccountDistribution
	#' @param fieldNames The field values to give the modified ThirdPartyFormatAccountDistribution. Each defaults to NULL.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{listSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Substitute Tracking
	#' @return The modified ThirdPartyFormatAccountDistribution
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	modifyThirdPartyFormatAccountDistribution <- function(ThirdPartyFormatAccountDistributionID, ThirdPartyFormatAccountID = NULL, AccountID = NULL, DistributionPercent = NULL, entityId = 1, schoolYearId = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment())

		body <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper()) %>% purrr::compact()

		modifySkyObject(module = "SubstituteTracking", objectName = "ThirdPartyFormatAccountDistribution", objectId = ThirdPartyFormatAccountDistributionID, body = list(DataObject = body), searchFields = append("ThirdPartyFormatAccountDistributionID", body %>% names()), entityId = entityId, schoolYearId = schoolYearId, flatten = flatten, returnResponse = returnResponse)
	}

	#' List ExcelFileFormats
	#'
	#' This function returns a dataframe or json object of ExcelFileFormats
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given ExcelFileFormats. Defaults to FALSE for all return fields which, for convenience, returns all fields for the ExcelFileFormats.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object listed as text. Run \code{\link{getSchemaForObjects}}('ExcelFileFormat') to get more field paths.
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
	#' @concept Substitute Tracking
	#' @return A list of ExcelFileFormats
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	listExcelFileFormats <- function(searchConditionsList = NULL, ExcelFileFormatID = F, SkywardID = F, SkywardHash = F, ThirdPartyFormatID = F, NumberOfHeaderRows = F, SubstituteDurationColumnNumber = F, AbsentDurationColumnNumber = F, SubstituteEmployeeColumnNumber = F, AbsentEmployeeColumnNumber = F, TimeOffTypeColumnNumber = F, BuildingColumnNumber = F, DateColumnNumber = F, AccountColumnNumber = F, TimeOffDescriptionColumnNumber = F, ExceptionHandlingType = F, ExceptionExportType = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, fieldPaths = NULL, searchConditionsGroupType = "And", searchSortFieldNamesList = NULL, searchSortFieldNamesDescendingList = NULL, entityId = 1, schoolYearId = NULL, page = 1, pageSize = 100000, flatten = T, returnResponse = F){

		params <- as.list(environment())

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		listSkyObjects(module = "SubstituteTracking", objectName = "ExcelFileFormat", searchFields = searchFields %>% append(fieldPaths), page = page, pageSize = pageSize, SearchConditionsList = searchConditionsList, SearchConditionsGroupType = searchConditionsGroupType, SearchSortFieldNamesList = searchSortFieldNamesList, SearchSortFieldNamesDescendingList = searchSortFieldNamesDescendingList, entityId = entityId, schoolYearId = schoolYearId, flatten = flatten, returnResponse = returnResponse)
	}

	#' Get an ExcelFileFormat
	#'
	#' This function returns a dataframe or json object of an ExcelFileFormat
	#' @param ExcelFileFormatID The ID of the ExcelFileFormat to return.
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given ExcelFileFormat. Defaults to FALSE for all return fields which, for convenience, returns all fields for the ExcelFileFormat.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object listed as text. Run \code{\link{getSchemaForObjects}}('ExcelFileFormat') to get more field paths.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{listSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Substitute Tracking
	#' @return A dataframe or of ExcelFileFormat
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	getExcelFileFormat <- function(ExcelFileFormatID, SkywardID = F, SkywardHash = F, ThirdPartyFormatID = F, NumberOfHeaderRows = F, SubstituteDurationColumnNumber = F, AbsentDurationColumnNumber = F, SubstituteEmployeeColumnNumber = F, AbsentEmployeeColumnNumber = F, TimeOffTypeColumnNumber = F, BuildingColumnNumber = F, DateColumnNumber = F, AccountColumnNumber = F, TimeOffDescriptionColumnNumber = F, ExceptionHandlingType = F, ExceptionExportType = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, fieldPaths = NULL, entityId = 1, schoolYearId = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment()) %>% purrr::keep(names(.) != "ExcelFileFormatID")

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		getSkyObject(module = "SubstituteTracking", objectName = "ExcelFileFormat", objectId = ExcelFileFormatID, searchFields = searchFields, entityId = entityId, flatten = flatten, returnResponse = returnResponse)
	}

	#' Delete an ExcelFileFormat
	#'
	#' This function deletes an ExcelFileFormat
	#' @param ExcelFileFormatID The ID of the ExcelFileFormat to delete
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{listSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Substitute Tracking
	#' @return The ExcelFileFormatID of the deleted ExcelFileFormat.
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	deleteExcelFileFormat <- function(ExcelFileFormatID, ignoreWarnings = F, entityId = 1, schoolYearId = NULL, flatten = T, returnResponse = F){

		deleteSkyObject(module = "SubstituteTracking", objectName = "ExcelFileFormat", objectId = ExcelFileFormatID, ignoreWarnings = ignoreWarnings, entityId = entityId, flatten = flatten, returnResponse = returnResponse)
	}

	#' Create an ExcelFileFormat
	#'
	#' This function creates an ExcelFileFormat
	#' @param fieldNames The field values to give the created ExcelFileFormat. Each defaults to NULL.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{listSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Substitute Tracking
	#' @return A newly created ExcelFileFormat
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	createExcelFileFormat <- function(ThirdPartyFormatID = NULL, NumberOfHeaderRows = NULL, SubstituteDurationColumnNumber = NULL, AbsentDurationColumnNumber = NULL, SubstituteEmployeeColumnNumber = NULL, AbsentEmployeeColumnNumber = NULL, TimeOffTypeColumnNumber = NULL, BuildingColumnNumber = NULL, DateColumnNumber = NULL, AccountColumnNumber = NULL, TimeOffDescriptionColumnNumber = NULL, ExceptionHandlingType = NULL, ExceptionExportType = NULL, entityId = 1, schoolYearId = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment())

		body <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper()) %>% purrr::compact()

		createSkyObject(module = "SubstituteTracking", objectName = "ExcelFileFormat", body = list(DataObject = body), searchFields = append("ExcelFileFormatID", body %>% names()), entityId = entityId, schoolYearId = schoolYearId, flatten = flatten, returnResponse = returnResponse)
	}

	#' Modify an ExcelFileFormat
	#'
	#' This function modifies an ExcelFileFormat
	#' @param fieldNames The field values to give the modified ExcelFileFormat. Each defaults to NULL.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{listSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Substitute Tracking
	#' @return The modified ExcelFileFormat
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	modifyExcelFileFormat <- function(ExcelFileFormatID, ThirdPartyFormatID = NULL, NumberOfHeaderRows = NULL, SubstituteDurationColumnNumber = NULL, AbsentDurationColumnNumber = NULL, SubstituteEmployeeColumnNumber = NULL, AbsentEmployeeColumnNumber = NULL, TimeOffTypeColumnNumber = NULL, BuildingColumnNumber = NULL, DateColumnNumber = NULL, AccountColumnNumber = NULL, TimeOffDescriptionColumnNumber = NULL, ExceptionHandlingType = NULL, ExceptionExportType = NULL, entityId = 1, schoolYearId = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment())

		body <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper()) %>% purrr::compact()

		modifySkyObject(module = "SubstituteTracking", objectName = "ExcelFileFormat", objectId = ExcelFileFormatID, body = list(DataObject = body), searchFields = append("ExcelFileFormatID", body %>% names()), entityId = entityId, schoolYearId = schoolYearId, flatten = flatten, returnResponse = returnResponse)
	}
