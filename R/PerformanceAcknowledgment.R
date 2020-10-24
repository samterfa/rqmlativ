
	#' List TempCertificationLicensures
	#'
	#' This function returns a dataframe or json object of TempCertificationLicensures
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given TempCertificationLicensures. Defaults to FALSE for all return fields which, for convenience, returns all fields for the TempCertificationLicensures.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object listed as text. Run \code{\link{getSchemaForObjects}}('TempCertificationLicensure') to get more field paths.
	#' @param searchConditionsList A list of search conditions to filter results which are joined by the searchConditionsGroupType. Of the form {FieldName} {ConditionType} {SearchCondition}. For example, c('StudentID LessEqual 500', 'LastName Like Ander\%'). Run \code{\link{listSearchConditionTypes}} for a list of ConditionTypes. Defaults to NULL (unfiltered).
	#' @param searchConditionsGroupType The conjunction which joins multiple searchConditions in the searchConditionsList. Either 'Or' or 'And'. Defaults to 'And'.
	#' @param searchSortFieldNamesList The list of fields sort results by. Defaults to NULL (unsorted).
	#' @param searchSortFieldNamesDescendingList A list of T/F values corresponding to whether to sort each field in searchSortFieldNamesList in descending order. Defaults to F for each FieldName in searchSortFieldNamesList.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param page Results are paginated. The page of results to return. Default is 1.
	#' @param pageSize Results are paginated. The number of records per page to return. Default is 100,000 (essentially all records for most objects).
	#' @param flatten Whether to flatten results into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept PerformanceAcknowledgment
	#' @return A list of TempCertificationLicensures
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	listTempCertificationLicensures <- function(searchConditionsList = NULL, TempCertificationLicensureID = F, CertificationLicensureID = F, DistrictID = F, StudentID = F, StudentNameLFMS = F, CertificationLicensureDescriptionID = F, CertificationLicensureDescription = F, MetDate = F, WillUpdateExistingRecord = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, fieldPaths = NULL, searchConditionsGroupType = "And", searchSortFieldNamesList = NULL, searchSortFieldNamesDescendingList = NULL, entityId = 1, query = NULL, page = 1, pageSize = 100000, flatten = T, returnResponse = F){

		params <- as.list(environment())

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		listSkyObjects(module = "PerformanceAcknowledgment", objectName = "TempCertificationLicensure", searchFields = searchFields %>% append(fieldPaths), page = page, pageSize = pageSize, SearchConditionsList = searchConditionsList, SearchConditionsGroupType = searchConditionsGroupType, SearchSortFieldNamesList = searchSortFieldNamesList, SearchSortFieldNamesDescendingList = searchSortFieldNamesDescendingList, entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' Get a TempCertificationLicensure
	#'
	#' This function returns a dataframe or json object of a TempCertificationLicensure
	#' @param TempCertificationLicensureID The ID of the TempCertificationLicensure to return.
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given TempCertificationLicensure. Defaults to FALSE for all return fields which, for convenience, returns all fields for the TempCertificationLicensure.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object listed as text. Run \code{\link{getSchemaForObjects}}('TempCertificationLicensure') to get more field paths.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept PerformanceAcknowledgment
	#' @return A dataframe or of TempCertificationLicensure
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	getTempCertificationLicensure <- function(TempCertificationLicensureID, CertificationLicensureID = F, DistrictID = F, StudentID = F, StudentNameLFMS = F, CertificationLicensureDescriptionID = F, CertificationLicensureDescription = F, MetDate = F, WillUpdateExistingRecord = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, fieldPaths = NULL, entityId = 1, query = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment()) %>% purrr::keep(names(.) != "TempCertificationLicensureID")

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		getSkyObject(module = "PerformanceAcknowledgment", objectName = "TempCertificationLicensure", objectId = TempCertificationLicensureID, searchFields = searchFields, entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' Delete a TempCertificationLicensure
	#'
	#' This function deletes a TempCertificationLicensure
	#' @param TempCertificationLicensureID The ID of the TempCertificationLicensure to delete
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept PerformanceAcknowledgment
	#' @return The TempCertificationLicensureID of the deleted TempCertificationLicensure.
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	deleteTempCertificationLicensure <- function(TempCertificationLicensureID, ignoreWarnings = F, entityId = 1, query = NULL, flatten = T, returnResponse = F){

		deleteSkyObject(module = "PerformanceAcknowledgment", objectName = "TempCertificationLicensure", objectId = TempCertificationLicensureID, ignoreWarnings = ignoreWarnings, entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' Create a TempCertificationLicensure
	#'
	#' This function creates a TempCertificationLicensure
	#' @param fieldNames The field values to give the created TempCertificationLicensure. Each defaults to NULL.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept PerformanceAcknowledgment
	#' @return A newly created TempCertificationLicensure
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	createTempCertificationLicensure <- function(CertificationLicensureID = NULL, DistrictID = NULL, StudentID = NULL, StudentNameLFMS = NULL, CertificationLicensureDescriptionID = NULL, CertificationLicensureDescription = NULL, MetDate = NULL, entityId = 1, query = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment())

		body <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper()) %>% purrr::compact()

		createSkyObject(module = "PerformanceAcknowledgment", objectName = "TempCertificationLicensure", body = list(DataObject = body), searchFields = append("TempCertificationLicensureID", body %>% names()), entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' Modify a TempCertificationLicensure
	#'
	#' This function modifies a TempCertificationLicensure
	#' @param fieldNames The field values to give the modified TempCertificationLicensure. Each defaults to NULL.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept PerformanceAcknowledgment
	#' @return The modified TempCertificationLicensure
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	modifyTempCertificationLicensure <- function(TempCertificationLicensureID, CertificationLicensureID = NULL, DistrictID = NULL, StudentID = NULL, StudentNameLFMS = NULL, CertificationLicensureDescriptionID = NULL, CertificationLicensureDescription = NULL, MetDate = NULL, entityId = 1, query = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment())

		body <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper()) %>% purrr::compact()

		modifySkyObject(module = "PerformanceAcknowledgment", objectName = "TempCertificationLicensure", objectId = TempCertificationLicensureID, body = list(DataObject = body), searchFields = append("TempCertificationLicensureID", body %>% names()), entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}
