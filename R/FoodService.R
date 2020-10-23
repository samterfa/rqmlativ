
	#' List TempDayPaymentSummaries
	#'
	#' This function returns a dataframe or json object of TempDayPaymentSummaries
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given TempDayPaymentSummaries. Defaults to FALSE for all return fields which, for convenience, returns all fields for the TempDayPaymentSummaries.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object listed as text. Run \code{\link{getSchemaForObjects}}('TempDayPaymentSummary') to get more field paths.
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
	#' @concept Food Service
	#' @return A list of TempDayPaymentSummaries
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	listTempDayPaymentSummaries <- function(searchConditionsList = NULL, TempDayPaymentSummaryID = F, Day = F, PaymentTotal = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, fieldPaths = NULL, searchConditionsGroupType = "And", searchSortFieldNamesList = NULL, searchSortFieldNamesDescendingList = NULL, entityId = 1, schoolYearId = NULL, page = 1, pageSize = 100000, flatten = T, returnResponse = F){

		params <- as.list(environment())

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		listSkyObjects(module = "FoodService", objectName = "TempDayPaymentSummary", searchFields = searchFields %>% append(fieldPaths), page = page, pageSize = pageSize, SearchConditionsList = searchConditionsList, SearchConditionsGroupType = searchConditionsGroupType, SearchSortFieldNamesList = searchSortFieldNamesList, SearchSortFieldNamesDescendingList = searchSortFieldNamesDescendingList, entityId = entityId, schoolYearId = schoolYearId, flatten = flatten, returnResponse = returnResponse)
	}

	#' Get a TempDayPaymentSummary
	#'
	#' This function returns a dataframe or json object of a TempDayPaymentSummary
	#' @param TempDayPaymentSummaryID The ID of the TempDayPaymentSummary to return.
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given TempDayPaymentSummary. Defaults to FALSE for all return fields which, for convenience, returns all fields for the TempDayPaymentSummary.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object listed as text. Run \code{\link{getSchemaForObjects}}('TempDayPaymentSummary') to get more field paths.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{listSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Food Service
	#' @return A dataframe or of TempDayPaymentSummary
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	getTempDayPaymentSummary <- function(TempDayPaymentSummaryID, Day = F, PaymentTotal = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, fieldPaths = NULL, entityId = 1, schoolYearId = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment()) %>% purrr::keep(names(.) != "TempDayPaymentSummaryID")

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		getSkyObject(module = "FoodService", objectName = "TempDayPaymentSummary", objectId = TempDayPaymentSummaryID, searchFields = searchFields, entityId = entityId, flatten = flatten, returnResponse = returnResponse)
	}

	#' Delete a TempDayPaymentSummary
	#'
	#' This function deletes a TempDayPaymentSummary
	#' @param TempDayPaymentSummaryID The ID of the TempDayPaymentSummary to delete
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{listSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Food Service
	#' @return The TempDayPaymentSummaryID of the deleted TempDayPaymentSummary.
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	deleteTempDayPaymentSummary <- function(TempDayPaymentSummaryID, ignoreWarnings = F, entityId = 1, schoolYearId = NULL, flatten = T, returnResponse = F){

		deleteSkyObject(module = "FoodService", objectName = "TempDayPaymentSummary", objectId = TempDayPaymentSummaryID, ignoreWarnings = ignoreWarnings, entityId = entityId, flatten = flatten, returnResponse = returnResponse)
	}

	#' Create a TempDayPaymentSummary
	#'
	#' This function creates a TempDayPaymentSummary
	#' @param fieldNames The field values to give the created TempDayPaymentSummary. Each defaults to NULL.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{listSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Food Service
	#' @return A newly created TempDayPaymentSummary
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	createTempDayPaymentSummary <- function(Day = NULL, PaymentTotal = NULL, entityId = 1, schoolYearId = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment())

		body <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper()) %>% purrr::compact()

		createSkyObject(module = "FoodService", objectName = "TempDayPaymentSummary", body = list(DataObject = body), searchFields = append("TempDayPaymentSummaryID", body %>% names()), entityId = entityId, schoolYearId = schoolYearId, flatten = flatten, returnResponse = returnResponse)
	}

	#' Modify a TempDayPaymentSummary
	#'
	#' This function modifies a TempDayPaymentSummary
	#' @param fieldNames The field values to give the modified TempDayPaymentSummary. Each defaults to NULL.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{listSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Food Service
	#' @return The modified TempDayPaymentSummary
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	modifyTempDayPaymentSummary <- function(TempDayPaymentSummaryID, Day = NULL, PaymentTotal = NULL, entityId = 1, schoolYearId = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment())

		body <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper()) %>% purrr::compact()

		modifySkyObject(module = "FoodService", objectName = "TempDayPaymentSummary", objectId = TempDayPaymentSummaryID, body = list(DataObject = body), searchFields = append("TempDayPaymentSummaryID", body %>% names()), entityId = entityId, schoolYearId = schoolYearId, flatten = flatten, returnResponse = returnResponse)
	}

	#' List TempItemPurchaseSummaries
	#'
	#' This function returns a dataframe or json object of TempItemPurchaseSummaries
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given TempItemPurchaseSummaries. Defaults to FALSE for all return fields which, for convenience, returns all fields for the TempItemPurchaseSummaries.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object listed as text. Run \code{\link{getSchemaForObjects}}('TempItemPurchaseSummary') to get more field paths.
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
	#' @concept Food Service
	#' @return A list of TempItemPurchaseSummaries
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	listTempItemPurchaseSummaries <- function(searchConditionsList = NULL, TempItemPurchaseSummaryID = F, ItemDescription = F, PurchaseTotal = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, fieldPaths = NULL, searchConditionsGroupType = "And", searchSortFieldNamesList = NULL, searchSortFieldNamesDescendingList = NULL, entityId = 1, schoolYearId = NULL, page = 1, pageSize = 100000, flatten = T, returnResponse = F){

		params <- as.list(environment())

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		listSkyObjects(module = "FoodService", objectName = "TempItemPurchaseSummary", searchFields = searchFields %>% append(fieldPaths), page = page, pageSize = pageSize, SearchConditionsList = searchConditionsList, SearchConditionsGroupType = searchConditionsGroupType, SearchSortFieldNamesList = searchSortFieldNamesList, SearchSortFieldNamesDescendingList = searchSortFieldNamesDescendingList, entityId = entityId, schoolYearId = schoolYearId, flatten = flatten, returnResponse = returnResponse)
	}

	#' Get a TempItemPurchaseSummary
	#'
	#' This function returns a dataframe or json object of a TempItemPurchaseSummary
	#' @param TempItemPurchaseSummaryID The ID of the TempItemPurchaseSummary to return.
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given TempItemPurchaseSummary. Defaults to FALSE for all return fields which, for convenience, returns all fields for the TempItemPurchaseSummary.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object listed as text. Run \code{\link{getSchemaForObjects}}('TempItemPurchaseSummary') to get more field paths.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{listSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Food Service
	#' @return A dataframe or of TempItemPurchaseSummary
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	getTempItemPurchaseSummary <- function(TempItemPurchaseSummaryID, ItemDescription = F, PurchaseTotal = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, fieldPaths = NULL, entityId = 1, schoolYearId = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment()) %>% purrr::keep(names(.) != "TempItemPurchaseSummaryID")

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		getSkyObject(module = "FoodService", objectName = "TempItemPurchaseSummary", objectId = TempItemPurchaseSummaryID, searchFields = searchFields, entityId = entityId, flatten = flatten, returnResponse = returnResponse)
	}

	#' Delete a TempItemPurchaseSummary
	#'
	#' This function deletes a TempItemPurchaseSummary
	#' @param TempItemPurchaseSummaryID The ID of the TempItemPurchaseSummary to delete
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{listSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Food Service
	#' @return The TempItemPurchaseSummaryID of the deleted TempItemPurchaseSummary.
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	deleteTempItemPurchaseSummary <- function(TempItemPurchaseSummaryID, ignoreWarnings = F, entityId = 1, schoolYearId = NULL, flatten = T, returnResponse = F){

		deleteSkyObject(module = "FoodService", objectName = "TempItemPurchaseSummary", objectId = TempItemPurchaseSummaryID, ignoreWarnings = ignoreWarnings, entityId = entityId, flatten = flatten, returnResponse = returnResponse)
	}

	#' Create a TempItemPurchaseSummary
	#'
	#' This function creates a TempItemPurchaseSummary
	#' @param fieldNames The field values to give the created TempItemPurchaseSummary. Each defaults to NULL.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{listSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Food Service
	#' @return A newly created TempItemPurchaseSummary
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	createTempItemPurchaseSummary <- function(ItemDescription = NULL, PurchaseTotal = NULL, entityId = 1, schoolYearId = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment())

		body <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper()) %>% purrr::compact()

		createSkyObject(module = "FoodService", objectName = "TempItemPurchaseSummary", body = list(DataObject = body), searchFields = append("TempItemPurchaseSummaryID", body %>% names()), entityId = entityId, schoolYearId = schoolYearId, flatten = flatten, returnResponse = returnResponse)
	}

	#' Modify a TempItemPurchaseSummary
	#'
	#' This function modifies a TempItemPurchaseSummary
	#' @param fieldNames The field values to give the modified TempItemPurchaseSummary. Each defaults to NULL.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{listSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Food Service
	#' @return The modified TempItemPurchaseSummary
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	modifyTempItemPurchaseSummary <- function(TempItemPurchaseSummaryID, ItemDescription = NULL, PurchaseTotal = NULL, entityId = 1, schoolYearId = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment())

		body <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper()) %>% purrr::compact()

		modifySkyObject(module = "FoodService", objectName = "TempItemPurchaseSummary", objectId = TempItemPurchaseSummaryID, body = list(DataObject = body), searchFields = append("TempItemPurchaseSummaryID", body %>% names()), entityId = entityId, schoolYearId = schoolYearId, flatten = flatten, returnResponse = returnResponse)
	}

	#' List LineDevices
	#'
	#' This function returns a dataframe or json object of LineDevices
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given LineDevices. Defaults to FALSE for all return fields which, for convenience, returns all fields for the LineDevices.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object listed as text. Run \code{\link{getSchemaForObjects}}('LineDevice') to get more field paths.
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
	#' @concept Food Service
	#' @return A list of LineDevices
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	listLineDevices <- function(searchConditionsList = NULL, LineDeviceID = F, LineID = F, Name = F, DeviceType = F, ComPort = F, BaudRate = F, DataBit = F, Parity = F, StopBit = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, fieldPaths = NULL, searchConditionsGroupType = "And", searchSortFieldNamesList = NULL, searchSortFieldNamesDescendingList = NULL, entityId = 1, schoolYearId = NULL, page = 1, pageSize = 100000, flatten = T, returnResponse = F){

		params <- as.list(environment())

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		listSkyObjects(module = "FoodService", objectName = "LineDevice", searchFields = searchFields %>% append(fieldPaths), page = page, pageSize = pageSize, SearchConditionsList = searchConditionsList, SearchConditionsGroupType = searchConditionsGroupType, SearchSortFieldNamesList = searchSortFieldNamesList, SearchSortFieldNamesDescendingList = searchSortFieldNamesDescendingList, entityId = entityId, schoolYearId = schoolYearId, flatten = flatten, returnResponse = returnResponse)
	}

	#' Get a LineDevice
	#'
	#' This function returns a dataframe or json object of a LineDevice
	#' @param LineDeviceID The ID of the LineDevice to return.
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given LineDevice. Defaults to FALSE for all return fields which, for convenience, returns all fields for the LineDevice.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object listed as text. Run \code{\link{getSchemaForObjects}}('LineDevice') to get more field paths.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{listSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Food Service
	#' @return A dataframe or of LineDevice
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	getLineDevice <- function(LineDeviceID, LineID = F, Name = F, DeviceType = F, ComPort = F, BaudRate = F, DataBit = F, Parity = F, StopBit = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, fieldPaths = NULL, entityId = 1, schoolYearId = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment()) %>% purrr::keep(names(.) != "LineDeviceID")

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		getSkyObject(module = "FoodService", objectName = "LineDevice", objectId = LineDeviceID, searchFields = searchFields, entityId = entityId, flatten = flatten, returnResponse = returnResponse)
	}

	#' Delete a LineDevice
	#'
	#' This function deletes a LineDevice
	#' @param LineDeviceID The ID of the LineDevice to delete
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{listSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Food Service
	#' @return The LineDeviceID of the deleted LineDevice.
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	deleteLineDevice <- function(LineDeviceID, ignoreWarnings = F, entityId = 1, schoolYearId = NULL, flatten = T, returnResponse = F){

		deleteSkyObject(module = "FoodService", objectName = "LineDevice", objectId = LineDeviceID, ignoreWarnings = ignoreWarnings, entityId = entityId, flatten = flatten, returnResponse = returnResponse)
	}

	#' Create a LineDevice
	#'
	#' This function creates a LineDevice
	#' @param fieldNames The field values to give the created LineDevice. Each defaults to NULL.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{listSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Food Service
	#' @return A newly created LineDevice
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	createLineDevice <- function(LineID = NULL, Name = NULL, DeviceType = NULL, ComPort = NULL, BaudRate = NULL, DataBit = NULL, Parity = NULL, StopBit = NULL, entityId = 1, schoolYearId = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment())

		body <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper()) %>% purrr::compact()

		createSkyObject(module = "FoodService", objectName = "LineDevice", body = list(DataObject = body), searchFields = append("LineDeviceID", body %>% names()), entityId = entityId, schoolYearId = schoolYearId, flatten = flatten, returnResponse = returnResponse)
	}

	#' Modify a LineDevice
	#'
	#' This function modifies a LineDevice
	#' @param fieldNames The field values to give the modified LineDevice. Each defaults to NULL.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{listSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Food Service
	#' @return The modified LineDevice
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	modifyLineDevice <- function(LineDeviceID, LineID = NULL, Name = NULL, DeviceType = NULL, ComPort = NULL, BaudRate = NULL, DataBit = NULL, Parity = NULL, StopBit = NULL, entityId = 1, schoolYearId = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment())

		body <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper()) %>% purrr::compact()

		modifySkyObject(module = "FoodService", objectName = "LineDevice", objectId = LineDeviceID, body = list(DataObject = body), searchFields = append("LineDeviceID", body %>% names()), entityId = entityId, schoolYearId = schoolYearId, flatten = flatten, returnResponse = returnResponse)
	}

	#' List CustomerTransactions
	#'
	#' This function returns a dataframe or json object of CustomerTransactions
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given CustomerTransactions. Defaults to FALSE for all return fields which, for convenience, returns all fields for the CustomerTransactions.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object listed as text. Run \code{\link{getSchemaForObjects}}('CustomerTransaction') to get more field paths.
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
	#' @concept Food Service
	#' @return A list of CustomerTransactions
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	listCustomerTransactions <- function(searchConditionsList = NULL, CustomerID = F, PurchaseDetailID = F, PaymentID = F, Time = F, Amount = F, Balance = F, fieldPaths = NULL, searchConditionsGroupType = "And", searchSortFieldNamesList = NULL, searchSortFieldNamesDescendingList = NULL, entityId = 1, schoolYearId = NULL, page = 1, pageSize = 100000, flatten = T, returnResponse = F){

		params <- as.list(environment())

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		listSkyObjects(module = "FoodService", objectName = "CustomerTransaction", searchFields = searchFields %>% append(fieldPaths), page = page, pageSize = pageSize, SearchConditionsList = searchConditionsList, SearchConditionsGroupType = searchConditionsGroupType, SearchSortFieldNamesList = searchSortFieldNamesList, SearchSortFieldNamesDescendingList = searchSortFieldNamesDescendingList, entityId = entityId, schoolYearId = schoolYearId, flatten = flatten, returnResponse = returnResponse)
	}

	#' Get a CustomerTransaction
	#'
	#' This function returns a dataframe or json object of a CustomerTransaction
	#' @param CustomerTransactionID The ID of the CustomerTransaction to return.
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given CustomerTransaction. Defaults to FALSE for all return fields which, for convenience, returns all fields for the CustomerTransaction.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object listed as text. Run \code{\link{getSchemaForObjects}}('CustomerTransaction') to get more field paths.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{listSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Food Service
	#' @return A dataframe or of CustomerTransaction
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	getCustomerTransaction <- function(CustomerTransactionID, CustomerID = F, PurchaseDetailID = F, PaymentID = F, Time = F, Amount = F, Balance = F, fieldPaths = NULL, entityId = 1, schoolYearId = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment()) %>% purrr::keep(names(.) != "CustomerTransactionID")

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		getSkyObject(module = "FoodService", objectName = "CustomerTransaction", objectId = CustomerTransactionID, searchFields = searchFields, entityId = entityId, flatten = flatten, returnResponse = returnResponse)
	}

	#' Delete a CustomerTransaction
	#'
	#' This function deletes a CustomerTransaction
	#' @param CustomerTransactionID The ID of the CustomerTransaction to delete
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{listSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Food Service
	#' @return The CustomerTransactionID of the deleted CustomerTransaction.
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	deleteCustomerTransaction <- function(CustomerTransactionID, ignoreWarnings = F, entityId = 1, schoolYearId = NULL, flatten = T, returnResponse = F){

		deleteSkyObject(module = "FoodService", objectName = "CustomerTransaction", objectId = CustomerTransactionID, ignoreWarnings = ignoreWarnings, entityId = entityId, flatten = flatten, returnResponse = returnResponse)
	}

	#' Create a CustomerTransaction
	#'
	#' This function creates a CustomerTransaction
	#' @param fieldNames The field values to give the created CustomerTransaction. Each defaults to NULL.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{listSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Food Service
	#' @return A newly created CustomerTransaction
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	createCustomerTransaction <- function(PurchaseDetailID = NULL, PaymentID = NULL, Time = NULL, Amount = NULL, Balance = NULL, entityId = 1, schoolYearId = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment())

		body <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper()) %>% purrr::compact()

		createSkyObject(module = "FoodService", objectName = "CustomerTransaction", body = list(DataObject = body), searchFields = append("CustomerTransactionID", body %>% names()), entityId = entityId, schoolYearId = schoolYearId, flatten = flatten, returnResponse = returnResponse)
	}

	#' Modify a CustomerTransaction
	#'
	#' This function modifies a CustomerTransaction
	#' @param fieldNames The field values to give the modified CustomerTransaction. Each defaults to NULL.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{listSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Food Service
	#' @return The modified CustomerTransaction
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	modifyCustomerTransaction <- function(CustomerTransactionID, PurchaseDetailID = NULL, PaymentID = NULL, Time = NULL, Amount = NULL, Balance = NULL, entityId = 1, schoolYearId = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment())

		body <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper()) %>% purrr::compact()

		modifySkyObject(module = "FoodService", objectName = "CustomerTransaction", objectId = CustomerTransactionID, body = list(DataObject = body), searchFields = append("CustomerTransactionID", body %>% names()), entityId = entityId, schoolYearId = schoolYearId, flatten = flatten, returnResponse = returnResponse)
	}

	#' List DirectCertificationDenialReasons
	#'
	#' This function returns a dataframe or json object of DirectCertificationDenialReasons
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given DirectCertificationDenialReasons. Defaults to FALSE for all return fields which, for convenience, returns all fields for the DirectCertificationDenialReasons.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object listed as text. Run \code{\link{getSchemaForObjects}}('DirectCertificationDenialReason') to get more field paths.
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
	#' @concept Food Service
	#' @return A list of DirectCertificationDenialReasons
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	listDirectCertificationDenialReasons <- function(searchConditionsList = NULL, DirectCertificationDenialReasonID = F, DirectCertificationID = F, DenialReasonID = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, fieldPaths = NULL, searchConditionsGroupType = "And", searchSortFieldNamesList = NULL, searchSortFieldNamesDescendingList = NULL, entityId = 1, schoolYearId = NULL, page = 1, pageSize = 100000, flatten = T, returnResponse = F){

		params <- as.list(environment())

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		listSkyObjects(module = "FoodService", objectName = "DirectCertificationDenialReason", searchFields = searchFields %>% append(fieldPaths), page = page, pageSize = pageSize, SearchConditionsList = searchConditionsList, SearchConditionsGroupType = searchConditionsGroupType, SearchSortFieldNamesList = searchSortFieldNamesList, SearchSortFieldNamesDescendingList = searchSortFieldNamesDescendingList, entityId = entityId, schoolYearId = schoolYearId, flatten = flatten, returnResponse = returnResponse)
	}

	#' Get a DirectCertificationDenialReason
	#'
	#' This function returns a dataframe or json object of a DirectCertificationDenialReason
	#' @param DirectCertificationDenialReasonID The ID of the DirectCertificationDenialReason to return.
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given DirectCertificationDenialReason. Defaults to FALSE for all return fields which, for convenience, returns all fields for the DirectCertificationDenialReason.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object listed as text. Run \code{\link{getSchemaForObjects}}('DirectCertificationDenialReason') to get more field paths.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{listSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Food Service
	#' @return A dataframe or of DirectCertificationDenialReason
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	getDirectCertificationDenialReason <- function(DirectCertificationDenialReasonID, DirectCertificationID = F, DenialReasonID = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, fieldPaths = NULL, entityId = 1, schoolYearId = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment()) %>% purrr::keep(names(.) != "DirectCertificationDenialReasonID")

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		getSkyObject(module = "FoodService", objectName = "DirectCertificationDenialReason", objectId = DirectCertificationDenialReasonID, searchFields = searchFields, entityId = entityId, flatten = flatten, returnResponse = returnResponse)
	}

	#' Delete a DirectCertificationDenialReason
	#'
	#' This function deletes a DirectCertificationDenialReason
	#' @param DirectCertificationDenialReasonID The ID of the DirectCertificationDenialReason to delete
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{listSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Food Service
	#' @return The DirectCertificationDenialReasonID of the deleted DirectCertificationDenialReason.
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	deleteDirectCertificationDenialReason <- function(DirectCertificationDenialReasonID, ignoreWarnings = F, entityId = 1, schoolYearId = NULL, flatten = T, returnResponse = F){

		deleteSkyObject(module = "FoodService", objectName = "DirectCertificationDenialReason", objectId = DirectCertificationDenialReasonID, ignoreWarnings = ignoreWarnings, entityId = entityId, flatten = flatten, returnResponse = returnResponse)
	}

	#' Create a DirectCertificationDenialReason
	#'
	#' This function creates a DirectCertificationDenialReason
	#' @param fieldNames The field values to give the created DirectCertificationDenialReason. Each defaults to NULL.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{listSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Food Service
	#' @return A newly created DirectCertificationDenialReason
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	createDirectCertificationDenialReason <- function(DirectCertificationID = NULL, DenialReasonID = NULL, entityId = 1, schoolYearId = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment())

		body <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper()) %>% purrr::compact()

		createSkyObject(module = "FoodService", objectName = "DirectCertificationDenialReason", body = list(DataObject = body), searchFields = append("DirectCertificationDenialReasonID", body %>% names()), entityId = entityId, schoolYearId = schoolYearId, flatten = flatten, returnResponse = returnResponse)
	}

	#' Modify a DirectCertificationDenialReason
	#'
	#' This function modifies a DirectCertificationDenialReason
	#' @param fieldNames The field values to give the modified DirectCertificationDenialReason. Each defaults to NULL.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{listSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Food Service
	#' @return The modified DirectCertificationDenialReason
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	modifyDirectCertificationDenialReason <- function(DirectCertificationDenialReasonID, DirectCertificationID = NULL, DenialReasonID = NULL, entityId = 1, schoolYearId = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment())

		body <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper()) %>% purrr::compact()

		modifySkyObject(module = "FoodService", objectName = "DirectCertificationDenialReason", objectId = DirectCertificationDenialReasonID, body = list(DataObject = body), searchFields = append("DirectCertificationDenialReasonID", body %>% names()), entityId = entityId, schoolYearId = schoolYearId, flatten = flatten, returnResponse = returnResponse)
	}

	#' List FoodServicePaymentDetails
	#'
	#' This function returns a dataframe or json object of FoodServicePaymentDetails
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given FoodServicePaymentDetails. Defaults to FALSE for all return fields which, for convenience, returns all fields for the FoodServicePaymentDetails.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object listed as text. Run \code{\link{getSchemaForObjects}}('FoodServicePaymentDetail') to get more field paths.
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
	#' @concept Food Service
	#' @return A list of FoodServicePaymentDetails
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	listFoodServicePaymentDetails <- function(searchConditionsList = NULL, PaymentDetailID = F, PaymentID = F, Amount = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, TransactionConfirmationNumber = F, ComponentConfirmationNumber = F, fieldPaths = NULL, searchConditionsGroupType = "And", searchSortFieldNamesList = NULL, searchSortFieldNamesDescendingList = NULL, entityId = 1, schoolYearId = NULL, page = 1, pageSize = 100000, flatten = T, returnResponse = F){

		params <- as.list(environment())

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		listSkyObjects(module = "FoodService", objectName = "PaymentDetail", searchFields = searchFields %>% append(fieldPaths), page = page, pageSize = pageSize, SearchConditionsList = searchConditionsList, SearchConditionsGroupType = searchConditionsGroupType, SearchSortFieldNamesList = searchSortFieldNamesList, SearchSortFieldNamesDescendingList = searchSortFieldNamesDescendingList, entityId = entityId, schoolYearId = schoolYearId, flatten = flatten, returnResponse = returnResponse)
	}

	#' Get a FoodServicePaymentDetail
	#'
	#' This function returns a dataframe or json object of a FoodServicePaymentDetail
	#' @param FoodServicePaymentDetailID The ID of the FoodServicePaymentDetail to return.
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given FoodServicePaymentDetail. Defaults to FALSE for all return fields which, for convenience, returns all fields for the FoodServicePaymentDetail.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object listed as text. Run \code{\link{getSchemaForObjects}}('FoodServicePaymentDetail') to get more field paths.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{listSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Food Service
	#' @return A dataframe or of FoodServicePaymentDetail
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	getFoodServicePaymentDetail <- function(FoodServicePaymentDetailID, PaymentDetailID = F, PaymentID = F, Amount = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, TransactionConfirmationNumber = F, ComponentConfirmationNumber = F, fieldPaths = NULL, entityId = 1, schoolYearId = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment()) %>% purrr::keep(names(.) != "FoodServicePaymentDetailID")

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		getSkyObject(module = "FoodService", objectName = "PaymentDetail", objectId = FoodServicePaymentDetailID, searchFields = searchFields, entityId = entityId, flatten = flatten, returnResponse = returnResponse)
	}

	#' Delete a FoodServicePaymentDetail
	#'
	#' This function deletes a FoodServicePaymentDetail
	#' @param FoodServicePaymentDetailID The ID of the FoodServicePaymentDetail to delete
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{listSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Food Service
	#' @return The FoodServicePaymentDetailID of the deleted FoodServicePaymentDetail.
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	deleteFoodServicePaymentDetail <- function(FoodServicePaymentDetailID, ignoreWarnings = F, entityId = 1, schoolYearId = NULL, flatten = T, returnResponse = F){

		deleteSkyObject(module = "FoodService", objectName = "PaymentDetail", objectId = FoodServicePaymentDetailID, ignoreWarnings = ignoreWarnings, entityId = entityId, flatten = flatten, returnResponse = returnResponse)
	}

	#' Create a FoodServicePaymentDetail
	#'
	#' This function creates a FoodServicePaymentDetail
	#' @param fieldNames The field values to give the created FoodServicePaymentDetail. Each defaults to NULL.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{listSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Food Service
	#' @return A newly created FoodServicePaymentDetail
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	createFoodServicePaymentDetail <- function(PaymentID = NULL, Amount = NULL, TransactionConfirmationNumber = NULL, ComponentConfirmationNumber = NULL, entityId = 1, schoolYearId = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment())

		body <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper()) %>% purrr::compact()

		createSkyObject(module = "FoodService", objectName = "PaymentDetail", body = list(DataObject = body), searchFields = append("PaymentDetailID", body %>% names()), entityId = entityId, schoolYearId = schoolYearId, flatten = flatten, returnResponse = returnResponse)
	}

	#' Modify a FoodServicePaymentDetail
	#'
	#' This function modifies a FoodServicePaymentDetail
	#' @param fieldNames The field values to give the modified FoodServicePaymentDetail. Each defaults to NULL.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{listSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Food Service
	#' @return The modified FoodServicePaymentDetail
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	modifyFoodServicePaymentDetail <- function(PaymentDetailID, PaymentID = NULL, Amount = NULL, TransactionConfirmationNumber = NULL, ComponentConfirmationNumber = NULL, entityId = 1, schoolYearId = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment())

		body <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper()) %>% purrr::compact()

		modifySkyObject(module = "FoodService", objectName = "PaymentDetail", objectId = PaymentDetailID, body = list(DataObject = body), searchFields = append("PaymentDetailID", body %>% names()), entityId = entityId, schoolYearId = schoolYearId, flatten = flatten, returnResponse = returnResponse)
	}

	#' List TempCustomerCategories
	#'
	#' This function returns a dataframe or json object of TempCustomerCategories
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given TempCustomerCategories. Defaults to FALSE for all return fields which, for convenience, returns all fields for the TempCustomerCategories.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object listed as text. Run \code{\link{getSchemaForObjects}}('TempCustomerCategory') to get more field paths.
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
	#' @concept Food Service
	#' @return A list of TempCustomerCategories
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	listTempCustomerCategories <- function(searchConditionsList = NULL, TempCustomerCategoryID = F, CustomerID = F, NewEligibilityCategory = F, NewPriceCategory = F, FullNameLFM = F, GradeLevel = F, BirthDate = F, OldEligibilityCategory = F, OldPriceCategory = F, StudentID = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, WasManualEntry = F, NameID = F, StudentNumber = F, StudentStateNumber = F, OldEffectiveTime = F, NewEligibilityCategoryID = F, NewPriceCategoryID = F, OldFreeReducedMealCode = F, NewFreeReducedMealCode = F, NewFreeReducedMealID = F, NewEffectiveTime = F, HasExceptions = F, ExceptionNote = F, StateFreeReducedMealReasonWAID = F, IsStaff = F, IsSelected = F, IsActive = F, ExceptionStatus = F, LineNumber = F, ApplyTextbookBenefits = F, fieldPaths = NULL, searchConditionsGroupType = "And", searchSortFieldNamesList = NULL, searchSortFieldNamesDescendingList = NULL, entityId = 1, schoolYearId = NULL, page = 1, pageSize = 100000, flatten = T, returnResponse = F){

		params <- as.list(environment())

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		listSkyObjects(module = "FoodService", objectName = "TempCustomerCategory", searchFields = searchFields %>% append(fieldPaths), page = page, pageSize = pageSize, SearchConditionsList = searchConditionsList, SearchConditionsGroupType = searchConditionsGroupType, SearchSortFieldNamesList = searchSortFieldNamesList, SearchSortFieldNamesDescendingList = searchSortFieldNamesDescendingList, entityId = entityId, schoolYearId = schoolYearId, flatten = flatten, returnResponse = returnResponse)
	}

	#' Get a TempCustomerCategory
	#'
	#' This function returns a dataframe or json object of a TempCustomerCategory
	#' @param TempCustomerCategoryID The ID of the TempCustomerCategory to return.
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given TempCustomerCategory. Defaults to FALSE for all return fields which, for convenience, returns all fields for the TempCustomerCategory.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object listed as text. Run \code{\link{getSchemaForObjects}}('TempCustomerCategory') to get more field paths.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{listSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Food Service
	#' @return A dataframe or of TempCustomerCategory
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	getTempCustomerCategory <- function(TempCustomerCategoryID, CustomerID = F, NewEligibilityCategory = F, NewPriceCategory = F, FullNameLFM = F, GradeLevel = F, BirthDate = F, OldEligibilityCategory = F, OldPriceCategory = F, StudentID = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, WasManualEntry = F, NameID = F, StudentNumber = F, StudentStateNumber = F, OldEffectiveTime = F, NewEligibilityCategoryID = F, NewPriceCategoryID = F, OldFreeReducedMealCode = F, NewFreeReducedMealCode = F, NewFreeReducedMealID = F, NewEffectiveTime = F, HasExceptions = F, ExceptionNote = F, StateFreeReducedMealReasonWAID = F, IsStaff = F, IsSelected = F, IsActive = F, ExceptionStatus = F, LineNumber = F, ApplyTextbookBenefits = F, fieldPaths = NULL, entityId = 1, schoolYearId = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment()) %>% purrr::keep(names(.) != "TempCustomerCategoryID")

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		getSkyObject(module = "FoodService", objectName = "TempCustomerCategory", objectId = TempCustomerCategoryID, searchFields = searchFields, entityId = entityId, flatten = flatten, returnResponse = returnResponse)
	}

	#' Delete a TempCustomerCategory
	#'
	#' This function deletes a TempCustomerCategory
	#' @param TempCustomerCategoryID The ID of the TempCustomerCategory to delete
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{listSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Food Service
	#' @return The TempCustomerCategoryID of the deleted TempCustomerCategory.
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	deleteTempCustomerCategory <- function(TempCustomerCategoryID, ignoreWarnings = F, entityId = 1, schoolYearId = NULL, flatten = T, returnResponse = F){

		deleteSkyObject(module = "FoodService", objectName = "TempCustomerCategory", objectId = TempCustomerCategoryID, ignoreWarnings = ignoreWarnings, entityId = entityId, flatten = flatten, returnResponse = returnResponse)
	}

	#' Create a TempCustomerCategory
	#'
	#' This function creates a TempCustomerCategory
	#' @param fieldNames The field values to give the created TempCustomerCategory. Each defaults to NULL.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{listSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Food Service
	#' @return A newly created TempCustomerCategory
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	createTempCustomerCategory <- function(CustomerID = NULL, NewEligibilityCategory = NULL, NewPriceCategory = NULL, FullNameLFM = NULL, GradeLevel = NULL, BirthDate = NULL, OldEligibilityCategory = NULL, OldPriceCategory = NULL, StudentID = NULL, WasManualEntry = NULL, NameID = NULL, StudentNumber = NULL, StudentStateNumber = NULL, OldEffectiveTime = NULL, NewEligibilityCategoryID = NULL, NewPriceCategoryID = NULL, OldFreeReducedMealCode = NULL, NewFreeReducedMealCode = NULL, NewFreeReducedMealID = NULL, NewEffectiveTime = NULL, HasExceptions = NULL, ExceptionNote = NULL, StateFreeReducedMealReasonWAID = NULL, IsStaff = NULL, IsSelected = NULL, IsActive = NULL, ExceptionStatus = NULL, LineNumber = NULL, ApplyTextbookBenefits = NULL, entityId = 1, schoolYearId = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment())

		body <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper()) %>% purrr::compact()

		createSkyObject(module = "FoodService", objectName = "TempCustomerCategory", body = list(DataObject = body), searchFields = append("TempCustomerCategoryID", body %>% names()), entityId = entityId, schoolYearId = schoolYearId, flatten = flatten, returnResponse = returnResponse)
	}

	#' Modify a TempCustomerCategory
	#'
	#' This function modifies a TempCustomerCategory
	#' @param fieldNames The field values to give the modified TempCustomerCategory. Each defaults to NULL.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{listSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Food Service
	#' @return The modified TempCustomerCategory
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	modifyTempCustomerCategory <- function(TempCustomerCategoryID, CustomerID = NULL, NewEligibilityCategory = NULL, NewPriceCategory = NULL, FullNameLFM = NULL, GradeLevel = NULL, BirthDate = NULL, OldEligibilityCategory = NULL, OldPriceCategory = NULL, StudentID = NULL, WasManualEntry = NULL, NameID = NULL, StudentNumber = NULL, StudentStateNumber = NULL, OldEffectiveTime = NULL, NewEligibilityCategoryID = NULL, NewPriceCategoryID = NULL, OldFreeReducedMealCode = NULL, NewFreeReducedMealCode = NULL, NewFreeReducedMealID = NULL, NewEffectiveTime = NULL, HasExceptions = NULL, ExceptionNote = NULL, StateFreeReducedMealReasonWAID = NULL, IsStaff = NULL, IsSelected = NULL, IsActive = NULL, ExceptionStatus = NULL, LineNumber = NULL, ApplyTextbookBenefits = NULL, entityId = 1, schoolYearId = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment())

		body <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper()) %>% purrr::compact()

		modifySkyObject(module = "FoodService", objectName = "TempCustomerCategory", objectId = TempCustomerCategoryID, body = list(DataObject = body), searchFields = append("TempCustomerCategoryID", body %>% names()), entityId = entityId, schoolYearId = schoolYearId, flatten = flatten, returnResponse = returnResponse)
	}

	#' List CloseoutChecks
	#'
	#' This function returns a dataframe or json object of CloseoutChecks
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given CloseoutChecks. Defaults to FALSE for all return fields which, for convenience, returns all fields for the CloseoutChecks.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object listed as text. Run \code{\link{getSchemaForObjects}}('CloseoutCheck') to get more field paths.
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
	#' @concept Food Service
	#' @return A list of CloseoutChecks
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	listCloseoutChecks <- function(searchConditionsList = NULL, CloseoutCheckID = F, CloseoutTransactionID = F, CheckNumber = F, Amount = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, fieldPaths = NULL, searchConditionsGroupType = "And", searchSortFieldNamesList = NULL, searchSortFieldNamesDescendingList = NULL, entityId = 1, schoolYearId = NULL, page = 1, pageSize = 100000, flatten = T, returnResponse = F){

		params <- as.list(environment())

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		listSkyObjects(module = "FoodService", objectName = "CloseoutCheck", searchFields = searchFields %>% append(fieldPaths), page = page, pageSize = pageSize, SearchConditionsList = searchConditionsList, SearchConditionsGroupType = searchConditionsGroupType, SearchSortFieldNamesList = searchSortFieldNamesList, SearchSortFieldNamesDescendingList = searchSortFieldNamesDescendingList, entityId = entityId, schoolYearId = schoolYearId, flatten = flatten, returnResponse = returnResponse)
	}

	#' Get a CloseoutCheck
	#'
	#' This function returns a dataframe or json object of a CloseoutCheck
	#' @param CloseoutCheckID The ID of the CloseoutCheck to return.
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given CloseoutCheck. Defaults to FALSE for all return fields which, for convenience, returns all fields for the CloseoutCheck.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object listed as text. Run \code{\link{getSchemaForObjects}}('CloseoutCheck') to get more field paths.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{listSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Food Service
	#' @return A dataframe or of CloseoutCheck
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	getCloseoutCheck <- function(CloseoutCheckID, CloseoutTransactionID = F, CheckNumber = F, Amount = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, fieldPaths = NULL, entityId = 1, schoolYearId = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment()) %>% purrr::keep(names(.) != "CloseoutCheckID")

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		getSkyObject(module = "FoodService", objectName = "CloseoutCheck", objectId = CloseoutCheckID, searchFields = searchFields, entityId = entityId, flatten = flatten, returnResponse = returnResponse)
	}

	#' Delete a CloseoutCheck
	#'
	#' This function deletes a CloseoutCheck
	#' @param CloseoutCheckID The ID of the CloseoutCheck to delete
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{listSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Food Service
	#' @return The CloseoutCheckID of the deleted CloseoutCheck.
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	deleteCloseoutCheck <- function(CloseoutCheckID, ignoreWarnings = F, entityId = 1, schoolYearId = NULL, flatten = T, returnResponse = F){

		deleteSkyObject(module = "FoodService", objectName = "CloseoutCheck", objectId = CloseoutCheckID, ignoreWarnings = ignoreWarnings, entityId = entityId, flatten = flatten, returnResponse = returnResponse)
	}

	#' Create a CloseoutCheck
	#'
	#' This function creates a CloseoutCheck
	#' @param fieldNames The field values to give the created CloseoutCheck. Each defaults to NULL.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{listSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Food Service
	#' @return A newly created CloseoutCheck
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	createCloseoutCheck <- function(CloseoutTransactionID = NULL, CheckNumber = NULL, Amount = NULL, entityId = 1, schoolYearId = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment())

		body <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper()) %>% purrr::compact()

		createSkyObject(module = "FoodService", objectName = "CloseoutCheck", body = list(DataObject = body), searchFields = append("CloseoutCheckID", body %>% names()), entityId = entityId, schoolYearId = schoolYearId, flatten = flatten, returnResponse = returnResponse)
	}

	#' Modify a CloseoutCheck
	#'
	#' This function modifies a CloseoutCheck
	#' @param fieldNames The field values to give the modified CloseoutCheck. Each defaults to NULL.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{listSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Food Service
	#' @return The modified CloseoutCheck
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	modifyCloseoutCheck <- function(CloseoutCheckID, CloseoutTransactionID = NULL, CheckNumber = NULL, Amount = NULL, entityId = 1, schoolYearId = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment())

		body <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper()) %>% purrr::compact()

		modifySkyObject(module = "FoodService", objectName = "CloseoutCheck", objectId = CloseoutCheckID, body = list(DataObject = body), searchFields = append("CloseoutCheckID", body %>% names()), entityId = entityId, schoolYearId = schoolYearId, flatten = flatten, returnResponse = returnResponse)
	}

	#' List CloseoutTransactions
	#'
	#' This function returns a dataframe or json object of CloseoutTransactions
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given CloseoutTransactions. Defaults to FALSE for all return fields which, for convenience, returns all fields for the CloseoutTransactions.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object listed as text. Run \code{\link{getSchemaForObjects}}('CloseoutTransaction') to get more field paths.
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
	#' @concept Food Service
	#' @return A list of CloseoutTransactions
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	listCloseoutTransactions <- function(searchConditionsList = NULL, CloseoutTransactionID = F, LineCloseoutID = F, StartingBalance = F, EndingBalance = F, CloseoutAmount = F, QuantityPennies = F, QuantityNickels = F, QuantityDimes = F, QuantityQuarters = F, QuantityHalfDollars = F, QuantityDollarCoins = F, QuantityDollarBills = F, QuantityFiveDollarBills = F, QuantityTenDollarBills = F, QuantityTwentyDollarBills = F, QuantityFiftyDollarBills = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, fieldPaths = NULL, searchConditionsGroupType = "And", searchSortFieldNamesList = NULL, searchSortFieldNamesDescendingList = NULL, entityId = 1, schoolYearId = NULL, page = 1, pageSize = 100000, flatten = T, returnResponse = F){

		params <- as.list(environment())

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		listSkyObjects(module = "FoodService", objectName = "CloseoutTransaction", searchFields = searchFields %>% append(fieldPaths), page = page, pageSize = pageSize, SearchConditionsList = searchConditionsList, SearchConditionsGroupType = searchConditionsGroupType, SearchSortFieldNamesList = searchSortFieldNamesList, SearchSortFieldNamesDescendingList = searchSortFieldNamesDescendingList, entityId = entityId, schoolYearId = schoolYearId, flatten = flatten, returnResponse = returnResponse)
	}

	#' Get a CloseoutTransaction
	#'
	#' This function returns a dataframe or json object of a CloseoutTransaction
	#' @param CloseoutTransactionID The ID of the CloseoutTransaction to return.
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given CloseoutTransaction. Defaults to FALSE for all return fields which, for convenience, returns all fields for the CloseoutTransaction.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object listed as text. Run \code{\link{getSchemaForObjects}}('CloseoutTransaction') to get more field paths.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{listSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Food Service
	#' @return A dataframe or of CloseoutTransaction
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	getCloseoutTransaction <- function(CloseoutTransactionID, LineCloseoutID = F, StartingBalance = F, EndingBalance = F, CloseoutAmount = F, QuantityPennies = F, QuantityNickels = F, QuantityDimes = F, QuantityQuarters = F, QuantityHalfDollars = F, QuantityDollarCoins = F, QuantityDollarBills = F, QuantityFiveDollarBills = F, QuantityTenDollarBills = F, QuantityTwentyDollarBills = F, QuantityFiftyDollarBills = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, fieldPaths = NULL, entityId = 1, schoolYearId = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment()) %>% purrr::keep(names(.) != "CloseoutTransactionID")

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		getSkyObject(module = "FoodService", objectName = "CloseoutTransaction", objectId = CloseoutTransactionID, searchFields = searchFields, entityId = entityId, flatten = flatten, returnResponse = returnResponse)
	}

	#' Delete a CloseoutTransaction
	#'
	#' This function deletes a CloseoutTransaction
	#' @param CloseoutTransactionID The ID of the CloseoutTransaction to delete
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{listSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Food Service
	#' @return The CloseoutTransactionID of the deleted CloseoutTransaction.
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	deleteCloseoutTransaction <- function(CloseoutTransactionID, ignoreWarnings = F, entityId = 1, schoolYearId = NULL, flatten = T, returnResponse = F){

		deleteSkyObject(module = "FoodService", objectName = "CloseoutTransaction", objectId = CloseoutTransactionID, ignoreWarnings = ignoreWarnings, entityId = entityId, flatten = flatten, returnResponse = returnResponse)
	}

	#' Create a CloseoutTransaction
	#'
	#' This function creates a CloseoutTransaction
	#' @param fieldNames The field values to give the created CloseoutTransaction. Each defaults to NULL.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{listSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Food Service
	#' @return A newly created CloseoutTransaction
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	createCloseoutTransaction <- function(LineCloseoutID = NULL, StartingBalance = NULL, EndingBalance = NULL, CloseoutAmount = NULL, QuantityPennies = NULL, QuantityNickels = NULL, QuantityDimes = NULL, QuantityQuarters = NULL, QuantityHalfDollars = NULL, QuantityDollarCoins = NULL, QuantityDollarBills = NULL, QuantityFiveDollarBills = NULL, QuantityTenDollarBills = NULL, QuantityTwentyDollarBills = NULL, QuantityFiftyDollarBills = NULL, entityId = 1, schoolYearId = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment())

		body <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper()) %>% purrr::compact()

		createSkyObject(module = "FoodService", objectName = "CloseoutTransaction", body = list(DataObject = body), searchFields = append("CloseoutTransactionID", body %>% names()), entityId = entityId, schoolYearId = schoolYearId, flatten = flatten, returnResponse = returnResponse)
	}

	#' Modify a CloseoutTransaction
	#'
	#' This function modifies a CloseoutTransaction
	#' @param fieldNames The field values to give the modified CloseoutTransaction. Each defaults to NULL.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{listSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Food Service
	#' @return The modified CloseoutTransaction
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	modifyCloseoutTransaction <- function(CloseoutTransactionID, LineCloseoutID = NULL, StartingBalance = NULL, EndingBalance = NULL, CloseoutAmount = NULL, QuantityPennies = NULL, QuantityNickels = NULL, QuantityDimes = NULL, QuantityQuarters = NULL, QuantityHalfDollars = NULL, QuantityDollarCoins = NULL, QuantityDollarBills = NULL, QuantityFiveDollarBills = NULL, QuantityTenDollarBills = NULL, QuantityTwentyDollarBills = NULL, QuantityFiftyDollarBills = NULL, entityId = 1, schoolYearId = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment())

		body <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper()) %>% purrr::compact()

		modifySkyObject(module = "FoodService", objectName = "CloseoutTransaction", objectId = CloseoutTransactionID, body = list(DataObject = body), searchFields = append("CloseoutTransactionID", body %>% names()), entityId = entityId, schoolYearId = schoolYearId, flatten = flatten, returnResponse = returnResponse)
	}

	#' List LineCloseouts
	#'
	#' This function returns a dataframe or json object of LineCloseouts
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given LineCloseouts. Defaults to FALSE for all return fields which, for convenience, returns all fields for the LineCloseouts.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object listed as text. Run \code{\link{getSchemaForObjects}}('LineCloseout') to get more field paths.
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
	#' @concept Food Service
	#' @return A list of LineCloseouts
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	listLineCloseouts <- function(searchConditionsList = NULL, LineCloseoutID = F, LineID = F, StartTime = F, CloseoutTime = F, IsFinalized = F, LatestCloseoutAmount = F, LatestStartingBalance = F, CashReceivedAmount = F, CheckReceivedAmount = F, TotalReceivedAmount = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, FinalizedByFullNameLFM = F, fieldPaths = NULL, searchConditionsGroupType = "And", searchSortFieldNamesList = NULL, searchSortFieldNamesDescendingList = NULL, entityId = 1, schoolYearId = NULL, page = 1, pageSize = 100000, flatten = T, returnResponse = F){

		params <- as.list(environment())

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		listSkyObjects(module = "FoodService", objectName = "LineCloseout", searchFields = searchFields %>% append(fieldPaths), page = page, pageSize = pageSize, SearchConditionsList = searchConditionsList, SearchConditionsGroupType = searchConditionsGroupType, SearchSortFieldNamesList = searchSortFieldNamesList, SearchSortFieldNamesDescendingList = searchSortFieldNamesDescendingList, entityId = entityId, schoolYearId = schoolYearId, flatten = flatten, returnResponse = returnResponse)
	}

	#' Get a LineCloseout
	#'
	#' This function returns a dataframe or json object of a LineCloseout
	#' @param LineCloseoutID The ID of the LineCloseout to return.
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given LineCloseout. Defaults to FALSE for all return fields which, for convenience, returns all fields for the LineCloseout.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object listed as text. Run \code{\link{getSchemaForObjects}}('LineCloseout') to get more field paths.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{listSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Food Service
	#' @return A dataframe or of LineCloseout
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	getLineCloseout <- function(LineCloseoutID, LineID = F, StartTime = F, CloseoutTime = F, IsFinalized = F, LatestCloseoutAmount = F, LatestStartingBalance = F, CashReceivedAmount = F, CheckReceivedAmount = F, TotalReceivedAmount = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, FinalizedByFullNameLFM = F, fieldPaths = NULL, entityId = 1, schoolYearId = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment()) %>% purrr::keep(names(.) != "LineCloseoutID")

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		getSkyObject(module = "FoodService", objectName = "LineCloseout", objectId = LineCloseoutID, searchFields = searchFields, entityId = entityId, flatten = flatten, returnResponse = returnResponse)
	}

	#' Delete a LineCloseout
	#'
	#' This function deletes a LineCloseout
	#' @param LineCloseoutID The ID of the LineCloseout to delete
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{listSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Food Service
	#' @return The LineCloseoutID of the deleted LineCloseout.
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	deleteLineCloseout <- function(LineCloseoutID, ignoreWarnings = F, entityId = 1, schoolYearId = NULL, flatten = T, returnResponse = F){

		deleteSkyObject(module = "FoodService", objectName = "LineCloseout", objectId = LineCloseoutID, ignoreWarnings = ignoreWarnings, entityId = entityId, flatten = flatten, returnResponse = returnResponse)
	}

	#' Create a LineCloseout
	#'
	#' This function creates a LineCloseout
	#' @param fieldNames The field values to give the created LineCloseout. Each defaults to NULL.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{listSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Food Service
	#' @return A newly created LineCloseout
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	createLineCloseout <- function(LineID = NULL, StartTime = NULL, CloseoutTime = NULL, entityId = 1, schoolYearId = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment())

		body <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper()) %>% purrr::compact()

		createSkyObject(module = "FoodService", objectName = "LineCloseout", body = list(DataObject = body), searchFields = append("LineCloseoutID", body %>% names()), entityId = entityId, schoolYearId = schoolYearId, flatten = flatten, returnResponse = returnResponse)
	}

	#' Modify a LineCloseout
	#'
	#' This function modifies a LineCloseout
	#' @param fieldNames The field values to give the modified LineCloseout. Each defaults to NULL.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{listSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Food Service
	#' @return The modified LineCloseout
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	modifyLineCloseout <- function(LineCloseoutID, LineID = NULL, StartTime = NULL, CloseoutTime = NULL, entityId = 1, schoolYearId = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment())

		body <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper()) %>% purrr::compact()

		modifySkyObject(module = "FoodService", objectName = "LineCloseout", objectId = LineCloseoutID, body = list(DataObject = body), searchFields = append("LineCloseoutID", body %>% names()), entityId = entityId, schoolYearId = schoolYearId, flatten = flatten, returnResponse = returnResponse)
	}

	#' List POSSessions
	#'
	#' This function returns a dataframe or json object of POSSessions
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given POSSessions. Defaults to FALSE for all return fields which, for convenience, returns all fields for the POSSessions.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object listed as text. Run \code{\link{getSchemaForObjects}}('POSSession') to get more field paths.
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
	#' @concept Food Service
	#' @return A list of POSSessions
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	listPOSSessions <- function(searchConditionsList = NULL, POSSessionID = F, UserID = F, LineID = F, LastPingTime = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, fieldPaths = NULL, searchConditionsGroupType = "And", searchSortFieldNamesList = NULL, searchSortFieldNamesDescendingList = NULL, entityId = 1, schoolYearId = NULL, page = 1, pageSize = 100000, flatten = T, returnResponse = F){

		params <- as.list(environment())

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		listSkyObjects(module = "FoodService", objectName = "POSSession", searchFields = searchFields %>% append(fieldPaths), page = page, pageSize = pageSize, SearchConditionsList = searchConditionsList, SearchConditionsGroupType = searchConditionsGroupType, SearchSortFieldNamesList = searchSortFieldNamesList, SearchSortFieldNamesDescendingList = searchSortFieldNamesDescendingList, entityId = entityId, schoolYearId = schoolYearId, flatten = flatten, returnResponse = returnResponse)
	}

	#' Get a POSSession
	#'
	#' This function returns a dataframe or json object of a POSSession
	#' @param POSSessionID The ID of the POSSession to return.
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given POSSession. Defaults to FALSE for all return fields which, for convenience, returns all fields for the POSSession.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object listed as text. Run \code{\link{getSchemaForObjects}}('POSSession') to get more field paths.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{listSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Food Service
	#' @return A dataframe or of POSSession
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	getPOSSession <- function(POSSessionID, UserID = F, LineID = F, LastPingTime = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, fieldPaths = NULL, entityId = 1, schoolYearId = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment()) %>% purrr::keep(names(.) != "POSSessionID")

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		getSkyObject(module = "FoodService", objectName = "POSSession", objectId = POSSessionID, searchFields = searchFields, entityId = entityId, flatten = flatten, returnResponse = returnResponse)
	}

	#' Delete a POSSession
	#'
	#' This function deletes a POSSession
	#' @param POSSessionID The ID of the POSSession to delete
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{listSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Food Service
	#' @return The POSSessionID of the deleted POSSession.
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	deletePOSSession <- function(POSSessionID, ignoreWarnings = F, entityId = 1, schoolYearId = NULL, flatten = T, returnResponse = F){

		deleteSkyObject(module = "FoodService", objectName = "POSSession", objectId = POSSessionID, ignoreWarnings = ignoreWarnings, entityId = entityId, flatten = flatten, returnResponse = returnResponse)
	}

	#' Create a POSSession
	#'
	#' This function creates a POSSession
	#' @param fieldNames The field values to give the created POSSession. Each defaults to NULL.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{listSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Food Service
	#' @return A newly created POSSession
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	createPOSSession <- function(UserID = NULL, LineID = NULL, LastPingTime = NULL, entityId = 1, schoolYearId = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment())

		body <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper()) %>% purrr::compact()

		createSkyObject(module = "FoodService", objectName = "POSSession", body = list(DataObject = body), searchFields = append("POSSessionID", body %>% names()), entityId = entityId, schoolYearId = schoolYearId, flatten = flatten, returnResponse = returnResponse)
	}

	#' Modify a POSSession
	#'
	#' This function modifies a POSSession
	#' @param fieldNames The field values to give the modified POSSession. Each defaults to NULL.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{listSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Food Service
	#' @return The modified POSSession
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	modifyPOSSession <- function(POSSessionID, UserID = NULL, LineID = NULL, LastPingTime = NULL, entityId = 1, schoolYearId = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment())

		body <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper()) %>% purrr::compact()

		modifySkyObject(module = "FoodService", objectName = "POSSession", objectId = POSSessionID, body = list(DataObject = body), searchFields = append("POSSessionID", body %>% names()), entityId = entityId, schoolYearId = schoolYearId, flatten = flatten, returnResponse = returnResponse)
	}

	#' List AlertMessages
	#'
	#' This function returns a dataframe or json object of AlertMessages
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given AlertMessages. Defaults to FALSE for all return fields which, for convenience, returns all fields for the AlertMessages.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object listed as text. Run \code{\link{getSchemaForObjects}}('AlertMessage') to get more field paths.
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
	#' @concept Food Service
	#' @return A list of AlertMessages
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	listAlertMessages <- function(searchConditionsList = NULL, AlertMessageID = F, Code = F, Description = F, DistrictID = F, DistrictGroupKey = F, IsCritical = F, CodeDescription = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, fieldPaths = NULL, searchConditionsGroupType = "And", searchSortFieldNamesList = NULL, searchSortFieldNamesDescendingList = NULL, entityId = 1, schoolYearId = NULL, page = 1, pageSize = 100000, flatten = T, returnResponse = F){

		params <- as.list(environment())

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		listSkyObjects(module = "FoodService", objectName = "AlertMessage", searchFields = searchFields %>% append(fieldPaths), page = page, pageSize = pageSize, SearchConditionsList = searchConditionsList, SearchConditionsGroupType = searchConditionsGroupType, SearchSortFieldNamesList = searchSortFieldNamesList, SearchSortFieldNamesDescendingList = searchSortFieldNamesDescendingList, entityId = entityId, schoolYearId = schoolYearId, flatten = flatten, returnResponse = returnResponse)
	}

	#' Get an AlertMessage
	#'
	#' This function returns a dataframe or json object of an AlertMessage
	#' @param AlertMessageID The ID of the AlertMessage to return.
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given AlertMessage. Defaults to FALSE for all return fields which, for convenience, returns all fields for the AlertMessage.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object listed as text. Run \code{\link{getSchemaForObjects}}('AlertMessage') to get more field paths.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{listSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Food Service
	#' @return A dataframe or of AlertMessage
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	getAlertMessage <- function(AlertMessageID, Code = F, Description = F, DistrictID = F, DistrictGroupKey = F, IsCritical = F, CodeDescription = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, fieldPaths = NULL, entityId = 1, schoolYearId = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment()) %>% purrr::keep(names(.) != "AlertMessageID")

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		getSkyObject(module = "FoodService", objectName = "AlertMessage", objectId = AlertMessageID, searchFields = searchFields, entityId = entityId, flatten = flatten, returnResponse = returnResponse)
	}

	#' Delete an AlertMessage
	#'
	#' This function deletes an AlertMessage
	#' @param AlertMessageID The ID of the AlertMessage to delete
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{listSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Food Service
	#' @return The AlertMessageID of the deleted AlertMessage.
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	deleteAlertMessage <- function(AlertMessageID, ignoreWarnings = F, entityId = 1, schoolYearId = NULL, flatten = T, returnResponse = F){

		deleteSkyObject(module = "FoodService", objectName = "AlertMessage", objectId = AlertMessageID, ignoreWarnings = ignoreWarnings, entityId = entityId, flatten = flatten, returnResponse = returnResponse)
	}

	#' Create an AlertMessage
	#'
	#' This function creates an AlertMessage
	#' @param fieldNames The field values to give the created AlertMessage. Each defaults to NULL.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{listSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Food Service
	#' @return A newly created AlertMessage
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	createAlertMessage <- function(Code = NULL, Description = NULL, DistrictID = NULL, DistrictGroupKey = NULL, IsCritical = NULL, entityId = 1, schoolYearId = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment())

		body <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper()) %>% purrr::compact()

		createSkyObject(module = "FoodService", objectName = "AlertMessage", body = list(DataObject = body), searchFields = append("AlertMessageID", body %>% names()), entityId = entityId, schoolYearId = schoolYearId, flatten = flatten, returnResponse = returnResponse)
	}

	#' Modify an AlertMessage
	#'
	#' This function modifies an AlertMessage
	#' @param fieldNames The field values to give the modified AlertMessage. Each defaults to NULL.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{listSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Food Service
	#' @return The modified AlertMessage
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	modifyAlertMessage <- function(AlertMessageID, Code = NULL, Description = NULL, DistrictID = NULL, DistrictGroupKey = NULL, IsCritical = NULL, entityId = 1, schoolYearId = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment())

		body <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper()) %>% purrr::compact()

		modifySkyObject(module = "FoodService", objectName = "AlertMessage", objectId = AlertMessageID, body = list(DataObject = body), searchFields = append("AlertMessageID", body %>% names()), entityId = entityId, schoolYearId = schoolYearId, flatten = flatten, returnResponse = returnResponse)
	}

	#' List ApplicationContactHistories
	#'
	#' This function returns a dataframe or json object of ApplicationContactHistories
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given ApplicationContactHistories. Defaults to FALSE for all return fields which, for convenience, returns all fields for the ApplicationContactHistories.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object listed as text. Run \code{\link{getSchemaForObjects}}('ApplicationContactHistory') to get more field paths.
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
	#' @concept Food Service
	#' @return A list of ApplicationContactHistories
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	listApplicationContactHistories <- function(searchConditionsList = NULL, ApplicationContactHistoryID = F, ApplicationID = F, NameID = F, ContactTime = F, ContactMethod = F, Comment = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, ContactReason = F, fieldPaths = NULL, searchConditionsGroupType = "And", searchSortFieldNamesList = NULL, searchSortFieldNamesDescendingList = NULL, entityId = 1, schoolYearId = NULL, page = 1, pageSize = 100000, flatten = T, returnResponse = F){

		params <- as.list(environment())

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		listSkyObjects(module = "FoodService", objectName = "ApplicationContactHistory", searchFields = searchFields %>% append(fieldPaths), page = page, pageSize = pageSize, SearchConditionsList = searchConditionsList, SearchConditionsGroupType = searchConditionsGroupType, SearchSortFieldNamesList = searchSortFieldNamesList, SearchSortFieldNamesDescendingList = searchSortFieldNamesDescendingList, entityId = entityId, schoolYearId = schoolYearId, flatten = flatten, returnResponse = returnResponse)
	}

	#' Get an ApplicationContactHistory
	#'
	#' This function returns a dataframe or json object of an ApplicationContactHistory
	#' @param ApplicationContactHistoryID The ID of the ApplicationContactHistory to return.
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given ApplicationContactHistory. Defaults to FALSE for all return fields which, for convenience, returns all fields for the ApplicationContactHistory.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object listed as text. Run \code{\link{getSchemaForObjects}}('ApplicationContactHistory') to get more field paths.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{listSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Food Service
	#' @return A dataframe or of ApplicationContactHistory
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	getApplicationContactHistory <- function(ApplicationContactHistoryID, ApplicationID = F, NameID = F, ContactTime = F, ContactMethod = F, Comment = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, ContactReason = F, fieldPaths = NULL, entityId = 1, schoolYearId = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment()) %>% purrr::keep(names(.) != "ApplicationContactHistoryID")

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		getSkyObject(module = "FoodService", objectName = "ApplicationContactHistory", objectId = ApplicationContactHistoryID, searchFields = searchFields, entityId = entityId, flatten = flatten, returnResponse = returnResponse)
	}

	#' Delete an ApplicationContactHistory
	#'
	#' This function deletes an ApplicationContactHistory
	#' @param ApplicationContactHistoryID The ID of the ApplicationContactHistory to delete
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{listSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Food Service
	#' @return The ApplicationContactHistoryID of the deleted ApplicationContactHistory.
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	deleteApplicationContactHistory <- function(ApplicationContactHistoryID, ignoreWarnings = F, entityId = 1, schoolYearId = NULL, flatten = T, returnResponse = F){

		deleteSkyObject(module = "FoodService", objectName = "ApplicationContactHistory", objectId = ApplicationContactHistoryID, ignoreWarnings = ignoreWarnings, entityId = entityId, flatten = flatten, returnResponse = returnResponse)
	}

	#' Create an ApplicationContactHistory
	#'
	#' This function creates an ApplicationContactHistory
	#' @param fieldNames The field values to give the created ApplicationContactHistory. Each defaults to NULL.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{listSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Food Service
	#' @return A newly created ApplicationContactHistory
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	createApplicationContactHistory <- function(ApplicationID = NULL, NameID = NULL, ContactTime = NULL, ContactMethod = NULL, Comment = NULL, ContactReason = NULL, entityId = 1, schoolYearId = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment())

		body <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper()) %>% purrr::compact()

		createSkyObject(module = "FoodService", objectName = "ApplicationContactHistory", body = list(DataObject = body), searchFields = append("ApplicationContactHistoryID", body %>% names()), entityId = entityId, schoolYearId = schoolYearId, flatten = flatten, returnResponse = returnResponse)
	}

	#' Modify an ApplicationContactHistory
	#'
	#' This function modifies an ApplicationContactHistory
	#' @param fieldNames The field values to give the modified ApplicationContactHistory. Each defaults to NULL.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{listSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Food Service
	#' @return The modified ApplicationContactHistory
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	modifyApplicationContactHistory <- function(ApplicationContactHistoryID, ApplicationID = NULL, NameID = NULL, ContactTime = NULL, ContactMethod = NULL, Comment = NULL, ContactReason = NULL, entityId = 1, schoolYearId = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment())

		body <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper()) %>% purrr::compact()

		modifySkyObject(module = "FoodService", objectName = "ApplicationContactHistory", objectId = ApplicationContactHistoryID, body = list(DataObject = body), searchFields = append("ApplicationContactHistoryID", body %>% names()), entityId = entityId, schoolYearId = schoolYearId, flatten = flatten, returnResponse = returnResponse)
	}

	#' List DirectCertificationContactHistories
	#'
	#' This function returns a dataframe or json object of DirectCertificationContactHistories
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given DirectCertificationContactHistories. Defaults to FALSE for all return fields which, for convenience, returns all fields for the DirectCertificationContactHistories.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object listed as text. Run \code{\link{getSchemaForObjects}}('DirectCertificationContactHistory') to get more field paths.
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
	#' @concept Food Service
	#' @return A list of DirectCertificationContactHistories
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	listDirectCertificationContactHistories <- function(searchConditionsList = NULL, DirectCertificationContactHistoryID = F, DirectCertificationID = F, NameID = F, ContactTime = F, ContactMethod = F, Comment = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, fieldPaths = NULL, searchConditionsGroupType = "And", searchSortFieldNamesList = NULL, searchSortFieldNamesDescendingList = NULL, entityId = 1, schoolYearId = NULL, page = 1, pageSize = 100000, flatten = T, returnResponse = F){

		params <- as.list(environment())

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		listSkyObjects(module = "FoodService", objectName = "DirectCertificationContactHistory", searchFields = searchFields %>% append(fieldPaths), page = page, pageSize = pageSize, SearchConditionsList = searchConditionsList, SearchConditionsGroupType = searchConditionsGroupType, SearchSortFieldNamesList = searchSortFieldNamesList, SearchSortFieldNamesDescendingList = searchSortFieldNamesDescendingList, entityId = entityId, schoolYearId = schoolYearId, flatten = flatten, returnResponse = returnResponse)
	}

	#' Get a DirectCertificationContactHistory
	#'
	#' This function returns a dataframe or json object of a DirectCertificationContactHistory
	#' @param DirectCertificationContactHistoryID The ID of the DirectCertificationContactHistory to return.
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given DirectCertificationContactHistory. Defaults to FALSE for all return fields which, for convenience, returns all fields for the DirectCertificationContactHistory.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object listed as text. Run \code{\link{getSchemaForObjects}}('DirectCertificationContactHistory') to get more field paths.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{listSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Food Service
	#' @return A dataframe or of DirectCertificationContactHistory
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	getDirectCertificationContactHistory <- function(DirectCertificationContactHistoryID, DirectCertificationID = F, NameID = F, ContactTime = F, ContactMethod = F, Comment = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, fieldPaths = NULL, entityId = 1, schoolYearId = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment()) %>% purrr::keep(names(.) != "DirectCertificationContactHistoryID")

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		getSkyObject(module = "FoodService", objectName = "DirectCertificationContactHistory", objectId = DirectCertificationContactHistoryID, searchFields = searchFields, entityId = entityId, flatten = flatten, returnResponse = returnResponse)
	}

	#' Delete a DirectCertificationContactHistory
	#'
	#' This function deletes a DirectCertificationContactHistory
	#' @param DirectCertificationContactHistoryID The ID of the DirectCertificationContactHistory to delete
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{listSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Food Service
	#' @return The DirectCertificationContactHistoryID of the deleted DirectCertificationContactHistory.
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	deleteDirectCertificationContactHistory <- function(DirectCertificationContactHistoryID, ignoreWarnings = F, entityId = 1, schoolYearId = NULL, flatten = T, returnResponse = F){

		deleteSkyObject(module = "FoodService", objectName = "DirectCertificationContactHistory", objectId = DirectCertificationContactHistoryID, ignoreWarnings = ignoreWarnings, entityId = entityId, flatten = flatten, returnResponse = returnResponse)
	}

	#' Create a DirectCertificationContactHistory
	#'
	#' This function creates a DirectCertificationContactHistory
	#' @param fieldNames The field values to give the created DirectCertificationContactHistory. Each defaults to NULL.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{listSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Food Service
	#' @return A newly created DirectCertificationContactHistory
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	createDirectCertificationContactHistory <- function(DirectCertificationID = NULL, NameID = NULL, ContactTime = NULL, ContactMethod = NULL, Comment = NULL, entityId = 1, schoolYearId = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment())

		body <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper()) %>% purrr::compact()

		createSkyObject(module = "FoodService", objectName = "DirectCertificationContactHistory", body = list(DataObject = body), searchFields = append("DirectCertificationContactHistoryID", body %>% names()), entityId = entityId, schoolYearId = schoolYearId, flatten = flatten, returnResponse = returnResponse)
	}

	#' Modify a DirectCertificationContactHistory
	#'
	#' This function modifies a DirectCertificationContactHistory
	#' @param fieldNames The field values to give the modified DirectCertificationContactHistory. Each defaults to NULL.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{listSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Food Service
	#' @return The modified DirectCertificationContactHistory
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	modifyDirectCertificationContactHistory <- function(DirectCertificationContactHistoryID, DirectCertificationID = NULL, NameID = NULL, ContactTime = NULL, ContactMethod = NULL, Comment = NULL, entityId = 1, schoolYearId = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment())

		body <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper()) %>% purrr::compact()

		modifySkyObject(module = "FoodService", objectName = "DirectCertificationContactHistory", objectId = DirectCertificationContactHistoryID, body = list(DataObject = body), searchFields = append("DirectCertificationContactHistoryID", body %>% names()), entityId = entityId, schoolYearId = schoolYearId, flatten = flatten, returnResponse = returnResponse)
	}

	#' List FoodServiceConfigDistrictYears
	#'
	#' This function returns a dataframe or json object of FoodServiceConfigDistrictYears
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given FoodServiceConfigDistrictYears. Defaults to FALSE for all return fields which, for convenience, returns all fields for the FoodServiceConfigDistrictYears.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object listed as text. Run \code{\link{getSchemaForObjects}}('FoodServiceConfigDistrictYear') to get more field paths.
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
	#' @concept Food Service
	#' @return A list of FoodServiceConfigDistrictYears
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	listFoodServiceConfigDistrictYears <- function(searchConditionsList = NULL, ConfigDistrictYearID = F, DistrictID = F, SchoolYearID = F, EligibilityCategoryIDFreeDefault = F, EligibilityCategoryIDReducedDefault = F, EligibilityCategoryIDPaidDefault = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, ReimbursementCountByDefaultEntity = F, ConfigDistrictYearIDClonedFrom = F, AllowOnlineApplicationEntry = F, OnlineApplicationEntryPermission = F, fieldPaths = NULL, searchConditionsGroupType = "And", searchSortFieldNamesList = NULL, searchSortFieldNamesDescendingList = NULL, entityId = 1, schoolYearId = NULL, page = 1, pageSize = 100000, flatten = T, returnResponse = F){

		params <- as.list(environment())

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		listSkyObjects(module = "FoodService", objectName = "ConfigDistrictYear", searchFields = searchFields %>% append(fieldPaths), page = page, pageSize = pageSize, SearchConditionsList = searchConditionsList, SearchConditionsGroupType = searchConditionsGroupType, SearchSortFieldNamesList = searchSortFieldNamesList, SearchSortFieldNamesDescendingList = searchSortFieldNamesDescendingList, entityId = entityId, schoolYearId = schoolYearId, flatten = flatten, returnResponse = returnResponse)
	}

	#' Get a FoodServiceConfigDistrictYear
	#'
	#' This function returns a dataframe or json object of a FoodServiceConfigDistrictYear
	#' @param FoodServiceConfigDistrictYearID The ID of the FoodServiceConfigDistrictYear to return.
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given FoodServiceConfigDistrictYear. Defaults to FALSE for all return fields which, for convenience, returns all fields for the FoodServiceConfigDistrictYear.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object listed as text. Run \code{\link{getSchemaForObjects}}('FoodServiceConfigDistrictYear') to get more field paths.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{listSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Food Service
	#' @return A dataframe or of FoodServiceConfigDistrictYear
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	getFoodServiceConfigDistrictYear <- function(FoodServiceConfigDistrictYearID, ConfigDistrictYearID = F, DistrictID = F, SchoolYearID = F, EligibilityCategoryIDFreeDefault = F, EligibilityCategoryIDReducedDefault = F, EligibilityCategoryIDPaidDefault = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, ReimbursementCountByDefaultEntity = F, ConfigDistrictYearIDClonedFrom = F, AllowOnlineApplicationEntry = F, OnlineApplicationEntryPermission = F, fieldPaths = NULL, entityId = 1, schoolYearId = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment()) %>% purrr::keep(names(.) != "FoodServiceConfigDistrictYearID")

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		getSkyObject(module = "FoodService", objectName = "ConfigDistrictYear", objectId = FoodServiceConfigDistrictYearID, searchFields = searchFields, entityId = entityId, flatten = flatten, returnResponse = returnResponse)
	}

	#' Delete a FoodServiceConfigDistrictYear
	#'
	#' This function deletes a FoodServiceConfigDistrictYear
	#' @param FoodServiceConfigDistrictYearID The ID of the FoodServiceConfigDistrictYear to delete
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{listSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Food Service
	#' @return The FoodServiceConfigDistrictYearID of the deleted FoodServiceConfigDistrictYear.
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	deleteFoodServiceConfigDistrictYear <- function(FoodServiceConfigDistrictYearID, ignoreWarnings = F, entityId = 1, schoolYearId = NULL, flatten = T, returnResponse = F){

		deleteSkyObject(module = "FoodService", objectName = "ConfigDistrictYear", objectId = FoodServiceConfigDistrictYearID, ignoreWarnings = ignoreWarnings, entityId = entityId, flatten = flatten, returnResponse = returnResponse)
	}

	#' Create a FoodServiceConfigDistrictYear
	#'
	#' This function creates a FoodServiceConfigDistrictYear
	#' @param fieldNames The field values to give the created FoodServiceConfigDistrictYear. Each defaults to NULL.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{listSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Food Service
	#' @return A newly created FoodServiceConfigDistrictYear
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	createFoodServiceConfigDistrictYear <- function(DistrictID = NULL, SchoolYearID = NULL, EligibilityCategoryIDFreeDefault = NULL, EligibilityCategoryIDReducedDefault = NULL, EligibilityCategoryIDPaidDefault = NULL, ReimbursementCountByDefaultEntity = NULL, ConfigDistrictYearIDClonedFrom = NULL, AllowOnlineApplicationEntry = NULL, OnlineApplicationEntryPermission = NULL, entityId = 1, schoolYearId = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment())

		body <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper()) %>% purrr::compact()

		createSkyObject(module = "FoodService", objectName = "ConfigDistrictYear", body = list(DataObject = body), searchFields = append("ConfigDistrictYearID", body %>% names()), entityId = entityId, schoolYearId = schoolYearId, flatten = flatten, returnResponse = returnResponse)
	}

	#' Modify a FoodServiceConfigDistrictYear
	#'
	#' This function modifies a FoodServiceConfigDistrictYear
	#' @param fieldNames The field values to give the modified FoodServiceConfigDistrictYear. Each defaults to NULL.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{listSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Food Service
	#' @return The modified FoodServiceConfigDistrictYear
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	modifyFoodServiceConfigDistrictYear <- function(ConfigDistrictYearID, DistrictID = NULL, SchoolYearID = NULL, EligibilityCategoryIDFreeDefault = NULL, EligibilityCategoryIDReducedDefault = NULL, EligibilityCategoryIDPaidDefault = NULL, ReimbursementCountByDefaultEntity = NULL, ConfigDistrictYearIDClonedFrom = NULL, AllowOnlineApplicationEntry = NULL, OnlineApplicationEntryPermission = NULL, entityId = 1, schoolYearId = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment())

		body <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper()) %>% purrr::compact()

		modifySkyObject(module = "FoodService", objectName = "ConfigDistrictYear", objectId = ConfigDistrictYearID, body = list(DataObject = body), searchFields = append("ConfigDistrictYearID", body %>% names()), entityId = entityId, schoolYearId = schoolYearId, flatten = flatten, returnResponse = returnResponse)
	}

	#' List FoodServiceConfigEntityGroupYears
	#'
	#' This function returns a dataframe or json object of FoodServiceConfigEntityGroupYears
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given FoodServiceConfigEntityGroupYears. Defaults to FALSE for all return fields which, for convenience, returns all fields for the FoodServiceConfigEntityGroupYears.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object listed as text. Run \code{\link{getSchemaForObjects}}('FoodServiceConfigEntityGroupYear') to get more field paths.
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
	#' @concept Food Service
	#' @return A list of FoodServiceConfigEntityGroupYears
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	listFoodServiceConfigEntityGroupYears <- function(searchConditionsList = NULL, ConfigEntityGroupYearID = F, SchoolYearID = F, EntityID = F, EntityGroupKey = F, PINLength = F, UseStudentNumber = F, EligibilityCategoryIDAdultDefault = F, EligibilityCategoryIDStaffDefault = F, EligibilityCategoryIDStudentDefault = F, PriceCategoryIDAdultDefault = F, PriceCategoryIDStaffDefault = F, PriceCategoryIDStudentDefault = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, AllowCustomerCreateOnEnrollment = F, AllowCustomerCreateOnStaffAdd = F, UseStaffNumber = F, ConfigEntityGroupYearIDClonedFrom = F, ApplicationFormatID = F, fieldPaths = NULL, searchConditionsGroupType = "And", searchSortFieldNamesList = NULL, searchSortFieldNamesDescendingList = NULL, entityId = 1, schoolYearId = NULL, page = 1, pageSize = 100000, flatten = T, returnResponse = F){

		params <- as.list(environment())

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		listSkyObjects(module = "FoodService", objectName = "ConfigEntityGroupYear", searchFields = searchFields %>% append(fieldPaths), page = page, pageSize = pageSize, SearchConditionsList = searchConditionsList, SearchConditionsGroupType = searchConditionsGroupType, SearchSortFieldNamesList = searchSortFieldNamesList, SearchSortFieldNamesDescendingList = searchSortFieldNamesDescendingList, entityId = entityId, schoolYearId = schoolYearId, flatten = flatten, returnResponse = returnResponse)
	}

	#' Get a FoodServiceConfigEntityGroupYear
	#'
	#' This function returns a dataframe or json object of a FoodServiceConfigEntityGroupYear
	#' @param FoodServiceConfigEntityGroupYearID The ID of the FoodServiceConfigEntityGroupYear to return.
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given FoodServiceConfigEntityGroupYear. Defaults to FALSE for all return fields which, for convenience, returns all fields for the FoodServiceConfigEntityGroupYear.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object listed as text. Run \code{\link{getSchemaForObjects}}('FoodServiceConfigEntityGroupYear') to get more field paths.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{listSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Food Service
	#' @return A dataframe or of FoodServiceConfigEntityGroupYear
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	getFoodServiceConfigEntityGroupYear <- function(FoodServiceConfigEntityGroupYearID, ConfigEntityGroupYearID = F, SchoolYearID = F, EntityID = F, EntityGroupKey = F, PINLength = F, UseStudentNumber = F, EligibilityCategoryIDAdultDefault = F, EligibilityCategoryIDStaffDefault = F, EligibilityCategoryIDStudentDefault = F, PriceCategoryIDAdultDefault = F, PriceCategoryIDStaffDefault = F, PriceCategoryIDStudentDefault = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, AllowCustomerCreateOnEnrollment = F, AllowCustomerCreateOnStaffAdd = F, UseStaffNumber = F, ConfigEntityGroupYearIDClonedFrom = F, ApplicationFormatID = F, fieldPaths = NULL, entityId = 1, schoolYearId = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment()) %>% purrr::keep(names(.) != "FoodServiceConfigEntityGroupYearID")

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		getSkyObject(module = "FoodService", objectName = "ConfigEntityGroupYear", objectId = FoodServiceConfigEntityGroupYearID, searchFields = searchFields, entityId = entityId, flatten = flatten, returnResponse = returnResponse)
	}

	#' Delete a FoodServiceConfigEntityGroupYear
	#'
	#' This function deletes a FoodServiceConfigEntityGroupYear
	#' @param FoodServiceConfigEntityGroupYearID The ID of the FoodServiceConfigEntityGroupYear to delete
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{listSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Food Service
	#' @return The FoodServiceConfigEntityGroupYearID of the deleted FoodServiceConfigEntityGroupYear.
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	deleteFoodServiceConfigEntityGroupYear <- function(FoodServiceConfigEntityGroupYearID, ignoreWarnings = F, entityId = 1, schoolYearId = NULL, flatten = T, returnResponse = F){

		deleteSkyObject(module = "FoodService", objectName = "ConfigEntityGroupYear", objectId = FoodServiceConfigEntityGroupYearID, ignoreWarnings = ignoreWarnings, entityId = entityId, flatten = flatten, returnResponse = returnResponse)
	}

	#' Create a FoodServiceConfigEntityGroupYear
	#'
	#' This function creates a FoodServiceConfigEntityGroupYear
	#' @param fieldNames The field values to give the created FoodServiceConfigEntityGroupYear. Each defaults to NULL.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{listSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Food Service
	#' @return A newly created FoodServiceConfigEntityGroupYear
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	createFoodServiceConfigEntityGroupYear <- function(SchoolYearID = NULL, EntityID = NULL, EntityGroupKey = NULL, PINLength = NULL, UseStudentNumber = NULL, EligibilityCategoryIDAdultDefault = NULL, EligibilityCategoryIDStaffDefault = NULL, EligibilityCategoryIDStudentDefault = NULL, PriceCategoryIDAdultDefault = NULL, PriceCategoryIDStaffDefault = NULL, PriceCategoryIDStudentDefault = NULL, AllowCustomerCreateOnEnrollment = NULL, AllowCustomerCreateOnStaffAdd = NULL, UseStaffNumber = NULL, ConfigEntityGroupYearIDClonedFrom = NULL, ApplicationFormatID = NULL, entityId = 1, schoolYearId = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment())

		body <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper()) %>% purrr::compact()

		createSkyObject(module = "FoodService", objectName = "ConfigEntityGroupYear", body = list(DataObject = body), searchFields = append("ConfigEntityGroupYearID", body %>% names()), entityId = entityId, schoolYearId = schoolYearId, flatten = flatten, returnResponse = returnResponse)
	}

	#' Modify a FoodServiceConfigEntityGroupYear
	#'
	#' This function modifies a FoodServiceConfigEntityGroupYear
	#' @param fieldNames The field values to give the modified FoodServiceConfigEntityGroupYear. Each defaults to NULL.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{listSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Food Service
	#' @return The modified FoodServiceConfigEntityGroupYear
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	modifyFoodServiceConfigEntityGroupYear <- function(ConfigEntityGroupYearID, SchoolYearID = NULL, EntityID = NULL, EntityGroupKey = NULL, PINLength = NULL, UseStudentNumber = NULL, EligibilityCategoryIDAdultDefault = NULL, EligibilityCategoryIDStaffDefault = NULL, EligibilityCategoryIDStudentDefault = NULL, PriceCategoryIDAdultDefault = NULL, PriceCategoryIDStaffDefault = NULL, PriceCategoryIDStudentDefault = NULL, AllowCustomerCreateOnEnrollment = NULL, AllowCustomerCreateOnStaffAdd = NULL, UseStaffNumber = NULL, ConfigEntityGroupYearIDClonedFrom = NULL, ApplicationFormatID = NULL, entityId = 1, schoolYearId = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment())

		body <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper()) %>% purrr::compact()

		modifySkyObject(module = "FoodService", objectName = "ConfigEntityGroupYear", objectId = ConfigEntityGroupYearID, body = list(DataObject = body), searchFields = append("ConfigEntityGroupYearID", body %>% names()), entityId = entityId, schoolYearId = schoolYearId, flatten = flatten, returnResponse = returnResponse)
	}

	#' List CategoryDefaults
	#'
	#' This function returns a dataframe or json object of CategoryDefaults
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given CategoryDefaults. Defaults to FALSE for all return fields which, for convenience, returns all fields for the CategoryDefaults.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object listed as text. Run \code{\link{getSchemaForObjects}}('CategoryDefault') to get more field paths.
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
	#' @concept Food Service
	#' @return A list of CategoryDefaults
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	listCategoryDefaults <- function(searchConditionsList = NULL, CategoryDefaultID = F, ConfigEntityGroupYearID = F, EntityGroupKey = F, EligibilityCategoryID = F, PriceCategoryID = F, GradeLevelIDLow = F, GradeLevelIDHigh = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, CategoryDefaultIDClonedFrom = F, fieldPaths = NULL, searchConditionsGroupType = "And", searchSortFieldNamesList = NULL, searchSortFieldNamesDescendingList = NULL, entityId = 1, schoolYearId = NULL, page = 1, pageSize = 100000, flatten = T, returnResponse = F){

		params <- as.list(environment())

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		listSkyObjects(module = "FoodService", objectName = "CategoryDefault", searchFields = searchFields %>% append(fieldPaths), page = page, pageSize = pageSize, SearchConditionsList = searchConditionsList, SearchConditionsGroupType = searchConditionsGroupType, SearchSortFieldNamesList = searchSortFieldNamesList, SearchSortFieldNamesDescendingList = searchSortFieldNamesDescendingList, entityId = entityId, schoolYearId = schoolYearId, flatten = flatten, returnResponse = returnResponse)
	}

	#' Get a CategoryDefault
	#'
	#' This function returns a dataframe or json object of a CategoryDefault
	#' @param CategoryDefaultID The ID of the CategoryDefault to return.
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given CategoryDefault. Defaults to FALSE for all return fields which, for convenience, returns all fields for the CategoryDefault.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object listed as text. Run \code{\link{getSchemaForObjects}}('CategoryDefault') to get more field paths.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{listSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Food Service
	#' @return A dataframe or of CategoryDefault
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	getCategoryDefault <- function(CategoryDefaultID, ConfigEntityGroupYearID = F, EntityGroupKey = F, EligibilityCategoryID = F, PriceCategoryID = F, GradeLevelIDLow = F, GradeLevelIDHigh = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, CategoryDefaultIDClonedFrom = F, fieldPaths = NULL, entityId = 1, schoolYearId = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment()) %>% purrr::keep(names(.) != "CategoryDefaultID")

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		getSkyObject(module = "FoodService", objectName = "CategoryDefault", objectId = CategoryDefaultID, searchFields = searchFields, entityId = entityId, flatten = flatten, returnResponse = returnResponse)
	}

	#' Delete a CategoryDefault
	#'
	#' This function deletes a CategoryDefault
	#' @param CategoryDefaultID The ID of the CategoryDefault to delete
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{listSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Food Service
	#' @return The CategoryDefaultID of the deleted CategoryDefault.
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	deleteCategoryDefault <- function(CategoryDefaultID, ignoreWarnings = F, entityId = 1, schoolYearId = NULL, flatten = T, returnResponse = F){

		deleteSkyObject(module = "FoodService", objectName = "CategoryDefault", objectId = CategoryDefaultID, ignoreWarnings = ignoreWarnings, entityId = entityId, flatten = flatten, returnResponse = returnResponse)
	}

	#' Create a CategoryDefault
	#'
	#' This function creates a CategoryDefault
	#' @param fieldNames The field values to give the created CategoryDefault. Each defaults to NULL.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{listSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Food Service
	#' @return A newly created CategoryDefault
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	createCategoryDefault <- function(ConfigEntityGroupYearID = NULL, EntityGroupKey = NULL, EligibilityCategoryID = NULL, PriceCategoryID = NULL, GradeLevelIDLow = NULL, GradeLevelIDHigh = NULL, CategoryDefaultIDClonedFrom = NULL, entityId = 1, schoolYearId = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment())

		body <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper()) %>% purrr::compact()

		createSkyObject(module = "FoodService", objectName = "CategoryDefault", body = list(DataObject = body), searchFields = append("CategoryDefaultID", body %>% names()), entityId = entityId, schoolYearId = schoolYearId, flatten = flatten, returnResponse = returnResponse)
	}

	#' Modify a CategoryDefault
	#'
	#' This function modifies a CategoryDefault
	#' @param fieldNames The field values to give the modified CategoryDefault. Each defaults to NULL.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{listSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Food Service
	#' @return The modified CategoryDefault
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	modifyCategoryDefault <- function(CategoryDefaultID, ConfigEntityGroupYearID = NULL, EntityGroupKey = NULL, EligibilityCategoryID = NULL, PriceCategoryID = NULL, GradeLevelIDLow = NULL, GradeLevelIDHigh = NULL, CategoryDefaultIDClonedFrom = NULL, entityId = 1, schoolYearId = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment())

		body <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper()) %>% purrr::compact()

		modifySkyObject(module = "FoodService", objectName = "CategoryDefault", objectId = CategoryDefaultID, body = list(DataObject = body), searchFields = append("CategoryDefaultID", body %>% names()), entityId = entityId, schoolYearId = schoolYearId, flatten = flatten, returnResponse = returnResponse)
	}

	#' List AssistanceSources
	#'
	#' This function returns a dataframe or json object of AssistanceSources
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given AssistanceSources. Defaults to FALSE for all return fields which, for convenience, returns all fields for the AssistanceSources.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object listed as text. Run \code{\link{getSchemaForObjects}}('AssistanceSource') to get more field paths.
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
	#' @concept Food Service
	#' @return A list of AssistanceSources
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	listAssistanceSources <- function(searchConditionsList = NULL, AssistanceSourceID = F, Code = F, Description = F, FundingRank = F, CodeDescription = F, ValidYearLow = F, ValidYearHigh = F, IsInvalid = F, SkywardID = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, SkywardHash = F, IsSNAP = F, IsReduced = F, IsIneligible = F, fieldPaths = NULL, searchConditionsGroupType = "And", searchSortFieldNamesList = NULL, searchSortFieldNamesDescendingList = NULL, entityId = 1, schoolYearId = NULL, page = 1, pageSize = 100000, flatten = T, returnResponse = F){

		params <- as.list(environment())

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		listSkyObjects(module = "FoodService", objectName = "AssistanceSource", searchFields = searchFields %>% append(fieldPaths), page = page, pageSize = pageSize, SearchConditionsList = searchConditionsList, SearchConditionsGroupType = searchConditionsGroupType, SearchSortFieldNamesList = searchSortFieldNamesList, SearchSortFieldNamesDescendingList = searchSortFieldNamesDescendingList, entityId = entityId, schoolYearId = schoolYearId, flatten = flatten, returnResponse = returnResponse)
	}

	#' Get an AssistanceSource
	#'
	#' This function returns a dataframe or json object of an AssistanceSource
	#' @param AssistanceSourceID The ID of the AssistanceSource to return.
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given AssistanceSource. Defaults to FALSE for all return fields which, for convenience, returns all fields for the AssistanceSource.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object listed as text. Run \code{\link{getSchemaForObjects}}('AssistanceSource') to get more field paths.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{listSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Food Service
	#' @return A dataframe or of AssistanceSource
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	getAssistanceSource <- function(AssistanceSourceID, Code = F, Description = F, FundingRank = F, CodeDescription = F, ValidYearLow = F, ValidYearHigh = F, IsInvalid = F, SkywardID = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, SkywardHash = F, IsSNAP = F, IsReduced = F, IsIneligible = F, fieldPaths = NULL, entityId = 1, schoolYearId = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment()) %>% purrr::keep(names(.) != "AssistanceSourceID")

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		getSkyObject(module = "FoodService", objectName = "AssistanceSource", objectId = AssistanceSourceID, searchFields = searchFields, entityId = entityId, flatten = flatten, returnResponse = returnResponse)
	}

	#' Delete an AssistanceSource
	#'
	#' This function deletes an AssistanceSource
	#' @param AssistanceSourceID The ID of the AssistanceSource to delete
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{listSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Food Service
	#' @return The AssistanceSourceID of the deleted AssistanceSource.
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	deleteAssistanceSource <- function(AssistanceSourceID, ignoreWarnings = F, entityId = 1, schoolYearId = NULL, flatten = T, returnResponse = F){

		deleteSkyObject(module = "FoodService", objectName = "AssistanceSource", objectId = AssistanceSourceID, ignoreWarnings = ignoreWarnings, entityId = entityId, flatten = flatten, returnResponse = returnResponse)
	}

	#' Create an AssistanceSource
	#'
	#' This function creates an AssistanceSource
	#' @param fieldNames The field values to give the created AssistanceSource. Each defaults to NULL.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{listSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Food Service
	#' @return A newly created AssistanceSource
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	createAssistanceSource <- function(Code = NULL, Description = NULL, ValidYearLow = NULL, ValidYearHigh = NULL, IsInvalid = NULL, IsSNAP = NULL, entityId = 1, schoolYearId = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment())

		body <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper()) %>% purrr::compact()

		createSkyObject(module = "FoodService", objectName = "AssistanceSource", body = list(DataObject = body), searchFields = append("AssistanceSourceID", body %>% names()), entityId = entityId, schoolYearId = schoolYearId, flatten = flatten, returnResponse = returnResponse)
	}

	#' Modify an AssistanceSource
	#'
	#' This function modifies an AssistanceSource
	#' @param fieldNames The field values to give the modified AssistanceSource. Each defaults to NULL.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{listSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Food Service
	#' @return The modified AssistanceSource
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	modifyAssistanceSource <- function(AssistanceSourceID, Code = NULL, Description = NULL, ValidYearLow = NULL, ValidYearHigh = NULL, IsInvalid = NULL, IsSNAP = NULL, entityId = 1, schoolYearId = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment())

		body <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper()) %>% purrr::compact()

		modifySkyObject(module = "FoodService", objectName = "AssistanceSource", objectId = AssistanceSourceID, body = list(DataObject = body), searchFields = append("AssistanceSourceID", body %>% names()), entityId = entityId, schoolYearId = schoolYearId, flatten = flatten, returnResponse = returnResponse)
	}

	#' List ApplicationDenialReasons
	#'
	#' This function returns a dataframe or json object of ApplicationDenialReasons
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given ApplicationDenialReasons. Defaults to FALSE for all return fields which, for convenience, returns all fields for the ApplicationDenialReasons.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object listed as text. Run \code{\link{getSchemaForObjects}}('ApplicationDenialReason') to get more field paths.
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
	#' @concept Food Service
	#' @return A list of ApplicationDenialReasons
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	listApplicationDenialReasons <- function(searchConditionsList = NULL, ApplicationDenialReasonID = F, ApplicationID = F, DenialReasonID = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, fieldPaths = NULL, searchConditionsGroupType = "And", searchSortFieldNamesList = NULL, searchSortFieldNamesDescendingList = NULL, entityId = 1, schoolYearId = NULL, page = 1, pageSize = 100000, flatten = T, returnResponse = F){

		params <- as.list(environment())

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		listSkyObjects(module = "FoodService", objectName = "ApplicationDenialReason", searchFields = searchFields %>% append(fieldPaths), page = page, pageSize = pageSize, SearchConditionsList = searchConditionsList, SearchConditionsGroupType = searchConditionsGroupType, SearchSortFieldNamesList = searchSortFieldNamesList, SearchSortFieldNamesDescendingList = searchSortFieldNamesDescendingList, entityId = entityId, schoolYearId = schoolYearId, flatten = flatten, returnResponse = returnResponse)
	}

	#' Get an ApplicationDenialReason
	#'
	#' This function returns a dataframe or json object of an ApplicationDenialReason
	#' @param ApplicationDenialReasonID The ID of the ApplicationDenialReason to return.
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given ApplicationDenialReason. Defaults to FALSE for all return fields which, for convenience, returns all fields for the ApplicationDenialReason.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object listed as text. Run \code{\link{getSchemaForObjects}}('ApplicationDenialReason') to get more field paths.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{listSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Food Service
	#' @return A dataframe or of ApplicationDenialReason
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	getApplicationDenialReason <- function(ApplicationDenialReasonID, ApplicationID = F, DenialReasonID = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, fieldPaths = NULL, entityId = 1, schoolYearId = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment()) %>% purrr::keep(names(.) != "ApplicationDenialReasonID")

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		getSkyObject(module = "FoodService", objectName = "ApplicationDenialReason", objectId = ApplicationDenialReasonID, searchFields = searchFields, entityId = entityId, flatten = flatten, returnResponse = returnResponse)
	}

	#' Delete an ApplicationDenialReason
	#'
	#' This function deletes an ApplicationDenialReason
	#' @param ApplicationDenialReasonID The ID of the ApplicationDenialReason to delete
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{listSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Food Service
	#' @return The ApplicationDenialReasonID of the deleted ApplicationDenialReason.
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	deleteApplicationDenialReason <- function(ApplicationDenialReasonID, ignoreWarnings = F, entityId = 1, schoolYearId = NULL, flatten = T, returnResponse = F){

		deleteSkyObject(module = "FoodService", objectName = "ApplicationDenialReason", objectId = ApplicationDenialReasonID, ignoreWarnings = ignoreWarnings, entityId = entityId, flatten = flatten, returnResponse = returnResponse)
	}

	#' Create an ApplicationDenialReason
	#'
	#' This function creates an ApplicationDenialReason
	#' @param fieldNames The field values to give the created ApplicationDenialReason. Each defaults to NULL.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{listSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Food Service
	#' @return A newly created ApplicationDenialReason
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	createApplicationDenialReason <- function(ApplicationID = NULL, DenialReasonID = NULL, entityId = 1, schoolYearId = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment())

		body <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper()) %>% purrr::compact()

		createSkyObject(module = "FoodService", objectName = "ApplicationDenialReason", body = list(DataObject = body), searchFields = append("ApplicationDenialReasonID", body %>% names()), entityId = entityId, schoolYearId = schoolYearId, flatten = flatten, returnResponse = returnResponse)
	}

	#' Modify an ApplicationDenialReason
	#'
	#' This function modifies an ApplicationDenialReason
	#' @param fieldNames The field values to give the modified ApplicationDenialReason. Each defaults to NULL.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{listSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Food Service
	#' @return The modified ApplicationDenialReason
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	modifyApplicationDenialReason <- function(ApplicationDenialReasonID, ApplicationID = NULL, DenialReasonID = NULL, entityId = 1, schoolYearId = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment())

		body <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper()) %>% purrr::compact()

		modifySkyObject(module = "FoodService", objectName = "ApplicationDenialReason", objectId = ApplicationDenialReasonID, body = list(DataObject = body), searchFields = append("ApplicationDenialReasonID", body %>% names()), entityId = entityId, schoolYearId = schoolYearId, flatten = flatten, returnResponse = returnResponse)
	}

	#' List Applications
	#'
	#' This function returns a dataframe or json object of Applications
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given Applications. Defaults to FALSE for all return fields which, for convenience, returns all fields for the Applications.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object listed as text. Run \code{\link{getSchemaForObjects}}('Application') to get more field paths.
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
	#' @concept Food Service
	#' @return A list of Applications
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	listApplications <- function(searchConditionsList = NULL, ApplicationID = F, SchoolYearID = F, DistrictID = F, SignatureDate = F, DistrictReceivedDate = F, EffectiveDate = F, HouseholdSize = F, Status = F, ApplicationNumber = F, EntryMethod = F, AssistanceSourceID = F, CaseNumber = F, NoSSN = F, LastFourSSN = F, EligibilityCategoryID = F, IsDenied = F, Comment = F, ExcludeRandomSelectionReasonID = F, SelectedForVerification = F, DirectVerificationComplete = F, IsWIP = F, IsInactive = F, AnnualIncomeTotal = F, IsErrorProne = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, IsApplicationReadOnly = F, PreviouslySelectedForVerification = F, FirstRequestSent = F, SecondRequestSent = F, NoResponse = F, DirectVerificationCompletionDate = F, FirstRequestNoticeDate = F, FirstRequestExpectedDate = F, SecondRequestNoticeDate = F, SecondRequestExpectedDate = F, VerificationResult = F, NoticeType = F, StatusChangeEffectiveDate = F, UserIDVerifiedBy = F, VerificationDate = F, ReasonForChangeIncome = F, ReasonForChangeHouseholdSize = F, ReasonForChangeOther = F, NameIDDeterminedBy = F, DeterminedDate = F, SelectedChangeReasonIncome = F, SelectedChangeReasonHouseHoldSize = F, SelectedChangeReasonDirectlyVerified = F, SelectedChangeReasonNoResponse = F, SelectedChangeReasonOther = F, VerificationComplete = F, IsSelectedForVerificationAsCaseNumber = F, VerifiedForCauseReason = F, VerifiedForCause = F, IsInVerificationPoolSelectedSchool = F, TotalChildIncomeAmount = F, TotalChildFrequency = F, NoChildIncome = F, ConfirmationReviewReplaceApplication = F, BenefitLevelChangeDate = F, VerificationConfirmationDate = F, UserIDConfirmedBy = F, ConfirmationStatus = F, IsVerificationConfirmed = F, ProgramOfEligibility = F, MealCode = F, AcceptFeeManagementOptionalBenefit = F, HasExcludeRandomSelectionReason = F, OnlineApplicationID = F, ApplicationFormatID = F, IsApplicationComplete = F, fieldPaths = NULL, searchConditionsGroupType = "And", searchSortFieldNamesList = NULL, searchSortFieldNamesDescendingList = NULL, entityId = 1, schoolYearId = NULL, page = 1, pageSize = 100000, flatten = T, returnResponse = F){

		params <- as.list(environment())

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		listSkyObjects(module = "FoodService", objectName = "Application", searchFields = searchFields %>% append(fieldPaths), page = page, pageSize = pageSize, SearchConditionsList = searchConditionsList, SearchConditionsGroupType = searchConditionsGroupType, SearchSortFieldNamesList = searchSortFieldNamesList, SearchSortFieldNamesDescendingList = searchSortFieldNamesDescendingList, entityId = entityId, schoolYearId = schoolYearId, flatten = flatten, returnResponse = returnResponse)
	}

	#' Get an Application
	#'
	#' This function returns a dataframe or json object of an Application
	#' @param ApplicationID The ID of the Application to return.
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given Application. Defaults to FALSE for all return fields which, for convenience, returns all fields for the Application.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object listed as text. Run \code{\link{getSchemaForObjects}}('Application') to get more field paths.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{listSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Food Service
	#' @return A dataframe or of Application
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	getApplication <- function(ApplicationID, SchoolYearID = F, DistrictID = F, SignatureDate = F, DistrictReceivedDate = F, EffectiveDate = F, HouseholdSize = F, Status = F, ApplicationNumber = F, EntryMethod = F, AssistanceSourceID = F, CaseNumber = F, NoSSN = F, LastFourSSN = F, EligibilityCategoryID = F, IsDenied = F, Comment = F, ExcludeRandomSelectionReasonID = F, SelectedForVerification = F, DirectVerificationComplete = F, IsWIP = F, IsInactive = F, AnnualIncomeTotal = F, IsErrorProne = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, IsApplicationReadOnly = F, PreviouslySelectedForVerification = F, FirstRequestSent = F, SecondRequestSent = F, NoResponse = F, DirectVerificationCompletionDate = F, FirstRequestNoticeDate = F, FirstRequestExpectedDate = F, SecondRequestNoticeDate = F, SecondRequestExpectedDate = F, VerificationResult = F, NoticeType = F, StatusChangeEffectiveDate = F, UserIDVerifiedBy = F, VerificationDate = F, ReasonForChangeIncome = F, ReasonForChangeHouseholdSize = F, ReasonForChangeOther = F, NameIDDeterminedBy = F, DeterminedDate = F, SelectedChangeReasonIncome = F, SelectedChangeReasonHouseHoldSize = F, SelectedChangeReasonDirectlyVerified = F, SelectedChangeReasonNoResponse = F, SelectedChangeReasonOther = F, VerificationComplete = F, IsSelectedForVerificationAsCaseNumber = F, VerifiedForCauseReason = F, VerifiedForCause = F, IsInVerificationPoolSelectedSchool = F, TotalChildIncomeAmount = F, TotalChildFrequency = F, NoChildIncome = F, ConfirmationReviewReplaceApplication = F, BenefitLevelChangeDate = F, VerificationConfirmationDate = F, UserIDConfirmedBy = F, ConfirmationStatus = F, IsVerificationConfirmed = F, ProgramOfEligibility = F, MealCode = F, AcceptFeeManagementOptionalBenefit = F, HasExcludeRandomSelectionReason = F, OnlineApplicationID = F, ApplicationFormatID = F, IsApplicationComplete = F, fieldPaths = NULL, entityId = 1, schoolYearId = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment()) %>% purrr::keep(names(.) != "ApplicationID")

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		getSkyObject(module = "FoodService", objectName = "Application", objectId = ApplicationID, searchFields = searchFields, entityId = entityId, flatten = flatten, returnResponse = returnResponse)
	}

	#' Delete an Application
	#'
	#' This function deletes an Application
	#' @param ApplicationID The ID of the Application to delete
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{listSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Food Service
	#' @return The ApplicationID of the deleted Application.
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	deleteApplication <- function(ApplicationID, ignoreWarnings = F, entityId = 1, schoolYearId = NULL, flatten = T, returnResponse = F){

		deleteSkyObject(module = "FoodService", objectName = "Application", objectId = ApplicationID, ignoreWarnings = ignoreWarnings, entityId = entityId, flatten = flatten, returnResponse = returnResponse)
	}

	#' Create an Application
	#'
	#' This function creates an Application
	#' @param fieldNames The field values to give the created Application. Each defaults to NULL.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{listSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Food Service
	#' @return A newly created Application
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	createApplication <- function(SchoolYearID = NULL, DistrictID = NULL, SignatureDate = NULL, DistrictReceivedDate = NULL, EffectiveDate = NULL, HouseholdSize = NULL, Status = NULL, ApplicationNumber = NULL, EntryMethod = NULL, AssistanceSourceID = NULL, CaseNumber = NULL, NoSSN = NULL, LastFourSSN = NULL, EligibilityCategoryID = NULL, IsDenied = NULL, Comment = NULL, ExcludeRandomSelectionReasonID = NULL, SelectedForVerification = NULL, DirectVerificationComplete = NULL, PreviouslySelectedForVerification = NULL, FirstRequestSent = NULL, SecondRequestSent = NULL, NoResponse = NULL, DirectVerificationCompletionDate = NULL, FirstRequestNoticeDate = NULL, FirstRequestExpectedDate = NULL, SecondRequestNoticeDate = NULL, SecondRequestExpectedDate = NULL, VerificationResult = NULL, NoticeType = NULL, StatusChangeEffectiveDate = NULL, UserIDVerifiedBy = NULL, VerificationDate = NULL, ReasonForChangeIncome = NULL, ReasonForChangeHouseholdSize = NULL, ReasonForChangeOther = NULL, NameIDDeterminedBy = NULL, DeterminedDate = NULL, SelectedChangeReasonIncome = NULL, SelectedChangeReasonHouseHoldSize = NULL, SelectedChangeReasonDirectlyVerified = NULL, SelectedChangeReasonNoResponse = NULL, SelectedChangeReasonOther = NULL, IsSelectedForVerificationAsCaseNumber = NULL, VerifiedForCauseReason = NULL, VerifiedForCause = NULL, TotalChildIncomeAmount = NULL, TotalChildFrequency = NULL, NoChildIncome = NULL, ConfirmationReviewReplaceApplication = NULL, BenefitLevelChangeDate = NULL, VerificationConfirmationDate = NULL, UserIDConfirmedBy = NULL, ConfirmationStatus = NULL, ProgramOfEligibility = NULL, MealCode = NULL, AcceptFeeManagementOptionalBenefit = NULL, OnlineApplicationID = NULL, ApplicationFormatID = NULL, entityId = 1, schoolYearId = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment())

		body <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper()) %>% purrr::compact()

		createSkyObject(module = "FoodService", objectName = "Application", body = list(DataObject = body), searchFields = append("ApplicationID", body %>% names()), entityId = entityId, schoolYearId = schoolYearId, flatten = flatten, returnResponse = returnResponse)
	}

	#' Modify an Application
	#'
	#' This function modifies an Application
	#' @param fieldNames The field values to give the modified Application. Each defaults to NULL.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{listSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Food Service
	#' @return The modified Application
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	modifyApplication <- function(ApplicationID, SchoolYearID = NULL, DistrictID = NULL, SignatureDate = NULL, DistrictReceivedDate = NULL, EffectiveDate = NULL, HouseholdSize = NULL, Status = NULL, ApplicationNumber = NULL, EntryMethod = NULL, AssistanceSourceID = NULL, CaseNumber = NULL, NoSSN = NULL, LastFourSSN = NULL, EligibilityCategoryID = NULL, IsDenied = NULL, Comment = NULL, ExcludeRandomSelectionReasonID = NULL, SelectedForVerification = NULL, DirectVerificationComplete = NULL, PreviouslySelectedForVerification = NULL, FirstRequestSent = NULL, SecondRequestSent = NULL, NoResponse = NULL, DirectVerificationCompletionDate = NULL, FirstRequestNoticeDate = NULL, FirstRequestExpectedDate = NULL, SecondRequestNoticeDate = NULL, SecondRequestExpectedDate = NULL, VerificationResult = NULL, NoticeType = NULL, StatusChangeEffectiveDate = NULL, UserIDVerifiedBy = NULL, VerificationDate = NULL, ReasonForChangeIncome = NULL, ReasonForChangeHouseholdSize = NULL, ReasonForChangeOther = NULL, NameIDDeterminedBy = NULL, DeterminedDate = NULL, SelectedChangeReasonIncome = NULL, SelectedChangeReasonHouseHoldSize = NULL, SelectedChangeReasonDirectlyVerified = NULL, SelectedChangeReasonNoResponse = NULL, SelectedChangeReasonOther = NULL, IsSelectedForVerificationAsCaseNumber = NULL, VerifiedForCauseReason = NULL, VerifiedForCause = NULL, TotalChildIncomeAmount = NULL, TotalChildFrequency = NULL, NoChildIncome = NULL, ConfirmationReviewReplaceApplication = NULL, BenefitLevelChangeDate = NULL, VerificationConfirmationDate = NULL, UserIDConfirmedBy = NULL, ConfirmationStatus = NULL, ProgramOfEligibility = NULL, MealCode = NULL, AcceptFeeManagementOptionalBenefit = NULL, OnlineApplicationID = NULL, ApplicationFormatID = NULL, entityId = 1, schoolYearId = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment())

		body <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper()) %>% purrr::compact()

		modifySkyObject(module = "FoodService", objectName = "Application", objectId = ApplicationID, body = list(DataObject = body), searchFields = append("ApplicationID", body %>% names()), entityId = entityId, schoolYearId = schoolYearId, flatten = flatten, returnResponse = returnResponse)
	}

	#' List ApplicationMembers
	#'
	#' This function returns a dataframe or json object of ApplicationMembers
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given ApplicationMembers. Defaults to FALSE for all return fields which, for convenience, returns all fields for the ApplicationMembers.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object listed as text. Run \code{\link{getSchemaForObjects}}('ApplicationMember') to get more field paths.
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
	#' @concept Food Service
	#' @return A list of ApplicationMembers
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	listApplicationMembers <- function(searchConditionsList = NULL, ApplicationMemberID = F, NameID = F, ApplicationID = F, FreeformName = F, UseFreeformName = F, NoIncome = F, FosterChild = F, EarningsAmount = F, EarningsFrequency = F, WelfareAmount = F, WelfareFrequency = F, RetirementAmount = F, RetirementFrequency = F, OtherAmount = F, OtherFrequency = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, IsFreeFormChildName = F, IsHeadStartChild = F, IsHomelessChild = F, IsMigrantChild = F, IsRunawayChild = F, WeeklyChildAmount = F, BiWeeklyChildAmount = F, BiMonthlyChildAmount = F, MonthlyChildAmount = F, AnnuallyChildAmount = F, IsHomelessMigrantRunawayChild = F, IsLivingWithParentCaretaker = F, IsStudent = F, BirthDate = F, GradeLevelID = F, BuildingID = F, IsChildMember = F, SchoolID = F, SelfEmploymentAmount = F, SelfEmploymentFrequency = F, StudentNumber = F, fieldPaths = NULL, searchConditionsGroupType = "And", searchSortFieldNamesList = NULL, searchSortFieldNamesDescendingList = NULL, entityId = 1, schoolYearId = NULL, page = 1, pageSize = 100000, flatten = T, returnResponse = F){

		params <- as.list(environment())

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		listSkyObjects(module = "FoodService", objectName = "ApplicationMember", searchFields = searchFields %>% append(fieldPaths), page = page, pageSize = pageSize, SearchConditionsList = searchConditionsList, SearchConditionsGroupType = searchConditionsGroupType, SearchSortFieldNamesList = searchSortFieldNamesList, SearchSortFieldNamesDescendingList = searchSortFieldNamesDescendingList, entityId = entityId, schoolYearId = schoolYearId, flatten = flatten, returnResponse = returnResponse)
	}

	#' Get an ApplicationMember
	#'
	#' This function returns a dataframe or json object of an ApplicationMember
	#' @param ApplicationMemberID The ID of the ApplicationMember to return.
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given ApplicationMember. Defaults to FALSE for all return fields which, for convenience, returns all fields for the ApplicationMember.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object listed as text. Run \code{\link{getSchemaForObjects}}('ApplicationMember') to get more field paths.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{listSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Food Service
	#' @return A dataframe or of ApplicationMember
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	getApplicationMember <- function(ApplicationMemberID, NameID = F, ApplicationID = F, FreeformName = F, UseFreeformName = F, NoIncome = F, FosterChild = F, EarningsAmount = F, EarningsFrequency = F, WelfareAmount = F, WelfareFrequency = F, RetirementAmount = F, RetirementFrequency = F, OtherAmount = F, OtherFrequency = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, IsFreeFormChildName = F, IsHeadStartChild = F, IsHomelessChild = F, IsMigrantChild = F, IsRunawayChild = F, WeeklyChildAmount = F, BiWeeklyChildAmount = F, BiMonthlyChildAmount = F, MonthlyChildAmount = F, AnnuallyChildAmount = F, IsHomelessMigrantRunawayChild = F, IsLivingWithParentCaretaker = F, IsStudent = F, BirthDate = F, GradeLevelID = F, BuildingID = F, IsChildMember = F, SchoolID = F, SelfEmploymentAmount = F, SelfEmploymentFrequency = F, StudentNumber = F, fieldPaths = NULL, entityId = 1, schoolYearId = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment()) %>% purrr::keep(names(.) != "ApplicationMemberID")

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		getSkyObject(module = "FoodService", objectName = "ApplicationMember", objectId = ApplicationMemberID, searchFields = searchFields, entityId = entityId, flatten = flatten, returnResponse = returnResponse)
	}

	#' Delete an ApplicationMember
	#'
	#' This function deletes an ApplicationMember
	#' @param ApplicationMemberID The ID of the ApplicationMember to delete
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{listSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Food Service
	#' @return The ApplicationMemberID of the deleted ApplicationMember.
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	deleteApplicationMember <- function(ApplicationMemberID, ignoreWarnings = F, entityId = 1, schoolYearId = NULL, flatten = T, returnResponse = F){

		deleteSkyObject(module = "FoodService", objectName = "ApplicationMember", objectId = ApplicationMemberID, ignoreWarnings = ignoreWarnings, entityId = entityId, flatten = flatten, returnResponse = returnResponse)
	}

	#' Create an ApplicationMember
	#'
	#' This function creates an ApplicationMember
	#' @param fieldNames The field values to give the created ApplicationMember. Each defaults to NULL.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{listSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Food Service
	#' @return A newly created ApplicationMember
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	createApplicationMember <- function(NameID = NULL, ApplicationID = NULL, FreeformName = NULL, NoIncome = NULL, FosterChild = NULL, EarningsAmount = NULL, EarningsFrequency = NULL, WelfareAmount = NULL, WelfareFrequency = NULL, RetirementAmount = NULL, RetirementFrequency = NULL, OtherAmount = NULL, OtherFrequency = NULL, IsFreeFormChildName = NULL, IsHeadStartChild = NULL, IsHomelessChild = NULL, IsMigrantChild = NULL, IsRunawayChild = NULL, WeeklyChildAmount = NULL, BiWeeklyChildAmount = NULL, BiMonthlyChildAmount = NULL, MonthlyChildAmount = NULL, AnnuallyChildAmount = NULL, IsHomelessMigrantRunawayChild = NULL, IsLivingWithParentCaretaker = NULL, IsStudent = NULL, BirthDate = NULL, GradeLevelID = NULL, BuildingID = NULL, IsChildMember = NULL, SchoolID = NULL, SelfEmploymentAmount = NULL, SelfEmploymentFrequency = NULL, StudentNumber = NULL, entityId = 1, schoolYearId = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment())

		body <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper()) %>% purrr::compact()

		createSkyObject(module = "FoodService", objectName = "ApplicationMember", body = list(DataObject = body), searchFields = append("ApplicationMemberID", body %>% names()), entityId = entityId, schoolYearId = schoolYearId, flatten = flatten, returnResponse = returnResponse)
	}

	#' Modify an ApplicationMember
	#'
	#' This function modifies an ApplicationMember
	#' @param fieldNames The field values to give the modified ApplicationMember. Each defaults to NULL.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{listSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Food Service
	#' @return The modified ApplicationMember
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	modifyApplicationMember <- function(ApplicationMemberID, NameID = NULL, ApplicationID = NULL, FreeformName = NULL, NoIncome = NULL, FosterChild = NULL, EarningsAmount = NULL, EarningsFrequency = NULL, WelfareAmount = NULL, WelfareFrequency = NULL, RetirementAmount = NULL, RetirementFrequency = NULL, OtherAmount = NULL, OtherFrequency = NULL, IsFreeFormChildName = NULL, IsHeadStartChild = NULL, IsHomelessChild = NULL, IsMigrantChild = NULL, IsRunawayChild = NULL, WeeklyChildAmount = NULL, BiWeeklyChildAmount = NULL, BiMonthlyChildAmount = NULL, MonthlyChildAmount = NULL, AnnuallyChildAmount = NULL, IsHomelessMigrantRunawayChild = NULL, IsLivingWithParentCaretaker = NULL, IsStudent = NULL, BirthDate = NULL, GradeLevelID = NULL, BuildingID = NULL, IsChildMember = NULL, SchoolID = NULL, SelfEmploymentAmount = NULL, SelfEmploymentFrequency = NULL, StudentNumber = NULL, entityId = 1, schoolYearId = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment())

		body <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper()) %>% purrr::compact()

		modifySkyObject(module = "FoodService", objectName = "ApplicationMember", objectId = ApplicationMemberID, body = list(DataObject = body), searchFields = append("ApplicationMemberID", body %>% names()), entityId = entityId, schoolYearId = schoolYearId, flatten = flatten, returnResponse = returnResponse)
	}

	#' List FoodServiceConfigDistricts
	#'
	#' This function returns a dataframe or json object of FoodServiceConfigDistricts
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given FoodServiceConfigDistricts. Defaults to FALSE for all return fields which, for convenience, returns all fields for the FoodServiceConfigDistricts.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object listed as text. Run \code{\link{getSchemaForObjects}}('FoodServiceConfigDistrict') to get more field paths.
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
	#' @concept Food Service
	#' @return A list of FoodServiceConfigDistricts
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	listFoodServiceConfigDistricts <- function(searchConditionsList = NULL, ConfigDistrictID = F, DistrictID = F, NextReceiptNumber = F, BankAccountID = F, AccountIDPrepaidLiability = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, UsePIN = F, UseAccountingUpdate = F, UseConfirmationStepDuringVerification = F, CreatePayor = F, CalculateBalanceByFamily = F, OnlineApplicationStateWebsiteURL = F, MediaIDOnlineApplicationInstructions = F, HasInstructions = F, OnlineApplicationStateWebsiteURLDisplayText = F, NameIDOnlineApplicationReviewingOfficial = F, NameEmailIDOnlineApplication = F, OnlineApplicationContactPhoneNumber = F, OnlineApplicationContactPhoneExtension = F, OnlineApplicationOverrideNonDiscriminationStatement = F, OnlineApplicationNonDiscriminationStatementOverride = F, FileDesinationIDBalanceImport = F, BalanceImportFileName = F, FileDestinationIDCustomerCategoryImport = F, CustomerCategoryImportFileName = F, fieldPaths = NULL, searchConditionsGroupType = "And", searchSortFieldNamesList = NULL, searchSortFieldNamesDescendingList = NULL, entityId = 1, schoolYearId = NULL, page = 1, pageSize = 100000, flatten = T, returnResponse = F){

		params <- as.list(environment())

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		listSkyObjects(module = "FoodService", objectName = "ConfigDistrict", searchFields = searchFields %>% append(fieldPaths), page = page, pageSize = pageSize, SearchConditionsList = searchConditionsList, SearchConditionsGroupType = searchConditionsGroupType, SearchSortFieldNamesList = searchSortFieldNamesList, SearchSortFieldNamesDescendingList = searchSortFieldNamesDescendingList, entityId = entityId, schoolYearId = schoolYearId, flatten = flatten, returnResponse = returnResponse)
	}

	#' Get a FoodServiceConfigDistrict
	#'
	#' This function returns a dataframe or json object of a FoodServiceConfigDistrict
	#' @param FoodServiceConfigDistrictID The ID of the FoodServiceConfigDistrict to return.
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given FoodServiceConfigDistrict. Defaults to FALSE for all return fields which, for convenience, returns all fields for the FoodServiceConfigDistrict.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object listed as text. Run \code{\link{getSchemaForObjects}}('FoodServiceConfigDistrict') to get more field paths.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{listSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Food Service
	#' @return A dataframe or of FoodServiceConfigDistrict
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	getFoodServiceConfigDistrict <- function(FoodServiceConfigDistrictID, ConfigDistrictID = F, DistrictID = F, NextReceiptNumber = F, BankAccountID = F, AccountIDPrepaidLiability = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, UsePIN = F, UseAccountingUpdate = F, UseConfirmationStepDuringVerification = F, CreatePayor = F, CalculateBalanceByFamily = F, OnlineApplicationStateWebsiteURL = F, MediaIDOnlineApplicationInstructions = F, HasInstructions = F, OnlineApplicationStateWebsiteURLDisplayText = F, NameIDOnlineApplicationReviewingOfficial = F, NameEmailIDOnlineApplication = F, OnlineApplicationContactPhoneNumber = F, OnlineApplicationContactPhoneExtension = F, OnlineApplicationOverrideNonDiscriminationStatement = F, OnlineApplicationNonDiscriminationStatementOverride = F, FileDesinationIDBalanceImport = F, BalanceImportFileName = F, FileDestinationIDCustomerCategoryImport = F, CustomerCategoryImportFileName = F, fieldPaths = NULL, entityId = 1, schoolYearId = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment()) %>% purrr::keep(names(.) != "FoodServiceConfigDistrictID")

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		getSkyObject(module = "FoodService", objectName = "ConfigDistrict", objectId = FoodServiceConfigDistrictID, searchFields = searchFields, entityId = entityId, flatten = flatten, returnResponse = returnResponse)
	}

	#' Delete a FoodServiceConfigDistrict
	#'
	#' This function deletes a FoodServiceConfigDistrict
	#' @param FoodServiceConfigDistrictID The ID of the FoodServiceConfigDistrict to delete
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{listSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Food Service
	#' @return The FoodServiceConfigDistrictID of the deleted FoodServiceConfigDistrict.
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	deleteFoodServiceConfigDistrict <- function(FoodServiceConfigDistrictID, ignoreWarnings = F, entityId = 1, schoolYearId = NULL, flatten = T, returnResponse = F){

		deleteSkyObject(module = "FoodService", objectName = "ConfigDistrict", objectId = FoodServiceConfigDistrictID, ignoreWarnings = ignoreWarnings, entityId = entityId, flatten = flatten, returnResponse = returnResponse)
	}

	#' Create a FoodServiceConfigDistrict
	#'
	#' This function creates a FoodServiceConfigDistrict
	#' @param fieldNames The field values to give the created FoodServiceConfigDistrict. Each defaults to NULL.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{listSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Food Service
	#' @return A newly created FoodServiceConfigDistrict
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	createFoodServiceConfigDistrict <- function(DistrictID = NULL, NextReceiptNumber = NULL, BankAccountID = NULL, AccountIDPrepaidLiability = NULL, UsePIN = NULL, UseAccountingUpdate = NULL, UseConfirmationStepDuringVerification = NULL, CreatePayor = NULL, CalculateBalanceByFamily = NULL, OnlineApplicationStateWebsiteURL = NULL, MediaIDOnlineApplicationInstructions = NULL, OnlineApplicationStateWebsiteURLDisplayText = NULL, NameIDOnlineApplicationReviewingOfficial = NULL, NameEmailIDOnlineApplication = NULL, OnlineApplicationContactPhoneNumber = NULL, OnlineApplicationContactPhoneExtension = NULL, OnlineApplicationOverrideNonDiscriminationStatement = NULL, OnlineApplicationNonDiscriminationStatementOverride = NULL, FileDesinationIDBalanceImport = NULL, BalanceImportFileName = NULL, FileDestinationIDCustomerCategoryImport = NULL, CustomerCategoryImportFileName = NULL, entityId = 1, schoolYearId = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment())

		body <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper()) %>% purrr::compact()

		createSkyObject(module = "FoodService", objectName = "ConfigDistrict", body = list(DataObject = body), searchFields = append("ConfigDistrictID", body %>% names()), entityId = entityId, schoolYearId = schoolYearId, flatten = flatten, returnResponse = returnResponse)
	}

	#' Modify a FoodServiceConfigDistrict
	#'
	#' This function modifies a FoodServiceConfigDistrict
	#' @param fieldNames The field values to give the modified FoodServiceConfigDistrict. Each defaults to NULL.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{listSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Food Service
	#' @return The modified FoodServiceConfigDistrict
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	modifyFoodServiceConfigDistrict <- function(ConfigDistrictID, DistrictID = NULL, NextReceiptNumber = NULL, BankAccountID = NULL, AccountIDPrepaidLiability = NULL, UsePIN = NULL, UseAccountingUpdate = NULL, UseConfirmationStepDuringVerification = NULL, CreatePayor = NULL, CalculateBalanceByFamily = NULL, OnlineApplicationStateWebsiteURL = NULL, MediaIDOnlineApplicationInstructions = NULL, OnlineApplicationStateWebsiteURLDisplayText = NULL, NameIDOnlineApplicationReviewingOfficial = NULL, NameEmailIDOnlineApplication = NULL, OnlineApplicationContactPhoneNumber = NULL, OnlineApplicationContactPhoneExtension = NULL, OnlineApplicationOverrideNonDiscriminationStatement = NULL, OnlineApplicationNonDiscriminationStatementOverride = NULL, FileDesinationIDBalanceImport = NULL, BalanceImportFileName = NULL, FileDestinationIDCustomerCategoryImport = NULL, CustomerCategoryImportFileName = NULL, entityId = 1, schoolYearId = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment())

		body <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper()) %>% purrr::compact()

		modifySkyObject(module = "FoodService", objectName = "ConfigDistrict", objectId = ConfigDistrictID, body = list(DataObject = body), searchFields = append("ConfigDistrictID", body %>% names()), entityId = entityId, schoolYearId = schoolYearId, flatten = flatten, returnResponse = returnResponse)
	}

	#' List CustomerItemTypeLimits
	#'
	#' This function returns a dataframe or json object of CustomerItemTypeLimits
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given CustomerItemTypeLimits. Defaults to FALSE for all return fields which, for convenience, returns all fields for the CustomerItemTypeLimits.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object listed as text. Run \code{\link{getSchemaForObjects}}('CustomerItemTypeLimit') to get more field paths.
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
	#' @concept Food Service
	#' @return A list of CustomerItemTypeLimits
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	listCustomerItemTypeLimits <- function(searchConditionsList = NULL, CustomerItemTypeLimitID = F, CustomerID = F, ItemTypeID = F, SpendingLimit = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, fieldPaths = NULL, searchConditionsGroupType = "And", searchSortFieldNamesList = NULL, searchSortFieldNamesDescendingList = NULL, entityId = 1, schoolYearId = NULL, page = 1, pageSize = 100000, flatten = T, returnResponse = F){

		params <- as.list(environment())

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		listSkyObjects(module = "FoodService", objectName = "CustomerItemTypeLimit", searchFields = searchFields %>% append(fieldPaths), page = page, pageSize = pageSize, SearchConditionsList = searchConditionsList, SearchConditionsGroupType = searchConditionsGroupType, SearchSortFieldNamesList = searchSortFieldNamesList, SearchSortFieldNamesDescendingList = searchSortFieldNamesDescendingList, entityId = entityId, schoolYearId = schoolYearId, flatten = flatten, returnResponse = returnResponse)
	}

	#' Get a CustomerItemTypeLimit
	#'
	#' This function returns a dataframe or json object of a CustomerItemTypeLimit
	#' @param CustomerItemTypeLimitID The ID of the CustomerItemTypeLimit to return.
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given CustomerItemTypeLimit. Defaults to FALSE for all return fields which, for convenience, returns all fields for the CustomerItemTypeLimit.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object listed as text. Run \code{\link{getSchemaForObjects}}('CustomerItemTypeLimit') to get more field paths.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{listSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Food Service
	#' @return A dataframe or of CustomerItemTypeLimit
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	getCustomerItemTypeLimit <- function(CustomerItemTypeLimitID, CustomerID = F, ItemTypeID = F, SpendingLimit = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, fieldPaths = NULL, entityId = 1, schoolYearId = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment()) %>% purrr::keep(names(.) != "CustomerItemTypeLimitID")

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		getSkyObject(module = "FoodService", objectName = "CustomerItemTypeLimit", objectId = CustomerItemTypeLimitID, searchFields = searchFields, entityId = entityId, flatten = flatten, returnResponse = returnResponse)
	}

	#' Delete a CustomerItemTypeLimit
	#'
	#' This function deletes a CustomerItemTypeLimit
	#' @param CustomerItemTypeLimitID The ID of the CustomerItemTypeLimit to delete
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{listSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Food Service
	#' @return The CustomerItemTypeLimitID of the deleted CustomerItemTypeLimit.
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	deleteCustomerItemTypeLimit <- function(CustomerItemTypeLimitID, ignoreWarnings = F, entityId = 1, schoolYearId = NULL, flatten = T, returnResponse = F){

		deleteSkyObject(module = "FoodService", objectName = "CustomerItemTypeLimit", objectId = CustomerItemTypeLimitID, ignoreWarnings = ignoreWarnings, entityId = entityId, flatten = flatten, returnResponse = returnResponse)
	}

	#' Create a CustomerItemTypeLimit
	#'
	#' This function creates a CustomerItemTypeLimit
	#' @param fieldNames The field values to give the created CustomerItemTypeLimit. Each defaults to NULL.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{listSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Food Service
	#' @return A newly created CustomerItemTypeLimit
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	createCustomerItemTypeLimit <- function(CustomerID = NULL, ItemTypeID = NULL, SpendingLimit = NULL, entityId = 1, schoolYearId = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment())

		body <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper()) %>% purrr::compact()

		createSkyObject(module = "FoodService", objectName = "CustomerItemTypeLimit", body = list(DataObject = body), searchFields = append("CustomerItemTypeLimitID", body %>% names()), entityId = entityId, schoolYearId = schoolYearId, flatten = flatten, returnResponse = returnResponse)
	}

	#' Modify a CustomerItemTypeLimit
	#'
	#' This function modifies a CustomerItemTypeLimit
	#' @param fieldNames The field values to give the modified CustomerItemTypeLimit. Each defaults to NULL.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{listSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Food Service
	#' @return The modified CustomerItemTypeLimit
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	modifyCustomerItemTypeLimit <- function(CustomerItemTypeLimitID, CustomerID = NULL, ItemTypeID = NULL, SpendingLimit = NULL, entityId = 1, schoolYearId = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment())

		body <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper()) %>% purrr::compact()

		modifySkyObject(module = "FoodService", objectName = "CustomerItemTypeLimit", objectId = CustomerItemTypeLimitID, body = list(DataObject = body), searchFields = append("CustomerItemTypeLimitID", body %>% names()), entityId = entityId, schoolYearId = schoolYearId, flatten = flatten, returnResponse = returnResponse)
	}

	#' List LineTabs
	#'
	#' This function returns a dataframe or json object of LineTabs
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given LineTabs. Defaults to FALSE for all return fields which, for convenience, returns all fields for the LineTabs.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object listed as text. Run \code{\link{getSchemaForObjects}}('LineTab') to get more field paths.
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
	#' @concept Food Service
	#' @return A list of LineTabs
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	listLineTabs <- function(searchConditionsList = NULL, LineTabID = F, LineID = F, TabID = F, Order = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, fieldPaths = NULL, searchConditionsGroupType = "And", searchSortFieldNamesList = NULL, searchSortFieldNamesDescendingList = NULL, entityId = 1, schoolYearId = NULL, page = 1, pageSize = 100000, flatten = T, returnResponse = F){

		params <- as.list(environment())

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		listSkyObjects(module = "FoodService", objectName = "LineTab", searchFields = searchFields %>% append(fieldPaths), page = page, pageSize = pageSize, SearchConditionsList = searchConditionsList, SearchConditionsGroupType = searchConditionsGroupType, SearchSortFieldNamesList = searchSortFieldNamesList, SearchSortFieldNamesDescendingList = searchSortFieldNamesDescendingList, entityId = entityId, schoolYearId = schoolYearId, flatten = flatten, returnResponse = returnResponse)
	}

	#' Get a LineTab
	#'
	#' This function returns a dataframe or json object of a LineTab
	#' @param LineTabID The ID of the LineTab to return.
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given LineTab. Defaults to FALSE for all return fields which, for convenience, returns all fields for the LineTab.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object listed as text. Run \code{\link{getSchemaForObjects}}('LineTab') to get more field paths.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{listSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Food Service
	#' @return A dataframe or of LineTab
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	getLineTab <- function(LineTabID, LineID = F, TabID = F, Order = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, fieldPaths = NULL, entityId = 1, schoolYearId = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment()) %>% purrr::keep(names(.) != "LineTabID")

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		getSkyObject(module = "FoodService", objectName = "LineTab", objectId = LineTabID, searchFields = searchFields, entityId = entityId, flatten = flatten, returnResponse = returnResponse)
	}

	#' Delete a LineTab
	#'
	#' This function deletes a LineTab
	#' @param LineTabID The ID of the LineTab to delete
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{listSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Food Service
	#' @return The LineTabID of the deleted LineTab.
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	deleteLineTab <- function(LineTabID, ignoreWarnings = F, entityId = 1, schoolYearId = NULL, flatten = T, returnResponse = F){

		deleteSkyObject(module = "FoodService", objectName = "LineTab", objectId = LineTabID, ignoreWarnings = ignoreWarnings, entityId = entityId, flatten = flatten, returnResponse = returnResponse)
	}

	#' Create a LineTab
	#'
	#' This function creates a LineTab
	#' @param fieldNames The field values to give the created LineTab. Each defaults to NULL.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{listSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Food Service
	#' @return A newly created LineTab
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	createLineTab <- function(LineID = NULL, TabID = NULL, Order = NULL, entityId = 1, schoolYearId = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment())

		body <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper()) %>% purrr::compact()

		createSkyObject(module = "FoodService", objectName = "LineTab", body = list(DataObject = body), searchFields = append("LineTabID", body %>% names()), entityId = entityId, schoolYearId = schoolYearId, flatten = flatten, returnResponse = returnResponse)
	}

	#' Modify a LineTab
	#'
	#' This function modifies a LineTab
	#' @param fieldNames The field values to give the modified LineTab. Each defaults to NULL.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{listSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Food Service
	#' @return The modified LineTab
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	modifyLineTab <- function(LineTabID, LineID = NULL, TabID = NULL, Order = NULL, entityId = 1, schoolYearId = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment())

		body <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper()) %>% purrr::compact()

		modifySkyObject(module = "FoodService", objectName = "LineTab", objectId = LineTabID, body = list(DataObject = body), searchFields = append("LineTabID", body %>% names()), entityId = entityId, schoolYearId = schoolYearId, flatten = flatten, returnResponse = returnResponse)
	}

	#' List EconomicIndicatorMNS
	#'
	#' This function returns a dataframe or json object of EconomicIndicatorMNS
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given EconomicIndicatorMNS. Defaults to FALSE for all return fields which, for convenience, returns all fields for the EconomicIndicatorMNS.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object listed as text. Run \code{\link{getSchemaForObjects}}('EconomicIndicatorMN') to get more field paths.
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
	#' @concept Food Service
	#' @return A list of EconomicIndicatorMNS
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	listEconomicIndicatorMNS <- function(searchConditionsList = NULL, EconomicIndicatorMNID = F, DistrictID = F, StateEconomicIndicatorCodeMNID = F, EntryComment = F, ExitComment = F, SchoolYearID = F, StartDate = F, EndDate = F, IsActive = F, StudentID = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, fieldPaths = NULL, searchConditionsGroupType = "And", searchSortFieldNamesList = NULL, searchSortFieldNamesDescendingList = NULL, entityId = 1, schoolYearId = NULL, page = 1, pageSize = 100000, flatten = T, returnResponse = F){

		params <- as.list(environment())

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		listSkyObjects(module = "FoodService", objectName = "EconomicIndicatorMN", searchFields = searchFields %>% append(fieldPaths), page = page, pageSize = pageSize, SearchConditionsList = searchConditionsList, SearchConditionsGroupType = searchConditionsGroupType, SearchSortFieldNamesList = searchSortFieldNamesList, SearchSortFieldNamesDescendingList = searchSortFieldNamesDescendingList, entityId = entityId, schoolYearId = schoolYearId, flatten = flatten, returnResponse = returnResponse)
	}

	#' Get an EconomicIndicatorMN
	#'
	#' This function returns a dataframe or json object of an EconomicIndicatorMN
	#' @param EconomicIndicatorMNID The ID of the EconomicIndicatorMN to return.
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given EconomicIndicatorMN. Defaults to FALSE for all return fields which, for convenience, returns all fields for the EconomicIndicatorMN.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object listed as text. Run \code{\link{getSchemaForObjects}}('EconomicIndicatorMN') to get more field paths.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{listSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Food Service
	#' @return A dataframe or of EconomicIndicatorMN
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	getEconomicIndicatorMN <- function(EconomicIndicatorMNID, DistrictID = F, StateEconomicIndicatorCodeMNID = F, EntryComment = F, ExitComment = F, SchoolYearID = F, StartDate = F, EndDate = F, IsActive = F, StudentID = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, fieldPaths = NULL, entityId = 1, schoolYearId = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment()) %>% purrr::keep(names(.) != "EconomicIndicatorMNID")

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		getSkyObject(module = "FoodService", objectName = "EconomicIndicatorMN", objectId = EconomicIndicatorMNID, searchFields = searchFields, entityId = entityId, flatten = flatten, returnResponse = returnResponse)
	}

	#' Delete an EconomicIndicatorMN
	#'
	#' This function deletes an EconomicIndicatorMN
	#' @param EconomicIndicatorMNID The ID of the EconomicIndicatorMN to delete
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{listSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Food Service
	#' @return The EconomicIndicatorMNID of the deleted EconomicIndicatorMN.
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	deleteEconomicIndicatorMN <- function(EconomicIndicatorMNID, ignoreWarnings = F, entityId = 1, schoolYearId = NULL, flatten = T, returnResponse = F){

		deleteSkyObject(module = "FoodService", objectName = "EconomicIndicatorMN", objectId = EconomicIndicatorMNID, ignoreWarnings = ignoreWarnings, entityId = entityId, flatten = flatten, returnResponse = returnResponse)
	}

	#' Create an EconomicIndicatorMN
	#'
	#' This function creates an EconomicIndicatorMN
	#' @param fieldNames The field values to give the created EconomicIndicatorMN. Each defaults to NULL.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{listSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Food Service
	#' @return A newly created EconomicIndicatorMN
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	createEconomicIndicatorMN <- function(DistrictID = NULL, StateEconomicIndicatorCodeMNID = NULL, EntryComment = NULL, ExitComment = NULL, SchoolYearID = NULL, StartDate = NULL, EndDate = NULL, StudentID = NULL, entityId = 1, schoolYearId = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment())

		body <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper()) %>% purrr::compact()

		createSkyObject(module = "FoodService", objectName = "EconomicIndicatorMN", body = list(DataObject = body), searchFields = append("EconomicIndicatorMNID", body %>% names()), entityId = entityId, schoolYearId = schoolYearId, flatten = flatten, returnResponse = returnResponse)
	}

	#' Modify an EconomicIndicatorMN
	#'
	#' This function modifies an EconomicIndicatorMN
	#' @param fieldNames The field values to give the modified EconomicIndicatorMN. Each defaults to NULL.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{listSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Food Service
	#' @return The modified EconomicIndicatorMN
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	modifyEconomicIndicatorMN <- function(EconomicIndicatorMNID, DistrictID = NULL, StateEconomicIndicatorCodeMNID = NULL, EntryComment = NULL, ExitComment = NULL, SchoolYearID = NULL, StartDate = NULL, EndDate = NULL, StudentID = NULL, entityId = 1, schoolYearId = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment())

		body <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper()) %>% purrr::compact()

		modifySkyObject(module = "FoodService", objectName = "EconomicIndicatorMN", objectId = EconomicIndicatorMNID, body = list(DataObject = body), searchFields = append("EconomicIndicatorMNID", body %>% names()), entityId = entityId, schoolYearId = schoolYearId, flatten = flatten, returnResponse = returnResponse)
	}

	#' List Tabs
	#'
	#' This function returns a dataframe or json object of Tabs
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given Tabs. Defaults to FALSE for all return fields which, for convenience, returns all fields for the Tabs.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object listed as text. Run \code{\link{getSchemaForObjects}}('Tab') to get more field paths.
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
	#' @concept Food Service
	#' @return A list of Tabs
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	listTabs <- function(searchConditionsList = NULL, TabID = F, DistrictID = F, StartTime = F, StopTime = F, DisplayName = F, Description = F, NumberOfColumns = F, NumberOfRows = F, NumberOfItems = F, LinesAssigned = F, MaxItems = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, fieldPaths = NULL, searchConditionsGroupType = "And", searchSortFieldNamesList = NULL, searchSortFieldNamesDescendingList = NULL, entityId = 1, schoolYearId = NULL, page = 1, pageSize = 100000, flatten = T, returnResponse = F){

		params <- as.list(environment())

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		listSkyObjects(module = "FoodService", objectName = "Tab", searchFields = searchFields %>% append(fieldPaths), page = page, pageSize = pageSize, SearchConditionsList = searchConditionsList, SearchConditionsGroupType = searchConditionsGroupType, SearchSortFieldNamesList = searchSortFieldNamesList, SearchSortFieldNamesDescendingList = searchSortFieldNamesDescendingList, entityId = entityId, schoolYearId = schoolYearId, flatten = flatten, returnResponse = returnResponse)
	}

	#' Get a Tab
	#'
	#' This function returns a dataframe or json object of a Tab
	#' @param TabID The ID of the Tab to return.
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given Tab. Defaults to FALSE for all return fields which, for convenience, returns all fields for the Tab.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object listed as text. Run \code{\link{getSchemaForObjects}}('Tab') to get more field paths.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{listSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Food Service
	#' @return A dataframe or of Tab
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	getTab <- function(TabID, DistrictID = F, StartTime = F, StopTime = F, DisplayName = F, Description = F, NumberOfColumns = F, NumberOfRows = F, NumberOfItems = F, LinesAssigned = F, MaxItems = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, fieldPaths = NULL, entityId = 1, schoolYearId = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment()) %>% purrr::keep(names(.) != "TabID")

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		getSkyObject(module = "FoodService", objectName = "Tab", objectId = TabID, searchFields = searchFields, entityId = entityId, flatten = flatten, returnResponse = returnResponse)
	}

	#' Delete a Tab
	#'
	#' This function deletes a Tab
	#' @param TabID The ID of the Tab to delete
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{listSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Food Service
	#' @return The TabID of the deleted Tab.
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	deleteTab <- function(TabID, ignoreWarnings = F, entityId = 1, schoolYearId = NULL, flatten = T, returnResponse = F){

		deleteSkyObject(module = "FoodService", objectName = "Tab", objectId = TabID, ignoreWarnings = ignoreWarnings, entityId = entityId, flatten = flatten, returnResponse = returnResponse)
	}

	#' Create a Tab
	#'
	#' This function creates a Tab
	#' @param fieldNames The field values to give the created Tab. Each defaults to NULL.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{listSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Food Service
	#' @return A newly created Tab
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	createTab <- function(DistrictID = NULL, StartTime = NULL, StopTime = NULL, DisplayName = NULL, Description = NULL, NumberOfColumns = NULL, NumberOfRows = NULL, entityId = 1, schoolYearId = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment())

		body <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper()) %>% purrr::compact()

		createSkyObject(module = "FoodService", objectName = "Tab", body = list(DataObject = body), searchFields = append("TabID", body %>% names()), entityId = entityId, schoolYearId = schoolYearId, flatten = flatten, returnResponse = returnResponse)
	}

	#' Modify a Tab
	#'
	#' This function modifies a Tab
	#' @param fieldNames The field values to give the modified Tab. Each defaults to NULL.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{listSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Food Service
	#' @return The modified Tab
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	modifyTab <- function(TabID, DistrictID = NULL, StartTime = NULL, StopTime = NULL, DisplayName = NULL, Description = NULL, NumberOfColumns = NULL, NumberOfRows = NULL, entityId = 1, schoolYearId = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment())

		body <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper()) %>% purrr::compact()

		modifySkyObject(module = "FoodService", objectName = "Tab", objectId = TabID, body = list(DataObject = body), searchFields = append("TabID", body %>% names()), entityId = entityId, schoolYearId = schoolYearId, flatten = flatten, returnResponse = returnResponse)
	}

	#' List TabItems
	#'
	#' This function returns a dataframe or json object of TabItems
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given TabItems. Defaults to FALSE for all return fields which, for convenience, returns all fields for the TabItems.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object listed as text. Run \code{\link{getSchemaForObjects}}('TabItem') to get more field paths.
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
	#' @concept Food Service
	#' @return A list of TabItems
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	listTabItems <- function(searchConditionsList = NULL, TabItemID = F, TabID = F, ItemID = F, Column = F, Row = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, fieldPaths = NULL, searchConditionsGroupType = "And", searchSortFieldNamesList = NULL, searchSortFieldNamesDescendingList = NULL, entityId = 1, schoolYearId = NULL, page = 1, pageSize = 100000, flatten = T, returnResponse = F){

		params <- as.list(environment())

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		listSkyObjects(module = "FoodService", objectName = "TabItem", searchFields = searchFields %>% append(fieldPaths), page = page, pageSize = pageSize, SearchConditionsList = searchConditionsList, SearchConditionsGroupType = searchConditionsGroupType, SearchSortFieldNamesList = searchSortFieldNamesList, SearchSortFieldNamesDescendingList = searchSortFieldNamesDescendingList, entityId = entityId, schoolYearId = schoolYearId, flatten = flatten, returnResponse = returnResponse)
	}

	#' Get a TabItem
	#'
	#' This function returns a dataframe or json object of a TabItem
	#' @param TabItemID The ID of the TabItem to return.
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given TabItem. Defaults to FALSE for all return fields which, for convenience, returns all fields for the TabItem.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object listed as text. Run \code{\link{getSchemaForObjects}}('TabItem') to get more field paths.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{listSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Food Service
	#' @return A dataframe or of TabItem
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	getTabItem <- function(TabItemID, TabID = F, ItemID = F, Column = F, Row = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, fieldPaths = NULL, entityId = 1, schoolYearId = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment()) %>% purrr::keep(names(.) != "TabItemID")

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		getSkyObject(module = "FoodService", objectName = "TabItem", objectId = TabItemID, searchFields = searchFields, entityId = entityId, flatten = flatten, returnResponse = returnResponse)
	}

	#' Delete a TabItem
	#'
	#' This function deletes a TabItem
	#' @param TabItemID The ID of the TabItem to delete
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{listSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Food Service
	#' @return The TabItemID of the deleted TabItem.
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	deleteTabItem <- function(TabItemID, ignoreWarnings = F, entityId = 1, schoolYearId = NULL, flatten = T, returnResponse = F){

		deleteSkyObject(module = "FoodService", objectName = "TabItem", objectId = TabItemID, ignoreWarnings = ignoreWarnings, entityId = entityId, flatten = flatten, returnResponse = returnResponse)
	}

	#' Create a TabItem
	#'
	#' This function creates a TabItem
	#' @param fieldNames The field values to give the created TabItem. Each defaults to NULL.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{listSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Food Service
	#' @return A newly created TabItem
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	createTabItem <- function(TabID = NULL, ItemID = NULL, Column = NULL, Row = NULL, entityId = 1, schoolYearId = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment())

		body <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper()) %>% purrr::compact()

		createSkyObject(module = "FoodService", objectName = "TabItem", body = list(DataObject = body), searchFields = append("TabItemID", body %>% names()), entityId = entityId, schoolYearId = schoolYearId, flatten = flatten, returnResponse = returnResponse)
	}

	#' Modify a TabItem
	#'
	#' This function modifies a TabItem
	#' @param fieldNames The field values to give the modified TabItem. Each defaults to NULL.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{listSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Food Service
	#' @return The modified TabItem
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	modifyTabItem <- function(TabItemID, TabID = NULL, ItemID = NULL, Column = NULL, Row = NULL, entityId = 1, schoolYearId = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment())

		body <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper()) %>% purrr::compact()

		modifySkyObject(module = "FoodService", objectName = "TabItem", objectId = TabItemID, body = list(DataObject = body), searchFields = append("TabItemID", body %>% names()), entityId = entityId, schoolYearId = schoolYearId, flatten = flatten, returnResponse = returnResponse)
	}

	#' List LineStandardItems
	#'
	#' This function returns a dataframe or json object of LineStandardItems
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given LineStandardItems. Defaults to FALSE for all return fields which, for convenience, returns all fields for the LineStandardItems.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object listed as text. Run \code{\link{getSchemaForObjects}}('LineStandardItem') to get more field paths.
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
	#' @concept Food Service
	#' @return A list of LineStandardItems
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	listLineStandardItems <- function(searchConditionsList = NULL, LineStandardItemID = F, LineID = F, ItemID = F, StartTime = F, EndTime = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, fieldPaths = NULL, searchConditionsGroupType = "And", searchSortFieldNamesList = NULL, searchSortFieldNamesDescendingList = NULL, entityId = 1, schoolYearId = NULL, page = 1, pageSize = 100000, flatten = T, returnResponse = F){

		params <- as.list(environment())

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		listSkyObjects(module = "FoodService", objectName = "LineStandardItem", searchFields = searchFields %>% append(fieldPaths), page = page, pageSize = pageSize, SearchConditionsList = searchConditionsList, SearchConditionsGroupType = searchConditionsGroupType, SearchSortFieldNamesList = searchSortFieldNamesList, SearchSortFieldNamesDescendingList = searchSortFieldNamesDescendingList, entityId = entityId, schoolYearId = schoolYearId, flatten = flatten, returnResponse = returnResponse)
	}

	#' Get a LineStandardItem
	#'
	#' This function returns a dataframe or json object of a LineStandardItem
	#' @param LineStandardItemID The ID of the LineStandardItem to return.
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given LineStandardItem. Defaults to FALSE for all return fields which, for convenience, returns all fields for the LineStandardItem.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object listed as text. Run \code{\link{getSchemaForObjects}}('LineStandardItem') to get more field paths.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{listSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Food Service
	#' @return A dataframe or of LineStandardItem
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	getLineStandardItem <- function(LineStandardItemID, LineID = F, ItemID = F, StartTime = F, EndTime = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, fieldPaths = NULL, entityId = 1, schoolYearId = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment()) %>% purrr::keep(names(.) != "LineStandardItemID")

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		getSkyObject(module = "FoodService", objectName = "LineStandardItem", objectId = LineStandardItemID, searchFields = searchFields, entityId = entityId, flatten = flatten, returnResponse = returnResponse)
	}

	#' Delete a LineStandardItem
	#'
	#' This function deletes a LineStandardItem
	#' @param LineStandardItemID The ID of the LineStandardItem to delete
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{listSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Food Service
	#' @return The LineStandardItemID of the deleted LineStandardItem.
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	deleteLineStandardItem <- function(LineStandardItemID, ignoreWarnings = F, entityId = 1, schoolYearId = NULL, flatten = T, returnResponse = F){

		deleteSkyObject(module = "FoodService", objectName = "LineStandardItem", objectId = LineStandardItemID, ignoreWarnings = ignoreWarnings, entityId = entityId, flatten = flatten, returnResponse = returnResponse)
	}

	#' Create a LineStandardItem
	#'
	#' This function creates a LineStandardItem
	#' @param fieldNames The field values to give the created LineStandardItem. Each defaults to NULL.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{listSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Food Service
	#' @return A newly created LineStandardItem
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	createLineStandardItem <- function(LineID = NULL, ItemID = NULL, StartTime = NULL, EndTime = NULL, entityId = 1, schoolYearId = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment())

		body <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper()) %>% purrr::compact()

		createSkyObject(module = "FoodService", objectName = "LineStandardItem", body = list(DataObject = body), searchFields = append("LineStandardItemID", body %>% names()), entityId = entityId, schoolYearId = schoolYearId, flatten = flatten, returnResponse = returnResponse)
	}

	#' Modify a LineStandardItem
	#'
	#' This function modifies a LineStandardItem
	#' @param fieldNames The field values to give the modified LineStandardItem. Each defaults to NULL.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{listSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Food Service
	#' @return The modified LineStandardItem
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	modifyLineStandardItem <- function(LineStandardItemID, LineID = NULL, ItemID = NULL, StartTime = NULL, EndTime = NULL, entityId = 1, schoolYearId = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment())

		body <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper()) %>% purrr::compact()

		modifySkyObject(module = "FoodService", objectName = "LineStandardItem", objectId = LineStandardItemID, body = list(DataObject = body), searchFields = append("LineStandardItemID", body %>% names()), entityId = entityId, schoolYearId = schoolYearId, flatten = flatten, returnResponse = returnResponse)
	}

	#' List DirectCertifications
	#'
	#' This function returns a dataframe or json object of DirectCertifications
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given DirectCertifications. Defaults to FALSE for all return fields which, for convenience, returns all fields for the DirectCertifications.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object listed as text. Run \code{\link{getSchemaForObjects}}('DirectCertification') to get more field paths.
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
	#' @concept Food Service
	#' @return A list of DirectCertifications
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	listDirectCertifications <- function(searchConditionsList = NULL, DirectCertificationID = F, DirectCertificationImportDetailID = F, SchoolYearID = F, AssistanceSourceID = F, CustomerID = F, IsDenied = F, EffectiveDate = F, Status = F, EntryMethod = F, Comment = F, CaseNumber = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, IsSNAPLetterCertification = F, fieldPaths = NULL, searchConditionsGroupType = "And", searchSortFieldNamesList = NULL, searchSortFieldNamesDescendingList = NULL, entityId = 1, schoolYearId = NULL, page = 1, pageSize = 100000, flatten = T, returnResponse = F){

		params <- as.list(environment())

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		listSkyObjects(module = "FoodService", objectName = "DirectCertification", searchFields = searchFields %>% append(fieldPaths), page = page, pageSize = pageSize, SearchConditionsList = searchConditionsList, SearchConditionsGroupType = searchConditionsGroupType, SearchSortFieldNamesList = searchSortFieldNamesList, SearchSortFieldNamesDescendingList = searchSortFieldNamesDescendingList, entityId = entityId, schoolYearId = schoolYearId, flatten = flatten, returnResponse = returnResponse)
	}

	#' Get a DirectCertification
	#'
	#' This function returns a dataframe or json object of a DirectCertification
	#' @param DirectCertificationID The ID of the DirectCertification to return.
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given DirectCertification. Defaults to FALSE for all return fields which, for convenience, returns all fields for the DirectCertification.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object listed as text. Run \code{\link{getSchemaForObjects}}('DirectCertification') to get more field paths.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{listSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Food Service
	#' @return A dataframe or of DirectCertification
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	getDirectCertification <- function(DirectCertificationID, DirectCertificationImportDetailID = F, SchoolYearID = F, AssistanceSourceID = F, CustomerID = F, IsDenied = F, EffectiveDate = F, Status = F, EntryMethod = F, Comment = F, CaseNumber = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, IsSNAPLetterCertification = F, fieldPaths = NULL, entityId = 1, schoolYearId = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment()) %>% purrr::keep(names(.) != "DirectCertificationID")

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		getSkyObject(module = "FoodService", objectName = "DirectCertification", objectId = DirectCertificationID, searchFields = searchFields, entityId = entityId, flatten = flatten, returnResponse = returnResponse)
	}

	#' Delete a DirectCertification
	#'
	#' This function deletes a DirectCertification
	#' @param DirectCertificationID The ID of the DirectCertification to delete
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{listSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Food Service
	#' @return The DirectCertificationID of the deleted DirectCertification.
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	deleteDirectCertification <- function(DirectCertificationID, ignoreWarnings = F, entityId = 1, schoolYearId = NULL, flatten = T, returnResponse = F){

		deleteSkyObject(module = "FoodService", objectName = "DirectCertification", objectId = DirectCertificationID, ignoreWarnings = ignoreWarnings, entityId = entityId, flatten = flatten, returnResponse = returnResponse)
	}

	#' Create a DirectCertification
	#'
	#' This function creates a DirectCertification
	#' @param fieldNames The field values to give the created DirectCertification. Each defaults to NULL.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{listSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Food Service
	#' @return A newly created DirectCertification
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	createDirectCertification <- function(DirectCertificationImportDetailID = NULL, SchoolYearID = NULL, AssistanceSourceID = NULL, CustomerID = NULL, IsDenied = NULL, EffectiveDate = NULL, Status = NULL, EntryMethod = NULL, Comment = NULL, CaseNumber = NULL, IsSNAPLetterCertification = NULL, entityId = 1, schoolYearId = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment())

		body <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper()) %>% purrr::compact()

		createSkyObject(module = "FoodService", objectName = "DirectCertification", body = list(DataObject = body), searchFields = append("DirectCertificationID", body %>% names()), entityId = entityId, schoolYearId = schoolYearId, flatten = flatten, returnResponse = returnResponse)
	}

	#' Modify a DirectCertification
	#'
	#' This function modifies a DirectCertification
	#' @param fieldNames The field values to give the modified DirectCertification. Each defaults to NULL.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{listSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Food Service
	#' @return The modified DirectCertification
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	modifyDirectCertification <- function(DirectCertificationID, DirectCertificationImportDetailID = NULL, SchoolYearID = NULL, AssistanceSourceID = NULL, CustomerID = NULL, IsDenied = NULL, EffectiveDate = NULL, Status = NULL, EntryMethod = NULL, Comment = NULL, CaseNumber = NULL, IsSNAPLetterCertification = NULL, entityId = 1, schoolYearId = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment())

		body <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper()) %>% purrr::compact()

		modifySkyObject(module = "FoodService", objectName = "DirectCertification", objectId = DirectCertificationID, body = list(DataObject = body), searchFields = append("DirectCertificationID", body %>% names()), entityId = entityId, schoolYearId = schoolYearId, flatten = flatten, returnResponse = returnResponse)
	}

	#' List DirectCertificationImportDetails
	#'
	#' This function returns a dataframe or json object of DirectCertificationImportDetails
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given DirectCertificationImportDetails. Defaults to FALSE for all return fields which, for convenience, returns all fields for the DirectCertificationImportDetails.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object listed as text. Run \code{\link{getSchemaForObjects}}('DirectCertificationImportDetail') to get more field paths.
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
	#' @concept Food Service
	#' @return A list of DirectCertificationImportDetails
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	listDirectCertificationImportDetails <- function(searchConditionsList = NULL, DirectCertificationImportDetailID = F, DirectCertificationImportID = F, NameID = F, FirstName = F, MiddleName = F, LastName = F, BirthDate = F, AssistanceSource = F, Eligibility = F, Status = F, Comment = F, CaseNumber = F, SourceID = F, StateSpecificStudentNumber = F, Gender = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, FreeOrReducedIdentifier = F, fieldPaths = NULL, searchConditionsGroupType = "And", searchSortFieldNamesList = NULL, searchSortFieldNamesDescendingList = NULL, entityId = 1, schoolYearId = NULL, page = 1, pageSize = 100000, flatten = T, returnResponse = F){

		params <- as.list(environment())

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		listSkyObjects(module = "FoodService", objectName = "DirectCertificationImportDetail", searchFields = searchFields %>% append(fieldPaths), page = page, pageSize = pageSize, SearchConditionsList = searchConditionsList, SearchConditionsGroupType = searchConditionsGroupType, SearchSortFieldNamesList = searchSortFieldNamesList, SearchSortFieldNamesDescendingList = searchSortFieldNamesDescendingList, entityId = entityId, schoolYearId = schoolYearId, flatten = flatten, returnResponse = returnResponse)
	}

	#' Get a DirectCertificationImportDetail
	#'
	#' This function returns a dataframe or json object of a DirectCertificationImportDetail
	#' @param DirectCertificationImportDetailID The ID of the DirectCertificationImportDetail to return.
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given DirectCertificationImportDetail. Defaults to FALSE for all return fields which, for convenience, returns all fields for the DirectCertificationImportDetail.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object listed as text. Run \code{\link{getSchemaForObjects}}('DirectCertificationImportDetail') to get more field paths.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{listSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Food Service
	#' @return A dataframe or of DirectCertificationImportDetail
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	getDirectCertificationImportDetail <- function(DirectCertificationImportDetailID, DirectCertificationImportID = F, NameID = F, FirstName = F, MiddleName = F, LastName = F, BirthDate = F, AssistanceSource = F, Eligibility = F, Status = F, Comment = F, CaseNumber = F, SourceID = F, StateSpecificStudentNumber = F, Gender = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, FreeOrReducedIdentifier = F, fieldPaths = NULL, entityId = 1, schoolYearId = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment()) %>% purrr::keep(names(.) != "DirectCertificationImportDetailID")

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		getSkyObject(module = "FoodService", objectName = "DirectCertificationImportDetail", objectId = DirectCertificationImportDetailID, searchFields = searchFields, entityId = entityId, flatten = flatten, returnResponse = returnResponse)
	}

	#' Delete a DirectCertificationImportDetail
	#'
	#' This function deletes a DirectCertificationImportDetail
	#' @param DirectCertificationImportDetailID The ID of the DirectCertificationImportDetail to delete
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{listSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Food Service
	#' @return The DirectCertificationImportDetailID of the deleted DirectCertificationImportDetail.
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	deleteDirectCertificationImportDetail <- function(DirectCertificationImportDetailID, ignoreWarnings = F, entityId = 1, schoolYearId = NULL, flatten = T, returnResponse = F){

		deleteSkyObject(module = "FoodService", objectName = "DirectCertificationImportDetail", objectId = DirectCertificationImportDetailID, ignoreWarnings = ignoreWarnings, entityId = entityId, flatten = flatten, returnResponse = returnResponse)
	}

	#' Create a DirectCertificationImportDetail
	#'
	#' This function creates a DirectCertificationImportDetail
	#' @param fieldNames The field values to give the created DirectCertificationImportDetail. Each defaults to NULL.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{listSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Food Service
	#' @return A newly created DirectCertificationImportDetail
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	createDirectCertificationImportDetail <- function(DirectCertificationImportID = NULL, NameID = NULL, FirstName = NULL, MiddleName = NULL, LastName = NULL, BirthDate = NULL, AssistanceSource = NULL, Eligibility = NULL, Status = NULL, Comment = NULL, CaseNumber = NULL, SourceID = NULL, StateSpecificStudentNumber = NULL, Gender = NULL, FreeOrReducedIdentifier = NULL, entityId = 1, schoolYearId = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment())

		body <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper()) %>% purrr::compact()

		createSkyObject(module = "FoodService", objectName = "DirectCertificationImportDetail", body = list(DataObject = body), searchFields = append("DirectCertificationImportDetailID", body %>% names()), entityId = entityId, schoolYearId = schoolYearId, flatten = flatten, returnResponse = returnResponse)
	}

	#' Modify a DirectCertificationImportDetail
	#'
	#' This function modifies a DirectCertificationImportDetail
	#' @param fieldNames The field values to give the modified DirectCertificationImportDetail. Each defaults to NULL.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{listSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Food Service
	#' @return The modified DirectCertificationImportDetail
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	modifyDirectCertificationImportDetail <- function(DirectCertificationImportDetailID, DirectCertificationImportID = NULL, NameID = NULL, FirstName = NULL, MiddleName = NULL, LastName = NULL, BirthDate = NULL, AssistanceSource = NULL, Eligibility = NULL, Status = NULL, Comment = NULL, CaseNumber = NULL, SourceID = NULL, StateSpecificStudentNumber = NULL, Gender = NULL, FreeOrReducedIdentifier = NULL, entityId = 1, schoolYearId = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment())

		body <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper()) %>% purrr::compact()

		modifySkyObject(module = "FoodService", objectName = "DirectCertificationImportDetail", objectId = DirectCertificationImportDetailID, body = list(DataObject = body), searchFields = append("DirectCertificationImportDetailID", body %>% names()), entityId = entityId, schoolYearId = schoolYearId, flatten = flatten, returnResponse = returnResponse)
	}

	#' List DirectCertificationImports
	#'
	#' This function returns a dataframe or json object of DirectCertificationImports
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given DirectCertificationImports. Defaults to FALSE for all return fields which, for convenience, returns all fields for the DirectCertificationImports.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object listed as text. Run \code{\link{getSchemaForObjects}}('DirectCertificationImport') to get more field paths.
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
	#' @concept Food Service
	#' @return A list of DirectCertificationImports
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	listDirectCertificationImports <- function(searchConditionsList = NULL, DirectCertificationImportID = F, DistrictID = F, SchoolYearID = F, EligibilityCategoryIDFree = F, MediaID = F, EffectiveDate = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, EligibilityCategoryIDReduced = F, fieldPaths = NULL, searchConditionsGroupType = "And", searchSortFieldNamesList = NULL, searchSortFieldNamesDescendingList = NULL, entityId = 1, schoolYearId = NULL, page = 1, pageSize = 100000, flatten = T, returnResponse = F){

		params <- as.list(environment())

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		listSkyObjects(module = "FoodService", objectName = "DirectCertificationImport", searchFields = searchFields %>% append(fieldPaths), page = page, pageSize = pageSize, SearchConditionsList = searchConditionsList, SearchConditionsGroupType = searchConditionsGroupType, SearchSortFieldNamesList = searchSortFieldNamesList, SearchSortFieldNamesDescendingList = searchSortFieldNamesDescendingList, entityId = entityId, schoolYearId = schoolYearId, flatten = flatten, returnResponse = returnResponse)
	}

	#' Get a DirectCertificationImport
	#'
	#' This function returns a dataframe or json object of a DirectCertificationImport
	#' @param DirectCertificationImportID The ID of the DirectCertificationImport to return.
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given DirectCertificationImport. Defaults to FALSE for all return fields which, for convenience, returns all fields for the DirectCertificationImport.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object listed as text. Run \code{\link{getSchemaForObjects}}('DirectCertificationImport') to get more field paths.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{listSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Food Service
	#' @return A dataframe or of DirectCertificationImport
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	getDirectCertificationImport <- function(DirectCertificationImportID, DistrictID = F, SchoolYearID = F, EligibilityCategoryIDFree = F, MediaID = F, EffectiveDate = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, EligibilityCategoryIDReduced = F, fieldPaths = NULL, entityId = 1, schoolYearId = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment()) %>% purrr::keep(names(.) != "DirectCertificationImportID")

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		getSkyObject(module = "FoodService", objectName = "DirectCertificationImport", objectId = DirectCertificationImportID, searchFields = searchFields, entityId = entityId, flatten = flatten, returnResponse = returnResponse)
	}

	#' Delete a DirectCertificationImport
	#'
	#' This function deletes a DirectCertificationImport
	#' @param DirectCertificationImportID The ID of the DirectCertificationImport to delete
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{listSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Food Service
	#' @return The DirectCertificationImportID of the deleted DirectCertificationImport.
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	deleteDirectCertificationImport <- function(DirectCertificationImportID, ignoreWarnings = F, entityId = 1, schoolYearId = NULL, flatten = T, returnResponse = F){

		deleteSkyObject(module = "FoodService", objectName = "DirectCertificationImport", objectId = DirectCertificationImportID, ignoreWarnings = ignoreWarnings, entityId = entityId, flatten = flatten, returnResponse = returnResponse)
	}

	#' Create a DirectCertificationImport
	#'
	#' This function creates a DirectCertificationImport
	#' @param fieldNames The field values to give the created DirectCertificationImport. Each defaults to NULL.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{listSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Food Service
	#' @return A newly created DirectCertificationImport
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	createDirectCertificationImport <- function(DistrictID = NULL, SchoolYearID = NULL, EligibilityCategoryIDFree = NULL, MediaID = NULL, EffectiveDate = NULL, EligibilityCategoryIDReduced = NULL, entityId = 1, schoolYearId = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment())

		body <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper()) %>% purrr::compact()

		createSkyObject(module = "FoodService", objectName = "DirectCertificationImport", body = list(DataObject = body), searchFields = append("DirectCertificationImportID", body %>% names()), entityId = entityId, schoolYearId = schoolYearId, flatten = flatten, returnResponse = returnResponse)
	}

	#' Modify a DirectCertificationImport
	#'
	#' This function modifies a DirectCertificationImport
	#' @param fieldNames The field values to give the modified DirectCertificationImport. Each defaults to NULL.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{listSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Food Service
	#' @return The modified DirectCertificationImport
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	modifyDirectCertificationImport <- function(DirectCertificationImportID, DistrictID = NULL, SchoolYearID = NULL, EligibilityCategoryIDFree = NULL, MediaID = NULL, EffectiveDate = NULL, EligibilityCategoryIDReduced = NULL, entityId = 1, schoolYearId = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment())

		body <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper()) %>% purrr::compact()

		modifySkyObject(module = "FoodService", objectName = "DirectCertificationImport", objectId = DirectCertificationImportID, body = list(DataObject = body), searchFields = append("DirectCertificationImportID", body %>% names()), entityId = entityId, schoolYearId = schoolYearId, flatten = flatten, returnResponse = returnResponse)
	}

	#' List ExcludeRandomSelectionReasons
	#'
	#' This function returns a dataframe or json object of ExcludeRandomSelectionReasons
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given ExcludeRandomSelectionReasons. Defaults to FALSE for all return fields which, for convenience, returns all fields for the ExcludeRandomSelectionReasons.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object listed as text. Run \code{\link{getSchemaForObjects}}('ExcludeRandomSelectionReason') to get more field paths.
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
	#' @concept Food Service
	#' @return A list of ExcludeRandomSelectionReasons
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	listExcludeRandomSelectionReasons <- function(searchConditionsList = NULL, ExcludeRandomSelectionReasonID = F, Code = F, Description = F, DistrictID = F, DistrictGroupKey = F, CodeDescription = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, fieldPaths = NULL, searchConditionsGroupType = "And", searchSortFieldNamesList = NULL, searchSortFieldNamesDescendingList = NULL, entityId = 1, schoolYearId = NULL, page = 1, pageSize = 100000, flatten = T, returnResponse = F){

		params <- as.list(environment())

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		listSkyObjects(module = "FoodService", objectName = "ExcludeRandomSelectionReason", searchFields = searchFields %>% append(fieldPaths), page = page, pageSize = pageSize, SearchConditionsList = searchConditionsList, SearchConditionsGroupType = searchConditionsGroupType, SearchSortFieldNamesList = searchSortFieldNamesList, SearchSortFieldNamesDescendingList = searchSortFieldNamesDescendingList, entityId = entityId, schoolYearId = schoolYearId, flatten = flatten, returnResponse = returnResponse)
	}

	#' Get an ExcludeRandomSelectionReason
	#'
	#' This function returns a dataframe or json object of an ExcludeRandomSelectionReason
	#' @param ExcludeRandomSelectionReasonID The ID of the ExcludeRandomSelectionReason to return.
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given ExcludeRandomSelectionReason. Defaults to FALSE for all return fields which, for convenience, returns all fields for the ExcludeRandomSelectionReason.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object listed as text. Run \code{\link{getSchemaForObjects}}('ExcludeRandomSelectionReason') to get more field paths.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{listSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Food Service
	#' @return A dataframe or of ExcludeRandomSelectionReason
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	getExcludeRandomSelectionReason <- function(ExcludeRandomSelectionReasonID, Code = F, Description = F, DistrictID = F, DistrictGroupKey = F, CodeDescription = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, fieldPaths = NULL, entityId = 1, schoolYearId = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment()) %>% purrr::keep(names(.) != "ExcludeRandomSelectionReasonID")

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		getSkyObject(module = "FoodService", objectName = "ExcludeRandomSelectionReason", objectId = ExcludeRandomSelectionReasonID, searchFields = searchFields, entityId = entityId, flatten = flatten, returnResponse = returnResponse)
	}

	#' Delete an ExcludeRandomSelectionReason
	#'
	#' This function deletes an ExcludeRandomSelectionReason
	#' @param ExcludeRandomSelectionReasonID The ID of the ExcludeRandomSelectionReason to delete
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{listSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Food Service
	#' @return The ExcludeRandomSelectionReasonID of the deleted ExcludeRandomSelectionReason.
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	deleteExcludeRandomSelectionReason <- function(ExcludeRandomSelectionReasonID, ignoreWarnings = F, entityId = 1, schoolYearId = NULL, flatten = T, returnResponse = F){

		deleteSkyObject(module = "FoodService", objectName = "ExcludeRandomSelectionReason", objectId = ExcludeRandomSelectionReasonID, ignoreWarnings = ignoreWarnings, entityId = entityId, flatten = flatten, returnResponse = returnResponse)
	}

	#' Create an ExcludeRandomSelectionReason
	#'
	#' This function creates an ExcludeRandomSelectionReason
	#' @param fieldNames The field values to give the created ExcludeRandomSelectionReason. Each defaults to NULL.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{listSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Food Service
	#' @return A newly created ExcludeRandomSelectionReason
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	createExcludeRandomSelectionReason <- function(Code = NULL, Description = NULL, DistrictID = NULL, DistrictGroupKey = NULL, entityId = 1, schoolYearId = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment())

		body <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper()) %>% purrr::compact()

		createSkyObject(module = "FoodService", objectName = "ExcludeRandomSelectionReason", body = list(DataObject = body), searchFields = append("ExcludeRandomSelectionReasonID", body %>% names()), entityId = entityId, schoolYearId = schoolYearId, flatten = flatten, returnResponse = returnResponse)
	}

	#' Modify an ExcludeRandomSelectionReason
	#'
	#' This function modifies an ExcludeRandomSelectionReason
	#' @param fieldNames The field values to give the modified ExcludeRandomSelectionReason. Each defaults to NULL.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{listSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Food Service
	#' @return The modified ExcludeRandomSelectionReason
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	modifyExcludeRandomSelectionReason <- function(ExcludeRandomSelectionReasonID, Code = NULL, Description = NULL, DistrictID = NULL, DistrictGroupKey = NULL, entityId = 1, schoolYearId = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment())

		body <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper()) %>% purrr::compact()

		modifySkyObject(module = "FoodService", objectName = "ExcludeRandomSelectionReason", objectId = ExcludeRandomSelectionReasonID, body = list(DataObject = body), searchFields = append("ExcludeRandomSelectionReasonID", body %>% names()), entityId = entityId, schoolYearId = schoolYearId, flatten = flatten, returnResponse = returnResponse)
	}

	#' List DenialReasons
	#'
	#' This function returns a dataframe or json object of DenialReasons
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given DenialReasons. Defaults to FALSE for all return fields which, for convenience, returns all fields for the DenialReasons.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object listed as text. Run \code{\link{getSchemaForObjects}}('DenialReason') to get more field paths.
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
	#' @concept Food Service
	#' @return A list of DenialReasons
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	listDenialReasons <- function(searchConditionsList = NULL, DenialReasonID = F, Code = F, Description = F, DistrictID = F, DistrictGroupKey = F, CodeDescription = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, fieldPaths = NULL, searchConditionsGroupType = "And", searchSortFieldNamesList = NULL, searchSortFieldNamesDescendingList = NULL, entityId = 1, schoolYearId = NULL, page = 1, pageSize = 100000, flatten = T, returnResponse = F){

		params <- as.list(environment())

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		listSkyObjects(module = "FoodService", objectName = "DenialReason", searchFields = searchFields %>% append(fieldPaths), page = page, pageSize = pageSize, SearchConditionsList = searchConditionsList, SearchConditionsGroupType = searchConditionsGroupType, SearchSortFieldNamesList = searchSortFieldNamesList, SearchSortFieldNamesDescendingList = searchSortFieldNamesDescendingList, entityId = entityId, schoolYearId = schoolYearId, flatten = flatten, returnResponse = returnResponse)
	}

	#' Get a DenialReason
	#'
	#' This function returns a dataframe or json object of a DenialReason
	#' @param DenialReasonID The ID of the DenialReason to return.
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given DenialReason. Defaults to FALSE for all return fields which, for convenience, returns all fields for the DenialReason.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object listed as text. Run \code{\link{getSchemaForObjects}}('DenialReason') to get more field paths.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{listSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Food Service
	#' @return A dataframe or of DenialReason
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	getDenialReason <- function(DenialReasonID, Code = F, Description = F, DistrictID = F, DistrictGroupKey = F, CodeDescription = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, fieldPaths = NULL, entityId = 1, schoolYearId = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment()) %>% purrr::keep(names(.) != "DenialReasonID")

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		getSkyObject(module = "FoodService", objectName = "DenialReason", objectId = DenialReasonID, searchFields = searchFields, entityId = entityId, flatten = flatten, returnResponse = returnResponse)
	}

	#' Delete a DenialReason
	#'
	#' This function deletes a DenialReason
	#' @param DenialReasonID The ID of the DenialReason to delete
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{listSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Food Service
	#' @return The DenialReasonID of the deleted DenialReason.
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	deleteDenialReason <- function(DenialReasonID, ignoreWarnings = F, entityId = 1, schoolYearId = NULL, flatten = T, returnResponse = F){

		deleteSkyObject(module = "FoodService", objectName = "DenialReason", objectId = DenialReasonID, ignoreWarnings = ignoreWarnings, entityId = entityId, flatten = flatten, returnResponse = returnResponse)
	}

	#' Create a DenialReason
	#'
	#' This function creates a DenialReason
	#' @param fieldNames The field values to give the created DenialReason. Each defaults to NULL.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{listSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Food Service
	#' @return A newly created DenialReason
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	createDenialReason <- function(Code = NULL, Description = NULL, DistrictID = NULL, DistrictGroupKey = NULL, entityId = 1, schoolYearId = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment())

		body <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper()) %>% purrr::compact()

		createSkyObject(module = "FoodService", objectName = "DenialReason", body = list(DataObject = body), searchFields = append("DenialReasonID", body %>% names()), entityId = entityId, schoolYearId = schoolYearId, flatten = flatten, returnResponse = returnResponse)
	}

	#' Modify a DenialReason
	#'
	#' This function modifies a DenialReason
	#' @param fieldNames The field values to give the modified DenialReason. Each defaults to NULL.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{listSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Food Service
	#' @return The modified DenialReason
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	modifyDenialReason <- function(DenialReasonID, Code = NULL, Description = NULL, DistrictID = NULL, DistrictGroupKey = NULL, entityId = 1, schoolYearId = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment())

		body <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper()) %>% purrr::compact()

		modifySkyObject(module = "FoodService", objectName = "DenialReason", objectId = DenialReasonID, body = list(DataObject = body), searchFields = append("DenialReasonID", body %>% names()), entityId = entityId, schoolYearId = schoolYearId, flatten = flatten, returnResponse = returnResponse)
	}

	#' List NextPINS
	#'
	#' This function returns a dataframe or json object of NextPINS
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given NextPINS. Defaults to FALSE for all return fields which, for convenience, returns all fields for the NextPINS.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object listed as text. Run \code{\link{getSchemaForObjects}}('NextPIN') to get more field paths.
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
	#' @concept Food Service
	#' @return A list of NextPINS
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	listNextPINS <- function(searchConditionsList = NULL, NextPINID = F, DistrictID = F, Length = F, SequenceNumber = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, fieldPaths = NULL, searchConditionsGroupType = "And", searchSortFieldNamesList = NULL, searchSortFieldNamesDescendingList = NULL, entityId = 1, schoolYearId = NULL, page = 1, pageSize = 100000, flatten = T, returnResponse = F){

		params <- as.list(environment())

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		listSkyObjects(module = "FoodService", objectName = "NextPIN", searchFields = searchFields %>% append(fieldPaths), page = page, pageSize = pageSize, SearchConditionsList = searchConditionsList, SearchConditionsGroupType = searchConditionsGroupType, SearchSortFieldNamesList = searchSortFieldNamesList, SearchSortFieldNamesDescendingList = searchSortFieldNamesDescendingList, entityId = entityId, schoolYearId = schoolYearId, flatten = flatten, returnResponse = returnResponse)
	}

	#' Get a NextPIN
	#'
	#' This function returns a dataframe or json object of a NextPIN
	#' @param NextPINID The ID of the NextPIN to return.
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given NextPIN. Defaults to FALSE for all return fields which, for convenience, returns all fields for the NextPIN.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object listed as text. Run \code{\link{getSchemaForObjects}}('NextPIN') to get more field paths.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{listSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Food Service
	#' @return A dataframe or of NextPIN
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	getNextPIN <- function(NextPINID, DistrictID = F, Length = F, SequenceNumber = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, fieldPaths = NULL, entityId = 1, schoolYearId = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment()) %>% purrr::keep(names(.) != "NextPINID")

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		getSkyObject(module = "FoodService", objectName = "NextPIN", objectId = NextPINID, searchFields = searchFields, entityId = entityId, flatten = flatten, returnResponse = returnResponse)
	}

	#' Delete a NextPIN
	#'
	#' This function deletes a NextPIN
	#' @param NextPINID The ID of the NextPIN to delete
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{listSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Food Service
	#' @return The NextPINID of the deleted NextPIN.
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	deleteNextPIN <- function(NextPINID, ignoreWarnings = F, entityId = 1, schoolYearId = NULL, flatten = T, returnResponse = F){

		deleteSkyObject(module = "FoodService", objectName = "NextPIN", objectId = NextPINID, ignoreWarnings = ignoreWarnings, entityId = entityId, flatten = flatten, returnResponse = returnResponse)
	}

	#' Create a NextPIN
	#'
	#' This function creates a NextPIN
	#' @param fieldNames The field values to give the created NextPIN. Each defaults to NULL.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{listSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Food Service
	#' @return A newly created NextPIN
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	createNextPIN <- function(DistrictID = NULL, Length = NULL, SequenceNumber = NULL, entityId = 1, schoolYearId = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment())

		body <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper()) %>% purrr::compact()

		createSkyObject(module = "FoodService", objectName = "NextPIN", body = list(DataObject = body), searchFields = append("NextPINID", body %>% names()), entityId = entityId, schoolYearId = schoolYearId, flatten = flatten, returnResponse = returnResponse)
	}

	#' Modify a NextPIN
	#'
	#' This function modifies a NextPIN
	#' @param fieldNames The field values to give the modified NextPIN. Each defaults to NULL.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{listSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Food Service
	#' @return The modified NextPIN
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	modifyNextPIN <- function(NextPINID, DistrictID = NULL, Length = NULL, SequenceNumber = NULL, entityId = 1, schoolYearId = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment())

		body <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper()) %>% purrr::compact()

		modifySkyObject(module = "FoodService", objectName = "NextPIN", objectId = NextPINID, body = list(DataObject = body), searchFields = append("NextPINID", body %>% names()), entityId = entityId, schoolYearId = schoolYearId, flatten = flatten, returnResponse = returnResponse)
	}

	#' List FoodServicePayments
	#'
	#' This function returns a dataframe or json object of FoodServicePayments
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given FoodServicePayments. Defaults to FALSE for all return fields which, for convenience, returns all fields for the FoodServicePayments.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object listed as text. Run \code{\link{getSchemaForObjects}}('FoodServicePayment') to get more field paths.
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
	#' @concept Food Service
	#' @return A list of FoodServicePayments
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	listFoodServicePayments <- function(searchConditionsList = NULL, PaymentID = F, PayorID = F, LineID = F, PaymentTime = F, TenderType = F, Type = F, CheckNumber = F, BaseCurrencyAmount = F, Note = F, ReceiptNumber = F, Status = F, PaymentIDOriginal = F, AccountingUpdateID = F, PaymentAmountWithAdjustments = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, NameIDPaidBy = F, ReconciledAfterSelectedBankRec = F, ReadOnlyOnBankRec = F, LineCloseoutID = F, fieldPaths = NULL, searchConditionsGroupType = "And", searchSortFieldNamesList = NULL, searchSortFieldNamesDescendingList = NULL, entityId = 1, schoolYearId = NULL, page = 1, pageSize = 100000, flatten = T, returnResponse = F){

		params <- as.list(environment())

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		listSkyObjects(module = "FoodService", objectName = "Payment", searchFields = searchFields %>% append(fieldPaths), page = page, pageSize = pageSize, SearchConditionsList = searchConditionsList, SearchConditionsGroupType = searchConditionsGroupType, SearchSortFieldNamesList = searchSortFieldNamesList, SearchSortFieldNamesDescendingList = searchSortFieldNamesDescendingList, entityId = entityId, schoolYearId = schoolYearId, flatten = flatten, returnResponse = returnResponse)
	}

	#' Get a FoodServicePayment
	#'
	#' This function returns a dataframe or json object of a FoodServicePayment
	#' @param FoodServicePaymentID The ID of the FoodServicePayment to return.
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given FoodServicePayment. Defaults to FALSE for all return fields which, for convenience, returns all fields for the FoodServicePayment.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object listed as text. Run \code{\link{getSchemaForObjects}}('FoodServicePayment') to get more field paths.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{listSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Food Service
	#' @return A dataframe or of FoodServicePayment
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	getFoodServicePayment <- function(FoodServicePaymentID, PaymentID = F, PayorID = F, LineID = F, PaymentTime = F, TenderType = F, Type = F, CheckNumber = F, BaseCurrencyAmount = F, Note = F, ReceiptNumber = F, Status = F, PaymentIDOriginal = F, AccountingUpdateID = F, PaymentAmountWithAdjustments = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, NameIDPaidBy = F, ReconciledAfterSelectedBankRec = F, ReadOnlyOnBankRec = F, LineCloseoutID = F, fieldPaths = NULL, entityId = 1, schoolYearId = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment()) %>% purrr::keep(names(.) != "FoodServicePaymentID")

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		getSkyObject(module = "FoodService", objectName = "Payment", objectId = FoodServicePaymentID, searchFields = searchFields, entityId = entityId, flatten = flatten, returnResponse = returnResponse)
	}

	#' Delete a FoodServicePayment
	#'
	#' This function deletes a FoodServicePayment
	#' @param FoodServicePaymentID The ID of the FoodServicePayment to delete
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{listSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Food Service
	#' @return The FoodServicePaymentID of the deleted FoodServicePayment.
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	deleteFoodServicePayment <- function(FoodServicePaymentID, ignoreWarnings = F, entityId = 1, schoolYearId = NULL, flatten = T, returnResponse = F){

		deleteSkyObject(module = "FoodService", objectName = "Payment", objectId = FoodServicePaymentID, ignoreWarnings = ignoreWarnings, entityId = entityId, flatten = flatten, returnResponse = returnResponse)
	}

	#' Create a FoodServicePayment
	#'
	#' This function creates a FoodServicePayment
	#' @param fieldNames The field values to give the created FoodServicePayment. Each defaults to NULL.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{listSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Food Service
	#' @return A newly created FoodServicePayment
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	createFoodServicePayment <- function(PayorID = NULL, LineID = NULL, PaymentTime = NULL, TenderType = NULL, Type = NULL, CheckNumber = NULL, BaseCurrencyAmount = NULL, Note = NULL, ReceiptNumber = NULL, Status = NULL, PaymentIDOriginal = NULL, AccountingUpdateID = NULL, NameIDPaidBy = NULL, LineCloseoutID = NULL, entityId = 1, schoolYearId = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment())

		body <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper()) %>% purrr::compact()

		createSkyObject(module = "FoodService", objectName = "Payment", body = list(DataObject = body), searchFields = append("PaymentID", body %>% names()), entityId = entityId, schoolYearId = schoolYearId, flatten = flatten, returnResponse = returnResponse)
	}

	#' Modify a FoodServicePayment
	#'
	#' This function modifies a FoodServicePayment
	#' @param fieldNames The field values to give the modified FoodServicePayment. Each defaults to NULL.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{listSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Food Service
	#' @return The modified FoodServicePayment
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	modifyFoodServicePayment <- function(PaymentID, PayorID = NULL, LineID = NULL, PaymentTime = NULL, TenderType = NULL, Type = NULL, CheckNumber = NULL, BaseCurrencyAmount = NULL, Note = NULL, ReceiptNumber = NULL, Status = NULL, PaymentIDOriginal = NULL, AccountingUpdateID = NULL, NameIDPaidBy = NULL, LineCloseoutID = NULL, entityId = 1, schoolYearId = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment())

		body <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper()) %>% purrr::compact()

		modifySkyObject(module = "FoodService", objectName = "Payment", objectId = PaymentID, body = list(DataObject = body), searchFields = append("PaymentID", body %>% names()), entityId = entityId, schoolYearId = schoolYearId, flatten = flatten, returnResponse = returnResponse)
	}

	#' List IncomeEligibilities
	#'
	#' This function returns a dataframe or json object of IncomeEligibilities
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given IncomeEligibilities. Defaults to FALSE for all return fields which, for convenience, returns all fields for the IncomeEligibilities.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object listed as text. Run \code{\link{getSchemaForObjects}}('IncomeEligibility') to get more field paths.
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
	#' @concept Food Service
	#' @return A list of IncomeEligibilities
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	listIncomeEligibilities <- function(searchConditionsList = NULL, IncomeEligibilityID = F, SkywardID = F, StartDate = F, EndDate = F, EachAdditionalFreeAnnualAmount = F, EachAdditionalReducedAnnualAmount = F, EachAdditionalFreeMonthlyAmount = F, EachAdditionalReducedMonthlyAmount = F, EachAdditionalFreeTwicePerMonthAmount = F, EachAdditionalReducedTwicePerMonthAmount = F, EachAdditionalFreeEveryTwoWeeksAmount = F, EachAdditionalReducedEveryTwoWeeksAmount = F, EachAdditionalFreeWeeklyAmount = F, EachAdditionalReducedWeeklyAmount = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, MaxLoadedFamilySize = F, SkywardHash = F, fieldPaths = NULL, searchConditionsGroupType = "And", searchSortFieldNamesList = NULL, searchSortFieldNamesDescendingList = NULL, entityId = 1, schoolYearId = NULL, page = 1, pageSize = 100000, flatten = T, returnResponse = F){

		params <- as.list(environment())

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		listSkyObjects(module = "FoodService", objectName = "IncomeEligibility", searchFields = searchFields %>% append(fieldPaths), page = page, pageSize = pageSize, SearchConditionsList = searchConditionsList, SearchConditionsGroupType = searchConditionsGroupType, SearchSortFieldNamesList = searchSortFieldNamesList, SearchSortFieldNamesDescendingList = searchSortFieldNamesDescendingList, entityId = entityId, schoolYearId = schoolYearId, flatten = flatten, returnResponse = returnResponse)
	}

	#' Get an IncomeEligibility
	#'
	#' This function returns a dataframe or json object of an IncomeEligibility
	#' @param IncomeEligibilityID The ID of the IncomeEligibility to return.
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given IncomeEligibility. Defaults to FALSE for all return fields which, for convenience, returns all fields for the IncomeEligibility.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object listed as text. Run \code{\link{getSchemaForObjects}}('IncomeEligibility') to get more field paths.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{listSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Food Service
	#' @return A dataframe or of IncomeEligibility
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	getIncomeEligibility <- function(IncomeEligibilityID, SkywardID = F, StartDate = F, EndDate = F, EachAdditionalFreeAnnualAmount = F, EachAdditionalReducedAnnualAmount = F, EachAdditionalFreeMonthlyAmount = F, EachAdditionalReducedMonthlyAmount = F, EachAdditionalFreeTwicePerMonthAmount = F, EachAdditionalReducedTwicePerMonthAmount = F, EachAdditionalFreeEveryTwoWeeksAmount = F, EachAdditionalReducedEveryTwoWeeksAmount = F, EachAdditionalFreeWeeklyAmount = F, EachAdditionalReducedWeeklyAmount = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, MaxLoadedFamilySize = F, SkywardHash = F, fieldPaths = NULL, entityId = 1, schoolYearId = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment()) %>% purrr::keep(names(.) != "IncomeEligibilityID")

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		getSkyObject(module = "FoodService", objectName = "IncomeEligibility", objectId = IncomeEligibilityID, searchFields = searchFields, entityId = entityId, flatten = flatten, returnResponse = returnResponse)
	}

	#' Delete an IncomeEligibility
	#'
	#' This function deletes an IncomeEligibility
	#' @param IncomeEligibilityID The ID of the IncomeEligibility to delete
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{listSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Food Service
	#' @return The IncomeEligibilityID of the deleted IncomeEligibility.
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	deleteIncomeEligibility <- function(IncomeEligibilityID, ignoreWarnings = F, entityId = 1, schoolYearId = NULL, flatten = T, returnResponse = F){

		deleteSkyObject(module = "FoodService", objectName = "IncomeEligibility", objectId = IncomeEligibilityID, ignoreWarnings = ignoreWarnings, entityId = entityId, flatten = flatten, returnResponse = returnResponse)
	}

	#' Create an IncomeEligibility
	#'
	#' This function creates an IncomeEligibility
	#' @param fieldNames The field values to give the created IncomeEligibility. Each defaults to NULL.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{listSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Food Service
	#' @return A newly created IncomeEligibility
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	createIncomeEligibility <- function(StartDate = NULL, EndDate = NULL, EachAdditionalFreeAnnualAmount = NULL, EachAdditionalReducedAnnualAmount = NULL, EachAdditionalFreeMonthlyAmount = NULL, EachAdditionalReducedMonthlyAmount = NULL, EachAdditionalFreeTwicePerMonthAmount = NULL, EachAdditionalReducedTwicePerMonthAmount = NULL, EachAdditionalFreeEveryTwoWeeksAmount = NULL, EachAdditionalReducedEveryTwoWeeksAmount = NULL, EachAdditionalFreeWeeklyAmount = NULL, EachAdditionalReducedWeeklyAmount = NULL, entityId = 1, schoolYearId = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment())

		body <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper()) %>% purrr::compact()

		createSkyObject(module = "FoodService", objectName = "IncomeEligibility", body = list(DataObject = body), searchFields = append("IncomeEligibilityID", body %>% names()), entityId = entityId, schoolYearId = schoolYearId, flatten = flatten, returnResponse = returnResponse)
	}

	#' Modify an IncomeEligibility
	#'
	#' This function modifies an IncomeEligibility
	#' @param fieldNames The field values to give the modified IncomeEligibility. Each defaults to NULL.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{listSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Food Service
	#' @return The modified IncomeEligibility
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	modifyIncomeEligibility <- function(IncomeEligibilityID, StartDate = NULL, EndDate = NULL, EachAdditionalFreeAnnualAmount = NULL, EachAdditionalReducedAnnualAmount = NULL, EachAdditionalFreeMonthlyAmount = NULL, EachAdditionalReducedMonthlyAmount = NULL, EachAdditionalFreeTwicePerMonthAmount = NULL, EachAdditionalReducedTwicePerMonthAmount = NULL, EachAdditionalFreeEveryTwoWeeksAmount = NULL, EachAdditionalReducedEveryTwoWeeksAmount = NULL, EachAdditionalFreeWeeklyAmount = NULL, EachAdditionalReducedWeeklyAmount = NULL, entityId = 1, schoolYearId = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment())

		body <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper()) %>% purrr::compact()

		modifySkyObject(module = "FoodService", objectName = "IncomeEligibility", objectId = IncomeEligibilityID, body = list(DataObject = body), searchFields = append("IncomeEligibilityID", body %>% names()), entityId = entityId, schoolYearId = schoolYearId, flatten = flatten, returnResponse = returnResponse)
	}

	#' List IncomeEligibilityAmounts
	#'
	#' This function returns a dataframe or json object of IncomeEligibilityAmounts
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given IncomeEligibilityAmounts. Defaults to FALSE for all return fields which, for convenience, returns all fields for the IncomeEligibilityAmounts.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object listed as text. Run \code{\link{getSchemaForObjects}}('IncomeEligibilityAmount') to get more field paths.
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
	#' @concept Food Service
	#' @return A list of IncomeEligibilityAmounts
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	listIncomeEligibilityAmounts <- function(searchConditionsList = NULL, IncomeEligibilityAmountID = F, IncomeEligibilityID = F, SkywardID = F, FamilySize = F, EligibilityCategoryType = F, TimePeriodType = F, Amount = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, SkywardHash = F, fieldPaths = NULL, searchConditionsGroupType = "And", searchSortFieldNamesList = NULL, searchSortFieldNamesDescendingList = NULL, entityId = 1, schoolYearId = NULL, page = 1, pageSize = 100000, flatten = T, returnResponse = F){

		params <- as.list(environment())

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		listSkyObjects(module = "FoodService", objectName = "IncomeEligibilityAmount", searchFields = searchFields %>% append(fieldPaths), page = page, pageSize = pageSize, SearchConditionsList = searchConditionsList, SearchConditionsGroupType = searchConditionsGroupType, SearchSortFieldNamesList = searchSortFieldNamesList, SearchSortFieldNamesDescendingList = searchSortFieldNamesDescendingList, entityId = entityId, schoolYearId = schoolYearId, flatten = flatten, returnResponse = returnResponse)
	}

	#' Get an IncomeEligibilityAmount
	#'
	#' This function returns a dataframe or json object of an IncomeEligibilityAmount
	#' @param IncomeEligibilityAmountID The ID of the IncomeEligibilityAmount to return.
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given IncomeEligibilityAmount. Defaults to FALSE for all return fields which, for convenience, returns all fields for the IncomeEligibilityAmount.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object listed as text. Run \code{\link{getSchemaForObjects}}('IncomeEligibilityAmount') to get more field paths.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{listSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Food Service
	#' @return A dataframe or of IncomeEligibilityAmount
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	getIncomeEligibilityAmount <- function(IncomeEligibilityAmountID, IncomeEligibilityID = F, SkywardID = F, FamilySize = F, EligibilityCategoryType = F, TimePeriodType = F, Amount = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, SkywardHash = F, fieldPaths = NULL, entityId = 1, schoolYearId = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment()) %>% purrr::keep(names(.) != "IncomeEligibilityAmountID")

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		getSkyObject(module = "FoodService", objectName = "IncomeEligibilityAmount", objectId = IncomeEligibilityAmountID, searchFields = searchFields, entityId = entityId, flatten = flatten, returnResponse = returnResponse)
	}

	#' Delete an IncomeEligibilityAmount
	#'
	#' This function deletes an IncomeEligibilityAmount
	#' @param IncomeEligibilityAmountID The ID of the IncomeEligibilityAmount to delete
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{listSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Food Service
	#' @return The IncomeEligibilityAmountID of the deleted IncomeEligibilityAmount.
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	deleteIncomeEligibilityAmount <- function(IncomeEligibilityAmountID, ignoreWarnings = F, entityId = 1, schoolYearId = NULL, flatten = T, returnResponse = F){

		deleteSkyObject(module = "FoodService", objectName = "IncomeEligibilityAmount", objectId = IncomeEligibilityAmountID, ignoreWarnings = ignoreWarnings, entityId = entityId, flatten = flatten, returnResponse = returnResponse)
	}

	#' Create an IncomeEligibilityAmount
	#'
	#' This function creates an IncomeEligibilityAmount
	#' @param fieldNames The field values to give the created IncomeEligibilityAmount. Each defaults to NULL.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{listSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Food Service
	#' @return A newly created IncomeEligibilityAmount
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	createIncomeEligibilityAmount <- function(IncomeEligibilityID = NULL, FamilySize = NULL, EligibilityCategoryType = NULL, TimePeriodType = NULL, Amount = NULL, entityId = 1, schoolYearId = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment())

		body <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper()) %>% purrr::compact()

		createSkyObject(module = "FoodService", objectName = "IncomeEligibilityAmount", body = list(DataObject = body), searchFields = append("IncomeEligibilityAmountID", body %>% names()), entityId = entityId, schoolYearId = schoolYearId, flatten = flatten, returnResponse = returnResponse)
	}

	#' Modify an IncomeEligibilityAmount
	#'
	#' This function modifies an IncomeEligibilityAmount
	#' @param fieldNames The field values to give the modified IncomeEligibilityAmount. Each defaults to NULL.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{listSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Food Service
	#' @return The modified IncomeEligibilityAmount
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	modifyIncomeEligibilityAmount <- function(IncomeEligibilityAmountID, IncomeEligibilityID = NULL, FamilySize = NULL, EligibilityCategoryType = NULL, TimePeriodType = NULL, Amount = NULL, entityId = 1, schoolYearId = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment())

		body <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper()) %>% purrr::compact()

		modifySkyObject(module = "FoodService", objectName = "IncomeEligibilityAmount", objectId = IncomeEligibilityAmountID, body = list(DataObject = body), searchFields = append("IncomeEligibilityAmountID", body %>% names()), entityId = entityId, schoolYearId = schoolYearId, flatten = flatten, returnResponse = returnResponse)
	}

	#' List CustomerAlertMessages
	#'
	#' This function returns a dataframe or json object of CustomerAlertMessages
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given CustomerAlertMessages. Defaults to FALSE for all return fields which, for convenience, returns all fields for the CustomerAlertMessages.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object listed as text. Run \code{\link{getSchemaForObjects}}('CustomerAlertMessage') to get more field paths.
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
	#' @concept Food Service
	#' @return A list of CustomerAlertMessages
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	listCustomerAlertMessages <- function(searchConditionsList = NULL, CustomerAlertMessageID = F, CustomerID = F, AlertMessageID = F, IsCritical = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, fieldPaths = NULL, searchConditionsGroupType = "And", searchSortFieldNamesList = NULL, searchSortFieldNamesDescendingList = NULL, entityId = 1, schoolYearId = NULL, page = 1, pageSize = 100000, flatten = T, returnResponse = F){

		params <- as.list(environment())

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		listSkyObjects(module = "FoodService", objectName = "CustomerAlertMessage", searchFields = searchFields %>% append(fieldPaths), page = page, pageSize = pageSize, SearchConditionsList = searchConditionsList, SearchConditionsGroupType = searchConditionsGroupType, SearchSortFieldNamesList = searchSortFieldNamesList, SearchSortFieldNamesDescendingList = searchSortFieldNamesDescendingList, entityId = entityId, schoolYearId = schoolYearId, flatten = flatten, returnResponse = returnResponse)
	}

	#' Get a CustomerAlertMessage
	#'
	#' This function returns a dataframe or json object of a CustomerAlertMessage
	#' @param CustomerAlertMessageID The ID of the CustomerAlertMessage to return.
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given CustomerAlertMessage. Defaults to FALSE for all return fields which, for convenience, returns all fields for the CustomerAlertMessage.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object listed as text. Run \code{\link{getSchemaForObjects}}('CustomerAlertMessage') to get more field paths.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{listSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Food Service
	#' @return A dataframe or of CustomerAlertMessage
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	getCustomerAlertMessage <- function(CustomerAlertMessageID, CustomerID = F, AlertMessageID = F, IsCritical = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, fieldPaths = NULL, entityId = 1, schoolYearId = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment()) %>% purrr::keep(names(.) != "CustomerAlertMessageID")

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		getSkyObject(module = "FoodService", objectName = "CustomerAlertMessage", objectId = CustomerAlertMessageID, searchFields = searchFields, entityId = entityId, flatten = flatten, returnResponse = returnResponse)
	}

	#' Delete a CustomerAlertMessage
	#'
	#' This function deletes a CustomerAlertMessage
	#' @param CustomerAlertMessageID The ID of the CustomerAlertMessage to delete
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{listSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Food Service
	#' @return The CustomerAlertMessageID of the deleted CustomerAlertMessage.
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	deleteCustomerAlertMessage <- function(CustomerAlertMessageID, ignoreWarnings = F, entityId = 1, schoolYearId = NULL, flatten = T, returnResponse = F){

		deleteSkyObject(module = "FoodService", objectName = "CustomerAlertMessage", objectId = CustomerAlertMessageID, ignoreWarnings = ignoreWarnings, entityId = entityId, flatten = flatten, returnResponse = returnResponse)
	}

	#' Create a CustomerAlertMessage
	#'
	#' This function creates a CustomerAlertMessage
	#' @param fieldNames The field values to give the created CustomerAlertMessage. Each defaults to NULL.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{listSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Food Service
	#' @return A newly created CustomerAlertMessage
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	createCustomerAlertMessage <- function(CustomerID = NULL, AlertMessageID = NULL, IsCritical = NULL, entityId = 1, schoolYearId = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment())

		body <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper()) %>% purrr::compact()

		createSkyObject(module = "FoodService", objectName = "CustomerAlertMessage", body = list(DataObject = body), searchFields = append("CustomerAlertMessageID", body %>% names()), entityId = entityId, schoolYearId = schoolYearId, flatten = flatten, returnResponse = returnResponse)
	}

	#' Modify a CustomerAlertMessage
	#'
	#' This function modifies a CustomerAlertMessage
	#' @param fieldNames The field values to give the modified CustomerAlertMessage. Each defaults to NULL.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{listSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Food Service
	#' @return The modified CustomerAlertMessage
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	modifyCustomerAlertMessage <- function(CustomerAlertMessageID, CustomerID = NULL, AlertMessageID = NULL, IsCritical = NULL, entityId = 1, schoolYearId = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment())

		body <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper()) %>% purrr::compact()

		modifySkyObject(module = "FoodService", objectName = "CustomerAlertMessage", objectId = CustomerAlertMessageID, body = list(DataObject = body), searchFields = append("CustomerAlertMessageID", body %>% names()), entityId = entityId, schoolYearId = schoolYearId, flatten = flatten, returnResponse = returnResponse)
	}

	#' List CustomerCategories
	#'
	#' This function returns a dataframe or json object of CustomerCategories
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given CustomerCategories. Defaults to FALSE for all return fields which, for convenience, returns all fields for the CustomerCategories.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object listed as text. Run \code{\link{getSchemaForObjects}}('CustomerCategory') to get more field paths.
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
	#' @concept Food Service
	#' @return A list of CustomerCategories
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	listCustomerCategories <- function(searchConditionsList = NULL, CustomerCategoryID = F, CustomerID = F, EligibilityCategoryID = F, PriceCategoryID = F, Note = F, EffectiveTime = F, EffectiveDate = F, IsFromDirectCertification = F, DirectCertificationID = F, IsFromApplicationMember = F, ApplicationMemberID = F, IsManual = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, IsMostRecentForVerificationCollectionReport = F, IsCurrentCustomerCategory = F, fieldPaths = NULL, searchConditionsGroupType = "And", searchSortFieldNamesList = NULL, searchSortFieldNamesDescendingList = NULL, entityId = 1, schoolYearId = NULL, page = 1, pageSize = 100000, flatten = T, returnResponse = F){

		params <- as.list(environment())

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		listSkyObjects(module = "FoodService", objectName = "CustomerCategory", searchFields = searchFields %>% append(fieldPaths), page = page, pageSize = pageSize, SearchConditionsList = searchConditionsList, SearchConditionsGroupType = searchConditionsGroupType, SearchSortFieldNamesList = searchSortFieldNamesList, SearchSortFieldNamesDescendingList = searchSortFieldNamesDescendingList, entityId = entityId, schoolYearId = schoolYearId, flatten = flatten, returnResponse = returnResponse)
	}

	#' Get a CustomerCategory
	#'
	#' This function returns a dataframe or json object of a CustomerCategory
	#' @param CustomerCategoryID The ID of the CustomerCategory to return.
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given CustomerCategory. Defaults to FALSE for all return fields which, for convenience, returns all fields for the CustomerCategory.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object listed as text. Run \code{\link{getSchemaForObjects}}('CustomerCategory') to get more field paths.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{listSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Food Service
	#' @return A dataframe or of CustomerCategory
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	getCustomerCategory <- function(CustomerCategoryID, CustomerID = F, EligibilityCategoryID = F, PriceCategoryID = F, Note = F, EffectiveTime = F, EffectiveDate = F, IsFromDirectCertification = F, DirectCertificationID = F, IsFromApplicationMember = F, ApplicationMemberID = F, IsManual = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, IsMostRecentForVerificationCollectionReport = F, IsCurrentCustomerCategory = F, fieldPaths = NULL, entityId = 1, schoolYearId = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment()) %>% purrr::keep(names(.) != "CustomerCategoryID")

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		getSkyObject(module = "FoodService", objectName = "CustomerCategory", objectId = CustomerCategoryID, searchFields = searchFields, entityId = entityId, flatten = flatten, returnResponse = returnResponse)
	}

	#' Delete a CustomerCategory
	#'
	#' This function deletes a CustomerCategory
	#' @param CustomerCategoryID The ID of the CustomerCategory to delete
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{listSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Food Service
	#' @return The CustomerCategoryID of the deleted CustomerCategory.
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	deleteCustomerCategory <- function(CustomerCategoryID, ignoreWarnings = F, entityId = 1, schoolYearId = NULL, flatten = T, returnResponse = F){

		deleteSkyObject(module = "FoodService", objectName = "CustomerCategory", objectId = CustomerCategoryID, ignoreWarnings = ignoreWarnings, entityId = entityId, flatten = flatten, returnResponse = returnResponse)
	}

	#' Create a CustomerCategory
	#'
	#' This function creates a CustomerCategory
	#' @param fieldNames The field values to give the created CustomerCategory. Each defaults to NULL.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{listSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Food Service
	#' @return A newly created CustomerCategory
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	createCustomerCategory <- function(CustomerID = NULL, EligibilityCategoryID = NULL, PriceCategoryID = NULL, Note = NULL, EffectiveTime = NULL, DirectCertificationID = NULL, ApplicationMemberID = NULL, entityId = 1, schoolYearId = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment())

		body <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper()) %>% purrr::compact()

		createSkyObject(module = "FoodService", objectName = "CustomerCategory", body = list(DataObject = body), searchFields = append("CustomerCategoryID", body %>% names()), entityId = entityId, schoolYearId = schoolYearId, flatten = flatten, returnResponse = returnResponse)
	}

	#' Modify a CustomerCategory
	#'
	#' This function modifies a CustomerCategory
	#' @param fieldNames The field values to give the modified CustomerCategory. Each defaults to NULL.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{listSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Food Service
	#' @return The modified CustomerCategory
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	modifyCustomerCategory <- function(CustomerCategoryID, CustomerID = NULL, EligibilityCategoryID = NULL, PriceCategoryID = NULL, Note = NULL, EffectiveTime = NULL, DirectCertificationID = NULL, ApplicationMemberID = NULL, entityId = 1, schoolYearId = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment())

		body <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper()) %>% purrr::compact()

		modifySkyObject(module = "FoodService", objectName = "CustomerCategory", objectId = CustomerCategoryID, body = list(DataObject = body), searchFields = append("CustomerCategoryID", body %>% names()), entityId = entityId, schoolYearId = schoolYearId, flatten = flatten, returnResponse = returnResponse)
	}

	#' List FoodServiceCustomers
	#'
	#' This function returns a dataframe or json object of FoodServiceCustomers
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given FoodServiceCustomers. Defaults to FALSE for all return fields which, for convenience, returns all fields for the FoodServiceCustomers.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object listed as text. Run \code{\link{getSchemaForObjects}}('FoodServiceCustomer') to get more field paths.
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
	#' @concept Food Service
	#' @return A list of FoodServiceCustomers
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	listFoodServiceCustomers <- function(searchConditionsList = NULL, CustomerID = F, NameID = F, DistrictID = F, IsCurrentActive = F, PIN = F, StopAllPurchases = F, PINLength = F, PINSequenceNumber = F, CurrentBalance = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, AsOfBalanceDate = F, BalanceAsOfDate = F, IsFutureActive = F, IsCurrentOrFutureActive = F, fieldPaths = NULL, searchConditionsGroupType = "And", searchSortFieldNamesList = NULL, searchSortFieldNamesDescendingList = NULL, entityId = 1, schoolYearId = NULL, page = 1, pageSize = 100000, flatten = T, returnResponse = F){

		params <- as.list(environment())

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		listSkyObjects(module = "FoodService", objectName = "Customer", searchFields = searchFields %>% append(fieldPaths), page = page, pageSize = pageSize, SearchConditionsList = searchConditionsList, SearchConditionsGroupType = searchConditionsGroupType, SearchSortFieldNamesList = searchSortFieldNamesList, SearchSortFieldNamesDescendingList = searchSortFieldNamesDescendingList, entityId = entityId, schoolYearId = schoolYearId, flatten = flatten, returnResponse = returnResponse)
	}

	#' Get a FoodServiceCustomer
	#'
	#' This function returns a dataframe or json object of a FoodServiceCustomer
	#' @param FoodServiceCustomerID The ID of the FoodServiceCustomer to return.
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given FoodServiceCustomer. Defaults to FALSE for all return fields which, for convenience, returns all fields for the FoodServiceCustomer.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object listed as text. Run \code{\link{getSchemaForObjects}}('FoodServiceCustomer') to get more field paths.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{listSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Food Service
	#' @return A dataframe or of FoodServiceCustomer
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	getFoodServiceCustomer <- function(FoodServiceCustomerID, CustomerID = F, NameID = F, DistrictID = F, IsCurrentActive = F, PIN = F, StopAllPurchases = F, PINLength = F, PINSequenceNumber = F, CurrentBalance = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, AsOfBalanceDate = F, BalanceAsOfDate = F, IsFutureActive = F, IsCurrentOrFutureActive = F, fieldPaths = NULL, entityId = 1, schoolYearId = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment()) %>% purrr::keep(names(.) != "FoodServiceCustomerID")

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		getSkyObject(module = "FoodService", objectName = "Customer", objectId = FoodServiceCustomerID, searchFields = searchFields, entityId = entityId, flatten = flatten, returnResponse = returnResponse)
	}

	#' Delete a FoodServiceCustomer
	#'
	#' This function deletes a FoodServiceCustomer
	#' @param FoodServiceCustomerID The ID of the FoodServiceCustomer to delete
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{listSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Food Service
	#' @return The FoodServiceCustomerID of the deleted FoodServiceCustomer.
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	deleteFoodServiceCustomer <- function(FoodServiceCustomerID, ignoreWarnings = F, entityId = 1, schoolYearId = NULL, flatten = T, returnResponse = F){

		deleteSkyObject(module = "FoodService", objectName = "Customer", objectId = FoodServiceCustomerID, ignoreWarnings = ignoreWarnings, entityId = entityId, flatten = flatten, returnResponse = returnResponse)
	}

	#' Create a FoodServiceCustomer
	#'
	#' This function creates a FoodServiceCustomer
	#' @param fieldNames The field values to give the created FoodServiceCustomer. Each defaults to NULL.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{listSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Food Service
	#' @return A newly created FoodServiceCustomer
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	createFoodServiceCustomer <- function(NameID = NULL, DistrictID = NULL, IsCurrentActive = NULL, PIN = NULL, StopAllPurchases = NULL, AsOfBalanceDate = NULL, BalanceAsOfDate = NULL, IsFutureActive = NULL, entityId = 1, schoolYearId = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment())

		body <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper()) %>% purrr::compact()

		createSkyObject(module = "FoodService", objectName = "Customer", body = list(DataObject = body), searchFields = append("CustomerID", body %>% names()), entityId = entityId, schoolYearId = schoolYearId, flatten = flatten, returnResponse = returnResponse)
	}

	#' Modify a FoodServiceCustomer
	#'
	#' This function modifies a FoodServiceCustomer
	#' @param fieldNames The field values to give the modified FoodServiceCustomer. Each defaults to NULL.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{listSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Food Service
	#' @return The modified FoodServiceCustomer
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	modifyFoodServiceCustomer <- function(CustomerID, NameID = NULL, DistrictID = NULL, IsCurrentActive = NULL, PIN = NULL, StopAllPurchases = NULL, AsOfBalanceDate = NULL, BalanceAsOfDate = NULL, IsFutureActive = NULL, entityId = 1, schoolYearId = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment())

		body <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper()) %>% purrr::compact()

		modifySkyObject(module = "FoodService", objectName = "Customer", objectId = CustomerID, body = list(DataObject = body), searchFields = append("CustomerID", body %>% names()), entityId = entityId, schoolYearId = schoolYearId, flatten = flatten, returnResponse = returnResponse)
	}

	#' List Lines
	#'
	#' This function returns a dataframe or json object of Lines
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given Lines. Defaults to FALSE for all return fields which, for convenience, returns all fields for the Lines.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object listed as text. Run \code{\link{getSchemaForObjects}}('Line') to get more field paths.
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
	#' @concept Food Service
	#' @return A list of Lines
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	listLines <- function(searchConditionsList = NULL, LineID = F, Code = F, Description = F, CodeDescription = F, EntityID = F, ShowProfilePicture = F, ShowItemPicture = F, AcceptPayment = F, UseLowBalanceAlert = F, LowBalanceAlertThreshold = F, DefaultDisplayAmountReceived = F, DefaultDisplayAmountApplied = F, ShowGovernmentFundedPrices = F, QueuingEnabled = F, Host = F, MaskID = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, OnlyShowCustomersEnrolledInEntity = F, AllowLineWorkersToEditPurchase = F, AllowLineWorkersToDisableStandardItem = F, AllowChangeSaleDate = F, fieldPaths = NULL, searchConditionsGroupType = "And", searchSortFieldNamesList = NULL, searchSortFieldNamesDescendingList = NULL, entityId = 1, schoolYearId = NULL, page = 1, pageSize = 100000, flatten = T, returnResponse = F){

		params <- as.list(environment())

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		listSkyObjects(module = "FoodService", objectName = "Line", searchFields = searchFields %>% append(fieldPaths), page = page, pageSize = pageSize, SearchConditionsList = searchConditionsList, SearchConditionsGroupType = searchConditionsGroupType, SearchSortFieldNamesList = searchSortFieldNamesList, SearchSortFieldNamesDescendingList = searchSortFieldNamesDescendingList, entityId = entityId, schoolYearId = schoolYearId, flatten = flatten, returnResponse = returnResponse)
	}

	#' Get a Line
	#'
	#' This function returns a dataframe or json object of a Line
	#' @param LineID The ID of the Line to return.
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given Line. Defaults to FALSE for all return fields which, for convenience, returns all fields for the Line.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object listed as text. Run \code{\link{getSchemaForObjects}}('Line') to get more field paths.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{listSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Food Service
	#' @return A dataframe or of Line
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	getLine <- function(LineID, Code = F, Description = F, CodeDescription = F, EntityID = F, ShowProfilePicture = F, ShowItemPicture = F, AcceptPayment = F, UseLowBalanceAlert = F, LowBalanceAlertThreshold = F, DefaultDisplayAmountReceived = F, DefaultDisplayAmountApplied = F, ShowGovernmentFundedPrices = F, QueuingEnabled = F, Host = F, MaskID = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, OnlyShowCustomersEnrolledInEntity = F, AllowLineWorkersToEditPurchase = F, AllowLineWorkersToDisableStandardItem = F, AllowChangeSaleDate = F, fieldPaths = NULL, entityId = 1, schoolYearId = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment()) %>% purrr::keep(names(.) != "LineID")

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		getSkyObject(module = "FoodService", objectName = "Line", objectId = LineID, searchFields = searchFields, entityId = entityId, flatten = flatten, returnResponse = returnResponse)
	}

	#' Delete a Line
	#'
	#' This function deletes a Line
	#' @param LineID The ID of the Line to delete
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{listSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Food Service
	#' @return The LineID of the deleted Line.
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	deleteLine <- function(LineID, ignoreWarnings = F, entityId = 1, schoolYearId = NULL, flatten = T, returnResponse = F){

		deleteSkyObject(module = "FoodService", objectName = "Line", objectId = LineID, ignoreWarnings = ignoreWarnings, entityId = entityId, flatten = flatten, returnResponse = returnResponse)
	}

	#' Create a Line
	#'
	#' This function creates a Line
	#' @param fieldNames The field values to give the created Line. Each defaults to NULL.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{listSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Food Service
	#' @return A newly created Line
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	createLine <- function(Code = NULL, Description = NULL, EntityID = NULL, ShowProfilePicture = NULL, ShowItemPicture = NULL, AcceptPayment = NULL, UseLowBalanceAlert = NULL, LowBalanceAlertThreshold = NULL, DefaultDisplayAmountReceived = NULL, DefaultDisplayAmountApplied = NULL, ShowGovernmentFundedPrices = NULL, QueuingEnabled = NULL, Host = NULL, MaskID = NULL, OnlyShowCustomersEnrolledInEntity = NULL, AllowLineWorkersToEditPurchase = NULL, AllowLineWorkersToDisableStandardItem = NULL, AllowChangeSaleDate = NULL, entityId = 1, schoolYearId = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment())

		body <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper()) %>% purrr::compact()

		createSkyObject(module = "FoodService", objectName = "Line", body = list(DataObject = body), searchFields = append("LineID", body %>% names()), entityId = entityId, schoolYearId = schoolYearId, flatten = flatten, returnResponse = returnResponse)
	}

	#' Modify a Line
	#'
	#' This function modifies a Line
	#' @param fieldNames The field values to give the modified Line. Each defaults to NULL.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{listSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Food Service
	#' @return The modified Line
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	modifyLine <- function(LineID, Code = NULL, Description = NULL, EntityID = NULL, ShowProfilePicture = NULL, ShowItemPicture = NULL, AcceptPayment = NULL, UseLowBalanceAlert = NULL, LowBalanceAlertThreshold = NULL, DefaultDisplayAmountReceived = NULL, DefaultDisplayAmountApplied = NULL, ShowGovernmentFundedPrices = NULL, QueuingEnabled = NULL, Host = NULL, MaskID = NULL, OnlyShowCustomersEnrolledInEntity = NULL, AllowLineWorkersToEditPurchase = NULL, AllowLineWorkersToDisableStandardItem = NULL, AllowChangeSaleDate = NULL, entityId = 1, schoolYearId = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment())

		body <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper()) %>% purrr::compact()

		modifySkyObject(module = "FoodService", objectName = "Line", objectId = LineID, body = list(DataObject = body), searchFields = append("LineID", body %>% names()), entityId = entityId, schoolYearId = schoolYearId, flatten = flatten, returnResponse = returnResponse)
	}

	#' List FoodServiceItems
	#'
	#' This function returns a dataframe or json object of FoodServiceItems
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given FoodServiceItems. Defaults to FALSE for all return fields which, for convenience, returns all fields for the FoodServiceItems.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object listed as text. Run \code{\link{getSchemaForObjects}}('FoodServiceItem') to get more field paths.
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
	#' @concept Food Service
	#' @return A list of FoodServiceItems
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	listFoodServiceItems <- function(searchConditionsList = NULL, ItemID = F, Code = F, Description = F, IsActive = F, ItemTypeID = F, IsGovernmentFunded = F, UsePriceCategory = F, UseEligibilityCategory = F, CodeDescription = F, IsPricingComplete = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, fieldPaths = NULL, searchConditionsGroupType = "And", searchSortFieldNamesList = NULL, searchSortFieldNamesDescendingList = NULL, entityId = 1, schoolYearId = NULL, page = 1, pageSize = 100000, flatten = T, returnResponse = F){

		params <- as.list(environment())

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		listSkyObjects(module = "FoodService", objectName = "Item", searchFields = searchFields %>% append(fieldPaths), page = page, pageSize = pageSize, SearchConditionsList = searchConditionsList, SearchConditionsGroupType = searchConditionsGroupType, SearchSortFieldNamesList = searchSortFieldNamesList, SearchSortFieldNamesDescendingList = searchSortFieldNamesDescendingList, entityId = entityId, schoolYearId = schoolYearId, flatten = flatten, returnResponse = returnResponse)
	}

	#' Get a FoodServiceItem
	#'
	#' This function returns a dataframe or json object of a FoodServiceItem
	#' @param FoodServiceItemID The ID of the FoodServiceItem to return.
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given FoodServiceItem. Defaults to FALSE for all return fields which, for convenience, returns all fields for the FoodServiceItem.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object listed as text. Run \code{\link{getSchemaForObjects}}('FoodServiceItem') to get more field paths.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{listSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Food Service
	#' @return A dataframe or of FoodServiceItem
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	getFoodServiceItem <- function(FoodServiceItemID, ItemID = F, Code = F, Description = F, IsActive = F, ItemTypeID = F, IsGovernmentFunded = F, UsePriceCategory = F, UseEligibilityCategory = F, CodeDescription = F, IsPricingComplete = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, fieldPaths = NULL, entityId = 1, schoolYearId = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment()) %>% purrr::keep(names(.) != "FoodServiceItemID")

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		getSkyObject(module = "FoodService", objectName = "Item", objectId = FoodServiceItemID, searchFields = searchFields, entityId = entityId, flatten = flatten, returnResponse = returnResponse)
	}

	#' Delete a FoodServiceItem
	#'
	#' This function deletes a FoodServiceItem
	#' @param FoodServiceItemID The ID of the FoodServiceItem to delete
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{listSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Food Service
	#' @return The FoodServiceItemID of the deleted FoodServiceItem.
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	deleteFoodServiceItem <- function(FoodServiceItemID, ignoreWarnings = F, entityId = 1, schoolYearId = NULL, flatten = T, returnResponse = F){

		deleteSkyObject(module = "FoodService", objectName = "Item", objectId = FoodServiceItemID, ignoreWarnings = ignoreWarnings, entityId = entityId, flatten = flatten, returnResponse = returnResponse)
	}

	#' Create a FoodServiceItem
	#'
	#' This function creates a FoodServiceItem
	#' @param fieldNames The field values to give the created FoodServiceItem. Each defaults to NULL.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{listSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Food Service
	#' @return A newly created FoodServiceItem
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	createFoodServiceItem <- function(Code = NULL, Description = NULL, IsActive = NULL, ItemTypeID = NULL, IsGovernmentFunded = NULL, UsePriceCategory = NULL, UseEligibilityCategory = NULL, entityId = 1, schoolYearId = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment())

		body <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper()) %>% purrr::compact()

		createSkyObject(module = "FoodService", objectName = "Item", body = list(DataObject = body), searchFields = append("ItemID", body %>% names()), entityId = entityId, schoolYearId = schoolYearId, flatten = flatten, returnResponse = returnResponse)
	}

	#' Modify a FoodServiceItem
	#'
	#' This function modifies a FoodServiceItem
	#' @param fieldNames The field values to give the modified FoodServiceItem. Each defaults to NULL.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{listSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Food Service
	#' @return The modified FoodServiceItem
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	modifyFoodServiceItem <- function(ItemID, Code = NULL, Description = NULL, IsActive = NULL, ItemTypeID = NULL, IsGovernmentFunded = NULL, UsePriceCategory = NULL, UseEligibilityCategory = NULL, entityId = 1, schoolYearId = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment())

		body <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper()) %>% purrr::compact()

		modifySkyObject(module = "FoodService", objectName = "Item", objectId = ItemID, body = list(DataObject = body), searchFields = append("ItemID", body %>% names()), entityId = entityId, schoolYearId = schoolYearId, flatten = flatten, returnResponse = returnResponse)
	}

	#' List ItemPrices
	#'
	#' This function returns a dataframe or json object of ItemPrices
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given ItemPrices. Defaults to FALSE for all return fields which, for convenience, returns all fields for the ItemPrices.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object listed as text. Run \code{\link{getSchemaForObjects}}('ItemPrice') to get more field paths.
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
	#' @concept Food Service
	#' @return A list of ItemPrices
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	listItemPrices <- function(searchConditionsList = NULL, ItemPriceID = F, ItemID = F, DistrictGroupKey = F, ItemLimit = F, IsGovernmentFunded = F, EligibilityCategoryID = F, PriceCategoryID = F, EffectiveTime = F, EffectiveDate = F, PurchaseAmount = F, AdditionalPurchaseAmount = F, IsCurrentActivePrice = F, NeedsAdditionalSetup = F, RenderIsGovernmentFundedReadOnly = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, StopAdditionalPurchases = F, fieldPaths = NULL, searchConditionsGroupType = "And", searchSortFieldNamesList = NULL, searchSortFieldNamesDescendingList = NULL, entityId = 1, schoolYearId = NULL, page = 1, pageSize = 100000, flatten = T, returnResponse = F){

		params <- as.list(environment())

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		listSkyObjects(module = "FoodService", objectName = "ItemPrice", searchFields = searchFields %>% append(fieldPaths), page = page, pageSize = pageSize, SearchConditionsList = searchConditionsList, SearchConditionsGroupType = searchConditionsGroupType, SearchSortFieldNamesList = searchSortFieldNamesList, SearchSortFieldNamesDescendingList = searchSortFieldNamesDescendingList, entityId = entityId, schoolYearId = schoolYearId, flatten = flatten, returnResponse = returnResponse)
	}

	#' Get an ItemPrice
	#'
	#' This function returns a dataframe or json object of an ItemPrice
	#' @param ItemPriceID The ID of the ItemPrice to return.
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given ItemPrice. Defaults to FALSE for all return fields which, for convenience, returns all fields for the ItemPrice.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object listed as text. Run \code{\link{getSchemaForObjects}}('ItemPrice') to get more field paths.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{listSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Food Service
	#' @return A dataframe or of ItemPrice
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	getItemPrice <- function(ItemPriceID, ItemID = F, DistrictGroupKey = F, ItemLimit = F, IsGovernmentFunded = F, EligibilityCategoryID = F, PriceCategoryID = F, EffectiveTime = F, EffectiveDate = F, PurchaseAmount = F, AdditionalPurchaseAmount = F, IsCurrentActivePrice = F, NeedsAdditionalSetup = F, RenderIsGovernmentFundedReadOnly = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, StopAdditionalPurchases = F, fieldPaths = NULL, entityId = 1, schoolYearId = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment()) %>% purrr::keep(names(.) != "ItemPriceID")

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		getSkyObject(module = "FoodService", objectName = "ItemPrice", objectId = ItemPriceID, searchFields = searchFields, entityId = entityId, flatten = flatten, returnResponse = returnResponse)
	}

	#' Delete an ItemPrice
	#'
	#' This function deletes an ItemPrice
	#' @param ItemPriceID The ID of the ItemPrice to delete
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{listSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Food Service
	#' @return The ItemPriceID of the deleted ItemPrice.
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	deleteItemPrice <- function(ItemPriceID, ignoreWarnings = F, entityId = 1, schoolYearId = NULL, flatten = T, returnResponse = F){

		deleteSkyObject(module = "FoodService", objectName = "ItemPrice", objectId = ItemPriceID, ignoreWarnings = ignoreWarnings, entityId = entityId, flatten = flatten, returnResponse = returnResponse)
	}

	#' Create an ItemPrice
	#'
	#' This function creates an ItemPrice
	#' @param fieldNames The field values to give the created ItemPrice. Each defaults to NULL.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{listSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Food Service
	#' @return A newly created ItemPrice
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	createItemPrice <- function(ItemID = NULL, DistrictGroupKey = NULL, ItemLimit = NULL, IsGovernmentFunded = NULL, EligibilityCategoryID = NULL, PriceCategoryID = NULL, EffectiveTime = NULL, PurchaseAmount = NULL, AdditionalPurchaseAmount = NULL, StopAdditionalPurchases = NULL, entityId = 1, schoolYearId = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment())

		body <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper()) %>% purrr::compact()

		createSkyObject(module = "FoodService", objectName = "ItemPrice", body = list(DataObject = body), searchFields = append("ItemPriceID", body %>% names()), entityId = entityId, schoolYearId = schoolYearId, flatten = flatten, returnResponse = returnResponse)
	}

	#' Modify an ItemPrice
	#'
	#' This function modifies an ItemPrice
	#' @param fieldNames The field values to give the modified ItemPrice. Each defaults to NULL.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{listSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Food Service
	#' @return The modified ItemPrice
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	modifyItemPrice <- function(ItemPriceID, ItemID = NULL, DistrictGroupKey = NULL, ItemLimit = NULL, IsGovernmentFunded = NULL, EligibilityCategoryID = NULL, PriceCategoryID = NULL, EffectiveTime = NULL, PurchaseAmount = NULL, AdditionalPurchaseAmount = NULL, StopAdditionalPurchases = NULL, entityId = 1, schoolYearId = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment())

		body <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper()) %>% purrr::compact()

		modifySkyObject(module = "FoodService", objectName = "ItemPrice", objectId = ItemPriceID, body = list(DataObject = body), searchFields = append("ItemPriceID", body %>% names()), entityId = entityId, schoolYearId = schoolYearId, flatten = flatten, returnResponse = returnResponse)
	}

	#' List ItemTypes
	#'
	#' This function returns a dataframe or json object of ItemTypes
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given ItemTypes. Defaults to FALSE for all return fields which, for convenience, returns all fields for the ItemTypes.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object listed as text. Run \code{\link{getSchemaForObjects}}('ItemType') to get more field paths.
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
	#' @concept Food Service
	#' @return A list of ItemTypes
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	listItemTypes <- function(searchConditionsList = NULL, ItemTypeID = F, Code = F, Description = F, DistrictID = F, AccountID = F, CodeDescription = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, ReimbursableMealTypeID = F, fieldPaths = NULL, searchConditionsGroupType = "And", searchSortFieldNamesList = NULL, searchSortFieldNamesDescendingList = NULL, entityId = 1, schoolYearId = NULL, page = 1, pageSize = 100000, flatten = T, returnResponse = F){

		params <- as.list(environment())

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		listSkyObjects(module = "FoodService", objectName = "ItemType", searchFields = searchFields %>% append(fieldPaths), page = page, pageSize = pageSize, SearchConditionsList = searchConditionsList, SearchConditionsGroupType = searchConditionsGroupType, SearchSortFieldNamesList = searchSortFieldNamesList, SearchSortFieldNamesDescendingList = searchSortFieldNamesDescendingList, entityId = entityId, schoolYearId = schoolYearId, flatten = flatten, returnResponse = returnResponse)
	}

	#' Get an ItemType
	#'
	#' This function returns a dataframe or json object of an ItemType
	#' @param ItemTypeID The ID of the ItemType to return.
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given ItemType. Defaults to FALSE for all return fields which, for convenience, returns all fields for the ItemType.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object listed as text. Run \code{\link{getSchemaForObjects}}('ItemType') to get more field paths.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{listSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Food Service
	#' @return A dataframe or of ItemType
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	getItemType <- function(ItemTypeID, Code = F, Description = F, DistrictID = F, AccountID = F, CodeDescription = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, ReimbursableMealTypeID = F, fieldPaths = NULL, entityId = 1, schoolYearId = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment()) %>% purrr::keep(names(.) != "ItemTypeID")

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		getSkyObject(module = "FoodService", objectName = "ItemType", objectId = ItemTypeID, searchFields = searchFields, entityId = entityId, flatten = flatten, returnResponse = returnResponse)
	}

	#' Delete an ItemType
	#'
	#' This function deletes an ItemType
	#' @param ItemTypeID The ID of the ItemType to delete
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{listSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Food Service
	#' @return The ItemTypeID of the deleted ItemType.
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	deleteItemType <- function(ItemTypeID, ignoreWarnings = F, entityId = 1, schoolYearId = NULL, flatten = T, returnResponse = F){

		deleteSkyObject(module = "FoodService", objectName = "ItemType", objectId = ItemTypeID, ignoreWarnings = ignoreWarnings, entityId = entityId, flatten = flatten, returnResponse = returnResponse)
	}

	#' Create an ItemType
	#'
	#' This function creates an ItemType
	#' @param fieldNames The field values to give the created ItemType. Each defaults to NULL.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{listSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Food Service
	#' @return A newly created ItemType
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	createItemType <- function(Code = NULL, Description = NULL, DistrictID = NULL, AccountID = NULL, ReimbursableMealTypeID = NULL, entityId = 1, schoolYearId = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment())

		body <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper()) %>% purrr::compact()

		createSkyObject(module = "FoodService", objectName = "ItemType", body = list(DataObject = body), searchFields = append("ItemTypeID", body %>% names()), entityId = entityId, schoolYearId = schoolYearId, flatten = flatten, returnResponse = returnResponse)
	}

	#' Modify an ItemType
	#'
	#' This function modifies an ItemType
	#' @param fieldNames The field values to give the modified ItemType. Each defaults to NULL.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{listSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Food Service
	#' @return The modified ItemType
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	modifyItemType <- function(ItemTypeID, Code = NULL, Description = NULL, DistrictID = NULL, AccountID = NULL, ReimbursableMealTypeID = NULL, entityId = 1, schoolYearId = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment())

		body <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper()) %>% purrr::compact()

		modifySkyObject(module = "FoodService", objectName = "ItemType", objectId = ItemTypeID, body = list(DataObject = body), searchFields = append("ItemTypeID", body %>% names()), entityId = entityId, schoolYearId = schoolYearId, flatten = flatten, returnResponse = returnResponse)
	}

	#' List FoodServicePayors
	#'
	#' This function returns a dataframe or json object of FoodServicePayors
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given FoodServicePayors. Defaults to FALSE for all return fields which, for convenience, returns all fields for the FoodServicePayors.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object listed as text. Run \code{\link{getSchemaForObjects}}('FoodServicePayor') to get more field paths.
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
	#' @concept Food Service
	#' @return A list of FoodServicePayors
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	listFoodServicePayors <- function(searchConditionsList = NULL, PayorID = F, NameID = F, DistrictID = F, IsActive = F, CurrentPayments = F, CurrentPurchases = F, CurrentBalance = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, BalanceAtEndOfDay = F, fieldPaths = NULL, searchConditionsGroupType = "And", searchSortFieldNamesList = NULL, searchSortFieldNamesDescendingList = NULL, entityId = 1, schoolYearId = NULL, page = 1, pageSize = 100000, flatten = T, returnResponse = F){

		params <- as.list(environment())

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		listSkyObjects(module = "FoodService", objectName = "Payor", searchFields = searchFields %>% append(fieldPaths), page = page, pageSize = pageSize, SearchConditionsList = searchConditionsList, SearchConditionsGroupType = searchConditionsGroupType, SearchSortFieldNamesList = searchSortFieldNamesList, SearchSortFieldNamesDescendingList = searchSortFieldNamesDescendingList, entityId = entityId, schoolYearId = schoolYearId, flatten = flatten, returnResponse = returnResponse)
	}

	#' Get a FoodServicePayor
	#'
	#' This function returns a dataframe or json object of a FoodServicePayor
	#' @param FoodServicePayorID The ID of the FoodServicePayor to return.
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given FoodServicePayor. Defaults to FALSE for all return fields which, for convenience, returns all fields for the FoodServicePayor.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object listed as text. Run \code{\link{getSchemaForObjects}}('FoodServicePayor') to get more field paths.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{listSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Food Service
	#' @return A dataframe or of FoodServicePayor
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	getFoodServicePayor <- function(FoodServicePayorID, PayorID = F, NameID = F, DistrictID = F, IsActive = F, CurrentPayments = F, CurrentPurchases = F, CurrentBalance = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, BalanceAtEndOfDay = F, fieldPaths = NULL, entityId = 1, schoolYearId = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment()) %>% purrr::keep(names(.) != "FoodServicePayorID")

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		getSkyObject(module = "FoodService", objectName = "Payor", objectId = FoodServicePayorID, searchFields = searchFields, entityId = entityId, flatten = flatten, returnResponse = returnResponse)
	}

	#' Delete a FoodServicePayor
	#'
	#' This function deletes a FoodServicePayor
	#' @param FoodServicePayorID The ID of the FoodServicePayor to delete
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{listSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Food Service
	#' @return The FoodServicePayorID of the deleted FoodServicePayor.
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	deleteFoodServicePayor <- function(FoodServicePayorID, ignoreWarnings = F, entityId = 1, schoolYearId = NULL, flatten = T, returnResponse = F){

		deleteSkyObject(module = "FoodService", objectName = "Payor", objectId = FoodServicePayorID, ignoreWarnings = ignoreWarnings, entityId = entityId, flatten = flatten, returnResponse = returnResponse)
	}

	#' Create a FoodServicePayor
	#'
	#' This function creates a FoodServicePayor
	#' @param fieldNames The field values to give the created FoodServicePayor. Each defaults to NULL.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{listSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Food Service
	#' @return A newly created FoodServicePayor
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	createFoodServicePayor <- function(NameID = NULL, DistrictID = NULL, IsActive = NULL, entityId = 1, schoolYearId = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment())

		body <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper()) %>% purrr::compact()

		createSkyObject(module = "FoodService", objectName = "Payor", body = list(DataObject = body), searchFields = append("PayorID", body %>% names()), entityId = entityId, schoolYearId = schoolYearId, flatten = flatten, returnResponse = returnResponse)
	}

	#' Modify a FoodServicePayor
	#'
	#' This function modifies a FoodServicePayor
	#' @param fieldNames The field values to give the modified FoodServicePayor. Each defaults to NULL.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{listSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Food Service
	#' @return The modified FoodServicePayor
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	modifyFoodServicePayor <- function(PayorID, NameID = NULL, DistrictID = NULL, IsActive = NULL, entityId = 1, schoolYearId = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment())

		body <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper()) %>% purrr::compact()

		modifySkyObject(module = "FoodService", objectName = "Payor", objectId = PayorID, body = list(DataObject = body), searchFields = append("PayorID", body %>% names()), entityId = entityId, schoolYearId = schoolYearId, flatten = flatten, returnResponse = returnResponse)
	}

	#' List PayorCustomers
	#'
	#' This function returns a dataframe or json object of PayorCustomers
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given PayorCustomers. Defaults to FALSE for all return fields which, for convenience, returns all fields for the PayorCustomers.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object listed as text. Run \code{\link{getSchemaForObjects}}('PayorCustomer') to get more field paths.
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
	#' @concept Food Service
	#' @return A list of PayorCustomers
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	listPayorCustomers <- function(searchConditionsList = NULL, PayorCustomerID = F, PayorID = F, CustomerID = F, DistributionPercent = F, IsPrimaryPayor = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, fieldPaths = NULL, searchConditionsGroupType = "And", searchSortFieldNamesList = NULL, searchSortFieldNamesDescendingList = NULL, entityId = 1, schoolYearId = NULL, page = 1, pageSize = 100000, flatten = T, returnResponse = F){

		params <- as.list(environment())

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		listSkyObjects(module = "FoodService", objectName = "PayorCustomer", searchFields = searchFields %>% append(fieldPaths), page = page, pageSize = pageSize, SearchConditionsList = searchConditionsList, SearchConditionsGroupType = searchConditionsGroupType, SearchSortFieldNamesList = searchSortFieldNamesList, SearchSortFieldNamesDescendingList = searchSortFieldNamesDescendingList, entityId = entityId, schoolYearId = schoolYearId, flatten = flatten, returnResponse = returnResponse)
	}

	#' Get a PayorCustomer
	#'
	#' This function returns a dataframe or json object of a PayorCustomer
	#' @param PayorCustomerID The ID of the PayorCustomer to return.
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given PayorCustomer. Defaults to FALSE for all return fields which, for convenience, returns all fields for the PayorCustomer.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object listed as text. Run \code{\link{getSchemaForObjects}}('PayorCustomer') to get more field paths.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{listSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Food Service
	#' @return A dataframe or of PayorCustomer
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	getPayorCustomer <- function(PayorCustomerID, PayorID = F, CustomerID = F, DistributionPercent = F, IsPrimaryPayor = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, fieldPaths = NULL, entityId = 1, schoolYearId = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment()) %>% purrr::keep(names(.) != "PayorCustomerID")

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		getSkyObject(module = "FoodService", objectName = "PayorCustomer", objectId = PayorCustomerID, searchFields = searchFields, entityId = entityId, flatten = flatten, returnResponse = returnResponse)
	}

	#' Delete a PayorCustomer
	#'
	#' This function deletes a PayorCustomer
	#' @param PayorCustomerID The ID of the PayorCustomer to delete
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{listSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Food Service
	#' @return The PayorCustomerID of the deleted PayorCustomer.
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	deletePayorCustomer <- function(PayorCustomerID, ignoreWarnings = F, entityId = 1, schoolYearId = NULL, flatten = T, returnResponse = F){

		deleteSkyObject(module = "FoodService", objectName = "PayorCustomer", objectId = PayorCustomerID, ignoreWarnings = ignoreWarnings, entityId = entityId, flatten = flatten, returnResponse = returnResponse)
	}

	#' Create a PayorCustomer
	#'
	#' This function creates a PayorCustomer
	#' @param fieldNames The field values to give the created PayorCustomer. Each defaults to NULL.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{listSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Food Service
	#' @return A newly created PayorCustomer
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	createPayorCustomer <- function(PayorID = NULL, CustomerID = NULL, DistributionPercent = NULL, IsPrimaryPayor = NULL, entityId = 1, schoolYearId = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment())

		body <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper()) %>% purrr::compact()

		createSkyObject(module = "FoodService", objectName = "PayorCustomer", body = list(DataObject = body), searchFields = append("PayorCustomerID", body %>% names()), entityId = entityId, schoolYearId = schoolYearId, flatten = flatten, returnResponse = returnResponse)
	}

	#' Modify a PayorCustomer
	#'
	#' This function modifies a PayorCustomer
	#' @param fieldNames The field values to give the modified PayorCustomer. Each defaults to NULL.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{listSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Food Service
	#' @return The modified PayorCustomer
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	modifyPayorCustomer <- function(PayorCustomerID, PayorID = NULL, CustomerID = NULL, DistributionPercent = NULL, IsPrimaryPayor = NULL, entityId = 1, schoolYearId = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment())

		body <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper()) %>% purrr::compact()

		modifySkyObject(module = "FoodService", objectName = "PayorCustomer", objectId = PayorCustomerID, body = list(DataObject = body), searchFields = append("PayorCustomerID", body %>% names()), entityId = entityId, schoolYearId = schoolYearId, flatten = flatten, returnResponse = returnResponse)
	}

	#' List PriceCategories
	#'
	#' This function returns a dataframe or json object of PriceCategories
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given PriceCategories. Defaults to FALSE for all return fields which, for convenience, returns all fields for the PriceCategories.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object listed as text. Run \code{\link{getSchemaForObjects}}('PriceCategory') to get more field paths.
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
	#' @concept Food Service
	#' @return A list of PriceCategories
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	listPriceCategories <- function(searchConditionsList = NULL, PriceCategoryID = F, Code = F, Description = F, DistrictID = F, DistrictGroupKey = F, CodeDescription = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, fieldPaths = NULL, searchConditionsGroupType = "And", searchSortFieldNamesList = NULL, searchSortFieldNamesDescendingList = NULL, entityId = 1, schoolYearId = NULL, page = 1, pageSize = 100000, flatten = T, returnResponse = F){

		params <- as.list(environment())

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		listSkyObjects(module = "FoodService", objectName = "PriceCategory", searchFields = searchFields %>% append(fieldPaths), page = page, pageSize = pageSize, SearchConditionsList = searchConditionsList, SearchConditionsGroupType = searchConditionsGroupType, SearchSortFieldNamesList = searchSortFieldNamesList, SearchSortFieldNamesDescendingList = searchSortFieldNamesDescendingList, entityId = entityId, schoolYearId = schoolYearId, flatten = flatten, returnResponse = returnResponse)
	}

	#' Get a PriceCategory
	#'
	#' This function returns a dataframe or json object of a PriceCategory
	#' @param PriceCategoryID The ID of the PriceCategory to return.
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given PriceCategory. Defaults to FALSE for all return fields which, for convenience, returns all fields for the PriceCategory.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object listed as text. Run \code{\link{getSchemaForObjects}}('PriceCategory') to get more field paths.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{listSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Food Service
	#' @return A dataframe or of PriceCategory
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	getPriceCategory <- function(PriceCategoryID, Code = F, Description = F, DistrictID = F, DistrictGroupKey = F, CodeDescription = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, fieldPaths = NULL, entityId = 1, schoolYearId = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment()) %>% purrr::keep(names(.) != "PriceCategoryID")

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		getSkyObject(module = "FoodService", objectName = "PriceCategory", objectId = PriceCategoryID, searchFields = searchFields, entityId = entityId, flatten = flatten, returnResponse = returnResponse)
	}

	#' Delete a PriceCategory
	#'
	#' This function deletes a PriceCategory
	#' @param PriceCategoryID The ID of the PriceCategory to delete
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{listSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Food Service
	#' @return The PriceCategoryID of the deleted PriceCategory.
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	deletePriceCategory <- function(PriceCategoryID, ignoreWarnings = F, entityId = 1, schoolYearId = NULL, flatten = T, returnResponse = F){

		deleteSkyObject(module = "FoodService", objectName = "PriceCategory", objectId = PriceCategoryID, ignoreWarnings = ignoreWarnings, entityId = entityId, flatten = flatten, returnResponse = returnResponse)
	}

	#' Create a PriceCategory
	#'
	#' This function creates a PriceCategory
	#' @param fieldNames The field values to give the created PriceCategory. Each defaults to NULL.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{listSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Food Service
	#' @return A newly created PriceCategory
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	createPriceCategory <- function(Code = NULL, Description = NULL, DistrictID = NULL, DistrictGroupKey = NULL, entityId = 1, schoolYearId = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment())

		body <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper()) %>% purrr::compact()

		createSkyObject(module = "FoodService", objectName = "PriceCategory", body = list(DataObject = body), searchFields = append("PriceCategoryID", body %>% names()), entityId = entityId, schoolYearId = schoolYearId, flatten = flatten, returnResponse = returnResponse)
	}

	#' Modify a PriceCategory
	#'
	#' This function modifies a PriceCategory
	#' @param fieldNames The field values to give the modified PriceCategory. Each defaults to NULL.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{listSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Food Service
	#' @return The modified PriceCategory
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	modifyPriceCategory <- function(PriceCategoryID, Code = NULL, Description = NULL, DistrictID = NULL, DistrictGroupKey = NULL, entityId = 1, schoolYearId = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment())

		body <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper()) %>% purrr::compact()

		modifySkyObject(module = "FoodService", objectName = "PriceCategory", objectId = PriceCategoryID, body = list(DataObject = body), searchFields = append("PriceCategoryID", body %>% names()), entityId = entityId, schoolYearId = schoolYearId, flatten = flatten, returnResponse = returnResponse)
	}

	#' List EligibilityCategories
	#'
	#' This function returns a dataframe or json object of EligibilityCategories
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given EligibilityCategories. Defaults to FALSE for all return fields which, for convenience, returns all fields for the EligibilityCategories.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object listed as text. Run \code{\link{getSchemaForObjects}}('EligibilityCategory') to get more field paths.
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
	#' @concept Food Service
	#' @return A list of EligibilityCategories
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	listEligibilityCategories <- function(searchConditionsList = NULL, EligibilityCategoryID = F, Code = F, Description = F, DistrictID = F, DistrictGroupKey = F, Type = F, UsePriceCategory = F, MaskID = F, ExcludeFromCustomerCategoryImport = F, CodeDescription = F, CodeInUse = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, EdFiSchoolFoodServiceProgramServiceTypeID = F, fieldPaths = NULL, searchConditionsGroupType = "And", searchSortFieldNamesList = NULL, searchSortFieldNamesDescendingList = NULL, entityId = 1, schoolYearId = NULL, page = 1, pageSize = 100000, flatten = T, returnResponse = F){

		params <- as.list(environment())

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		listSkyObjects(module = "FoodService", objectName = "EligibilityCategory", searchFields = searchFields %>% append(fieldPaths), page = page, pageSize = pageSize, SearchConditionsList = searchConditionsList, SearchConditionsGroupType = searchConditionsGroupType, SearchSortFieldNamesList = searchSortFieldNamesList, SearchSortFieldNamesDescendingList = searchSortFieldNamesDescendingList, entityId = entityId, schoolYearId = schoolYearId, flatten = flatten, returnResponse = returnResponse)
	}

	#' Get an EligibilityCategory
	#'
	#' This function returns a dataframe or json object of an EligibilityCategory
	#' @param EligibilityCategoryID The ID of the EligibilityCategory to return.
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given EligibilityCategory. Defaults to FALSE for all return fields which, for convenience, returns all fields for the EligibilityCategory.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object listed as text. Run \code{\link{getSchemaForObjects}}('EligibilityCategory') to get more field paths.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{listSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Food Service
	#' @return A dataframe or of EligibilityCategory
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	getEligibilityCategory <- function(EligibilityCategoryID, Code = F, Description = F, DistrictID = F, DistrictGroupKey = F, Type = F, UsePriceCategory = F, MaskID = F, ExcludeFromCustomerCategoryImport = F, CodeDescription = F, CodeInUse = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, EdFiSchoolFoodServiceProgramServiceTypeID = F, fieldPaths = NULL, entityId = 1, schoolYearId = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment()) %>% purrr::keep(names(.) != "EligibilityCategoryID")

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		getSkyObject(module = "FoodService", objectName = "EligibilityCategory", objectId = EligibilityCategoryID, searchFields = searchFields, entityId = entityId, flatten = flatten, returnResponse = returnResponse)
	}

	#' Delete an EligibilityCategory
	#'
	#' This function deletes an EligibilityCategory
	#' @param EligibilityCategoryID The ID of the EligibilityCategory to delete
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{listSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Food Service
	#' @return The EligibilityCategoryID of the deleted EligibilityCategory.
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	deleteEligibilityCategory <- function(EligibilityCategoryID, ignoreWarnings = F, entityId = 1, schoolYearId = NULL, flatten = T, returnResponse = F){

		deleteSkyObject(module = "FoodService", objectName = "EligibilityCategory", objectId = EligibilityCategoryID, ignoreWarnings = ignoreWarnings, entityId = entityId, flatten = flatten, returnResponse = returnResponse)
	}

	#' Create an EligibilityCategory
	#'
	#' This function creates an EligibilityCategory
	#' @param fieldNames The field values to give the created EligibilityCategory. Each defaults to NULL.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{listSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Food Service
	#' @return A newly created EligibilityCategory
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	createEligibilityCategory <- function(Code = NULL, Description = NULL, DistrictID = NULL, DistrictGroupKey = NULL, Type = NULL, UsePriceCategory = NULL, MaskID = NULL, ExcludeFromCustomerCategoryImport = NULL, EdFiSchoolFoodServiceProgramServiceTypeID = NULL, entityId = 1, schoolYearId = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment())

		body <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper()) %>% purrr::compact()

		createSkyObject(module = "FoodService", objectName = "EligibilityCategory", body = list(DataObject = body), searchFields = append("EligibilityCategoryID", body %>% names()), entityId = entityId, schoolYearId = schoolYearId, flatten = flatten, returnResponse = returnResponse)
	}

	#' Modify an EligibilityCategory
	#'
	#' This function modifies an EligibilityCategory
	#' @param fieldNames The field values to give the modified EligibilityCategory. Each defaults to NULL.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{listSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Food Service
	#' @return The modified EligibilityCategory
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	modifyEligibilityCategory <- function(EligibilityCategoryID, Code = NULL, Description = NULL, DistrictID = NULL, DistrictGroupKey = NULL, Type = NULL, UsePriceCategory = NULL, MaskID = NULL, ExcludeFromCustomerCategoryImport = NULL, EdFiSchoolFoodServiceProgramServiceTypeID = NULL, entityId = 1, schoolYearId = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment())

		body <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper()) %>% purrr::compact()

		modifySkyObject(module = "FoodService", objectName = "EligibilityCategory", objectId = EligibilityCategoryID, body = list(DataObject = body), searchFields = append("EligibilityCategoryID", body %>% names()), entityId = entityId, schoolYearId = schoolYearId, flatten = flatten, returnResponse = returnResponse)
	}

	#' List PINDefaults
	#'
	#' This function returns a dataframe or json object of PINDefaults
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given PINDefaults. Defaults to FALSE for all return fields which, for convenience, returns all fields for the PINDefaults.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object listed as text. Run \code{\link{getSchemaForObjects}}('PINDefault') to get more field paths.
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
	#' @concept Food Service
	#' @return A list of PINDefaults
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	listPINDefaults <- function(searchConditionsList = NULL, PINDefaultID = F, ConfigEntityGroupYearID = F, EntityGroupKey = F, PINLength = F, GradeLevelIDHigh = F, GradeLevelIDLow = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, PINDefaultIDClonedFrom = F, fieldPaths = NULL, searchConditionsGroupType = "And", searchSortFieldNamesList = NULL, searchSortFieldNamesDescendingList = NULL, entityId = 1, schoolYearId = NULL, page = 1, pageSize = 100000, flatten = T, returnResponse = F){

		params <- as.list(environment())

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		listSkyObjects(module = "FoodService", objectName = "PINDefault", searchFields = searchFields %>% append(fieldPaths), page = page, pageSize = pageSize, SearchConditionsList = searchConditionsList, SearchConditionsGroupType = searchConditionsGroupType, SearchSortFieldNamesList = searchSortFieldNamesList, SearchSortFieldNamesDescendingList = searchSortFieldNamesDescendingList, entityId = entityId, schoolYearId = schoolYearId, flatten = flatten, returnResponse = returnResponse)
	}

	#' Get a PINDefault
	#'
	#' This function returns a dataframe or json object of a PINDefault
	#' @param PINDefaultID The ID of the PINDefault to return.
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given PINDefault. Defaults to FALSE for all return fields which, for convenience, returns all fields for the PINDefault.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object listed as text. Run \code{\link{getSchemaForObjects}}('PINDefault') to get more field paths.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{listSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Food Service
	#' @return A dataframe or of PINDefault
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	getPINDefault <- function(PINDefaultID, ConfigEntityGroupYearID = F, EntityGroupKey = F, PINLength = F, GradeLevelIDHigh = F, GradeLevelIDLow = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, PINDefaultIDClonedFrom = F, fieldPaths = NULL, entityId = 1, schoolYearId = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment()) %>% purrr::keep(names(.) != "PINDefaultID")

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		getSkyObject(module = "FoodService", objectName = "PINDefault", objectId = PINDefaultID, searchFields = searchFields, entityId = entityId, flatten = flatten, returnResponse = returnResponse)
	}

	#' Delete a PINDefault
	#'
	#' This function deletes a PINDefault
	#' @param PINDefaultID The ID of the PINDefault to delete
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{listSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Food Service
	#' @return The PINDefaultID of the deleted PINDefault.
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	deletePINDefault <- function(PINDefaultID, ignoreWarnings = F, entityId = 1, schoolYearId = NULL, flatten = T, returnResponse = F){

		deleteSkyObject(module = "FoodService", objectName = "PINDefault", objectId = PINDefaultID, ignoreWarnings = ignoreWarnings, entityId = entityId, flatten = flatten, returnResponse = returnResponse)
	}

	#' Create a PINDefault
	#'
	#' This function creates a PINDefault
	#' @param fieldNames The field values to give the created PINDefault. Each defaults to NULL.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{listSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Food Service
	#' @return A newly created PINDefault
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	createPINDefault <- function(ConfigEntityGroupYearID = NULL, EntityGroupKey = NULL, PINLength = NULL, GradeLevelIDHigh = NULL, GradeLevelIDLow = NULL, PINDefaultIDClonedFrom = NULL, entityId = 1, schoolYearId = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment())

		body <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper()) %>% purrr::compact()

		createSkyObject(module = "FoodService", objectName = "PINDefault", body = list(DataObject = body), searchFields = append("PINDefaultID", body %>% names()), entityId = entityId, schoolYearId = schoolYearId, flatten = flatten, returnResponse = returnResponse)
	}

	#' Modify a PINDefault
	#'
	#' This function modifies a PINDefault
	#' @param fieldNames The field values to give the modified PINDefault. Each defaults to NULL.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{listSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Food Service
	#' @return The modified PINDefault
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	modifyPINDefault <- function(PINDefaultID, ConfigEntityGroupYearID = NULL, EntityGroupKey = NULL, PINLength = NULL, GradeLevelIDHigh = NULL, GradeLevelIDLow = NULL, PINDefaultIDClonedFrom = NULL, entityId = 1, schoolYearId = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment())

		body <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper()) %>% purrr::compact()

		modifySkyObject(module = "FoodService", objectName = "PINDefault", objectId = PINDefaultID, body = list(DataObject = body), searchFields = append("PINDefaultID", body %>% names()), entityId = entityId, schoolYearId = schoolYearId, flatten = flatten, returnResponse = returnResponse)
	}

	#' List TempFailedCustomers
	#'
	#' This function returns a dataframe or json object of TempFailedCustomers
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given TempFailedCustomers. Defaults to FALSE for all return fields which, for convenience, returns all fields for the TempFailedCustomers.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object listed as text. Run \code{\link{getSchemaForObjects}}('TempFailedCustomer') to get more field paths.
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
	#' @concept Food Service
	#' @return A list of TempFailedCustomers
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	listTempFailedCustomers <- function(searchConditionsList = NULL, TempFailedCustomerID = F, Note = F, FullNameLFM = F, StudentNumber = F, StaffNumber = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, Balance = F, fieldPaths = NULL, searchConditionsGroupType = "And", searchSortFieldNamesList = NULL, searchSortFieldNamesDescendingList = NULL, entityId = 1, schoolYearId = NULL, page = 1, pageSize = 100000, flatten = T, returnResponse = F){

		params <- as.list(environment())

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		listSkyObjects(module = "FoodService", objectName = "TempFailedCustomer", searchFields = searchFields %>% append(fieldPaths), page = page, pageSize = pageSize, SearchConditionsList = searchConditionsList, SearchConditionsGroupType = searchConditionsGroupType, SearchSortFieldNamesList = searchSortFieldNamesList, SearchSortFieldNamesDescendingList = searchSortFieldNamesDescendingList, entityId = entityId, schoolYearId = schoolYearId, flatten = flatten, returnResponse = returnResponse)
	}

	#' Get a TempFailedCustomer
	#'
	#' This function returns a dataframe or json object of a TempFailedCustomer
	#' @param TempFailedCustomerID The ID of the TempFailedCustomer to return.
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given TempFailedCustomer. Defaults to FALSE for all return fields which, for convenience, returns all fields for the TempFailedCustomer.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object listed as text. Run \code{\link{getSchemaForObjects}}('TempFailedCustomer') to get more field paths.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{listSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Food Service
	#' @return A dataframe or of TempFailedCustomer
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	getTempFailedCustomer <- function(TempFailedCustomerID, Note = F, FullNameLFM = F, StudentNumber = F, StaffNumber = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, Balance = F, fieldPaths = NULL, entityId = 1, schoolYearId = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment()) %>% purrr::keep(names(.) != "TempFailedCustomerID")

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		getSkyObject(module = "FoodService", objectName = "TempFailedCustomer", objectId = TempFailedCustomerID, searchFields = searchFields, entityId = entityId, flatten = flatten, returnResponse = returnResponse)
	}

	#' Delete a TempFailedCustomer
	#'
	#' This function deletes a TempFailedCustomer
	#' @param TempFailedCustomerID The ID of the TempFailedCustomer to delete
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{listSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Food Service
	#' @return The TempFailedCustomerID of the deleted TempFailedCustomer.
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	deleteTempFailedCustomer <- function(TempFailedCustomerID, ignoreWarnings = F, entityId = 1, schoolYearId = NULL, flatten = T, returnResponse = F){

		deleteSkyObject(module = "FoodService", objectName = "TempFailedCustomer", objectId = TempFailedCustomerID, ignoreWarnings = ignoreWarnings, entityId = entityId, flatten = flatten, returnResponse = returnResponse)
	}

	#' Create a TempFailedCustomer
	#'
	#' This function creates a TempFailedCustomer
	#' @param fieldNames The field values to give the created TempFailedCustomer. Each defaults to NULL.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{listSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Food Service
	#' @return A newly created TempFailedCustomer
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	createTempFailedCustomer <- function(Note = NULL, FullNameLFM = NULL, StudentNumber = NULL, StaffNumber = NULL, Balance = NULL, entityId = 1, schoolYearId = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment())

		body <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper()) %>% purrr::compact()

		createSkyObject(module = "FoodService", objectName = "TempFailedCustomer", body = list(DataObject = body), searchFields = append("TempFailedCustomerID", body %>% names()), entityId = entityId, schoolYearId = schoolYearId, flatten = flatten, returnResponse = returnResponse)
	}

	#' Modify a TempFailedCustomer
	#'
	#' This function modifies a TempFailedCustomer
	#' @param fieldNames The field values to give the modified TempFailedCustomer. Each defaults to NULL.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{listSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Food Service
	#' @return The modified TempFailedCustomer
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	modifyTempFailedCustomer <- function(TempFailedCustomerID, Note = NULL, FullNameLFM = NULL, StudentNumber = NULL, StaffNumber = NULL, Balance = NULL, entityId = 1, schoolYearId = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment())

		body <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper()) %>% purrr::compact()

		modifySkyObject(module = "FoodService", objectName = "TempFailedCustomer", objectId = TempFailedCustomerID, body = list(DataObject = body), searchFields = append("TempFailedCustomerID", body %>% names()), entityId = entityId, schoolYearId = schoolYearId, flatten = flatten, returnResponse = returnResponse)
	}

	#' List Purchases
	#'
	#' This function returns a dataframe or json object of Purchases
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given Purchases. Defaults to FALSE for all return fields which, for convenience, returns all fields for the Purchases.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object listed as text. Run \code{\link{getSchemaForObjects}}('Purchase') to get more field paths.
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
	#' @concept Food Service
	#' @return A list of Purchases
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	listPurchases <- function(searchConditionsList = NULL, PurchaseID = F, CustomerID = F, LineID = F, ItemID = F, PurchaseTime = F, IsVoid = F, IsSecondPurchase = F, Note = F, PurchaseDate = F, Amount = F, IsLastItemPurchaseForDay = F, IsPayorPurchase = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, ItemPriceID = F, CanBeVoided = F, LineCloseoutID = F, fieldPaths = NULL, searchConditionsGroupType = "And", searchSortFieldNamesList = NULL, searchSortFieldNamesDescendingList = NULL, entityId = 1, schoolYearId = NULL, page = 1, pageSize = 100000, flatten = T, returnResponse = F){

		params <- as.list(environment())

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		listSkyObjects(module = "FoodService", objectName = "Purchase", searchFields = searchFields %>% append(fieldPaths), page = page, pageSize = pageSize, SearchConditionsList = searchConditionsList, SearchConditionsGroupType = searchConditionsGroupType, SearchSortFieldNamesList = searchSortFieldNamesList, SearchSortFieldNamesDescendingList = searchSortFieldNamesDescendingList, entityId = entityId, schoolYearId = schoolYearId, flatten = flatten, returnResponse = returnResponse)
	}

	#' Get a Purchase
	#'
	#' This function returns a dataframe or json object of a Purchase
	#' @param PurchaseID The ID of the Purchase to return.
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given Purchase. Defaults to FALSE for all return fields which, for convenience, returns all fields for the Purchase.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object listed as text. Run \code{\link{getSchemaForObjects}}('Purchase') to get more field paths.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{listSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Food Service
	#' @return A dataframe or of Purchase
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	getPurchase <- function(PurchaseID, CustomerID = F, LineID = F, ItemID = F, PurchaseTime = F, IsVoid = F, IsSecondPurchase = F, Note = F, PurchaseDate = F, Amount = F, IsLastItemPurchaseForDay = F, IsPayorPurchase = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, ItemPriceID = F, CanBeVoided = F, LineCloseoutID = F, fieldPaths = NULL, entityId = 1, schoolYearId = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment()) %>% purrr::keep(names(.) != "PurchaseID")

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		getSkyObject(module = "FoodService", objectName = "Purchase", objectId = PurchaseID, searchFields = searchFields, entityId = entityId, flatten = flatten, returnResponse = returnResponse)
	}

	#' Delete a Purchase
	#'
	#' This function deletes a Purchase
	#' @param PurchaseID The ID of the Purchase to delete
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{listSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Food Service
	#' @return The PurchaseID of the deleted Purchase.
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	deletePurchase <- function(PurchaseID, ignoreWarnings = F, entityId = 1, schoolYearId = NULL, flatten = T, returnResponse = F){

		deleteSkyObject(module = "FoodService", objectName = "Purchase", objectId = PurchaseID, ignoreWarnings = ignoreWarnings, entityId = entityId, flatten = flatten, returnResponse = returnResponse)
	}

	#' Create a Purchase
	#'
	#' This function creates a Purchase
	#' @param fieldNames The field values to give the created Purchase. Each defaults to NULL.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{listSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Food Service
	#' @return A newly created Purchase
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	createPurchase <- function(CustomerID = NULL, LineID = NULL, ItemID = NULL, PurchaseTime = NULL, IsVoid = NULL, IsSecondPurchase = NULL, Note = NULL, ItemPriceID = NULL, LineCloseoutID = NULL, entityId = 1, schoolYearId = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment())

		body <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper()) %>% purrr::compact()

		createSkyObject(module = "FoodService", objectName = "Purchase", body = list(DataObject = body), searchFields = append("PurchaseID", body %>% names()), entityId = entityId, schoolYearId = schoolYearId, flatten = flatten, returnResponse = returnResponse)
	}

	#' Modify a Purchase
	#'
	#' This function modifies a Purchase
	#' @param fieldNames The field values to give the modified Purchase. Each defaults to NULL.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{listSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Food Service
	#' @return The modified Purchase
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	modifyPurchase <- function(PurchaseID, CustomerID = NULL, LineID = NULL, ItemID = NULL, PurchaseTime = NULL, IsVoid = NULL, IsSecondPurchase = NULL, Note = NULL, ItemPriceID = NULL, LineCloseoutID = NULL, entityId = 1, schoolYearId = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment())

		body <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper()) %>% purrr::compact()

		modifySkyObject(module = "FoodService", objectName = "Purchase", objectId = PurchaseID, body = list(DataObject = body), searchFields = append("PurchaseID", body %>% names()), entityId = entityId, schoolYearId = schoolYearId, flatten = flatten, returnResponse = returnResponse)
	}

	#' List PurchaseDetails
	#'
	#' This function returns a dataframe or json object of PurchaseDetails
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given PurchaseDetails. Defaults to FALSE for all return fields which, for convenience, returns all fields for the PurchaseDetails.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object listed as text. Run \code{\link{getSchemaForObjects}}('PurchaseDetail') to get more field paths.
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
	#' @concept Food Service
	#' @return A list of PurchaseDetails
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	listPurchaseDetails <- function(searchConditionsList = NULL, PurchaseDetailID = F, PurchaseID = F, PayorID = F, AccountingUpdateID = F, PaymentType = F, Status = F, PurchaseType = F, Amount = F, Note = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, ReconciledAfterSelectedBankRec = F, ReadOnlyOnBankRec = F, fieldPaths = NULL, searchConditionsGroupType = "And", searchSortFieldNamesList = NULL, searchSortFieldNamesDescendingList = NULL, entityId = 1, schoolYearId = NULL, page = 1, pageSize = 100000, flatten = T, returnResponse = F){

		params <- as.list(environment())

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		listSkyObjects(module = "FoodService", objectName = "PurchaseDetail", searchFields = searchFields %>% append(fieldPaths), page = page, pageSize = pageSize, SearchConditionsList = searchConditionsList, SearchConditionsGroupType = searchConditionsGroupType, SearchSortFieldNamesList = searchSortFieldNamesList, SearchSortFieldNamesDescendingList = searchSortFieldNamesDescendingList, entityId = entityId, schoolYearId = schoolYearId, flatten = flatten, returnResponse = returnResponse)
	}

	#' Get a PurchaseDetail
	#'
	#' This function returns a dataframe or json object of a PurchaseDetail
	#' @param PurchaseDetailID The ID of the PurchaseDetail to return.
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given PurchaseDetail. Defaults to FALSE for all return fields which, for convenience, returns all fields for the PurchaseDetail.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object listed as text. Run \code{\link{getSchemaForObjects}}('PurchaseDetail') to get more field paths.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{listSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Food Service
	#' @return A dataframe or of PurchaseDetail
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	getPurchaseDetail <- function(PurchaseDetailID, PurchaseID = F, PayorID = F, AccountingUpdateID = F, PaymentType = F, Status = F, PurchaseType = F, Amount = F, Note = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, ReconciledAfterSelectedBankRec = F, ReadOnlyOnBankRec = F, fieldPaths = NULL, entityId = 1, schoolYearId = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment()) %>% purrr::keep(names(.) != "PurchaseDetailID")

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		getSkyObject(module = "FoodService", objectName = "PurchaseDetail", objectId = PurchaseDetailID, searchFields = searchFields, entityId = entityId, flatten = flatten, returnResponse = returnResponse)
	}

	#' Delete a PurchaseDetail
	#'
	#' This function deletes a PurchaseDetail
	#' @param PurchaseDetailID The ID of the PurchaseDetail to delete
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{listSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Food Service
	#' @return The PurchaseDetailID of the deleted PurchaseDetail.
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	deletePurchaseDetail <- function(PurchaseDetailID, ignoreWarnings = F, entityId = 1, schoolYearId = NULL, flatten = T, returnResponse = F){

		deleteSkyObject(module = "FoodService", objectName = "PurchaseDetail", objectId = PurchaseDetailID, ignoreWarnings = ignoreWarnings, entityId = entityId, flatten = flatten, returnResponse = returnResponse)
	}

	#' Create a PurchaseDetail
	#'
	#' This function creates a PurchaseDetail
	#' @param fieldNames The field values to give the created PurchaseDetail. Each defaults to NULL.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{listSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Food Service
	#' @return A newly created PurchaseDetail
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	createPurchaseDetail <- function(PurchaseID = NULL, PayorID = NULL, AccountingUpdateID = NULL, PaymentType = NULL, Status = NULL, PurchaseType = NULL, Amount = NULL, Note = NULL, entityId = 1, schoolYearId = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment())

		body <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper()) %>% purrr::compact()

		createSkyObject(module = "FoodService", objectName = "PurchaseDetail", body = list(DataObject = body), searchFields = append("PurchaseDetailID", body %>% names()), entityId = entityId, schoolYearId = schoolYearId, flatten = flatten, returnResponse = returnResponse)
	}

	#' Modify a PurchaseDetail
	#'
	#' This function modifies a PurchaseDetail
	#' @param fieldNames The field values to give the modified PurchaseDetail. Each defaults to NULL.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{listSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Food Service
	#' @return The modified PurchaseDetail
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	modifyPurchaseDetail <- function(PurchaseDetailID, PurchaseID = NULL, PayorID = NULL, AccountingUpdateID = NULL, PaymentType = NULL, Status = NULL, PurchaseType = NULL, Amount = NULL, Note = NULL, entityId = 1, schoolYearId = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment())

		body <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper()) %>% purrr::compact()

		modifySkyObject(module = "FoodService", objectName = "PurchaseDetail", objectId = PurchaseDetailID, body = list(DataObject = body), searchFields = append("PurchaseDetailID", body %>% names()), entityId = entityId, schoolYearId = schoolYearId, flatten = flatten, returnResponse = returnResponse)
	}

	#' List AccountingExports
	#'
	#' This function returns a dataframe or json object of AccountingExports
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given AccountingExports. Defaults to FALSE for all return fields which, for convenience, returns all fields for the AccountingExports.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object listed as text. Run \code{\link{getSchemaForObjects}}('AccountingExport') to get more field paths.
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
	#' @concept Food Service
	#' @return A list of AccountingExports
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	listAccountingExports <- function(searchConditionsList = NULL, AccountingExportID = F, AccountingUpdateID = F, EntityID = F, MediaID = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, fieldPaths = NULL, searchConditionsGroupType = "And", searchSortFieldNamesList = NULL, searchSortFieldNamesDescendingList = NULL, entityId = 1, schoolYearId = NULL, page = 1, pageSize = 100000, flatten = T, returnResponse = F){

		params <- as.list(environment())

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		listSkyObjects(module = "FoodService", objectName = "AccountingExport", searchFields = searchFields %>% append(fieldPaths), page = page, pageSize = pageSize, SearchConditionsList = searchConditionsList, SearchConditionsGroupType = searchConditionsGroupType, SearchSortFieldNamesList = searchSortFieldNamesList, SearchSortFieldNamesDescendingList = searchSortFieldNamesDescendingList, entityId = entityId, schoolYearId = schoolYearId, flatten = flatten, returnResponse = returnResponse)
	}

	#' Get an AccountingExport
	#'
	#' This function returns a dataframe or json object of an AccountingExport
	#' @param AccountingExportID The ID of the AccountingExport to return.
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given AccountingExport. Defaults to FALSE for all return fields which, for convenience, returns all fields for the AccountingExport.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object listed as text. Run \code{\link{getSchemaForObjects}}('AccountingExport') to get more field paths.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{listSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Food Service
	#' @return A dataframe or of AccountingExport
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	getAccountingExport <- function(AccountingExportID, AccountingUpdateID = F, EntityID = F, MediaID = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, fieldPaths = NULL, entityId = 1, schoolYearId = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment()) %>% purrr::keep(names(.) != "AccountingExportID")

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		getSkyObject(module = "FoodService", objectName = "AccountingExport", objectId = AccountingExportID, searchFields = searchFields, entityId = entityId, flatten = flatten, returnResponse = returnResponse)
	}

	#' Delete an AccountingExport
	#'
	#' This function deletes an AccountingExport
	#' @param AccountingExportID The ID of the AccountingExport to delete
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{listSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Food Service
	#' @return The AccountingExportID of the deleted AccountingExport.
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	deleteAccountingExport <- function(AccountingExportID, ignoreWarnings = F, entityId = 1, schoolYearId = NULL, flatten = T, returnResponse = F){

		deleteSkyObject(module = "FoodService", objectName = "AccountingExport", objectId = AccountingExportID, ignoreWarnings = ignoreWarnings, entityId = entityId, flatten = flatten, returnResponse = returnResponse)
	}

	#' Create an AccountingExport
	#'
	#' This function creates an AccountingExport
	#' @param fieldNames The field values to give the created AccountingExport. Each defaults to NULL.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{listSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Food Service
	#' @return A newly created AccountingExport
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	createAccountingExport <- function(AccountingUpdateID = NULL, EntityID = NULL, MediaID = NULL, entityId = 1, schoolYearId = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment())

		body <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper()) %>% purrr::compact()

		createSkyObject(module = "FoodService", objectName = "AccountingExport", body = list(DataObject = body), searchFields = append("AccountingExportID", body %>% names()), entityId = entityId, schoolYearId = schoolYearId, flatten = flatten, returnResponse = returnResponse)
	}

	#' Modify an AccountingExport
	#'
	#' This function modifies an AccountingExport
	#' @param fieldNames The field values to give the modified AccountingExport. Each defaults to NULL.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{listSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Food Service
	#' @return The modified AccountingExport
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	modifyAccountingExport <- function(AccountingExportID, AccountingUpdateID = NULL, EntityID = NULL, MediaID = NULL, entityId = 1, schoolYearId = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment())

		body <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper()) %>% purrr::compact()

		modifySkyObject(module = "FoodService", objectName = "AccountingExport", objectId = AccountingExportID, body = list(DataObject = body), searchFields = append("AccountingExportID", body %>% names()), entityId = entityId, schoolYearId = schoolYearId, flatten = flatten, returnResponse = returnResponse)
	}

	#' List TempApplications
	#'
	#' This function returns a dataframe or json object of TempApplications
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given TempApplications. Defaults to FALSE for all return fields which, for convenience, returns all fields for the TempApplications.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object listed as text. Run \code{\link{getSchemaForObjects}}('TempApplication') to get more field paths.
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
	#' @concept Food Service
	#' @return A list of TempApplications
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	listTempApplications <- function(searchConditionsList = NULL, TempApplicationID = F, ApplicationNumber = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, ApplicationID = F, FullNameLFM = F, EffectiveDate = F, ExceptionMessage = F, HasExceptions = F, ExceptionStatus = F, fieldPaths = NULL, searchConditionsGroupType = "And", searchSortFieldNamesList = NULL, searchSortFieldNamesDescendingList = NULL, entityId = 1, schoolYearId = NULL, page = 1, pageSize = 100000, flatten = T, returnResponse = F){

		params <- as.list(environment())

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		listSkyObjects(module = "FoodService", objectName = "TempApplication", searchFields = searchFields %>% append(fieldPaths), page = page, pageSize = pageSize, SearchConditionsList = searchConditionsList, SearchConditionsGroupType = searchConditionsGroupType, SearchSortFieldNamesList = searchSortFieldNamesList, SearchSortFieldNamesDescendingList = searchSortFieldNamesDescendingList, entityId = entityId, schoolYearId = schoolYearId, flatten = flatten, returnResponse = returnResponse)
	}

	#' Get a TempApplication
	#'
	#' This function returns a dataframe or json object of a TempApplication
	#' @param TempApplicationID The ID of the TempApplication to return.
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given TempApplication. Defaults to FALSE for all return fields which, for convenience, returns all fields for the TempApplication.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object listed as text. Run \code{\link{getSchemaForObjects}}('TempApplication') to get more field paths.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{listSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Food Service
	#' @return A dataframe or of TempApplication
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	getTempApplication <- function(TempApplicationID, ApplicationNumber = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, ApplicationID = F, FullNameLFM = F, EffectiveDate = F, ExceptionMessage = F, HasExceptions = F, ExceptionStatus = F, fieldPaths = NULL, entityId = 1, schoolYearId = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment()) %>% purrr::keep(names(.) != "TempApplicationID")

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		getSkyObject(module = "FoodService", objectName = "TempApplication", objectId = TempApplicationID, searchFields = searchFields, entityId = entityId, flatten = flatten, returnResponse = returnResponse)
	}

	#' Delete a TempApplication
	#'
	#' This function deletes a TempApplication
	#' @param TempApplicationID The ID of the TempApplication to delete
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{listSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Food Service
	#' @return The TempApplicationID of the deleted TempApplication.
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	deleteTempApplication <- function(TempApplicationID, ignoreWarnings = F, entityId = 1, schoolYearId = NULL, flatten = T, returnResponse = F){

		deleteSkyObject(module = "FoodService", objectName = "TempApplication", objectId = TempApplicationID, ignoreWarnings = ignoreWarnings, entityId = entityId, flatten = flatten, returnResponse = returnResponse)
	}

	#' Create a TempApplication
	#'
	#' This function creates a TempApplication
	#' @param fieldNames The field values to give the created TempApplication. Each defaults to NULL.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{listSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Food Service
	#' @return A newly created TempApplication
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	createTempApplication <- function(ApplicationNumber = NULL, ApplicationID = NULL, FullNameLFM = NULL, EffectiveDate = NULL, ExceptionMessage = NULL, HasExceptions = NULL, ExceptionStatus = NULL, entityId = 1, schoolYearId = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment())

		body <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper()) %>% purrr::compact()

		createSkyObject(module = "FoodService", objectName = "TempApplication", body = list(DataObject = body), searchFields = append("TempApplicationID", body %>% names()), entityId = entityId, schoolYearId = schoolYearId, flatten = flatten, returnResponse = returnResponse)
	}

	#' Modify a TempApplication
	#'
	#' This function modifies a TempApplication
	#' @param fieldNames The field values to give the modified TempApplication. Each defaults to NULL.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{listSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Food Service
	#' @return The modified TempApplication
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	modifyTempApplication <- function(TempApplicationID, ApplicationNumber = NULL, ApplicationID = NULL, FullNameLFM = NULL, EffectiveDate = NULL, ExceptionMessage = NULL, HasExceptions = NULL, ExceptionStatus = NULL, entityId = 1, schoolYearId = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment())

		body <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper()) %>% purrr::compact()

		modifySkyObject(module = "FoodService", objectName = "TempApplication", objectId = TempApplicationID, body = list(DataObject = body), searchFields = append("TempApplicationID", body %>% names()), entityId = entityId, schoolYearId = schoolYearId, flatten = flatten, returnResponse = returnResponse)
	}

	#' List VerificationSelectionRunHistories
	#'
	#' This function returns a dataframe or json object of VerificationSelectionRunHistories
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given VerificationSelectionRunHistories. Defaults to FALSE for all return fields which, for convenience, returns all fields for the VerificationSelectionRunHistories.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object listed as text. Run \code{\link{getSchemaForObjects}}('VerificationSelectionRunHistory') to get more field paths.
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
	#' @concept Food Service
	#' @return A list of VerificationSelectionRunHistories
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	listVerificationSelectionRunHistories <- function(searchConditionsList = NULL, VerificationSelectionRunHistoryID = F, SchoolYearID = F, DistrictID = F, NumberOfAvailableApplications = F, NumberOfSelectedApplications = F, ReplaceLastRanDate = F, IsMostRecentForSchoolYear = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, SampleSelectionMethod = F, SelectionTime = F, fieldPaths = NULL, searchConditionsGroupType = "And", searchSortFieldNamesList = NULL, searchSortFieldNamesDescendingList = NULL, entityId = 1, schoolYearId = NULL, page = 1, pageSize = 100000, flatten = T, returnResponse = F){

		params <- as.list(environment())

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		listSkyObjects(module = "FoodService", objectName = "VerificationSelectionRunHistory", searchFields = searchFields %>% append(fieldPaths), page = page, pageSize = pageSize, SearchConditionsList = searchConditionsList, SearchConditionsGroupType = searchConditionsGroupType, SearchSortFieldNamesList = searchSortFieldNamesList, SearchSortFieldNamesDescendingList = searchSortFieldNamesDescendingList, entityId = entityId, schoolYearId = schoolYearId, flatten = flatten, returnResponse = returnResponse)
	}

	#' Get a VerificationSelectionRunHistory
	#'
	#' This function returns a dataframe or json object of a VerificationSelectionRunHistory
	#' @param VerificationSelectionRunHistoryID The ID of the VerificationSelectionRunHistory to return.
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given VerificationSelectionRunHistory. Defaults to FALSE for all return fields which, for convenience, returns all fields for the VerificationSelectionRunHistory.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object listed as text. Run \code{\link{getSchemaForObjects}}('VerificationSelectionRunHistory') to get more field paths.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{listSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Food Service
	#' @return A dataframe or of VerificationSelectionRunHistory
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	getVerificationSelectionRunHistory <- function(VerificationSelectionRunHistoryID, SchoolYearID = F, DistrictID = F, NumberOfAvailableApplications = F, NumberOfSelectedApplications = F, ReplaceLastRanDate = F, IsMostRecentForSchoolYear = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, SampleSelectionMethod = F, SelectionTime = F, fieldPaths = NULL, entityId = 1, schoolYearId = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment()) %>% purrr::keep(names(.) != "VerificationSelectionRunHistoryID")

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		getSkyObject(module = "FoodService", objectName = "VerificationSelectionRunHistory", objectId = VerificationSelectionRunHistoryID, searchFields = searchFields, entityId = entityId, flatten = flatten, returnResponse = returnResponse)
	}

	#' Delete a VerificationSelectionRunHistory
	#'
	#' This function deletes a VerificationSelectionRunHistory
	#' @param VerificationSelectionRunHistoryID The ID of the VerificationSelectionRunHistory to delete
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{listSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Food Service
	#' @return The VerificationSelectionRunHistoryID of the deleted VerificationSelectionRunHistory.
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	deleteVerificationSelectionRunHistory <- function(VerificationSelectionRunHistoryID, ignoreWarnings = F, entityId = 1, schoolYearId = NULL, flatten = T, returnResponse = F){

		deleteSkyObject(module = "FoodService", objectName = "VerificationSelectionRunHistory", objectId = VerificationSelectionRunHistoryID, ignoreWarnings = ignoreWarnings, entityId = entityId, flatten = flatten, returnResponse = returnResponse)
	}

	#' Create a VerificationSelectionRunHistory
	#'
	#' This function creates a VerificationSelectionRunHistory
	#' @param fieldNames The field values to give the created VerificationSelectionRunHistory. Each defaults to NULL.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{listSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Food Service
	#' @return A newly created VerificationSelectionRunHistory
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	createVerificationSelectionRunHistory <- function(SchoolYearID = NULL, DistrictID = NULL, NumberOfAvailableApplications = NULL, NumberOfSelectedApplications = NULL, ReplaceLastRanDate = NULL, SampleSelectionMethod = NULL, SelectionTime = NULL, entityId = 1, schoolYearId = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment())

		body <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper()) %>% purrr::compact()

		createSkyObject(module = "FoodService", objectName = "VerificationSelectionRunHistory", body = list(DataObject = body), searchFields = append("VerificationSelectionRunHistoryID", body %>% names()), entityId = entityId, schoolYearId = schoolYearId, flatten = flatten, returnResponse = returnResponse)
	}

	#' Modify a VerificationSelectionRunHistory
	#'
	#' This function modifies a VerificationSelectionRunHistory
	#' @param fieldNames The field values to give the modified VerificationSelectionRunHistory. Each defaults to NULL.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{listSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Food Service
	#' @return The modified VerificationSelectionRunHistory
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	modifyVerificationSelectionRunHistory <- function(VerificationSelectionRunHistoryID, SchoolYearID = NULL, DistrictID = NULL, NumberOfAvailableApplications = NULL, NumberOfSelectedApplications = NULL, ReplaceLastRanDate = NULL, SampleSelectionMethod = NULL, SelectionTime = NULL, entityId = 1, schoolYearId = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment())

		body <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper()) %>% purrr::compact()

		modifySkyObject(module = "FoodService", objectName = "VerificationSelectionRunHistory", objectId = VerificationSelectionRunHistoryID, body = list(DataObject = body), searchFields = append("VerificationSelectionRunHistoryID", body %>% names()), entityId = entityId, schoolYearId = schoolYearId, flatten = flatten, returnResponse = returnResponse)
	}

	#' List MonthlyClaimExclusions
	#'
	#' This function returns a dataframe or json object of MonthlyClaimExclusions
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given MonthlyClaimExclusions. Defaults to FALSE for all return fields which, for convenience, returns all fields for the MonthlyClaimExclusions.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object listed as text. Run \code{\link{getSchemaForObjects}}('MonthlyClaimExclusion') to get more field paths.
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
	#' @concept Food Service
	#' @return A list of MonthlyClaimExclusions
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	listMonthlyClaimExclusions <- function(searchConditionsList = NULL, MonthlyClaimExclusionID = F, CustomerID = F, StartDate = F, EndDate = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, ExcludeFromBreakfast = F, ExcludeFromLunch = F, fieldPaths = NULL, searchConditionsGroupType = "And", searchSortFieldNamesList = NULL, searchSortFieldNamesDescendingList = NULL, entityId = 1, schoolYearId = NULL, page = 1, pageSize = 100000, flatten = T, returnResponse = F){

		params <- as.list(environment())

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		listSkyObjects(module = "FoodService", objectName = "MonthlyClaimExclusion", searchFields = searchFields %>% append(fieldPaths), page = page, pageSize = pageSize, SearchConditionsList = searchConditionsList, SearchConditionsGroupType = searchConditionsGroupType, SearchSortFieldNamesList = searchSortFieldNamesList, SearchSortFieldNamesDescendingList = searchSortFieldNamesDescendingList, entityId = entityId, schoolYearId = schoolYearId, flatten = flatten, returnResponse = returnResponse)
	}

	#' Get a MonthlyClaimExclusion
	#'
	#' This function returns a dataframe or json object of a MonthlyClaimExclusion
	#' @param MonthlyClaimExclusionID The ID of the MonthlyClaimExclusion to return.
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given MonthlyClaimExclusion. Defaults to FALSE for all return fields which, for convenience, returns all fields for the MonthlyClaimExclusion.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object listed as text. Run \code{\link{getSchemaForObjects}}('MonthlyClaimExclusion') to get more field paths.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{listSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Food Service
	#' @return A dataframe or of MonthlyClaimExclusion
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	getMonthlyClaimExclusion <- function(MonthlyClaimExclusionID, CustomerID = F, StartDate = F, EndDate = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, ExcludeFromBreakfast = F, ExcludeFromLunch = F, fieldPaths = NULL, entityId = 1, schoolYearId = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment()) %>% purrr::keep(names(.) != "MonthlyClaimExclusionID")

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		getSkyObject(module = "FoodService", objectName = "MonthlyClaimExclusion", objectId = MonthlyClaimExclusionID, searchFields = searchFields, entityId = entityId, flatten = flatten, returnResponse = returnResponse)
	}

	#' Delete a MonthlyClaimExclusion
	#'
	#' This function deletes a MonthlyClaimExclusion
	#' @param MonthlyClaimExclusionID The ID of the MonthlyClaimExclusion to delete
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{listSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Food Service
	#' @return The MonthlyClaimExclusionID of the deleted MonthlyClaimExclusion.
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	deleteMonthlyClaimExclusion <- function(MonthlyClaimExclusionID, ignoreWarnings = F, entityId = 1, schoolYearId = NULL, flatten = T, returnResponse = F){

		deleteSkyObject(module = "FoodService", objectName = "MonthlyClaimExclusion", objectId = MonthlyClaimExclusionID, ignoreWarnings = ignoreWarnings, entityId = entityId, flatten = flatten, returnResponse = returnResponse)
	}

	#' Create a MonthlyClaimExclusion
	#'
	#' This function creates a MonthlyClaimExclusion
	#' @param fieldNames The field values to give the created MonthlyClaimExclusion. Each defaults to NULL.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{listSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Food Service
	#' @return A newly created MonthlyClaimExclusion
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	createMonthlyClaimExclusion <- function(CustomerID = NULL, StartDate = NULL, EndDate = NULL, ExcludeFromBreakfast = NULL, ExcludeFromLunch = NULL, entityId = 1, schoolYearId = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment())

		body <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper()) %>% purrr::compact()

		createSkyObject(module = "FoodService", objectName = "MonthlyClaimExclusion", body = list(DataObject = body), searchFields = append("MonthlyClaimExclusionID", body %>% names()), entityId = entityId, schoolYearId = schoolYearId, flatten = flatten, returnResponse = returnResponse)
	}

	#' Modify a MonthlyClaimExclusion
	#'
	#' This function modifies a MonthlyClaimExclusion
	#' @param fieldNames The field values to give the modified MonthlyClaimExclusion. Each defaults to NULL.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{listSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Food Service
	#' @return The modified MonthlyClaimExclusion
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	modifyMonthlyClaimExclusion <- function(MonthlyClaimExclusionID, CustomerID = NULL, StartDate = NULL, EndDate = NULL, ExcludeFromBreakfast = NULL, ExcludeFromLunch = NULL, entityId = 1, schoolYearId = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment())

		body <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper()) %>% purrr::compact()

		modifySkyObject(module = "FoodService", objectName = "MonthlyClaimExclusion", objectId = MonthlyClaimExclusionID, body = list(DataObject = body), searchFields = append("MonthlyClaimExclusionID", body %>% names()), entityId = entityId, schoolYearId = schoolYearId, flatten = flatten, returnResponse = returnResponse)
	}

	#' List VerificationSelectionRunHistorySchools
	#'
	#' This function returns a dataframe or json object of VerificationSelectionRunHistorySchools
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given VerificationSelectionRunHistorySchools. Defaults to FALSE for all return fields which, for convenience, returns all fields for the VerificationSelectionRunHistorySchools.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object listed as text. Run \code{\link{getSchemaForObjects}}('VerificationSelectionRunHistorySchool') to get more field paths.
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
	#' @concept Food Service
	#' @return A list of VerificationSelectionRunHistorySchools
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	listVerificationSelectionRunHistorySchools <- function(searchConditionsList = NULL, VerificationSelectionRunHistorySchoolID = F, SchoolID = F, VerificationSelectionRunHistoryID = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, fieldPaths = NULL, searchConditionsGroupType = "And", searchSortFieldNamesList = NULL, searchSortFieldNamesDescendingList = NULL, entityId = 1, schoolYearId = NULL, page = 1, pageSize = 100000, flatten = T, returnResponse = F){

		params <- as.list(environment())

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		listSkyObjects(module = "FoodService", objectName = "VerificationSelectionRunHistorySchool", searchFields = searchFields %>% append(fieldPaths), page = page, pageSize = pageSize, SearchConditionsList = searchConditionsList, SearchConditionsGroupType = searchConditionsGroupType, SearchSortFieldNamesList = searchSortFieldNamesList, SearchSortFieldNamesDescendingList = searchSortFieldNamesDescendingList, entityId = entityId, schoolYearId = schoolYearId, flatten = flatten, returnResponse = returnResponse)
	}

	#' Get a VerificationSelectionRunHistorySchool
	#'
	#' This function returns a dataframe or json object of a VerificationSelectionRunHistorySchool
	#' @param VerificationSelectionRunHistorySchoolID The ID of the VerificationSelectionRunHistorySchool to return.
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given VerificationSelectionRunHistorySchool. Defaults to FALSE for all return fields which, for convenience, returns all fields for the VerificationSelectionRunHistorySchool.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object listed as text. Run \code{\link{getSchemaForObjects}}('VerificationSelectionRunHistorySchool') to get more field paths.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{listSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Food Service
	#' @return A dataframe or of VerificationSelectionRunHistorySchool
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	getVerificationSelectionRunHistorySchool <- function(VerificationSelectionRunHistorySchoolID, SchoolID = F, VerificationSelectionRunHistoryID = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, fieldPaths = NULL, entityId = 1, schoolYearId = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment()) %>% purrr::keep(names(.) != "VerificationSelectionRunHistorySchoolID")

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		getSkyObject(module = "FoodService", objectName = "VerificationSelectionRunHistorySchool", objectId = VerificationSelectionRunHistorySchoolID, searchFields = searchFields, entityId = entityId, flatten = flatten, returnResponse = returnResponse)
	}

	#' Delete a VerificationSelectionRunHistorySchool
	#'
	#' This function deletes a VerificationSelectionRunHistorySchool
	#' @param VerificationSelectionRunHistorySchoolID The ID of the VerificationSelectionRunHistorySchool to delete
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{listSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Food Service
	#' @return The VerificationSelectionRunHistorySchoolID of the deleted VerificationSelectionRunHistorySchool.
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	deleteVerificationSelectionRunHistorySchool <- function(VerificationSelectionRunHistorySchoolID, ignoreWarnings = F, entityId = 1, schoolYearId = NULL, flatten = T, returnResponse = F){

		deleteSkyObject(module = "FoodService", objectName = "VerificationSelectionRunHistorySchool", objectId = VerificationSelectionRunHistorySchoolID, ignoreWarnings = ignoreWarnings, entityId = entityId, flatten = flatten, returnResponse = returnResponse)
	}

	#' Create a VerificationSelectionRunHistorySchool
	#'
	#' This function creates a VerificationSelectionRunHistorySchool
	#' @param fieldNames The field values to give the created VerificationSelectionRunHistorySchool. Each defaults to NULL.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{listSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Food Service
	#' @return A newly created VerificationSelectionRunHistorySchool
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	createVerificationSelectionRunHistorySchool <- function(SchoolID = NULL, VerificationSelectionRunHistoryID = NULL, entityId = 1, schoolYearId = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment())

		body <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper()) %>% purrr::compact()

		createSkyObject(module = "FoodService", objectName = "VerificationSelectionRunHistorySchool", body = list(DataObject = body), searchFields = append("VerificationSelectionRunHistorySchoolID", body %>% names()), entityId = entityId, schoolYearId = schoolYearId, flatten = flatten, returnResponse = returnResponse)
	}

	#' Modify a VerificationSelectionRunHistorySchool
	#'
	#' This function modifies a VerificationSelectionRunHistorySchool
	#' @param fieldNames The field values to give the modified VerificationSelectionRunHistorySchool. Each defaults to NULL.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{listSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Food Service
	#' @return The modified VerificationSelectionRunHistorySchool
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	modifyVerificationSelectionRunHistorySchool <- function(VerificationSelectionRunHistorySchoolID, SchoolID = NULL, VerificationSelectionRunHistoryID = NULL, entityId = 1, schoolYearId = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment())

		body <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper()) %>% purrr::compact()

		modifySkyObject(module = "FoodService", objectName = "VerificationSelectionRunHistorySchool", objectId = VerificationSelectionRunHistorySchoolID, body = list(DataObject = body), searchFields = append("VerificationSelectionRunHistorySchoolID", body %>% names()), entityId = entityId, schoolYearId = schoolYearId, flatten = flatten, returnResponse = returnResponse)
	}

	#' List TempDirectCertifications
	#'
	#' This function returns a dataframe or json object of TempDirectCertifications
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given TempDirectCertifications. Defaults to FALSE for all return fields which, for convenience, returns all fields for the TempDirectCertifications.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object listed as text. Run \code{\link{getSchemaForObjects}}('TempDirectCertification') to get more field paths.
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
	#' @concept Food Service
	#' @return A list of TempDirectCertifications
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	listTempDirectCertifications <- function(searchConditionsList = NULL, TempDirectCertificationID = F, LastName = F, FirstName = F, MiddleName = F, Birthdate = F, GenderCode = F, StateSpecificStudentNumber = F, StateMatchCode = F, SourceID = F, AssistanceSourceCode = F, CaseNumber = F, FreeOrReducedIdentifier = F, DefaultEntity = F, GraduationYear = F, EffectiveDate = F, DistrictName = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, AssistanceSourceID = F, FosterChild = F, SnapParticipant = F, EligibilityCategory = F, EligibilityCategoryID = F, StateFreeReducedMealReasonWAID = F, StateLayoutNumber = F, ExceptionMessage = F, StudentNumber = F, fieldPaths = NULL, searchConditionsGroupType = "And", searchSortFieldNamesList = NULL, searchSortFieldNamesDescendingList = NULL, entityId = 1, schoolYearId = NULL, page = 1, pageSize = 100000, flatten = T, returnResponse = F){

		params <- as.list(environment())

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		listSkyObjects(module = "FoodService", objectName = "TempDirectCertification", searchFields = searchFields %>% append(fieldPaths), page = page, pageSize = pageSize, SearchConditionsList = searchConditionsList, SearchConditionsGroupType = searchConditionsGroupType, SearchSortFieldNamesList = searchSortFieldNamesList, SearchSortFieldNamesDescendingList = searchSortFieldNamesDescendingList, entityId = entityId, schoolYearId = schoolYearId, flatten = flatten, returnResponse = returnResponse)
	}

	#' Get a TempDirectCertification
	#'
	#' This function returns a dataframe or json object of a TempDirectCertification
	#' @param TempDirectCertificationID The ID of the TempDirectCertification to return.
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given TempDirectCertification. Defaults to FALSE for all return fields which, for convenience, returns all fields for the TempDirectCertification.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object listed as text. Run \code{\link{getSchemaForObjects}}('TempDirectCertification') to get more field paths.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{listSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Food Service
	#' @return A dataframe or of TempDirectCertification
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	getTempDirectCertification <- function(TempDirectCertificationID, LastName = F, FirstName = F, MiddleName = F, Birthdate = F, GenderCode = F, StateSpecificStudentNumber = F, StateMatchCode = F, SourceID = F, AssistanceSourceCode = F, CaseNumber = F, FreeOrReducedIdentifier = F, DefaultEntity = F, GraduationYear = F, EffectiveDate = F, DistrictName = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, AssistanceSourceID = F, FosterChild = F, SnapParticipant = F, EligibilityCategory = F, EligibilityCategoryID = F, StateFreeReducedMealReasonWAID = F, StateLayoutNumber = F, ExceptionMessage = F, StudentNumber = F, fieldPaths = NULL, entityId = 1, schoolYearId = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment()) %>% purrr::keep(names(.) != "TempDirectCertificationID")

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		getSkyObject(module = "FoodService", objectName = "TempDirectCertification", objectId = TempDirectCertificationID, searchFields = searchFields, entityId = entityId, flatten = flatten, returnResponse = returnResponse)
	}

	#' Delete a TempDirectCertification
	#'
	#' This function deletes a TempDirectCertification
	#' @param TempDirectCertificationID The ID of the TempDirectCertification to delete
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{listSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Food Service
	#' @return The TempDirectCertificationID of the deleted TempDirectCertification.
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	deleteTempDirectCertification <- function(TempDirectCertificationID, ignoreWarnings = F, entityId = 1, schoolYearId = NULL, flatten = T, returnResponse = F){

		deleteSkyObject(module = "FoodService", objectName = "TempDirectCertification", objectId = TempDirectCertificationID, ignoreWarnings = ignoreWarnings, entityId = entityId, flatten = flatten, returnResponse = returnResponse)
	}

	#' Create a TempDirectCertification
	#'
	#' This function creates a TempDirectCertification
	#' @param fieldNames The field values to give the created TempDirectCertification. Each defaults to NULL.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{listSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Food Service
	#' @return A newly created TempDirectCertification
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	createTempDirectCertification <- function(LastName = NULL, FirstName = NULL, MiddleName = NULL, Birthdate = NULL, GenderCode = NULL, StateSpecificStudentNumber = NULL, StateMatchCode = NULL, SourceID = NULL, AssistanceSourceCode = NULL, CaseNumber = NULL, FreeOrReducedIdentifier = NULL, DefaultEntity = NULL, GraduationYear = NULL, EffectiveDate = NULL, DistrictName = NULL, AssistanceSourceID = NULL, FosterChild = NULL, SnapParticipant = NULL, EligibilityCategory = NULL, StateLayoutNumber = NULL, ExceptionMessage = NULL, StudentNumber = NULL, entityId = 1, schoolYearId = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment())

		body <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper()) %>% purrr::compact()

		createSkyObject(module = "FoodService", objectName = "TempDirectCertification", body = list(DataObject = body), searchFields = append("TempDirectCertificationID", body %>% names()), entityId = entityId, schoolYearId = schoolYearId, flatten = flatten, returnResponse = returnResponse)
	}

	#' Modify a TempDirectCertification
	#'
	#' This function modifies a TempDirectCertification
	#' @param fieldNames The field values to give the modified TempDirectCertification. Each defaults to NULL.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{listSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Food Service
	#' @return The modified TempDirectCertification
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	modifyTempDirectCertification <- function(TempDirectCertificationID, LastName = NULL, FirstName = NULL, MiddleName = NULL, Birthdate = NULL, GenderCode = NULL, StateSpecificStudentNumber = NULL, StateMatchCode = NULL, SourceID = NULL, AssistanceSourceCode = NULL, CaseNumber = NULL, FreeOrReducedIdentifier = NULL, DefaultEntity = NULL, GraduationYear = NULL, EffectiveDate = NULL, DistrictName = NULL, AssistanceSourceID = NULL, FosterChild = NULL, SnapParticipant = NULL, EligibilityCategory = NULL, StateLayoutNumber = NULL, ExceptionMessage = NULL, StudentNumber = NULL, entityId = 1, schoolYearId = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment())

		body <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper()) %>% purrr::compact()

		modifySkyObject(module = "FoodService", objectName = "TempDirectCertification", objectId = TempDirectCertificationID, body = list(DataObject = body), searchFields = append("TempDirectCertificationID", body %>% names()), entityId = entityId, schoolYearId = schoolYearId, flatten = flatten, returnResponse = returnResponse)
	}

	#' List ReimbursementMonths
	#'
	#' This function returns a dataframe or json object of ReimbursementMonths
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given ReimbursementMonths. Defaults to FALSE for all return fields which, for convenience, returns all fields for the ReimbursementMonths.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object listed as text. Run \code{\link{getSchemaForObjects}}('ReimbursementMonth') to get more field paths.
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
	#' @concept Food Service
	#' @return A list of ReimbursementMonths
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	listReimbursementMonths <- function(searchConditionsList = NULL, ReimbursementMonthID = F, SkywardID = F, SkywardHash = F, ReportingYear = F, NumericYear = F, DisplayOrder = F, NumericMonth = F, Description = F, ServedTotalFree = F, ServedTotalReduced = F, ServedTotalPaid = F, ServedTotalTotal = F, ServedTotalOther = F, ApprovedTotalFree = F, ApprovedTotalReduced = F, ServedADPFree = F, ServedADPReduced = F, ServedADPPaid = F, ServedADPTotal = F, ServedADPOther = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, OperatingDays = F, ADA = F, AttendanceFactor = F, HighestExpectedMealsFree = F, HighestExpectedMealsReduced = F, HighestExpectedMealsPaid = F, HighestExpectedMealsTotal = F, PaidEligibleTotal = F, fieldPaths = NULL, searchConditionsGroupType = "And", searchSortFieldNamesList = NULL, searchSortFieldNamesDescendingList = NULL, entityId = 1, schoolYearId = NULL, page = 1, pageSize = 100000, flatten = T, returnResponse = F){

		params <- as.list(environment())

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		listSkyObjects(module = "FoodService", objectName = "ReimbursementMonth", searchFields = searchFields %>% append(fieldPaths), page = page, pageSize = pageSize, SearchConditionsList = searchConditionsList, SearchConditionsGroupType = searchConditionsGroupType, SearchSortFieldNamesList = searchSortFieldNamesList, SearchSortFieldNamesDescendingList = searchSortFieldNamesDescendingList, entityId = entityId, schoolYearId = schoolYearId, flatten = flatten, returnResponse = returnResponse)
	}

	#' Get a ReimbursementMonth
	#'
	#' This function returns a dataframe or json object of a ReimbursementMonth
	#' @param ReimbursementMonthID The ID of the ReimbursementMonth to return.
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given ReimbursementMonth. Defaults to FALSE for all return fields which, for convenience, returns all fields for the ReimbursementMonth.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object listed as text. Run \code{\link{getSchemaForObjects}}('ReimbursementMonth') to get more field paths.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{listSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Food Service
	#' @return A dataframe or of ReimbursementMonth
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	getReimbursementMonth <- function(ReimbursementMonthID, SkywardID = F, SkywardHash = F, ReportingYear = F, NumericYear = F, DisplayOrder = F, NumericMonth = F, Description = F, ServedTotalFree = F, ServedTotalReduced = F, ServedTotalPaid = F, ServedTotalTotal = F, ServedTotalOther = F, ApprovedTotalFree = F, ApprovedTotalReduced = F, ServedADPFree = F, ServedADPReduced = F, ServedADPPaid = F, ServedADPTotal = F, ServedADPOther = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, OperatingDays = F, ADA = F, AttendanceFactor = F, HighestExpectedMealsFree = F, HighestExpectedMealsReduced = F, HighestExpectedMealsPaid = F, HighestExpectedMealsTotal = F, PaidEligibleTotal = F, fieldPaths = NULL, entityId = 1, schoolYearId = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment()) %>% purrr::keep(names(.) != "ReimbursementMonthID")

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		getSkyObject(module = "FoodService", objectName = "ReimbursementMonth", objectId = ReimbursementMonthID, searchFields = searchFields, entityId = entityId, flatten = flatten, returnResponse = returnResponse)
	}

	#' Delete a ReimbursementMonth
	#'
	#' This function deletes a ReimbursementMonth
	#' @param ReimbursementMonthID The ID of the ReimbursementMonth to delete
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{listSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Food Service
	#' @return The ReimbursementMonthID of the deleted ReimbursementMonth.
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	deleteReimbursementMonth <- function(ReimbursementMonthID, ignoreWarnings = F, entityId = 1, schoolYearId = NULL, flatten = T, returnResponse = F){

		deleteSkyObject(module = "FoodService", objectName = "ReimbursementMonth", objectId = ReimbursementMonthID, ignoreWarnings = ignoreWarnings, entityId = entityId, flatten = flatten, returnResponse = returnResponse)
	}

	#' Create a ReimbursementMonth
	#'
	#' This function creates a ReimbursementMonth
	#' @param fieldNames The field values to give the created ReimbursementMonth. Each defaults to NULL.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{listSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Food Service
	#' @return A newly created ReimbursementMonth
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	createReimbursementMonth <- function(NumericYear = NULL, DisplayOrder = NULL, NumericMonth = NULL, Description = NULL, entityId = 1, schoolYearId = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment())

		body <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper()) %>% purrr::compact()

		createSkyObject(module = "FoodService", objectName = "ReimbursementMonth", body = list(DataObject = body), searchFields = append("ReimbursementMonthID", body %>% names()), entityId = entityId, schoolYearId = schoolYearId, flatten = flatten, returnResponse = returnResponse)
	}

	#' Modify a ReimbursementMonth
	#'
	#' This function modifies a ReimbursementMonth
	#' @param fieldNames The field values to give the modified ReimbursementMonth. Each defaults to NULL.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{listSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Food Service
	#' @return The modified ReimbursementMonth
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	modifyReimbursementMonth <- function(ReimbursementMonthID, NumericYear = NULL, DisplayOrder = NULL, NumericMonth = NULL, Description = NULL, entityId = 1, schoolYearId = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment())

		body <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper()) %>% purrr::compact()

		modifySkyObject(module = "FoodService", objectName = "ReimbursementMonth", objectId = ReimbursementMonthID, body = list(DataObject = body), searchFields = append("ReimbursementMonthID", body %>% names()), entityId = entityId, schoolYearId = schoolYearId, flatten = flatten, returnResponse = returnResponse)
	}

	#' List ReimbursementMonthlyClaims
	#'
	#' This function returns a dataframe or json object of ReimbursementMonthlyClaims
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given ReimbursementMonthlyClaims. Defaults to FALSE for all return fields which, for convenience, returns all fields for the ReimbursementMonthlyClaims.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object listed as text. Run \code{\link{getSchemaForObjects}}('ReimbursementMonthlyClaim') to get more field paths.
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
	#' @concept Food Service
	#' @return A list of ReimbursementMonthlyClaims
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	listReimbursementMonthlyClaims <- function(searchConditionsList = NULL, DistrictID = F, EntityID = F, ReimbursementMonthID = F, NumericDay = F, MealType = F, ServedCountFree = F, ServedCountReduced = F, ServedCountPaid = F, ServedCountTotal = F, ServedCountOther = F, IsAdultPurchase = F, SchoolID = F, fieldPaths = NULL, searchConditionsGroupType = "And", searchSortFieldNamesList = NULL, searchSortFieldNamesDescendingList = NULL, entityId = 1, schoolYearId = NULL, page = 1, pageSize = 100000, flatten = T, returnResponse = F){

		params <- as.list(environment())

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		listSkyObjects(module = "FoodService", objectName = "ReimbursementMonthlyClaim", searchFields = searchFields %>% append(fieldPaths), page = page, pageSize = pageSize, SearchConditionsList = searchConditionsList, SearchConditionsGroupType = searchConditionsGroupType, SearchSortFieldNamesList = searchSortFieldNamesList, SearchSortFieldNamesDescendingList = searchSortFieldNamesDescendingList, entityId = entityId, schoolYearId = schoolYearId, flatten = flatten, returnResponse = returnResponse)
	}

	#' Get a ReimbursementMonthlyClaim
	#'
	#' This function returns a dataframe or json object of a ReimbursementMonthlyClaim
	#' @param ReimbursementMonthlyClaimID The ID of the ReimbursementMonthlyClaim to return.
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given ReimbursementMonthlyClaim. Defaults to FALSE for all return fields which, for convenience, returns all fields for the ReimbursementMonthlyClaim.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object listed as text. Run \code{\link{getSchemaForObjects}}('ReimbursementMonthlyClaim') to get more field paths.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{listSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Food Service
	#' @return A dataframe or of ReimbursementMonthlyClaim
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	getReimbursementMonthlyClaim <- function(ReimbursementMonthlyClaimID, DistrictID = F, EntityID = F, ReimbursementMonthID = F, NumericDay = F, MealType = F, ServedCountFree = F, ServedCountReduced = F, ServedCountPaid = F, ServedCountTotal = F, ServedCountOther = F, IsAdultPurchase = F, SchoolID = F, fieldPaths = NULL, entityId = 1, schoolYearId = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment()) %>% purrr::keep(names(.) != "ReimbursementMonthlyClaimID")

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		getSkyObject(module = "FoodService", objectName = "ReimbursementMonthlyClaim", objectId = ReimbursementMonthlyClaimID, searchFields = searchFields, entityId = entityId, flatten = flatten, returnResponse = returnResponse)
	}

	#' Delete a ReimbursementMonthlyClaim
	#'
	#' This function deletes a ReimbursementMonthlyClaim
	#' @param ReimbursementMonthlyClaimID The ID of the ReimbursementMonthlyClaim to delete
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{listSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Food Service
	#' @return The ReimbursementMonthlyClaimID of the deleted ReimbursementMonthlyClaim.
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	deleteReimbursementMonthlyClaim <- function(ReimbursementMonthlyClaimID, ignoreWarnings = F, entityId = 1, schoolYearId = NULL, flatten = T, returnResponse = F){

		deleteSkyObject(module = "FoodService", objectName = "ReimbursementMonthlyClaim", objectId = ReimbursementMonthlyClaimID, ignoreWarnings = ignoreWarnings, entityId = entityId, flatten = flatten, returnResponse = returnResponse)
	}

	#' Create a ReimbursementMonthlyClaim
	#'
	#' This function creates a ReimbursementMonthlyClaim
	#' @param fieldNames The field values to give the created ReimbursementMonthlyClaim. Each defaults to NULL.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{listSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Food Service
	#' @return A newly created ReimbursementMonthlyClaim
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	createReimbursementMonthlyClaim <- function(EntityID = NULL, ReimbursementMonthID = NULL, NumericDay = NULL, MealType = NULL, ServedCountFree = NULL, ServedCountReduced = NULL, ServedCountPaid = NULL, ServedCountTotal = NULL, ServedCountOther = NULL, IsAdultPurchase = NULL, SchoolID = NULL, entityId = 1, schoolYearId = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment())

		body <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper()) %>% purrr::compact()

		createSkyObject(module = "FoodService", objectName = "ReimbursementMonthlyClaim", body = list(DataObject = body), searchFields = append("ReimbursementMonthlyClaimID", body %>% names()), entityId = entityId, schoolYearId = schoolYearId, flatten = flatten, returnResponse = returnResponse)
	}

	#' Modify a ReimbursementMonthlyClaim
	#'
	#' This function modifies a ReimbursementMonthlyClaim
	#' @param fieldNames The field values to give the modified ReimbursementMonthlyClaim. Each defaults to NULL.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{listSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Food Service
	#' @return The modified ReimbursementMonthlyClaim
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	modifyReimbursementMonthlyClaim <- function(ReimbursementMonthlyClaimID, EntityID = NULL, ReimbursementMonthID = NULL, NumericDay = NULL, MealType = NULL, ServedCountFree = NULL, ServedCountReduced = NULL, ServedCountPaid = NULL, ServedCountTotal = NULL, ServedCountOther = NULL, IsAdultPurchase = NULL, SchoolID = NULL, entityId = 1, schoolYearId = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment())

		body <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper()) %>% purrr::compact()

		modifySkyObject(module = "FoodService", objectName = "ReimbursementMonthlyClaim", objectId = ReimbursementMonthlyClaimID, body = list(DataObject = body), searchFields = append("ReimbursementMonthlyClaimID", body %>% names()), entityId = entityId, schoolYearId = schoolYearId, flatten = flatten, returnResponse = returnResponse)
	}

	#' List ReimbursementRunHistories
	#'
	#' This function returns a dataframe or json object of ReimbursementRunHistories
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given ReimbursementRunHistories. Defaults to FALSE for all return fields which, for convenience, returns all fields for the ReimbursementRunHistories.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object listed as text. Run \code{\link{getSchemaForObjects}}('ReimbursementRunHistory') to get more field paths.
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
	#' @concept Food Service
	#' @return A list of ReimbursementRunHistories
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	listReimbursementRunHistories <- function(searchConditionsList = NULL, ReimbursementRunHistoryID = F, ReimbursementMonthID = F, DistrictID = F, SchoolYearID = F, StartDateTime = F, EndDateTime = F, Type = F, MediaID = F, ScopeAccessAllowed = F, HasValidMedia = F, CanAccessMedia = F, IsExport = F, IsLocked = F, Status = F, RunData = F, UserIDCanceledBy = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, RunParameters = F, fieldPaths = NULL, searchConditionsGroupType = "And", searchSortFieldNamesList = NULL, searchSortFieldNamesDescendingList = NULL, entityId = 1, schoolYearId = NULL, page = 1, pageSize = 100000, flatten = T, returnResponse = F){

		params <- as.list(environment())

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		listSkyObjects(module = "FoodService", objectName = "ReimbursementRunHistory", searchFields = searchFields %>% append(fieldPaths), page = page, pageSize = pageSize, SearchConditionsList = searchConditionsList, SearchConditionsGroupType = searchConditionsGroupType, SearchSortFieldNamesList = searchSortFieldNamesList, SearchSortFieldNamesDescendingList = searchSortFieldNamesDescendingList, entityId = entityId, schoolYearId = schoolYearId, flatten = flatten, returnResponse = returnResponse)
	}

	#' Get a ReimbursementRunHistory
	#'
	#' This function returns a dataframe or json object of a ReimbursementRunHistory
	#' @param ReimbursementRunHistoryID The ID of the ReimbursementRunHistory to return.
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given ReimbursementRunHistory. Defaults to FALSE for all return fields which, for convenience, returns all fields for the ReimbursementRunHistory.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object listed as text. Run \code{\link{getSchemaForObjects}}('ReimbursementRunHistory') to get more field paths.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{listSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Food Service
	#' @return A dataframe or of ReimbursementRunHistory
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	getReimbursementRunHistory <- function(ReimbursementRunHistoryID, ReimbursementMonthID = F, DistrictID = F, SchoolYearID = F, StartDateTime = F, EndDateTime = F, Type = F, MediaID = F, ScopeAccessAllowed = F, HasValidMedia = F, CanAccessMedia = F, IsExport = F, IsLocked = F, Status = F, RunData = F, UserIDCanceledBy = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, RunParameters = F, fieldPaths = NULL, entityId = 1, schoolYearId = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment()) %>% purrr::keep(names(.) != "ReimbursementRunHistoryID")

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		getSkyObject(module = "FoodService", objectName = "ReimbursementRunHistory", objectId = ReimbursementRunHistoryID, searchFields = searchFields, entityId = entityId, flatten = flatten, returnResponse = returnResponse)
	}

	#' Delete a ReimbursementRunHistory
	#'
	#' This function deletes a ReimbursementRunHistory
	#' @param ReimbursementRunHistoryID The ID of the ReimbursementRunHistory to delete
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{listSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Food Service
	#' @return The ReimbursementRunHistoryID of the deleted ReimbursementRunHistory.
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	deleteReimbursementRunHistory <- function(ReimbursementRunHistoryID, ignoreWarnings = F, entityId = 1, schoolYearId = NULL, flatten = T, returnResponse = F){

		deleteSkyObject(module = "FoodService", objectName = "ReimbursementRunHistory", objectId = ReimbursementRunHistoryID, ignoreWarnings = ignoreWarnings, entityId = entityId, flatten = flatten, returnResponse = returnResponse)
	}

	#' Create a ReimbursementRunHistory
	#'
	#' This function creates a ReimbursementRunHistory
	#' @param fieldNames The field values to give the created ReimbursementRunHistory. Each defaults to NULL.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{listSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Food Service
	#' @return A newly created ReimbursementRunHistory
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	createReimbursementRunHistory <- function(ReimbursementMonthID = NULL, DistrictID = NULL, SchoolYearID = NULL, StartDateTime = NULL, EndDateTime = NULL, Type = NULL, MediaID = NULL, Status = NULL, UserIDCanceledBy = NULL, entityId = 1, schoolYearId = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment())

		body <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper()) %>% purrr::compact()

		createSkyObject(module = "FoodService", objectName = "ReimbursementRunHistory", body = list(DataObject = body), searchFields = append("ReimbursementRunHistoryID", body %>% names()), entityId = entityId, schoolYearId = schoolYearId, flatten = flatten, returnResponse = returnResponse)
	}

	#' Modify a ReimbursementRunHistory
	#'
	#' This function modifies a ReimbursementRunHistory
	#' @param fieldNames The field values to give the modified ReimbursementRunHistory. Each defaults to NULL.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{listSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Food Service
	#' @return The modified ReimbursementRunHistory
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	modifyReimbursementRunHistory <- function(ReimbursementRunHistoryID, ReimbursementMonthID = NULL, DistrictID = NULL, SchoolYearID = NULL, StartDateTime = NULL, EndDateTime = NULL, Type = NULL, MediaID = NULL, Status = NULL, UserIDCanceledBy = NULL, entityId = 1, schoolYearId = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment())

		body <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper()) %>% purrr::compact()

		modifySkyObject(module = "FoodService", objectName = "ReimbursementRunHistory", objectId = ReimbursementRunHistoryID, body = list(DataObject = body), searchFields = append("ReimbursementRunHistoryID", body %>% names()), entityId = entityId, schoolYearId = schoolYearId, flatten = flatten, returnResponse = returnResponse)
	}

	#' List ReimbursementDays
	#'
	#' This function returns a dataframe or json object of ReimbursementDays
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given ReimbursementDays. Defaults to FALSE for all return fields which, for convenience, returns all fields for the ReimbursementDays.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object listed as text. Run \code{\link{getSchemaForObjects}}('ReimbursementDay') to get more field paths.
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
	#' @concept Food Service
	#' @return A list of ReimbursementDays
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	listReimbursementDays <- function(searchConditionsList = NULL, ReimbursementDayID = F, DistrictID = F, EntityID = F, StudentID = F, CustomerID = F, ReimbursementRunHistoryID = F, ReimbursementMonthID = F, PurchaseID = F, EligibilityType = F, TrackingType = F, NumericDay = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, IsAdultPurchase = F, ReimbursableMealTypeID = F, MealTypeCode = F, KeyHash = F, UpdateHash = F, IsDuplicateDayApproval = F, IsLastDayApproval = F, StateMealProgramTypeID = F, MealProgramTypeCode = F, SchoolID = F, fieldPaths = NULL, searchConditionsGroupType = "And", searchSortFieldNamesList = NULL, searchSortFieldNamesDescendingList = NULL, entityId = 1, schoolYearId = NULL, page = 1, pageSize = 100000, flatten = T, returnResponse = F){

		params <- as.list(environment())

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		listSkyObjects(module = "FoodService", objectName = "ReimbursementDay", searchFields = searchFields %>% append(fieldPaths), page = page, pageSize = pageSize, SearchConditionsList = searchConditionsList, SearchConditionsGroupType = searchConditionsGroupType, SearchSortFieldNamesList = searchSortFieldNamesList, SearchSortFieldNamesDescendingList = searchSortFieldNamesDescendingList, entityId = entityId, schoolYearId = schoolYearId, flatten = flatten, returnResponse = returnResponse)
	}

	#' Get a ReimbursementDay
	#'
	#' This function returns a dataframe or json object of a ReimbursementDay
	#' @param ReimbursementDayID The ID of the ReimbursementDay to return.
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given ReimbursementDay. Defaults to FALSE for all return fields which, for convenience, returns all fields for the ReimbursementDay.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object listed as text. Run \code{\link{getSchemaForObjects}}('ReimbursementDay') to get more field paths.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{listSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Food Service
	#' @return A dataframe or of ReimbursementDay
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	getReimbursementDay <- function(ReimbursementDayID, DistrictID = F, EntityID = F, StudentID = F, CustomerID = F, ReimbursementRunHistoryID = F, ReimbursementMonthID = F, PurchaseID = F, EligibilityType = F, TrackingType = F, NumericDay = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, IsAdultPurchase = F, ReimbursableMealTypeID = F, MealTypeCode = F, KeyHash = F, UpdateHash = F, IsDuplicateDayApproval = F, IsLastDayApproval = F, StateMealProgramTypeID = F, MealProgramTypeCode = F, SchoolID = F, fieldPaths = NULL, entityId = 1, schoolYearId = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment()) %>% purrr::keep(names(.) != "ReimbursementDayID")

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		getSkyObject(module = "FoodService", objectName = "ReimbursementDay", objectId = ReimbursementDayID, searchFields = searchFields, entityId = entityId, flatten = flatten, returnResponse = returnResponse)
	}

	#' Delete a ReimbursementDay
	#'
	#' This function deletes a ReimbursementDay
	#' @param ReimbursementDayID The ID of the ReimbursementDay to delete
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{listSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Food Service
	#' @return The ReimbursementDayID of the deleted ReimbursementDay.
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	deleteReimbursementDay <- function(ReimbursementDayID, ignoreWarnings = F, entityId = 1, schoolYearId = NULL, flatten = T, returnResponse = F){

		deleteSkyObject(module = "FoodService", objectName = "ReimbursementDay", objectId = ReimbursementDayID, ignoreWarnings = ignoreWarnings, entityId = entityId, flatten = flatten, returnResponse = returnResponse)
	}

	#' Create a ReimbursementDay
	#'
	#' This function creates a ReimbursementDay
	#' @param fieldNames The field values to give the created ReimbursementDay. Each defaults to NULL.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{listSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Food Service
	#' @return A newly created ReimbursementDay
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	createReimbursementDay <- function(DistrictID = NULL, EntityID = NULL, StudentID = NULL, CustomerID = NULL, ReimbursementRunHistoryID = NULL, ReimbursementMonthID = NULL, PurchaseID = NULL, EligibilityType = NULL, TrackingType = NULL, NumericDay = NULL, IsAdultPurchase = NULL, ReimbursableMealTypeID = NULL, MealTypeCode = NULL, IsDuplicateDayApproval = NULL, IsLastDayApproval = NULL, StateMealProgramTypeID = NULL, MealProgramTypeCode = NULL, SchoolID = NULL, entityId = 1, schoolYearId = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment())

		body <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper()) %>% purrr::compact()

		createSkyObject(module = "FoodService", objectName = "ReimbursementDay", body = list(DataObject = body), searchFields = append("ReimbursementDayID", body %>% names()), entityId = entityId, schoolYearId = schoolYearId, flatten = flatten, returnResponse = returnResponse)
	}

	#' Modify a ReimbursementDay
	#'
	#' This function modifies a ReimbursementDay
	#' @param fieldNames The field values to give the modified ReimbursementDay. Each defaults to NULL.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{listSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Food Service
	#' @return The modified ReimbursementDay
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	modifyReimbursementDay <- function(ReimbursementDayID, DistrictID = NULL, EntityID = NULL, StudentID = NULL, CustomerID = NULL, ReimbursementRunHistoryID = NULL, ReimbursementMonthID = NULL, PurchaseID = NULL, EligibilityType = NULL, TrackingType = NULL, NumericDay = NULL, IsAdultPurchase = NULL, ReimbursableMealTypeID = NULL, MealTypeCode = NULL, IsDuplicateDayApproval = NULL, IsLastDayApproval = NULL, StateMealProgramTypeID = NULL, MealProgramTypeCode = NULL, SchoolID = NULL, entityId = 1, schoolYearId = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment())

		body <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper()) %>% purrr::compact()

		modifySkyObject(module = "FoodService", objectName = "ReimbursementDay", objectId = ReimbursementDayID, body = list(DataObject = body), searchFields = append("ReimbursementDayID", body %>% names()), entityId = entityId, schoolYearId = schoolYearId, flatten = flatten, returnResponse = returnResponse)
	}

	#' List ReimbursementMonthlyClaimByEntities
	#'
	#' This function returns a dataframe or json object of ReimbursementMonthlyClaimByEntities
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given ReimbursementMonthlyClaimByEntities. Defaults to FALSE for all return fields which, for convenience, returns all fields for the ReimbursementMonthlyClaimByEntities.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object listed as text. Run \code{\link{getSchemaForObjects}}('ReimbursementMonthlyClaimByEntity') to get more field paths.
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
	#' @concept Food Service
	#' @return A list of ReimbursementMonthlyClaimByEntities
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	listReimbursementMonthlyClaimByEntities <- function(searchConditionsList = NULL, ReimbursementMonthID = F, MealType = F, ServedCountFree = F, ServedCountReduced = F, ServedCountPaid = F, ServedCountTotal = F, ServedCountOther = F, DistrictID = F, EntityID = F, ApprovedCountFree = F, ApprovedCountReduced = F, PaidEligible = F, fieldPaths = NULL, searchConditionsGroupType = "And", searchSortFieldNamesList = NULL, searchSortFieldNamesDescendingList = NULL, entityId = 1, schoolYearId = NULL, page = 1, pageSize = 100000, flatten = T, returnResponse = F){

		params <- as.list(environment())

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		listSkyObjects(module = "FoodService", objectName = "ReimbursementMonthlyClaimByEntity", searchFields = searchFields %>% append(fieldPaths), page = page, pageSize = pageSize, SearchConditionsList = searchConditionsList, SearchConditionsGroupType = searchConditionsGroupType, SearchSortFieldNamesList = searchSortFieldNamesList, SearchSortFieldNamesDescendingList = searchSortFieldNamesDescendingList, entityId = entityId, schoolYearId = schoolYearId, flatten = flatten, returnResponse = returnResponse)
	}

	#' Get a ReimbursementMonthlyClaimByEntity
	#'
	#' This function returns a dataframe or json object of a ReimbursementMonthlyClaimByEntity
	#' @param ReimbursementMonthlyClaimByEntityID The ID of the ReimbursementMonthlyClaimByEntity to return.
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given ReimbursementMonthlyClaimByEntity. Defaults to FALSE for all return fields which, for convenience, returns all fields for the ReimbursementMonthlyClaimByEntity.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object listed as text. Run \code{\link{getSchemaForObjects}}('ReimbursementMonthlyClaimByEntity') to get more field paths.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{listSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Food Service
	#' @return A dataframe or of ReimbursementMonthlyClaimByEntity
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	getReimbursementMonthlyClaimByEntity <- function(ReimbursementMonthlyClaimByEntityID, ReimbursementMonthID = F, MealType = F, ServedCountFree = F, ServedCountReduced = F, ServedCountPaid = F, ServedCountTotal = F, ServedCountOther = F, DistrictID = F, EntityID = F, ApprovedCountFree = F, ApprovedCountReduced = F, PaidEligible = F, fieldPaths = NULL, entityId = 1, schoolYearId = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment()) %>% purrr::keep(names(.) != "ReimbursementMonthlyClaimByEntityID")

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		getSkyObject(module = "FoodService", objectName = "ReimbursementMonthlyClaimByEntity", objectId = ReimbursementMonthlyClaimByEntityID, searchFields = searchFields, entityId = entityId, flatten = flatten, returnResponse = returnResponse)
	}

	#' Delete a ReimbursementMonthlyClaimByEntity
	#'
	#' This function deletes a ReimbursementMonthlyClaimByEntity
	#' @param ReimbursementMonthlyClaimByEntityID The ID of the ReimbursementMonthlyClaimByEntity to delete
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{listSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Food Service
	#' @return The ReimbursementMonthlyClaimByEntityID of the deleted ReimbursementMonthlyClaimByEntity.
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	deleteReimbursementMonthlyClaimByEntity <- function(ReimbursementMonthlyClaimByEntityID, ignoreWarnings = F, entityId = 1, schoolYearId = NULL, flatten = T, returnResponse = F){

		deleteSkyObject(module = "FoodService", objectName = "ReimbursementMonthlyClaimByEntity", objectId = ReimbursementMonthlyClaimByEntityID, ignoreWarnings = ignoreWarnings, entityId = entityId, flatten = flatten, returnResponse = returnResponse)
	}

	#' Create a ReimbursementMonthlyClaimByEntity
	#'
	#' This function creates a ReimbursementMonthlyClaimByEntity
	#' @param fieldNames The field values to give the created ReimbursementMonthlyClaimByEntity. Each defaults to NULL.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{listSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Food Service
	#' @return A newly created ReimbursementMonthlyClaimByEntity
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	createReimbursementMonthlyClaimByEntity <- function(ReimbursementMonthID = NULL, MealType = NULL, ServedCountFree = NULL, ServedCountReduced = NULL, ServedCountPaid = NULL, ServedCountTotal = NULL, ServedCountOther = NULL, DistrictID = NULL, ApprovedCountFree = NULL, ApprovedCountReduced = NULL, PaidEligible = NULL, entityId = 1, schoolYearId = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment())

		body <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper()) %>% purrr::compact()

		createSkyObject(module = "FoodService", objectName = "ReimbursementMonthlyClaimByEntity", body = list(DataObject = body), searchFields = append("ReimbursementMonthlyClaimByEntityID", body %>% names()), entityId = entityId, schoolYearId = schoolYearId, flatten = flatten, returnResponse = returnResponse)
	}

	#' Modify a ReimbursementMonthlyClaimByEntity
	#'
	#' This function modifies a ReimbursementMonthlyClaimByEntity
	#' @param fieldNames The field values to give the modified ReimbursementMonthlyClaimByEntity. Each defaults to NULL.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{listSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Food Service
	#' @return The modified ReimbursementMonthlyClaimByEntity
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	modifyReimbursementMonthlyClaimByEntity <- function(ReimbursementMonthlyClaimByEntityID, ReimbursementMonthID = NULL, MealType = NULL, ServedCountFree = NULL, ServedCountReduced = NULL, ServedCountPaid = NULL, ServedCountTotal = NULL, ServedCountOther = NULL, DistrictID = NULL, ApprovedCountFree = NULL, ApprovedCountReduced = NULL, PaidEligible = NULL, entityId = 1, schoolYearId = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment())

		body <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper()) %>% purrr::compact()

		modifySkyObject(module = "FoodService", objectName = "ReimbursementMonthlyClaimByEntity", objectId = ReimbursementMonthlyClaimByEntityID, body = list(DataObject = body), searchFields = append("ReimbursementMonthlyClaimByEntityID", body %>% names()), entityId = entityId, schoolYearId = schoolYearId, flatten = flatten, returnResponse = returnResponse)
	}

	#' List ReimbursementStudentTotals
	#'
	#' This function returns a dataframe or json object of ReimbursementStudentTotals
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given ReimbursementStudentTotals. Defaults to FALSE for all return fields which, for convenience, returns all fields for the ReimbursementStudentTotals.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object listed as text. Run \code{\link{getSchemaForObjects}}('ReimbursementStudentTotal') to get more field paths.
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
	#' @concept Food Service
	#' @return A list of ReimbursementStudentTotals
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	listReimbursementStudentTotals <- function(searchConditionsList = NULL, ReimbursementStudentTotalID = F, EntityID = F, ReimbursementRunHistoryID = F, ReimbursementMonthID = F, SumOfDailyAttendance = F, HighestDailyTotalEnrollment = F, HighestParticipantsFree = F, HighestParticipantsReduced = F, HighestParticipantsPaid = F, HighestParticipantsTotal = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, KeyHash = F, UpdateHash = F, HighestParticipantsCEP = F, HighestParticipantsFreeCEP = F, HighestParticipantsReducedCEP = F, SchoolID = F, fieldPaths = NULL, searchConditionsGroupType = "And", searchSortFieldNamesList = NULL, searchSortFieldNamesDescendingList = NULL, entityId = 1, schoolYearId = NULL, page = 1, pageSize = 100000, flatten = T, returnResponse = F){

		params <- as.list(environment())

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		listSkyObjects(module = "FoodService", objectName = "ReimbursementStudentTotal", searchFields = searchFields %>% append(fieldPaths), page = page, pageSize = pageSize, SearchConditionsList = searchConditionsList, SearchConditionsGroupType = searchConditionsGroupType, SearchSortFieldNamesList = searchSortFieldNamesList, SearchSortFieldNamesDescendingList = searchSortFieldNamesDescendingList, entityId = entityId, schoolYearId = schoolYearId, flatten = flatten, returnResponse = returnResponse)
	}

	#' Get a ReimbursementStudentTotal
	#'
	#' This function returns a dataframe or json object of a ReimbursementStudentTotal
	#' @param ReimbursementStudentTotalID The ID of the ReimbursementStudentTotal to return.
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given ReimbursementStudentTotal. Defaults to FALSE for all return fields which, for convenience, returns all fields for the ReimbursementStudentTotal.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object listed as text. Run \code{\link{getSchemaForObjects}}('ReimbursementStudentTotal') to get more field paths.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{listSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Food Service
	#' @return A dataframe or of ReimbursementStudentTotal
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	getReimbursementStudentTotal <- function(ReimbursementStudentTotalID, EntityID = F, ReimbursementRunHistoryID = F, ReimbursementMonthID = F, SumOfDailyAttendance = F, HighestDailyTotalEnrollment = F, HighestParticipantsFree = F, HighestParticipantsReduced = F, HighestParticipantsPaid = F, HighestParticipantsTotal = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, KeyHash = F, UpdateHash = F, HighestParticipantsCEP = F, HighestParticipantsFreeCEP = F, HighestParticipantsReducedCEP = F, SchoolID = F, fieldPaths = NULL, entityId = 1, schoolYearId = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment()) %>% purrr::keep(names(.) != "ReimbursementStudentTotalID")

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		getSkyObject(module = "FoodService", objectName = "ReimbursementStudentTotal", objectId = ReimbursementStudentTotalID, searchFields = searchFields, entityId = entityId, flatten = flatten, returnResponse = returnResponse)
	}

	#' Delete a ReimbursementStudentTotal
	#'
	#' This function deletes a ReimbursementStudentTotal
	#' @param ReimbursementStudentTotalID The ID of the ReimbursementStudentTotal to delete
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{listSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Food Service
	#' @return The ReimbursementStudentTotalID of the deleted ReimbursementStudentTotal.
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	deleteReimbursementStudentTotal <- function(ReimbursementStudentTotalID, ignoreWarnings = F, entityId = 1, schoolYearId = NULL, flatten = T, returnResponse = F){

		deleteSkyObject(module = "FoodService", objectName = "ReimbursementStudentTotal", objectId = ReimbursementStudentTotalID, ignoreWarnings = ignoreWarnings, entityId = entityId, flatten = flatten, returnResponse = returnResponse)
	}

	#' Create a ReimbursementStudentTotal
	#'
	#' This function creates a ReimbursementStudentTotal
	#' @param fieldNames The field values to give the created ReimbursementStudentTotal. Each defaults to NULL.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{listSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Food Service
	#' @return A newly created ReimbursementStudentTotal
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	createReimbursementStudentTotal <- function(EntityID = NULL, ReimbursementRunHistoryID = NULL, ReimbursementMonthID = NULL, SumOfDailyAttendance = NULL, HighestDailyTotalEnrollment = NULL, HighestParticipantsFree = NULL, HighestParticipantsReduced = NULL, HighestParticipantsPaid = NULL, HighestParticipantsTotal = NULL, HighestParticipantsCEP = NULL, HighestParticipantsFreeCEP = NULL, HighestParticipantsReducedCEP = NULL, SchoolID = NULL, entityId = 1, schoolYearId = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment())

		body <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper()) %>% purrr::compact()

		createSkyObject(module = "FoodService", objectName = "ReimbursementStudentTotal", body = list(DataObject = body), searchFields = append("ReimbursementStudentTotalID", body %>% names()), entityId = entityId, schoolYearId = schoolYearId, flatten = flatten, returnResponse = returnResponse)
	}

	#' Modify a ReimbursementStudentTotal
	#'
	#' This function modifies a ReimbursementStudentTotal
	#' @param fieldNames The field values to give the modified ReimbursementStudentTotal. Each defaults to NULL.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{listSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Food Service
	#' @return The modified ReimbursementStudentTotal
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	modifyReimbursementStudentTotal <- function(ReimbursementStudentTotalID, EntityID = NULL, ReimbursementRunHistoryID = NULL, ReimbursementMonthID = NULL, SumOfDailyAttendance = NULL, HighestDailyTotalEnrollment = NULL, HighestParticipantsFree = NULL, HighestParticipantsReduced = NULL, HighestParticipantsPaid = NULL, HighestParticipantsTotal = NULL, HighestParticipantsCEP = NULL, HighestParticipantsFreeCEP = NULL, HighestParticipantsReducedCEP = NULL, SchoolID = NULL, entityId = 1, schoolYearId = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment())

		body <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper()) %>% purrr::compact()

		modifySkyObject(module = "FoodService", objectName = "ReimbursementStudentTotal", objectId = ReimbursementStudentTotalID, body = list(DataObject = body), searchFields = append("ReimbursementStudentTotalID", body %>% names()), entityId = entityId, schoolYearId = schoolYearId, flatten = flatten, returnResponse = returnResponse)
	}

	#' List FoodServiceConfigEntities
	#'
	#' This function returns a dataframe or json object of FoodServiceConfigEntities
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given FoodServiceConfigEntities. Defaults to FALSE for all return fields which, for convenience, returns all fields for the FoodServiceConfigEntities.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object listed as text. Run \code{\link{getSchemaForObjects}}('FoodServiceConfigEntity') to get more field paths.
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
	#' @concept Food Service
	#' @return A list of FoodServiceConfigEntities
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	listFoodServiceConfigEntities <- function(searchConditionsList = NULL, ConfigEntityID = F, EntityID = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, fieldPaths = NULL, searchConditionsGroupType = "And", searchSortFieldNamesList = NULL, searchSortFieldNamesDescendingList = NULL, entityId = 1, schoolYearId = NULL, page = 1, pageSize = 100000, flatten = T, returnResponse = F){

		params <- as.list(environment())

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		listSkyObjects(module = "FoodService", objectName = "ConfigEntity", searchFields = searchFields %>% append(fieldPaths), page = page, pageSize = pageSize, SearchConditionsList = searchConditionsList, SearchConditionsGroupType = searchConditionsGroupType, SearchSortFieldNamesList = searchSortFieldNamesList, SearchSortFieldNamesDescendingList = searchSortFieldNamesDescendingList, entityId = entityId, schoolYearId = schoolYearId, flatten = flatten, returnResponse = returnResponse)
	}

	#' Get a FoodServiceConfigEntity
	#'
	#' This function returns a dataframe or json object of a FoodServiceConfigEntity
	#' @param FoodServiceConfigEntityID The ID of the FoodServiceConfigEntity to return.
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given FoodServiceConfigEntity. Defaults to FALSE for all return fields which, for convenience, returns all fields for the FoodServiceConfigEntity.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object listed as text. Run \code{\link{getSchemaForObjects}}('FoodServiceConfigEntity') to get more field paths.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{listSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Food Service
	#' @return A dataframe or of FoodServiceConfigEntity
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	getFoodServiceConfigEntity <- function(FoodServiceConfigEntityID, ConfigEntityID = F, EntityID = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, fieldPaths = NULL, entityId = 1, schoolYearId = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment()) %>% purrr::keep(names(.) != "FoodServiceConfigEntityID")

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		getSkyObject(module = "FoodService", objectName = "ConfigEntity", objectId = FoodServiceConfigEntityID, searchFields = searchFields, entityId = entityId, flatten = flatten, returnResponse = returnResponse)
	}

	#' Delete a FoodServiceConfigEntity
	#'
	#' This function deletes a FoodServiceConfigEntity
	#' @param FoodServiceConfigEntityID The ID of the FoodServiceConfigEntity to delete
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{listSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Food Service
	#' @return The FoodServiceConfigEntityID of the deleted FoodServiceConfigEntity.
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	deleteFoodServiceConfigEntity <- function(FoodServiceConfigEntityID, ignoreWarnings = F, entityId = 1, schoolYearId = NULL, flatten = T, returnResponse = F){

		deleteSkyObject(module = "FoodService", objectName = "ConfigEntity", objectId = FoodServiceConfigEntityID, ignoreWarnings = ignoreWarnings, entityId = entityId, flatten = flatten, returnResponse = returnResponse)
	}

	#' Create a FoodServiceConfigEntity
	#'
	#' This function creates a FoodServiceConfigEntity
	#' @param fieldNames The field values to give the created FoodServiceConfigEntity. Each defaults to NULL.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{listSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Food Service
	#' @return A newly created FoodServiceConfigEntity
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	createFoodServiceConfigEntity <- function(EntityID = NULL, entityId = 1, schoolYearId = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment())

		body <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper()) %>% purrr::compact()

		createSkyObject(module = "FoodService", objectName = "ConfigEntity", body = list(DataObject = body), searchFields = append("ConfigEntityID", body %>% names()), entityId = entityId, schoolYearId = schoolYearId, flatten = flatten, returnResponse = returnResponse)
	}

	#' Modify a FoodServiceConfigEntity
	#'
	#' This function modifies a FoodServiceConfigEntity
	#' @param fieldNames The field values to give the modified FoodServiceConfigEntity. Each defaults to NULL.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{listSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Food Service
	#' @return The modified FoodServiceConfigEntity
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	modifyFoodServiceConfigEntity <- function(ConfigEntityID, EntityID = NULL, entityId = 1, schoolYearId = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment())

		body <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper()) %>% purrr::compact()

		modifySkyObject(module = "FoodService", objectName = "ConfigEntity", objectId = ConfigEntityID, body = list(DataObject = body), searchFields = append("ConfigEntityID", body %>% names()), entityId = entityId, schoolYearId = schoolYearId, flatten = flatten, returnResponse = returnResponse)
	}

	#' List ReimbursableMealTypes
	#'
	#' This function returns a dataframe or json object of ReimbursableMealTypes
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given ReimbursableMealTypes. Defaults to FALSE for all return fields which, for convenience, returns all fields for the ReimbursableMealTypes.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object listed as text. Run \code{\link{getSchemaForObjects}}('ReimbursableMealType') to get more field paths.
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
	#' @concept Food Service
	#' @return A list of ReimbursableMealTypes
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	listReimbursableMealTypes <- function(searchConditionsList = NULL, ReimbursableMealTypeID = F, Code = F, Description = F, CodeDescription = F, ValidYearLow = F, ValidYearHigh = F, IsInvalid = F, SkywardID = F, SkywardHash = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, fieldPaths = NULL, searchConditionsGroupType = "And", searchSortFieldNamesList = NULL, searchSortFieldNamesDescendingList = NULL, entityId = 1, schoolYearId = NULL, page = 1, pageSize = 100000, flatten = T, returnResponse = F){

		params <- as.list(environment())

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		listSkyObjects(module = "FoodService", objectName = "ReimbursableMealType", searchFields = searchFields %>% append(fieldPaths), page = page, pageSize = pageSize, SearchConditionsList = searchConditionsList, SearchConditionsGroupType = searchConditionsGroupType, SearchSortFieldNamesList = searchSortFieldNamesList, SearchSortFieldNamesDescendingList = searchSortFieldNamesDescendingList, entityId = entityId, schoolYearId = schoolYearId, flatten = flatten, returnResponse = returnResponse)
	}

	#' Get a ReimbursableMealType
	#'
	#' This function returns a dataframe or json object of a ReimbursableMealType
	#' @param ReimbursableMealTypeID The ID of the ReimbursableMealType to return.
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given ReimbursableMealType. Defaults to FALSE for all return fields which, for convenience, returns all fields for the ReimbursableMealType.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object listed as text. Run \code{\link{getSchemaForObjects}}('ReimbursableMealType') to get more field paths.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{listSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Food Service
	#' @return A dataframe or of ReimbursableMealType
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	getReimbursableMealType <- function(ReimbursableMealTypeID, Code = F, Description = F, CodeDescription = F, ValidYearLow = F, ValidYearHigh = F, IsInvalid = F, SkywardID = F, SkywardHash = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, fieldPaths = NULL, entityId = 1, schoolYearId = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment()) %>% purrr::keep(names(.) != "ReimbursableMealTypeID")

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		getSkyObject(module = "FoodService", objectName = "ReimbursableMealType", objectId = ReimbursableMealTypeID, searchFields = searchFields, entityId = entityId, flatten = flatten, returnResponse = returnResponse)
	}

	#' Delete a ReimbursableMealType
	#'
	#' This function deletes a ReimbursableMealType
	#' @param ReimbursableMealTypeID The ID of the ReimbursableMealType to delete
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{listSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Food Service
	#' @return The ReimbursableMealTypeID of the deleted ReimbursableMealType.
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	deleteReimbursableMealType <- function(ReimbursableMealTypeID, ignoreWarnings = F, entityId = 1, schoolYearId = NULL, flatten = T, returnResponse = F){

		deleteSkyObject(module = "FoodService", objectName = "ReimbursableMealType", objectId = ReimbursableMealTypeID, ignoreWarnings = ignoreWarnings, entityId = entityId, flatten = flatten, returnResponse = returnResponse)
	}

	#' Create a ReimbursableMealType
	#'
	#' This function creates a ReimbursableMealType
	#' @param fieldNames The field values to give the created ReimbursableMealType. Each defaults to NULL.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{listSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Food Service
	#' @return A newly created ReimbursableMealType
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	createReimbursableMealType <- function(Code = NULL, Description = NULL, ValidYearLow = NULL, ValidYearHigh = NULL, IsInvalid = NULL, entityId = 1, schoolYearId = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment())

		body <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper()) %>% purrr::compact()

		createSkyObject(module = "FoodService", objectName = "ReimbursableMealType", body = list(DataObject = body), searchFields = append("ReimbursableMealTypeID", body %>% names()), entityId = entityId, schoolYearId = schoolYearId, flatten = flatten, returnResponse = returnResponse)
	}

	#' Modify a ReimbursableMealType
	#'
	#' This function modifies a ReimbursableMealType
	#' @param fieldNames The field values to give the modified ReimbursableMealType. Each defaults to NULL.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{listSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Food Service
	#' @return The modified ReimbursableMealType
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	modifyReimbursableMealType <- function(ReimbursableMealTypeID, Code = NULL, Description = NULL, ValidYearLow = NULL, ValidYearHigh = NULL, IsInvalid = NULL, entityId = 1, schoolYearId = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment())

		body <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper()) %>% purrr::compact()

		modifySkyObject(module = "FoodService", objectName = "ReimbursableMealType", objectId = ReimbursableMealTypeID, body = list(DataObject = body), searchFields = append("ReimbursableMealTypeID", body %>% names()), entityId = entityId, schoolYearId = schoolYearId, flatten = flatten, returnResponse = returnResponse)
	}

	#' List TempDirectVerifications
	#'
	#' This function returns a dataframe or json object of TempDirectVerifications
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given TempDirectVerifications. Defaults to FALSE for all return fields which, for convenience, returns all fields for the TempDirectVerifications.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object listed as text. Run \code{\link{getSchemaForObjects}}('TempDirectVerification') to get more field paths.
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
	#' @concept Food Service
	#' @return A list of TempDirectVerifications
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	listTempDirectVerifications <- function(searchConditionsList = NULL, TempDirectVerificationID = F, LastName = F, FirstName = F, Birthdate = F, ProgramOfEligibility = F, MealCode = F, EDGNumber = F, HasExceptions = F, ExceptionNote = F, NotInImportFile = F, ApplicationMemberID = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, fieldPaths = NULL, searchConditionsGroupType = "And", searchSortFieldNamesList = NULL, searchSortFieldNamesDescendingList = NULL, entityId = 1, schoolYearId = NULL, page = 1, pageSize = 100000, flatten = T, returnResponse = F){

		params <- as.list(environment())

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		listSkyObjects(module = "FoodService", objectName = "TempDirectVerification", searchFields = searchFields %>% append(fieldPaths), page = page, pageSize = pageSize, SearchConditionsList = searchConditionsList, SearchConditionsGroupType = searchConditionsGroupType, SearchSortFieldNamesList = searchSortFieldNamesList, SearchSortFieldNamesDescendingList = searchSortFieldNamesDescendingList, entityId = entityId, schoolYearId = schoolYearId, flatten = flatten, returnResponse = returnResponse)
	}

	#' Get a TempDirectVerification
	#'
	#' This function returns a dataframe or json object of a TempDirectVerification
	#' @param TempDirectVerificationID The ID of the TempDirectVerification to return.
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given TempDirectVerification. Defaults to FALSE for all return fields which, for convenience, returns all fields for the TempDirectVerification.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object listed as text. Run \code{\link{getSchemaForObjects}}('TempDirectVerification') to get more field paths.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{listSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Food Service
	#' @return A dataframe or of TempDirectVerification
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	getTempDirectVerification <- function(TempDirectVerificationID, LastName = F, FirstName = F, Birthdate = F, ProgramOfEligibility = F, MealCode = F, EDGNumber = F, HasExceptions = F, ExceptionNote = F, NotInImportFile = F, ApplicationMemberID = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, fieldPaths = NULL, entityId = 1, schoolYearId = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment()) %>% purrr::keep(names(.) != "TempDirectVerificationID")

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		getSkyObject(module = "FoodService", objectName = "TempDirectVerification", objectId = TempDirectVerificationID, searchFields = searchFields, entityId = entityId, flatten = flatten, returnResponse = returnResponse)
	}

	#' Delete a TempDirectVerification
	#'
	#' This function deletes a TempDirectVerification
	#' @param TempDirectVerificationID The ID of the TempDirectVerification to delete
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{listSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Food Service
	#' @return The TempDirectVerificationID of the deleted TempDirectVerification.
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	deleteTempDirectVerification <- function(TempDirectVerificationID, ignoreWarnings = F, entityId = 1, schoolYearId = NULL, flatten = T, returnResponse = F){

		deleteSkyObject(module = "FoodService", objectName = "TempDirectVerification", objectId = TempDirectVerificationID, ignoreWarnings = ignoreWarnings, entityId = entityId, flatten = flatten, returnResponse = returnResponse)
	}

	#' Create a TempDirectVerification
	#'
	#' This function creates a TempDirectVerification
	#' @param fieldNames The field values to give the created TempDirectVerification. Each defaults to NULL.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{listSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Food Service
	#' @return A newly created TempDirectVerification
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	createTempDirectVerification <- function(LastName = NULL, FirstName = NULL, Birthdate = NULL, ProgramOfEligibility = NULL, MealCode = NULL, EDGNumber = NULL, HasExceptions = NULL, ExceptionNote = NULL, NotInImportFile = NULL, ApplicationMemberID = NULL, entityId = 1, schoolYearId = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment())

		body <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper()) %>% purrr::compact()

		createSkyObject(module = "FoodService", objectName = "TempDirectVerification", body = list(DataObject = body), searchFields = append("TempDirectVerificationID", body %>% names()), entityId = entityId, schoolYearId = schoolYearId, flatten = flatten, returnResponse = returnResponse)
	}

	#' Modify a TempDirectVerification
	#'
	#' This function modifies a TempDirectVerification
	#' @param fieldNames The field values to give the modified TempDirectVerification. Each defaults to NULL.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{listSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Food Service
	#' @return The modified TempDirectVerification
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	modifyTempDirectVerification <- function(TempDirectVerificationID, LastName = NULL, FirstName = NULL, Birthdate = NULL, ProgramOfEligibility = NULL, MealCode = NULL, EDGNumber = NULL, HasExceptions = NULL, ExceptionNote = NULL, NotInImportFile = NULL, ApplicationMemberID = NULL, entityId = 1, schoolYearId = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment())

		body <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper()) %>% purrr::compact()

		modifySkyObject(module = "FoodService", objectName = "TempDirectVerification", objectId = TempDirectVerificationID, body = list(DataObject = body), searchFields = append("TempDirectVerificationID", body %>% names()), entityId = entityId, schoolYearId = schoolYearId, flatten = flatten, returnResponse = returnResponse)
	}

	#' List DistrictReimbursementMonthlyClaims
	#'
	#' This function returns a dataframe or json object of DistrictReimbursementMonthlyClaims
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given DistrictReimbursementMonthlyClaims. Defaults to FALSE for all return fields which, for convenience, returns all fields for the DistrictReimbursementMonthlyClaims.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object listed as text. Run \code{\link{getSchemaForObjects}}('DistrictReimbursementMonthlyClaim') to get more field paths.
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
	#' @concept Food Service
	#' @return A list of DistrictReimbursementMonthlyClaims
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	listDistrictReimbursementMonthlyClaims <- function(searchConditionsList = NULL, DistrictID = F, NumericDay = F, ApprovedCountFree = F, ApprovedCountReduced = F, PaidEligible = F, ParticipationFree = F, ParticipationReduced = F, ParticipationPaid = F, ReimbursementMonthID = F, MealType = F, ServedCountFree = F, ServedCountReduced = F, ServedCountPaid = F, ServedCountTotal = F, ServedCountOther = F, ServedCountCEP = F, fieldPaths = NULL, searchConditionsGroupType = "And", searchSortFieldNamesList = NULL, searchSortFieldNamesDescendingList = NULL, entityId = 1, schoolYearId = NULL, page = 1, pageSize = 100000, flatten = T, returnResponse = F){

		params <- as.list(environment())

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		listSkyObjects(module = "FoodService", objectName = "DistrictReimbursementMonthlyClaim", searchFields = searchFields %>% append(fieldPaths), page = page, pageSize = pageSize, SearchConditionsList = searchConditionsList, SearchConditionsGroupType = searchConditionsGroupType, SearchSortFieldNamesList = searchSortFieldNamesList, SearchSortFieldNamesDescendingList = searchSortFieldNamesDescendingList, entityId = entityId, schoolYearId = schoolYearId, flatten = flatten, returnResponse = returnResponse)
	}

	#' Get a DistrictReimbursementMonthlyClaim
	#'
	#' This function returns a dataframe or json object of a DistrictReimbursementMonthlyClaim
	#' @param DistrictReimbursementMonthlyClaimID The ID of the DistrictReimbursementMonthlyClaim to return.
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given DistrictReimbursementMonthlyClaim. Defaults to FALSE for all return fields which, for convenience, returns all fields for the DistrictReimbursementMonthlyClaim.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object listed as text. Run \code{\link{getSchemaForObjects}}('DistrictReimbursementMonthlyClaim') to get more field paths.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{listSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Food Service
	#' @return A dataframe or of DistrictReimbursementMonthlyClaim
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	getDistrictReimbursementMonthlyClaim <- function(DistrictReimbursementMonthlyClaimID, DistrictID = F, NumericDay = F, ApprovedCountFree = F, ApprovedCountReduced = F, PaidEligible = F, ParticipationFree = F, ParticipationReduced = F, ParticipationPaid = F, ReimbursementMonthID = F, MealType = F, ServedCountFree = F, ServedCountReduced = F, ServedCountPaid = F, ServedCountTotal = F, ServedCountOther = F, ServedCountCEP = F, fieldPaths = NULL, entityId = 1, schoolYearId = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment()) %>% purrr::keep(names(.) != "DistrictReimbursementMonthlyClaimID")

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		getSkyObject(module = "FoodService", objectName = "DistrictReimbursementMonthlyClaim", objectId = DistrictReimbursementMonthlyClaimID, searchFields = searchFields, entityId = entityId, flatten = flatten, returnResponse = returnResponse)
	}

	#' Delete a DistrictReimbursementMonthlyClaim
	#'
	#' This function deletes a DistrictReimbursementMonthlyClaim
	#' @param DistrictReimbursementMonthlyClaimID The ID of the DistrictReimbursementMonthlyClaim to delete
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{listSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Food Service
	#' @return The DistrictReimbursementMonthlyClaimID of the deleted DistrictReimbursementMonthlyClaim.
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	deleteDistrictReimbursementMonthlyClaim <- function(DistrictReimbursementMonthlyClaimID, ignoreWarnings = F, entityId = 1, schoolYearId = NULL, flatten = T, returnResponse = F){

		deleteSkyObject(module = "FoodService", objectName = "DistrictReimbursementMonthlyClaim", objectId = DistrictReimbursementMonthlyClaimID, ignoreWarnings = ignoreWarnings, entityId = entityId, flatten = flatten, returnResponse = returnResponse)
	}

	#' Create a DistrictReimbursementMonthlyClaim
	#'
	#' This function creates a DistrictReimbursementMonthlyClaim
	#' @param fieldNames The field values to give the created DistrictReimbursementMonthlyClaim. Each defaults to NULL.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{listSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Food Service
	#' @return A newly created DistrictReimbursementMonthlyClaim
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	createDistrictReimbursementMonthlyClaim <- function(NumericDay = NULL, ApprovedCountFree = NULL, ApprovedCountReduced = NULL, PaidEligible = NULL, ReimbursementMonthID = NULL, MealType = NULL, ServedCountFree = NULL, ServedCountReduced = NULL, ServedCountPaid = NULL, ServedCountTotal = NULL, ServedCountOther = NULL, ServedCountCEP = NULL, entityId = 1, schoolYearId = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment())

		body <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper()) %>% purrr::compact()

		createSkyObject(module = "FoodService", objectName = "DistrictReimbursementMonthlyClaim", body = list(DataObject = body), searchFields = append("DistrictReimbursementMonthlyClaimID", body %>% names()), entityId = entityId, schoolYearId = schoolYearId, flatten = flatten, returnResponse = returnResponse)
	}

	#' Modify a DistrictReimbursementMonthlyClaim
	#'
	#' This function modifies a DistrictReimbursementMonthlyClaim
	#' @param fieldNames The field values to give the modified DistrictReimbursementMonthlyClaim. Each defaults to NULL.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{listSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Food Service
	#' @return The modified DistrictReimbursementMonthlyClaim
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	modifyDistrictReimbursementMonthlyClaim <- function(DistrictReimbursementMonthlyClaimID, NumericDay = NULL, ApprovedCountFree = NULL, ApprovedCountReduced = NULL, PaidEligible = NULL, ReimbursementMonthID = NULL, MealType = NULL, ServedCountFree = NULL, ServedCountReduced = NULL, ServedCountPaid = NULL, ServedCountTotal = NULL, ServedCountOther = NULL, ServedCountCEP = NULL, entityId = 1, schoolYearId = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment())

		body <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper()) %>% purrr::compact()

		modifySkyObject(module = "FoodService", objectName = "DistrictReimbursementMonthlyClaim", objectId = DistrictReimbursementMonthlyClaimID, body = list(DataObject = body), searchFields = append("DistrictReimbursementMonthlyClaimID", body %>% names()), entityId = entityId, schoolYearId = schoolYearId, flatten = flatten, returnResponse = returnResponse)
	}

	#' List EntityReimbursementMonthlyClaims
	#'
	#' This function returns a dataframe or json object of EntityReimbursementMonthlyClaims
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given EntityReimbursementMonthlyClaims. Defaults to FALSE for all return fields which, for convenience, returns all fields for the EntityReimbursementMonthlyClaims.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object listed as text. Run \code{\link{getSchemaForObjects}}('EntityReimbursementMonthlyClaim') to get more field paths.
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
	#' @concept Food Service
	#' @return A list of EntityReimbursementMonthlyClaims
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	listEntityReimbursementMonthlyClaims <- function(searchConditionsList = NULL, EntityID = F, NumericDay = F, ApprovedCountFree = F, ApprovedCountReduced = F, PaidEligible = F, ParticipationFree = F, ParticipationReduced = F, ParticipationPaid = F, ReimbursementMonthID = F, MealType = F, ServedCountFree = F, ServedCountReduced = F, ServedCountPaid = F, ServedCountTotal = F, ServedCountOther = F, ServedCountCEP = F, fieldPaths = NULL, searchConditionsGroupType = "And", searchSortFieldNamesList = NULL, searchSortFieldNamesDescendingList = NULL, entityId = 1, schoolYearId = NULL, page = 1, pageSize = 100000, flatten = T, returnResponse = F){

		params <- as.list(environment())

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		listSkyObjects(module = "FoodService", objectName = "EntityReimbursementMonthlyClaim", searchFields = searchFields %>% append(fieldPaths), page = page, pageSize = pageSize, SearchConditionsList = searchConditionsList, SearchConditionsGroupType = searchConditionsGroupType, SearchSortFieldNamesList = searchSortFieldNamesList, SearchSortFieldNamesDescendingList = searchSortFieldNamesDescendingList, entityId = entityId, schoolYearId = schoolYearId, flatten = flatten, returnResponse = returnResponse)
	}

	#' Get an EntityReimbursementMonthlyClaim
	#'
	#' This function returns a dataframe or json object of an EntityReimbursementMonthlyClaim
	#' @param EntityReimbursementMonthlyClaimID The ID of the EntityReimbursementMonthlyClaim to return.
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given EntityReimbursementMonthlyClaim. Defaults to FALSE for all return fields which, for convenience, returns all fields for the EntityReimbursementMonthlyClaim.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object listed as text. Run \code{\link{getSchemaForObjects}}('EntityReimbursementMonthlyClaim') to get more field paths.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{listSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Food Service
	#' @return A dataframe or of EntityReimbursementMonthlyClaim
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	getEntityReimbursementMonthlyClaim <- function(EntityReimbursementMonthlyClaimID, EntityID = F, NumericDay = F, ApprovedCountFree = F, ApprovedCountReduced = F, PaidEligible = F, ParticipationFree = F, ParticipationReduced = F, ParticipationPaid = F, ReimbursementMonthID = F, MealType = F, ServedCountFree = F, ServedCountReduced = F, ServedCountPaid = F, ServedCountTotal = F, ServedCountOther = F, ServedCountCEP = F, fieldPaths = NULL, entityId = 1, schoolYearId = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment()) %>% purrr::keep(names(.) != "EntityReimbursementMonthlyClaimID")

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		getSkyObject(module = "FoodService", objectName = "EntityReimbursementMonthlyClaim", objectId = EntityReimbursementMonthlyClaimID, searchFields = searchFields, entityId = entityId, flatten = flatten, returnResponse = returnResponse)
	}

	#' Delete an EntityReimbursementMonthlyClaim
	#'
	#' This function deletes an EntityReimbursementMonthlyClaim
	#' @param EntityReimbursementMonthlyClaimID The ID of the EntityReimbursementMonthlyClaim to delete
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{listSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Food Service
	#' @return The EntityReimbursementMonthlyClaimID of the deleted EntityReimbursementMonthlyClaim.
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	deleteEntityReimbursementMonthlyClaim <- function(EntityReimbursementMonthlyClaimID, ignoreWarnings = F, entityId = 1, schoolYearId = NULL, flatten = T, returnResponse = F){

		deleteSkyObject(module = "FoodService", objectName = "EntityReimbursementMonthlyClaim", objectId = EntityReimbursementMonthlyClaimID, ignoreWarnings = ignoreWarnings, entityId = entityId, flatten = flatten, returnResponse = returnResponse)
	}

	#' Create an EntityReimbursementMonthlyClaim
	#'
	#' This function creates an EntityReimbursementMonthlyClaim
	#' @param fieldNames The field values to give the created EntityReimbursementMonthlyClaim. Each defaults to NULL.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{listSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Food Service
	#' @return A newly created EntityReimbursementMonthlyClaim
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	createEntityReimbursementMonthlyClaim <- function(NumericDay = NULL, ApprovedCountFree = NULL, ApprovedCountReduced = NULL, PaidEligible = NULL, ReimbursementMonthID = NULL, MealType = NULL, ServedCountFree = NULL, ServedCountReduced = NULL, ServedCountPaid = NULL, ServedCountTotal = NULL, ServedCountOther = NULL, ServedCountCEP = NULL, entityId = 1, schoolYearId = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment())

		body <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper()) %>% purrr::compact()

		createSkyObject(module = "FoodService", objectName = "EntityReimbursementMonthlyClaim", body = list(DataObject = body), searchFields = append("EntityReimbursementMonthlyClaimID", body %>% names()), entityId = entityId, schoolYearId = schoolYearId, flatten = flatten, returnResponse = returnResponse)
	}

	#' Modify an EntityReimbursementMonthlyClaim
	#'
	#' This function modifies an EntityReimbursementMonthlyClaim
	#' @param fieldNames The field values to give the modified EntityReimbursementMonthlyClaim. Each defaults to NULL.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{listSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Food Service
	#' @return The modified EntityReimbursementMonthlyClaim
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	modifyEntityReimbursementMonthlyClaim <- function(EntityReimbursementMonthlyClaimID, NumericDay = NULL, ApprovedCountFree = NULL, ApprovedCountReduced = NULL, PaidEligible = NULL, ReimbursementMonthID = NULL, MealType = NULL, ServedCountFree = NULL, ServedCountReduced = NULL, ServedCountPaid = NULL, ServedCountTotal = NULL, ServedCountOther = NULL, ServedCountCEP = NULL, entityId = 1, schoolYearId = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment())

		body <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper()) %>% purrr::compact()

		modifySkyObject(module = "FoodService", objectName = "EntityReimbursementMonthlyClaim", objectId = EntityReimbursementMonthlyClaimID, body = list(DataObject = body), searchFields = append("EntityReimbursementMonthlyClaimID", body %>% names()), entityId = entityId, schoolYearId = schoolYearId, flatten = flatten, returnResponse = returnResponse)
	}

	#' List ReimbursementMonthlyClaimRecordGenerators
	#'
	#' This function returns a dataframe or json object of ReimbursementMonthlyClaimRecordGenerators
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given ReimbursementMonthlyClaimRecordGenerators. Defaults to FALSE for all return fields which, for convenience, returns all fields for the ReimbursementMonthlyClaimRecordGenerators.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object listed as text. Run \code{\link{getSchemaForObjects}}('ReimbursementMonthlyClaimRecordGenerator') to get more field paths.
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
	#' @concept Food Service
	#' @return A list of ReimbursementMonthlyClaimRecordGenerators
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	listReimbursementMonthlyClaimRecordGenerators <- function(searchConditionsList = NULL, DistrictID = F, EntityID = F, StudentID = F, CustomerID = F, ReimbursementMonthID = F, PurchaseID = F, MealType = F, EligibilityType = F, TrackingType = F, NumericDay = F, IsAdultPurchase = F, ReimbursableMealTypeID = F, MilkCost = F, StateGradeLevelWAID = F, IsSevereNeeds = F, IsServedReducedKindergartenToGrade3 = F, IsSecondMeal = F, OtherRevenue = F, MealProgramTypeCode = F, SchoolID = F, fieldPaths = NULL, searchConditionsGroupType = "And", searchSortFieldNamesList = NULL, searchSortFieldNamesDescendingList = NULL, entityId = 1, schoolYearId = NULL, page = 1, pageSize = 100000, flatten = T, returnResponse = F){

		params <- as.list(environment())

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		listSkyObjects(module = "FoodService", objectName = "ReimbursementMonthlyClaimRecordGenerator", searchFields = searchFields %>% append(fieldPaths), page = page, pageSize = pageSize, SearchConditionsList = searchConditionsList, SearchConditionsGroupType = searchConditionsGroupType, SearchSortFieldNamesList = searchSortFieldNamesList, SearchSortFieldNamesDescendingList = searchSortFieldNamesDescendingList, entityId = entityId, schoolYearId = schoolYearId, flatten = flatten, returnResponse = returnResponse)
	}

	#' Get a ReimbursementMonthlyClaimRecordGenerator
	#'
	#' This function returns a dataframe or json object of a ReimbursementMonthlyClaimRecordGenerator
	#' @param ReimbursementMonthlyClaimRecordGeneratorID The ID of the ReimbursementMonthlyClaimRecordGenerator to return.
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given ReimbursementMonthlyClaimRecordGenerator. Defaults to FALSE for all return fields which, for convenience, returns all fields for the ReimbursementMonthlyClaimRecordGenerator.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object listed as text. Run \code{\link{getSchemaForObjects}}('ReimbursementMonthlyClaimRecordGenerator') to get more field paths.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{listSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Food Service
	#' @return A dataframe or of ReimbursementMonthlyClaimRecordGenerator
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	getReimbursementMonthlyClaimRecordGenerator <- function(ReimbursementMonthlyClaimRecordGeneratorID, DistrictID = F, EntityID = F, StudentID = F, CustomerID = F, ReimbursementMonthID = F, PurchaseID = F, MealType = F, EligibilityType = F, TrackingType = F, NumericDay = F, IsAdultPurchase = F, ReimbursableMealTypeID = F, MilkCost = F, StateGradeLevelWAID = F, IsSevereNeeds = F, IsServedReducedKindergartenToGrade3 = F, IsSecondMeal = F, OtherRevenue = F, MealProgramTypeCode = F, SchoolID = F, fieldPaths = NULL, entityId = 1, schoolYearId = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment()) %>% purrr::keep(names(.) != "ReimbursementMonthlyClaimRecordGeneratorID")

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		getSkyObject(module = "FoodService", objectName = "ReimbursementMonthlyClaimRecordGenerator", objectId = ReimbursementMonthlyClaimRecordGeneratorID, searchFields = searchFields, entityId = entityId, flatten = flatten, returnResponse = returnResponse)
	}

	#' Delete a ReimbursementMonthlyClaimRecordGenerator
	#'
	#' This function deletes a ReimbursementMonthlyClaimRecordGenerator
	#' @param ReimbursementMonthlyClaimRecordGeneratorID The ID of the ReimbursementMonthlyClaimRecordGenerator to delete
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{listSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Food Service
	#' @return The ReimbursementMonthlyClaimRecordGeneratorID of the deleted ReimbursementMonthlyClaimRecordGenerator.
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	deleteReimbursementMonthlyClaimRecordGenerator <- function(ReimbursementMonthlyClaimRecordGeneratorID, ignoreWarnings = F, entityId = 1, schoolYearId = NULL, flatten = T, returnResponse = F){

		deleteSkyObject(module = "FoodService", objectName = "ReimbursementMonthlyClaimRecordGenerator", objectId = ReimbursementMonthlyClaimRecordGeneratorID, ignoreWarnings = ignoreWarnings, entityId = entityId, flatten = flatten, returnResponse = returnResponse)
	}

	#' Create a ReimbursementMonthlyClaimRecordGenerator
	#'
	#' This function creates a ReimbursementMonthlyClaimRecordGenerator
	#' @param fieldNames The field values to give the created ReimbursementMonthlyClaimRecordGenerator. Each defaults to NULL.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{listSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Food Service
	#' @return A newly created ReimbursementMonthlyClaimRecordGenerator
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	createReimbursementMonthlyClaimRecordGenerator <- function(EntityID = NULL, StudentID = NULL, CustomerID = NULL, ReimbursementMonthID = NULL, PurchaseID = NULL, MealType = NULL, EligibilityType = NULL, TrackingType = NULL, NumericDay = NULL, IsAdultPurchase = NULL, ReimbursableMealTypeID = NULL, MilkCost = NULL, StateGradeLevelWAID = NULL, IsSevereNeeds = NULL, IsServedReducedKindergartenToGrade3 = NULL, IsSecondMeal = NULL, OtherRevenue = NULL, MealProgramTypeCode = NULL, SchoolID = NULL, entityId = 1, schoolYearId = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment())

		body <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper()) %>% purrr::compact()

		createSkyObject(module = "FoodService", objectName = "ReimbursementMonthlyClaimRecordGenerator", body = list(DataObject = body), searchFields = append("ReimbursementMonthlyClaimRecordGeneratorID", body %>% names()), entityId = entityId, schoolYearId = schoolYearId, flatten = flatten, returnResponse = returnResponse)
	}

	#' Modify a ReimbursementMonthlyClaimRecordGenerator
	#'
	#' This function modifies a ReimbursementMonthlyClaimRecordGenerator
	#' @param fieldNames The field values to give the modified ReimbursementMonthlyClaimRecordGenerator. Each defaults to NULL.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{listSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Food Service
	#' @return The modified ReimbursementMonthlyClaimRecordGenerator
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	modifyReimbursementMonthlyClaimRecordGenerator <- function(ReimbursementMonthlyClaimRecordGeneratorID, EntityID = NULL, StudentID = NULL, CustomerID = NULL, ReimbursementMonthID = NULL, PurchaseID = NULL, MealType = NULL, EligibilityType = NULL, TrackingType = NULL, NumericDay = NULL, IsAdultPurchase = NULL, ReimbursableMealTypeID = NULL, MilkCost = NULL, StateGradeLevelWAID = NULL, IsSevereNeeds = NULL, IsServedReducedKindergartenToGrade3 = NULL, IsSecondMeal = NULL, OtherRevenue = NULL, MealProgramTypeCode = NULL, SchoolID = NULL, entityId = 1, schoolYearId = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment())

		body <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper()) %>% purrr::compact()

		modifySkyObject(module = "FoodService", objectName = "ReimbursementMonthlyClaimRecordGenerator", objectId = ReimbursementMonthlyClaimRecordGeneratorID, body = list(DataObject = body), searchFields = append("ReimbursementMonthlyClaimRecordGeneratorID", body %>% names()), entityId = entityId, schoolYearId = schoolYearId, flatten = flatten, returnResponse = returnResponse)
	}

	#' List StateMealProgramTypes
	#'
	#' This function returns a dataframe or json object of StateMealProgramTypes
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given StateMealProgramTypes. Defaults to FALSE for all return fields which, for convenience, returns all fields for the StateMealProgramTypes.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object listed as text. Run \code{\link{getSchemaForObjects}}('StateMealProgramType') to get more field paths.
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
	#' @concept Food Service
	#' @return A list of StateMealProgramTypes
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	listStateMealProgramTypes <- function(searchConditionsList = NULL, StateMealProgramTypeID = F, Code = F, Description = F, CodeDescription = F, SkywardID = F, SkywardHash = F, ValidYearLow = F, ValidYearHigh = F, IsInvalid = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, fieldPaths = NULL, searchConditionsGroupType = "And", searchSortFieldNamesList = NULL, searchSortFieldNamesDescendingList = NULL, entityId = 1, schoolYearId = NULL, page = 1, pageSize = 100000, flatten = T, returnResponse = F){

		params <- as.list(environment())

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		listSkyObjects(module = "FoodService", objectName = "StateMealProgramType", searchFields = searchFields %>% append(fieldPaths), page = page, pageSize = pageSize, SearchConditionsList = searchConditionsList, SearchConditionsGroupType = searchConditionsGroupType, SearchSortFieldNamesList = searchSortFieldNamesList, SearchSortFieldNamesDescendingList = searchSortFieldNamesDescendingList, entityId = entityId, schoolYearId = schoolYearId, flatten = flatten, returnResponse = returnResponse)
	}

	#' Get a StateMealProgramType
	#'
	#' This function returns a dataframe or json object of a StateMealProgramType
	#' @param StateMealProgramTypeID The ID of the StateMealProgramType to return.
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given StateMealProgramType. Defaults to FALSE for all return fields which, for convenience, returns all fields for the StateMealProgramType.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object listed as text. Run \code{\link{getSchemaForObjects}}('StateMealProgramType') to get more field paths.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{listSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Food Service
	#' @return A dataframe or of StateMealProgramType
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	getStateMealProgramType <- function(StateMealProgramTypeID, Code = F, Description = F, CodeDescription = F, SkywardID = F, SkywardHash = F, ValidYearLow = F, ValidYearHigh = F, IsInvalid = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, fieldPaths = NULL, entityId = 1, schoolYearId = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment()) %>% purrr::keep(names(.) != "StateMealProgramTypeID")

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		getSkyObject(module = "FoodService", objectName = "StateMealProgramType", objectId = StateMealProgramTypeID, searchFields = searchFields, entityId = entityId, flatten = flatten, returnResponse = returnResponse)
	}

	#' Delete a StateMealProgramType
	#'
	#' This function deletes a StateMealProgramType
	#' @param StateMealProgramTypeID The ID of the StateMealProgramType to delete
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{listSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Food Service
	#' @return The StateMealProgramTypeID of the deleted StateMealProgramType.
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	deleteStateMealProgramType <- function(StateMealProgramTypeID, ignoreWarnings = F, entityId = 1, schoolYearId = NULL, flatten = T, returnResponse = F){

		deleteSkyObject(module = "FoodService", objectName = "StateMealProgramType", objectId = StateMealProgramTypeID, ignoreWarnings = ignoreWarnings, entityId = entityId, flatten = flatten, returnResponse = returnResponse)
	}

	#' Create a StateMealProgramType
	#'
	#' This function creates a StateMealProgramType
	#' @param fieldNames The field values to give the created StateMealProgramType. Each defaults to NULL.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{listSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Food Service
	#' @return A newly created StateMealProgramType
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	createStateMealProgramType <- function(Code = NULL, Description = NULL, ValidYearLow = NULL, ValidYearHigh = NULL, IsInvalid = NULL, entityId = 1, schoolYearId = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment())

		body <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper()) %>% purrr::compact()

		createSkyObject(module = "FoodService", objectName = "StateMealProgramType", body = list(DataObject = body), searchFields = append("StateMealProgramTypeID", body %>% names()), entityId = entityId, schoolYearId = schoolYearId, flatten = flatten, returnResponse = returnResponse)
	}

	#' Modify a StateMealProgramType
	#'
	#' This function modifies a StateMealProgramType
	#' @param fieldNames The field values to give the modified StateMealProgramType. Each defaults to NULL.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{listSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Food Service
	#' @return The modified StateMealProgramType
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	modifyStateMealProgramType <- function(StateMealProgramTypeID, Code = NULL, Description = NULL, ValidYearLow = NULL, ValidYearHigh = NULL, IsInvalid = NULL, entityId = 1, schoolYearId = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment())

		body <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper()) %>% purrr::compact()

		modifySkyObject(module = "FoodService", objectName = "StateMealProgramType", objectId = StateMealProgramTypeID, body = list(DataObject = body), searchFields = append("StateMealProgramTypeID", body %>% names()), entityId = entityId, schoolYearId = schoolYearId, flatten = flatten, returnResponse = returnResponse)
	}

	#' List TempWarningCustomers
	#'
	#' This function returns a dataframe or json object of TempWarningCustomers
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given TempWarningCustomers. Defaults to FALSE for all return fields which, for convenience, returns all fields for the TempWarningCustomers.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object listed as text. Run \code{\link{getSchemaForObjects}}('TempWarningCustomer') to get more field paths.
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
	#' @concept Food Service
	#' @return A list of TempWarningCustomers
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	listTempWarningCustomers <- function(searchConditionsList = NULL, TempWarningCustomerID = F, Note = F, FullNameLFM = F, StudentNumber = F, StaffNumber = F, Balance = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, fieldPaths = NULL, searchConditionsGroupType = "And", searchSortFieldNamesList = NULL, searchSortFieldNamesDescendingList = NULL, entityId = 1, schoolYearId = NULL, page = 1, pageSize = 100000, flatten = T, returnResponse = F){

		params <- as.list(environment())

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		listSkyObjects(module = "FoodService", objectName = "TempWarningCustomer", searchFields = searchFields %>% append(fieldPaths), page = page, pageSize = pageSize, SearchConditionsList = searchConditionsList, SearchConditionsGroupType = searchConditionsGroupType, SearchSortFieldNamesList = searchSortFieldNamesList, SearchSortFieldNamesDescendingList = searchSortFieldNamesDescendingList, entityId = entityId, schoolYearId = schoolYearId, flatten = flatten, returnResponse = returnResponse)
	}

	#' Get a TempWarningCustomer
	#'
	#' This function returns a dataframe or json object of a TempWarningCustomer
	#' @param TempWarningCustomerID The ID of the TempWarningCustomer to return.
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given TempWarningCustomer. Defaults to FALSE for all return fields which, for convenience, returns all fields for the TempWarningCustomer.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object listed as text. Run \code{\link{getSchemaForObjects}}('TempWarningCustomer') to get more field paths.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{listSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Food Service
	#' @return A dataframe or of TempWarningCustomer
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	getTempWarningCustomer <- function(TempWarningCustomerID, Note = F, FullNameLFM = F, StudentNumber = F, StaffNumber = F, Balance = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, fieldPaths = NULL, entityId = 1, schoolYearId = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment()) %>% purrr::keep(names(.) != "TempWarningCustomerID")

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		getSkyObject(module = "FoodService", objectName = "TempWarningCustomer", objectId = TempWarningCustomerID, searchFields = searchFields, entityId = entityId, flatten = flatten, returnResponse = returnResponse)
	}

	#' Delete a TempWarningCustomer
	#'
	#' This function deletes a TempWarningCustomer
	#' @param TempWarningCustomerID The ID of the TempWarningCustomer to delete
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{listSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Food Service
	#' @return The TempWarningCustomerID of the deleted TempWarningCustomer.
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	deleteTempWarningCustomer <- function(TempWarningCustomerID, ignoreWarnings = F, entityId = 1, schoolYearId = NULL, flatten = T, returnResponse = F){

		deleteSkyObject(module = "FoodService", objectName = "TempWarningCustomer", objectId = TempWarningCustomerID, ignoreWarnings = ignoreWarnings, entityId = entityId, flatten = flatten, returnResponse = returnResponse)
	}

	#' Create a TempWarningCustomer
	#'
	#' This function creates a TempWarningCustomer
	#' @param fieldNames The field values to give the created TempWarningCustomer. Each defaults to NULL.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{listSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Food Service
	#' @return A newly created TempWarningCustomer
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	createTempWarningCustomer <- function(Note = NULL, FullNameLFM = NULL, StudentNumber = NULL, StaffNumber = NULL, Balance = NULL, entityId = 1, schoolYearId = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment())

		body <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper()) %>% purrr::compact()

		createSkyObject(module = "FoodService", objectName = "TempWarningCustomer", body = list(DataObject = body), searchFields = append("TempWarningCustomerID", body %>% names()), entityId = entityId, schoolYearId = schoolYearId, flatten = flatten, returnResponse = returnResponse)
	}

	#' Modify a TempWarningCustomer
	#'
	#' This function modifies a TempWarningCustomer
	#' @param fieldNames The field values to give the modified TempWarningCustomer. Each defaults to NULL.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{listSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Food Service
	#' @return The modified TempWarningCustomer
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	modifyTempWarningCustomer <- function(TempWarningCustomerID, Note = NULL, FullNameLFM = NULL, StudentNumber = NULL, StaffNumber = NULL, Balance = NULL, entityId = 1, schoolYearId = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment())

		body <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper()) %>% purrr::compact()

		modifySkyObject(module = "FoodService", objectName = "TempWarningCustomer", objectId = TempWarningCustomerID, body = list(DataObject = body), searchFields = append("TempWarningCustomerID", body %>% names()), entityId = entityId, schoolYearId = schoolYearId, flatten = flatten, returnResponse = returnResponse)
	}

	#' List ReimbursementMonthlyClaimByEntitySchools
	#'
	#' This function returns a dataframe or json object of ReimbursementMonthlyClaimByEntitySchools
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given ReimbursementMonthlyClaimByEntitySchools. Defaults to FALSE for all return fields which, for convenience, returns all fields for the ReimbursementMonthlyClaimByEntitySchools.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object listed as text. Run \code{\link{getSchemaForObjects}}('ReimbursementMonthlyClaimByEntitySchool') to get more field paths.
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
	#' @concept Food Service
	#' @return A list of ReimbursementMonthlyClaimByEntitySchools
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	listReimbursementMonthlyClaimByEntitySchools <- function(searchConditionsList = NULL, SchoolID = F, DistrictID = F, EntityID = F, ApprovedCountFree = F, ApprovedCountReduced = F, PaidEligible = F, ReimbursementMonthID = F, MealType = F, ServedCountFree = F, ServedCountReduced = F, ServedCountPaid = F, ServedCountTotal = F, ServedCountOther = F, fieldPaths = NULL, searchConditionsGroupType = "And", searchSortFieldNamesList = NULL, searchSortFieldNamesDescendingList = NULL, entityId = 1, schoolYearId = NULL, page = 1, pageSize = 100000, flatten = T, returnResponse = F){

		params <- as.list(environment())

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		listSkyObjects(module = "FoodService", objectName = "ReimbursementMonthlyClaimByEntitySchool", searchFields = searchFields %>% append(fieldPaths), page = page, pageSize = pageSize, SearchConditionsList = searchConditionsList, SearchConditionsGroupType = searchConditionsGroupType, SearchSortFieldNamesList = searchSortFieldNamesList, SearchSortFieldNamesDescendingList = searchSortFieldNamesDescendingList, entityId = entityId, schoolYearId = schoolYearId, flatten = flatten, returnResponse = returnResponse)
	}

	#' Get a ReimbursementMonthlyClaimByEntitySchool
	#'
	#' This function returns a dataframe or json object of a ReimbursementMonthlyClaimByEntitySchool
	#' @param ReimbursementMonthlyClaimByEntitySchoolID The ID of the ReimbursementMonthlyClaimByEntitySchool to return.
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given ReimbursementMonthlyClaimByEntitySchool. Defaults to FALSE for all return fields which, for convenience, returns all fields for the ReimbursementMonthlyClaimByEntitySchool.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object listed as text. Run \code{\link{getSchemaForObjects}}('ReimbursementMonthlyClaimByEntitySchool') to get more field paths.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{listSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Food Service
	#' @return A dataframe or of ReimbursementMonthlyClaimByEntitySchool
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	getReimbursementMonthlyClaimByEntitySchool <- function(ReimbursementMonthlyClaimByEntitySchoolID, SchoolID = F, DistrictID = F, EntityID = F, ApprovedCountFree = F, ApprovedCountReduced = F, PaidEligible = F, ReimbursementMonthID = F, MealType = F, ServedCountFree = F, ServedCountReduced = F, ServedCountPaid = F, ServedCountTotal = F, ServedCountOther = F, fieldPaths = NULL, entityId = 1, schoolYearId = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment()) %>% purrr::keep(names(.) != "ReimbursementMonthlyClaimByEntitySchoolID")

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		getSkyObject(module = "FoodService", objectName = "ReimbursementMonthlyClaimByEntitySchool", objectId = ReimbursementMonthlyClaimByEntitySchoolID, searchFields = searchFields, entityId = entityId, flatten = flatten, returnResponse = returnResponse)
	}

	#' Delete a ReimbursementMonthlyClaimByEntitySchool
	#'
	#' This function deletes a ReimbursementMonthlyClaimByEntitySchool
	#' @param ReimbursementMonthlyClaimByEntitySchoolID The ID of the ReimbursementMonthlyClaimByEntitySchool to delete
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{listSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Food Service
	#' @return The ReimbursementMonthlyClaimByEntitySchoolID of the deleted ReimbursementMonthlyClaimByEntitySchool.
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	deleteReimbursementMonthlyClaimByEntitySchool <- function(ReimbursementMonthlyClaimByEntitySchoolID, ignoreWarnings = F, entityId = 1, schoolYearId = NULL, flatten = T, returnResponse = F){

		deleteSkyObject(module = "FoodService", objectName = "ReimbursementMonthlyClaimByEntitySchool", objectId = ReimbursementMonthlyClaimByEntitySchoolID, ignoreWarnings = ignoreWarnings, entityId = entityId, flatten = flatten, returnResponse = returnResponse)
	}

	#' Create a ReimbursementMonthlyClaimByEntitySchool
	#'
	#' This function creates a ReimbursementMonthlyClaimByEntitySchool
	#' @param fieldNames The field values to give the created ReimbursementMonthlyClaimByEntitySchool. Each defaults to NULL.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{listSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Food Service
	#' @return A newly created ReimbursementMonthlyClaimByEntitySchool
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	createReimbursementMonthlyClaimByEntitySchool <- function(DistrictID = NULL, EntityID = NULL, ApprovedCountFree = NULL, ApprovedCountReduced = NULL, PaidEligible = NULL, ReimbursementMonthID = NULL, MealType = NULL, ServedCountFree = NULL, ServedCountReduced = NULL, ServedCountPaid = NULL, ServedCountTotal = NULL, ServedCountOther = NULL, entityId = 1, schoolYearId = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment())

		body <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper()) %>% purrr::compact()

		createSkyObject(module = "FoodService", objectName = "ReimbursementMonthlyClaimByEntitySchool", body = list(DataObject = body), searchFields = append("ReimbursementMonthlyClaimByEntitySchoolID", body %>% names()), entityId = entityId, schoolYearId = schoolYearId, flatten = flatten, returnResponse = returnResponse)
	}

	#' Modify a ReimbursementMonthlyClaimByEntitySchool
	#'
	#' This function modifies a ReimbursementMonthlyClaimByEntitySchool
	#' @param fieldNames The field values to give the modified ReimbursementMonthlyClaimByEntitySchool. Each defaults to NULL.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{listSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Food Service
	#' @return The modified ReimbursementMonthlyClaimByEntitySchool
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	modifyReimbursementMonthlyClaimByEntitySchool <- function(ReimbursementMonthlyClaimByEntitySchoolID, DistrictID = NULL, EntityID = NULL, ApprovedCountFree = NULL, ApprovedCountReduced = NULL, PaidEligible = NULL, ReimbursementMonthID = NULL, MealType = NULL, ServedCountFree = NULL, ServedCountReduced = NULL, ServedCountPaid = NULL, ServedCountTotal = NULL, ServedCountOther = NULL, entityId = 1, schoolYearId = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment())

		body <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper()) %>% purrr::compact()

		modifySkyObject(module = "FoodService", objectName = "ReimbursementMonthlyClaimByEntitySchool", objectId = ReimbursementMonthlyClaimByEntitySchoolID, body = list(DataObject = body), searchFields = append("ReimbursementMonthlyClaimByEntitySchoolID", body %>% names()), entityId = entityId, schoolYearId = schoolYearId, flatten = flatten, returnResponse = returnResponse)
	}

	#' List ReimbursementMonthlyClaimBySchools
	#'
	#' This function returns a dataframe or json object of ReimbursementMonthlyClaimBySchools
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given ReimbursementMonthlyClaimBySchools. Defaults to FALSE for all return fields which, for convenience, returns all fields for the ReimbursementMonthlyClaimBySchools.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object listed as text. Run \code{\link{getSchemaForObjects}}('ReimbursementMonthlyClaimBySchool') to get more field paths.
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
	#' @concept Food Service
	#' @return A list of ReimbursementMonthlyClaimBySchools
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	listReimbursementMonthlyClaimBySchools <- function(searchConditionsList = NULL, SchoolID = F, DistrictID = F, ApprovedCountFree = F, ApprovedCountReduced = F, PaidEligible = F, ReimbursementMonthID = F, MealType = F, ServedCountFree = F, ServedCountReduced = F, ServedCountPaid = F, ServedCountTotal = F, ServedCountOther = F, fieldPaths = NULL, searchConditionsGroupType = "And", searchSortFieldNamesList = NULL, searchSortFieldNamesDescendingList = NULL, entityId = 1, schoolYearId = NULL, page = 1, pageSize = 100000, flatten = T, returnResponse = F){

		params <- as.list(environment())

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		listSkyObjects(module = "FoodService", objectName = "ReimbursementMonthlyClaimBySchool", searchFields = searchFields %>% append(fieldPaths), page = page, pageSize = pageSize, SearchConditionsList = searchConditionsList, SearchConditionsGroupType = searchConditionsGroupType, SearchSortFieldNamesList = searchSortFieldNamesList, SearchSortFieldNamesDescendingList = searchSortFieldNamesDescendingList, entityId = entityId, schoolYearId = schoolYearId, flatten = flatten, returnResponse = returnResponse)
	}

	#' Get a ReimbursementMonthlyClaimBySchool
	#'
	#' This function returns a dataframe or json object of a ReimbursementMonthlyClaimBySchool
	#' @param ReimbursementMonthlyClaimBySchoolID The ID of the ReimbursementMonthlyClaimBySchool to return.
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given ReimbursementMonthlyClaimBySchool. Defaults to FALSE for all return fields which, for convenience, returns all fields for the ReimbursementMonthlyClaimBySchool.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object listed as text. Run \code{\link{getSchemaForObjects}}('ReimbursementMonthlyClaimBySchool') to get more field paths.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{listSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Food Service
	#' @return A dataframe or of ReimbursementMonthlyClaimBySchool
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	getReimbursementMonthlyClaimBySchool <- function(ReimbursementMonthlyClaimBySchoolID, SchoolID = F, DistrictID = F, ApprovedCountFree = F, ApprovedCountReduced = F, PaidEligible = F, ReimbursementMonthID = F, MealType = F, ServedCountFree = F, ServedCountReduced = F, ServedCountPaid = F, ServedCountTotal = F, ServedCountOther = F, fieldPaths = NULL, entityId = 1, schoolYearId = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment()) %>% purrr::keep(names(.) != "ReimbursementMonthlyClaimBySchoolID")

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		getSkyObject(module = "FoodService", objectName = "ReimbursementMonthlyClaimBySchool", objectId = ReimbursementMonthlyClaimBySchoolID, searchFields = searchFields, entityId = entityId, flatten = flatten, returnResponse = returnResponse)
	}

	#' Delete a ReimbursementMonthlyClaimBySchool
	#'
	#' This function deletes a ReimbursementMonthlyClaimBySchool
	#' @param ReimbursementMonthlyClaimBySchoolID The ID of the ReimbursementMonthlyClaimBySchool to delete
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{listSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Food Service
	#' @return The ReimbursementMonthlyClaimBySchoolID of the deleted ReimbursementMonthlyClaimBySchool.
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	deleteReimbursementMonthlyClaimBySchool <- function(ReimbursementMonthlyClaimBySchoolID, ignoreWarnings = F, entityId = 1, schoolYearId = NULL, flatten = T, returnResponse = F){

		deleteSkyObject(module = "FoodService", objectName = "ReimbursementMonthlyClaimBySchool", objectId = ReimbursementMonthlyClaimBySchoolID, ignoreWarnings = ignoreWarnings, entityId = entityId, flatten = flatten, returnResponse = returnResponse)
	}

	#' Create a ReimbursementMonthlyClaimBySchool
	#'
	#' This function creates a ReimbursementMonthlyClaimBySchool
	#' @param fieldNames The field values to give the created ReimbursementMonthlyClaimBySchool. Each defaults to NULL.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{listSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Food Service
	#' @return A newly created ReimbursementMonthlyClaimBySchool
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	createReimbursementMonthlyClaimBySchool <- function(DistrictID = NULL, ApprovedCountFree = NULL, ApprovedCountReduced = NULL, PaidEligible = NULL, ReimbursementMonthID = NULL, MealType = NULL, ServedCountFree = NULL, ServedCountReduced = NULL, ServedCountPaid = NULL, ServedCountTotal = NULL, ServedCountOther = NULL, entityId = 1, schoolYearId = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment())

		body <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper()) %>% purrr::compact()

		createSkyObject(module = "FoodService", objectName = "ReimbursementMonthlyClaimBySchool", body = list(DataObject = body), searchFields = append("ReimbursementMonthlyClaimBySchoolID", body %>% names()), entityId = entityId, schoolYearId = schoolYearId, flatten = flatten, returnResponse = returnResponse)
	}

	#' Modify a ReimbursementMonthlyClaimBySchool
	#'
	#' This function modifies a ReimbursementMonthlyClaimBySchool
	#' @param fieldNames The field values to give the modified ReimbursementMonthlyClaimBySchool. Each defaults to NULL.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{listSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Food Service
	#' @return The modified ReimbursementMonthlyClaimBySchool
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	modifyReimbursementMonthlyClaimBySchool <- function(ReimbursementMonthlyClaimBySchoolID, DistrictID = NULL, ApprovedCountFree = NULL, ApprovedCountReduced = NULL, PaidEligible = NULL, ReimbursementMonthID = NULL, MealType = NULL, ServedCountFree = NULL, ServedCountReduced = NULL, ServedCountPaid = NULL, ServedCountTotal = NULL, ServedCountOther = NULL, entityId = 1, schoolYearId = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment())

		body <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper()) %>% purrr::compact()

		modifySkyObject(module = "FoodService", objectName = "ReimbursementMonthlyClaimBySchool", objectId = ReimbursementMonthlyClaimBySchoolID, body = list(DataObject = body), searchFields = append("ReimbursementMonthlyClaimBySchoolID", body %>% names()), entityId = entityId, schoolYearId = schoolYearId, flatten = flatten, returnResponse = returnResponse)
	}

	#' List DistrictSchoolReimbursementMonthlyClaims
	#'
	#' This function returns a dataframe or json object of DistrictSchoolReimbursementMonthlyClaims
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given DistrictSchoolReimbursementMonthlyClaims. Defaults to FALSE for all return fields which, for convenience, returns all fields for the DistrictSchoolReimbursementMonthlyClaims.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object listed as text. Run \code{\link{getSchemaForObjects}}('DistrictSchoolReimbursementMonthlyClaim') to get more field paths.
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
	#' @concept Food Service
	#' @return A list of DistrictSchoolReimbursementMonthlyClaims
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	listDistrictSchoolReimbursementMonthlyClaims <- function(searchConditionsList = NULL, SchoolID = F, DistrictID = F, NumericDay = F, ServedCountCEP = F, ApprovedCountFree = F, ApprovedCountReduced = F, PaidEligible = F, ParticipationFree = F, ParticipationReduced = F, ParticipationPaid = F, ReimbursementMonthID = F, MealType = F, ServedCountFree = F, ServedCountReduced = F, ServedCountPaid = F, ServedCountTotal = F, ServedCountOther = F, fieldPaths = NULL, searchConditionsGroupType = "And", searchSortFieldNamesList = NULL, searchSortFieldNamesDescendingList = NULL, entityId = 1, schoolYearId = NULL, page = 1, pageSize = 100000, flatten = T, returnResponse = F){

		params <- as.list(environment())

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		listSkyObjects(module = "FoodService", objectName = "DistrictSchoolReimbursementMonthlyClaim", searchFields = searchFields %>% append(fieldPaths), page = page, pageSize = pageSize, SearchConditionsList = searchConditionsList, SearchConditionsGroupType = searchConditionsGroupType, SearchSortFieldNamesList = searchSortFieldNamesList, SearchSortFieldNamesDescendingList = searchSortFieldNamesDescendingList, entityId = entityId, schoolYearId = schoolYearId, flatten = flatten, returnResponse = returnResponse)
	}

	#' Get a DistrictSchoolReimbursementMonthlyClaim
	#'
	#' This function returns a dataframe or json object of a DistrictSchoolReimbursementMonthlyClaim
	#' @param DistrictSchoolReimbursementMonthlyClaimID The ID of the DistrictSchoolReimbursementMonthlyClaim to return.
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given DistrictSchoolReimbursementMonthlyClaim. Defaults to FALSE for all return fields which, for convenience, returns all fields for the DistrictSchoolReimbursementMonthlyClaim.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object listed as text. Run \code{\link{getSchemaForObjects}}('DistrictSchoolReimbursementMonthlyClaim') to get more field paths.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{listSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Food Service
	#' @return A dataframe or of DistrictSchoolReimbursementMonthlyClaim
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	getDistrictSchoolReimbursementMonthlyClaim <- function(DistrictSchoolReimbursementMonthlyClaimID, SchoolID = F, DistrictID = F, NumericDay = F, ServedCountCEP = F, ApprovedCountFree = F, ApprovedCountReduced = F, PaidEligible = F, ParticipationFree = F, ParticipationReduced = F, ParticipationPaid = F, ReimbursementMonthID = F, MealType = F, ServedCountFree = F, ServedCountReduced = F, ServedCountPaid = F, ServedCountTotal = F, ServedCountOther = F, fieldPaths = NULL, entityId = 1, schoolYearId = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment()) %>% purrr::keep(names(.) != "DistrictSchoolReimbursementMonthlyClaimID")

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		getSkyObject(module = "FoodService", objectName = "DistrictSchoolReimbursementMonthlyClaim", objectId = DistrictSchoolReimbursementMonthlyClaimID, searchFields = searchFields, entityId = entityId, flatten = flatten, returnResponse = returnResponse)
	}

	#' Delete a DistrictSchoolReimbursementMonthlyClaim
	#'
	#' This function deletes a DistrictSchoolReimbursementMonthlyClaim
	#' @param DistrictSchoolReimbursementMonthlyClaimID The ID of the DistrictSchoolReimbursementMonthlyClaim to delete
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{listSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Food Service
	#' @return The DistrictSchoolReimbursementMonthlyClaimID of the deleted DistrictSchoolReimbursementMonthlyClaim.
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	deleteDistrictSchoolReimbursementMonthlyClaim <- function(DistrictSchoolReimbursementMonthlyClaimID, ignoreWarnings = F, entityId = 1, schoolYearId = NULL, flatten = T, returnResponse = F){

		deleteSkyObject(module = "FoodService", objectName = "DistrictSchoolReimbursementMonthlyClaim", objectId = DistrictSchoolReimbursementMonthlyClaimID, ignoreWarnings = ignoreWarnings, entityId = entityId, flatten = flatten, returnResponse = returnResponse)
	}

	#' Create a DistrictSchoolReimbursementMonthlyClaim
	#'
	#' This function creates a DistrictSchoolReimbursementMonthlyClaim
	#' @param fieldNames The field values to give the created DistrictSchoolReimbursementMonthlyClaim. Each defaults to NULL.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{listSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Food Service
	#' @return A newly created DistrictSchoolReimbursementMonthlyClaim
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	createDistrictSchoolReimbursementMonthlyClaim <- function(DistrictID = NULL, NumericDay = NULL, ServedCountCEP = NULL, ApprovedCountFree = NULL, ApprovedCountReduced = NULL, PaidEligible = NULL, ReimbursementMonthID = NULL, MealType = NULL, ServedCountFree = NULL, ServedCountReduced = NULL, ServedCountPaid = NULL, ServedCountTotal = NULL, ServedCountOther = NULL, entityId = 1, schoolYearId = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment())

		body <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper()) %>% purrr::compact()

		createSkyObject(module = "FoodService", objectName = "DistrictSchoolReimbursementMonthlyClaim", body = list(DataObject = body), searchFields = append("DistrictSchoolReimbursementMonthlyClaimID", body %>% names()), entityId = entityId, schoolYearId = schoolYearId, flatten = flatten, returnResponse = returnResponse)
	}

	#' Modify a DistrictSchoolReimbursementMonthlyClaim
	#'
	#' This function modifies a DistrictSchoolReimbursementMonthlyClaim
	#' @param fieldNames The field values to give the modified DistrictSchoolReimbursementMonthlyClaim. Each defaults to NULL.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{listSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Food Service
	#' @return The modified DistrictSchoolReimbursementMonthlyClaim
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	modifyDistrictSchoolReimbursementMonthlyClaim <- function(DistrictSchoolReimbursementMonthlyClaimID, DistrictID = NULL, NumericDay = NULL, ServedCountCEP = NULL, ApprovedCountFree = NULL, ApprovedCountReduced = NULL, PaidEligible = NULL, ReimbursementMonthID = NULL, MealType = NULL, ServedCountFree = NULL, ServedCountReduced = NULL, ServedCountPaid = NULL, ServedCountTotal = NULL, ServedCountOther = NULL, entityId = 1, schoolYearId = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment())

		body <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper()) %>% purrr::compact()

		modifySkyObject(module = "FoodService", objectName = "DistrictSchoolReimbursementMonthlyClaim", objectId = DistrictSchoolReimbursementMonthlyClaimID, body = list(DataObject = body), searchFields = append("DistrictSchoolReimbursementMonthlyClaimID", body %>% names()), entityId = entityId, schoolYearId = schoolYearId, flatten = flatten, returnResponse = returnResponse)
	}

	#' List EntitySchoolReimbursementMonthlyClaims
	#'
	#' This function returns a dataframe or json object of EntitySchoolReimbursementMonthlyClaims
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given EntitySchoolReimbursementMonthlyClaims. Defaults to FALSE for all return fields which, for convenience, returns all fields for the EntitySchoolReimbursementMonthlyClaims.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object listed as text. Run \code{\link{getSchemaForObjects}}('EntitySchoolReimbursementMonthlyClaim') to get more field paths.
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
	#' @concept Food Service
	#' @return A list of EntitySchoolReimbursementMonthlyClaims
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	listEntitySchoolReimbursementMonthlyClaims <- function(searchConditionsList = NULL, SchoolID = F, EntityID = F, NumericDay = F, ServedCountCEP = F, ApprovedCountFree = F, ApprovedCountReduced = F, PaidEligible = F, ParticipationFree = F, ParticipationReduced = F, ParticipationPaid = F, ReimbursementMonthID = F, MealType = F, ServedCountFree = F, ServedCountReduced = F, ServedCountPaid = F, ServedCountTotal = F, ServedCountOther = F, fieldPaths = NULL, searchConditionsGroupType = "And", searchSortFieldNamesList = NULL, searchSortFieldNamesDescendingList = NULL, entityId = 1, schoolYearId = NULL, page = 1, pageSize = 100000, flatten = T, returnResponse = F){

		params <- as.list(environment())

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		listSkyObjects(module = "FoodService", objectName = "EntitySchoolReimbursementMonthlyClaim", searchFields = searchFields %>% append(fieldPaths), page = page, pageSize = pageSize, SearchConditionsList = searchConditionsList, SearchConditionsGroupType = searchConditionsGroupType, SearchSortFieldNamesList = searchSortFieldNamesList, SearchSortFieldNamesDescendingList = searchSortFieldNamesDescendingList, entityId = entityId, schoolYearId = schoolYearId, flatten = flatten, returnResponse = returnResponse)
	}

	#' Get an EntitySchoolReimbursementMonthlyClaim
	#'
	#' This function returns a dataframe or json object of an EntitySchoolReimbursementMonthlyClaim
	#' @param EntitySchoolReimbursementMonthlyClaimID The ID of the EntitySchoolReimbursementMonthlyClaim to return.
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given EntitySchoolReimbursementMonthlyClaim. Defaults to FALSE for all return fields which, for convenience, returns all fields for the EntitySchoolReimbursementMonthlyClaim.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object listed as text. Run \code{\link{getSchemaForObjects}}('EntitySchoolReimbursementMonthlyClaim') to get more field paths.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{listSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Food Service
	#' @return A dataframe or of EntitySchoolReimbursementMonthlyClaim
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	getEntitySchoolReimbursementMonthlyClaim <- function(EntitySchoolReimbursementMonthlyClaimID, SchoolID = F, EntityID = F, NumericDay = F, ServedCountCEP = F, ApprovedCountFree = F, ApprovedCountReduced = F, PaidEligible = F, ParticipationFree = F, ParticipationReduced = F, ParticipationPaid = F, ReimbursementMonthID = F, MealType = F, ServedCountFree = F, ServedCountReduced = F, ServedCountPaid = F, ServedCountTotal = F, ServedCountOther = F, fieldPaths = NULL, entityId = 1, schoolYearId = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment()) %>% purrr::keep(names(.) != "EntitySchoolReimbursementMonthlyClaimID")

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		getSkyObject(module = "FoodService", objectName = "EntitySchoolReimbursementMonthlyClaim", objectId = EntitySchoolReimbursementMonthlyClaimID, searchFields = searchFields, entityId = entityId, flatten = flatten, returnResponse = returnResponse)
	}

	#' Delete an EntitySchoolReimbursementMonthlyClaim
	#'
	#' This function deletes an EntitySchoolReimbursementMonthlyClaim
	#' @param EntitySchoolReimbursementMonthlyClaimID The ID of the EntitySchoolReimbursementMonthlyClaim to delete
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{listSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Food Service
	#' @return The EntitySchoolReimbursementMonthlyClaimID of the deleted EntitySchoolReimbursementMonthlyClaim.
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	deleteEntitySchoolReimbursementMonthlyClaim <- function(EntitySchoolReimbursementMonthlyClaimID, ignoreWarnings = F, entityId = 1, schoolYearId = NULL, flatten = T, returnResponse = F){

		deleteSkyObject(module = "FoodService", objectName = "EntitySchoolReimbursementMonthlyClaim", objectId = EntitySchoolReimbursementMonthlyClaimID, ignoreWarnings = ignoreWarnings, entityId = entityId, flatten = flatten, returnResponse = returnResponse)
	}

	#' Create an EntitySchoolReimbursementMonthlyClaim
	#'
	#' This function creates an EntitySchoolReimbursementMonthlyClaim
	#' @param fieldNames The field values to give the created EntitySchoolReimbursementMonthlyClaim. Each defaults to NULL.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{listSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Food Service
	#' @return A newly created EntitySchoolReimbursementMonthlyClaim
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	createEntitySchoolReimbursementMonthlyClaim <- function(EntityID = NULL, NumericDay = NULL, ServedCountCEP = NULL, ApprovedCountFree = NULL, ApprovedCountReduced = NULL, PaidEligible = NULL, ReimbursementMonthID = NULL, MealType = NULL, ServedCountFree = NULL, ServedCountReduced = NULL, ServedCountPaid = NULL, ServedCountTotal = NULL, ServedCountOther = NULL, entityId = 1, schoolYearId = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment())

		body <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper()) %>% purrr::compact()

		createSkyObject(module = "FoodService", objectName = "EntitySchoolReimbursementMonthlyClaim", body = list(DataObject = body), searchFields = append("EntitySchoolReimbursementMonthlyClaimID", body %>% names()), entityId = entityId, schoolYearId = schoolYearId, flatten = flatten, returnResponse = returnResponse)
	}

	#' Modify an EntitySchoolReimbursementMonthlyClaim
	#'
	#' This function modifies an EntitySchoolReimbursementMonthlyClaim
	#' @param fieldNames The field values to give the modified EntitySchoolReimbursementMonthlyClaim. Each defaults to NULL.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{listSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Food Service
	#' @return The modified EntitySchoolReimbursementMonthlyClaim
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	modifyEntitySchoolReimbursementMonthlyClaim <- function(EntitySchoolReimbursementMonthlyClaimID, EntityID = NULL, NumericDay = NULL, ServedCountCEP = NULL, ApprovedCountFree = NULL, ApprovedCountReduced = NULL, PaidEligible = NULL, ReimbursementMonthID = NULL, MealType = NULL, ServedCountFree = NULL, ServedCountReduced = NULL, ServedCountPaid = NULL, ServedCountTotal = NULL, ServedCountOther = NULL, entityId = 1, schoolYearId = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment())

		body <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper()) %>% purrr::compact()

		modifySkyObject(module = "FoodService", objectName = "EntitySchoolReimbursementMonthlyClaim", objectId = EntitySchoolReimbursementMonthlyClaimID, body = list(DataObject = body), searchFields = append("EntitySchoolReimbursementMonthlyClaimID", body %>% names()), entityId = entityId, schoolYearId = schoolYearId, flatten = flatten, returnResponse = returnResponse)
	}

	#' List CurrentOrFutureCustomerCategories
	#'
	#' This function returns a dataframe or json object of CurrentOrFutureCustomerCategories
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given CurrentOrFutureCustomerCategories. Defaults to FALSE for all return fields which, for convenience, returns all fields for the CurrentOrFutureCustomerCategories.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object listed as text. Run \code{\link{getSchemaForObjects}}('CurrentOrFutureCustomerCategory') to get more field paths.
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
	#' @concept Food Service
	#' @return A list of CurrentOrFutureCustomerCategories
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	listCurrentOrFutureCustomerCategories <- function(searchConditionsList = NULL, CustomerCategoryID = F, CustomerID = F, fieldPaths = NULL, searchConditionsGroupType = "And", searchSortFieldNamesList = NULL, searchSortFieldNamesDescendingList = NULL, entityId = 1, schoolYearId = NULL, page = 1, pageSize = 100000, flatten = T, returnResponse = F){

		params <- as.list(environment())

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		listSkyObjects(module = "FoodService", objectName = "CurrentOrFutureCustomerCategory", searchFields = searchFields %>% append(fieldPaths), page = page, pageSize = pageSize, SearchConditionsList = searchConditionsList, SearchConditionsGroupType = searchConditionsGroupType, SearchSortFieldNamesList = searchSortFieldNamesList, SearchSortFieldNamesDescendingList = searchSortFieldNamesDescendingList, entityId = entityId, schoolYearId = schoolYearId, flatten = flatten, returnResponse = returnResponse)
	}

	#' Get a CurrentOrFutureCustomerCategory
	#'
	#' This function returns a dataframe or json object of a CurrentOrFutureCustomerCategory
	#' @param CurrentOrFutureCustomerCategoryID The ID of the CurrentOrFutureCustomerCategory to return.
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given CurrentOrFutureCustomerCategory. Defaults to FALSE for all return fields which, for convenience, returns all fields for the CurrentOrFutureCustomerCategory.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object listed as text. Run \code{\link{getSchemaForObjects}}('CurrentOrFutureCustomerCategory') to get more field paths.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{listSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Food Service
	#' @return A dataframe or of CurrentOrFutureCustomerCategory
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	getCurrentOrFutureCustomerCategory <- function(CurrentOrFutureCustomerCategoryID, CustomerCategoryID = F, CustomerID = F, fieldPaths = NULL, entityId = 1, schoolYearId = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment()) %>% purrr::keep(names(.) != "CurrentOrFutureCustomerCategoryID")

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		getSkyObject(module = "FoodService", objectName = "CurrentOrFutureCustomerCategory", objectId = CurrentOrFutureCustomerCategoryID, searchFields = searchFields, entityId = entityId, flatten = flatten, returnResponse = returnResponse)
	}

	#' Delete a CurrentOrFutureCustomerCategory
	#'
	#' This function deletes a CurrentOrFutureCustomerCategory
	#' @param CurrentOrFutureCustomerCategoryID The ID of the CurrentOrFutureCustomerCategory to delete
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{listSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Food Service
	#' @return The CurrentOrFutureCustomerCategoryID of the deleted CurrentOrFutureCustomerCategory.
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	deleteCurrentOrFutureCustomerCategory <- function(CurrentOrFutureCustomerCategoryID, ignoreWarnings = F, entityId = 1, schoolYearId = NULL, flatten = T, returnResponse = F){

		deleteSkyObject(module = "FoodService", objectName = "CurrentOrFutureCustomerCategory", objectId = CurrentOrFutureCustomerCategoryID, ignoreWarnings = ignoreWarnings, entityId = entityId, flatten = flatten, returnResponse = returnResponse)
	}

	#' List ApplicationFormats
	#'
	#' This function returns a dataframe or json object of ApplicationFormats
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given ApplicationFormats. Defaults to FALSE for all return fields which, for convenience, returns all fields for the ApplicationFormats.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object listed as text. Run \code{\link{getSchemaForObjects}}('ApplicationFormat') to get more field paths.
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
	#' @concept Food Service
	#' @return A list of ApplicationFormats
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	listApplicationFormats <- function(searchConditionsList = NULL, ApplicationFormatID = F, SkywardID = F, SkywardHash = F, DisplayTitle = F, IsActive = F, IsDefault = F, ObjectID = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, fieldPaths = NULL, searchConditionsGroupType = "And", searchSortFieldNamesList = NULL, searchSortFieldNamesDescendingList = NULL, entityId = 1, schoolYearId = NULL, page = 1, pageSize = 100000, flatten = T, returnResponse = F){

		params <- as.list(environment())

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		listSkyObjects(module = "FoodService", objectName = "ApplicationFormat", searchFields = searchFields %>% append(fieldPaths), page = page, pageSize = pageSize, SearchConditionsList = searchConditionsList, SearchConditionsGroupType = searchConditionsGroupType, SearchSortFieldNamesList = searchSortFieldNamesList, SearchSortFieldNamesDescendingList = searchSortFieldNamesDescendingList, entityId = entityId, schoolYearId = schoolYearId, flatten = flatten, returnResponse = returnResponse)
	}

	#' Get an ApplicationFormat
	#'
	#' This function returns a dataframe or json object of an ApplicationFormat
	#' @param ApplicationFormatID The ID of the ApplicationFormat to return.
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given ApplicationFormat. Defaults to FALSE for all return fields which, for convenience, returns all fields for the ApplicationFormat.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object listed as text. Run \code{\link{getSchemaForObjects}}('ApplicationFormat') to get more field paths.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{listSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Food Service
	#' @return A dataframe or of ApplicationFormat
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	getApplicationFormat <- function(ApplicationFormatID, SkywardID = F, SkywardHash = F, DisplayTitle = F, IsActive = F, IsDefault = F, ObjectID = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, fieldPaths = NULL, entityId = 1, schoolYearId = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment()) %>% purrr::keep(names(.) != "ApplicationFormatID")

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		getSkyObject(module = "FoodService", objectName = "ApplicationFormat", objectId = ApplicationFormatID, searchFields = searchFields, entityId = entityId, flatten = flatten, returnResponse = returnResponse)
	}

	#' Delete an ApplicationFormat
	#'
	#' This function deletes an ApplicationFormat
	#' @param ApplicationFormatID The ID of the ApplicationFormat to delete
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{listSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Food Service
	#' @return The ApplicationFormatID of the deleted ApplicationFormat.
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	deleteApplicationFormat <- function(ApplicationFormatID, ignoreWarnings = F, entityId = 1, schoolYearId = NULL, flatten = T, returnResponse = F){

		deleteSkyObject(module = "FoodService", objectName = "ApplicationFormat", objectId = ApplicationFormatID, ignoreWarnings = ignoreWarnings, entityId = entityId, flatten = flatten, returnResponse = returnResponse)
	}

	#' Create an ApplicationFormat
	#'
	#' This function creates an ApplicationFormat
	#' @param fieldNames The field values to give the created ApplicationFormat. Each defaults to NULL.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{listSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Food Service
	#' @return A newly created ApplicationFormat
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	createApplicationFormat <- function(DisplayTitle = NULL, IsActive = NULL, IsDefault = NULL, ObjectID = NULL, entityId = 1, schoolYearId = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment())

		body <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper()) %>% purrr::compact()

		createSkyObject(module = "FoodService", objectName = "ApplicationFormat", body = list(DataObject = body), searchFields = append("ApplicationFormatID", body %>% names()), entityId = entityId, schoolYearId = schoolYearId, flatten = flatten, returnResponse = returnResponse)
	}

	#' Modify an ApplicationFormat
	#'
	#' This function modifies an ApplicationFormat
	#' @param fieldNames The field values to give the modified ApplicationFormat. Each defaults to NULL.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{listSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Food Service
	#' @return The modified ApplicationFormat
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	modifyApplicationFormat <- function(ApplicationFormatID, DisplayTitle = NULL, IsActive = NULL, IsDefault = NULL, ObjectID = NULL, entityId = 1, schoolYearId = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment())

		body <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper()) %>% purrr::compact()

		modifySkyObject(module = "FoodService", objectName = "ApplicationFormat", objectId = ApplicationFormatID, body = list(DataObject = body), searchFields = append("ApplicationFormatID", body %>% names()), entityId = entityId, schoolYearId = schoolYearId, flatten = flatten, returnResponse = returnResponse)
	}

	#' List ApplicationSections
	#'
	#' This function returns a dataframe or json object of ApplicationSections
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given ApplicationSections. Defaults to FALSE for all return fields which, for convenience, returns all fields for the ApplicationSections.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object listed as text. Run \code{\link{getSchemaForObjects}}('ApplicationSection') to get more field paths.
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
	#' @concept Food Service
	#' @return A list of ApplicationSections
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	listApplicationSections <- function(searchConditionsList = NULL, ApplicationSectionID = F, ApplicationStepID = F, DisplayOrder = F, Instruction = F, Description = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, fieldPaths = NULL, searchConditionsGroupType = "And", searchSortFieldNamesList = NULL, searchSortFieldNamesDescendingList = NULL, entityId = 1, schoolYearId = NULL, page = 1, pageSize = 100000, flatten = T, returnResponse = F){

		params <- as.list(environment())

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		listSkyObjects(module = "FoodService", objectName = "ApplicationSection", searchFields = searchFields %>% append(fieldPaths), page = page, pageSize = pageSize, SearchConditionsList = searchConditionsList, SearchConditionsGroupType = searchConditionsGroupType, SearchSortFieldNamesList = searchSortFieldNamesList, SearchSortFieldNamesDescendingList = searchSortFieldNamesDescendingList, entityId = entityId, schoolYearId = schoolYearId, flatten = flatten, returnResponse = returnResponse)
	}

	#' Get an ApplicationSection
	#'
	#' This function returns a dataframe or json object of an ApplicationSection
	#' @param ApplicationSectionID The ID of the ApplicationSection to return.
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given ApplicationSection. Defaults to FALSE for all return fields which, for convenience, returns all fields for the ApplicationSection.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object listed as text. Run \code{\link{getSchemaForObjects}}('ApplicationSection') to get more field paths.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{listSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Food Service
	#' @return A dataframe or of ApplicationSection
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	getApplicationSection <- function(ApplicationSectionID, ApplicationStepID = F, DisplayOrder = F, Instruction = F, Description = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, fieldPaths = NULL, entityId = 1, schoolYearId = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment()) %>% purrr::keep(names(.) != "ApplicationSectionID")

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		getSkyObject(module = "FoodService", objectName = "ApplicationSection", objectId = ApplicationSectionID, searchFields = searchFields, entityId = entityId, flatten = flatten, returnResponse = returnResponse)
	}

	#' Delete an ApplicationSection
	#'
	#' This function deletes an ApplicationSection
	#' @param ApplicationSectionID The ID of the ApplicationSection to delete
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{listSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Food Service
	#' @return The ApplicationSectionID of the deleted ApplicationSection.
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	deleteApplicationSection <- function(ApplicationSectionID, ignoreWarnings = F, entityId = 1, schoolYearId = NULL, flatten = T, returnResponse = F){

		deleteSkyObject(module = "FoodService", objectName = "ApplicationSection", objectId = ApplicationSectionID, ignoreWarnings = ignoreWarnings, entityId = entityId, flatten = flatten, returnResponse = returnResponse)
	}

	#' Create an ApplicationSection
	#'
	#' This function creates an ApplicationSection
	#' @param fieldNames The field values to give the created ApplicationSection. Each defaults to NULL.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{listSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Food Service
	#' @return A newly created ApplicationSection
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	createApplicationSection <- function(ApplicationStepID = NULL, DisplayOrder = NULL, Instruction = NULL, Description = NULL, entityId = 1, schoolYearId = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment())

		body <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper()) %>% purrr::compact()

		createSkyObject(module = "FoodService", objectName = "ApplicationSection", body = list(DataObject = body), searchFields = append("ApplicationSectionID", body %>% names()), entityId = entityId, schoolYearId = schoolYearId, flatten = flatten, returnResponse = returnResponse)
	}

	#' Modify an ApplicationSection
	#'
	#' This function modifies an ApplicationSection
	#' @param fieldNames The field values to give the modified ApplicationSection. Each defaults to NULL.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{listSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Food Service
	#' @return The modified ApplicationSection
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	modifyApplicationSection <- function(ApplicationSectionID, ApplicationStepID = NULL, DisplayOrder = NULL, Instruction = NULL, Description = NULL, entityId = 1, schoolYearId = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment())

		body <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper()) %>% purrr::compact()

		modifySkyObject(module = "FoodService", objectName = "ApplicationSection", objectId = ApplicationSectionID, body = list(DataObject = body), searchFields = append("ApplicationSectionID", body %>% names()), entityId = entityId, schoolYearId = schoolYearId, flatten = flatten, returnResponse = returnResponse)
	}

	#' List ApplicationSectionDependentElementGroups
	#'
	#' This function returns a dataframe or json object of ApplicationSectionDependentElementGroups
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given ApplicationSectionDependentElementGroups. Defaults to FALSE for all return fields which, for convenience, returns all fields for the ApplicationSectionDependentElementGroups.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object listed as text. Run \code{\link{getSchemaForObjects}}('ApplicationSectionDependentElementGroup') to get more field paths.
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
	#' @concept Food Service
	#' @return A list of ApplicationSectionDependentElementGroups
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	listApplicationSectionDependentElementGroups <- function(searchConditionsList = NULL, ApplicationSectionDependentElementGroupID = F, ApplicationSectionID = F, ApplicationSectionElementGroupID = F, ApplicationSectionElementIDShowWhenTrue = F, ApplicationSectionElementIDShowWhenFalse = F, ApplicationSectionElementIDGovernor = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, fieldPaths = NULL, searchConditionsGroupType = "And", searchSortFieldNamesList = NULL, searchSortFieldNamesDescendingList = NULL, entityId = 1, schoolYearId = NULL, page = 1, pageSize = 100000, flatten = T, returnResponse = F){

		params <- as.list(environment())

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		listSkyObjects(module = "FoodService", objectName = "ApplicationSectionDependentElementGroup", searchFields = searchFields %>% append(fieldPaths), page = page, pageSize = pageSize, SearchConditionsList = searchConditionsList, SearchConditionsGroupType = searchConditionsGroupType, SearchSortFieldNamesList = searchSortFieldNamesList, SearchSortFieldNamesDescendingList = searchSortFieldNamesDescendingList, entityId = entityId, schoolYearId = schoolYearId, flatten = flatten, returnResponse = returnResponse)
	}

	#' Get an ApplicationSectionDependentElementGroup
	#'
	#' This function returns a dataframe or json object of an ApplicationSectionDependentElementGroup
	#' @param ApplicationSectionDependentElementGroupID The ID of the ApplicationSectionDependentElementGroup to return.
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given ApplicationSectionDependentElementGroup. Defaults to FALSE for all return fields which, for convenience, returns all fields for the ApplicationSectionDependentElementGroup.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object listed as text. Run \code{\link{getSchemaForObjects}}('ApplicationSectionDependentElementGroup') to get more field paths.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{listSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Food Service
	#' @return A dataframe or of ApplicationSectionDependentElementGroup
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	getApplicationSectionDependentElementGroup <- function(ApplicationSectionDependentElementGroupID, ApplicationSectionID = F, ApplicationSectionElementGroupID = F, ApplicationSectionElementIDShowWhenTrue = F, ApplicationSectionElementIDShowWhenFalse = F, ApplicationSectionElementIDGovernor = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, fieldPaths = NULL, entityId = 1, schoolYearId = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment()) %>% purrr::keep(names(.) != "ApplicationSectionDependentElementGroupID")

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		getSkyObject(module = "FoodService", objectName = "ApplicationSectionDependentElementGroup", objectId = ApplicationSectionDependentElementGroupID, searchFields = searchFields, entityId = entityId, flatten = flatten, returnResponse = returnResponse)
	}

	#' Delete an ApplicationSectionDependentElementGroup
	#'
	#' This function deletes an ApplicationSectionDependentElementGroup
	#' @param ApplicationSectionDependentElementGroupID The ID of the ApplicationSectionDependentElementGroup to delete
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{listSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Food Service
	#' @return The ApplicationSectionDependentElementGroupID of the deleted ApplicationSectionDependentElementGroup.
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	deleteApplicationSectionDependentElementGroup <- function(ApplicationSectionDependentElementGroupID, ignoreWarnings = F, entityId = 1, schoolYearId = NULL, flatten = T, returnResponse = F){

		deleteSkyObject(module = "FoodService", objectName = "ApplicationSectionDependentElementGroup", objectId = ApplicationSectionDependentElementGroupID, ignoreWarnings = ignoreWarnings, entityId = entityId, flatten = flatten, returnResponse = returnResponse)
	}

	#' Create an ApplicationSectionDependentElementGroup
	#'
	#' This function creates an ApplicationSectionDependentElementGroup
	#' @param fieldNames The field values to give the created ApplicationSectionDependentElementGroup. Each defaults to NULL.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{listSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Food Service
	#' @return A newly created ApplicationSectionDependentElementGroup
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	createApplicationSectionDependentElementGroup <- function(ApplicationSectionID = NULL, ApplicationSectionElementGroupID = NULL, ApplicationSectionElementIDShowWhenTrue = NULL, ApplicationSectionElementIDShowWhenFalse = NULL, ApplicationSectionElementIDGovernor = NULL, entityId = 1, schoolYearId = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment())

		body <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper()) %>% purrr::compact()

		createSkyObject(module = "FoodService", objectName = "ApplicationSectionDependentElementGroup", body = list(DataObject = body), searchFields = append("ApplicationSectionDependentElementGroupID", body %>% names()), entityId = entityId, schoolYearId = schoolYearId, flatten = flatten, returnResponse = returnResponse)
	}

	#' Modify an ApplicationSectionDependentElementGroup
	#'
	#' This function modifies an ApplicationSectionDependentElementGroup
	#' @param fieldNames The field values to give the modified ApplicationSectionDependentElementGroup. Each defaults to NULL.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{listSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Food Service
	#' @return The modified ApplicationSectionDependentElementGroup
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	modifyApplicationSectionDependentElementGroup <- function(ApplicationSectionDependentElementGroupID, ApplicationSectionID = NULL, ApplicationSectionElementGroupID = NULL, ApplicationSectionElementIDShowWhenTrue = NULL, ApplicationSectionElementIDShowWhenFalse = NULL, ApplicationSectionElementIDGovernor = NULL, entityId = 1, schoolYearId = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment())

		body <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper()) %>% purrr::compact()

		modifySkyObject(module = "FoodService", objectName = "ApplicationSectionDependentElementGroup", objectId = ApplicationSectionDependentElementGroupID, body = list(DataObject = body), searchFields = append("ApplicationSectionDependentElementGroupID", body %>% names()), entityId = entityId, schoolYearId = schoolYearId, flatten = flatten, returnResponse = returnResponse)
	}

	#' List ApplicationSectionDistricts
	#'
	#' This function returns a dataframe or json object of ApplicationSectionDistricts
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given ApplicationSectionDistricts. Defaults to FALSE for all return fields which, for convenience, returns all fields for the ApplicationSectionDistricts.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object listed as text. Run \code{\link{getSchemaForObjects}}('ApplicationSectionDistrict') to get more field paths.
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
	#' @concept Food Service
	#' @return A list of ApplicationSectionDistricts
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	listApplicationSectionDistricts <- function(searchConditionsList = NULL, ApplicationSectionDistrictID = F, ApplicationSectionID = F, DistrictID = F, UseInstructionOverride = F, InstructionOverride = F, UseDescriptionOverride = F, DescriptionOverride = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, fieldPaths = NULL, searchConditionsGroupType = "And", searchSortFieldNamesList = NULL, searchSortFieldNamesDescendingList = NULL, entityId = 1, schoolYearId = NULL, page = 1, pageSize = 100000, flatten = T, returnResponse = F){

		params <- as.list(environment())

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		listSkyObjects(module = "FoodService", objectName = "ApplicationSectionDistrict", searchFields = searchFields %>% append(fieldPaths), page = page, pageSize = pageSize, SearchConditionsList = searchConditionsList, SearchConditionsGroupType = searchConditionsGroupType, SearchSortFieldNamesList = searchSortFieldNamesList, SearchSortFieldNamesDescendingList = searchSortFieldNamesDescendingList, entityId = entityId, schoolYearId = schoolYearId, flatten = flatten, returnResponse = returnResponse)
	}

	#' Get an ApplicationSectionDistrict
	#'
	#' This function returns a dataframe or json object of an ApplicationSectionDistrict
	#' @param ApplicationSectionDistrictID The ID of the ApplicationSectionDistrict to return.
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given ApplicationSectionDistrict. Defaults to FALSE for all return fields which, for convenience, returns all fields for the ApplicationSectionDistrict.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object listed as text. Run \code{\link{getSchemaForObjects}}('ApplicationSectionDistrict') to get more field paths.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{listSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Food Service
	#' @return A dataframe or of ApplicationSectionDistrict
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	getApplicationSectionDistrict <- function(ApplicationSectionDistrictID, ApplicationSectionID = F, DistrictID = F, UseInstructionOverride = F, InstructionOverride = F, UseDescriptionOverride = F, DescriptionOverride = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, fieldPaths = NULL, entityId = 1, schoolYearId = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment()) %>% purrr::keep(names(.) != "ApplicationSectionDistrictID")

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		getSkyObject(module = "FoodService", objectName = "ApplicationSectionDistrict", objectId = ApplicationSectionDistrictID, searchFields = searchFields, entityId = entityId, flatten = flatten, returnResponse = returnResponse)
	}

	#' Delete an ApplicationSectionDistrict
	#'
	#' This function deletes an ApplicationSectionDistrict
	#' @param ApplicationSectionDistrictID The ID of the ApplicationSectionDistrict to delete
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{listSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Food Service
	#' @return The ApplicationSectionDistrictID of the deleted ApplicationSectionDistrict.
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	deleteApplicationSectionDistrict <- function(ApplicationSectionDistrictID, ignoreWarnings = F, entityId = 1, schoolYearId = NULL, flatten = T, returnResponse = F){

		deleteSkyObject(module = "FoodService", objectName = "ApplicationSectionDistrict", objectId = ApplicationSectionDistrictID, ignoreWarnings = ignoreWarnings, entityId = entityId, flatten = flatten, returnResponse = returnResponse)
	}

	#' Create an ApplicationSectionDistrict
	#'
	#' This function creates an ApplicationSectionDistrict
	#' @param fieldNames The field values to give the created ApplicationSectionDistrict. Each defaults to NULL.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{listSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Food Service
	#' @return A newly created ApplicationSectionDistrict
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	createApplicationSectionDistrict <- function(ApplicationSectionID = NULL, DistrictID = NULL, UseInstructionOverride = NULL, InstructionOverride = NULL, UseDescriptionOverride = NULL, DescriptionOverride = NULL, entityId = 1, schoolYearId = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment())

		body <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper()) %>% purrr::compact()

		createSkyObject(module = "FoodService", objectName = "ApplicationSectionDistrict", body = list(DataObject = body), searchFields = append("ApplicationSectionDistrictID", body %>% names()), entityId = entityId, schoolYearId = schoolYearId, flatten = flatten, returnResponse = returnResponse)
	}

	#' Modify an ApplicationSectionDistrict
	#'
	#' This function modifies an ApplicationSectionDistrict
	#' @param fieldNames The field values to give the modified ApplicationSectionDistrict. Each defaults to NULL.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{listSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Food Service
	#' @return The modified ApplicationSectionDistrict
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	modifyApplicationSectionDistrict <- function(ApplicationSectionDistrictID, ApplicationSectionID = NULL, DistrictID = NULL, UseInstructionOverride = NULL, InstructionOverride = NULL, UseDescriptionOverride = NULL, DescriptionOverride = NULL, entityId = 1, schoolYearId = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment())

		body <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper()) %>% purrr::compact()

		modifySkyObject(module = "FoodService", objectName = "ApplicationSectionDistrict", objectId = ApplicationSectionDistrictID, body = list(DataObject = body), searchFields = append("ApplicationSectionDistrictID", body %>% names()), entityId = entityId, schoolYearId = schoolYearId, flatten = flatten, returnResponse = returnResponse)
	}

	#' List ApplicationSectionElements
	#'
	#' This function returns a dataframe or json object of ApplicationSectionElements
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given ApplicationSectionElements. Defaults to FALSE for all return fields which, for convenience, returns all fields for the ApplicationSectionElements.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object listed as text. Run \code{\link{getSchemaForObjects}}('ApplicationSectionElement') to get more field paths.
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
	#' @concept Food Service
	#' @return A list of ApplicationSectionElements
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	listApplicationSectionElements <- function(searchConditionsList = NULL, ApplicationSectionElementID = F, ApplicationSectionID = F, ApplicationSectionElementGroupID = F, ApplicationSectionDependentElementGroupID = F, FieldPathGuid = F, DisplayNameOverride = F, DisplayFormatOverride = F, LineNumber = F, DisplayOrder = F, IsHidden = F, IsReadOnly = F, DefaultValue = F, ElementViewPart = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, fieldPaths = NULL, searchConditionsGroupType = "And", searchSortFieldNamesList = NULL, searchSortFieldNamesDescendingList = NULL, entityId = 1, schoolYearId = NULL, page = 1, pageSize = 100000, flatten = T, returnResponse = F){

		params <- as.list(environment())

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		listSkyObjects(module = "FoodService", objectName = "ApplicationSectionElement", searchFields = searchFields %>% append(fieldPaths), page = page, pageSize = pageSize, SearchConditionsList = searchConditionsList, SearchConditionsGroupType = searchConditionsGroupType, SearchSortFieldNamesList = searchSortFieldNamesList, SearchSortFieldNamesDescendingList = searchSortFieldNamesDescendingList, entityId = entityId, schoolYearId = schoolYearId, flatten = flatten, returnResponse = returnResponse)
	}

	#' Get an ApplicationSectionElement
	#'
	#' This function returns a dataframe or json object of an ApplicationSectionElement
	#' @param ApplicationSectionElementID The ID of the ApplicationSectionElement to return.
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given ApplicationSectionElement. Defaults to FALSE for all return fields which, for convenience, returns all fields for the ApplicationSectionElement.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object listed as text. Run \code{\link{getSchemaForObjects}}('ApplicationSectionElement') to get more field paths.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{listSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Food Service
	#' @return A dataframe or of ApplicationSectionElement
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	getApplicationSectionElement <- function(ApplicationSectionElementID, ApplicationSectionID = F, ApplicationSectionElementGroupID = F, ApplicationSectionDependentElementGroupID = F, FieldPathGuid = F, DisplayNameOverride = F, DisplayFormatOverride = F, LineNumber = F, DisplayOrder = F, IsHidden = F, IsReadOnly = F, DefaultValue = F, ElementViewPart = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, fieldPaths = NULL, entityId = 1, schoolYearId = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment()) %>% purrr::keep(names(.) != "ApplicationSectionElementID")

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		getSkyObject(module = "FoodService", objectName = "ApplicationSectionElement", objectId = ApplicationSectionElementID, searchFields = searchFields, entityId = entityId, flatten = flatten, returnResponse = returnResponse)
	}

	#' Delete an ApplicationSectionElement
	#'
	#' This function deletes an ApplicationSectionElement
	#' @param ApplicationSectionElementID The ID of the ApplicationSectionElement to delete
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{listSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Food Service
	#' @return The ApplicationSectionElementID of the deleted ApplicationSectionElement.
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	deleteApplicationSectionElement <- function(ApplicationSectionElementID, ignoreWarnings = F, entityId = 1, schoolYearId = NULL, flatten = T, returnResponse = F){

		deleteSkyObject(module = "FoodService", objectName = "ApplicationSectionElement", objectId = ApplicationSectionElementID, ignoreWarnings = ignoreWarnings, entityId = entityId, flatten = flatten, returnResponse = returnResponse)
	}

	#' Create an ApplicationSectionElement
	#'
	#' This function creates an ApplicationSectionElement
	#' @param fieldNames The field values to give the created ApplicationSectionElement. Each defaults to NULL.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{listSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Food Service
	#' @return A newly created ApplicationSectionElement
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	createApplicationSectionElement <- function(ApplicationSectionID = NULL, ApplicationSectionElementGroupID = NULL, ApplicationSectionDependentElementGroupID = NULL, DisplayNameOverride = NULL, LineNumber = NULL, DisplayOrder = NULL, IsHidden = NULL, IsReadOnly = NULL, DefaultValue = NULL, entityId = 1, schoolYearId = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment())

		body <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper()) %>% purrr::compact()

		createSkyObject(module = "FoodService", objectName = "ApplicationSectionElement", body = list(DataObject = body), searchFields = append("ApplicationSectionElementID", body %>% names()), entityId = entityId, schoolYearId = schoolYearId, flatten = flatten, returnResponse = returnResponse)
	}

	#' Modify an ApplicationSectionElement
	#'
	#' This function modifies an ApplicationSectionElement
	#' @param fieldNames The field values to give the modified ApplicationSectionElement. Each defaults to NULL.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{listSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Food Service
	#' @return The modified ApplicationSectionElement
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	modifyApplicationSectionElement <- function(ApplicationSectionElementID, ApplicationSectionID = NULL, ApplicationSectionElementGroupID = NULL, ApplicationSectionDependentElementGroupID = NULL, DisplayNameOverride = NULL, LineNumber = NULL, DisplayOrder = NULL, IsHidden = NULL, IsReadOnly = NULL, DefaultValue = NULL, entityId = 1, schoolYearId = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment())

		body <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper()) %>% purrr::compact()

		modifySkyObject(module = "FoodService", objectName = "ApplicationSectionElement", objectId = ApplicationSectionElementID, body = list(DataObject = body), searchFields = append("ApplicationSectionElementID", body %>% names()), entityId = entityId, schoolYearId = schoolYearId, flatten = flatten, returnResponse = returnResponse)
	}

	#' List ApplicationSectionElementGroups
	#'
	#' This function returns a dataframe or json object of ApplicationSectionElementGroups
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given ApplicationSectionElementGroups. Defaults to FALSE for all return fields which, for convenience, returns all fields for the ApplicationSectionElementGroups.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object listed as text. Run \code{\link{getSchemaForObjects}}('ApplicationSectionElementGroup') to get more field paths.
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
	#' @concept Food Service
	#' @return A list of ApplicationSectionElementGroups
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	listApplicationSectionElementGroups <- function(searchConditionsList = NULL, ApplicationSectionElementGroupID = F, ApplicationSectionID = F, LineNumber = F, DisplayOrder = F, RelationshipPathGuid = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, fieldPaths = NULL, searchConditionsGroupType = "And", searchSortFieldNamesList = NULL, searchSortFieldNamesDescendingList = NULL, entityId = 1, schoolYearId = NULL, page = 1, pageSize = 100000, flatten = T, returnResponse = F){

		params <- as.list(environment())

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		listSkyObjects(module = "FoodService", objectName = "ApplicationSectionElementGroup", searchFields = searchFields %>% append(fieldPaths), page = page, pageSize = pageSize, SearchConditionsList = searchConditionsList, SearchConditionsGroupType = searchConditionsGroupType, SearchSortFieldNamesList = searchSortFieldNamesList, SearchSortFieldNamesDescendingList = searchSortFieldNamesDescendingList, entityId = entityId, schoolYearId = schoolYearId, flatten = flatten, returnResponse = returnResponse)
	}

	#' Get an ApplicationSectionElementGroup
	#'
	#' This function returns a dataframe or json object of an ApplicationSectionElementGroup
	#' @param ApplicationSectionElementGroupID The ID of the ApplicationSectionElementGroup to return.
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given ApplicationSectionElementGroup. Defaults to FALSE for all return fields which, for convenience, returns all fields for the ApplicationSectionElementGroup.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object listed as text. Run \code{\link{getSchemaForObjects}}('ApplicationSectionElementGroup') to get more field paths.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{listSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Food Service
	#' @return A dataframe or of ApplicationSectionElementGroup
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	getApplicationSectionElementGroup <- function(ApplicationSectionElementGroupID, ApplicationSectionID = F, LineNumber = F, DisplayOrder = F, RelationshipPathGuid = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, fieldPaths = NULL, entityId = 1, schoolYearId = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment()) %>% purrr::keep(names(.) != "ApplicationSectionElementGroupID")

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		getSkyObject(module = "FoodService", objectName = "ApplicationSectionElementGroup", objectId = ApplicationSectionElementGroupID, searchFields = searchFields, entityId = entityId, flatten = flatten, returnResponse = returnResponse)
	}

	#' Delete an ApplicationSectionElementGroup
	#'
	#' This function deletes an ApplicationSectionElementGroup
	#' @param ApplicationSectionElementGroupID The ID of the ApplicationSectionElementGroup to delete
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{listSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Food Service
	#' @return The ApplicationSectionElementGroupID of the deleted ApplicationSectionElementGroup.
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	deleteApplicationSectionElementGroup <- function(ApplicationSectionElementGroupID, ignoreWarnings = F, entityId = 1, schoolYearId = NULL, flatten = T, returnResponse = F){

		deleteSkyObject(module = "FoodService", objectName = "ApplicationSectionElementGroup", objectId = ApplicationSectionElementGroupID, ignoreWarnings = ignoreWarnings, entityId = entityId, flatten = flatten, returnResponse = returnResponse)
	}

	#' Create an ApplicationSectionElementGroup
	#'
	#' This function creates an ApplicationSectionElementGroup
	#' @param fieldNames The field values to give the created ApplicationSectionElementGroup. Each defaults to NULL.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{listSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Food Service
	#' @return A newly created ApplicationSectionElementGroup
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	createApplicationSectionElementGroup <- function(ApplicationSectionID = NULL, LineNumber = NULL, DisplayOrder = NULL, entityId = 1, schoolYearId = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment())

		body <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper()) %>% purrr::compact()

		createSkyObject(module = "FoodService", objectName = "ApplicationSectionElementGroup", body = list(DataObject = body), searchFields = append("ApplicationSectionElementGroupID", body %>% names()), entityId = entityId, schoolYearId = schoolYearId, flatten = flatten, returnResponse = returnResponse)
	}

	#' Modify an ApplicationSectionElementGroup
	#'
	#' This function modifies an ApplicationSectionElementGroup
	#' @param fieldNames The field values to give the modified ApplicationSectionElementGroup. Each defaults to NULL.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{listSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Food Service
	#' @return The modified ApplicationSectionElementGroup
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	modifyApplicationSectionElementGroup <- function(ApplicationSectionElementGroupID, ApplicationSectionID = NULL, LineNumber = NULL, DisplayOrder = NULL, entityId = 1, schoolYearId = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment())

		body <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper()) %>% purrr::compact()

		modifySkyObject(module = "FoodService", objectName = "ApplicationSectionElementGroup", objectId = ApplicationSectionElementGroupID, body = list(DataObject = body), searchFields = append("ApplicationSectionElementGroupID", body %>% names()), entityId = entityId, schoolYearId = schoolYearId, flatten = flatten, returnResponse = returnResponse)
	}

	#' List ApplicationStepDistricts
	#'
	#' This function returns a dataframe or json object of ApplicationStepDistricts
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given ApplicationStepDistricts. Defaults to FALSE for all return fields which, for convenience, returns all fields for the ApplicationStepDistricts.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object listed as text. Run \code{\link{getSchemaForObjects}}('ApplicationStepDistrict') to get more field paths.
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
	#' @concept Food Service
	#' @return A list of ApplicationStepDistricts
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	listApplicationStepDistricts <- function(searchConditionsList = NULL, ApplicationStepDistrictID = F, ApplicationStepID = F, DistrictID = F, UseInstructionOverride = F, InstructionOverride = F, UseDescriptionOverride = F, DescriptionOverride = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, fieldPaths = NULL, searchConditionsGroupType = "And", searchSortFieldNamesList = NULL, searchSortFieldNamesDescendingList = NULL, entityId = 1, schoolYearId = NULL, page = 1, pageSize = 100000, flatten = T, returnResponse = F){

		params <- as.list(environment())

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		listSkyObjects(module = "FoodService", objectName = "ApplicationStepDistrict", searchFields = searchFields %>% append(fieldPaths), page = page, pageSize = pageSize, SearchConditionsList = searchConditionsList, SearchConditionsGroupType = searchConditionsGroupType, SearchSortFieldNamesList = searchSortFieldNamesList, SearchSortFieldNamesDescendingList = searchSortFieldNamesDescendingList, entityId = entityId, schoolYearId = schoolYearId, flatten = flatten, returnResponse = returnResponse)
	}

	#' Get an ApplicationStepDistrict
	#'
	#' This function returns a dataframe or json object of an ApplicationStepDistrict
	#' @param ApplicationStepDistrictID The ID of the ApplicationStepDistrict to return.
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given ApplicationStepDistrict. Defaults to FALSE for all return fields which, for convenience, returns all fields for the ApplicationStepDistrict.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object listed as text. Run \code{\link{getSchemaForObjects}}('ApplicationStepDistrict') to get more field paths.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{listSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Food Service
	#' @return A dataframe or of ApplicationStepDistrict
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	getApplicationStepDistrict <- function(ApplicationStepDistrictID, ApplicationStepID = F, DistrictID = F, UseInstructionOverride = F, InstructionOverride = F, UseDescriptionOverride = F, DescriptionOverride = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, fieldPaths = NULL, entityId = 1, schoolYearId = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment()) %>% purrr::keep(names(.) != "ApplicationStepDistrictID")

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		getSkyObject(module = "FoodService", objectName = "ApplicationStepDistrict", objectId = ApplicationStepDistrictID, searchFields = searchFields, entityId = entityId, flatten = flatten, returnResponse = returnResponse)
	}

	#' Delete an ApplicationStepDistrict
	#'
	#' This function deletes an ApplicationStepDistrict
	#' @param ApplicationStepDistrictID The ID of the ApplicationStepDistrict to delete
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{listSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Food Service
	#' @return The ApplicationStepDistrictID of the deleted ApplicationStepDistrict.
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	deleteApplicationStepDistrict <- function(ApplicationStepDistrictID, ignoreWarnings = F, entityId = 1, schoolYearId = NULL, flatten = T, returnResponse = F){

		deleteSkyObject(module = "FoodService", objectName = "ApplicationStepDistrict", objectId = ApplicationStepDistrictID, ignoreWarnings = ignoreWarnings, entityId = entityId, flatten = flatten, returnResponse = returnResponse)
	}

	#' Create an ApplicationStepDistrict
	#'
	#' This function creates an ApplicationStepDistrict
	#' @param fieldNames The field values to give the created ApplicationStepDistrict. Each defaults to NULL.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{listSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Food Service
	#' @return A newly created ApplicationStepDistrict
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	createApplicationStepDistrict <- function(ApplicationStepID = NULL, DistrictID = NULL, UseInstructionOverride = NULL, InstructionOverride = NULL, UseDescriptionOverride = NULL, DescriptionOverride = NULL, entityId = 1, schoolYearId = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment())

		body <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper()) %>% purrr::compact()

		createSkyObject(module = "FoodService", objectName = "ApplicationStepDistrict", body = list(DataObject = body), searchFields = append("ApplicationStepDistrictID", body %>% names()), entityId = entityId, schoolYearId = schoolYearId, flatten = flatten, returnResponse = returnResponse)
	}

	#' Modify an ApplicationStepDistrict
	#'
	#' This function modifies an ApplicationStepDistrict
	#' @param fieldNames The field values to give the modified ApplicationStepDistrict. Each defaults to NULL.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{listSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Food Service
	#' @return The modified ApplicationStepDistrict
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	modifyApplicationStepDistrict <- function(ApplicationStepDistrictID, ApplicationStepID = NULL, DistrictID = NULL, UseInstructionOverride = NULL, InstructionOverride = NULL, UseDescriptionOverride = NULL, DescriptionOverride = NULL, entityId = 1, schoolYearId = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment())

		body <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper()) %>% purrr::compact()

		modifySkyObject(module = "FoodService", objectName = "ApplicationStepDistrict", objectId = ApplicationStepDistrictID, body = list(DataObject = body), searchFields = append("ApplicationStepDistrictID", body %>% names()), entityId = entityId, schoolYearId = schoolYearId, flatten = flatten, returnResponse = returnResponse)
	}

	#' List ApplicationSteps
	#'
	#' This function returns a dataframe or json object of ApplicationSteps
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given ApplicationSteps. Defaults to FALSE for all return fields which, for convenience, returns all fields for the ApplicationSteps.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object listed as text. Run \code{\link{getSchemaForObjects}}('ApplicationStep') to get more field paths.
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
	#' @concept Food Service
	#' @return A list of ApplicationSteps
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	listApplicationSteps <- function(searchConditionsList = NULL, ApplicationStepID = F, ApplicationFormatID = F, Name = F, ShortDescription = F, DisplayOrder = F, Instruction = F, Description = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, fieldPaths = NULL, searchConditionsGroupType = "And", searchSortFieldNamesList = NULL, searchSortFieldNamesDescendingList = NULL, entityId = 1, schoolYearId = NULL, page = 1, pageSize = 100000, flatten = T, returnResponse = F){

		params <- as.list(environment())

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		listSkyObjects(module = "FoodService", objectName = "ApplicationStep", searchFields = searchFields %>% append(fieldPaths), page = page, pageSize = pageSize, SearchConditionsList = searchConditionsList, SearchConditionsGroupType = searchConditionsGroupType, SearchSortFieldNamesList = searchSortFieldNamesList, SearchSortFieldNamesDescendingList = searchSortFieldNamesDescendingList, entityId = entityId, schoolYearId = schoolYearId, flatten = flatten, returnResponse = returnResponse)
	}

	#' Get an ApplicationStep
	#'
	#' This function returns a dataframe or json object of an ApplicationStep
	#' @param ApplicationStepID The ID of the ApplicationStep to return.
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given ApplicationStep. Defaults to FALSE for all return fields which, for convenience, returns all fields for the ApplicationStep.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object listed as text. Run \code{\link{getSchemaForObjects}}('ApplicationStep') to get more field paths.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{listSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Food Service
	#' @return A dataframe or of ApplicationStep
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	getApplicationStep <- function(ApplicationStepID, ApplicationFormatID = F, Name = F, ShortDescription = F, DisplayOrder = F, Instruction = F, Description = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, fieldPaths = NULL, entityId = 1, schoolYearId = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment()) %>% purrr::keep(names(.) != "ApplicationStepID")

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		getSkyObject(module = "FoodService", objectName = "ApplicationStep", objectId = ApplicationStepID, searchFields = searchFields, entityId = entityId, flatten = flatten, returnResponse = returnResponse)
	}

	#' Delete an ApplicationStep
	#'
	#' This function deletes an ApplicationStep
	#' @param ApplicationStepID The ID of the ApplicationStep to delete
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{listSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Food Service
	#' @return The ApplicationStepID of the deleted ApplicationStep.
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	deleteApplicationStep <- function(ApplicationStepID, ignoreWarnings = F, entityId = 1, schoolYearId = NULL, flatten = T, returnResponse = F){

		deleteSkyObject(module = "FoodService", objectName = "ApplicationStep", objectId = ApplicationStepID, ignoreWarnings = ignoreWarnings, entityId = entityId, flatten = flatten, returnResponse = returnResponse)
	}

	#' Create an ApplicationStep
	#'
	#' This function creates an ApplicationStep
	#' @param fieldNames The field values to give the created ApplicationStep. Each defaults to NULL.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{listSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Food Service
	#' @return A newly created ApplicationStep
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	createApplicationStep <- function(ApplicationFormatID = NULL, Name = NULL, ShortDescription = NULL, DisplayOrder = NULL, Instruction = NULL, Description = NULL, entityId = 1, schoolYearId = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment())

		body <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper()) %>% purrr::compact()

		createSkyObject(module = "FoodService", objectName = "ApplicationStep", body = list(DataObject = body), searchFields = append("ApplicationStepID", body %>% names()), entityId = entityId, schoolYearId = schoolYearId, flatten = flatten, returnResponse = returnResponse)
	}

	#' Modify an ApplicationStep
	#'
	#' This function modifies an ApplicationStep
	#' @param fieldNames The field values to give the modified ApplicationStep. Each defaults to NULL.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{listSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Food Service
	#' @return The modified ApplicationStep
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	modifyApplicationStep <- function(ApplicationStepID, ApplicationFormatID = NULL, Name = NULL, ShortDescription = NULL, DisplayOrder = NULL, Instruction = NULL, Description = NULL, entityId = 1, schoolYearId = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment())

		body <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper()) %>% purrr::compact()

		modifySkyObject(module = "FoodService", objectName = "ApplicationStep", objectId = ApplicationStepID, body = list(DataObject = body), searchFields = append("ApplicationStepID", body %>% names()), entityId = entityId, schoolYearId = schoolYearId, flatten = flatten, returnResponse = returnResponse)
	}

	#' List OnlineApplications
	#'
	#' This function returns a dataframe or json object of OnlineApplications
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given OnlineApplications. Defaults to FALSE for all return fields which, for convenience, returns all fields for the OnlineApplications.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object listed as text. Run \code{\link{getSchemaForObjects}}('OnlineApplication') to get more field paths.
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
	#' @concept Food Service
	#' @return A list of OnlineApplications
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	listOnlineApplications <- function(searchConditionsList = NULL, OnlineApplicationID = F, StreetNumberAndName = F, City = F, StateCode = F, ZipCode = F, ZipCodeAddOn = F, PhoneNumber = F, PhoneExtension = F, WorkPhoneNumber = F, WorkPhoneExtension = F, EmailAddress = F, NameIDElectronicSignature = F, Status = F, ReturnReason = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, fieldPaths = NULL, searchConditionsGroupType = "And", searchSortFieldNamesList = NULL, searchSortFieldNamesDescendingList = NULL, entityId = 1, schoolYearId = NULL, page = 1, pageSize = 100000, flatten = T, returnResponse = F){

		params <- as.list(environment())

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		listSkyObjects(module = "FoodService", objectName = "OnlineApplication", searchFields = searchFields %>% append(fieldPaths), page = page, pageSize = pageSize, SearchConditionsList = searchConditionsList, SearchConditionsGroupType = searchConditionsGroupType, SearchSortFieldNamesList = searchSortFieldNamesList, SearchSortFieldNamesDescendingList = searchSortFieldNamesDescendingList, entityId = entityId, schoolYearId = schoolYearId, flatten = flatten, returnResponse = returnResponse)
	}

	#' Get an OnlineApplication
	#'
	#' This function returns a dataframe or json object of an OnlineApplication
	#' @param OnlineApplicationID The ID of the OnlineApplication to return.
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given OnlineApplication. Defaults to FALSE for all return fields which, for convenience, returns all fields for the OnlineApplication.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object listed as text. Run \code{\link{getSchemaForObjects}}('OnlineApplication') to get more field paths.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{listSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Food Service
	#' @return A dataframe or of OnlineApplication
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	getOnlineApplication <- function(OnlineApplicationID, StreetNumberAndName = F, City = F, StateCode = F, ZipCode = F, ZipCodeAddOn = F, PhoneNumber = F, PhoneExtension = F, WorkPhoneNumber = F, WorkPhoneExtension = F, EmailAddress = F, NameIDElectronicSignature = F, Status = F, ReturnReason = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, fieldPaths = NULL, entityId = 1, schoolYearId = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment()) %>% purrr::keep(names(.) != "OnlineApplicationID")

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		getSkyObject(module = "FoodService", objectName = "OnlineApplication", objectId = OnlineApplicationID, searchFields = searchFields, entityId = entityId, flatten = flatten, returnResponse = returnResponse)
	}

	#' Delete an OnlineApplication
	#'
	#' This function deletes an OnlineApplication
	#' @param OnlineApplicationID The ID of the OnlineApplication to delete
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{listSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Food Service
	#' @return The OnlineApplicationID of the deleted OnlineApplication.
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	deleteOnlineApplication <- function(OnlineApplicationID, ignoreWarnings = F, entityId = 1, schoolYearId = NULL, flatten = T, returnResponse = F){

		deleteSkyObject(module = "FoodService", objectName = "OnlineApplication", objectId = OnlineApplicationID, ignoreWarnings = ignoreWarnings, entityId = entityId, flatten = flatten, returnResponse = returnResponse)
	}

	#' Create an OnlineApplication
	#'
	#' This function creates an OnlineApplication
	#' @param fieldNames The field values to give the created OnlineApplication. Each defaults to NULL.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{listSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Food Service
	#' @return A newly created OnlineApplication
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	createOnlineApplication <- function(StreetNumberAndName = NULL, City = NULL, StateCode = NULL, ZipCode = NULL, ZipCodeAddOn = NULL, PhoneNumber = NULL, PhoneExtension = NULL, WorkPhoneNumber = NULL, WorkPhoneExtension = NULL, EmailAddress = NULL, NameIDElectronicSignature = NULL, Status = NULL, ReturnReason = NULL, entityId = 1, schoolYearId = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment())

		body <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper()) %>% purrr::compact()

		createSkyObject(module = "FoodService", objectName = "OnlineApplication", body = list(DataObject = body), searchFields = append("OnlineApplicationID", body %>% names()), entityId = entityId, schoolYearId = schoolYearId, flatten = flatten, returnResponse = returnResponse)
	}

	#' Modify an OnlineApplication
	#'
	#' This function modifies an OnlineApplication
	#' @param fieldNames The field values to give the modified OnlineApplication. Each defaults to NULL.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{listSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Food Service
	#' @return The modified OnlineApplication
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	modifyOnlineApplication <- function(OnlineApplicationID, StreetNumberAndName = NULL, City = NULL, StateCode = NULL, ZipCode = NULL, ZipCodeAddOn = NULL, PhoneNumber = NULL, PhoneExtension = NULL, WorkPhoneNumber = NULL, WorkPhoneExtension = NULL, EmailAddress = NULL, NameIDElectronicSignature = NULL, Status = NULL, ReturnReason = NULL, entityId = 1, schoolYearId = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment())

		body <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper()) %>% purrr::compact()

		modifySkyObject(module = "FoodService", objectName = "OnlineApplication", objectId = OnlineApplicationID, body = list(DataObject = body), searchFields = append("OnlineApplicationID", body %>% names()), entityId = entityId, schoolYearId = schoolYearId, flatten = flatten, returnResponse = returnResponse)
	}
