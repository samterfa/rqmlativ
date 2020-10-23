
	#' List TempRollUnpaidCustomerFees
	#'
	#' This function returns a dataframe or json object of TempRollUnpaidCustomerFees
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given TempRollUnpaidCustomerFees. Defaults to FALSE for all return fields which, for convenience, returns all fields for the TempRollUnpaidCustomerFees.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object listed as text. Run \code{\link{getSchemaForObjects}}('TempRollUnpaidCustomerFee') to get more field paths.
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
	#' @concept Year End
	#' @return A list of TempRollUnpaidCustomerFees
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	listTempRollUnpaidCustomerFees <- function(searchConditionsList = NULL, TempRollUnpaidCustomerFeeID = F, UnappliedAmount = F, AmountDue = F, IsExceptionRecord = F, ExceptionReason = F, EffectiveDate = F, DueDate = F, Description = F, SourceCustomerFeeID = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, CustomerID = F, CustomerName = F, CreateCustomerEntityYear = F, RollType = F, Comment = F, SourceFeeID = F, FeeIDTransferFrom = F, SourceFeeCode = F, SourceFeeAmount = F, TargetEntityID = F, TargetEntityCode = F, fieldPaths = NULL, searchConditionsGroupType = "And", searchSortFieldNamesList = NULL, searchSortFieldNamesDescendingList = NULL, entityId = 1, schoolYearId = NULL, page = 1, pageSize = 100000, flatten = T, returnResponse = F){

		params <- as.list(environment())

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		listSkyObjects(module = "YearEnd", objectName = "TempRollUnpaidCustomerFee", searchFields = searchFields %>% append(fieldPaths), page = page, pageSize = pageSize, SearchConditionsList = searchConditionsList, SearchConditionsGroupType = searchConditionsGroupType, SearchSortFieldNamesList = searchSortFieldNamesList, SearchSortFieldNamesDescendingList = searchSortFieldNamesDescendingList, entityId = entityId, schoolYearId = schoolYearId, flatten = flatten, returnResponse = returnResponse)
	}

	#' Get a TempRollUnpaidCustomerFee
	#'
	#' This function returns a dataframe or json object of a TempRollUnpaidCustomerFee
	#' @param TempRollUnpaidCustomerFeeID The ID of the TempRollUnpaidCustomerFee to return.
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given TempRollUnpaidCustomerFee. Defaults to FALSE for all return fields which, for convenience, returns all fields for the TempRollUnpaidCustomerFee.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object listed as text. Run \code{\link{getSchemaForObjects}}('TempRollUnpaidCustomerFee') to get more field paths.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{listSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Year End
	#' @return A dataframe or of TempRollUnpaidCustomerFee
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	getTempRollUnpaidCustomerFee <- function(TempRollUnpaidCustomerFeeID, UnappliedAmount = F, AmountDue = F, IsExceptionRecord = F, ExceptionReason = F, EffectiveDate = F, DueDate = F, Description = F, SourceCustomerFeeID = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, CustomerID = F, CustomerName = F, CreateCustomerEntityYear = F, RollType = F, Comment = F, SourceFeeID = F, FeeIDTransferFrom = F, SourceFeeCode = F, SourceFeeAmount = F, TargetEntityID = F, TargetEntityCode = F, fieldPaths = NULL, entityId = 1, schoolYearId = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment()) %>% purrr::keep(names(.) != "TempRollUnpaidCustomerFeeID")

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		getSkyObject(module = "YearEnd", objectName = "TempRollUnpaidCustomerFee", objectId = TempRollUnpaidCustomerFeeID, searchFields = searchFields, entityId = entityId, flatten = flatten, returnResponse = returnResponse)
	}

	#' Delete a TempRollUnpaidCustomerFee
	#'
	#' This function deletes a TempRollUnpaidCustomerFee
	#' @param TempRollUnpaidCustomerFeeID The ID of the TempRollUnpaidCustomerFee to delete
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{listSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Year End
	#' @return The TempRollUnpaidCustomerFeeID of the deleted TempRollUnpaidCustomerFee.
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	deleteTempRollUnpaidCustomerFee <- function(TempRollUnpaidCustomerFeeID, ignoreWarnings = F, entityId = 1, schoolYearId = NULL, flatten = T, returnResponse = F){

		deleteSkyObject(module = "YearEnd", objectName = "TempRollUnpaidCustomerFee", objectId = TempRollUnpaidCustomerFeeID, ignoreWarnings = ignoreWarnings, entityId = entityId, flatten = flatten, returnResponse = returnResponse)
	}

	#' Create a TempRollUnpaidCustomerFee
	#'
	#' This function creates a TempRollUnpaidCustomerFee
	#' @param fieldNames The field values to give the created TempRollUnpaidCustomerFee. Each defaults to NULL.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{listSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Year End
	#' @return A newly created TempRollUnpaidCustomerFee
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	createTempRollUnpaidCustomerFee <- function(UnappliedAmount = NULL, AmountDue = NULL, ExceptionReason = NULL, EffectiveDate = NULL, DueDate = NULL, Description = NULL, SourceCustomerFeeID = NULL, CustomerID = NULL, CustomerName = NULL, CreateCustomerEntityYear = NULL, RollType = NULL, Comment = NULL, SourceFeeID = NULL, FeeIDTransferFrom = NULL, SourceFeeCode = NULL, SourceFeeAmount = NULL, TargetEntityID = NULL, TargetEntityCode = NULL, entityId = 1, schoolYearId = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment())

		body <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper()) %>% purrr::compact()

		createSkyObject(module = "YearEnd", objectName = "TempRollUnpaidCustomerFee", body = list(DataObject = body), searchFields = append("TempRollUnpaidCustomerFeeID", body %>% names()), entityId = entityId, schoolYearId = schoolYearId, flatten = flatten, returnResponse = returnResponse)
	}

	#' Modify a TempRollUnpaidCustomerFee
	#'
	#' This function modifies a TempRollUnpaidCustomerFee
	#' @param fieldNames The field values to give the modified TempRollUnpaidCustomerFee. Each defaults to NULL.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{listSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Year End
	#' @return The modified TempRollUnpaidCustomerFee
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	modifyTempRollUnpaidCustomerFee <- function(TempRollUnpaidCustomerFeeID, UnappliedAmount = NULL, AmountDue = NULL, ExceptionReason = NULL, EffectiveDate = NULL, DueDate = NULL, Description = NULL, SourceCustomerFeeID = NULL, CustomerID = NULL, CustomerName = NULL, CreateCustomerEntityYear = NULL, RollType = NULL, Comment = NULL, SourceFeeID = NULL, FeeIDTransferFrom = NULL, SourceFeeCode = NULL, SourceFeeAmount = NULL, TargetEntityID = NULL, TargetEntityCode = NULL, entityId = 1, schoolYearId = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment())

		body <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper()) %>% purrr::compact()

		modifySkyObject(module = "YearEnd", objectName = "TempRollUnpaidCustomerFee", objectId = TempRollUnpaidCustomerFeeID, body = list(DataObject = body), searchFields = append("TempRollUnpaidCustomerFeeID", body %>% names()), entityId = entityId, schoolYearId = schoolYearId, flatten = flatten, returnResponse = returnResponse)
	}

	#' List TempRollUnappliedPayments
	#'
	#' This function returns a dataframe or json object of TempRollUnappliedPayments
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given TempRollUnappliedPayments. Defaults to FALSE for all return fields which, for convenience, returns all fields for the TempRollUnappliedPayments.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object listed as text. Run \code{\link{getSchemaForObjects}}('TempRollUnappliedPayment') to get more field paths.
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
	#' @concept Year End
	#' @return A list of TempRollUnappliedPayments
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	listTempRollUnappliedPayments <- function(searchConditionsList = NULL, TempRollUnappliedPaymentID = F, CustomerID = F, CustomerName = F, UnappliedAmount = F, IsExceptionRecord = F, ExceptionReason = F, RollType = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, PayorID = F, CreateCustomerEntityYear = F, ErrorCount = F, fieldPaths = NULL, searchConditionsGroupType = "And", searchSortFieldNamesList = NULL, searchSortFieldNamesDescendingList = NULL, entityId = 1, schoolYearId = NULL, page = 1, pageSize = 100000, flatten = T, returnResponse = F){

		params <- as.list(environment())

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		listSkyObjects(module = "YearEnd", objectName = "TempRollUnappliedPayment", searchFields = searchFields %>% append(fieldPaths), page = page, pageSize = pageSize, SearchConditionsList = searchConditionsList, SearchConditionsGroupType = searchConditionsGroupType, SearchSortFieldNamesList = searchSortFieldNamesList, SearchSortFieldNamesDescendingList = searchSortFieldNamesDescendingList, entityId = entityId, schoolYearId = schoolYearId, flatten = flatten, returnResponse = returnResponse)
	}

	#' Get a TempRollUnappliedPayment
	#'
	#' This function returns a dataframe or json object of a TempRollUnappliedPayment
	#' @param TempRollUnappliedPaymentID The ID of the TempRollUnappliedPayment to return.
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given TempRollUnappliedPayment. Defaults to FALSE for all return fields which, for convenience, returns all fields for the TempRollUnappliedPayment.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object listed as text. Run \code{\link{getSchemaForObjects}}('TempRollUnappliedPayment') to get more field paths.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{listSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Year End
	#' @return A dataframe or of TempRollUnappliedPayment
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	getTempRollUnappliedPayment <- function(TempRollUnappliedPaymentID, CustomerID = F, CustomerName = F, UnappliedAmount = F, IsExceptionRecord = F, ExceptionReason = F, RollType = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, PayorID = F, CreateCustomerEntityYear = F, ErrorCount = F, fieldPaths = NULL, entityId = 1, schoolYearId = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment()) %>% purrr::keep(names(.) != "TempRollUnappliedPaymentID")

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		getSkyObject(module = "YearEnd", objectName = "TempRollUnappliedPayment", objectId = TempRollUnappliedPaymentID, searchFields = searchFields, entityId = entityId, flatten = flatten, returnResponse = returnResponse)
	}

	#' Delete a TempRollUnappliedPayment
	#'
	#' This function deletes a TempRollUnappliedPayment
	#' @param TempRollUnappliedPaymentID The ID of the TempRollUnappliedPayment to delete
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{listSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Year End
	#' @return The TempRollUnappliedPaymentID of the deleted TempRollUnappliedPayment.
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	deleteTempRollUnappliedPayment <- function(TempRollUnappliedPaymentID, ignoreWarnings = F, entityId = 1, schoolYearId = NULL, flatten = T, returnResponse = F){

		deleteSkyObject(module = "YearEnd", objectName = "TempRollUnappliedPayment", objectId = TempRollUnappliedPaymentID, ignoreWarnings = ignoreWarnings, entityId = entityId, flatten = flatten, returnResponse = returnResponse)
	}

	#' Create a TempRollUnappliedPayment
	#'
	#' This function creates a TempRollUnappliedPayment
	#' @param fieldNames The field values to give the created TempRollUnappliedPayment. Each defaults to NULL.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{listSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Year End
	#' @return A newly created TempRollUnappliedPayment
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	createTempRollUnappliedPayment <- function(CustomerID = NULL, CustomerName = NULL, UnappliedAmount = NULL, ExceptionReason = NULL, RollType = NULL, PayorID = NULL, CreateCustomerEntityYear = NULL, ErrorCount = NULL, entityId = 1, schoolYearId = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment())

		body <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper()) %>% purrr::compact()

		createSkyObject(module = "YearEnd", objectName = "TempRollUnappliedPayment", body = list(DataObject = body), searchFields = append("TempRollUnappliedPaymentID", body %>% names()), entityId = entityId, schoolYearId = schoolYearId, flatten = flatten, returnResponse = returnResponse)
	}

	#' Modify a TempRollUnappliedPayment
	#'
	#' This function modifies a TempRollUnappliedPayment
	#' @param fieldNames The field values to give the modified TempRollUnappliedPayment. Each defaults to NULL.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{listSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Year End
	#' @return The modified TempRollUnappliedPayment
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	modifyTempRollUnappliedPayment <- function(TempRollUnappliedPaymentID, CustomerID = NULL, CustomerName = NULL, UnappliedAmount = NULL, ExceptionReason = NULL, RollType = NULL, PayorID = NULL, CreateCustomerEntityYear = NULL, ErrorCount = NULL, entityId = 1, schoolYearId = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment())

		body <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper()) %>% purrr::compact()

		modifySkyObject(module = "YearEnd", objectName = "TempRollUnappliedPayment", objectId = TempRollUnappliedPaymentID, body = list(DataObject = body), searchFields = append("TempRollUnappliedPaymentID", body %>% names()), entityId = entityId, schoolYearId = schoolYearId, flatten = flatten, returnResponse = returnResponse)
	}

	#' List TempOpenPurchaseOrderExceptions
	#'
	#' This function returns a dataframe or json object of TempOpenPurchaseOrderExceptions
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given TempOpenPurchaseOrderExceptions. Defaults to FALSE for all return fields which, for convenience, returns all fields for the TempOpenPurchaseOrderExceptions.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object listed as text. Run \code{\link{getSchemaForObjects}}('TempOpenPurchaseOrderException') to get more field paths.
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
	#' @concept Year End
	#' @return A list of TempOpenPurchaseOrderExceptions
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	listTempOpenPurchaseOrderExceptions <- function(searchConditionsList = NULL, TempOpenPurchaseOrderExceptionID = F, PurchaseOrderNumber = F, VendorName = F, Description = F, OriginalPurchaseOrderAmount = F, RemainingEncumbrance = F, ExceptionReason = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, fieldPaths = NULL, searchConditionsGroupType = "And", searchSortFieldNamesList = NULL, searchSortFieldNamesDescendingList = NULL, entityId = 1, schoolYearId = NULL, page = 1, pageSize = 100000, flatten = T, returnResponse = F){

		params <- as.list(environment())

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		listSkyObjects(module = "YearEnd", objectName = "TempOpenPurchaseOrderException", searchFields = searchFields %>% append(fieldPaths), page = page, pageSize = pageSize, SearchConditionsList = searchConditionsList, SearchConditionsGroupType = searchConditionsGroupType, SearchSortFieldNamesList = searchSortFieldNamesList, SearchSortFieldNamesDescendingList = searchSortFieldNamesDescendingList, entityId = entityId, schoolYearId = schoolYearId, flatten = flatten, returnResponse = returnResponse)
	}

	#' Get a TempOpenPurchaseOrderException
	#'
	#' This function returns a dataframe or json object of a TempOpenPurchaseOrderException
	#' @param TempOpenPurchaseOrderExceptionID The ID of the TempOpenPurchaseOrderException to return.
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given TempOpenPurchaseOrderException. Defaults to FALSE for all return fields which, for convenience, returns all fields for the TempOpenPurchaseOrderException.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object listed as text. Run \code{\link{getSchemaForObjects}}('TempOpenPurchaseOrderException') to get more field paths.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{listSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Year End
	#' @return A dataframe or of TempOpenPurchaseOrderException
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	getTempOpenPurchaseOrderException <- function(TempOpenPurchaseOrderExceptionID, PurchaseOrderNumber = F, VendorName = F, Description = F, OriginalPurchaseOrderAmount = F, RemainingEncumbrance = F, ExceptionReason = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, fieldPaths = NULL, entityId = 1, schoolYearId = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment()) %>% purrr::keep(names(.) != "TempOpenPurchaseOrderExceptionID")

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		getSkyObject(module = "YearEnd", objectName = "TempOpenPurchaseOrderException", objectId = TempOpenPurchaseOrderExceptionID, searchFields = searchFields, entityId = entityId, flatten = flatten, returnResponse = returnResponse)
	}

	#' Delete a TempOpenPurchaseOrderException
	#'
	#' This function deletes a TempOpenPurchaseOrderException
	#' @param TempOpenPurchaseOrderExceptionID The ID of the TempOpenPurchaseOrderException to delete
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{listSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Year End
	#' @return The TempOpenPurchaseOrderExceptionID of the deleted TempOpenPurchaseOrderException.
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	deleteTempOpenPurchaseOrderException <- function(TempOpenPurchaseOrderExceptionID, ignoreWarnings = F, entityId = 1, schoolYearId = NULL, flatten = T, returnResponse = F){

		deleteSkyObject(module = "YearEnd", objectName = "TempOpenPurchaseOrderException", objectId = TempOpenPurchaseOrderExceptionID, ignoreWarnings = ignoreWarnings, entityId = entityId, flatten = flatten, returnResponse = returnResponse)
	}

	#' Create a TempOpenPurchaseOrderException
	#'
	#' This function creates a TempOpenPurchaseOrderException
	#' @param fieldNames The field values to give the created TempOpenPurchaseOrderException. Each defaults to NULL.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{listSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Year End
	#' @return A newly created TempOpenPurchaseOrderException
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	createTempOpenPurchaseOrderException <- function(PurchaseOrderNumber = NULL, VendorName = NULL, Description = NULL, OriginalPurchaseOrderAmount = NULL, RemainingEncumbrance = NULL, ExceptionReason = NULL, entityId = 1, schoolYearId = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment())

		body <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper()) %>% purrr::compact()

		createSkyObject(module = "YearEnd", objectName = "TempOpenPurchaseOrderException", body = list(DataObject = body), searchFields = append("TempOpenPurchaseOrderExceptionID", body %>% names()), entityId = entityId, schoolYearId = schoolYearId, flatten = flatten, returnResponse = returnResponse)
	}

	#' Modify a TempOpenPurchaseOrderException
	#'
	#' This function modifies a TempOpenPurchaseOrderException
	#' @param fieldNames The field values to give the modified TempOpenPurchaseOrderException. Each defaults to NULL.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{listSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Year End
	#' @return The modified TempOpenPurchaseOrderException
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	modifyTempOpenPurchaseOrderException <- function(TempOpenPurchaseOrderExceptionID, PurchaseOrderNumber = NULL, VendorName = NULL, Description = NULL, OriginalPurchaseOrderAmount = NULL, RemainingEncumbrance = NULL, ExceptionReason = NULL, entityId = 1, schoolYearId = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment())

		body <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper()) %>% purrr::compact()

		modifySkyObject(module = "YearEnd", objectName = "TempOpenPurchaseOrderException", objectId = TempOpenPurchaseOrderExceptionID, body = list(DataObject = body), searchFields = append("TempOpenPurchaseOrderExceptionID", body %>% names()), entityId = entityId, schoolYearId = schoolYearId, flatten = flatten, returnResponse = returnResponse)
	}

	#' List TempOpenPurchaseOrderMissingAccounts
	#'
	#' This function returns a dataframe or json object of TempOpenPurchaseOrderMissingAccounts
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given TempOpenPurchaseOrderMissingAccounts. Defaults to FALSE for all return fields which, for convenience, returns all fields for the TempOpenPurchaseOrderMissingAccounts.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object listed as text. Run \code{\link{getSchemaForObjects}}('TempOpenPurchaseOrderMissingAccount') to get more field paths.
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
	#' @concept Year End
	#' @return A list of TempOpenPurchaseOrderMissingAccounts
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	listTempOpenPurchaseOrderMissingAccounts <- function(searchConditionsList = NULL, TempOpenPurchaseOrderMissingAccountID = F, DisplayAccount = F, ErrorMessage = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, fieldPaths = NULL, searchConditionsGroupType = "And", searchSortFieldNamesList = NULL, searchSortFieldNamesDescendingList = NULL, entityId = 1, schoolYearId = NULL, page = 1, pageSize = 100000, flatten = T, returnResponse = F){

		params <- as.list(environment())

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		listSkyObjects(module = "YearEnd", objectName = "TempOpenPurchaseOrderMissingAccount", searchFields = searchFields %>% append(fieldPaths), page = page, pageSize = pageSize, SearchConditionsList = searchConditionsList, SearchConditionsGroupType = searchConditionsGroupType, SearchSortFieldNamesList = searchSortFieldNamesList, SearchSortFieldNamesDescendingList = searchSortFieldNamesDescendingList, entityId = entityId, schoolYearId = schoolYearId, flatten = flatten, returnResponse = returnResponse)
	}

	#' Get a TempOpenPurchaseOrderMissingAccount
	#'
	#' This function returns a dataframe or json object of a TempOpenPurchaseOrderMissingAccount
	#' @param TempOpenPurchaseOrderMissingAccountID The ID of the TempOpenPurchaseOrderMissingAccount to return.
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given TempOpenPurchaseOrderMissingAccount. Defaults to FALSE for all return fields which, for convenience, returns all fields for the TempOpenPurchaseOrderMissingAccount.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object listed as text. Run \code{\link{getSchemaForObjects}}('TempOpenPurchaseOrderMissingAccount') to get more field paths.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{listSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Year End
	#' @return A dataframe or of TempOpenPurchaseOrderMissingAccount
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	getTempOpenPurchaseOrderMissingAccount <- function(TempOpenPurchaseOrderMissingAccountID, DisplayAccount = F, ErrorMessage = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, fieldPaths = NULL, entityId = 1, schoolYearId = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment()) %>% purrr::keep(names(.) != "TempOpenPurchaseOrderMissingAccountID")

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		getSkyObject(module = "YearEnd", objectName = "TempOpenPurchaseOrderMissingAccount", objectId = TempOpenPurchaseOrderMissingAccountID, searchFields = searchFields, entityId = entityId, flatten = flatten, returnResponse = returnResponse)
	}

	#' Delete a TempOpenPurchaseOrderMissingAccount
	#'
	#' This function deletes a TempOpenPurchaseOrderMissingAccount
	#' @param TempOpenPurchaseOrderMissingAccountID The ID of the TempOpenPurchaseOrderMissingAccount to delete
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{listSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Year End
	#' @return The TempOpenPurchaseOrderMissingAccountID of the deleted TempOpenPurchaseOrderMissingAccount.
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	deleteTempOpenPurchaseOrderMissingAccount <- function(TempOpenPurchaseOrderMissingAccountID, ignoreWarnings = F, entityId = 1, schoolYearId = NULL, flatten = T, returnResponse = F){

		deleteSkyObject(module = "YearEnd", objectName = "TempOpenPurchaseOrderMissingAccount", objectId = TempOpenPurchaseOrderMissingAccountID, ignoreWarnings = ignoreWarnings, entityId = entityId, flatten = flatten, returnResponse = returnResponse)
	}

	#' Create a TempOpenPurchaseOrderMissingAccount
	#'
	#' This function creates a TempOpenPurchaseOrderMissingAccount
	#' @param fieldNames The field values to give the created TempOpenPurchaseOrderMissingAccount. Each defaults to NULL.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{listSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Year End
	#' @return A newly created TempOpenPurchaseOrderMissingAccount
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	createTempOpenPurchaseOrderMissingAccount <- function(DisplayAccount = NULL, ErrorMessage = NULL, entityId = 1, schoolYearId = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment())

		body <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper()) %>% purrr::compact()

		createSkyObject(module = "YearEnd", objectName = "TempOpenPurchaseOrderMissingAccount", body = list(DataObject = body), searchFields = append("TempOpenPurchaseOrderMissingAccountID", body %>% names()), entityId = entityId, schoolYearId = schoolYearId, flatten = flatten, returnResponse = returnResponse)
	}

	#' Modify a TempOpenPurchaseOrderMissingAccount
	#'
	#' This function modifies a TempOpenPurchaseOrderMissingAccount
	#' @param fieldNames The field values to give the modified TempOpenPurchaseOrderMissingAccount. Each defaults to NULL.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{listSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Year End
	#' @return The modified TempOpenPurchaseOrderMissingAccount
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	modifyTempOpenPurchaseOrderMissingAccount <- function(TempOpenPurchaseOrderMissingAccountID, DisplayAccount = NULL, ErrorMessage = NULL, entityId = 1, schoolYearId = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment())

		body <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper()) %>% purrr::compact()

		modifySkyObject(module = "YearEnd", objectName = "TempOpenPurchaseOrderMissingAccount", objectId = TempOpenPurchaseOrderMissingAccountID, body = list(DataObject = body), searchFields = append("TempOpenPurchaseOrderMissingAccountID", body %>% names()), entityId = entityId, schoolYearId = schoolYearId, flatten = flatten, returnResponse = returnResponse)
	}

	#' List TempYearEndDateClones
	#'
	#' This function returns a dataframe or json object of TempYearEndDateClones
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given TempYearEndDateClones. Defaults to FALSE for all return fields which, for convenience, returns all fields for the TempYearEndDateClones.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object listed as text. Run \code{\link{getSchemaForObjects}}('TempYearEndDateClone') to get more field paths.
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
	#' @concept Year End
	#' @return A list of TempYearEndDateClones
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	listTempYearEndDateClones <- function(searchConditionsList = NULL, TempYearEndDateCloneID = F, SourceDate = F, TargetDate = F, SourceDateUsedBy = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, fieldPaths = NULL, searchConditionsGroupType = "And", searchSortFieldNamesList = NULL, searchSortFieldNamesDescendingList = NULL, entityId = 1, schoolYearId = NULL, page = 1, pageSize = 100000, flatten = T, returnResponse = F){

		params <- as.list(environment())

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		listSkyObjects(module = "YearEnd", objectName = "TempYearEndDateClone", searchFields = searchFields %>% append(fieldPaths), page = page, pageSize = pageSize, SearchConditionsList = searchConditionsList, SearchConditionsGroupType = searchConditionsGroupType, SearchSortFieldNamesList = searchSortFieldNamesList, SearchSortFieldNamesDescendingList = searchSortFieldNamesDescendingList, entityId = entityId, schoolYearId = schoolYearId, flatten = flatten, returnResponse = returnResponse)
	}

	#' Get a TempYearEndDateClone
	#'
	#' This function returns a dataframe or json object of a TempYearEndDateClone
	#' @param TempYearEndDateCloneID The ID of the TempYearEndDateClone to return.
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given TempYearEndDateClone. Defaults to FALSE for all return fields which, for convenience, returns all fields for the TempYearEndDateClone.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object listed as text. Run \code{\link{getSchemaForObjects}}('TempYearEndDateClone') to get more field paths.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{listSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Year End
	#' @return A dataframe or of TempYearEndDateClone
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	getTempYearEndDateClone <- function(TempYearEndDateCloneID, SourceDate = F, TargetDate = F, SourceDateUsedBy = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, fieldPaths = NULL, entityId = 1, schoolYearId = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment()) %>% purrr::keep(names(.) != "TempYearEndDateCloneID")

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		getSkyObject(module = "YearEnd", objectName = "TempYearEndDateClone", objectId = TempYearEndDateCloneID, searchFields = searchFields, entityId = entityId, flatten = flatten, returnResponse = returnResponse)
	}

	#' Delete a TempYearEndDateClone
	#'
	#' This function deletes a TempYearEndDateClone
	#' @param TempYearEndDateCloneID The ID of the TempYearEndDateClone to delete
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{listSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Year End
	#' @return The TempYearEndDateCloneID of the deleted TempYearEndDateClone.
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	deleteTempYearEndDateClone <- function(TempYearEndDateCloneID, ignoreWarnings = F, entityId = 1, schoolYearId = NULL, flatten = T, returnResponse = F){

		deleteSkyObject(module = "YearEnd", objectName = "TempYearEndDateClone", objectId = TempYearEndDateCloneID, ignoreWarnings = ignoreWarnings, entityId = entityId, flatten = flatten, returnResponse = returnResponse)
	}

	#' Create a TempYearEndDateClone
	#'
	#' This function creates a TempYearEndDateClone
	#' @param fieldNames The field values to give the created TempYearEndDateClone. Each defaults to NULL.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{listSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Year End
	#' @return A newly created TempYearEndDateClone
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	createTempYearEndDateClone <- function(SourceDate = NULL, TargetDate = NULL, SourceDateUsedBy = NULL, entityId = 1, schoolYearId = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment())

		body <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper()) %>% purrr::compact()

		createSkyObject(module = "YearEnd", objectName = "TempYearEndDateClone", body = list(DataObject = body), searchFields = append("TempYearEndDateCloneID", body %>% names()), entityId = entityId, schoolYearId = schoolYearId, flatten = flatten, returnResponse = returnResponse)
	}

	#' Modify a TempYearEndDateClone
	#'
	#' This function modifies a TempYearEndDateClone
	#' @param fieldNames The field values to give the modified TempYearEndDateClone. Each defaults to NULL.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{listSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Year End
	#' @return The modified TempYearEndDateClone
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	modifyTempYearEndDateClone <- function(TempYearEndDateCloneID, SourceDate = NULL, TargetDate = NULL, SourceDateUsedBy = NULL, entityId = 1, schoolYearId = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment())

		body <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper()) %>% purrr::compact()

		modifySkyObject(module = "YearEnd", objectName = "TempYearEndDateClone", objectId = TempYearEndDateCloneID, body = list(DataObject = body), searchFields = append("TempYearEndDateCloneID", body %>% names()), entityId = entityId, schoolYearId = schoolYearId, flatten = flatten, returnResponse = returnResponse)
	}

	#' List TempYearEndTypeSummaries
	#'
	#' This function returns a dataframe or json object of TempYearEndTypeSummaries
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given TempYearEndTypeSummaries. Defaults to FALSE for all return fields which, for convenience, returns all fields for the TempYearEndTypeSummaries.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object listed as text. Run \code{\link{getSchemaForObjects}}('TempYearEndTypeSummary') to get more field paths.
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
	#' @concept Year End
	#' @return A list of TempYearEndTypeSummaries
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	listTempYearEndTypeSummaries <- function(searchConditionsList = NULL, TempYearEndTypeSummaryID = F, DisplayName = F, SourceRecordsFound = F, TargetRecordsFound = F, EstimatedRecordsToProcess = F, RecordsProcessedCount = F, SuccessCount = F, FailureCount = F, CloneUnsuccessful = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, SkywardIDObject = F, fieldPaths = NULL, searchConditionsGroupType = "And", searchSortFieldNamesList = NULL, searchSortFieldNamesDescendingList = NULL, entityId = 1, schoolYearId = NULL, page = 1, pageSize = 100000, flatten = T, returnResponse = F){

		params <- as.list(environment())

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		listSkyObjects(module = "YearEnd", objectName = "TempYearEndTypeSummary", searchFields = searchFields %>% append(fieldPaths), page = page, pageSize = pageSize, SearchConditionsList = searchConditionsList, SearchConditionsGroupType = searchConditionsGroupType, SearchSortFieldNamesList = searchSortFieldNamesList, SearchSortFieldNamesDescendingList = searchSortFieldNamesDescendingList, entityId = entityId, schoolYearId = schoolYearId, flatten = flatten, returnResponse = returnResponse)
	}

	#' Get a TempYearEndTypeSummary
	#'
	#' This function returns a dataframe or json object of a TempYearEndTypeSummary
	#' @param TempYearEndTypeSummaryID The ID of the TempYearEndTypeSummary to return.
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given TempYearEndTypeSummary. Defaults to FALSE for all return fields which, for convenience, returns all fields for the TempYearEndTypeSummary.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object listed as text. Run \code{\link{getSchemaForObjects}}('TempYearEndTypeSummary') to get more field paths.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{listSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Year End
	#' @return A dataframe or of TempYearEndTypeSummary
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	getTempYearEndTypeSummary <- function(TempYearEndTypeSummaryID, DisplayName = F, SourceRecordsFound = F, TargetRecordsFound = F, EstimatedRecordsToProcess = F, RecordsProcessedCount = F, SuccessCount = F, FailureCount = F, CloneUnsuccessful = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, SkywardIDObject = F, fieldPaths = NULL, entityId = 1, schoolYearId = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment()) %>% purrr::keep(names(.) != "TempYearEndTypeSummaryID")

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		getSkyObject(module = "YearEnd", objectName = "TempYearEndTypeSummary", objectId = TempYearEndTypeSummaryID, searchFields = searchFields, entityId = entityId, flatten = flatten, returnResponse = returnResponse)
	}

	#' Delete a TempYearEndTypeSummary
	#'
	#' This function deletes a TempYearEndTypeSummary
	#' @param TempYearEndTypeSummaryID The ID of the TempYearEndTypeSummary to delete
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{listSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Year End
	#' @return The TempYearEndTypeSummaryID of the deleted TempYearEndTypeSummary.
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	deleteTempYearEndTypeSummary <- function(TempYearEndTypeSummaryID, ignoreWarnings = F, entityId = 1, schoolYearId = NULL, flatten = T, returnResponse = F){

		deleteSkyObject(module = "YearEnd", objectName = "TempYearEndTypeSummary", objectId = TempYearEndTypeSummaryID, ignoreWarnings = ignoreWarnings, entityId = entityId, flatten = flatten, returnResponse = returnResponse)
	}

	#' Create a TempYearEndTypeSummary
	#'
	#' This function creates a TempYearEndTypeSummary
	#' @param fieldNames The field values to give the created TempYearEndTypeSummary. Each defaults to NULL.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{listSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Year End
	#' @return A newly created TempYearEndTypeSummary
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	createTempYearEndTypeSummary <- function(DisplayName = NULL, SourceRecordsFound = NULL, TargetRecordsFound = NULL, EstimatedRecordsToProcess = NULL, RecordsProcessedCount = NULL, SuccessCount = NULL, FailureCount = NULL, SkywardIDObject = NULL, entityId = 1, schoolYearId = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment())

		body <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper()) %>% purrr::compact()

		createSkyObject(module = "YearEnd", objectName = "TempYearEndTypeSummary", body = list(DataObject = body), searchFields = append("TempYearEndTypeSummaryID", body %>% names()), entityId = entityId, schoolYearId = schoolYearId, flatten = flatten, returnResponse = returnResponse)
	}

	#' Modify a TempYearEndTypeSummary
	#'
	#' This function modifies a TempYearEndTypeSummary
	#' @param fieldNames The field values to give the modified TempYearEndTypeSummary. Each defaults to NULL.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{listSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Year End
	#' @return The modified TempYearEndTypeSummary
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	modifyTempYearEndTypeSummary <- function(TempYearEndTypeSummaryID, DisplayName = NULL, SourceRecordsFound = NULL, TargetRecordsFound = NULL, EstimatedRecordsToProcess = NULL, RecordsProcessedCount = NULL, SuccessCount = NULL, FailureCount = NULL, SkywardIDObject = NULL, entityId = 1, schoolYearId = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment())

		body <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper()) %>% purrr::compact()

		modifySkyObject(module = "YearEnd", objectName = "TempYearEndTypeSummary", objectId = TempYearEndTypeSummaryID, body = list(DataObject = body), searchFields = append("TempYearEndTypeSummaryID", body %>% names()), entityId = entityId, schoolYearId = schoolYearId, flatten = flatten, returnResponse = returnResponse)
	}

	#' List TempYearEndSaveFailures
	#'
	#' This function returns a dataframe or json object of TempYearEndSaveFailures
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given TempYearEndSaveFailures. Defaults to FALSE for all return fields which, for convenience, returns all fields for the TempYearEndSaveFailures.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object listed as text. Run \code{\link{getSchemaForObjects}}('TempYearEndSaveFailure') to get more field paths.
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
	#' @concept Year End
	#' @return A list of TempYearEndSaveFailures
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	listTempYearEndSaveFailures <- function(searchConditionsList = NULL, TempYearEndSaveFailureID = F, Message = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, CodeDescription = F, SkywardIDObject = F, Description = F, fieldPaths = NULL, searchConditionsGroupType = "And", searchSortFieldNamesList = NULL, searchSortFieldNamesDescendingList = NULL, entityId = 1, schoolYearId = NULL, page = 1, pageSize = 100000, flatten = T, returnResponse = F){

		params <- as.list(environment())

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		listSkyObjects(module = "YearEnd", objectName = "TempYearEndSaveFailure", searchFields = searchFields %>% append(fieldPaths), page = page, pageSize = pageSize, SearchConditionsList = searchConditionsList, SearchConditionsGroupType = searchConditionsGroupType, SearchSortFieldNamesList = searchSortFieldNamesList, SearchSortFieldNamesDescendingList = searchSortFieldNamesDescendingList, entityId = entityId, schoolYearId = schoolYearId, flatten = flatten, returnResponse = returnResponse)
	}

	#' Get a TempYearEndSaveFailure
	#'
	#' This function returns a dataframe or json object of a TempYearEndSaveFailure
	#' @param TempYearEndSaveFailureID The ID of the TempYearEndSaveFailure to return.
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given TempYearEndSaveFailure. Defaults to FALSE for all return fields which, for convenience, returns all fields for the TempYearEndSaveFailure.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object listed as text. Run \code{\link{getSchemaForObjects}}('TempYearEndSaveFailure') to get more field paths.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{listSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Year End
	#' @return A dataframe or of TempYearEndSaveFailure
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	getTempYearEndSaveFailure <- function(TempYearEndSaveFailureID, Message = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, CodeDescription = F, SkywardIDObject = F, Description = F, fieldPaths = NULL, entityId = 1, schoolYearId = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment()) %>% purrr::keep(names(.) != "TempYearEndSaveFailureID")

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		getSkyObject(module = "YearEnd", objectName = "TempYearEndSaveFailure", objectId = TempYearEndSaveFailureID, searchFields = searchFields, entityId = entityId, flatten = flatten, returnResponse = returnResponse)
	}

	#' Delete a TempYearEndSaveFailure
	#'
	#' This function deletes a TempYearEndSaveFailure
	#' @param TempYearEndSaveFailureID The ID of the TempYearEndSaveFailure to delete
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{listSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Year End
	#' @return The TempYearEndSaveFailureID of the deleted TempYearEndSaveFailure.
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	deleteTempYearEndSaveFailure <- function(TempYearEndSaveFailureID, ignoreWarnings = F, entityId = 1, schoolYearId = NULL, flatten = T, returnResponse = F){

		deleteSkyObject(module = "YearEnd", objectName = "TempYearEndSaveFailure", objectId = TempYearEndSaveFailureID, ignoreWarnings = ignoreWarnings, entityId = entityId, flatten = flatten, returnResponse = returnResponse)
	}

	#' Create a TempYearEndSaveFailure
	#'
	#' This function creates a TempYearEndSaveFailure
	#' @param fieldNames The field values to give the created TempYearEndSaveFailure. Each defaults to NULL.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{listSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Year End
	#' @return A newly created TempYearEndSaveFailure
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	createTempYearEndSaveFailure <- function(Message = NULL, CodeDescription = NULL, SkywardIDObject = NULL, Description = NULL, entityId = 1, schoolYearId = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment())

		body <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper()) %>% purrr::compact()

		createSkyObject(module = "YearEnd", objectName = "TempYearEndSaveFailure", body = list(DataObject = body), searchFields = append("TempYearEndSaveFailureID", body %>% names()), entityId = entityId, schoolYearId = schoolYearId, flatten = flatten, returnResponse = returnResponse)
	}

	#' Modify a TempYearEndSaveFailure
	#'
	#' This function modifies a TempYearEndSaveFailure
	#' @param fieldNames The field values to give the modified TempYearEndSaveFailure. Each defaults to NULL.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{listSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Year End
	#' @return The modified TempYearEndSaveFailure
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	modifyTempYearEndSaveFailure <- function(TempYearEndSaveFailureID, Message = NULL, CodeDescription = NULL, SkywardIDObject = NULL, Description = NULL, entityId = 1, schoolYearId = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment())

		body <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper()) %>% purrr::compact()

		modifySkyObject(module = "YearEnd", objectName = "TempYearEndSaveFailure", objectId = TempYearEndSaveFailureID, body = list(DataObject = body), searchFields = append("TempYearEndSaveFailureID", body %>% names()), entityId = entityId, schoolYearId = schoolYearId, flatten = flatten, returnResponse = returnResponse)
	}

	#' List TempHRCodes
	#'
	#' This function returns a dataframe or json object of TempHRCodes
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given TempHRCodes. Defaults to FALSE for all return fields which, for convenience, returns all fields for the TempHRCodes.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object listed as text. Run \code{\link{getSchemaForObjects}}('TempHRCode') to get more field paths.
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
	#' @concept Year End
	#' @return A list of TempHRCodes
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	listTempHRCodes <- function(searchConditionsList = NULL, TempHRCodeID = F, ModuleName = F, ObjectName = F, Code = F, HRCodePrimaryKey = F, HasException = F, Message = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, fieldPaths = NULL, searchConditionsGroupType = "And", searchSortFieldNamesList = NULL, searchSortFieldNamesDescendingList = NULL, entityId = 1, schoolYearId = NULL, page = 1, pageSize = 100000, flatten = T, returnResponse = F){

		params <- as.list(environment())

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		listSkyObjects(module = "YearEnd", objectName = "TempHRCode", searchFields = searchFields %>% append(fieldPaths), page = page, pageSize = pageSize, SearchConditionsList = searchConditionsList, SearchConditionsGroupType = searchConditionsGroupType, SearchSortFieldNamesList = searchSortFieldNamesList, SearchSortFieldNamesDescendingList = searchSortFieldNamesDescendingList, entityId = entityId, schoolYearId = schoolYearId, flatten = flatten, returnResponse = returnResponse)
	}

	#' Get a TempHRCode
	#'
	#' This function returns a dataframe or json object of a TempHRCode
	#' @param TempHRCodeID The ID of the TempHRCode to return.
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given TempHRCode. Defaults to FALSE for all return fields which, for convenience, returns all fields for the TempHRCode.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object listed as text. Run \code{\link{getSchemaForObjects}}('TempHRCode') to get more field paths.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{listSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Year End
	#' @return A dataframe or of TempHRCode
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	getTempHRCode <- function(TempHRCodeID, ModuleName = F, ObjectName = F, Code = F, HRCodePrimaryKey = F, HasException = F, Message = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, fieldPaths = NULL, entityId = 1, schoolYearId = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment()) %>% purrr::keep(names(.) != "TempHRCodeID")

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		getSkyObject(module = "YearEnd", objectName = "TempHRCode", objectId = TempHRCodeID, searchFields = searchFields, entityId = entityId, flatten = flatten, returnResponse = returnResponse)
	}

	#' Delete a TempHRCode
	#'
	#' This function deletes a TempHRCode
	#' @param TempHRCodeID The ID of the TempHRCode to delete
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{listSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Year End
	#' @return The TempHRCodeID of the deleted TempHRCode.
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	deleteTempHRCode <- function(TempHRCodeID, ignoreWarnings = F, entityId = 1, schoolYearId = NULL, flatten = T, returnResponse = F){

		deleteSkyObject(module = "YearEnd", objectName = "TempHRCode", objectId = TempHRCodeID, ignoreWarnings = ignoreWarnings, entityId = entityId, flatten = flatten, returnResponse = returnResponse)
	}

	#' Create a TempHRCode
	#'
	#' This function creates a TempHRCode
	#' @param fieldNames The field values to give the created TempHRCode. Each defaults to NULL.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{listSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Year End
	#' @return A newly created TempHRCode
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	createTempHRCode <- function(ModuleName = NULL, ObjectName = NULL, Code = NULL, HRCodePrimaryKey = NULL, HasException = NULL, Message = NULL, entityId = 1, schoolYearId = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment())

		body <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper()) %>% purrr::compact()

		createSkyObject(module = "YearEnd", objectName = "TempHRCode", body = list(DataObject = body), searchFields = append("TempHRCodeID", body %>% names()), entityId = entityId, schoolYearId = schoolYearId, flatten = flatten, returnResponse = returnResponse)
	}

	#' Modify a TempHRCode
	#'
	#' This function modifies a TempHRCode
	#' @param fieldNames The field values to give the modified TempHRCode. Each defaults to NULL.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{listSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Year End
	#' @return The modified TempHRCode
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	modifyTempHRCode <- function(TempHRCodeID, ModuleName = NULL, ObjectName = NULL, Code = NULL, HRCodePrimaryKey = NULL, HasException = NULL, Message = NULL, entityId = 1, schoolYearId = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment())

		body <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper()) %>% purrr::compact()

		modifySkyObject(module = "YearEnd", objectName = "TempHRCode", objectId = TempHRCodeID, body = list(DataObject = body), searchFields = append("TempHRCodeID", body %>% names()), entityId = entityId, schoolYearId = schoolYearId, flatten = flatten, returnResponse = returnResponse)
	}

	#' List StudentYearEnds
	#'
	#' This function returns a dataframe or json object of StudentYearEnds
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given StudentYearEnds. Defaults to FALSE for all return fields which, for convenience, returns all fields for the StudentYearEnds.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object listed as text. Run \code{\link{getSchemaForObjects}}('StudentYearEnd') to get more field paths.
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
	#' @concept Year End
	#' @return A list of StudentYearEnds
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	listStudentYearEnds <- function(searchConditionsList = NULL, StudentYearEndID = F, SchoolYearID = F, EntityID = F, CloneDistrictCodesComplete = F, CloneAddressRangeDefaultsComplete = F, CloneEnrollmentCodesComplete = F, CloneCurriculumYearsComplete = F, CloneStaffCodesComplete = F, CloneCourseMasterComplete = F, CloneAttendanceCodesComplete = F, CloneGradingCodesComplete = F, CloneGradebookCodesComplete = F, CloneDemographicCodesComplete = F, CloneFamilyCodesComplete = F, CloneDisciplineCodesComplete = F, CloneActivityCodesComplete = F, CloneFoodServiceCodesComplete = F, CloneFeeManagementCodesComplete = F, CloneHealthCodesComplete = F, RollUnappliedPaymentsComplete = F, RollUnpaidCustomerFeesComplete = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, CloneBusRoutesComplete = F, CloneTransportationCodesComplete = F, CloneStudentCodesComplete = F, CloneCourseEntityOfferedToSetupComplete = F, CloneMTSSCodesComplete = F, fieldPaths = NULL, searchConditionsGroupType = "And", searchSortFieldNamesList = NULL, searchSortFieldNamesDescendingList = NULL, entityId = 1, schoolYearId = NULL, page = 1, pageSize = 100000, flatten = T, returnResponse = F){

		params <- as.list(environment())

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		listSkyObjects(module = "YearEnd", objectName = "StudentYearEnd", searchFields = searchFields %>% append(fieldPaths), page = page, pageSize = pageSize, SearchConditionsList = searchConditionsList, SearchConditionsGroupType = searchConditionsGroupType, SearchSortFieldNamesList = searchSortFieldNamesList, SearchSortFieldNamesDescendingList = searchSortFieldNamesDescendingList, entityId = entityId, schoolYearId = schoolYearId, flatten = flatten, returnResponse = returnResponse)
	}

	#' Get a StudentYearEnd
	#'
	#' This function returns a dataframe or json object of a StudentYearEnd
	#' @param StudentYearEndID The ID of the StudentYearEnd to return.
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given StudentYearEnd. Defaults to FALSE for all return fields which, for convenience, returns all fields for the StudentYearEnd.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object listed as text. Run \code{\link{getSchemaForObjects}}('StudentYearEnd') to get more field paths.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{listSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Year End
	#' @return A dataframe or of StudentYearEnd
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	getStudentYearEnd <- function(StudentYearEndID, SchoolYearID = F, EntityID = F, CloneDistrictCodesComplete = F, CloneAddressRangeDefaultsComplete = F, CloneEnrollmentCodesComplete = F, CloneCurriculumYearsComplete = F, CloneStaffCodesComplete = F, CloneCourseMasterComplete = F, CloneAttendanceCodesComplete = F, CloneGradingCodesComplete = F, CloneGradebookCodesComplete = F, CloneDemographicCodesComplete = F, CloneFamilyCodesComplete = F, CloneDisciplineCodesComplete = F, CloneActivityCodesComplete = F, CloneFoodServiceCodesComplete = F, CloneFeeManagementCodesComplete = F, CloneHealthCodesComplete = F, RollUnappliedPaymentsComplete = F, RollUnpaidCustomerFeesComplete = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, CloneBusRoutesComplete = F, CloneTransportationCodesComplete = F, CloneStudentCodesComplete = F, CloneCourseEntityOfferedToSetupComplete = F, CloneMTSSCodesComplete = F, fieldPaths = NULL, entityId = 1, schoolYearId = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment()) %>% purrr::keep(names(.) != "StudentYearEndID")

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		getSkyObject(module = "YearEnd", objectName = "StudentYearEnd", objectId = StudentYearEndID, searchFields = searchFields, entityId = entityId, flatten = flatten, returnResponse = returnResponse)
	}

	#' Delete a StudentYearEnd
	#'
	#' This function deletes a StudentYearEnd
	#' @param StudentYearEndID The ID of the StudentYearEnd to delete
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{listSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Year End
	#' @return The StudentYearEndID of the deleted StudentYearEnd.
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	deleteStudentYearEnd <- function(StudentYearEndID, ignoreWarnings = F, entityId = 1, schoolYearId = NULL, flatten = T, returnResponse = F){

		deleteSkyObject(module = "YearEnd", objectName = "StudentYearEnd", objectId = StudentYearEndID, ignoreWarnings = ignoreWarnings, entityId = entityId, flatten = flatten, returnResponse = returnResponse)
	}

	#' Create a StudentYearEnd
	#'
	#' This function creates a StudentYearEnd
	#' @param fieldNames The field values to give the created StudentYearEnd. Each defaults to NULL.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{listSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Year End
	#' @return A newly created StudentYearEnd
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	createStudentYearEnd <- function(SchoolYearID = NULL, EntityID = NULL, CloneDistrictCodesComplete = NULL, CloneAddressRangeDefaultsComplete = NULL, CloneEnrollmentCodesComplete = NULL, CloneCurriculumYearsComplete = NULL, CloneStaffCodesComplete = NULL, CloneCourseMasterComplete = NULL, CloneAttendanceCodesComplete = NULL, CloneGradingCodesComplete = NULL, CloneGradebookCodesComplete = NULL, CloneDemographicCodesComplete = NULL, CloneFamilyCodesComplete = NULL, CloneDisciplineCodesComplete = NULL, CloneActivityCodesComplete = NULL, CloneFoodServiceCodesComplete = NULL, CloneFeeManagementCodesComplete = NULL, CloneHealthCodesComplete = NULL, RollUnappliedPaymentsComplete = NULL, RollUnpaidCustomerFeesComplete = NULL, CloneBusRoutesComplete = NULL, CloneTransportationCodesComplete = NULL, CloneStudentCodesComplete = NULL, CloneCourseEntityOfferedToSetupComplete = NULL, CloneMTSSCodesComplete = NULL, entityId = 1, schoolYearId = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment())

		body <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper()) %>% purrr::compact()

		createSkyObject(module = "YearEnd", objectName = "StudentYearEnd", body = list(DataObject = body), searchFields = append("StudentYearEndID", body %>% names()), entityId = entityId, schoolYearId = schoolYearId, flatten = flatten, returnResponse = returnResponse)
	}

	#' Modify a StudentYearEnd
	#'
	#' This function modifies a StudentYearEnd
	#' @param fieldNames The field values to give the modified StudentYearEnd. Each defaults to NULL.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{listSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Year End
	#' @return The modified StudentYearEnd
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	modifyStudentYearEnd <- function(StudentYearEndID, SchoolYearID = NULL, EntityID = NULL, CloneDistrictCodesComplete = NULL, CloneAddressRangeDefaultsComplete = NULL, CloneEnrollmentCodesComplete = NULL, CloneCurriculumYearsComplete = NULL, CloneStaffCodesComplete = NULL, CloneCourseMasterComplete = NULL, CloneAttendanceCodesComplete = NULL, CloneGradingCodesComplete = NULL, CloneGradebookCodesComplete = NULL, CloneDemographicCodesComplete = NULL, CloneFamilyCodesComplete = NULL, CloneDisciplineCodesComplete = NULL, CloneActivityCodesComplete = NULL, CloneFoodServiceCodesComplete = NULL, CloneFeeManagementCodesComplete = NULL, CloneHealthCodesComplete = NULL, RollUnappliedPaymentsComplete = NULL, RollUnpaidCustomerFeesComplete = NULL, CloneBusRoutesComplete = NULL, CloneTransportationCodesComplete = NULL, CloneStudentCodesComplete = NULL, CloneCourseEntityOfferedToSetupComplete = NULL, CloneMTSSCodesComplete = NULL, entityId = 1, schoolYearId = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment())

		body <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper()) %>% purrr::compact()

		modifySkyObject(module = "YearEnd", objectName = "StudentYearEnd", objectId = StudentYearEndID, body = list(DataObject = body), searchFields = append("StudentYearEndID", body %>% names()), entityId = entityId, schoolYearId = schoolYearId, flatten = flatten, returnResponse = returnResponse)
	}
