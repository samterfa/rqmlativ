
	#' List AccountsReceivableAccruals
	#'
	#' This function returns a dataframe or json object of AccountsReceivableAccruals
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given AccountsReceivableAccruals. Defaults to FALSE for all return fields which, for convenience, returns all fields for the AccountsReceivableAccruals.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object listed as text. Run \code{\link{getSchemaForObjects}}('AccountsReceivableAccrual') to get more field paths.
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
	#' @concept Accounts Receivable
	#' @return A list of AccountsReceivableAccruals
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	listAccountsReceivableAccruals <- function(searchConditionsList = NULL, AccountsReceivableAccrualID = F, Code = F, Description = F, CodeDescription = F, MaskID = F, DistrictID = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, fieldPaths = NULL, searchConditionsGroupType = "And", searchSortFieldNamesList = NULL, searchSortFieldNamesDescendingList = NULL, entityId = 1, schoolYearId = NULL, page = 1, pageSize = 100000, flatten = T, returnResponse = F){

		params <- as.list(environment())

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		listSkyObjects(module = "AccountsReceivable", objectName = "AccountsReceivableAccrual", searchFields = searchFields %>% append(fieldPaths), page = page, pageSize = pageSize, SearchConditionsList = searchConditionsList, SearchConditionsGroupType = searchConditionsGroupType, SearchSortFieldNamesList = searchSortFieldNamesList, SearchSortFieldNamesDescendingList = searchSortFieldNamesDescendingList, entityId = entityId, schoolYearId = schoolYearId, flatten = flatten, returnResponse = returnResponse)
	}

	#' Get an AccountsReceivableAccrual
	#'
	#' This function returns a dataframe or json object of an AccountsReceivableAccrual
	#' @param AccountsReceivableAccrualID The ID of the AccountsReceivableAccrual to return.
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given AccountsReceivableAccrual. Defaults to FALSE for all return fields which, for convenience, returns all fields for the AccountsReceivableAccrual.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object listed as text. Run \code{\link{getSchemaForObjects}}('AccountsReceivableAccrual') to get more field paths.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{listSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Accounts Receivable
	#' @return A dataframe or of AccountsReceivableAccrual
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	getAccountsReceivableAccrual <- function(AccountsReceivableAccrualID, Code = F, Description = F, CodeDescription = F, MaskID = F, DistrictID = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, fieldPaths = NULL, entityId = 1, schoolYearId = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment()) %>% purrr::keep(names(.) != "AccountsReceivableAccrualID")

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		getSkyObject(module = "AccountsReceivable", objectName = "AccountsReceivableAccrual", objectId = AccountsReceivableAccrualID, searchFields = searchFields, entityId = entityId, flatten = flatten, returnResponse = returnResponse)
	}

	#' Delete an AccountsReceivableAccrual
	#'
	#' This function deletes an AccountsReceivableAccrual
	#' @param AccountsReceivableAccrualID The ID of the AccountsReceivableAccrual to delete
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{listSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Accounts Receivable
	#' @return The AccountsReceivableAccrualID of the deleted AccountsReceivableAccrual.
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	deleteAccountsReceivableAccrual <- function(AccountsReceivableAccrualID, ignoreWarnings = F, entityId = 1, schoolYearId = NULL, flatten = T, returnResponse = F){

		deleteSkyObject(module = "AccountsReceivable", objectName = "AccountsReceivableAccrual", objectId = AccountsReceivableAccrualID, ignoreWarnings = ignoreWarnings, entityId = entityId, flatten = flatten, returnResponse = returnResponse)
	}

	#' Create an AccountsReceivableAccrual
	#'
	#' This function creates an AccountsReceivableAccrual
	#' @param fieldNames The field values to give the created AccountsReceivableAccrual. Each defaults to NULL.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{listSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Accounts Receivable
	#' @return A newly created AccountsReceivableAccrual
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	createAccountsReceivableAccrual <- function(Code = NULL, Description = NULL, MaskID = NULL, DistrictID = NULL, entityId = 1, schoolYearId = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment())

		body <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper()) %>% compact()

		createSkyObject(module = "AccountsReceivable", objectName = "AccountsReceivableAccrual", body = list(DataObject = body), searchFields = append("AccountsReceivableAccrualID", body %>% names()), entityId = entityId, schoolYearId = schoolYearId, flatten = flatten, returnResponse = returnResponse)
	}

	#' Modify an AccountsReceivableAccrual
	#'
	#' This function modifies an AccountsReceivableAccrual
	#' @param fieldNames The field values to give the modified AccountsReceivableAccrual. Each defaults to NULL.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{listSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Accounts Receivable
	#' @return The modified AccountsReceivableAccrual
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	modifyAccountsReceivableAccrual <- function(AccountsReceivableAccrualID, Code = NULL, Description = NULL, MaskID = NULL, DistrictID = NULL, entityId = 1, schoolYearId = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment())

		body <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper()) %>% compact()

		modifySkyObject(module = "AccountsReceivable", objectName = "AccountsReceivableAccrual", objectId = AccountsReceivableAccrualID, body = list(DataObject = body), searchFields = append("AccountsReceivableAccrualID", body %>% names()), entityId = entityId, schoolYearId = schoolYearId, flatten = flatten, returnResponse = returnResponse)
	}

	#' List AgingThresholds
	#'
	#' This function returns a dataframe or json object of AgingThresholds
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given AgingThresholds. Defaults to FALSE for all return fields which, for convenience, returns all fields for the AgingThresholds.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object listed as text. Run \code{\link{getSchemaForObjects}}('AgingThreshold') to get more field paths.
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
	#' @concept Accounts Receivable
	#' @return A list of AgingThresholds
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	listAgingThresholds <- function(searchConditionsList = NULL, AgingThresholdID = F, Description = F, StartNumberOfDays = F, EndNumberOfDays = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, DistrictID = F, fieldPaths = NULL, searchConditionsGroupType = "And", searchSortFieldNamesList = NULL, searchSortFieldNamesDescendingList = NULL, entityId = 1, schoolYearId = NULL, page = 1, pageSize = 100000, flatten = T, returnResponse = F){

		params <- as.list(environment())

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		listSkyObjects(module = "AccountsReceivable", objectName = "AgingThreshold", searchFields = searchFields %>% append(fieldPaths), page = page, pageSize = pageSize, SearchConditionsList = searchConditionsList, SearchConditionsGroupType = searchConditionsGroupType, SearchSortFieldNamesList = searchSortFieldNamesList, SearchSortFieldNamesDescendingList = searchSortFieldNamesDescendingList, entityId = entityId, schoolYearId = schoolYearId, flatten = flatten, returnResponse = returnResponse)
	}

	#' Get an AgingThreshold
	#'
	#' This function returns a dataframe or json object of an AgingThreshold
	#' @param AgingThresholdID The ID of the AgingThreshold to return.
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given AgingThreshold. Defaults to FALSE for all return fields which, for convenience, returns all fields for the AgingThreshold.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object listed as text. Run \code{\link{getSchemaForObjects}}('AgingThreshold') to get more field paths.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{listSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Accounts Receivable
	#' @return A dataframe or of AgingThreshold
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	getAgingThreshold <- function(AgingThresholdID, Description = F, StartNumberOfDays = F, EndNumberOfDays = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, DistrictID = F, fieldPaths = NULL, entityId = 1, schoolYearId = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment()) %>% purrr::keep(names(.) != "AgingThresholdID")

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		getSkyObject(module = "AccountsReceivable", objectName = "AgingThreshold", objectId = AgingThresholdID, searchFields = searchFields, entityId = entityId, flatten = flatten, returnResponse = returnResponse)
	}

	#' Delete an AgingThreshold
	#'
	#' This function deletes an AgingThreshold
	#' @param AgingThresholdID The ID of the AgingThreshold to delete
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{listSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Accounts Receivable
	#' @return The AgingThresholdID of the deleted AgingThreshold.
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	deleteAgingThreshold <- function(AgingThresholdID, ignoreWarnings = F, entityId = 1, schoolYearId = NULL, flatten = T, returnResponse = F){

		deleteSkyObject(module = "AccountsReceivable", objectName = "AgingThreshold", objectId = AgingThresholdID, ignoreWarnings = ignoreWarnings, entityId = entityId, flatten = flatten, returnResponse = returnResponse)
	}

	#' Create an AgingThreshold
	#'
	#' This function creates an AgingThreshold
	#' @param fieldNames The field values to give the created AgingThreshold. Each defaults to NULL.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{listSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Accounts Receivable
	#' @return A newly created AgingThreshold
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	createAgingThreshold <- function(Description = NULL, StartNumberOfDays = NULL, EndNumberOfDays = NULL, DistrictID = NULL, entityId = 1, schoolYearId = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment())

		body <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper()) %>% compact()

		createSkyObject(module = "AccountsReceivable", objectName = "AgingThreshold", body = list(DataObject = body), searchFields = append("AgingThresholdID", body %>% names()), entityId = entityId, schoolYearId = schoolYearId, flatten = flatten, returnResponse = returnResponse)
	}

	#' Modify an AgingThreshold
	#'
	#' This function modifies an AgingThreshold
	#' @param fieldNames The field values to give the modified AgingThreshold. Each defaults to NULL.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{listSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Accounts Receivable
	#' @return The modified AgingThreshold
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	modifyAgingThreshold <- function(AgingThresholdID, Description = NULL, StartNumberOfDays = NULL, EndNumberOfDays = NULL, DistrictID = NULL, entityId = 1, schoolYearId = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment())

		body <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper()) %>% compact()

		modifySkyObject(module = "AccountsReceivable", objectName = "AgingThreshold", objectId = AgingThresholdID, body = list(DataObject = body), searchFields = append("AgingThresholdID", body %>% names()), entityId = entityId, schoolYearId = schoolYearId, flatten = flatten, returnResponse = returnResponse)
	}

	#' List AccountsReceivableConfigDistricts
	#'
	#' This function returns a dataframe or json object of AccountsReceivableConfigDistricts
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given AccountsReceivableConfigDistricts. Defaults to FALSE for all return fields which, for convenience, returns all fields for the AccountsReceivableConfigDistricts.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object listed as text. Run \code{\link{getSchemaForObjects}}('AccountsReceivableConfigDistrict') to get more field paths.
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
	#' @concept Accounts Receivable
	#' @return A list of AccountsReceivableConfigDistricts
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	listAccountsReceivableConfigDistricts <- function(searchConditionsList = NULL, ConfigDistrictID = F, DistrictID = F, InvoiceGroupLength = F, InvoiceNumberLength = F, BatchDefault = F, AccountIDUnapplied = F, NameIDBusinessOffice = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, ReportIDInvoiceDelivery = F, ReportIDStatementDelivery = F, EmailSendingAddressInvoice = F, EmailSendingAliasInvoice = F, EmailSendingAddressStatement = F, EmailSendingAliasStatement = F, InvoiceEmailSubject = F, InvoiceEmailBody = F, StatementEmailSubject = F, StatementEmailBody = F, fieldPaths = NULL, searchConditionsGroupType = "And", searchSortFieldNamesList = NULL, searchSortFieldNamesDescendingList = NULL, entityId = 1, schoolYearId = NULL, page = 1, pageSize = 100000, flatten = T, returnResponse = F){

		params <- as.list(environment())

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		listSkyObjects(module = "AccountsReceivable", objectName = "ConfigDistrict", searchFields = searchFields %>% append(fieldPaths), page = page, pageSize = pageSize, SearchConditionsList = searchConditionsList, SearchConditionsGroupType = searchConditionsGroupType, SearchSortFieldNamesList = searchSortFieldNamesList, SearchSortFieldNamesDescendingList = searchSortFieldNamesDescendingList, entityId = entityId, schoolYearId = schoolYearId, flatten = flatten, returnResponse = returnResponse)
	}

	#' Get an AccountsReceivableConfigDistrict
	#'
	#' This function returns a dataframe or json object of an AccountsReceivableConfigDistrict
	#' @param AccountsReceivableConfigDistrictID The ID of the AccountsReceivableConfigDistrict to return.
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given AccountsReceivableConfigDistrict. Defaults to FALSE for all return fields which, for convenience, returns all fields for the AccountsReceivableConfigDistrict.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object listed as text. Run \code{\link{getSchemaForObjects}}('AccountsReceivableConfigDistrict') to get more field paths.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{listSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Accounts Receivable
	#' @return A dataframe or of AccountsReceivableConfigDistrict
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	getAccountsReceivableConfigDistrict <- function(AccountsReceivableConfigDistrictID, ConfigDistrictID = F, DistrictID = F, InvoiceGroupLength = F, InvoiceNumberLength = F, BatchDefault = F, AccountIDUnapplied = F, NameIDBusinessOffice = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, ReportIDInvoiceDelivery = F, ReportIDStatementDelivery = F, EmailSendingAddressInvoice = F, EmailSendingAliasInvoice = F, EmailSendingAddressStatement = F, EmailSendingAliasStatement = F, InvoiceEmailSubject = F, InvoiceEmailBody = F, StatementEmailSubject = F, StatementEmailBody = F, fieldPaths = NULL, entityId = 1, schoolYearId = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment()) %>% purrr::keep(names(.) != "AccountsReceivableConfigDistrictID")

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		getSkyObject(module = "AccountsReceivable", objectName = "ConfigDistrict", objectId = AccountsReceivableConfigDistrictID, searchFields = searchFields, entityId = entityId, flatten = flatten, returnResponse = returnResponse)
	}

	#' Delete an AccountsReceivableConfigDistrict
	#'
	#' This function deletes an AccountsReceivableConfigDistrict
	#' @param AccountsReceivableConfigDistrictID The ID of the AccountsReceivableConfigDistrict to delete
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{listSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Accounts Receivable
	#' @return The AccountsReceivableConfigDistrictID of the deleted AccountsReceivableConfigDistrict.
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	deleteAccountsReceivableConfigDistrict <- function(AccountsReceivableConfigDistrictID, ignoreWarnings = F, entityId = 1, schoolYearId = NULL, flatten = T, returnResponse = F){

		deleteSkyObject(module = "AccountsReceivable", objectName = "ConfigDistrict", objectId = AccountsReceivableConfigDistrictID, ignoreWarnings = ignoreWarnings, entityId = entityId, flatten = flatten, returnResponse = returnResponse)
	}

	#' Create an AccountsReceivableConfigDistrict
	#'
	#' This function creates an AccountsReceivableConfigDistrict
	#' @param fieldNames The field values to give the created AccountsReceivableConfigDistrict. Each defaults to NULL.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{listSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Accounts Receivable
	#' @return A newly created AccountsReceivableConfigDistrict
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	createAccountsReceivableConfigDistrict <- function(DistrictID = NULL, InvoiceGroupLength = NULL, InvoiceNumberLength = NULL, BatchDefault = NULL, AccountIDUnapplied = NULL, NameIDBusinessOffice = NULL, ReportIDInvoiceDelivery = NULL, ReportIDStatementDelivery = NULL, EmailSendingAddressInvoice = NULL, EmailSendingAliasInvoice = NULL, EmailSendingAddressStatement = NULL, EmailSendingAliasStatement = NULL, InvoiceEmailSubject = NULL, InvoiceEmailBody = NULL, StatementEmailSubject = NULL, StatementEmailBody = NULL, entityId = 1, schoolYearId = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment())

		body <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper()) %>% compact()

		createSkyObject(module = "AccountsReceivable", objectName = "ConfigDistrict", body = list(DataObject = body), searchFields = append("ConfigDistrictID", body %>% names()), entityId = entityId, schoolYearId = schoolYearId, flatten = flatten, returnResponse = returnResponse)
	}

	#' Modify an AccountsReceivableConfigDistrict
	#'
	#' This function modifies an AccountsReceivableConfigDistrict
	#' @param fieldNames The field values to give the modified AccountsReceivableConfigDistrict. Each defaults to NULL.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{listSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Accounts Receivable
	#' @return The modified AccountsReceivableConfigDistrict
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	modifyAccountsReceivableConfigDistrict <- function(ConfigDistrictID, DistrictID = NULL, InvoiceGroupLength = NULL, InvoiceNumberLength = NULL, BatchDefault = NULL, AccountIDUnapplied = NULL, NameIDBusinessOffice = NULL, ReportIDInvoiceDelivery = NULL, ReportIDStatementDelivery = NULL, EmailSendingAddressInvoice = NULL, EmailSendingAliasInvoice = NULL, EmailSendingAddressStatement = NULL, EmailSendingAliasStatement = NULL, InvoiceEmailSubject = NULL, InvoiceEmailBody = NULL, StatementEmailSubject = NULL, StatementEmailBody = NULL, entityId = 1, schoolYearId = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment())

		body <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper()) %>% compact()

		modifySkyObject(module = "AccountsReceivable", objectName = "ConfigDistrict", objectId = ConfigDistrictID, body = list(DataObject = body), searchFields = append("ConfigDistrictID", body %>% names()), entityId = entityId, schoolYearId = schoolYearId, flatten = flatten, returnResponse = returnResponse)
	}

	#' List PayorDistricts
	#'
	#' This function returns a dataframe or json object of PayorDistricts
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given PayorDistricts. Defaults to FALSE for all return fields which, for convenience, returns all fields for the PayorDistricts.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object listed as text. Run \code{\link{getSchemaForObjects}}('PayorDistrict') to get more field paths.
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
	#' @concept Accounts Receivable
	#' @return A list of PayorDistricts
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	listPayorDistricts <- function(searchConditionsList = NULL, PayorDistrictID = F, PayorID = F, DistrictID = F, AccumulatedBalance = F, AmountDue = F, AmountPaid = F, UnappliedAmount = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, fieldPaths = NULL, searchConditionsGroupType = "And", searchSortFieldNamesList = NULL, searchSortFieldNamesDescendingList = NULL, entityId = 1, schoolYearId = NULL, page = 1, pageSize = 100000, flatten = T, returnResponse = F){

		params <- as.list(environment())

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		listSkyObjects(module = "AccountsReceivable", objectName = "PayorDistrict", searchFields = searchFields %>% append(fieldPaths), page = page, pageSize = pageSize, SearchConditionsList = searchConditionsList, SearchConditionsGroupType = searchConditionsGroupType, SearchSortFieldNamesList = searchSortFieldNamesList, SearchSortFieldNamesDescendingList = searchSortFieldNamesDescendingList, entityId = entityId, schoolYearId = schoolYearId, flatten = flatten, returnResponse = returnResponse)
	}

	#' Get a PayorDistrict
	#'
	#' This function returns a dataframe or json object of a PayorDistrict
	#' @param PayorDistrictID The ID of the PayorDistrict to return.
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given PayorDistrict. Defaults to FALSE for all return fields which, for convenience, returns all fields for the PayorDistrict.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object listed as text. Run \code{\link{getSchemaForObjects}}('PayorDistrict') to get more field paths.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{listSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Accounts Receivable
	#' @return A dataframe or of PayorDistrict
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	getPayorDistrict <- function(PayorDistrictID, PayorID = F, DistrictID = F, AccumulatedBalance = F, AmountDue = F, AmountPaid = F, UnappliedAmount = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, fieldPaths = NULL, entityId = 1, schoolYearId = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment()) %>% purrr::keep(names(.) != "PayorDistrictID")

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		getSkyObject(module = "AccountsReceivable", objectName = "PayorDistrict", objectId = PayorDistrictID, searchFields = searchFields, entityId = entityId, flatten = flatten, returnResponse = returnResponse)
	}

	#' Delete a PayorDistrict
	#'
	#' This function deletes a PayorDistrict
	#' @param PayorDistrictID The ID of the PayorDistrict to delete
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{listSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Accounts Receivable
	#' @return The PayorDistrictID of the deleted PayorDistrict.
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	deletePayorDistrict <- function(PayorDistrictID, ignoreWarnings = F, entityId = 1, schoolYearId = NULL, flatten = T, returnResponse = F){

		deleteSkyObject(module = "AccountsReceivable", objectName = "PayorDistrict", objectId = PayorDistrictID, ignoreWarnings = ignoreWarnings, entityId = entityId, flatten = flatten, returnResponse = returnResponse)
	}

	#' Create a PayorDistrict
	#'
	#' This function creates a PayorDistrict
	#' @param fieldNames The field values to give the created PayorDistrict. Each defaults to NULL.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{listSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Accounts Receivable
	#' @return A newly created PayorDistrict
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	createPayorDistrict <- function(PayorID = NULL, DistrictID = NULL, entityId = 1, schoolYearId = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment())

		body <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper()) %>% compact()

		createSkyObject(module = "AccountsReceivable", objectName = "PayorDistrict", body = list(DataObject = body), searchFields = append("PayorDistrictID", body %>% names()), entityId = entityId, schoolYearId = schoolYearId, flatten = flatten, returnResponse = returnResponse)
	}

	#' Modify a PayorDistrict
	#'
	#' This function modifies a PayorDistrict
	#' @param fieldNames The field values to give the modified PayorDistrict. Each defaults to NULL.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{listSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Accounts Receivable
	#' @return The modified PayorDistrict
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	modifyPayorDistrict <- function(PayorDistrictID, PayorID = NULL, DistrictID = NULL, entityId = 1, schoolYearId = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment())

		body <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper()) %>% compact()

		modifySkyObject(module = "AccountsReceivable", objectName = "PayorDistrict", objectId = PayorDistrictID, body = list(DataObject = body), searchFields = append("PayorDistrictID", body %>% names()), entityId = entityId, schoolYearId = schoolYearId, flatten = flatten, returnResponse = returnResponse)
	}

	#' List AccountsReceivablePayors
	#'
	#' This function returns a dataframe or json object of AccountsReceivablePayors
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given AccountsReceivablePayors. Defaults to FALSE for all return fields which, for convenience, returns all fields for the AccountsReceivablePayors.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object listed as text. Run \code{\link{getSchemaForObjects}}('AccountsReceivablePayor') to get more field paths.
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
	#' @concept Accounts Receivable
	#' @return A list of AccountsReceivablePayors
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	listAccountsReceivablePayors <- function(searchConditionsList = NULL, PayorID = F, NameID = F, AccountsReceivableContact = F, QuickKey = F, AttachmentCount = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, FullNameLFM = F, FullNameFL = F, FullNameFML = F, DefaultDeliveryType = F, fieldPaths = NULL, searchConditionsGroupType = "And", searchSortFieldNamesList = NULL, searchSortFieldNamesDescendingList = NULL, entityId = 1, schoolYearId = NULL, page = 1, pageSize = 100000, flatten = T, returnResponse = F){

		params <- as.list(environment())

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		listSkyObjects(module = "AccountsReceivable", objectName = "Payor", searchFields = searchFields %>% append(fieldPaths), page = page, pageSize = pageSize, SearchConditionsList = searchConditionsList, SearchConditionsGroupType = searchConditionsGroupType, SearchSortFieldNamesList = searchSortFieldNamesList, SearchSortFieldNamesDescendingList = searchSortFieldNamesDescendingList, entityId = entityId, schoolYearId = schoolYearId, flatten = flatten, returnResponse = returnResponse)
	}

	#' Get an AccountsReceivablePayor
	#'
	#' This function returns a dataframe or json object of an AccountsReceivablePayor
	#' @param AccountsReceivablePayorID The ID of the AccountsReceivablePayor to return.
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given AccountsReceivablePayor. Defaults to FALSE for all return fields which, for convenience, returns all fields for the AccountsReceivablePayor.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object listed as text. Run \code{\link{getSchemaForObjects}}('AccountsReceivablePayor') to get more field paths.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{listSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Accounts Receivable
	#' @return A dataframe or of AccountsReceivablePayor
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	getAccountsReceivablePayor <- function(AccountsReceivablePayorID, PayorID = F, NameID = F, AccountsReceivableContact = F, QuickKey = F, AttachmentCount = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, FullNameLFM = F, FullNameFL = F, FullNameFML = F, DefaultDeliveryType = F, fieldPaths = NULL, entityId = 1, schoolYearId = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment()) %>% purrr::keep(names(.) != "AccountsReceivablePayorID")

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		getSkyObject(module = "AccountsReceivable", objectName = "Payor", objectId = AccountsReceivablePayorID, searchFields = searchFields, entityId = entityId, flatten = flatten, returnResponse = returnResponse)
	}

	#' Delete an AccountsReceivablePayor
	#'
	#' This function deletes an AccountsReceivablePayor
	#' @param AccountsReceivablePayorID The ID of the AccountsReceivablePayor to delete
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{listSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Accounts Receivable
	#' @return The AccountsReceivablePayorID of the deleted AccountsReceivablePayor.
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	deleteAccountsReceivablePayor <- function(AccountsReceivablePayorID, ignoreWarnings = F, entityId = 1, schoolYearId = NULL, flatten = T, returnResponse = F){

		deleteSkyObject(module = "AccountsReceivable", objectName = "Payor", objectId = AccountsReceivablePayorID, ignoreWarnings = ignoreWarnings, entityId = entityId, flatten = flatten, returnResponse = returnResponse)
	}

	#' Create an AccountsReceivablePayor
	#'
	#' This function creates an AccountsReceivablePayor
	#' @param fieldNames The field values to give the created AccountsReceivablePayor. Each defaults to NULL.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{listSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Accounts Receivable
	#' @return A newly created AccountsReceivablePayor
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	createAccountsReceivablePayor <- function(NameID = NULL, AccountsReceivableContact = NULL, QuickKey = NULL, DefaultDeliveryType = NULL, entityId = 1, schoolYearId = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment())

		body <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper()) %>% compact()

		createSkyObject(module = "AccountsReceivable", objectName = "Payor", body = list(DataObject = body), searchFields = append("PayorID", body %>% names()), entityId = entityId, schoolYearId = schoolYearId, flatten = flatten, returnResponse = returnResponse)
	}

	#' Modify an AccountsReceivablePayor
	#'
	#' This function modifies an AccountsReceivablePayor
	#' @param fieldNames The field values to give the modified AccountsReceivablePayor. Each defaults to NULL.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{listSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Accounts Receivable
	#' @return The modified AccountsReceivablePayor
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	modifyAccountsReceivablePayor <- function(PayorID, NameID = NULL, AccountsReceivableContact = NULL, QuickKey = NULL, DefaultDeliveryType = NULL, entityId = 1, schoolYearId = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment())

		body <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper()) %>% compact()

		modifySkyObject(module = "AccountsReceivable", objectName = "Payor", objectId = PayorID, body = list(DataObject = body), searchFields = append("PayorID", body %>% names()), entityId = entityId, schoolYearId = schoolYearId, flatten = flatten, returnResponse = returnResponse)
	}

	#' List DiscountDetails
	#'
	#' This function returns a dataframe or json object of DiscountDetails
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given DiscountDetails. Defaults to FALSE for all return fields which, for convenience, returns all fields for the DiscountDetails.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object listed as text. Run \code{\link{getSchemaForObjects}}('DiscountDetail') to get more field paths.
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
	#' @concept Accounts Receivable
	#' @return A list of DiscountDetails
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	listDiscountDetails <- function(searchConditionsList = NULL, DiscountDetailID = F, DiscountID = F, DaysBeforeDueDate = F, Percent = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, fieldPaths = NULL, searchConditionsGroupType = "And", searchSortFieldNamesList = NULL, searchSortFieldNamesDescendingList = NULL, entityId = 1, schoolYearId = NULL, page = 1, pageSize = 100000, flatten = T, returnResponse = F){

		params <- as.list(environment())

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		listSkyObjects(module = "AccountsReceivable", objectName = "DiscountDetail", searchFields = searchFields %>% append(fieldPaths), page = page, pageSize = pageSize, SearchConditionsList = searchConditionsList, SearchConditionsGroupType = searchConditionsGroupType, SearchSortFieldNamesList = searchSortFieldNamesList, SearchSortFieldNamesDescendingList = searchSortFieldNamesDescendingList, entityId = entityId, schoolYearId = schoolYearId, flatten = flatten, returnResponse = returnResponse)
	}

	#' Get a DiscountDetail
	#'
	#' This function returns a dataframe or json object of a DiscountDetail
	#' @param DiscountDetailID The ID of the DiscountDetail to return.
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given DiscountDetail. Defaults to FALSE for all return fields which, for convenience, returns all fields for the DiscountDetail.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object listed as text. Run \code{\link{getSchemaForObjects}}('DiscountDetail') to get more field paths.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{listSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Accounts Receivable
	#' @return A dataframe or of DiscountDetail
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	getDiscountDetail <- function(DiscountDetailID, DiscountID = F, DaysBeforeDueDate = F, Percent = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, fieldPaths = NULL, entityId = 1, schoolYearId = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment()) %>% purrr::keep(names(.) != "DiscountDetailID")

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		getSkyObject(module = "AccountsReceivable", objectName = "DiscountDetail", objectId = DiscountDetailID, searchFields = searchFields, entityId = entityId, flatten = flatten, returnResponse = returnResponse)
	}

	#' Delete a DiscountDetail
	#'
	#' This function deletes a DiscountDetail
	#' @param DiscountDetailID The ID of the DiscountDetail to delete
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{listSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Accounts Receivable
	#' @return The DiscountDetailID of the deleted DiscountDetail.
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	deleteDiscountDetail <- function(DiscountDetailID, ignoreWarnings = F, entityId = 1, schoolYearId = NULL, flatten = T, returnResponse = F){

		deleteSkyObject(module = "AccountsReceivable", objectName = "DiscountDetail", objectId = DiscountDetailID, ignoreWarnings = ignoreWarnings, entityId = entityId, flatten = flatten, returnResponse = returnResponse)
	}

	#' Create a DiscountDetail
	#'
	#' This function creates a DiscountDetail
	#' @param fieldNames The field values to give the created DiscountDetail. Each defaults to NULL.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{listSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Accounts Receivable
	#' @return A newly created DiscountDetail
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	createDiscountDetail <- function(DiscountID = NULL, DaysBeforeDueDate = NULL, Percent = NULL, entityId = 1, schoolYearId = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment())

		body <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper()) %>% compact()

		createSkyObject(module = "AccountsReceivable", objectName = "DiscountDetail", body = list(DataObject = body), searchFields = append("DiscountDetailID", body %>% names()), entityId = entityId, schoolYearId = schoolYearId, flatten = flatten, returnResponse = returnResponse)
	}

	#' Modify a DiscountDetail
	#'
	#' This function modifies a DiscountDetail
	#' @param fieldNames The field values to give the modified DiscountDetail. Each defaults to NULL.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{listSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Accounts Receivable
	#' @return The modified DiscountDetail
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	modifyDiscountDetail <- function(DiscountDetailID, DiscountID = NULL, DaysBeforeDueDate = NULL, Percent = NULL, entityId = 1, schoolYearId = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment())

		body <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper()) %>% compact()

		modifySkyObject(module = "AccountsReceivable", objectName = "DiscountDetail", objectId = DiscountDetailID, body = list(DataObject = body), searchFields = append("DiscountDetailID", body %>% names()), entityId = entityId, schoolYearId = schoolYearId, flatten = flatten, returnResponse = returnResponse)
	}

	#' List AccountsReceivableDiscounts
	#'
	#' This function returns a dataframe or json object of AccountsReceivableDiscounts
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given AccountsReceivableDiscounts. Defaults to FALSE for all return fields which, for convenience, returns all fields for the AccountsReceivableDiscounts.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object listed as text. Run \code{\link{getSchemaForObjects}}('AccountsReceivableDiscount') to get more field paths.
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
	#' @concept Accounts Receivable
	#' @return A list of AccountsReceivableDiscounts
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	listAccountsReceivableDiscounts <- function(searchConditionsList = NULL, DiscountID = F, DistrictID = F, Code = F, Description = F, MaskIDDiscount = F, CodeDescription = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, fieldPaths = NULL, searchConditionsGroupType = "And", searchSortFieldNamesList = NULL, searchSortFieldNamesDescendingList = NULL, entityId = 1, schoolYearId = NULL, page = 1, pageSize = 100000, flatten = T, returnResponse = F){

		params <- as.list(environment())

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		listSkyObjects(module = "AccountsReceivable", objectName = "Discount", searchFields = searchFields %>% append(fieldPaths), page = page, pageSize = pageSize, SearchConditionsList = searchConditionsList, SearchConditionsGroupType = searchConditionsGroupType, SearchSortFieldNamesList = searchSortFieldNamesList, SearchSortFieldNamesDescendingList = searchSortFieldNamesDescendingList, entityId = entityId, schoolYearId = schoolYearId, flatten = flatten, returnResponse = returnResponse)
	}

	#' Get an AccountsReceivableDiscount
	#'
	#' This function returns a dataframe or json object of an AccountsReceivableDiscount
	#' @param AccountsReceivableDiscountID The ID of the AccountsReceivableDiscount to return.
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given AccountsReceivableDiscount. Defaults to FALSE for all return fields which, for convenience, returns all fields for the AccountsReceivableDiscount.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object listed as text. Run \code{\link{getSchemaForObjects}}('AccountsReceivableDiscount') to get more field paths.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{listSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Accounts Receivable
	#' @return A dataframe or of AccountsReceivableDiscount
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	getAccountsReceivableDiscount <- function(AccountsReceivableDiscountID, DiscountID = F, DistrictID = F, Code = F, Description = F, MaskIDDiscount = F, CodeDescription = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, fieldPaths = NULL, entityId = 1, schoolYearId = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment()) %>% purrr::keep(names(.) != "AccountsReceivableDiscountID")

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		getSkyObject(module = "AccountsReceivable", objectName = "Discount", objectId = AccountsReceivableDiscountID, searchFields = searchFields, entityId = entityId, flatten = flatten, returnResponse = returnResponse)
	}

	#' Delete an AccountsReceivableDiscount
	#'
	#' This function deletes an AccountsReceivableDiscount
	#' @param AccountsReceivableDiscountID The ID of the AccountsReceivableDiscount to delete
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{listSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Accounts Receivable
	#' @return The AccountsReceivableDiscountID of the deleted AccountsReceivableDiscount.
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	deleteAccountsReceivableDiscount <- function(AccountsReceivableDiscountID, ignoreWarnings = F, entityId = 1, schoolYearId = NULL, flatten = T, returnResponse = F){

		deleteSkyObject(module = "AccountsReceivable", objectName = "Discount", objectId = AccountsReceivableDiscountID, ignoreWarnings = ignoreWarnings, entityId = entityId, flatten = flatten, returnResponse = returnResponse)
	}

	#' Create an AccountsReceivableDiscount
	#'
	#' This function creates an AccountsReceivableDiscount
	#' @param fieldNames The field values to give the created AccountsReceivableDiscount. Each defaults to NULL.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{listSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Accounts Receivable
	#' @return A newly created AccountsReceivableDiscount
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	createAccountsReceivableDiscount <- function(DistrictID = NULL, Code = NULL, Description = NULL, MaskIDDiscount = NULL, entityId = 1, schoolYearId = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment())

		body <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper()) %>% compact()

		createSkyObject(module = "AccountsReceivable", objectName = "Discount", body = list(DataObject = body), searchFields = append("DiscountID", body %>% names()), entityId = entityId, schoolYearId = schoolYearId, flatten = flatten, returnResponse = returnResponse)
	}

	#' Modify an AccountsReceivableDiscount
	#'
	#' This function modifies an AccountsReceivableDiscount
	#' @param fieldNames The field values to give the modified AccountsReceivableDiscount. Each defaults to NULL.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{listSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Accounts Receivable
	#' @return The modified AccountsReceivableDiscount
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	modifyAccountsReceivableDiscount <- function(DiscountID, DistrictID = NULL, Code = NULL, Description = NULL, MaskIDDiscount = NULL, entityId = 1, schoolYearId = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment())

		body <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper()) %>% compact()

		modifySkyObject(module = "AccountsReceivable", objectName = "Discount", objectId = DiscountID, body = list(DataObject = body), searchFields = append("DiscountID", body %>% names()), entityId = entityId, schoolYearId = schoolYearId, flatten = flatten, returnResponse = returnResponse)
	}

	#' List AccountsReceivableInvoices
	#'
	#' This function returns a dataframe or json object of AccountsReceivableInvoices
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given AccountsReceivableInvoices. Defaults to FALSE for all return fields which, for convenience, returns all fields for the AccountsReceivableInvoices.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object listed as text. Run \code{\link{getSchemaForObjects}}('AccountsReceivableInvoice') to get more field paths.
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
	#' @concept Accounts Receivable
	#' @return A list of AccountsReceivableInvoices
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	listAccountsReceivableInvoices <- function(searchConditionsList = NULL, InvoiceID = F, FiscalYearID = F, PayorID = F, InvoiceGroupID = F, BankAccountID = F, AccountingUpdateIDBatch = F, Description = F, InvoiceNumber = F, Batch = F, InvoiceDate = F, DueDate = F, Status = F, Amount = F, CanClone = F, CanChangeAccount = F, CanMoveToOpen = F, CanMoveToHistory = F, BaseCurrencyAmountByAccount = F, AmountDue = F, AmountPaid = F, FormattedDescription = F, AgingDays = F, AttachmentCount = F, CurrentUserHasInvoiceGroupAccess = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, CanDelete = F, InvoiceThirdPartyImportID = F, DeliveryStatus = F, DeliveryCount = F, LastDeliveryTime = F, IsPrinted = F, IsEmailed = F, fieldPaths = NULL, searchConditionsGroupType = "And", searchSortFieldNamesList = NULL, searchSortFieldNamesDescendingList = NULL, entityId = 1, schoolYearId = NULL, page = 1, pageSize = 100000, flatten = T, returnResponse = F){

		params <- as.list(environment())

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		listSkyObjects(module = "AccountsReceivable", objectName = "Invoice", searchFields = searchFields %>% append(fieldPaths), page = page, pageSize = pageSize, SearchConditionsList = searchConditionsList, SearchConditionsGroupType = searchConditionsGroupType, SearchSortFieldNamesList = searchSortFieldNamesList, SearchSortFieldNamesDescendingList = searchSortFieldNamesDescendingList, entityId = entityId, schoolYearId = schoolYearId, flatten = flatten, returnResponse = returnResponse)
	}

	#' Get an AccountsReceivableInvoice
	#'
	#' This function returns a dataframe or json object of an AccountsReceivableInvoice
	#' @param AccountsReceivableInvoiceID The ID of the AccountsReceivableInvoice to return.
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given AccountsReceivableInvoice. Defaults to FALSE for all return fields which, for convenience, returns all fields for the AccountsReceivableInvoice.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object listed as text. Run \code{\link{getSchemaForObjects}}('AccountsReceivableInvoice') to get more field paths.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{listSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Accounts Receivable
	#' @return A dataframe or of AccountsReceivableInvoice
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	getAccountsReceivableInvoice <- function(AccountsReceivableInvoiceID, InvoiceID = F, FiscalYearID = F, PayorID = F, InvoiceGroupID = F, BankAccountID = F, AccountingUpdateIDBatch = F, Description = F, InvoiceNumber = F, Batch = F, InvoiceDate = F, DueDate = F, Status = F, Amount = F, CanClone = F, CanChangeAccount = F, CanMoveToOpen = F, CanMoveToHistory = F, BaseCurrencyAmountByAccount = F, AmountDue = F, AmountPaid = F, FormattedDescription = F, AgingDays = F, AttachmentCount = F, CurrentUserHasInvoiceGroupAccess = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, CanDelete = F, InvoiceThirdPartyImportID = F, DeliveryStatus = F, DeliveryCount = F, LastDeliveryTime = F, IsPrinted = F, IsEmailed = F, fieldPaths = NULL, entityId = 1, schoolYearId = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment()) %>% purrr::keep(names(.) != "AccountsReceivableInvoiceID")

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		getSkyObject(module = "AccountsReceivable", objectName = "Invoice", objectId = AccountsReceivableInvoiceID, searchFields = searchFields, entityId = entityId, flatten = flatten, returnResponse = returnResponse)
	}

	#' Delete an AccountsReceivableInvoice
	#'
	#' This function deletes an AccountsReceivableInvoice
	#' @param AccountsReceivableInvoiceID The ID of the AccountsReceivableInvoice to delete
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{listSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Accounts Receivable
	#' @return The AccountsReceivableInvoiceID of the deleted AccountsReceivableInvoice.
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	deleteAccountsReceivableInvoice <- function(AccountsReceivableInvoiceID, ignoreWarnings = F, entityId = 1, schoolYearId = NULL, flatten = T, returnResponse = F){

		deleteSkyObject(module = "AccountsReceivable", objectName = "Invoice", objectId = AccountsReceivableInvoiceID, ignoreWarnings = ignoreWarnings, entityId = entityId, flatten = flatten, returnResponse = returnResponse)
	}

	#' Create an AccountsReceivableInvoice
	#'
	#' This function creates an AccountsReceivableInvoice
	#' @param fieldNames The field values to give the created AccountsReceivableInvoice. Each defaults to NULL.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{listSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Accounts Receivable
	#' @return A newly created AccountsReceivableInvoice
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	createAccountsReceivableInvoice <- function(FiscalYearID = NULL, PayorID = NULL, InvoiceGroupID = NULL, BankAccountID = NULL, AccountingUpdateIDBatch = NULL, Description = NULL, InvoiceNumber = NULL, Batch = NULL, InvoiceDate = NULL, DueDate = NULL, Status = NULL, InvoiceThirdPartyImportID = NULL, DeliveryStatus = NULL, entityId = 1, schoolYearId = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment())

		body <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper()) %>% compact()

		createSkyObject(module = "AccountsReceivable", objectName = "Invoice", body = list(DataObject = body), searchFields = append("InvoiceID", body %>% names()), entityId = entityId, schoolYearId = schoolYearId, flatten = flatten, returnResponse = returnResponse)
	}

	#' Modify an AccountsReceivableInvoice
	#'
	#' This function modifies an AccountsReceivableInvoice
	#' @param fieldNames The field values to give the modified AccountsReceivableInvoice. Each defaults to NULL.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{listSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Accounts Receivable
	#' @return The modified AccountsReceivableInvoice
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	modifyAccountsReceivableInvoice <- function(InvoiceID, FiscalYearID = NULL, PayorID = NULL, InvoiceGroupID = NULL, BankAccountID = NULL, AccountingUpdateIDBatch = NULL, Description = NULL, InvoiceNumber = NULL, Batch = NULL, InvoiceDate = NULL, DueDate = NULL, Status = NULL, InvoiceThirdPartyImportID = NULL, DeliveryStatus = NULL, entityId = 1, schoolYearId = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment())

		body <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper()) %>% compact()

		modifySkyObject(module = "AccountsReceivable", objectName = "Invoice", objectId = InvoiceID, body = list(DataObject = body), searchFields = append("InvoiceID", body %>% names()), entityId = entityId, schoolYearId = schoolYearId, flatten = flatten, returnResponse = returnResponse)
	}

	#' List AccountsReceivableInvoiceAccountings
	#'
	#' This function returns a dataframe or json object of AccountsReceivableInvoiceAccountings
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given AccountsReceivableInvoiceAccountings. Defaults to FALSE for all return fields which, for convenience, returns all fields for the AccountsReceivableInvoiceAccountings.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object listed as text. Run \code{\link{getSchemaForObjects}}('AccountsReceivableInvoiceAccounting') to get more field paths.
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
	#' @concept Accounts Receivable
	#' @return A list of AccountsReceivableInvoiceAccountings
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	listAccountsReceivableInvoiceAccountings <- function(searchConditionsList = NULL, InvoiceAccountingID = F, InvoiceDetailID = F, AccountID = F, BaseCurrencyAmount = F, PreviousGrossPaymentAmount = F, PreviousPaymentAmount = F, PreviousDiscountAmount = F, AmountDue = F, FormattedDescription = F, BaseCurrencyPercent = F, CurrentUserHasInvoiceGroupAccess = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, GrantID = F, ProjectID = F, fieldPaths = NULL, searchConditionsGroupType = "And", searchSortFieldNamesList = NULL, searchSortFieldNamesDescendingList = NULL, entityId = 1, schoolYearId = NULL, page = 1, pageSize = 100000, flatten = T, returnResponse = F){

		params <- as.list(environment())

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		listSkyObjects(module = "AccountsReceivable", objectName = "InvoiceAccounting", searchFields = searchFields %>% append(fieldPaths), page = page, pageSize = pageSize, SearchConditionsList = searchConditionsList, SearchConditionsGroupType = searchConditionsGroupType, SearchSortFieldNamesList = searchSortFieldNamesList, SearchSortFieldNamesDescendingList = searchSortFieldNamesDescendingList, entityId = entityId, schoolYearId = schoolYearId, flatten = flatten, returnResponse = returnResponse)
	}

	#' Get an AccountsReceivableInvoiceAccounting
	#'
	#' This function returns a dataframe or json object of an AccountsReceivableInvoiceAccounting
	#' @param AccountsReceivableInvoiceAccountingID The ID of the AccountsReceivableInvoiceAccounting to return.
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given AccountsReceivableInvoiceAccounting. Defaults to FALSE for all return fields which, for convenience, returns all fields for the AccountsReceivableInvoiceAccounting.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object listed as text. Run \code{\link{getSchemaForObjects}}('AccountsReceivableInvoiceAccounting') to get more field paths.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{listSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Accounts Receivable
	#' @return A dataframe or of AccountsReceivableInvoiceAccounting
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	getAccountsReceivableInvoiceAccounting <- function(AccountsReceivableInvoiceAccountingID, InvoiceAccountingID = F, InvoiceDetailID = F, AccountID = F, BaseCurrencyAmount = F, PreviousGrossPaymentAmount = F, PreviousPaymentAmount = F, PreviousDiscountAmount = F, AmountDue = F, FormattedDescription = F, BaseCurrencyPercent = F, CurrentUserHasInvoiceGroupAccess = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, GrantID = F, ProjectID = F, fieldPaths = NULL, entityId = 1, schoolYearId = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment()) %>% purrr::keep(names(.) != "AccountsReceivableInvoiceAccountingID")

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		getSkyObject(module = "AccountsReceivable", objectName = "InvoiceAccounting", objectId = AccountsReceivableInvoiceAccountingID, searchFields = searchFields, entityId = entityId, flatten = flatten, returnResponse = returnResponse)
	}

	#' Delete an AccountsReceivableInvoiceAccounting
	#'
	#' This function deletes an AccountsReceivableInvoiceAccounting
	#' @param AccountsReceivableInvoiceAccountingID The ID of the AccountsReceivableInvoiceAccounting to delete
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{listSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Accounts Receivable
	#' @return The AccountsReceivableInvoiceAccountingID of the deleted AccountsReceivableInvoiceAccounting.
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	deleteAccountsReceivableInvoiceAccounting <- function(AccountsReceivableInvoiceAccountingID, ignoreWarnings = F, entityId = 1, schoolYearId = NULL, flatten = T, returnResponse = F){

		deleteSkyObject(module = "AccountsReceivable", objectName = "InvoiceAccounting", objectId = AccountsReceivableInvoiceAccountingID, ignoreWarnings = ignoreWarnings, entityId = entityId, flatten = flatten, returnResponse = returnResponse)
	}

	#' Create an AccountsReceivableInvoiceAccounting
	#'
	#' This function creates an AccountsReceivableInvoiceAccounting
	#' @param fieldNames The field values to give the created AccountsReceivableInvoiceAccounting. Each defaults to NULL.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{listSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Accounts Receivable
	#' @return A newly created AccountsReceivableInvoiceAccounting
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	createAccountsReceivableInvoiceAccounting <- function(InvoiceDetailID = NULL, AccountID = NULL, BaseCurrencyAmount = NULL, GrantID = NULL, ProjectID = NULL, entityId = 1, schoolYearId = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment())

		body <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper()) %>% compact()

		createSkyObject(module = "AccountsReceivable", objectName = "InvoiceAccounting", body = list(DataObject = body), searchFields = append("InvoiceAccountingID", body %>% names()), entityId = entityId, schoolYearId = schoolYearId, flatten = flatten, returnResponse = returnResponse)
	}

	#' Modify an AccountsReceivableInvoiceAccounting
	#'
	#' This function modifies an AccountsReceivableInvoiceAccounting
	#' @param fieldNames The field values to give the modified AccountsReceivableInvoiceAccounting. Each defaults to NULL.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{listSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Accounts Receivable
	#' @return The modified AccountsReceivableInvoiceAccounting
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	modifyAccountsReceivableInvoiceAccounting <- function(InvoiceAccountingID, InvoiceDetailID = NULL, AccountID = NULL, BaseCurrencyAmount = NULL, GrantID = NULL, ProjectID = NULL, entityId = 1, schoolYearId = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment())

		body <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper()) %>% compact()

		modifySkyObject(module = "AccountsReceivable", objectName = "InvoiceAccounting", objectId = InvoiceAccountingID, body = list(DataObject = body), searchFields = append("InvoiceAccountingID", body %>% names()), entityId = entityId, schoolYearId = schoolYearId, flatten = flatten, returnResponse = returnResponse)
	}

	#' List AccountsReceivableInvoiceDetails
	#'
	#' This function returns a dataframe or json object of AccountsReceivableInvoiceDetails
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given AccountsReceivableInvoiceDetails. Defaults to FALSE for all return fields which, for convenience, returns all fields for the AccountsReceivableInvoiceDetails.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object listed as text. Run \code{\link{getSchemaForObjects}}('AccountsReceivableInvoiceDetail') to get more field paths.
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
	#' @concept Accounts Receivable
	#' @return A list of AccountsReceivableInvoiceDetails
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	listAccountsReceivableInvoiceDetails <- function(searchConditionsList = NULL, InvoiceDetailID = F, InvoiceID = F, ItemID = F, UnitOfMeasureID = F, DiscountID = F, InvoiceDetailIDDeletedHistory = F, Description = F, Quantity = F, UnitCost = F, BaseCurrencyAmount = F, DisplayOrder = F, InvoiceTotal = F, DetailType = F, AmountDue = F, CanDelete = F, ItemCodeDetailDescription = F, CurrentUserHasInvoiceGroupAccess = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, PurchaseOrderNumber = F, fieldPaths = NULL, searchConditionsGroupType = "And", searchSortFieldNamesList = NULL, searchSortFieldNamesDescendingList = NULL, entityId = 1, schoolYearId = NULL, page = 1, pageSize = 100000, flatten = T, returnResponse = F){

		params <- as.list(environment())

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		listSkyObjects(module = "AccountsReceivable", objectName = "InvoiceDetail", searchFields = searchFields %>% append(fieldPaths), page = page, pageSize = pageSize, SearchConditionsList = searchConditionsList, SearchConditionsGroupType = searchConditionsGroupType, SearchSortFieldNamesList = searchSortFieldNamesList, SearchSortFieldNamesDescendingList = searchSortFieldNamesDescendingList, entityId = entityId, schoolYearId = schoolYearId, flatten = flatten, returnResponse = returnResponse)
	}

	#' Get an AccountsReceivableInvoiceDetail
	#'
	#' This function returns a dataframe or json object of an AccountsReceivableInvoiceDetail
	#' @param AccountsReceivableInvoiceDetailID The ID of the AccountsReceivableInvoiceDetail to return.
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given AccountsReceivableInvoiceDetail. Defaults to FALSE for all return fields which, for convenience, returns all fields for the AccountsReceivableInvoiceDetail.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object listed as text. Run \code{\link{getSchemaForObjects}}('AccountsReceivableInvoiceDetail') to get more field paths.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{listSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Accounts Receivable
	#' @return A dataframe or of AccountsReceivableInvoiceDetail
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	getAccountsReceivableInvoiceDetail <- function(AccountsReceivableInvoiceDetailID, InvoiceDetailID = F, InvoiceID = F, ItemID = F, UnitOfMeasureID = F, DiscountID = F, InvoiceDetailIDDeletedHistory = F, Description = F, Quantity = F, UnitCost = F, BaseCurrencyAmount = F, DisplayOrder = F, InvoiceTotal = F, DetailType = F, AmountDue = F, CanDelete = F, ItemCodeDetailDescription = F, CurrentUserHasInvoiceGroupAccess = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, PurchaseOrderNumber = F, fieldPaths = NULL, entityId = 1, schoolYearId = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment()) %>% purrr::keep(names(.) != "AccountsReceivableInvoiceDetailID")

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		getSkyObject(module = "AccountsReceivable", objectName = "InvoiceDetail", objectId = AccountsReceivableInvoiceDetailID, searchFields = searchFields, entityId = entityId, flatten = flatten, returnResponse = returnResponse)
	}

	#' Delete an AccountsReceivableInvoiceDetail
	#'
	#' This function deletes an AccountsReceivableInvoiceDetail
	#' @param AccountsReceivableInvoiceDetailID The ID of the AccountsReceivableInvoiceDetail to delete
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{listSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Accounts Receivable
	#' @return The AccountsReceivableInvoiceDetailID of the deleted AccountsReceivableInvoiceDetail.
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	deleteAccountsReceivableInvoiceDetail <- function(AccountsReceivableInvoiceDetailID, ignoreWarnings = F, entityId = 1, schoolYearId = NULL, flatten = T, returnResponse = F){

		deleteSkyObject(module = "AccountsReceivable", objectName = "InvoiceDetail", objectId = AccountsReceivableInvoiceDetailID, ignoreWarnings = ignoreWarnings, entityId = entityId, flatten = flatten, returnResponse = returnResponse)
	}

	#' Create an AccountsReceivableInvoiceDetail
	#'
	#' This function creates an AccountsReceivableInvoiceDetail
	#' @param fieldNames The field values to give the created AccountsReceivableInvoiceDetail. Each defaults to NULL.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{listSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Accounts Receivable
	#' @return A newly created AccountsReceivableInvoiceDetail
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	createAccountsReceivableInvoiceDetail <- function(InvoiceID = NULL, ItemID = NULL, UnitOfMeasureID = NULL, DiscountID = NULL, InvoiceDetailIDDeletedHistory = NULL, Description = NULL, Quantity = NULL, UnitCost = NULL, BaseCurrencyAmount = NULL, DisplayOrder = NULL, DetailType = NULL, PurchaseOrderNumber = NULL, entityId = 1, schoolYearId = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment())

		body <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper()) %>% compact()

		createSkyObject(module = "AccountsReceivable", objectName = "InvoiceDetail", body = list(DataObject = body), searchFields = append("InvoiceDetailID", body %>% names()), entityId = entityId, schoolYearId = schoolYearId, flatten = flatten, returnResponse = returnResponse)
	}

	#' Modify an AccountsReceivableInvoiceDetail
	#'
	#' This function modifies an AccountsReceivableInvoiceDetail
	#' @param fieldNames The field values to give the modified AccountsReceivableInvoiceDetail. Each defaults to NULL.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{listSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Accounts Receivable
	#' @return The modified AccountsReceivableInvoiceDetail
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	modifyAccountsReceivableInvoiceDetail <- function(InvoiceDetailID, InvoiceID = NULL, ItemID = NULL, UnitOfMeasureID = NULL, DiscountID = NULL, InvoiceDetailIDDeletedHistory = NULL, Description = NULL, Quantity = NULL, UnitCost = NULL, BaseCurrencyAmount = NULL, DisplayOrder = NULL, DetailType = NULL, PurchaseOrderNumber = NULL, entityId = 1, schoolYearId = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment())

		body <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper()) %>% compact()

		modifySkyObject(module = "AccountsReceivable", objectName = "InvoiceDetail", objectId = InvoiceDetailID, body = list(DataObject = body), searchFields = append("InvoiceDetailID", body %>% names()), entityId = entityId, schoolYearId = schoolYearId, flatten = flatten, returnResponse = returnResponse)
	}

	#' List AccountsReceivableInvoiceGroups
	#'
	#' This function returns a dataframe or json object of AccountsReceivableInvoiceGroups
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given AccountsReceivableInvoiceGroups. Defaults to FALSE for all return fields which, for convenience, returns all fields for the AccountsReceivableInvoiceGroups.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object listed as text. Run \code{\link{getSchemaForObjects}}('AccountsReceivableInvoiceGroup') to get more field paths.
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
	#' @concept Accounts Receivable
	#' @return A list of AccountsReceivableInvoiceGroups
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	listAccountsReceivableInvoiceGroups <- function(searchConditionsList = NULL, InvoiceGroupID = F, GroupIDAccount = F, DistrictID = F, Code = F, Description = F, IsActive = F, CodeDescription = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, fieldPaths = NULL, searchConditionsGroupType = "And", searchSortFieldNamesList = NULL, searchSortFieldNamesDescendingList = NULL, entityId = 1, schoolYearId = NULL, page = 1, pageSize = 100000, flatten = T, returnResponse = F){

		params <- as.list(environment())

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		listSkyObjects(module = "AccountsReceivable", objectName = "InvoiceGroup", searchFields = searchFields %>% append(fieldPaths), page = page, pageSize = pageSize, SearchConditionsList = searchConditionsList, SearchConditionsGroupType = searchConditionsGroupType, SearchSortFieldNamesList = searchSortFieldNamesList, SearchSortFieldNamesDescendingList = searchSortFieldNamesDescendingList, entityId = entityId, schoolYearId = schoolYearId, flatten = flatten, returnResponse = returnResponse)
	}

	#' Get an AccountsReceivableInvoiceGroup
	#'
	#' This function returns a dataframe or json object of an AccountsReceivableInvoiceGroup
	#' @param AccountsReceivableInvoiceGroupID The ID of the AccountsReceivableInvoiceGroup to return.
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given AccountsReceivableInvoiceGroup. Defaults to FALSE for all return fields which, for convenience, returns all fields for the AccountsReceivableInvoiceGroup.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object listed as text. Run \code{\link{getSchemaForObjects}}('AccountsReceivableInvoiceGroup') to get more field paths.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{listSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Accounts Receivable
	#' @return A dataframe or of AccountsReceivableInvoiceGroup
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	getAccountsReceivableInvoiceGroup <- function(AccountsReceivableInvoiceGroupID, InvoiceGroupID = F, GroupIDAccount = F, DistrictID = F, Code = F, Description = F, IsActive = F, CodeDescription = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, fieldPaths = NULL, entityId = 1, schoolYearId = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment()) %>% purrr::keep(names(.) != "AccountsReceivableInvoiceGroupID")

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		getSkyObject(module = "AccountsReceivable", objectName = "InvoiceGroup", objectId = AccountsReceivableInvoiceGroupID, searchFields = searchFields, entityId = entityId, flatten = flatten, returnResponse = returnResponse)
	}

	#' Delete an AccountsReceivableInvoiceGroup
	#'
	#' This function deletes an AccountsReceivableInvoiceGroup
	#' @param AccountsReceivableInvoiceGroupID The ID of the AccountsReceivableInvoiceGroup to delete
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{listSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Accounts Receivable
	#' @return The AccountsReceivableInvoiceGroupID of the deleted AccountsReceivableInvoiceGroup.
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	deleteAccountsReceivableInvoiceGroup <- function(AccountsReceivableInvoiceGroupID, ignoreWarnings = F, entityId = 1, schoolYearId = NULL, flatten = T, returnResponse = F){

		deleteSkyObject(module = "AccountsReceivable", objectName = "InvoiceGroup", objectId = AccountsReceivableInvoiceGroupID, ignoreWarnings = ignoreWarnings, entityId = entityId, flatten = flatten, returnResponse = returnResponse)
	}

	#' Create an AccountsReceivableInvoiceGroup
	#'
	#' This function creates an AccountsReceivableInvoiceGroup
	#' @param fieldNames The field values to give the created AccountsReceivableInvoiceGroup. Each defaults to NULL.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{listSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Accounts Receivable
	#' @return A newly created AccountsReceivableInvoiceGroup
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	createAccountsReceivableInvoiceGroup <- function(GroupIDAccount = NULL, DistrictID = NULL, Code = NULL, Description = NULL, IsActive = NULL, entityId = 1, schoolYearId = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment())

		body <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper()) %>% compact()

		createSkyObject(module = "AccountsReceivable", objectName = "InvoiceGroup", body = list(DataObject = body), searchFields = append("InvoiceGroupID", body %>% names()), entityId = entityId, schoolYearId = schoolYearId, flatten = flatten, returnResponse = returnResponse)
	}

	#' Modify an AccountsReceivableInvoiceGroup
	#'
	#' This function modifies an AccountsReceivableInvoiceGroup
	#' @param fieldNames The field values to give the modified AccountsReceivableInvoiceGroup. Each defaults to NULL.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{listSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Accounts Receivable
	#' @return The modified AccountsReceivableInvoiceGroup
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	modifyAccountsReceivableInvoiceGroup <- function(InvoiceGroupID, GroupIDAccount = NULL, DistrictID = NULL, Code = NULL, Description = NULL, IsActive = NULL, entityId = 1, schoolYearId = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment())

		body <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper()) %>% compact()

		modifySkyObject(module = "AccountsReceivable", objectName = "InvoiceGroup", objectId = InvoiceGroupID, body = list(DataObject = body), searchFields = append("InvoiceGroupID", body %>% names()), entityId = entityId, schoolYearId = schoolYearId, flatten = flatten, returnResponse = returnResponse)
	}

	#' List AccountsReceivableInvoiceGroupClearances
	#'
	#' This function returns a dataframe or json object of AccountsReceivableInvoiceGroupClearances
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given AccountsReceivableInvoiceGroupClearances. Defaults to FALSE for all return fields which, for convenience, returns all fields for the AccountsReceivableInvoiceGroupClearances.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object listed as text. Run \code{\link{getSchemaForObjects}}('AccountsReceivableInvoiceGroupClearance') to get more field paths.
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
	#' @concept Accounts Receivable
	#' @return A list of AccountsReceivableInvoiceGroupClearances
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	listAccountsReceivableInvoiceGroupClearances <- function(searchConditionsList = NULL, InvoiceGroupClearanceID = F, InvoiceGroupID = F, GroupIDSecurity = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, fieldPaths = NULL, searchConditionsGroupType = "And", searchSortFieldNamesList = NULL, searchSortFieldNamesDescendingList = NULL, entityId = 1, schoolYearId = NULL, page = 1, pageSize = 100000, flatten = T, returnResponse = F){

		params <- as.list(environment())

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		listSkyObjects(module = "AccountsReceivable", objectName = "InvoiceGroupClearance", searchFields = searchFields %>% append(fieldPaths), page = page, pageSize = pageSize, SearchConditionsList = searchConditionsList, SearchConditionsGroupType = searchConditionsGroupType, SearchSortFieldNamesList = searchSortFieldNamesList, SearchSortFieldNamesDescendingList = searchSortFieldNamesDescendingList, entityId = entityId, schoolYearId = schoolYearId, flatten = flatten, returnResponse = returnResponse)
	}

	#' Get an AccountsReceivableInvoiceGroupClearance
	#'
	#' This function returns a dataframe or json object of an AccountsReceivableInvoiceGroupClearance
	#' @param AccountsReceivableInvoiceGroupClearanceID The ID of the AccountsReceivableInvoiceGroupClearance to return.
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given AccountsReceivableInvoiceGroupClearance. Defaults to FALSE for all return fields which, for convenience, returns all fields for the AccountsReceivableInvoiceGroupClearance.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object listed as text. Run \code{\link{getSchemaForObjects}}('AccountsReceivableInvoiceGroupClearance') to get more field paths.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{listSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Accounts Receivable
	#' @return A dataframe or of AccountsReceivableInvoiceGroupClearance
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	getAccountsReceivableInvoiceGroupClearance <- function(AccountsReceivableInvoiceGroupClearanceID, InvoiceGroupClearanceID = F, InvoiceGroupID = F, GroupIDSecurity = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, fieldPaths = NULL, entityId = 1, schoolYearId = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment()) %>% purrr::keep(names(.) != "AccountsReceivableInvoiceGroupClearanceID")

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		getSkyObject(module = "AccountsReceivable", objectName = "InvoiceGroupClearance", objectId = AccountsReceivableInvoiceGroupClearanceID, searchFields = searchFields, entityId = entityId, flatten = flatten, returnResponse = returnResponse)
	}

	#' Delete an AccountsReceivableInvoiceGroupClearance
	#'
	#' This function deletes an AccountsReceivableInvoiceGroupClearance
	#' @param AccountsReceivableInvoiceGroupClearanceID The ID of the AccountsReceivableInvoiceGroupClearance to delete
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{listSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Accounts Receivable
	#' @return The AccountsReceivableInvoiceGroupClearanceID of the deleted AccountsReceivableInvoiceGroupClearance.
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	deleteAccountsReceivableInvoiceGroupClearance <- function(AccountsReceivableInvoiceGroupClearanceID, ignoreWarnings = F, entityId = 1, schoolYearId = NULL, flatten = T, returnResponse = F){

		deleteSkyObject(module = "AccountsReceivable", objectName = "InvoiceGroupClearance", objectId = AccountsReceivableInvoiceGroupClearanceID, ignoreWarnings = ignoreWarnings, entityId = entityId, flatten = flatten, returnResponse = returnResponse)
	}

	#' Create an AccountsReceivableInvoiceGroupClearance
	#'
	#' This function creates an AccountsReceivableInvoiceGroupClearance
	#' @param fieldNames The field values to give the created AccountsReceivableInvoiceGroupClearance. Each defaults to NULL.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{listSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Accounts Receivable
	#' @return A newly created AccountsReceivableInvoiceGroupClearance
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	createAccountsReceivableInvoiceGroupClearance <- function(InvoiceGroupID = NULL, GroupIDSecurity = NULL, entityId = 1, schoolYearId = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment())

		body <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper()) %>% compact()

		createSkyObject(module = "AccountsReceivable", objectName = "InvoiceGroupClearance", body = list(DataObject = body), searchFields = append("InvoiceGroupClearanceID", body %>% names()), entityId = entityId, schoolYearId = schoolYearId, flatten = flatten, returnResponse = returnResponse)
	}

	#' Modify an AccountsReceivableInvoiceGroupClearance
	#'
	#' This function modifies an AccountsReceivableInvoiceGroupClearance
	#' @param fieldNames The field values to give the modified AccountsReceivableInvoiceGroupClearance. Each defaults to NULL.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{listSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Accounts Receivable
	#' @return The modified AccountsReceivableInvoiceGroupClearance
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	modifyAccountsReceivableInvoiceGroupClearance <- function(InvoiceGroupClearanceID, InvoiceGroupID = NULL, GroupIDSecurity = NULL, entityId = 1, schoolYearId = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment())

		body <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper()) %>% compact()

		modifySkyObject(module = "AccountsReceivable", objectName = "InvoiceGroupClearance", objectId = InvoiceGroupClearanceID, body = list(DataObject = body), searchFields = append("InvoiceGroupClearanceID", body %>% names()), entityId = entityId, schoolYearId = schoolYearId, flatten = flatten, returnResponse = returnResponse)
	}

	#' List AccountsReceivableItems
	#'
	#' This function returns a dataframe or json object of AccountsReceivableItems
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given AccountsReceivableItems. Defaults to FALSE for all return fields which, for convenience, returns all fields for the AccountsReceivableItems.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object listed as text. Run \code{\link{getSchemaForObjects}}('AccountsReceivableItem') to get more field paths.
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
	#' @concept Accounts Receivable
	#' @return A list of AccountsReceivableItems
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	listAccountsReceivableItems <- function(searchConditionsList = NULL, ItemID = F, DistrictID = F, DiscountID = F, Code = F, Description = F, AccountID = F, UnitOfMeasureID = F, AccountsReceivableAccrualID = F, CodeDescription = F, UnitCost = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, IsActive = F, fieldPaths = NULL, searchConditionsGroupType = "And", searchSortFieldNamesList = NULL, searchSortFieldNamesDescendingList = NULL, entityId = 1, schoolYearId = NULL, page = 1, pageSize = 100000, flatten = T, returnResponse = F){

		params <- as.list(environment())

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		listSkyObjects(module = "AccountsReceivable", objectName = "Item", searchFields = searchFields %>% append(fieldPaths), page = page, pageSize = pageSize, SearchConditionsList = searchConditionsList, SearchConditionsGroupType = searchConditionsGroupType, SearchSortFieldNamesList = searchSortFieldNamesList, SearchSortFieldNamesDescendingList = searchSortFieldNamesDescendingList, entityId = entityId, schoolYearId = schoolYearId, flatten = flatten, returnResponse = returnResponse)
	}

	#' Get an AccountsReceivableItem
	#'
	#' This function returns a dataframe or json object of an AccountsReceivableItem
	#' @param AccountsReceivableItemID The ID of the AccountsReceivableItem to return.
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given AccountsReceivableItem. Defaults to FALSE for all return fields which, for convenience, returns all fields for the AccountsReceivableItem.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object listed as text. Run \code{\link{getSchemaForObjects}}('AccountsReceivableItem') to get more field paths.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{listSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Accounts Receivable
	#' @return A dataframe or of AccountsReceivableItem
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	getAccountsReceivableItem <- function(AccountsReceivableItemID, ItemID = F, DistrictID = F, DiscountID = F, Code = F, Description = F, AccountID = F, UnitOfMeasureID = F, AccountsReceivableAccrualID = F, CodeDescription = F, UnitCost = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, IsActive = F, fieldPaths = NULL, entityId = 1, schoolYearId = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment()) %>% purrr::keep(names(.) != "AccountsReceivableItemID")

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		getSkyObject(module = "AccountsReceivable", objectName = "Item", objectId = AccountsReceivableItemID, searchFields = searchFields, entityId = entityId, flatten = flatten, returnResponse = returnResponse)
	}

	#' Delete an AccountsReceivableItem
	#'
	#' This function deletes an AccountsReceivableItem
	#' @param AccountsReceivableItemID The ID of the AccountsReceivableItem to delete
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{listSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Accounts Receivable
	#' @return The AccountsReceivableItemID of the deleted AccountsReceivableItem.
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	deleteAccountsReceivableItem <- function(AccountsReceivableItemID, ignoreWarnings = F, entityId = 1, schoolYearId = NULL, flatten = T, returnResponse = F){

		deleteSkyObject(module = "AccountsReceivable", objectName = "Item", objectId = AccountsReceivableItemID, ignoreWarnings = ignoreWarnings, entityId = entityId, flatten = flatten, returnResponse = returnResponse)
	}

	#' Create an AccountsReceivableItem
	#'
	#' This function creates an AccountsReceivableItem
	#' @param fieldNames The field values to give the created AccountsReceivableItem. Each defaults to NULL.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{listSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Accounts Receivable
	#' @return A newly created AccountsReceivableItem
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	createAccountsReceivableItem <- function(DistrictID = NULL, DiscountID = NULL, Code = NULL, Description = NULL, AccountID = NULL, UnitOfMeasureID = NULL, AccountsReceivableAccrualID = NULL, UnitCost = NULL, IsActive = NULL, entityId = 1, schoolYearId = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment())

		body <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper()) %>% compact()

		createSkyObject(module = "AccountsReceivable", objectName = "Item", body = list(DataObject = body), searchFields = append("ItemID", body %>% names()), entityId = entityId, schoolYearId = schoolYearId, flatten = flatten, returnResponse = returnResponse)
	}

	#' Modify an AccountsReceivableItem
	#'
	#' This function modifies an AccountsReceivableItem
	#' @param fieldNames The field values to give the modified AccountsReceivableItem. Each defaults to NULL.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{listSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Accounts Receivable
	#' @return The modified AccountsReceivableItem
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	modifyAccountsReceivableItem <- function(ItemID, DistrictID = NULL, DiscountID = NULL, Code = NULL, Description = NULL, AccountID = NULL, UnitOfMeasureID = NULL, AccountsReceivableAccrualID = NULL, UnitCost = NULL, IsActive = NULL, entityId = 1, schoolYearId = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment())

		body <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper()) %>% compact()

		modifySkyObject(module = "AccountsReceivable", objectName = "Item", objectId = ItemID, body = list(DataObject = body), searchFields = append("ItemID", body %>% names()), entityId = entityId, schoolYearId = schoolYearId, flatten = flatten, returnResponse = returnResponse)
	}

	#' List NextInvoiceNumbers
	#'
	#' This function returns a dataframe or json object of NextInvoiceNumbers
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given NextInvoiceNumbers. Defaults to FALSE for all return fields which, for convenience, returns all fields for the NextInvoiceNumbers.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object listed as text. Run \code{\link{getSchemaForObjects}}('NextInvoiceNumber') to get more field paths.
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
	#' @concept Accounts Receivable
	#' @return A list of NextInvoiceNumbers
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	listNextInvoiceNumbers <- function(searchConditionsList = NULL, NextInvoiceNumberID = F, InvoiceGroupID = F, FiscalYearID = F, InvoiceNumber = F, IsActive = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, fieldPaths = NULL, searchConditionsGroupType = "And", searchSortFieldNamesList = NULL, searchSortFieldNamesDescendingList = NULL, entityId = 1, schoolYearId = NULL, page = 1, pageSize = 100000, flatten = T, returnResponse = F){

		params <- as.list(environment())

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		listSkyObjects(module = "AccountsReceivable", objectName = "NextInvoiceNumber", searchFields = searchFields %>% append(fieldPaths), page = page, pageSize = pageSize, SearchConditionsList = searchConditionsList, SearchConditionsGroupType = searchConditionsGroupType, SearchSortFieldNamesList = searchSortFieldNamesList, SearchSortFieldNamesDescendingList = searchSortFieldNamesDescendingList, entityId = entityId, schoolYearId = schoolYearId, flatten = flatten, returnResponse = returnResponse)
	}

	#' Get a NextInvoiceNumber
	#'
	#' This function returns a dataframe or json object of a NextInvoiceNumber
	#' @param NextInvoiceNumberID The ID of the NextInvoiceNumber to return.
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given NextInvoiceNumber. Defaults to FALSE for all return fields which, for convenience, returns all fields for the NextInvoiceNumber.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object listed as text. Run \code{\link{getSchemaForObjects}}('NextInvoiceNumber') to get more field paths.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{listSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Accounts Receivable
	#' @return A dataframe or of NextInvoiceNumber
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	getNextInvoiceNumber <- function(NextInvoiceNumberID, InvoiceGroupID = F, FiscalYearID = F, InvoiceNumber = F, IsActive = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, fieldPaths = NULL, entityId = 1, schoolYearId = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment()) %>% purrr::keep(names(.) != "NextInvoiceNumberID")

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		getSkyObject(module = "AccountsReceivable", objectName = "NextInvoiceNumber", objectId = NextInvoiceNumberID, searchFields = searchFields, entityId = entityId, flatten = flatten, returnResponse = returnResponse)
	}

	#' Delete a NextInvoiceNumber
	#'
	#' This function deletes a NextInvoiceNumber
	#' @param NextInvoiceNumberID The ID of the NextInvoiceNumber to delete
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{listSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Accounts Receivable
	#' @return The NextInvoiceNumberID of the deleted NextInvoiceNumber.
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	deleteNextInvoiceNumber <- function(NextInvoiceNumberID, ignoreWarnings = F, entityId = 1, schoolYearId = NULL, flatten = T, returnResponse = F){

		deleteSkyObject(module = "AccountsReceivable", objectName = "NextInvoiceNumber", objectId = NextInvoiceNumberID, ignoreWarnings = ignoreWarnings, entityId = entityId, flatten = flatten, returnResponse = returnResponse)
	}

	#' Create a NextInvoiceNumber
	#'
	#' This function creates a NextInvoiceNumber
	#' @param fieldNames The field values to give the created NextInvoiceNumber. Each defaults to NULL.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{listSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Accounts Receivable
	#' @return A newly created NextInvoiceNumber
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	createNextInvoiceNumber <- function(InvoiceGroupID = NULL, FiscalYearID = NULL, InvoiceNumber = NULL, IsActive = NULL, entityId = 1, schoolYearId = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment())

		body <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper()) %>% compact()

		createSkyObject(module = "AccountsReceivable", objectName = "NextInvoiceNumber", body = list(DataObject = body), searchFields = append("NextInvoiceNumberID", body %>% names()), entityId = entityId, schoolYearId = schoolYearId, flatten = flatten, returnResponse = returnResponse)
	}

	#' Modify a NextInvoiceNumber
	#'
	#' This function modifies a NextInvoiceNumber
	#' @param fieldNames The field values to give the modified NextInvoiceNumber. Each defaults to NULL.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{listSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Accounts Receivable
	#' @return The modified NextInvoiceNumber
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	modifyNextInvoiceNumber <- function(NextInvoiceNumberID, InvoiceGroupID = NULL, FiscalYearID = NULL, InvoiceNumber = NULL, IsActive = NULL, entityId = 1, schoolYearId = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment())

		body <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper()) %>% compact()

		modifySkyObject(module = "AccountsReceivable", objectName = "NextInvoiceNumber", objectId = NextInvoiceNumberID, body = list(DataObject = body), searchFields = append("NextInvoiceNumberID", body %>% names()), entityId = entityId, schoolYearId = schoolYearId, flatten = flatten, returnResponse = returnResponse)
	}

	#' List AccountsReceivableTempInvoices
	#'
	#' This function returns a dataframe or json object of AccountsReceivableTempInvoices
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given AccountsReceivableTempInvoices. Defaults to FALSE for all return fields which, for convenience, returns all fields for the AccountsReceivableTempInvoices.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object listed as text. Run \code{\link{getSchemaForObjects}}('AccountsReceivableTempInvoice') to get more field paths.
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
	#' @concept Accounts Receivable
	#' @return A list of AccountsReceivableTempInvoices
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	listAccountsReceivableTempInvoices <- function(searchConditionsList = NULL, TempInvoiceID = F, ErrorCount = F, InvoiceID = F, InvoiceGroupID = F, InvoiceGroupCodeDescription = F, FiscalYearID = F, FiscalYearDescription = F, PayorID = F, PayorLFM = F, BankAccountID = F, BankAccountCodeDescription = F, Description = F, Amount = F, Batch = F, InvoiceNumber = F, InvoiceDate = F, DueDate = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, HasErrors = F, LineNumber = F, DeliveryType = F, DeliveryStatus = F, AmountPaid = F, AmountDue = F, AccountingUpdateIDBatch = F, BatchPostDate = F, fieldPaths = NULL, searchConditionsGroupType = "And", searchSortFieldNamesList = NULL, searchSortFieldNamesDescendingList = NULL, entityId = 1, schoolYearId = NULL, page = 1, pageSize = 100000, flatten = T, returnResponse = F){

		params <- as.list(environment())

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		listSkyObjects(module = "AccountsReceivable", objectName = "TempInvoice", searchFields = searchFields %>% append(fieldPaths), page = page, pageSize = pageSize, SearchConditionsList = searchConditionsList, SearchConditionsGroupType = searchConditionsGroupType, SearchSortFieldNamesList = searchSortFieldNamesList, SearchSortFieldNamesDescendingList = searchSortFieldNamesDescendingList, entityId = entityId, schoolYearId = schoolYearId, flatten = flatten, returnResponse = returnResponse)
	}

	#' Get an AccountsReceivableTempInvoice
	#'
	#' This function returns a dataframe or json object of an AccountsReceivableTempInvoice
	#' @param AccountsReceivableTempInvoiceID The ID of the AccountsReceivableTempInvoice to return.
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given AccountsReceivableTempInvoice. Defaults to FALSE for all return fields which, for convenience, returns all fields for the AccountsReceivableTempInvoice.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object listed as text. Run \code{\link{getSchemaForObjects}}('AccountsReceivableTempInvoice') to get more field paths.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{listSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Accounts Receivable
	#' @return A dataframe or of AccountsReceivableTempInvoice
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	getAccountsReceivableTempInvoice <- function(AccountsReceivableTempInvoiceID, TempInvoiceID = F, ErrorCount = F, InvoiceID = F, InvoiceGroupID = F, InvoiceGroupCodeDescription = F, FiscalYearID = F, FiscalYearDescription = F, PayorID = F, PayorLFM = F, BankAccountID = F, BankAccountCodeDescription = F, Description = F, Amount = F, Batch = F, InvoiceNumber = F, InvoiceDate = F, DueDate = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, HasErrors = F, LineNumber = F, DeliveryType = F, DeliveryStatus = F, AmountPaid = F, AmountDue = F, AccountingUpdateIDBatch = F, BatchPostDate = F, fieldPaths = NULL, entityId = 1, schoolYearId = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment()) %>% purrr::keep(names(.) != "AccountsReceivableTempInvoiceID")

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		getSkyObject(module = "AccountsReceivable", objectName = "TempInvoice", objectId = AccountsReceivableTempInvoiceID, searchFields = searchFields, entityId = entityId, flatten = flatten, returnResponse = returnResponse)
	}

	#' Delete an AccountsReceivableTempInvoice
	#'
	#' This function deletes an AccountsReceivableTempInvoice
	#' @param AccountsReceivableTempInvoiceID The ID of the AccountsReceivableTempInvoice to delete
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{listSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Accounts Receivable
	#' @return The AccountsReceivableTempInvoiceID of the deleted AccountsReceivableTempInvoice.
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	deleteAccountsReceivableTempInvoice <- function(AccountsReceivableTempInvoiceID, ignoreWarnings = F, entityId = 1, schoolYearId = NULL, flatten = T, returnResponse = F){

		deleteSkyObject(module = "AccountsReceivable", objectName = "TempInvoice", objectId = AccountsReceivableTempInvoiceID, ignoreWarnings = ignoreWarnings, entityId = entityId, flatten = flatten, returnResponse = returnResponse)
	}

	#' Create an AccountsReceivableTempInvoice
	#'
	#' This function creates an AccountsReceivableTempInvoice
	#' @param fieldNames The field values to give the created AccountsReceivableTempInvoice. Each defaults to NULL.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{listSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Accounts Receivable
	#' @return A newly created AccountsReceivableTempInvoice
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	createAccountsReceivableTempInvoice <- function(ErrorCount = NULL, InvoiceID = NULL, InvoiceGroupID = NULL, InvoiceGroupCodeDescription = NULL, FiscalYearID = NULL, FiscalYearDescription = NULL, PayorID = NULL, PayorLFM = NULL, BankAccountID = NULL, BankAccountCodeDescription = NULL, Description = NULL, Amount = NULL, Batch = NULL, InvoiceNumber = NULL, InvoiceDate = NULL, DueDate = NULL, HasErrors = NULL, LineNumber = NULL, DeliveryType = NULL, DeliveryStatus = NULL, AmountPaid = NULL, AmountDue = NULL, AccountingUpdateIDBatch = NULL, BatchPostDate = NULL, entityId = 1, schoolYearId = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment())

		body <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper()) %>% compact()

		createSkyObject(module = "AccountsReceivable", objectName = "TempInvoice", body = list(DataObject = body), searchFields = append("TempInvoiceID", body %>% names()), entityId = entityId, schoolYearId = schoolYearId, flatten = flatten, returnResponse = returnResponse)
	}

	#' Modify an AccountsReceivableTempInvoice
	#'
	#' This function modifies an AccountsReceivableTempInvoice
	#' @param fieldNames The field values to give the modified AccountsReceivableTempInvoice. Each defaults to NULL.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{listSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Accounts Receivable
	#' @return The modified AccountsReceivableTempInvoice
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	modifyAccountsReceivableTempInvoice <- function(TempInvoiceID, ErrorCount = NULL, InvoiceID = NULL, InvoiceGroupID = NULL, InvoiceGroupCodeDescription = NULL, FiscalYearID = NULL, FiscalYearDescription = NULL, PayorID = NULL, PayorLFM = NULL, BankAccountID = NULL, BankAccountCodeDescription = NULL, Description = NULL, Amount = NULL, Batch = NULL, InvoiceNumber = NULL, InvoiceDate = NULL, DueDate = NULL, HasErrors = NULL, LineNumber = NULL, DeliveryType = NULL, DeliveryStatus = NULL, AmountPaid = NULL, AmountDue = NULL, AccountingUpdateIDBatch = NULL, BatchPostDate = NULL, entityId = 1, schoolYearId = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment())

		body <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper()) %>% compact()

		modifySkyObject(module = "AccountsReceivable", objectName = "TempInvoice", objectId = TempInvoiceID, body = list(DataObject = body), searchFields = append("TempInvoiceID", body %>% names()), entityId = entityId, schoolYearId = schoolYearId, flatten = flatten, returnResponse = returnResponse)
	}

	#' List TempInvoiceErrorDetails
	#'
	#' This function returns a dataframe or json object of TempInvoiceErrorDetails
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given TempInvoiceErrorDetails. Defaults to FALSE for all return fields which, for convenience, returns all fields for the TempInvoiceErrorDetails.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object listed as text. Run \code{\link{getSchemaForObjects}}('TempInvoiceErrorDetail') to get more field paths.
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
	#' @concept Accounts Receivable
	#' @return A list of TempInvoiceErrorDetails
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	listTempInvoiceErrorDetails <- function(searchConditionsList = NULL, TempInvoiceErrorDetailID = F, TempInvoiceID = F, Error = F, ErrorDetail = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, LineNumber = F, fieldPaths = NULL, searchConditionsGroupType = "And", searchSortFieldNamesList = NULL, searchSortFieldNamesDescendingList = NULL, entityId = 1, schoolYearId = NULL, page = 1, pageSize = 100000, flatten = T, returnResponse = F){

		params <- as.list(environment())

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		listSkyObjects(module = "AccountsReceivable", objectName = "TempInvoiceErrorDetail", searchFields = searchFields %>% append(fieldPaths), page = page, pageSize = pageSize, SearchConditionsList = searchConditionsList, SearchConditionsGroupType = searchConditionsGroupType, SearchSortFieldNamesList = searchSortFieldNamesList, SearchSortFieldNamesDescendingList = searchSortFieldNamesDescendingList, entityId = entityId, schoolYearId = schoolYearId, flatten = flatten, returnResponse = returnResponse)
	}

	#' Get a TempInvoiceErrorDetail
	#'
	#' This function returns a dataframe or json object of a TempInvoiceErrorDetail
	#' @param TempInvoiceErrorDetailID The ID of the TempInvoiceErrorDetail to return.
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given TempInvoiceErrorDetail. Defaults to FALSE for all return fields which, for convenience, returns all fields for the TempInvoiceErrorDetail.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object listed as text. Run \code{\link{getSchemaForObjects}}('TempInvoiceErrorDetail') to get more field paths.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{listSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Accounts Receivable
	#' @return A dataframe or of TempInvoiceErrorDetail
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	getTempInvoiceErrorDetail <- function(TempInvoiceErrorDetailID, TempInvoiceID = F, Error = F, ErrorDetail = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, LineNumber = F, fieldPaths = NULL, entityId = 1, schoolYearId = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment()) %>% purrr::keep(names(.) != "TempInvoiceErrorDetailID")

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		getSkyObject(module = "AccountsReceivable", objectName = "TempInvoiceErrorDetail", objectId = TempInvoiceErrorDetailID, searchFields = searchFields, entityId = entityId, flatten = flatten, returnResponse = returnResponse)
	}

	#' Delete a TempInvoiceErrorDetail
	#'
	#' This function deletes a TempInvoiceErrorDetail
	#' @param TempInvoiceErrorDetailID The ID of the TempInvoiceErrorDetail to delete
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{listSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Accounts Receivable
	#' @return The TempInvoiceErrorDetailID of the deleted TempInvoiceErrorDetail.
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	deleteTempInvoiceErrorDetail <- function(TempInvoiceErrorDetailID, ignoreWarnings = F, entityId = 1, schoolYearId = NULL, flatten = T, returnResponse = F){

		deleteSkyObject(module = "AccountsReceivable", objectName = "TempInvoiceErrorDetail", objectId = TempInvoiceErrorDetailID, ignoreWarnings = ignoreWarnings, entityId = entityId, flatten = flatten, returnResponse = returnResponse)
	}

	#' Create a TempInvoiceErrorDetail
	#'
	#' This function creates a TempInvoiceErrorDetail
	#' @param fieldNames The field values to give the created TempInvoiceErrorDetail. Each defaults to NULL.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{listSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Accounts Receivable
	#' @return A newly created TempInvoiceErrorDetail
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	createTempInvoiceErrorDetail <- function(TempInvoiceID = NULL, Error = NULL, ErrorDetail = NULL, LineNumber = NULL, entityId = 1, schoolYearId = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment())

		body <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper()) %>% compact()

		createSkyObject(module = "AccountsReceivable", objectName = "TempInvoiceErrorDetail", body = list(DataObject = body), searchFields = append("TempInvoiceErrorDetailID", body %>% names()), entityId = entityId, schoolYearId = schoolYearId, flatten = flatten, returnResponse = returnResponse)
	}

	#' Modify a TempInvoiceErrorDetail
	#'
	#' This function modifies a TempInvoiceErrorDetail
	#' @param fieldNames The field values to give the modified TempInvoiceErrorDetail. Each defaults to NULL.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{listSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Accounts Receivable
	#' @return The modified TempInvoiceErrorDetail
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	modifyTempInvoiceErrorDetail <- function(TempInvoiceErrorDetailID, TempInvoiceID = NULL, Error = NULL, ErrorDetail = NULL, LineNumber = NULL, entityId = 1, schoolYearId = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment())

		body <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper()) %>% compact()

		modifySkyObject(module = "AccountsReceivable", objectName = "TempInvoiceErrorDetail", objectId = TempInvoiceErrorDetailID, body = list(DataObject = body), searchFields = append("TempInvoiceErrorDetailID", body %>% names()), entityId = entityId, schoolYearId = schoolYearId, flatten = flatten, returnResponse = returnResponse)
	}

	#' List InvoiceThirdPartyFormats
	#'
	#' This function returns a dataframe or json object of InvoiceThirdPartyFormats
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given InvoiceThirdPartyFormats. Defaults to FALSE for all return fields which, for convenience, returns all fields for the InvoiceThirdPartyFormats.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object listed as text. Run \code{\link{getSchemaForObjects}}('InvoiceThirdPartyFormat') to get more field paths.
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
	#' @concept Accounts Receivable
	#' @return A list of InvoiceThirdPartyFormats
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	listInvoiceThirdPartyFormats <- function(searchConditionsList = NULL, InvoiceThirdPartyFormatID = F, SkywardID = F, SkywardHash = F, SkywardIDClonedFrom = F, DistrictID = F, Code = F, Description = F, CodeDescription = F, IsSystemLoaded = F, ImportType = F, DateFormat = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, DefaultBankAccountID = F, DefaultInvoiceGroupID = F, PayorIdentification = F, fieldPaths = NULL, searchConditionsGroupType = "And", searchSortFieldNamesList = NULL, searchSortFieldNamesDescendingList = NULL, entityId = 1, schoolYearId = NULL, page = 1, pageSize = 100000, flatten = T, returnResponse = F){

		params <- as.list(environment())

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		listSkyObjects(module = "AccountsReceivable", objectName = "InvoiceThirdPartyFormat", searchFields = searchFields %>% append(fieldPaths), page = page, pageSize = pageSize, SearchConditionsList = searchConditionsList, SearchConditionsGroupType = searchConditionsGroupType, SearchSortFieldNamesList = searchSortFieldNamesList, SearchSortFieldNamesDescendingList = searchSortFieldNamesDescendingList, entityId = entityId, schoolYearId = schoolYearId, flatten = flatten, returnResponse = returnResponse)
	}

	#' Get an InvoiceThirdPartyFormat
	#'
	#' This function returns a dataframe or json object of an InvoiceThirdPartyFormat
	#' @param InvoiceThirdPartyFormatID The ID of the InvoiceThirdPartyFormat to return.
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given InvoiceThirdPartyFormat. Defaults to FALSE for all return fields which, for convenience, returns all fields for the InvoiceThirdPartyFormat.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object listed as text. Run \code{\link{getSchemaForObjects}}('InvoiceThirdPartyFormat') to get more field paths.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{listSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Accounts Receivable
	#' @return A dataframe or of InvoiceThirdPartyFormat
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	getInvoiceThirdPartyFormat <- function(InvoiceThirdPartyFormatID, SkywardID = F, SkywardHash = F, SkywardIDClonedFrom = F, DistrictID = F, Code = F, Description = F, CodeDescription = F, IsSystemLoaded = F, ImportType = F, DateFormat = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, DefaultBankAccountID = F, DefaultInvoiceGroupID = F, PayorIdentification = F, fieldPaths = NULL, entityId = 1, schoolYearId = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment()) %>% purrr::keep(names(.) != "InvoiceThirdPartyFormatID")

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		getSkyObject(module = "AccountsReceivable", objectName = "InvoiceThirdPartyFormat", objectId = InvoiceThirdPartyFormatID, searchFields = searchFields, entityId = entityId, flatten = flatten, returnResponse = returnResponse)
	}

	#' Delete an InvoiceThirdPartyFormat
	#'
	#' This function deletes an InvoiceThirdPartyFormat
	#' @param InvoiceThirdPartyFormatID The ID of the InvoiceThirdPartyFormat to delete
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{listSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Accounts Receivable
	#' @return The InvoiceThirdPartyFormatID of the deleted InvoiceThirdPartyFormat.
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	deleteInvoiceThirdPartyFormat <- function(InvoiceThirdPartyFormatID, ignoreWarnings = F, entityId = 1, schoolYearId = NULL, flatten = T, returnResponse = F){

		deleteSkyObject(module = "AccountsReceivable", objectName = "InvoiceThirdPartyFormat", objectId = InvoiceThirdPartyFormatID, ignoreWarnings = ignoreWarnings, entityId = entityId, flatten = flatten, returnResponse = returnResponse)
	}

	#' Create an InvoiceThirdPartyFormat
	#'
	#' This function creates an InvoiceThirdPartyFormat
	#' @param fieldNames The field values to give the created InvoiceThirdPartyFormat. Each defaults to NULL.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{listSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Accounts Receivable
	#' @return A newly created InvoiceThirdPartyFormat
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	createInvoiceThirdPartyFormat <- function(SkywardIDClonedFrom = NULL, DistrictID = NULL, Code = NULL, Description = NULL, ImportType = NULL, DateFormat = NULL, DefaultBankAccountID = NULL, DefaultInvoiceGroupID = NULL, PayorIdentification = NULL, entityId = 1, schoolYearId = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment())

		body <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper()) %>% compact()

		createSkyObject(module = "AccountsReceivable", objectName = "InvoiceThirdPartyFormat", body = list(DataObject = body), searchFields = append("InvoiceThirdPartyFormatID", body %>% names()), entityId = entityId, schoolYearId = schoolYearId, flatten = flatten, returnResponse = returnResponse)
	}

	#' Modify an InvoiceThirdPartyFormat
	#'
	#' This function modifies an InvoiceThirdPartyFormat
	#' @param fieldNames The field values to give the modified InvoiceThirdPartyFormat. Each defaults to NULL.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{listSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Accounts Receivable
	#' @return The modified InvoiceThirdPartyFormat
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	modifyInvoiceThirdPartyFormat <- function(InvoiceThirdPartyFormatID, SkywardIDClonedFrom = NULL, DistrictID = NULL, Code = NULL, Description = NULL, ImportType = NULL, DateFormat = NULL, DefaultBankAccountID = NULL, DefaultInvoiceGroupID = NULL, PayorIdentification = NULL, entityId = 1, schoolYearId = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment())

		body <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper()) %>% compact()

		modifySkyObject(module = "AccountsReceivable", objectName = "InvoiceThirdPartyFormat", objectId = InvoiceThirdPartyFormatID, body = list(DataObject = body), searchFields = append("InvoiceThirdPartyFormatID", body %>% names()), entityId = entityId, schoolYearId = schoolYearId, flatten = flatten, returnResponse = returnResponse)
	}

	#' List InvoiceThirdPartyImports
	#'
	#' This function returns a dataframe or json object of InvoiceThirdPartyImports
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given InvoiceThirdPartyImports. Defaults to FALSE for all return fields which, for convenience, returns all fields for the InvoiceThirdPartyImports.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object listed as text. Run \code{\link{getSchemaForObjects}}('InvoiceThirdPartyImport') to get more field paths.
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
	#' @concept Accounts Receivable
	#' @return A list of InvoiceThirdPartyImports
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	listInvoiceThirdPartyImports <- function(searchConditionsList = NULL, InvoiceThirdPartyImportID = F, ImportTime = F, ImportData = F, MediaID = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, InvoiceThirdPartyFormatID = F, MediaIDFailedResult = F, fieldPaths = NULL, searchConditionsGroupType = "And", searchSortFieldNamesList = NULL, searchSortFieldNamesDescendingList = NULL, entityId = 1, schoolYearId = NULL, page = 1, pageSize = 100000, flatten = T, returnResponse = F){

		params <- as.list(environment())

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		listSkyObjects(module = "AccountsReceivable", objectName = "InvoiceThirdPartyImport", searchFields = searchFields %>% append(fieldPaths), page = page, pageSize = pageSize, SearchConditionsList = searchConditionsList, SearchConditionsGroupType = searchConditionsGroupType, SearchSortFieldNamesList = searchSortFieldNamesList, SearchSortFieldNamesDescendingList = searchSortFieldNamesDescendingList, entityId = entityId, schoolYearId = schoolYearId, flatten = flatten, returnResponse = returnResponse)
	}

	#' Get an InvoiceThirdPartyImport
	#'
	#' This function returns a dataframe or json object of an InvoiceThirdPartyImport
	#' @param InvoiceThirdPartyImportID The ID of the InvoiceThirdPartyImport to return.
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given InvoiceThirdPartyImport. Defaults to FALSE for all return fields which, for convenience, returns all fields for the InvoiceThirdPartyImport.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object listed as text. Run \code{\link{getSchemaForObjects}}('InvoiceThirdPartyImport') to get more field paths.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{listSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Accounts Receivable
	#' @return A dataframe or of InvoiceThirdPartyImport
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	getInvoiceThirdPartyImport <- function(InvoiceThirdPartyImportID, ImportTime = F, ImportData = F, MediaID = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, InvoiceThirdPartyFormatID = F, MediaIDFailedResult = F, fieldPaths = NULL, entityId = 1, schoolYearId = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment()) %>% purrr::keep(names(.) != "InvoiceThirdPartyImportID")

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		getSkyObject(module = "AccountsReceivable", objectName = "InvoiceThirdPartyImport", objectId = InvoiceThirdPartyImportID, searchFields = searchFields, entityId = entityId, flatten = flatten, returnResponse = returnResponse)
	}

	#' Delete an InvoiceThirdPartyImport
	#'
	#' This function deletes an InvoiceThirdPartyImport
	#' @param InvoiceThirdPartyImportID The ID of the InvoiceThirdPartyImport to delete
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{listSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Accounts Receivable
	#' @return The InvoiceThirdPartyImportID of the deleted InvoiceThirdPartyImport.
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	deleteInvoiceThirdPartyImport <- function(InvoiceThirdPartyImportID, ignoreWarnings = F, entityId = 1, schoolYearId = NULL, flatten = T, returnResponse = F){

		deleteSkyObject(module = "AccountsReceivable", objectName = "InvoiceThirdPartyImport", objectId = InvoiceThirdPartyImportID, ignoreWarnings = ignoreWarnings, entityId = entityId, flatten = flatten, returnResponse = returnResponse)
	}

	#' Create an InvoiceThirdPartyImport
	#'
	#' This function creates an InvoiceThirdPartyImport
	#' @param fieldNames The field values to give the created InvoiceThirdPartyImport. Each defaults to NULL.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{listSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Accounts Receivable
	#' @return A newly created InvoiceThirdPartyImport
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	createInvoiceThirdPartyImport <- function(ImportTime = NULL, ImportData = NULL, MediaID = NULL, InvoiceThirdPartyFormatID = NULL, MediaIDFailedResult = NULL, entityId = 1, schoolYearId = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment())

		body <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper()) %>% compact()

		createSkyObject(module = "AccountsReceivable", objectName = "InvoiceThirdPartyImport", body = list(DataObject = body), searchFields = append("InvoiceThirdPartyImportID", body %>% names()), entityId = entityId, schoolYearId = schoolYearId, flatten = flatten, returnResponse = returnResponse)
	}

	#' Modify an InvoiceThirdPartyImport
	#'
	#' This function modifies an InvoiceThirdPartyImport
	#' @param fieldNames The field values to give the modified InvoiceThirdPartyImport. Each defaults to NULL.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{listSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Accounts Receivable
	#' @return The modified InvoiceThirdPartyImport
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	modifyInvoiceThirdPartyImport <- function(InvoiceThirdPartyImportID, ImportTime = NULL, ImportData = NULL, MediaID = NULL, InvoiceThirdPartyFormatID = NULL, MediaIDFailedResult = NULL, entityId = 1, schoolYearId = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment())

		body <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper()) %>% compact()

		modifySkyObject(module = "AccountsReceivable", objectName = "InvoiceThirdPartyImport", objectId = InvoiceThirdPartyImportID, body = list(DataObject = body), searchFields = append("InvoiceThirdPartyImportID", body %>% names()), entityId = entityId, schoolYearId = schoolYearId, flatten = flatten, returnResponse = returnResponse)
	}

	#' List InvoiceThirdPartyFormatInvoiceGroups
	#'
	#' This function returns a dataframe or json object of InvoiceThirdPartyFormatInvoiceGroups
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given InvoiceThirdPartyFormatInvoiceGroups. Defaults to FALSE for all return fields which, for convenience, returns all fields for the InvoiceThirdPartyFormatInvoiceGroups.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object listed as text. Run \code{\link{getSchemaForObjects}}('InvoiceThirdPartyFormatInvoiceGroup') to get more field paths.
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
	#' @concept Accounts Receivable
	#' @return A list of InvoiceThirdPartyFormatInvoiceGroups
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	listInvoiceThirdPartyFormatInvoiceGroups <- function(searchConditionsList = NULL, InvoiceThirdPartyFormatInvoiceGroupID = F, InvoiceGroupID = F, InvoiceThirdPartyFormatID = F, ImportValue = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, fieldPaths = NULL, searchConditionsGroupType = "And", searchSortFieldNamesList = NULL, searchSortFieldNamesDescendingList = NULL, entityId = 1, schoolYearId = NULL, page = 1, pageSize = 100000, flatten = T, returnResponse = F){

		params <- as.list(environment())

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		listSkyObjects(module = "AccountsReceivable", objectName = "InvoiceThirdPartyFormatInvoiceGroup", searchFields = searchFields %>% append(fieldPaths), page = page, pageSize = pageSize, SearchConditionsList = searchConditionsList, SearchConditionsGroupType = searchConditionsGroupType, SearchSortFieldNamesList = searchSortFieldNamesList, SearchSortFieldNamesDescendingList = searchSortFieldNamesDescendingList, entityId = entityId, schoolYearId = schoolYearId, flatten = flatten, returnResponse = returnResponse)
	}

	#' Get an InvoiceThirdPartyFormatInvoiceGroup
	#'
	#' This function returns a dataframe or json object of an InvoiceThirdPartyFormatInvoiceGroup
	#' @param InvoiceThirdPartyFormatInvoiceGroupID The ID of the InvoiceThirdPartyFormatInvoiceGroup to return.
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given InvoiceThirdPartyFormatInvoiceGroup. Defaults to FALSE for all return fields which, for convenience, returns all fields for the InvoiceThirdPartyFormatInvoiceGroup.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object listed as text. Run \code{\link{getSchemaForObjects}}('InvoiceThirdPartyFormatInvoiceGroup') to get more field paths.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{listSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Accounts Receivable
	#' @return A dataframe or of InvoiceThirdPartyFormatInvoiceGroup
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	getInvoiceThirdPartyFormatInvoiceGroup <- function(InvoiceThirdPartyFormatInvoiceGroupID, InvoiceGroupID = F, InvoiceThirdPartyFormatID = F, ImportValue = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, fieldPaths = NULL, entityId = 1, schoolYearId = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment()) %>% purrr::keep(names(.) != "InvoiceThirdPartyFormatInvoiceGroupID")

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		getSkyObject(module = "AccountsReceivable", objectName = "InvoiceThirdPartyFormatInvoiceGroup", objectId = InvoiceThirdPartyFormatInvoiceGroupID, searchFields = searchFields, entityId = entityId, flatten = flatten, returnResponse = returnResponse)
	}

	#' Delete an InvoiceThirdPartyFormatInvoiceGroup
	#'
	#' This function deletes an InvoiceThirdPartyFormatInvoiceGroup
	#' @param InvoiceThirdPartyFormatInvoiceGroupID The ID of the InvoiceThirdPartyFormatInvoiceGroup to delete
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{listSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Accounts Receivable
	#' @return The InvoiceThirdPartyFormatInvoiceGroupID of the deleted InvoiceThirdPartyFormatInvoiceGroup.
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	deleteInvoiceThirdPartyFormatInvoiceGroup <- function(InvoiceThirdPartyFormatInvoiceGroupID, ignoreWarnings = F, entityId = 1, schoolYearId = NULL, flatten = T, returnResponse = F){

		deleteSkyObject(module = "AccountsReceivable", objectName = "InvoiceThirdPartyFormatInvoiceGroup", objectId = InvoiceThirdPartyFormatInvoiceGroupID, ignoreWarnings = ignoreWarnings, entityId = entityId, flatten = flatten, returnResponse = returnResponse)
	}

	#' Create an InvoiceThirdPartyFormatInvoiceGroup
	#'
	#' This function creates an InvoiceThirdPartyFormatInvoiceGroup
	#' @param fieldNames The field values to give the created InvoiceThirdPartyFormatInvoiceGroup. Each defaults to NULL.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{listSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Accounts Receivable
	#' @return A newly created InvoiceThirdPartyFormatInvoiceGroup
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	createInvoiceThirdPartyFormatInvoiceGroup <- function(InvoiceGroupID = NULL, InvoiceThirdPartyFormatID = NULL, ImportValue = NULL, entityId = 1, schoolYearId = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment())

		body <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper()) %>% compact()

		createSkyObject(module = "AccountsReceivable", objectName = "InvoiceThirdPartyFormatInvoiceGroup", body = list(DataObject = body), searchFields = append("InvoiceThirdPartyFormatInvoiceGroupID", body %>% names()), entityId = entityId, schoolYearId = schoolYearId, flatten = flatten, returnResponse = returnResponse)
	}

	#' Modify an InvoiceThirdPartyFormatInvoiceGroup
	#'
	#' This function modifies an InvoiceThirdPartyFormatInvoiceGroup
	#' @param fieldNames The field values to give the modified InvoiceThirdPartyFormatInvoiceGroup. Each defaults to NULL.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{listSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Accounts Receivable
	#' @return The modified InvoiceThirdPartyFormatInvoiceGroup
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	modifyInvoiceThirdPartyFormatInvoiceGroup <- function(InvoiceThirdPartyFormatInvoiceGroupID, InvoiceGroupID = NULL, InvoiceThirdPartyFormatID = NULL, ImportValue = NULL, entityId = 1, schoolYearId = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment())

		body <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper()) %>% compact()

		modifySkyObject(module = "AccountsReceivable", objectName = "InvoiceThirdPartyFormatInvoiceGroup", objectId = InvoiceThirdPartyFormatInvoiceGroupID, body = list(DataObject = body), searchFields = append("InvoiceThirdPartyFormatInvoiceGroupID", body %>% names()), entityId = entityId, schoolYearId = schoolYearId, flatten = flatten, returnResponse = returnResponse)
	}

	#' List InvoiceThirdPartyFormatPayors
	#'
	#' This function returns a dataframe or json object of InvoiceThirdPartyFormatPayors
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given InvoiceThirdPartyFormatPayors. Defaults to FALSE for all return fields which, for convenience, returns all fields for the InvoiceThirdPartyFormatPayors.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object listed as text. Run \code{\link{getSchemaForObjects}}('InvoiceThirdPartyFormatPayor') to get more field paths.
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
	#' @concept Accounts Receivable
	#' @return A list of InvoiceThirdPartyFormatPayors
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	listInvoiceThirdPartyFormatPayors <- function(searchConditionsList = NULL, InvoiceThirdPartyFormatPayorID = F, PayorID = F, InvoiceThirdPartyFormatID = F, ImportValue = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, fieldPaths = NULL, searchConditionsGroupType = "And", searchSortFieldNamesList = NULL, searchSortFieldNamesDescendingList = NULL, entityId = 1, schoolYearId = NULL, page = 1, pageSize = 100000, flatten = T, returnResponse = F){

		params <- as.list(environment())

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		listSkyObjects(module = "AccountsReceivable", objectName = "InvoiceThirdPartyFormatPayor", searchFields = searchFields %>% append(fieldPaths), page = page, pageSize = pageSize, SearchConditionsList = searchConditionsList, SearchConditionsGroupType = searchConditionsGroupType, SearchSortFieldNamesList = searchSortFieldNamesList, SearchSortFieldNamesDescendingList = searchSortFieldNamesDescendingList, entityId = entityId, schoolYearId = schoolYearId, flatten = flatten, returnResponse = returnResponse)
	}

	#' Get an InvoiceThirdPartyFormatPayor
	#'
	#' This function returns a dataframe or json object of an InvoiceThirdPartyFormatPayor
	#' @param InvoiceThirdPartyFormatPayorID The ID of the InvoiceThirdPartyFormatPayor to return.
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given InvoiceThirdPartyFormatPayor. Defaults to FALSE for all return fields which, for convenience, returns all fields for the InvoiceThirdPartyFormatPayor.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object listed as text. Run \code{\link{getSchemaForObjects}}('InvoiceThirdPartyFormatPayor') to get more field paths.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{listSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Accounts Receivable
	#' @return A dataframe or of InvoiceThirdPartyFormatPayor
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	getInvoiceThirdPartyFormatPayor <- function(InvoiceThirdPartyFormatPayorID, PayorID = F, InvoiceThirdPartyFormatID = F, ImportValue = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, fieldPaths = NULL, entityId = 1, schoolYearId = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment()) %>% purrr::keep(names(.) != "InvoiceThirdPartyFormatPayorID")

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		getSkyObject(module = "AccountsReceivable", objectName = "InvoiceThirdPartyFormatPayor", objectId = InvoiceThirdPartyFormatPayorID, searchFields = searchFields, entityId = entityId, flatten = flatten, returnResponse = returnResponse)
	}

	#' Delete an InvoiceThirdPartyFormatPayor
	#'
	#' This function deletes an InvoiceThirdPartyFormatPayor
	#' @param InvoiceThirdPartyFormatPayorID The ID of the InvoiceThirdPartyFormatPayor to delete
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{listSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Accounts Receivable
	#' @return The InvoiceThirdPartyFormatPayorID of the deleted InvoiceThirdPartyFormatPayor.
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	deleteInvoiceThirdPartyFormatPayor <- function(InvoiceThirdPartyFormatPayorID, ignoreWarnings = F, entityId = 1, schoolYearId = NULL, flatten = T, returnResponse = F){

		deleteSkyObject(module = "AccountsReceivable", objectName = "InvoiceThirdPartyFormatPayor", objectId = InvoiceThirdPartyFormatPayorID, ignoreWarnings = ignoreWarnings, entityId = entityId, flatten = flatten, returnResponse = returnResponse)
	}

	#' Create an InvoiceThirdPartyFormatPayor
	#'
	#' This function creates an InvoiceThirdPartyFormatPayor
	#' @param fieldNames The field values to give the created InvoiceThirdPartyFormatPayor. Each defaults to NULL.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{listSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Accounts Receivable
	#' @return A newly created InvoiceThirdPartyFormatPayor
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	createInvoiceThirdPartyFormatPayor <- function(PayorID = NULL, InvoiceThirdPartyFormatID = NULL, ImportValue = NULL, entityId = 1, schoolYearId = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment())

		body <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper()) %>% compact()

		createSkyObject(module = "AccountsReceivable", objectName = "InvoiceThirdPartyFormatPayor", body = list(DataObject = body), searchFields = append("InvoiceThirdPartyFormatPayorID", body %>% names()), entityId = entityId, schoolYearId = schoolYearId, flatten = flatten, returnResponse = returnResponse)
	}

	#' Modify an InvoiceThirdPartyFormatPayor
	#'
	#' This function modifies an InvoiceThirdPartyFormatPayor
	#' @param fieldNames The field values to give the modified InvoiceThirdPartyFormatPayor. Each defaults to NULL.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{listSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Accounts Receivable
	#' @return The modified InvoiceThirdPartyFormatPayor
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	modifyInvoiceThirdPartyFormatPayor <- function(InvoiceThirdPartyFormatPayorID, PayorID = NULL, InvoiceThirdPartyFormatID = NULL, ImportValue = NULL, entityId = 1, schoolYearId = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment())

		body <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper()) %>% compact()

		modifySkyObject(module = "AccountsReceivable", objectName = "InvoiceThirdPartyFormatPayor", objectId = InvoiceThirdPartyFormatPayorID, body = list(DataObject = body), searchFields = append("InvoiceThirdPartyFormatPayorID", body %>% names()), entityId = entityId, schoolYearId = schoolYearId, flatten = flatten, returnResponse = returnResponse)
	}

	#' List InvoiceThirdPartyFormatItems
	#'
	#' This function returns a dataframe or json object of InvoiceThirdPartyFormatItems
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given InvoiceThirdPartyFormatItems. Defaults to FALSE for all return fields which, for convenience, returns all fields for the InvoiceThirdPartyFormatItems.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object listed as text. Run \code{\link{getSchemaForObjects}}('InvoiceThirdPartyFormatItem') to get more field paths.
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
	#' @concept Accounts Receivable
	#' @return A list of InvoiceThirdPartyFormatItems
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	listInvoiceThirdPartyFormatItems <- function(searchConditionsList = NULL, InvoiceThirdPartyFormatItemID = F, ItemID = F, InvoiceThirdPartyFormatID = F, ImportValue = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, fieldPaths = NULL, searchConditionsGroupType = "And", searchSortFieldNamesList = NULL, searchSortFieldNamesDescendingList = NULL, entityId = 1, schoolYearId = NULL, page = 1, pageSize = 100000, flatten = T, returnResponse = F){

		params <- as.list(environment())

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		listSkyObjects(module = "AccountsReceivable", objectName = "InvoiceThirdPartyFormatItem", searchFields = searchFields %>% append(fieldPaths), page = page, pageSize = pageSize, SearchConditionsList = searchConditionsList, SearchConditionsGroupType = searchConditionsGroupType, SearchSortFieldNamesList = searchSortFieldNamesList, SearchSortFieldNamesDescendingList = searchSortFieldNamesDescendingList, entityId = entityId, schoolYearId = schoolYearId, flatten = flatten, returnResponse = returnResponse)
	}

	#' Get an InvoiceThirdPartyFormatItem
	#'
	#' This function returns a dataframe or json object of an InvoiceThirdPartyFormatItem
	#' @param InvoiceThirdPartyFormatItemID The ID of the InvoiceThirdPartyFormatItem to return.
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given InvoiceThirdPartyFormatItem. Defaults to FALSE for all return fields which, for convenience, returns all fields for the InvoiceThirdPartyFormatItem.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object listed as text. Run \code{\link{getSchemaForObjects}}('InvoiceThirdPartyFormatItem') to get more field paths.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{listSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Accounts Receivable
	#' @return A dataframe or of InvoiceThirdPartyFormatItem
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	getInvoiceThirdPartyFormatItem <- function(InvoiceThirdPartyFormatItemID, ItemID = F, InvoiceThirdPartyFormatID = F, ImportValue = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, fieldPaths = NULL, entityId = 1, schoolYearId = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment()) %>% purrr::keep(names(.) != "InvoiceThirdPartyFormatItemID")

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		getSkyObject(module = "AccountsReceivable", objectName = "InvoiceThirdPartyFormatItem", objectId = InvoiceThirdPartyFormatItemID, searchFields = searchFields, entityId = entityId, flatten = flatten, returnResponse = returnResponse)
	}

	#' Delete an InvoiceThirdPartyFormatItem
	#'
	#' This function deletes an InvoiceThirdPartyFormatItem
	#' @param InvoiceThirdPartyFormatItemID The ID of the InvoiceThirdPartyFormatItem to delete
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{listSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Accounts Receivable
	#' @return The InvoiceThirdPartyFormatItemID of the deleted InvoiceThirdPartyFormatItem.
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	deleteInvoiceThirdPartyFormatItem <- function(InvoiceThirdPartyFormatItemID, ignoreWarnings = F, entityId = 1, schoolYearId = NULL, flatten = T, returnResponse = F){

		deleteSkyObject(module = "AccountsReceivable", objectName = "InvoiceThirdPartyFormatItem", objectId = InvoiceThirdPartyFormatItemID, ignoreWarnings = ignoreWarnings, entityId = entityId, flatten = flatten, returnResponse = returnResponse)
	}

	#' Create an InvoiceThirdPartyFormatItem
	#'
	#' This function creates an InvoiceThirdPartyFormatItem
	#' @param fieldNames The field values to give the created InvoiceThirdPartyFormatItem. Each defaults to NULL.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{listSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Accounts Receivable
	#' @return A newly created InvoiceThirdPartyFormatItem
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	createInvoiceThirdPartyFormatItem <- function(ItemID = NULL, InvoiceThirdPartyFormatID = NULL, ImportValue = NULL, entityId = 1, schoolYearId = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment())

		body <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper()) %>% compact()

		createSkyObject(module = "AccountsReceivable", objectName = "InvoiceThirdPartyFormatItem", body = list(DataObject = body), searchFields = append("InvoiceThirdPartyFormatItemID", body %>% names()), entityId = entityId, schoolYearId = schoolYearId, flatten = flatten, returnResponse = returnResponse)
	}

	#' Modify an InvoiceThirdPartyFormatItem
	#'
	#' This function modifies an InvoiceThirdPartyFormatItem
	#' @param fieldNames The field values to give the modified InvoiceThirdPartyFormatItem. Each defaults to NULL.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{listSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Accounts Receivable
	#' @return The modified InvoiceThirdPartyFormatItem
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	modifyInvoiceThirdPartyFormatItem <- function(InvoiceThirdPartyFormatItemID, ItemID = NULL, InvoiceThirdPartyFormatID = NULL, ImportValue = NULL, entityId = 1, schoolYearId = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment())

		body <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper()) %>% compact()

		modifySkyObject(module = "AccountsReceivable", objectName = "InvoiceThirdPartyFormatItem", objectId = InvoiceThirdPartyFormatItemID, body = list(DataObject = body), searchFields = append("InvoiceThirdPartyFormatItemID", body %>% names()), entityId = entityId, schoolYearId = schoolYearId, flatten = flatten, returnResponse = returnResponse)
	}

	#' List InvoiceThirdPartyFormatDiscounts
	#'
	#' This function returns a dataframe or json object of InvoiceThirdPartyFormatDiscounts
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given InvoiceThirdPartyFormatDiscounts. Defaults to FALSE for all return fields which, for convenience, returns all fields for the InvoiceThirdPartyFormatDiscounts.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object listed as text. Run \code{\link{getSchemaForObjects}}('InvoiceThirdPartyFormatDiscount') to get more field paths.
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
	#' @concept Accounts Receivable
	#' @return A list of InvoiceThirdPartyFormatDiscounts
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	listInvoiceThirdPartyFormatDiscounts <- function(searchConditionsList = NULL, InvoiceThirdPartyFormatDiscountID = F, DiscountID = F, InvoiceThirdPartyFormatID = F, ImportValue = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, fieldPaths = NULL, searchConditionsGroupType = "And", searchSortFieldNamesList = NULL, searchSortFieldNamesDescendingList = NULL, entityId = 1, schoolYearId = NULL, page = 1, pageSize = 100000, flatten = T, returnResponse = F){

		params <- as.list(environment())

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		listSkyObjects(module = "AccountsReceivable", objectName = "InvoiceThirdPartyFormatDiscount", searchFields = searchFields %>% append(fieldPaths), page = page, pageSize = pageSize, SearchConditionsList = searchConditionsList, SearchConditionsGroupType = searchConditionsGroupType, SearchSortFieldNamesList = searchSortFieldNamesList, SearchSortFieldNamesDescendingList = searchSortFieldNamesDescendingList, entityId = entityId, schoolYearId = schoolYearId, flatten = flatten, returnResponse = returnResponse)
	}

	#' Get an InvoiceThirdPartyFormatDiscount
	#'
	#' This function returns a dataframe or json object of an InvoiceThirdPartyFormatDiscount
	#' @param InvoiceThirdPartyFormatDiscountID The ID of the InvoiceThirdPartyFormatDiscount to return.
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given InvoiceThirdPartyFormatDiscount. Defaults to FALSE for all return fields which, for convenience, returns all fields for the InvoiceThirdPartyFormatDiscount.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object listed as text. Run \code{\link{getSchemaForObjects}}('InvoiceThirdPartyFormatDiscount') to get more field paths.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{listSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Accounts Receivable
	#' @return A dataframe or of InvoiceThirdPartyFormatDiscount
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	getInvoiceThirdPartyFormatDiscount <- function(InvoiceThirdPartyFormatDiscountID, DiscountID = F, InvoiceThirdPartyFormatID = F, ImportValue = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, fieldPaths = NULL, entityId = 1, schoolYearId = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment()) %>% purrr::keep(names(.) != "InvoiceThirdPartyFormatDiscountID")

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		getSkyObject(module = "AccountsReceivable", objectName = "InvoiceThirdPartyFormatDiscount", objectId = InvoiceThirdPartyFormatDiscountID, searchFields = searchFields, entityId = entityId, flatten = flatten, returnResponse = returnResponse)
	}

	#' Delete an InvoiceThirdPartyFormatDiscount
	#'
	#' This function deletes an InvoiceThirdPartyFormatDiscount
	#' @param InvoiceThirdPartyFormatDiscountID The ID of the InvoiceThirdPartyFormatDiscount to delete
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{listSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Accounts Receivable
	#' @return The InvoiceThirdPartyFormatDiscountID of the deleted InvoiceThirdPartyFormatDiscount.
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	deleteInvoiceThirdPartyFormatDiscount <- function(InvoiceThirdPartyFormatDiscountID, ignoreWarnings = F, entityId = 1, schoolYearId = NULL, flatten = T, returnResponse = F){

		deleteSkyObject(module = "AccountsReceivable", objectName = "InvoiceThirdPartyFormatDiscount", objectId = InvoiceThirdPartyFormatDiscountID, ignoreWarnings = ignoreWarnings, entityId = entityId, flatten = flatten, returnResponse = returnResponse)
	}

	#' Create an InvoiceThirdPartyFormatDiscount
	#'
	#' This function creates an InvoiceThirdPartyFormatDiscount
	#' @param fieldNames The field values to give the created InvoiceThirdPartyFormatDiscount. Each defaults to NULL.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{listSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Accounts Receivable
	#' @return A newly created InvoiceThirdPartyFormatDiscount
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	createInvoiceThirdPartyFormatDiscount <- function(DiscountID = NULL, InvoiceThirdPartyFormatID = NULL, ImportValue = NULL, entityId = 1, schoolYearId = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment())

		body <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper()) %>% compact()

		createSkyObject(module = "AccountsReceivable", objectName = "InvoiceThirdPartyFormatDiscount", body = list(DataObject = body), searchFields = append("InvoiceThirdPartyFormatDiscountID", body %>% names()), entityId = entityId, schoolYearId = schoolYearId, flatten = flatten, returnResponse = returnResponse)
	}

	#' Modify an InvoiceThirdPartyFormatDiscount
	#'
	#' This function modifies an InvoiceThirdPartyFormatDiscount
	#' @param fieldNames The field values to give the modified InvoiceThirdPartyFormatDiscount. Each defaults to NULL.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{listSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Accounts Receivable
	#' @return The modified InvoiceThirdPartyFormatDiscount
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	modifyInvoiceThirdPartyFormatDiscount <- function(InvoiceThirdPartyFormatDiscountID, DiscountID = NULL, InvoiceThirdPartyFormatID = NULL, ImportValue = NULL, entityId = 1, schoolYearId = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment())

		body <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper()) %>% compact()

		modifySkyObject(module = "AccountsReceivable", objectName = "InvoiceThirdPartyFormatDiscount", objectId = InvoiceThirdPartyFormatDiscountID, body = list(DataObject = body), searchFields = append("InvoiceThirdPartyFormatDiscountID", body %>% names()), entityId = entityId, schoolYearId = schoolYearId, flatten = flatten, returnResponse = returnResponse)
	}

	#' List InvoiceThirdPartyFormatBankAccounts
	#'
	#' This function returns a dataframe or json object of InvoiceThirdPartyFormatBankAccounts
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given InvoiceThirdPartyFormatBankAccounts. Defaults to FALSE for all return fields which, for convenience, returns all fields for the InvoiceThirdPartyFormatBankAccounts.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object listed as text. Run \code{\link{getSchemaForObjects}}('InvoiceThirdPartyFormatBankAccount') to get more field paths.
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
	#' @concept Accounts Receivable
	#' @return A list of InvoiceThirdPartyFormatBankAccounts
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	listInvoiceThirdPartyFormatBankAccounts <- function(searchConditionsList = NULL, InvoiceThirdPartyFormatBankAccountID = F, BankAccountID = F, InvoiceThirdPartyFormatID = F, ImportValue = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, fieldPaths = NULL, searchConditionsGroupType = "And", searchSortFieldNamesList = NULL, searchSortFieldNamesDescendingList = NULL, entityId = 1, schoolYearId = NULL, page = 1, pageSize = 100000, flatten = T, returnResponse = F){

		params <- as.list(environment())

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		listSkyObjects(module = "AccountsReceivable", objectName = "InvoiceThirdPartyFormatBankAccount", searchFields = searchFields %>% append(fieldPaths), page = page, pageSize = pageSize, SearchConditionsList = searchConditionsList, SearchConditionsGroupType = searchConditionsGroupType, SearchSortFieldNamesList = searchSortFieldNamesList, SearchSortFieldNamesDescendingList = searchSortFieldNamesDescendingList, entityId = entityId, schoolYearId = schoolYearId, flatten = flatten, returnResponse = returnResponse)
	}

	#' Get an InvoiceThirdPartyFormatBankAccount
	#'
	#' This function returns a dataframe or json object of an InvoiceThirdPartyFormatBankAccount
	#' @param InvoiceThirdPartyFormatBankAccountID The ID of the InvoiceThirdPartyFormatBankAccount to return.
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given InvoiceThirdPartyFormatBankAccount. Defaults to FALSE for all return fields which, for convenience, returns all fields for the InvoiceThirdPartyFormatBankAccount.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object listed as text. Run \code{\link{getSchemaForObjects}}('InvoiceThirdPartyFormatBankAccount') to get more field paths.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{listSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Accounts Receivable
	#' @return A dataframe or of InvoiceThirdPartyFormatBankAccount
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	getInvoiceThirdPartyFormatBankAccount <- function(InvoiceThirdPartyFormatBankAccountID, BankAccountID = F, InvoiceThirdPartyFormatID = F, ImportValue = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, fieldPaths = NULL, entityId = 1, schoolYearId = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment()) %>% purrr::keep(names(.) != "InvoiceThirdPartyFormatBankAccountID")

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		getSkyObject(module = "AccountsReceivable", objectName = "InvoiceThirdPartyFormatBankAccount", objectId = InvoiceThirdPartyFormatBankAccountID, searchFields = searchFields, entityId = entityId, flatten = flatten, returnResponse = returnResponse)
	}

	#' Delete an InvoiceThirdPartyFormatBankAccount
	#'
	#' This function deletes an InvoiceThirdPartyFormatBankAccount
	#' @param InvoiceThirdPartyFormatBankAccountID The ID of the InvoiceThirdPartyFormatBankAccount to delete
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{listSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Accounts Receivable
	#' @return The InvoiceThirdPartyFormatBankAccountID of the deleted InvoiceThirdPartyFormatBankAccount.
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	deleteInvoiceThirdPartyFormatBankAccount <- function(InvoiceThirdPartyFormatBankAccountID, ignoreWarnings = F, entityId = 1, schoolYearId = NULL, flatten = T, returnResponse = F){

		deleteSkyObject(module = "AccountsReceivable", objectName = "InvoiceThirdPartyFormatBankAccount", objectId = InvoiceThirdPartyFormatBankAccountID, ignoreWarnings = ignoreWarnings, entityId = entityId, flatten = flatten, returnResponse = returnResponse)
	}

	#' Create an InvoiceThirdPartyFormatBankAccount
	#'
	#' This function creates an InvoiceThirdPartyFormatBankAccount
	#' @param fieldNames The field values to give the created InvoiceThirdPartyFormatBankAccount. Each defaults to NULL.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{listSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Accounts Receivable
	#' @return A newly created InvoiceThirdPartyFormatBankAccount
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	createInvoiceThirdPartyFormatBankAccount <- function(BankAccountID = NULL, InvoiceThirdPartyFormatID = NULL, ImportValue = NULL, entityId = 1, schoolYearId = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment())

		body <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper()) %>% compact()

		createSkyObject(module = "AccountsReceivable", objectName = "InvoiceThirdPartyFormatBankAccount", body = list(DataObject = body), searchFields = append("InvoiceThirdPartyFormatBankAccountID", body %>% names()), entityId = entityId, schoolYearId = schoolYearId, flatten = flatten, returnResponse = returnResponse)
	}

	#' Modify an InvoiceThirdPartyFormatBankAccount
	#'
	#' This function modifies an InvoiceThirdPartyFormatBankAccount
	#' @param fieldNames The field values to give the modified InvoiceThirdPartyFormatBankAccount. Each defaults to NULL.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{listSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Accounts Receivable
	#' @return The modified InvoiceThirdPartyFormatBankAccount
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	modifyInvoiceThirdPartyFormatBankAccount <- function(InvoiceThirdPartyFormatBankAccountID, BankAccountID = NULL, InvoiceThirdPartyFormatID = NULL, ImportValue = NULL, entityId = 1, schoolYearId = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment())

		body <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper()) %>% compact()

		modifySkyObject(module = "AccountsReceivable", objectName = "InvoiceThirdPartyFormatBankAccount", objectId = InvoiceThirdPartyFormatBankAccountID, body = list(DataObject = body), searchFields = append("InvoiceThirdPartyFormatBankAccountID", body %>% names()), entityId = entityId, schoolYearId = schoolYearId, flatten = flatten, returnResponse = returnResponse)
	}

	#' List InvoiceThirdPartyFormatAccounts
	#'
	#' This function returns a dataframe or json object of InvoiceThirdPartyFormatAccounts
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given InvoiceThirdPartyFormatAccounts. Defaults to FALSE for all return fields which, for convenience, returns all fields for the InvoiceThirdPartyFormatAccounts.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object listed as text. Run \code{\link{getSchemaForObjects}}('InvoiceThirdPartyFormatAccount') to get more field paths.
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
	#' @concept Accounts Receivable
	#' @return A list of InvoiceThirdPartyFormatAccounts
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	listInvoiceThirdPartyFormatAccounts <- function(searchConditionsList = NULL, InvoiceThirdPartyFormatAccountID = F, AccountID = F, InvoiceThirdPartyFormatID = F, ImportValue = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, fieldPaths = NULL, searchConditionsGroupType = "And", searchSortFieldNamesList = NULL, searchSortFieldNamesDescendingList = NULL, entityId = 1, schoolYearId = NULL, page = 1, pageSize = 100000, flatten = T, returnResponse = F){

		params <- as.list(environment())

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		listSkyObjects(module = "AccountsReceivable", objectName = "InvoiceThirdPartyFormatAccount", searchFields = searchFields %>% append(fieldPaths), page = page, pageSize = pageSize, SearchConditionsList = searchConditionsList, SearchConditionsGroupType = searchConditionsGroupType, SearchSortFieldNamesList = searchSortFieldNamesList, SearchSortFieldNamesDescendingList = searchSortFieldNamesDescendingList, entityId = entityId, schoolYearId = schoolYearId, flatten = flatten, returnResponse = returnResponse)
	}

	#' Get an InvoiceThirdPartyFormatAccount
	#'
	#' This function returns a dataframe or json object of an InvoiceThirdPartyFormatAccount
	#' @param InvoiceThirdPartyFormatAccountID The ID of the InvoiceThirdPartyFormatAccount to return.
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given InvoiceThirdPartyFormatAccount. Defaults to FALSE for all return fields which, for convenience, returns all fields for the InvoiceThirdPartyFormatAccount.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object listed as text. Run \code{\link{getSchemaForObjects}}('InvoiceThirdPartyFormatAccount') to get more field paths.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{listSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Accounts Receivable
	#' @return A dataframe or of InvoiceThirdPartyFormatAccount
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	getInvoiceThirdPartyFormatAccount <- function(InvoiceThirdPartyFormatAccountID, AccountID = F, InvoiceThirdPartyFormatID = F, ImportValue = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, fieldPaths = NULL, entityId = 1, schoolYearId = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment()) %>% purrr::keep(names(.) != "InvoiceThirdPartyFormatAccountID")

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		getSkyObject(module = "AccountsReceivable", objectName = "InvoiceThirdPartyFormatAccount", objectId = InvoiceThirdPartyFormatAccountID, searchFields = searchFields, entityId = entityId, flatten = flatten, returnResponse = returnResponse)
	}

	#' Delete an InvoiceThirdPartyFormatAccount
	#'
	#' This function deletes an InvoiceThirdPartyFormatAccount
	#' @param InvoiceThirdPartyFormatAccountID The ID of the InvoiceThirdPartyFormatAccount to delete
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{listSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Accounts Receivable
	#' @return The InvoiceThirdPartyFormatAccountID of the deleted InvoiceThirdPartyFormatAccount.
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	deleteInvoiceThirdPartyFormatAccount <- function(InvoiceThirdPartyFormatAccountID, ignoreWarnings = F, entityId = 1, schoolYearId = NULL, flatten = T, returnResponse = F){

		deleteSkyObject(module = "AccountsReceivable", objectName = "InvoiceThirdPartyFormatAccount", objectId = InvoiceThirdPartyFormatAccountID, ignoreWarnings = ignoreWarnings, entityId = entityId, flatten = flatten, returnResponse = returnResponse)
	}

	#' Create an InvoiceThirdPartyFormatAccount
	#'
	#' This function creates an InvoiceThirdPartyFormatAccount
	#' @param fieldNames The field values to give the created InvoiceThirdPartyFormatAccount. Each defaults to NULL.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{listSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Accounts Receivable
	#' @return A newly created InvoiceThirdPartyFormatAccount
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	createInvoiceThirdPartyFormatAccount <- function(AccountID = NULL, InvoiceThirdPartyFormatID = NULL, ImportValue = NULL, entityId = 1, schoolYearId = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment())

		body <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper()) %>% compact()

		createSkyObject(module = "AccountsReceivable", objectName = "InvoiceThirdPartyFormatAccount", body = list(DataObject = body), searchFields = append("InvoiceThirdPartyFormatAccountID", body %>% names()), entityId = entityId, schoolYearId = schoolYearId, flatten = flatten, returnResponse = returnResponse)
	}

	#' Modify an InvoiceThirdPartyFormatAccount
	#'
	#' This function modifies an InvoiceThirdPartyFormatAccount
	#' @param fieldNames The field values to give the modified InvoiceThirdPartyFormatAccount. Each defaults to NULL.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{listSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Accounts Receivable
	#' @return The modified InvoiceThirdPartyFormatAccount
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	modifyInvoiceThirdPartyFormatAccount <- function(InvoiceThirdPartyFormatAccountID, AccountID = NULL, InvoiceThirdPartyFormatID = NULL, ImportValue = NULL, entityId = 1, schoolYearId = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment())

		body <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper()) %>% compact()

		modifySkyObject(module = "AccountsReceivable", objectName = "InvoiceThirdPartyFormatAccount", objectId = InvoiceThirdPartyFormatAccountID, body = list(DataObject = body), searchFields = append("InvoiceThirdPartyFormatAccountID", body %>% names()), entityId = entityId, schoolYearId = schoolYearId, flatten = flatten, returnResponse = returnResponse)
	}

	#' List InvoiceThirdPartyFormatUnitOfMeasures
	#'
	#' This function returns a dataframe or json object of InvoiceThirdPartyFormatUnitOfMeasures
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given InvoiceThirdPartyFormatUnitOfMeasures. Defaults to FALSE for all return fields which, for convenience, returns all fields for the InvoiceThirdPartyFormatUnitOfMeasures.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object listed as text. Run \code{\link{getSchemaForObjects}}('InvoiceThirdPartyFormatUnitOfMeasure') to get more field paths.
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
	#' @concept Accounts Receivable
	#' @return A list of InvoiceThirdPartyFormatUnitOfMeasures
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	listInvoiceThirdPartyFormatUnitOfMeasures <- function(searchConditionsList = NULL, InvoiceThirdPartyFormatUnitOfMeasureID = F, UnitOfMeasureID = F, InvoiceThirdPartyFormatID = F, ImportValue = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, fieldPaths = NULL, searchConditionsGroupType = "And", searchSortFieldNamesList = NULL, searchSortFieldNamesDescendingList = NULL, entityId = 1, schoolYearId = NULL, page = 1, pageSize = 100000, flatten = T, returnResponse = F){

		params <- as.list(environment())

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		listSkyObjects(module = "AccountsReceivable", objectName = "InvoiceThirdPartyFormatUnitOfMeasure", searchFields = searchFields %>% append(fieldPaths), page = page, pageSize = pageSize, SearchConditionsList = searchConditionsList, SearchConditionsGroupType = searchConditionsGroupType, SearchSortFieldNamesList = searchSortFieldNamesList, SearchSortFieldNamesDescendingList = searchSortFieldNamesDescendingList, entityId = entityId, schoolYearId = schoolYearId, flatten = flatten, returnResponse = returnResponse)
	}

	#' Get an InvoiceThirdPartyFormatUnitOfMeasure
	#'
	#' This function returns a dataframe or json object of an InvoiceThirdPartyFormatUnitOfMeasure
	#' @param InvoiceThirdPartyFormatUnitOfMeasureID The ID of the InvoiceThirdPartyFormatUnitOfMeasure to return.
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given InvoiceThirdPartyFormatUnitOfMeasure. Defaults to FALSE for all return fields which, for convenience, returns all fields for the InvoiceThirdPartyFormatUnitOfMeasure.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object listed as text. Run \code{\link{getSchemaForObjects}}('InvoiceThirdPartyFormatUnitOfMeasure') to get more field paths.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{listSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Accounts Receivable
	#' @return A dataframe or of InvoiceThirdPartyFormatUnitOfMeasure
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	getInvoiceThirdPartyFormatUnitOfMeasure <- function(InvoiceThirdPartyFormatUnitOfMeasureID, UnitOfMeasureID = F, InvoiceThirdPartyFormatID = F, ImportValue = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, fieldPaths = NULL, entityId = 1, schoolYearId = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment()) %>% purrr::keep(names(.) != "InvoiceThirdPartyFormatUnitOfMeasureID")

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		getSkyObject(module = "AccountsReceivable", objectName = "InvoiceThirdPartyFormatUnitOfMeasure", objectId = InvoiceThirdPartyFormatUnitOfMeasureID, searchFields = searchFields, entityId = entityId, flatten = flatten, returnResponse = returnResponse)
	}

	#' Delete an InvoiceThirdPartyFormatUnitOfMeasure
	#'
	#' This function deletes an InvoiceThirdPartyFormatUnitOfMeasure
	#' @param InvoiceThirdPartyFormatUnitOfMeasureID The ID of the InvoiceThirdPartyFormatUnitOfMeasure to delete
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{listSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Accounts Receivable
	#' @return The InvoiceThirdPartyFormatUnitOfMeasureID of the deleted InvoiceThirdPartyFormatUnitOfMeasure.
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	deleteInvoiceThirdPartyFormatUnitOfMeasure <- function(InvoiceThirdPartyFormatUnitOfMeasureID, ignoreWarnings = F, entityId = 1, schoolYearId = NULL, flatten = T, returnResponse = F){

		deleteSkyObject(module = "AccountsReceivable", objectName = "InvoiceThirdPartyFormatUnitOfMeasure", objectId = InvoiceThirdPartyFormatUnitOfMeasureID, ignoreWarnings = ignoreWarnings, entityId = entityId, flatten = flatten, returnResponse = returnResponse)
	}

	#' Create an InvoiceThirdPartyFormatUnitOfMeasure
	#'
	#' This function creates an InvoiceThirdPartyFormatUnitOfMeasure
	#' @param fieldNames The field values to give the created InvoiceThirdPartyFormatUnitOfMeasure. Each defaults to NULL.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{listSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Accounts Receivable
	#' @return A newly created InvoiceThirdPartyFormatUnitOfMeasure
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	createInvoiceThirdPartyFormatUnitOfMeasure <- function(UnitOfMeasureID = NULL, InvoiceThirdPartyFormatID = NULL, ImportValue = NULL, entityId = 1, schoolYearId = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment())

		body <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper()) %>% compact()

		createSkyObject(module = "AccountsReceivable", objectName = "InvoiceThirdPartyFormatUnitOfMeasure", body = list(DataObject = body), searchFields = append("InvoiceThirdPartyFormatUnitOfMeasureID", body %>% names()), entityId = entityId, schoolYearId = schoolYearId, flatten = flatten, returnResponse = returnResponse)
	}

	#' Modify an InvoiceThirdPartyFormatUnitOfMeasure
	#'
	#' This function modifies an InvoiceThirdPartyFormatUnitOfMeasure
	#' @param fieldNames The field values to give the modified InvoiceThirdPartyFormatUnitOfMeasure. Each defaults to NULL.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{listSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Accounts Receivable
	#' @return The modified InvoiceThirdPartyFormatUnitOfMeasure
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	modifyInvoiceThirdPartyFormatUnitOfMeasure <- function(InvoiceThirdPartyFormatUnitOfMeasureID, UnitOfMeasureID = NULL, InvoiceThirdPartyFormatID = NULL, ImportValue = NULL, entityId = 1, schoolYearId = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment())

		body <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper()) %>% compact()

		modifySkyObject(module = "AccountsReceivable", objectName = "InvoiceThirdPartyFormatUnitOfMeasure", objectId = InvoiceThirdPartyFormatUnitOfMeasureID, body = list(DataObject = body), searchFields = append("InvoiceThirdPartyFormatUnitOfMeasureID", body %>% names()), entityId = entityId, schoolYearId = schoolYearId, flatten = flatten, returnResponse = returnResponse)
	}

	#' List InvoiceDelimitedFileFormats
	#'
	#' This function returns a dataframe or json object of InvoiceDelimitedFileFormats
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given InvoiceDelimitedFileFormats. Defaults to FALSE for all return fields which, for convenience, returns all fields for the InvoiceDelimitedFileFormats.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object listed as text. Run \code{\link{getSchemaForObjects}}('InvoiceDelimitedFileFormat') to get more field paths.
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
	#' @concept Accounts Receivable
	#' @return A list of InvoiceDelimitedFileFormats
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	listInvoiceDelimitedFileFormats <- function(searchConditionsList = NULL, InvoiceDelimitedFileFormatID = F, SkywardID = F, SkywardHash = F, InvoiceThirdPartyFormatID = F, NumberOfHeaderRows = F, DelimiterType = F, DelimiterCharacter = F, InvoiceGroupColumnNumber = F, PayorColumnNumber = F, BankAccountColumnNumber = F, BatchColumnNumber = F, InvoiceDateColumnNumber = F, DueDateColumnNumber = F, ItemColumnNumber = F, QuantityColumnNumber = F, UnitOfMeasureColumnNumber = F, UnitCostColumnNumber = F, DiscountColumnNumber = F, AccountColumnNumber = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, InvoiceDescriptionColumnNumber = F, InvoiceDetailDescriptionColumnNumber = F, PurchaseOrderNumberColumnNumber = F, fieldPaths = NULL, searchConditionsGroupType = "And", searchSortFieldNamesList = NULL, searchSortFieldNamesDescendingList = NULL, entityId = 1, schoolYearId = NULL, page = 1, pageSize = 100000, flatten = T, returnResponse = F){

		params <- as.list(environment())

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		listSkyObjects(module = "AccountsReceivable", objectName = "InvoiceDelimitedFileFormat", searchFields = searchFields %>% append(fieldPaths), page = page, pageSize = pageSize, SearchConditionsList = searchConditionsList, SearchConditionsGroupType = searchConditionsGroupType, SearchSortFieldNamesList = searchSortFieldNamesList, SearchSortFieldNamesDescendingList = searchSortFieldNamesDescendingList, entityId = entityId, schoolYearId = schoolYearId, flatten = flatten, returnResponse = returnResponse)
	}

	#' Get an InvoiceDelimitedFileFormat
	#'
	#' This function returns a dataframe or json object of an InvoiceDelimitedFileFormat
	#' @param InvoiceDelimitedFileFormatID The ID of the InvoiceDelimitedFileFormat to return.
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given InvoiceDelimitedFileFormat. Defaults to FALSE for all return fields which, for convenience, returns all fields for the InvoiceDelimitedFileFormat.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object listed as text. Run \code{\link{getSchemaForObjects}}('InvoiceDelimitedFileFormat') to get more field paths.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{listSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Accounts Receivable
	#' @return A dataframe or of InvoiceDelimitedFileFormat
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	getInvoiceDelimitedFileFormat <- function(InvoiceDelimitedFileFormatID, SkywardID = F, SkywardHash = F, InvoiceThirdPartyFormatID = F, NumberOfHeaderRows = F, DelimiterType = F, DelimiterCharacter = F, InvoiceGroupColumnNumber = F, PayorColumnNumber = F, BankAccountColumnNumber = F, BatchColumnNumber = F, InvoiceDateColumnNumber = F, DueDateColumnNumber = F, ItemColumnNumber = F, QuantityColumnNumber = F, UnitOfMeasureColumnNumber = F, UnitCostColumnNumber = F, DiscountColumnNumber = F, AccountColumnNumber = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, InvoiceDescriptionColumnNumber = F, InvoiceDetailDescriptionColumnNumber = F, PurchaseOrderNumberColumnNumber = F, fieldPaths = NULL, entityId = 1, schoolYearId = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment()) %>% purrr::keep(names(.) != "InvoiceDelimitedFileFormatID")

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		getSkyObject(module = "AccountsReceivable", objectName = "InvoiceDelimitedFileFormat", objectId = InvoiceDelimitedFileFormatID, searchFields = searchFields, entityId = entityId, flatten = flatten, returnResponse = returnResponse)
	}

	#' Delete an InvoiceDelimitedFileFormat
	#'
	#' This function deletes an InvoiceDelimitedFileFormat
	#' @param InvoiceDelimitedFileFormatID The ID of the InvoiceDelimitedFileFormat to delete
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{listSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Accounts Receivable
	#' @return The InvoiceDelimitedFileFormatID of the deleted InvoiceDelimitedFileFormat.
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	deleteInvoiceDelimitedFileFormat <- function(InvoiceDelimitedFileFormatID, ignoreWarnings = F, entityId = 1, schoolYearId = NULL, flatten = T, returnResponse = F){

		deleteSkyObject(module = "AccountsReceivable", objectName = "InvoiceDelimitedFileFormat", objectId = InvoiceDelimitedFileFormatID, ignoreWarnings = ignoreWarnings, entityId = entityId, flatten = flatten, returnResponse = returnResponse)
	}

	#' Create an InvoiceDelimitedFileFormat
	#'
	#' This function creates an InvoiceDelimitedFileFormat
	#' @param fieldNames The field values to give the created InvoiceDelimitedFileFormat. Each defaults to NULL.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{listSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Accounts Receivable
	#' @return A newly created InvoiceDelimitedFileFormat
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	createInvoiceDelimitedFileFormat <- function(InvoiceThirdPartyFormatID = NULL, NumberOfHeaderRows = NULL, DelimiterType = NULL, DelimiterCharacter = NULL, InvoiceGroupColumnNumber = NULL, PayorColumnNumber = NULL, BankAccountColumnNumber = NULL, BatchColumnNumber = NULL, InvoiceDateColumnNumber = NULL, DueDateColumnNumber = NULL, ItemColumnNumber = NULL, QuantityColumnNumber = NULL, UnitOfMeasureColumnNumber = NULL, UnitCostColumnNumber = NULL, DiscountColumnNumber = NULL, AccountColumnNumber = NULL, InvoiceDescriptionColumnNumber = NULL, InvoiceDetailDescriptionColumnNumber = NULL, PurchaseOrderNumberColumnNumber = NULL, entityId = 1, schoolYearId = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment())

		body <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper()) %>% compact()

		createSkyObject(module = "AccountsReceivable", objectName = "InvoiceDelimitedFileFormat", body = list(DataObject = body), searchFields = append("InvoiceDelimitedFileFormatID", body %>% names()), entityId = entityId, schoolYearId = schoolYearId, flatten = flatten, returnResponse = returnResponse)
	}

	#' Modify an InvoiceDelimitedFileFormat
	#'
	#' This function modifies an InvoiceDelimitedFileFormat
	#' @param fieldNames The field values to give the modified InvoiceDelimitedFileFormat. Each defaults to NULL.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{listSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Accounts Receivable
	#' @return The modified InvoiceDelimitedFileFormat
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	modifyInvoiceDelimitedFileFormat <- function(InvoiceDelimitedFileFormatID, InvoiceThirdPartyFormatID = NULL, NumberOfHeaderRows = NULL, DelimiterType = NULL, DelimiterCharacter = NULL, InvoiceGroupColumnNumber = NULL, PayorColumnNumber = NULL, BankAccountColumnNumber = NULL, BatchColumnNumber = NULL, InvoiceDateColumnNumber = NULL, DueDateColumnNumber = NULL, ItemColumnNumber = NULL, QuantityColumnNumber = NULL, UnitOfMeasureColumnNumber = NULL, UnitCostColumnNumber = NULL, DiscountColumnNumber = NULL, AccountColumnNumber = NULL, InvoiceDescriptionColumnNumber = NULL, InvoiceDetailDescriptionColumnNumber = NULL, PurchaseOrderNumberColumnNumber = NULL, entityId = 1, schoolYearId = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment())

		body <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper()) %>% compact()

		modifySkyObject(module = "AccountsReceivable", objectName = "InvoiceDelimitedFileFormat", objectId = InvoiceDelimitedFileFormatID, body = list(DataObject = body), searchFields = append("InvoiceDelimitedFileFormatID", body %>% names()), entityId = entityId, schoolYearId = schoolYearId, flatten = flatten, returnResponse = returnResponse)
	}

	#' List InvoiceFixedLengthFileFormats
	#'
	#' This function returns a dataframe or json object of InvoiceFixedLengthFileFormats
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given InvoiceFixedLengthFileFormats. Defaults to FALSE for all return fields which, for convenience, returns all fields for the InvoiceFixedLengthFileFormats.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object listed as text. Run \code{\link{getSchemaForObjects}}('InvoiceFixedLengthFileFormat') to get more field paths.
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
	#' @concept Accounts Receivable
	#' @return A list of InvoiceFixedLengthFileFormats
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	listInvoiceFixedLengthFileFormats <- function(searchConditionsList = NULL, InvoiceFixedLengthFileFormatID = F, SkywardID = F, SkywardHash = F, InvoiceThirdPartyFormatID = F, NumberOfHeaderRows = F, InvoiceGroupStartPosition = F, InvoiceGroupLength = F, PayorStartPosition = F, PayorLength = F, BankAccountStartPosition = F, BankAccountLength = F, InvoiceDescriptionStartPosition = F, InvoiceDescriptionLength = F, BatchStartPosition = F, BatchLength = F, InvoiceDateStartPosition = F, InvoiceDateLength = F, DueDateStartPosition = F, DueDateLength = F, ItemStartPosition = F, ItemLength = F, InvoiceDetailDescriptionStartPosition = F, InvoiceDetailDescriptionLength = F, QuantityStartPosition = F, QuantityLength = F, UnitOfMeasureStartPosition = F, UnitOfMeasureLength = F, UnitCostStartPosition = F, UnitCostLength = F, DiscountStartPosition = F, DiscountLength = F, AccountStartPosition = F, AccountLength = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, PurchaseOrderNumberStartPosition = F, PurchaseOrderNumberLength = F, fieldPaths = NULL, searchConditionsGroupType = "And", searchSortFieldNamesList = NULL, searchSortFieldNamesDescendingList = NULL, entityId = 1, schoolYearId = NULL, page = 1, pageSize = 100000, flatten = T, returnResponse = F){

		params <- as.list(environment())

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		listSkyObjects(module = "AccountsReceivable", objectName = "InvoiceFixedLengthFileFormat", searchFields = searchFields %>% append(fieldPaths), page = page, pageSize = pageSize, SearchConditionsList = searchConditionsList, SearchConditionsGroupType = searchConditionsGroupType, SearchSortFieldNamesList = searchSortFieldNamesList, SearchSortFieldNamesDescendingList = searchSortFieldNamesDescendingList, entityId = entityId, schoolYearId = schoolYearId, flatten = flatten, returnResponse = returnResponse)
	}

	#' Get an InvoiceFixedLengthFileFormat
	#'
	#' This function returns a dataframe or json object of an InvoiceFixedLengthFileFormat
	#' @param InvoiceFixedLengthFileFormatID The ID of the InvoiceFixedLengthFileFormat to return.
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given InvoiceFixedLengthFileFormat. Defaults to FALSE for all return fields which, for convenience, returns all fields for the InvoiceFixedLengthFileFormat.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object listed as text. Run \code{\link{getSchemaForObjects}}('InvoiceFixedLengthFileFormat') to get more field paths.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{listSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Accounts Receivable
	#' @return A dataframe or of InvoiceFixedLengthFileFormat
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	getInvoiceFixedLengthFileFormat <- function(InvoiceFixedLengthFileFormatID, SkywardID = F, SkywardHash = F, InvoiceThirdPartyFormatID = F, NumberOfHeaderRows = F, InvoiceGroupStartPosition = F, InvoiceGroupLength = F, PayorStartPosition = F, PayorLength = F, BankAccountStartPosition = F, BankAccountLength = F, InvoiceDescriptionStartPosition = F, InvoiceDescriptionLength = F, BatchStartPosition = F, BatchLength = F, InvoiceDateStartPosition = F, InvoiceDateLength = F, DueDateStartPosition = F, DueDateLength = F, ItemStartPosition = F, ItemLength = F, InvoiceDetailDescriptionStartPosition = F, InvoiceDetailDescriptionLength = F, QuantityStartPosition = F, QuantityLength = F, UnitOfMeasureStartPosition = F, UnitOfMeasureLength = F, UnitCostStartPosition = F, UnitCostLength = F, DiscountStartPosition = F, DiscountLength = F, AccountStartPosition = F, AccountLength = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, PurchaseOrderNumberStartPosition = F, PurchaseOrderNumberLength = F, fieldPaths = NULL, entityId = 1, schoolYearId = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment()) %>% purrr::keep(names(.) != "InvoiceFixedLengthFileFormatID")

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		getSkyObject(module = "AccountsReceivable", objectName = "InvoiceFixedLengthFileFormat", objectId = InvoiceFixedLengthFileFormatID, searchFields = searchFields, entityId = entityId, flatten = flatten, returnResponse = returnResponse)
	}

	#' Delete an InvoiceFixedLengthFileFormat
	#'
	#' This function deletes an InvoiceFixedLengthFileFormat
	#' @param InvoiceFixedLengthFileFormatID The ID of the InvoiceFixedLengthFileFormat to delete
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{listSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Accounts Receivable
	#' @return The InvoiceFixedLengthFileFormatID of the deleted InvoiceFixedLengthFileFormat.
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	deleteInvoiceFixedLengthFileFormat <- function(InvoiceFixedLengthFileFormatID, ignoreWarnings = F, entityId = 1, schoolYearId = NULL, flatten = T, returnResponse = F){

		deleteSkyObject(module = "AccountsReceivable", objectName = "InvoiceFixedLengthFileFormat", objectId = InvoiceFixedLengthFileFormatID, ignoreWarnings = ignoreWarnings, entityId = entityId, flatten = flatten, returnResponse = returnResponse)
	}

	#' Create an InvoiceFixedLengthFileFormat
	#'
	#' This function creates an InvoiceFixedLengthFileFormat
	#' @param fieldNames The field values to give the created InvoiceFixedLengthFileFormat. Each defaults to NULL.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{listSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Accounts Receivable
	#' @return A newly created InvoiceFixedLengthFileFormat
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	createInvoiceFixedLengthFileFormat <- function(InvoiceThirdPartyFormatID = NULL, NumberOfHeaderRows = NULL, InvoiceGroupStartPosition = NULL, InvoiceGroupLength = NULL, PayorStartPosition = NULL, PayorLength = NULL, BankAccountStartPosition = NULL, BankAccountLength = NULL, InvoiceDescriptionStartPosition = NULL, InvoiceDescriptionLength = NULL, BatchStartPosition = NULL, BatchLength = NULL, InvoiceDateStartPosition = NULL, InvoiceDateLength = NULL, DueDateStartPosition = NULL, DueDateLength = NULL, ItemStartPosition = NULL, ItemLength = NULL, InvoiceDetailDescriptionStartPosition = NULL, InvoiceDetailDescriptionLength = NULL, QuantityStartPosition = NULL, QuantityLength = NULL, UnitOfMeasureStartPosition = NULL, UnitOfMeasureLength = NULL, UnitCostStartPosition = NULL, UnitCostLength = NULL, DiscountStartPosition = NULL, DiscountLength = NULL, AccountStartPosition = NULL, AccountLength = NULL, PurchaseOrderNumberStartPosition = NULL, PurchaseOrderNumberLength = NULL, entityId = 1, schoolYearId = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment())

		body <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper()) %>% compact()

		createSkyObject(module = "AccountsReceivable", objectName = "InvoiceFixedLengthFileFormat", body = list(DataObject = body), searchFields = append("InvoiceFixedLengthFileFormatID", body %>% names()), entityId = entityId, schoolYearId = schoolYearId, flatten = flatten, returnResponse = returnResponse)
	}

	#' Modify an InvoiceFixedLengthFileFormat
	#'
	#' This function modifies an InvoiceFixedLengthFileFormat
	#' @param fieldNames The field values to give the modified InvoiceFixedLengthFileFormat. Each defaults to NULL.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{listSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Accounts Receivable
	#' @return The modified InvoiceFixedLengthFileFormat
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	modifyInvoiceFixedLengthFileFormat <- function(InvoiceFixedLengthFileFormatID, InvoiceThirdPartyFormatID = NULL, NumberOfHeaderRows = NULL, InvoiceGroupStartPosition = NULL, InvoiceGroupLength = NULL, PayorStartPosition = NULL, PayorLength = NULL, BankAccountStartPosition = NULL, BankAccountLength = NULL, InvoiceDescriptionStartPosition = NULL, InvoiceDescriptionLength = NULL, BatchStartPosition = NULL, BatchLength = NULL, InvoiceDateStartPosition = NULL, InvoiceDateLength = NULL, DueDateStartPosition = NULL, DueDateLength = NULL, ItemStartPosition = NULL, ItemLength = NULL, InvoiceDetailDescriptionStartPosition = NULL, InvoiceDetailDescriptionLength = NULL, QuantityStartPosition = NULL, QuantityLength = NULL, UnitOfMeasureStartPosition = NULL, UnitOfMeasureLength = NULL, UnitCostStartPosition = NULL, UnitCostLength = NULL, DiscountStartPosition = NULL, DiscountLength = NULL, AccountStartPosition = NULL, AccountLength = NULL, PurchaseOrderNumberStartPosition = NULL, PurchaseOrderNumberLength = NULL, entityId = 1, schoolYearId = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment())

		body <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper()) %>% compact()

		modifySkyObject(module = "AccountsReceivable", objectName = "InvoiceFixedLengthFileFormat", objectId = InvoiceFixedLengthFileFormatID, body = list(DataObject = body), searchFields = append("InvoiceFixedLengthFileFormatID", body %>% names()), entityId = entityId, schoolYearId = schoolYearId, flatten = flatten, returnResponse = returnResponse)
	}

	#' List AccountsReceivableTempExceptions
	#'
	#' This function returns a dataframe or json object of AccountsReceivableTempExceptions
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given AccountsReceivableTempExceptions. Defaults to FALSE for all return fields which, for convenience, returns all fields for the AccountsReceivableTempExceptions.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object listed as text. Run \code{\link{getSchemaForObjects}}('AccountsReceivableTempException') to get more field paths.
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
	#' @concept Accounts Receivable
	#' @return A list of AccountsReceivableTempExceptions
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	listAccountsReceivableTempExceptions <- function(searchConditionsList = NULL, TempExceptionID = F, Message = F, LineNumber = F, IsFatal = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, ParentObjectID = F, fieldPaths = NULL, searchConditionsGroupType = "And", searchSortFieldNamesList = NULL, searchSortFieldNamesDescendingList = NULL, entityId = 1, schoolYearId = NULL, page = 1, pageSize = 100000, flatten = T, returnResponse = F){

		params <- as.list(environment())

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		listSkyObjects(module = "AccountsReceivable", objectName = "TempException", searchFields = searchFields %>% append(fieldPaths), page = page, pageSize = pageSize, SearchConditionsList = searchConditionsList, SearchConditionsGroupType = searchConditionsGroupType, SearchSortFieldNamesList = searchSortFieldNamesList, SearchSortFieldNamesDescendingList = searchSortFieldNamesDescendingList, entityId = entityId, schoolYearId = schoolYearId, flatten = flatten, returnResponse = returnResponse)
	}

	#' Get an AccountsReceivableTempException
	#'
	#' This function returns a dataframe or json object of an AccountsReceivableTempException
	#' @param AccountsReceivableTempExceptionID The ID of the AccountsReceivableTempException to return.
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given AccountsReceivableTempException. Defaults to FALSE for all return fields which, for convenience, returns all fields for the AccountsReceivableTempException.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object listed as text. Run \code{\link{getSchemaForObjects}}('AccountsReceivableTempException') to get more field paths.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{listSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Accounts Receivable
	#' @return A dataframe or of AccountsReceivableTempException
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	getAccountsReceivableTempException <- function(AccountsReceivableTempExceptionID, TempExceptionID = F, Message = F, LineNumber = F, IsFatal = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, ParentObjectID = F, fieldPaths = NULL, entityId = 1, schoolYearId = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment()) %>% purrr::keep(names(.) != "AccountsReceivableTempExceptionID")

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		getSkyObject(module = "AccountsReceivable", objectName = "TempException", objectId = AccountsReceivableTempExceptionID, searchFields = searchFields, entityId = entityId, flatten = flatten, returnResponse = returnResponse)
	}

	#' Delete an AccountsReceivableTempException
	#'
	#' This function deletes an AccountsReceivableTempException
	#' @param AccountsReceivableTempExceptionID The ID of the AccountsReceivableTempException to delete
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{listSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Accounts Receivable
	#' @return The AccountsReceivableTempExceptionID of the deleted AccountsReceivableTempException.
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	deleteAccountsReceivableTempException <- function(AccountsReceivableTempExceptionID, ignoreWarnings = F, entityId = 1, schoolYearId = NULL, flatten = T, returnResponse = F){

		deleteSkyObject(module = "AccountsReceivable", objectName = "TempException", objectId = AccountsReceivableTempExceptionID, ignoreWarnings = ignoreWarnings, entityId = entityId, flatten = flatten, returnResponse = returnResponse)
	}

	#' Create an AccountsReceivableTempException
	#'
	#' This function creates an AccountsReceivableTempException
	#' @param fieldNames The field values to give the created AccountsReceivableTempException. Each defaults to NULL.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{listSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Accounts Receivable
	#' @return A newly created AccountsReceivableTempException
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	createAccountsReceivableTempException <- function(Message = NULL, LineNumber = NULL, IsFatal = NULL, ParentObjectID = NULL, entityId = 1, schoolYearId = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment())

		body <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper()) %>% compact()

		createSkyObject(module = "AccountsReceivable", objectName = "TempException", body = list(DataObject = body), searchFields = append("TempExceptionID", body %>% names()), entityId = entityId, schoolYearId = schoolYearId, flatten = flatten, returnResponse = returnResponse)
	}

	#' Modify an AccountsReceivableTempException
	#'
	#' This function modifies an AccountsReceivableTempException
	#' @param fieldNames The field values to give the modified AccountsReceivableTempException. Each defaults to NULL.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{listSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Accounts Receivable
	#' @return The modified AccountsReceivableTempException
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	modifyAccountsReceivableTempException <- function(TempExceptionID, Message = NULL, LineNumber = NULL, IsFatal = NULL, ParentObjectID = NULL, entityId = 1, schoolYearId = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment())

		body <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper()) %>% compact()

		modifySkyObject(module = "AccountsReceivable", objectName = "TempException", objectId = TempExceptionID, body = list(DataObject = body), searchFields = append("TempExceptionID", body %>% names()), entityId = entityId, schoolYearId = schoolYearId, flatten = flatten, returnResponse = returnResponse)
	}

	#' List AccountsReceivableTempInvoiceDetails
	#'
	#' This function returns a dataframe or json object of AccountsReceivableTempInvoiceDetails
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given AccountsReceivableTempInvoiceDetails. Defaults to FALSE for all return fields which, for convenience, returns all fields for the AccountsReceivableTempInvoiceDetails.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object listed as text. Run \code{\link{getSchemaForObjects}}('AccountsReceivableTempInvoiceDetail') to get more field paths.
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
	#' @concept Accounts Receivable
	#' @return A list of AccountsReceivableTempInvoiceDetails
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	listAccountsReceivableTempInvoiceDetails <- function(searchConditionsList = NULL, TempInvoiceDetailID = F, TempInvoiceID = F, ItemID = F, ItemCodeDescription = F, AccountID = F, AccountDistribution = F, Description = F, Quantity = F, UnitCost = F, BaseCurrencyAmount = F, DiscountID = F, DiscountCodeDescription = F, UnitOfMeasureID = F, UnitOfMeasureCodeDescription = F, LineNumber = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, PurchaseOrderNumber = F, fieldPaths = NULL, searchConditionsGroupType = "And", searchSortFieldNamesList = NULL, searchSortFieldNamesDescendingList = NULL, entityId = 1, schoolYearId = NULL, page = 1, pageSize = 100000, flatten = T, returnResponse = F){

		params <- as.list(environment())

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		listSkyObjects(module = "AccountsReceivable", objectName = "TempInvoiceDetail", searchFields = searchFields %>% append(fieldPaths), page = page, pageSize = pageSize, SearchConditionsList = searchConditionsList, SearchConditionsGroupType = searchConditionsGroupType, SearchSortFieldNamesList = searchSortFieldNamesList, SearchSortFieldNamesDescendingList = searchSortFieldNamesDescendingList, entityId = entityId, schoolYearId = schoolYearId, flatten = flatten, returnResponse = returnResponse)
	}

	#' Get an AccountsReceivableTempInvoiceDetail
	#'
	#' This function returns a dataframe or json object of an AccountsReceivableTempInvoiceDetail
	#' @param AccountsReceivableTempInvoiceDetailID The ID of the AccountsReceivableTempInvoiceDetail to return.
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given AccountsReceivableTempInvoiceDetail. Defaults to FALSE for all return fields which, for convenience, returns all fields for the AccountsReceivableTempInvoiceDetail.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object listed as text. Run \code{\link{getSchemaForObjects}}('AccountsReceivableTempInvoiceDetail') to get more field paths.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{listSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Accounts Receivable
	#' @return A dataframe or of AccountsReceivableTempInvoiceDetail
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	getAccountsReceivableTempInvoiceDetail <- function(AccountsReceivableTempInvoiceDetailID, TempInvoiceDetailID = F, TempInvoiceID = F, ItemID = F, ItemCodeDescription = F, AccountID = F, AccountDistribution = F, Description = F, Quantity = F, UnitCost = F, BaseCurrencyAmount = F, DiscountID = F, DiscountCodeDescription = F, UnitOfMeasureID = F, UnitOfMeasureCodeDescription = F, LineNumber = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, PurchaseOrderNumber = F, fieldPaths = NULL, entityId = 1, schoolYearId = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment()) %>% purrr::keep(names(.) != "AccountsReceivableTempInvoiceDetailID")

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		getSkyObject(module = "AccountsReceivable", objectName = "TempInvoiceDetail", objectId = AccountsReceivableTempInvoiceDetailID, searchFields = searchFields, entityId = entityId, flatten = flatten, returnResponse = returnResponse)
	}

	#' Delete an AccountsReceivableTempInvoiceDetail
	#'
	#' This function deletes an AccountsReceivableTempInvoiceDetail
	#' @param AccountsReceivableTempInvoiceDetailID The ID of the AccountsReceivableTempInvoiceDetail to delete
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{listSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Accounts Receivable
	#' @return The AccountsReceivableTempInvoiceDetailID of the deleted AccountsReceivableTempInvoiceDetail.
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	deleteAccountsReceivableTempInvoiceDetail <- function(AccountsReceivableTempInvoiceDetailID, ignoreWarnings = F, entityId = 1, schoolYearId = NULL, flatten = T, returnResponse = F){

		deleteSkyObject(module = "AccountsReceivable", objectName = "TempInvoiceDetail", objectId = AccountsReceivableTempInvoiceDetailID, ignoreWarnings = ignoreWarnings, entityId = entityId, flatten = flatten, returnResponse = returnResponse)
	}

	#' Create an AccountsReceivableTempInvoiceDetail
	#'
	#' This function creates an AccountsReceivableTempInvoiceDetail
	#' @param fieldNames The field values to give the created AccountsReceivableTempInvoiceDetail. Each defaults to NULL.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{listSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Accounts Receivable
	#' @return A newly created AccountsReceivableTempInvoiceDetail
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	createAccountsReceivableTempInvoiceDetail <- function(TempInvoiceID = NULL, ItemID = NULL, ItemCodeDescription = NULL, AccountID = NULL, AccountDistribution = NULL, Description = NULL, Quantity = NULL, UnitCost = NULL, BaseCurrencyAmount = NULL, DiscountID = NULL, DiscountCodeDescription = NULL, UnitOfMeasureID = NULL, UnitOfMeasureCodeDescription = NULL, LineNumber = NULL, PurchaseOrderNumber = NULL, entityId = 1, schoolYearId = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment())

		body <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper()) %>% compact()

		createSkyObject(module = "AccountsReceivable", objectName = "TempInvoiceDetail", body = list(DataObject = body), searchFields = append("TempInvoiceDetailID", body %>% names()), entityId = entityId, schoolYearId = schoolYearId, flatten = flatten, returnResponse = returnResponse)
	}

	#' Modify an AccountsReceivableTempInvoiceDetail
	#'
	#' This function modifies an AccountsReceivableTempInvoiceDetail
	#' @param fieldNames The field values to give the modified AccountsReceivableTempInvoiceDetail. Each defaults to NULL.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{listSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Accounts Receivable
	#' @return The modified AccountsReceivableTempInvoiceDetail
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	modifyAccountsReceivableTempInvoiceDetail <- function(TempInvoiceDetailID, TempInvoiceID = NULL, ItemID = NULL, ItemCodeDescription = NULL, AccountID = NULL, AccountDistribution = NULL, Description = NULL, Quantity = NULL, UnitCost = NULL, BaseCurrencyAmount = NULL, DiscountID = NULL, DiscountCodeDescription = NULL, UnitOfMeasureID = NULL, UnitOfMeasureCodeDescription = NULL, LineNumber = NULL, PurchaseOrderNumber = NULL, entityId = 1, schoolYearId = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment())

		body <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper()) %>% compact()

		modifySkyObject(module = "AccountsReceivable", objectName = "TempInvoiceDetail", objectId = TempInvoiceDetailID, body = list(DataObject = body), searchFields = append("TempInvoiceDetailID", body %>% names()), entityId = entityId, schoolYearId = schoolYearId, flatten = flatten, returnResponse = returnResponse)
	}

	#' List ConfigDistrictEmailTypes
	#'
	#' This function returns a dataframe or json object of ConfigDistrictEmailTypes
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given ConfigDistrictEmailTypes. Defaults to FALSE for all return fields which, for convenience, returns all fields for the ConfigDistrictEmailTypes.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object listed as text. Run \code{\link{getSchemaForObjects}}('ConfigDistrictEmailType') to get more field paths.
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
	#' @concept Accounts Receivable
	#' @return A list of ConfigDistrictEmailTypes
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	listConfigDistrictEmailTypes <- function(searchConditionsList = NULL, ConfigDistrictEmailTypeID = F, EmailTypeID = F, ConfigDistrictID = F, Rank = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, fieldPaths = NULL, searchConditionsGroupType = "And", searchSortFieldNamesList = NULL, searchSortFieldNamesDescendingList = NULL, entityId = 1, schoolYearId = NULL, page = 1, pageSize = 100000, flatten = T, returnResponse = F){

		params <- as.list(environment())

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		listSkyObjects(module = "AccountsReceivable", objectName = "ConfigDistrictEmailType", searchFields = searchFields %>% append(fieldPaths), page = page, pageSize = pageSize, SearchConditionsList = searchConditionsList, SearchConditionsGroupType = searchConditionsGroupType, SearchSortFieldNamesList = searchSortFieldNamesList, SearchSortFieldNamesDescendingList = searchSortFieldNamesDescendingList, entityId = entityId, schoolYearId = schoolYearId, flatten = flatten, returnResponse = returnResponse)
	}

	#' Get a ConfigDistrictEmailType
	#'
	#' This function returns a dataframe or json object of a ConfigDistrictEmailType
	#' @param ConfigDistrictEmailTypeID The ID of the ConfigDistrictEmailType to return.
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given ConfigDistrictEmailType. Defaults to FALSE for all return fields which, for convenience, returns all fields for the ConfigDistrictEmailType.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object listed as text. Run \code{\link{getSchemaForObjects}}('ConfigDistrictEmailType') to get more field paths.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{listSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Accounts Receivable
	#' @return A dataframe or of ConfigDistrictEmailType
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	getConfigDistrictEmailType <- function(ConfigDistrictEmailTypeID, EmailTypeID = F, ConfigDistrictID = F, Rank = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, fieldPaths = NULL, entityId = 1, schoolYearId = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment()) %>% purrr::keep(names(.) != "ConfigDistrictEmailTypeID")

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		getSkyObject(module = "AccountsReceivable", objectName = "ConfigDistrictEmailType", objectId = ConfigDistrictEmailTypeID, searchFields = searchFields, entityId = entityId, flatten = flatten, returnResponse = returnResponse)
	}

	#' Delete a ConfigDistrictEmailType
	#'
	#' This function deletes a ConfigDistrictEmailType
	#' @param ConfigDistrictEmailTypeID The ID of the ConfigDistrictEmailType to delete
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{listSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Accounts Receivable
	#' @return The ConfigDistrictEmailTypeID of the deleted ConfigDistrictEmailType.
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	deleteConfigDistrictEmailType <- function(ConfigDistrictEmailTypeID, ignoreWarnings = F, entityId = 1, schoolYearId = NULL, flatten = T, returnResponse = F){

		deleteSkyObject(module = "AccountsReceivable", objectName = "ConfigDistrictEmailType", objectId = ConfigDistrictEmailTypeID, ignoreWarnings = ignoreWarnings, entityId = entityId, flatten = flatten, returnResponse = returnResponse)
	}

	#' Create a ConfigDistrictEmailType
	#'
	#' This function creates a ConfigDistrictEmailType
	#' @param fieldNames The field values to give the created ConfigDistrictEmailType. Each defaults to NULL.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{listSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Accounts Receivable
	#' @return A newly created ConfigDistrictEmailType
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	createConfigDistrictEmailType <- function(EmailTypeID = NULL, ConfigDistrictID = NULL, Rank = NULL, entityId = 1, schoolYearId = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment())

		body <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper()) %>% compact()

		createSkyObject(module = "AccountsReceivable", objectName = "ConfigDistrictEmailType", body = list(DataObject = body), searchFields = append("ConfigDistrictEmailTypeID", body %>% names()), entityId = entityId, schoolYearId = schoolYearId, flatten = flatten, returnResponse = returnResponse)
	}

	#' Modify a ConfigDistrictEmailType
	#'
	#' This function modifies a ConfigDistrictEmailType
	#' @param fieldNames The field values to give the modified ConfigDistrictEmailType. Each defaults to NULL.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{listSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Accounts Receivable
	#' @return The modified ConfigDistrictEmailType
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	modifyConfigDistrictEmailType <- function(ConfigDistrictEmailTypeID, EmailTypeID = NULL, ConfigDistrictID = NULL, Rank = NULL, entityId = 1, schoolYearId = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment())

		body <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper()) %>% compact()

		modifySkyObject(module = "AccountsReceivable", objectName = "ConfigDistrictEmailType", objectId = ConfigDistrictEmailTypeID, body = list(DataObject = body), searchFields = append("ConfigDistrictEmailTypeID", body %>% names()), entityId = entityId, schoolYearId = schoolYearId, flatten = flatten, returnResponse = returnResponse)
	}

	#' List DeliveryExceptions
	#'
	#' This function returns a dataframe or json object of DeliveryExceptions
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given DeliveryExceptions. Defaults to FALSE for all return fields which, for convenience, returns all fields for the DeliveryExceptions.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object listed as text. Run \code{\link{getSchemaForObjects}}('DeliveryException') to get more field paths.
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
	#' @concept Accounts Receivable
	#' @return A list of DeliveryExceptions
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	listDeliveryExceptions <- function(searchConditionsList = NULL, DeliveryExceptionID = F, InvoiceDeliveryID = F, StatementDeliveryID = F, Message = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, fieldPaths = NULL, searchConditionsGroupType = "And", searchSortFieldNamesList = NULL, searchSortFieldNamesDescendingList = NULL, entityId = 1, schoolYearId = NULL, page = 1, pageSize = 100000, flatten = T, returnResponse = F){

		params <- as.list(environment())

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		listSkyObjects(module = "AccountsReceivable", objectName = "DeliveryException", searchFields = searchFields %>% append(fieldPaths), page = page, pageSize = pageSize, SearchConditionsList = searchConditionsList, SearchConditionsGroupType = searchConditionsGroupType, SearchSortFieldNamesList = searchSortFieldNamesList, SearchSortFieldNamesDescendingList = searchSortFieldNamesDescendingList, entityId = entityId, schoolYearId = schoolYearId, flatten = flatten, returnResponse = returnResponse)
	}

	#' Get a DeliveryException
	#'
	#' This function returns a dataframe or json object of a DeliveryException
	#' @param DeliveryExceptionID The ID of the DeliveryException to return.
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given DeliveryException. Defaults to FALSE for all return fields which, for convenience, returns all fields for the DeliveryException.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object listed as text. Run \code{\link{getSchemaForObjects}}('DeliveryException') to get more field paths.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{listSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Accounts Receivable
	#' @return A dataframe or of DeliveryException
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	getDeliveryException <- function(DeliveryExceptionID, InvoiceDeliveryID = F, StatementDeliveryID = F, Message = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, fieldPaths = NULL, entityId = 1, schoolYearId = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment()) %>% purrr::keep(names(.) != "DeliveryExceptionID")

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		getSkyObject(module = "AccountsReceivable", objectName = "DeliveryException", objectId = DeliveryExceptionID, searchFields = searchFields, entityId = entityId, flatten = flatten, returnResponse = returnResponse)
	}

	#' Delete a DeliveryException
	#'
	#' This function deletes a DeliveryException
	#' @param DeliveryExceptionID The ID of the DeliveryException to delete
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{listSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Accounts Receivable
	#' @return The DeliveryExceptionID of the deleted DeliveryException.
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	deleteDeliveryException <- function(DeliveryExceptionID, ignoreWarnings = F, entityId = 1, schoolYearId = NULL, flatten = T, returnResponse = F){

		deleteSkyObject(module = "AccountsReceivable", objectName = "DeliveryException", objectId = DeliveryExceptionID, ignoreWarnings = ignoreWarnings, entityId = entityId, flatten = flatten, returnResponse = returnResponse)
	}

	#' Create a DeliveryException
	#'
	#' This function creates a DeliveryException
	#' @param fieldNames The field values to give the created DeliveryException. Each defaults to NULL.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{listSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Accounts Receivable
	#' @return A newly created DeliveryException
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	createDeliveryException <- function(InvoiceDeliveryID = NULL, StatementDeliveryID = NULL, Message = NULL, entityId = 1, schoolYearId = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment())

		body <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper()) %>% compact()

		createSkyObject(module = "AccountsReceivable", objectName = "DeliveryException", body = list(DataObject = body), searchFields = append("DeliveryExceptionID", body %>% names()), entityId = entityId, schoolYearId = schoolYearId, flatten = flatten, returnResponse = returnResponse)
	}

	#' Modify a DeliveryException
	#'
	#' This function modifies a DeliveryException
	#' @param fieldNames The field values to give the modified DeliveryException. Each defaults to NULL.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{listSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Accounts Receivable
	#' @return The modified DeliveryException
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	modifyDeliveryException <- function(DeliveryExceptionID, InvoiceDeliveryID = NULL, StatementDeliveryID = NULL, Message = NULL, entityId = 1, schoolYearId = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment())

		body <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper()) %>% compact()

		modifySkyObject(module = "AccountsReceivable", objectName = "DeliveryException", objectId = DeliveryExceptionID, body = list(DataObject = body), searchFields = append("DeliveryExceptionID", body %>% names()), entityId = entityId, schoolYearId = schoolYearId, flatten = flatten, returnResponse = returnResponse)
	}

	#' List InvoiceDeliveries
	#'
	#' This function returns a dataframe or json object of InvoiceDeliveries
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given InvoiceDeliveries. Defaults to FALSE for all return fields which, for convenience, returns all fields for the InvoiceDeliveries.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object listed as text. Run \code{\link{getSchemaForObjects}}('InvoiceDelivery') to get more field paths.
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
	#' @concept Accounts Receivable
	#' @return A list of InvoiceDeliveries
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	listInvoiceDeliveries <- function(searchConditionsList = NULL, InvoiceDeliveryID = F, InvoiceID = F, UserIDSentBy = F, MediaID = F, SentTime = F, DeliveryType = F, Status = F, EmailRecipient = F, EmailSendingAddress = F, EmailSendingAlias = F, EmailBody = F, EmailSubject = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, fieldPaths = NULL, searchConditionsGroupType = "And", searchSortFieldNamesList = NULL, searchSortFieldNamesDescendingList = NULL, entityId = 1, schoolYearId = NULL, page = 1, pageSize = 100000, flatten = T, returnResponse = F){

		params <- as.list(environment())

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		listSkyObjects(module = "AccountsReceivable", objectName = "InvoiceDelivery", searchFields = searchFields %>% append(fieldPaths), page = page, pageSize = pageSize, SearchConditionsList = searchConditionsList, SearchConditionsGroupType = searchConditionsGroupType, SearchSortFieldNamesList = searchSortFieldNamesList, SearchSortFieldNamesDescendingList = searchSortFieldNamesDescendingList, entityId = entityId, schoolYearId = schoolYearId, flatten = flatten, returnResponse = returnResponse)
	}

	#' Get an InvoiceDelivery
	#'
	#' This function returns a dataframe or json object of an InvoiceDelivery
	#' @param InvoiceDeliveryID The ID of the InvoiceDelivery to return.
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given InvoiceDelivery. Defaults to FALSE for all return fields which, for convenience, returns all fields for the InvoiceDelivery.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object listed as text. Run \code{\link{getSchemaForObjects}}('InvoiceDelivery') to get more field paths.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{listSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Accounts Receivable
	#' @return A dataframe or of InvoiceDelivery
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	getInvoiceDelivery <- function(InvoiceDeliveryID, InvoiceID = F, UserIDSentBy = F, MediaID = F, SentTime = F, DeliveryType = F, Status = F, EmailRecipient = F, EmailSendingAddress = F, EmailSendingAlias = F, EmailBody = F, EmailSubject = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, fieldPaths = NULL, entityId = 1, schoolYearId = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment()) %>% purrr::keep(names(.) != "InvoiceDeliveryID")

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		getSkyObject(module = "AccountsReceivable", objectName = "InvoiceDelivery", objectId = InvoiceDeliveryID, searchFields = searchFields, entityId = entityId, flatten = flatten, returnResponse = returnResponse)
	}

	#' Delete an InvoiceDelivery
	#'
	#' This function deletes an InvoiceDelivery
	#' @param InvoiceDeliveryID The ID of the InvoiceDelivery to delete
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{listSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Accounts Receivable
	#' @return The InvoiceDeliveryID of the deleted InvoiceDelivery.
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	deleteInvoiceDelivery <- function(InvoiceDeliveryID, ignoreWarnings = F, entityId = 1, schoolYearId = NULL, flatten = T, returnResponse = F){

		deleteSkyObject(module = "AccountsReceivable", objectName = "InvoiceDelivery", objectId = InvoiceDeliveryID, ignoreWarnings = ignoreWarnings, entityId = entityId, flatten = flatten, returnResponse = returnResponse)
	}

	#' Create an InvoiceDelivery
	#'
	#' This function creates an InvoiceDelivery
	#' @param fieldNames The field values to give the created InvoiceDelivery. Each defaults to NULL.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{listSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Accounts Receivable
	#' @return A newly created InvoiceDelivery
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	createInvoiceDelivery <- function(InvoiceID = NULL, UserIDSentBy = NULL, MediaID = NULL, SentTime = NULL, DeliveryType = NULL, Status = NULL, EmailRecipient = NULL, EmailSendingAddress = NULL, EmailSendingAlias = NULL, EmailBody = NULL, EmailSubject = NULL, entityId = 1, schoolYearId = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment())

		body <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper()) %>% compact()

		createSkyObject(module = "AccountsReceivable", objectName = "InvoiceDelivery", body = list(DataObject = body), searchFields = append("InvoiceDeliveryID", body %>% names()), entityId = entityId, schoolYearId = schoolYearId, flatten = flatten, returnResponse = returnResponse)
	}

	#' Modify an InvoiceDelivery
	#'
	#' This function modifies an InvoiceDelivery
	#' @param fieldNames The field values to give the modified InvoiceDelivery. Each defaults to NULL.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{listSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Accounts Receivable
	#' @return The modified InvoiceDelivery
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	modifyInvoiceDelivery <- function(InvoiceDeliveryID, InvoiceID = NULL, UserIDSentBy = NULL, MediaID = NULL, SentTime = NULL, DeliveryType = NULL, Status = NULL, EmailRecipient = NULL, EmailSendingAddress = NULL, EmailSendingAlias = NULL, EmailBody = NULL, EmailSubject = NULL, entityId = 1, schoolYearId = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment())

		body <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper()) %>% compact()

		modifySkyObject(module = "AccountsReceivable", objectName = "InvoiceDelivery", objectId = InvoiceDeliveryID, body = list(DataObject = body), searchFields = append("InvoiceDeliveryID", body %>% names()), entityId = entityId, schoolYearId = schoolYearId, flatten = flatten, returnResponse = returnResponse)
	}

	#' List StatementDeliveries
	#'
	#' This function returns a dataframe or json object of StatementDeliveries
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given StatementDeliveries. Defaults to FALSE for all return fields which, for convenience, returns all fields for the StatementDeliveries.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object listed as text. Run \code{\link{getSchemaForObjects}}('StatementDelivery') to get more field paths.
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
	#' @concept Accounts Receivable
	#' @return A list of StatementDeliveries
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	listStatementDeliveries <- function(searchConditionsList = NULL, StatementDeliveryID = F, PayorID = F, UserIDSentBy = F, MediaID = F, SentTime = F, DeliveryType = F, Status = F, EmailRecipient = F, EmailSendingAddress = F, EmailSendingAlias = F, EmailBody = F, EmailSubject = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, fieldPaths = NULL, searchConditionsGroupType = "And", searchSortFieldNamesList = NULL, searchSortFieldNamesDescendingList = NULL, entityId = 1, schoolYearId = NULL, page = 1, pageSize = 100000, flatten = T, returnResponse = F){

		params <- as.list(environment())

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		listSkyObjects(module = "AccountsReceivable", objectName = "StatementDelivery", searchFields = searchFields %>% append(fieldPaths), page = page, pageSize = pageSize, SearchConditionsList = searchConditionsList, SearchConditionsGroupType = searchConditionsGroupType, SearchSortFieldNamesList = searchSortFieldNamesList, SearchSortFieldNamesDescendingList = searchSortFieldNamesDescendingList, entityId = entityId, schoolYearId = schoolYearId, flatten = flatten, returnResponse = returnResponse)
	}

	#' Get a StatementDelivery
	#'
	#' This function returns a dataframe or json object of a StatementDelivery
	#' @param StatementDeliveryID The ID of the StatementDelivery to return.
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given StatementDelivery. Defaults to FALSE for all return fields which, for convenience, returns all fields for the StatementDelivery.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object listed as text. Run \code{\link{getSchemaForObjects}}('StatementDelivery') to get more field paths.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{listSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Accounts Receivable
	#' @return A dataframe or of StatementDelivery
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	getStatementDelivery <- function(StatementDeliveryID, PayorID = F, UserIDSentBy = F, MediaID = F, SentTime = F, DeliveryType = F, Status = F, EmailRecipient = F, EmailSendingAddress = F, EmailSendingAlias = F, EmailBody = F, EmailSubject = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, fieldPaths = NULL, entityId = 1, schoolYearId = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment()) %>% purrr::keep(names(.) != "StatementDeliveryID")

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		getSkyObject(module = "AccountsReceivable", objectName = "StatementDelivery", objectId = StatementDeliveryID, searchFields = searchFields, entityId = entityId, flatten = flatten, returnResponse = returnResponse)
	}

	#' Delete a StatementDelivery
	#'
	#' This function deletes a StatementDelivery
	#' @param StatementDeliveryID The ID of the StatementDelivery to delete
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{listSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Accounts Receivable
	#' @return The StatementDeliveryID of the deleted StatementDelivery.
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	deleteStatementDelivery <- function(StatementDeliveryID, ignoreWarnings = F, entityId = 1, schoolYearId = NULL, flatten = T, returnResponse = F){

		deleteSkyObject(module = "AccountsReceivable", objectName = "StatementDelivery", objectId = StatementDeliveryID, ignoreWarnings = ignoreWarnings, entityId = entityId, flatten = flatten, returnResponse = returnResponse)
	}

	#' Create a StatementDelivery
	#'
	#' This function creates a StatementDelivery
	#' @param fieldNames The field values to give the created StatementDelivery. Each defaults to NULL.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{listSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Accounts Receivable
	#' @return A newly created StatementDelivery
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	createStatementDelivery <- function(PayorID = NULL, UserIDSentBy = NULL, MediaID = NULL, SentTime = NULL, DeliveryType = NULL, Status = NULL, EmailRecipient = NULL, EmailSendingAddress = NULL, EmailSendingAlias = NULL, EmailBody = NULL, EmailSubject = NULL, entityId = 1, schoolYearId = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment())

		body <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper()) %>% compact()

		createSkyObject(module = "AccountsReceivable", objectName = "StatementDelivery", body = list(DataObject = body), searchFields = append("StatementDeliveryID", body %>% names()), entityId = entityId, schoolYearId = schoolYearId, flatten = flatten, returnResponse = returnResponse)
	}

	#' Modify a StatementDelivery
	#'
	#' This function modifies a StatementDelivery
	#' @param fieldNames The field values to give the modified StatementDelivery. Each defaults to NULL.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{listSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Accounts Receivable
	#' @return The modified StatementDelivery
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	modifyStatementDelivery <- function(StatementDeliveryID, PayorID = NULL, UserIDSentBy = NULL, MediaID = NULL, SentTime = NULL, DeliveryType = NULL, Status = NULL, EmailRecipient = NULL, EmailSendingAddress = NULL, EmailSendingAlias = NULL, EmailBody = NULL, EmailSubject = NULL, entityId = 1, schoolYearId = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment())

		body <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper()) %>% compact()

		modifySkyObject(module = "AccountsReceivable", objectName = "StatementDelivery", objectId = StatementDeliveryID, body = list(DataObject = body), searchFields = append("StatementDeliveryID", body %>% names()), entityId = entityId, schoolYearId = schoolYearId, flatten = flatten, returnResponse = returnResponse)
	}

	#' List TempPayors
	#'
	#' This function returns a dataframe or json object of TempPayors
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given TempPayors. Defaults to FALSE for all return fields which, for convenience, returns all fields for the TempPayors.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object listed as text. Run \code{\link{getSchemaForObjects}}('TempPayor') to get more field paths.
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
	#' @concept Accounts Receivable
	#' @return A list of TempPayors
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	listTempPayors <- function(searchConditionsList = NULL, TempPayorID = F, PayorID = F, LastName = F, FirstName = F, MiddleName = F, QuickKey = F, AmountDue = F, UnappliedAmount = F, AccountsReceivableContact = F, MailingAddress = F, DeliveryType = F, HasErrors = F, ErrorCount = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, fieldPaths = NULL, searchConditionsGroupType = "And", searchSortFieldNamesList = NULL, searchSortFieldNamesDescendingList = NULL, entityId = 1, schoolYearId = NULL, page = 1, pageSize = 100000, flatten = T, returnResponse = F){

		params <- as.list(environment())

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		listSkyObjects(module = "AccountsReceivable", objectName = "TempPayor", searchFields = searchFields %>% append(fieldPaths), page = page, pageSize = pageSize, SearchConditionsList = searchConditionsList, SearchConditionsGroupType = searchConditionsGroupType, SearchSortFieldNamesList = searchSortFieldNamesList, SearchSortFieldNamesDescendingList = searchSortFieldNamesDescendingList, entityId = entityId, schoolYearId = schoolYearId, flatten = flatten, returnResponse = returnResponse)
	}

	#' Get a TempPayor
	#'
	#' This function returns a dataframe or json object of a TempPayor
	#' @param TempPayorID The ID of the TempPayor to return.
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given TempPayor. Defaults to FALSE for all return fields which, for convenience, returns all fields for the TempPayor.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object listed as text. Run \code{\link{getSchemaForObjects}}('TempPayor') to get more field paths.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{listSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Accounts Receivable
	#' @return A dataframe or of TempPayor
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	getTempPayor <- function(TempPayorID, PayorID = F, LastName = F, FirstName = F, MiddleName = F, QuickKey = F, AmountDue = F, UnappliedAmount = F, AccountsReceivableContact = F, MailingAddress = F, DeliveryType = F, HasErrors = F, ErrorCount = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, fieldPaths = NULL, entityId = 1, schoolYearId = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment()) %>% purrr::keep(names(.) != "TempPayorID")

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		getSkyObject(module = "AccountsReceivable", objectName = "TempPayor", objectId = TempPayorID, searchFields = searchFields, entityId = entityId, flatten = flatten, returnResponse = returnResponse)
	}

	#' Delete a TempPayor
	#'
	#' This function deletes a TempPayor
	#' @param TempPayorID The ID of the TempPayor to delete
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{listSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Accounts Receivable
	#' @return The TempPayorID of the deleted TempPayor.
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	deleteTempPayor <- function(TempPayorID, ignoreWarnings = F, entityId = 1, schoolYearId = NULL, flatten = T, returnResponse = F){

		deleteSkyObject(module = "AccountsReceivable", objectName = "TempPayor", objectId = TempPayorID, ignoreWarnings = ignoreWarnings, entityId = entityId, flatten = flatten, returnResponse = returnResponse)
	}

	#' Create a TempPayor
	#'
	#' This function creates a TempPayor
	#' @param fieldNames The field values to give the created TempPayor. Each defaults to NULL.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{listSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Accounts Receivable
	#' @return A newly created TempPayor
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	createTempPayor <- function(PayorID = NULL, LastName = NULL, FirstName = NULL, MiddleName = NULL, QuickKey = NULL, AmountDue = NULL, UnappliedAmount = NULL, AccountsReceivableContact = NULL, MailingAddress = NULL, DeliveryType = NULL, HasErrors = NULL, ErrorCount = NULL, entityId = 1, schoolYearId = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment())

		body <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper()) %>% compact()

		createSkyObject(module = "AccountsReceivable", objectName = "TempPayor", body = list(DataObject = body), searchFields = append("TempPayorID", body %>% names()), entityId = entityId, schoolYearId = schoolYearId, flatten = flatten, returnResponse = returnResponse)
	}

	#' Modify a TempPayor
	#'
	#' This function modifies a TempPayor
	#' @param fieldNames The field values to give the modified TempPayor. Each defaults to NULL.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{listSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Accounts Receivable
	#' @return The modified TempPayor
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	modifyTempPayor <- function(TempPayorID, PayorID = NULL, LastName = NULL, FirstName = NULL, MiddleName = NULL, QuickKey = NULL, AmountDue = NULL, UnappliedAmount = NULL, AccountsReceivableContact = NULL, MailingAddress = NULL, DeliveryType = NULL, HasErrors = NULL, ErrorCount = NULL, entityId = 1, schoolYearId = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment())

		body <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper()) %>% compact()

		modifySkyObject(module = "AccountsReceivable", objectName = "TempPayor", objectId = TempPayorID, body = list(DataObject = body), searchFields = append("TempPayorID", body %>% names()), entityId = entityId, schoolYearId = schoolYearId, flatten = flatten, returnResponse = returnResponse)
	}
