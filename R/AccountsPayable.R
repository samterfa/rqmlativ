
	#' List AccountsPayableRuns
	#'
	#' This function returns a dataframe or json object of AccountsPayableRuns
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given AccountsPayableRuns. Defaults to FALSE for all return fields which, for convenience, returns all fields for the AccountsPayableRuns.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object listed as text. Run \code{\link{getSchemaForObjects}}('AccountsPayableRun') to get more field paths.
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
	#' @concept Accounts Payable
	#' @return A list of AccountsPayableRuns
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	listAccountsPayableRuns <- function(searchConditionsList = NULL, AccountsPayableRunID = F, DistrictID = F, BudgetaryPostDate = F, CheckDate = F, BankAccountID = F, Description = F, Status = F, PrintStatus = F, Type = F, AccountingUpdateID = F, WillCreateACH = F, TotalPayoutAmount = F, CheckTransactionCount = F, AttachmentCount = F, RenderHistoryUnprintedOption = F, RenderDelete = F, RenderSelectInvoices = F, RenderSelectInvoicesVoid = F, RenderCalculate = F, RenderAccountingRegister = F, RenderUpdate = F, RenderUpdateVoid = F, RenderAccountingRegisterVoid = F, RenderPurge = F, RenderPurgeVoid = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, PaymentDateFilter = F, StartingACHNumber = F, EndingACHNumber = F, StartingCheckNumber = F, EndingCheckNumber = F, CheckDateYear = F, fieldPaths = NULL, searchConditionsGroupType = "And", searchSortFieldNamesList = NULL, searchSortFieldNamesDescendingList = NULL, entityId = 1, schoolYearId = NULL, page = 1, pageSize = 100000, flatten = T, returnResponse = F){

		params <- as.list(environment())

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		listSkyObjects(module = "AccountsPayable", objectName = "AccountsPayableRun", searchFields = searchFields %>% append(fieldPaths), page = page, pageSize = pageSize, SearchConditionsList = searchConditionsList, SearchConditionsGroupType = searchConditionsGroupType, SearchSortFieldNamesList = searchSortFieldNamesList, SearchSortFieldNamesDescendingList = searchSortFieldNamesDescendingList, entityId = entityId, schoolYearId = schoolYearId, flatten = flatten, returnResponse = returnResponse)
	}

	#' Get an AccountsPayableRun
	#'
	#' This function returns a dataframe or json object of an AccountsPayableRun
	#' @param AccountsPayableRunID The ID of the AccountsPayableRun to return.
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given AccountsPayableRun. Defaults to FALSE for all return fields which, for convenience, returns all fields for the AccountsPayableRun.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object listed as text. Run \code{\link{getSchemaForObjects}}('AccountsPayableRun') to get more field paths.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{listSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Accounts Payable
	#' @return A dataframe or of AccountsPayableRun
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	getAccountsPayableRun <- function(AccountsPayableRunID, DistrictID = F, BudgetaryPostDate = F, CheckDate = F, BankAccountID = F, Description = F, Status = F, PrintStatus = F, Type = F, AccountingUpdateID = F, WillCreateACH = F, TotalPayoutAmount = F, CheckTransactionCount = F, AttachmentCount = F, RenderHistoryUnprintedOption = F, RenderDelete = F, RenderSelectInvoices = F, RenderSelectInvoicesVoid = F, RenderCalculate = F, RenderAccountingRegister = F, RenderUpdate = F, RenderUpdateVoid = F, RenderAccountingRegisterVoid = F, RenderPurge = F, RenderPurgeVoid = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, PaymentDateFilter = F, StartingACHNumber = F, EndingACHNumber = F, StartingCheckNumber = F, EndingCheckNumber = F, CheckDateYear = F, fieldPaths = NULL, entityId = 1, schoolYearId = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment()) %>% purrr::keep(names(.) != "AccountsPayableRunID")

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		getSkyObject(module = "AccountsPayable", objectName = "AccountsPayableRun", objectId = AccountsPayableRunID, searchFields = searchFields, entityId = entityId, flatten = flatten, returnResponse = returnResponse)
	}

	#' Delete an AccountsPayableRun
	#'
	#' This function deletes an AccountsPayableRun
	#' @param AccountsPayableRunID The ID of the AccountsPayableRun to delete
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{listSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Accounts Payable
	#' @return The AccountsPayableRunID of the deleted AccountsPayableRun.
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	deleteAccountsPayableRun <- function(AccountsPayableRunID, ignoreWarnings = F, entityId = 1, schoolYearId = NULL, flatten = T, returnResponse = F){

		deleteSkyObject(module = "AccountsPayable", objectName = "AccountsPayableRun", objectId = AccountsPayableRunID, ignoreWarnings = ignoreWarnings, entityId = entityId, flatten = flatten, returnResponse = returnResponse)
	}

	#' Create an AccountsPayableRun
	#'
	#' This function creates an AccountsPayableRun
	#' @param fieldNames The field values to give the created AccountsPayableRun. Each defaults to NULL.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{listSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Accounts Payable
	#' @return A newly created AccountsPayableRun
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	createAccountsPayableRun <- function(DistrictID = NULL, BudgetaryPostDate = NULL, CheckDate = NULL, BankAccountID = NULL, Description = NULL, Status = NULL, PrintStatus = NULL, Type = NULL, AccountingUpdateID = NULL, WillCreateACH = NULL, PaymentDateFilter = NULL, entityId = 1, schoolYearId = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment())

		body <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper()) %>% compact()

		createSkyObject(module = "AccountsPayable", objectName = "AccountsPayableRun", body = list(DataObject = body), searchFields = append("AccountsPayableRunID", body %>% names()), entityId = entityId, schoolYearId = schoolYearId, flatten = flatten, returnResponse = returnResponse)
	}

	#' Modify an AccountsPayableRun
	#'
	#' This function modifies an AccountsPayableRun
	#' @param fieldNames The field values to give the modified AccountsPayableRun. Each defaults to NULL.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{listSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Accounts Payable
	#' @return The modified AccountsPayableRun
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	modifyAccountsPayableRun <- function(AccountsPayableRunID, DistrictID = NULL, BudgetaryPostDate = NULL, CheckDate = NULL, BankAccountID = NULL, Description = NULL, Status = NULL, PrintStatus = NULL, Type = NULL, AccountingUpdateID = NULL, WillCreateACH = NULL, PaymentDateFilter = NULL, entityId = 1, schoolYearId = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment())

		body <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper()) %>% compact()

		modifySkyObject(module = "AccountsPayable", objectName = "AccountsPayableRun", objectId = AccountsPayableRunID, body = list(DataObject = body), searchFields = append("AccountsPayableRunID", body %>% names()), entityId = entityId, schoolYearId = schoolYearId, flatten = flatten, returnResponse = returnResponse)
	}

	#' List AccountsPayableRunExceptions
	#'
	#' This function returns a dataframe or json object of AccountsPayableRunExceptions
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given AccountsPayableRunExceptions. Defaults to FALSE for all return fields which, for convenience, returns all fields for the AccountsPayableRunExceptions.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object listed as text. Run \code{\link{getSchemaForObjects}}('AccountsPayableRunException') to get more field paths.
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
	#' @concept Accounts Payable
	#' @return A list of AccountsPayableRunExceptions
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	listAccountsPayableRunExceptions <- function(searchConditionsList = NULL, AccountsPayableRunExceptionID = F, AccountsPayableRunID = F, VendorID = F, Message = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, fieldPaths = NULL, searchConditionsGroupType = "And", searchSortFieldNamesList = NULL, searchSortFieldNamesDescendingList = NULL, entityId = 1, schoolYearId = NULL, page = 1, pageSize = 100000, flatten = T, returnResponse = F){

		params <- as.list(environment())

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		listSkyObjects(module = "AccountsPayable", objectName = "AccountsPayableRunException", searchFields = searchFields %>% append(fieldPaths), page = page, pageSize = pageSize, SearchConditionsList = searchConditionsList, SearchConditionsGroupType = searchConditionsGroupType, SearchSortFieldNamesList = searchSortFieldNamesList, SearchSortFieldNamesDescendingList = searchSortFieldNamesDescendingList, entityId = entityId, schoolYearId = schoolYearId, flatten = flatten, returnResponse = returnResponse)
	}

	#' Get an AccountsPayableRunException
	#'
	#' This function returns a dataframe or json object of an AccountsPayableRunException
	#' @param AccountsPayableRunExceptionID The ID of the AccountsPayableRunException to return.
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given AccountsPayableRunException. Defaults to FALSE for all return fields which, for convenience, returns all fields for the AccountsPayableRunException.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object listed as text. Run \code{\link{getSchemaForObjects}}('AccountsPayableRunException') to get more field paths.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{listSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Accounts Payable
	#' @return A dataframe or of AccountsPayableRunException
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	getAccountsPayableRunException <- function(AccountsPayableRunExceptionID, AccountsPayableRunID = F, VendorID = F, Message = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, fieldPaths = NULL, entityId = 1, schoolYearId = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment()) %>% purrr::keep(names(.) != "AccountsPayableRunExceptionID")

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		getSkyObject(module = "AccountsPayable", objectName = "AccountsPayableRunException", objectId = AccountsPayableRunExceptionID, searchFields = searchFields, entityId = entityId, flatten = flatten, returnResponse = returnResponse)
	}

	#' Delete an AccountsPayableRunException
	#'
	#' This function deletes an AccountsPayableRunException
	#' @param AccountsPayableRunExceptionID The ID of the AccountsPayableRunException to delete
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{listSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Accounts Payable
	#' @return The AccountsPayableRunExceptionID of the deleted AccountsPayableRunException.
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	deleteAccountsPayableRunException <- function(AccountsPayableRunExceptionID, ignoreWarnings = F, entityId = 1, schoolYearId = NULL, flatten = T, returnResponse = F){

		deleteSkyObject(module = "AccountsPayable", objectName = "AccountsPayableRunException", objectId = AccountsPayableRunExceptionID, ignoreWarnings = ignoreWarnings, entityId = entityId, flatten = flatten, returnResponse = returnResponse)
	}

	#' Create an AccountsPayableRunException
	#'
	#' This function creates an AccountsPayableRunException
	#' @param fieldNames The field values to give the created AccountsPayableRunException. Each defaults to NULL.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{listSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Accounts Payable
	#' @return A newly created AccountsPayableRunException
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	createAccountsPayableRunException <- function(AccountsPayableRunID = NULL, VendorID = NULL, Message = NULL, entityId = 1, schoolYearId = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment())

		body <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper()) %>% compact()

		createSkyObject(module = "AccountsPayable", objectName = "AccountsPayableRunException", body = list(DataObject = body), searchFields = append("AccountsPayableRunExceptionID", body %>% names()), entityId = entityId, schoolYearId = schoolYearId, flatten = flatten, returnResponse = returnResponse)
	}

	#' Modify an AccountsPayableRunException
	#'
	#' This function modifies an AccountsPayableRunException
	#' @param fieldNames The field values to give the modified AccountsPayableRunException. Each defaults to NULL.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{listSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Accounts Payable
	#' @return The modified AccountsPayableRunException
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	modifyAccountsPayableRunException <- function(AccountsPayableRunExceptionID, AccountsPayableRunID = NULL, VendorID = NULL, Message = NULL, entityId = 1, schoolYearId = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment())

		body <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper()) %>% compact()

		modifySkyObject(module = "AccountsPayable", objectName = "AccountsPayableRunException", objectId = AccountsPayableRunExceptionID, body = list(DataObject = body), searchFields = append("AccountsPayableRunExceptionID", body %>% names()), entityId = entityId, schoolYearId = schoolYearId, flatten = flatten, returnResponse = returnResponse)
	}

	#' List AccountsPayableCheckTransactions
	#'
	#' This function returns a dataframe or json object of AccountsPayableCheckTransactions
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given AccountsPayableCheckTransactions. Defaults to FALSE for all return fields which, for convenience, returns all fields for the AccountsPayableCheckTransactions.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object listed as text. Run \code{\link{getSchemaForObjects}}('AccountsPayableCheckTransaction') to get more field paths.
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
	#' @concept Accounts Payable
	#' @return A list of AccountsPayableCheckTransactions
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	listAccountsPayableCheckTransactions <- function(searchConditionsList = NULL, CheckTransactionID = F, AccountsPayableRunID = F, VendorID = F, NetCheckAmount = F, CheckTransactionIDVoidedFrom = F, CheckNumberOverride = F, CheckNumber = F, PaymentType = F, IsVoided = F, AttachmentCount = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, IsACHOnBankReconciliation = F, fieldPaths = NULL, searchConditionsGroupType = "And", searchSortFieldNamesList = NULL, searchSortFieldNamesDescendingList = NULL, entityId = 1, schoolYearId = NULL, page = 1, pageSize = 100000, flatten = T, returnResponse = F){

		params <- as.list(environment())

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		listSkyObjects(module = "AccountsPayable", objectName = "CheckTransaction", searchFields = searchFields %>% append(fieldPaths), page = page, pageSize = pageSize, SearchConditionsList = searchConditionsList, SearchConditionsGroupType = searchConditionsGroupType, SearchSortFieldNamesList = searchSortFieldNamesList, SearchSortFieldNamesDescendingList = searchSortFieldNamesDescendingList, entityId = entityId, schoolYearId = schoolYearId, flatten = flatten, returnResponse = returnResponse)
	}

	#' Get an AccountsPayableCheckTransaction
	#'
	#' This function returns a dataframe or json object of an AccountsPayableCheckTransaction
	#' @param AccountsPayableCheckTransactionID The ID of the AccountsPayableCheckTransaction to return.
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given AccountsPayableCheckTransaction. Defaults to FALSE for all return fields which, for convenience, returns all fields for the AccountsPayableCheckTransaction.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object listed as text. Run \code{\link{getSchemaForObjects}}('AccountsPayableCheckTransaction') to get more field paths.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{listSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Accounts Payable
	#' @return A dataframe or of AccountsPayableCheckTransaction
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	getAccountsPayableCheckTransaction <- function(AccountsPayableCheckTransactionID, CheckTransactionID = F, AccountsPayableRunID = F, VendorID = F, NetCheckAmount = F, CheckTransactionIDVoidedFrom = F, CheckNumberOverride = F, CheckNumber = F, PaymentType = F, IsVoided = F, AttachmentCount = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, IsACHOnBankReconciliation = F, fieldPaths = NULL, entityId = 1, schoolYearId = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment()) %>% purrr::keep(names(.) != "AccountsPayableCheckTransactionID")

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		getSkyObject(module = "AccountsPayable", objectName = "CheckTransaction", objectId = AccountsPayableCheckTransactionID, searchFields = searchFields, entityId = entityId, flatten = flatten, returnResponse = returnResponse)
	}

	#' Delete an AccountsPayableCheckTransaction
	#'
	#' This function deletes an AccountsPayableCheckTransaction
	#' @param AccountsPayableCheckTransactionID The ID of the AccountsPayableCheckTransaction to delete
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{listSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Accounts Payable
	#' @return The AccountsPayableCheckTransactionID of the deleted AccountsPayableCheckTransaction.
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	deleteAccountsPayableCheckTransaction <- function(AccountsPayableCheckTransactionID, ignoreWarnings = F, entityId = 1, schoolYearId = NULL, flatten = T, returnResponse = F){

		deleteSkyObject(module = "AccountsPayable", objectName = "CheckTransaction", objectId = AccountsPayableCheckTransactionID, ignoreWarnings = ignoreWarnings, entityId = entityId, flatten = flatten, returnResponse = returnResponse)
	}

	#' Create an AccountsPayableCheckTransaction
	#'
	#' This function creates an AccountsPayableCheckTransaction
	#' @param fieldNames The field values to give the created AccountsPayableCheckTransaction. Each defaults to NULL.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{listSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Accounts Payable
	#' @return A newly created AccountsPayableCheckTransaction
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	createAccountsPayableCheckTransaction <- function(AccountsPayableRunID = NULL, VendorID = NULL, NetCheckAmount = NULL, CheckTransactionIDVoidedFrom = NULL, CheckNumberOverride = NULL, PaymentType = NULL, entityId = 1, schoolYearId = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment())

		body <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper()) %>% compact()

		createSkyObject(module = "AccountsPayable", objectName = "CheckTransaction", body = list(DataObject = body), searchFields = append("CheckTransactionID", body %>% names()), entityId = entityId, schoolYearId = schoolYearId, flatten = flatten, returnResponse = returnResponse)
	}

	#' Modify an AccountsPayableCheckTransaction
	#'
	#' This function modifies an AccountsPayableCheckTransaction
	#' @param fieldNames The field values to give the modified AccountsPayableCheckTransaction. Each defaults to NULL.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{listSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Accounts Payable
	#' @return The modified AccountsPayableCheckTransaction
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	modifyAccountsPayableCheckTransaction <- function(CheckTransactionID, AccountsPayableRunID = NULL, VendorID = NULL, NetCheckAmount = NULL, CheckTransactionIDVoidedFrom = NULL, CheckNumberOverride = NULL, PaymentType = NULL, entityId = 1, schoolYearId = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment())

		body <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper()) %>% compact()

		modifySkyObject(module = "AccountsPayable", objectName = "CheckTransaction", objectId = CheckTransactionID, body = list(DataObject = body), searchFields = append("CheckTransactionID", body %>% names()), entityId = entityId, schoolYearId = schoolYearId, flatten = flatten, returnResponse = returnResponse)
	}

	#' List AccountsPayableConfigDistricts
	#'
	#' This function returns a dataframe or json object of AccountsPayableConfigDistricts
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given AccountsPayableConfigDistricts. Defaults to FALSE for all return fields which, for convenience, returns all fields for the AccountsPayableConfigDistricts.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object listed as text. Run \code{\link{getSchemaForObjects}}('AccountsPayableConfigDistrict') to get more field paths.
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
	#' @concept Accounts Payable
	#' @return A list of AccountsPayableConfigDistricts
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	listAccountsPayableConfigDistricts <- function(searchConditionsList = NULL, ConfigDistrictID = F, DistrictID = F, SendInvoiceApprovedMessage = F, InvoiceRequestApprovedMessageSubject = F, InvoiceRequestApprovedMessageContent = F, SendInvoiceDeniedMessage = F, InvoiceRequestDeniedMessageSubject = F, InvoiceRequestDeniedMessageContent = F, SendInvoiceWaitingMessage = F, InvoiceRequestWaitingMessageSubject = F, InvoiceRequestWaitingMessageContent = F, SendCreditCardTransactionPreApprovalApprovedMessage = F, SendCreditCardTransactionPostApprovalApprovedMessage = F, CreditCardTransactionRequestApprovedMessageSubject = F, CreditCardTransactionRequestApprovedMessageContent = F, SendCreditCardTransactionPreApprovalDeniedMessage = F, SendCreditCardTransactionPostApprovalDeniedMessage = F, CreditCardTransactionRequestDeniedMessageSubject = F, CreditCardTransactionRequestDeniedMessageContent = F, SendCreditCardTransactionPreApprovalWaitingMessage = F, SendCreditCardTransactionPostApprovalWaitingMessage = F, CreditCardTransactionRequestWaitingMessageSubject = F, CreditCardTransactionRequestWaitingMessageContent = F, MaskIDAccrual = F, UseAvailableFundWarning = F, UseAvailableFundError = F, PercentOverPurchaseOrder = F, AvailableFundWarningXMLFilter = F, SecurityStandardWarningFilterData = F, AvailableFundErrorXMLFilter = F, SecurityStandardErrorFilterData = F, BatchDefault = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, SendExpenseReimbursementPreApprovalApprovedMessage = F, SendExpenseReimbursementPostApprovalApprovedMessage = F, ExpenseReimbursementRequestApprovedMessageSubject = F, ExpenseReimbursementRequestApprovedMessageContent = F, SendExpenseReimbursementPreApprovalDeniedMessage = F, SendExpenseReimbursementPostApprovalDeniedMessage = F, ExpenseReimbursementRequestDeniedMessageSubject = F, ExpenseReimbursementRequestDeniedMessageContent = F, SendExpenseReimbursementPreApprovalWaitingMessage = F, SendExpenseReimbursementPostApprovalWaitingMessage = F, ExpenseReimbursementRequestWaitingMessageSubject = F, ExpenseReimbursementRequestWaitingMessageContent = F, OneFundPerCheck = F, DirectDepositNotificationSubject = F, DirectDepositNotificationContent = F, MaskIDAccrualActivityAccounting = F, fieldPaths = NULL, searchConditionsGroupType = "And", searchSortFieldNamesList = NULL, searchSortFieldNamesDescendingList = NULL, entityId = 1, schoolYearId = NULL, page = 1, pageSize = 100000, flatten = T, returnResponse = F){

		params <- as.list(environment())

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		listSkyObjects(module = "AccountsPayable", objectName = "ConfigDistrict", searchFields = searchFields %>% append(fieldPaths), page = page, pageSize = pageSize, SearchConditionsList = searchConditionsList, SearchConditionsGroupType = searchConditionsGroupType, SearchSortFieldNamesList = searchSortFieldNamesList, SearchSortFieldNamesDescendingList = searchSortFieldNamesDescendingList, entityId = entityId, schoolYearId = schoolYearId, flatten = flatten, returnResponse = returnResponse)
	}

	#' Get an AccountsPayableConfigDistrict
	#'
	#' This function returns a dataframe or json object of an AccountsPayableConfigDistrict
	#' @param AccountsPayableConfigDistrictID The ID of the AccountsPayableConfigDistrict to return.
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given AccountsPayableConfigDistrict. Defaults to FALSE for all return fields which, for convenience, returns all fields for the AccountsPayableConfigDistrict.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object listed as text. Run \code{\link{getSchemaForObjects}}('AccountsPayableConfigDistrict') to get more field paths.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{listSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Accounts Payable
	#' @return A dataframe or of AccountsPayableConfigDistrict
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	getAccountsPayableConfigDistrict <- function(AccountsPayableConfigDistrictID, ConfigDistrictID = F, DistrictID = F, SendInvoiceApprovedMessage = F, InvoiceRequestApprovedMessageSubject = F, InvoiceRequestApprovedMessageContent = F, SendInvoiceDeniedMessage = F, InvoiceRequestDeniedMessageSubject = F, InvoiceRequestDeniedMessageContent = F, SendInvoiceWaitingMessage = F, InvoiceRequestWaitingMessageSubject = F, InvoiceRequestWaitingMessageContent = F, SendCreditCardTransactionPreApprovalApprovedMessage = F, SendCreditCardTransactionPostApprovalApprovedMessage = F, CreditCardTransactionRequestApprovedMessageSubject = F, CreditCardTransactionRequestApprovedMessageContent = F, SendCreditCardTransactionPreApprovalDeniedMessage = F, SendCreditCardTransactionPostApprovalDeniedMessage = F, CreditCardTransactionRequestDeniedMessageSubject = F, CreditCardTransactionRequestDeniedMessageContent = F, SendCreditCardTransactionPreApprovalWaitingMessage = F, SendCreditCardTransactionPostApprovalWaitingMessage = F, CreditCardTransactionRequestWaitingMessageSubject = F, CreditCardTransactionRequestWaitingMessageContent = F, MaskIDAccrual = F, UseAvailableFundWarning = F, UseAvailableFundError = F, PercentOverPurchaseOrder = F, AvailableFundWarningXMLFilter = F, SecurityStandardWarningFilterData = F, AvailableFundErrorXMLFilter = F, SecurityStandardErrorFilterData = F, BatchDefault = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, SendExpenseReimbursementPreApprovalApprovedMessage = F, SendExpenseReimbursementPostApprovalApprovedMessage = F, ExpenseReimbursementRequestApprovedMessageSubject = F, ExpenseReimbursementRequestApprovedMessageContent = F, SendExpenseReimbursementPreApprovalDeniedMessage = F, SendExpenseReimbursementPostApprovalDeniedMessage = F, ExpenseReimbursementRequestDeniedMessageSubject = F, ExpenseReimbursementRequestDeniedMessageContent = F, SendExpenseReimbursementPreApprovalWaitingMessage = F, SendExpenseReimbursementPostApprovalWaitingMessage = F, ExpenseReimbursementRequestWaitingMessageSubject = F, ExpenseReimbursementRequestWaitingMessageContent = F, OneFundPerCheck = F, DirectDepositNotificationSubject = F, DirectDepositNotificationContent = F, MaskIDAccrualActivityAccounting = F, fieldPaths = NULL, entityId = 1, schoolYearId = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment()) %>% purrr::keep(names(.) != "AccountsPayableConfigDistrictID")

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		getSkyObject(module = "AccountsPayable", objectName = "ConfigDistrict", objectId = AccountsPayableConfigDistrictID, searchFields = searchFields, entityId = entityId, flatten = flatten, returnResponse = returnResponse)
	}

	#' Delete an AccountsPayableConfigDistrict
	#'
	#' This function deletes an AccountsPayableConfigDistrict
	#' @param AccountsPayableConfigDistrictID The ID of the AccountsPayableConfigDistrict to delete
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{listSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Accounts Payable
	#' @return The AccountsPayableConfigDistrictID of the deleted AccountsPayableConfigDistrict.
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	deleteAccountsPayableConfigDistrict <- function(AccountsPayableConfigDistrictID, ignoreWarnings = F, entityId = 1, schoolYearId = NULL, flatten = T, returnResponse = F){

		deleteSkyObject(module = "AccountsPayable", objectName = "ConfigDistrict", objectId = AccountsPayableConfigDistrictID, ignoreWarnings = ignoreWarnings, entityId = entityId, flatten = flatten, returnResponse = returnResponse)
	}

	#' Create an AccountsPayableConfigDistrict
	#'
	#' This function creates an AccountsPayableConfigDistrict
	#' @param fieldNames The field values to give the created AccountsPayableConfigDistrict. Each defaults to NULL.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{listSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Accounts Payable
	#' @return A newly created AccountsPayableConfigDistrict
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	createAccountsPayableConfigDistrict <- function(DistrictID = NULL, SendInvoiceApprovedMessage = NULL, InvoiceRequestApprovedMessageSubject = NULL, InvoiceRequestApprovedMessageContent = NULL, SendInvoiceDeniedMessage = NULL, InvoiceRequestDeniedMessageSubject = NULL, InvoiceRequestDeniedMessageContent = NULL, SendInvoiceWaitingMessage = NULL, InvoiceRequestWaitingMessageSubject = NULL, InvoiceRequestWaitingMessageContent = NULL, SendCreditCardTransactionPreApprovalApprovedMessage = NULL, SendCreditCardTransactionPostApprovalApprovedMessage = NULL, CreditCardTransactionRequestApprovedMessageSubject = NULL, CreditCardTransactionRequestApprovedMessageContent = NULL, SendCreditCardTransactionPreApprovalDeniedMessage = NULL, SendCreditCardTransactionPostApprovalDeniedMessage = NULL, CreditCardTransactionRequestDeniedMessageSubject = NULL, CreditCardTransactionRequestDeniedMessageContent = NULL, SendCreditCardTransactionPreApprovalWaitingMessage = NULL, SendCreditCardTransactionPostApprovalWaitingMessage = NULL, CreditCardTransactionRequestWaitingMessageSubject = NULL, CreditCardTransactionRequestWaitingMessageContent = NULL, MaskIDAccrual = NULL, UseAvailableFundWarning = NULL, UseAvailableFundError = NULL, PercentOverPurchaseOrder = NULL, BatchDefault = NULL, SendExpenseReimbursementPreApprovalApprovedMessage = NULL, SendExpenseReimbursementPostApprovalApprovedMessage = NULL, ExpenseReimbursementRequestApprovedMessageSubject = NULL, ExpenseReimbursementRequestApprovedMessageContent = NULL, SendExpenseReimbursementPreApprovalDeniedMessage = NULL, SendExpenseReimbursementPostApprovalDeniedMessage = NULL, ExpenseReimbursementRequestDeniedMessageSubject = NULL, ExpenseReimbursementRequestDeniedMessageContent = NULL, SendExpenseReimbursementPreApprovalWaitingMessage = NULL, SendExpenseReimbursementPostApprovalWaitingMessage = NULL, ExpenseReimbursementRequestWaitingMessageSubject = NULL, ExpenseReimbursementRequestWaitingMessageContent = NULL, OneFundPerCheck = NULL, DirectDepositNotificationSubject = NULL, DirectDepositNotificationContent = NULL, MaskIDAccrualActivityAccounting = NULL, entityId = 1, schoolYearId = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment())

		body <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper()) %>% compact()

		createSkyObject(module = "AccountsPayable", objectName = "ConfigDistrict", body = list(DataObject = body), searchFields = append("ConfigDistrictID", body %>% names()), entityId = entityId, schoolYearId = schoolYearId, flatten = flatten, returnResponse = returnResponse)
	}

	#' Modify an AccountsPayableConfigDistrict
	#'
	#' This function modifies an AccountsPayableConfigDistrict
	#' @param fieldNames The field values to give the modified AccountsPayableConfigDistrict. Each defaults to NULL.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{listSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Accounts Payable
	#' @return The modified AccountsPayableConfigDistrict
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	modifyAccountsPayableConfigDistrict <- function(ConfigDistrictID, DistrictID = NULL, SendInvoiceApprovedMessage = NULL, InvoiceRequestApprovedMessageSubject = NULL, InvoiceRequestApprovedMessageContent = NULL, SendInvoiceDeniedMessage = NULL, InvoiceRequestDeniedMessageSubject = NULL, InvoiceRequestDeniedMessageContent = NULL, SendInvoiceWaitingMessage = NULL, InvoiceRequestWaitingMessageSubject = NULL, InvoiceRequestWaitingMessageContent = NULL, SendCreditCardTransactionPreApprovalApprovedMessage = NULL, SendCreditCardTransactionPostApprovalApprovedMessage = NULL, CreditCardTransactionRequestApprovedMessageSubject = NULL, CreditCardTransactionRequestApprovedMessageContent = NULL, SendCreditCardTransactionPreApprovalDeniedMessage = NULL, SendCreditCardTransactionPostApprovalDeniedMessage = NULL, CreditCardTransactionRequestDeniedMessageSubject = NULL, CreditCardTransactionRequestDeniedMessageContent = NULL, SendCreditCardTransactionPreApprovalWaitingMessage = NULL, SendCreditCardTransactionPostApprovalWaitingMessage = NULL, CreditCardTransactionRequestWaitingMessageSubject = NULL, CreditCardTransactionRequestWaitingMessageContent = NULL, MaskIDAccrual = NULL, UseAvailableFundWarning = NULL, UseAvailableFundError = NULL, PercentOverPurchaseOrder = NULL, BatchDefault = NULL, SendExpenseReimbursementPreApprovalApprovedMessage = NULL, SendExpenseReimbursementPostApprovalApprovedMessage = NULL, ExpenseReimbursementRequestApprovedMessageSubject = NULL, ExpenseReimbursementRequestApprovedMessageContent = NULL, SendExpenseReimbursementPreApprovalDeniedMessage = NULL, SendExpenseReimbursementPostApprovalDeniedMessage = NULL, ExpenseReimbursementRequestDeniedMessageSubject = NULL, ExpenseReimbursementRequestDeniedMessageContent = NULL, SendExpenseReimbursementPreApprovalWaitingMessage = NULL, SendExpenseReimbursementPostApprovalWaitingMessage = NULL, ExpenseReimbursementRequestWaitingMessageSubject = NULL, ExpenseReimbursementRequestWaitingMessageContent = NULL, OneFundPerCheck = NULL, DirectDepositNotificationSubject = NULL, DirectDepositNotificationContent = NULL, MaskIDAccrualActivityAccounting = NULL, entityId = 1, schoolYearId = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment())

		body <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper()) %>% compact()

		modifySkyObject(module = "AccountsPayable", objectName = "ConfigDistrict", objectId = ConfigDistrictID, body = list(DataObject = body), searchFields = append("ConfigDistrictID", body %>% names()), entityId = entityId, schoolYearId = schoolYearId, flatten = flatten, returnResponse = returnResponse)
	}

	#' List CreditCardCheckouts
	#'
	#' This function returns a dataframe or json object of CreditCardCheckouts
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given CreditCardCheckouts. Defaults to FALSE for all return fields which, for convenience, returns all fields for the CreditCardCheckouts.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object listed as text. Run \code{\link{getSchemaForObjects}}('CreditCardCheckout') to get more field paths.
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
	#' @concept Accounts Payable
	#' @return A list of CreditCardCheckouts
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	listCreditCardCheckouts <- function(searchConditionsList = NULL, CreditCardCheckoutID = F, CreditCardID = F, EffectiveTime = F, UserIDCheckoutBy = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, CurrentUserHasCreditCardGroupAccess = F, fieldPaths = NULL, searchConditionsGroupType = "And", searchSortFieldNamesList = NULL, searchSortFieldNamesDescendingList = NULL, entityId = 1, schoolYearId = NULL, page = 1, pageSize = 100000, flatten = T, returnResponse = F){

		params <- as.list(environment())

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		listSkyObjects(module = "AccountsPayable", objectName = "CreditCardCheckout", searchFields = searchFields %>% append(fieldPaths), page = page, pageSize = pageSize, SearchConditionsList = searchConditionsList, SearchConditionsGroupType = searchConditionsGroupType, SearchSortFieldNamesList = searchSortFieldNamesList, SearchSortFieldNamesDescendingList = searchSortFieldNamesDescendingList, entityId = entityId, schoolYearId = schoolYearId, flatten = flatten, returnResponse = returnResponse)
	}

	#' Get a CreditCardCheckout
	#'
	#' This function returns a dataframe or json object of a CreditCardCheckout
	#' @param CreditCardCheckoutID The ID of the CreditCardCheckout to return.
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given CreditCardCheckout. Defaults to FALSE for all return fields which, for convenience, returns all fields for the CreditCardCheckout.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object listed as text. Run \code{\link{getSchemaForObjects}}('CreditCardCheckout') to get more field paths.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{listSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Accounts Payable
	#' @return A dataframe or of CreditCardCheckout
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	getCreditCardCheckout <- function(CreditCardCheckoutID, CreditCardID = F, EffectiveTime = F, UserIDCheckoutBy = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, CurrentUserHasCreditCardGroupAccess = F, fieldPaths = NULL, entityId = 1, schoolYearId = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment()) %>% purrr::keep(names(.) != "CreditCardCheckoutID")

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		getSkyObject(module = "AccountsPayable", objectName = "CreditCardCheckout", objectId = CreditCardCheckoutID, searchFields = searchFields, entityId = entityId, flatten = flatten, returnResponse = returnResponse)
	}

	#' Delete a CreditCardCheckout
	#'
	#' This function deletes a CreditCardCheckout
	#' @param CreditCardCheckoutID The ID of the CreditCardCheckout to delete
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{listSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Accounts Payable
	#' @return The CreditCardCheckoutID of the deleted CreditCardCheckout.
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	deleteCreditCardCheckout <- function(CreditCardCheckoutID, ignoreWarnings = F, entityId = 1, schoolYearId = NULL, flatten = T, returnResponse = F){

		deleteSkyObject(module = "AccountsPayable", objectName = "CreditCardCheckout", objectId = CreditCardCheckoutID, ignoreWarnings = ignoreWarnings, entityId = entityId, flatten = flatten, returnResponse = returnResponse)
	}

	#' Create a CreditCardCheckout
	#'
	#' This function creates a CreditCardCheckout
	#' @param fieldNames The field values to give the created CreditCardCheckout. Each defaults to NULL.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{listSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Accounts Payable
	#' @return A newly created CreditCardCheckout
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	createCreditCardCheckout <- function(CreditCardID = NULL, EffectiveTime = NULL, UserIDCheckoutBy = NULL, entityId = 1, schoolYearId = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment())

		body <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper()) %>% compact()

		createSkyObject(module = "AccountsPayable", objectName = "CreditCardCheckout", body = list(DataObject = body), searchFields = append("CreditCardCheckoutID", body %>% names()), entityId = entityId, schoolYearId = schoolYearId, flatten = flatten, returnResponse = returnResponse)
	}

	#' Modify a CreditCardCheckout
	#'
	#' This function modifies a CreditCardCheckout
	#' @param fieldNames The field values to give the modified CreditCardCheckout. Each defaults to NULL.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{listSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Accounts Payable
	#' @return The modified CreditCardCheckout
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	modifyCreditCardCheckout <- function(CreditCardCheckoutID, CreditCardID = NULL, EffectiveTime = NULL, UserIDCheckoutBy = NULL, entityId = 1, schoolYearId = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment())

		body <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper()) %>% compact()

		modifySkyObject(module = "AccountsPayable", objectName = "CreditCardCheckout", objectId = CreditCardCheckoutID, body = list(DataObject = body), searchFields = append("CreditCardCheckoutID", body %>% names()), entityId = entityId, schoolYearId = schoolYearId, flatten = flatten, returnResponse = returnResponse)
	}

	#' List ExpenseReimbursements
	#'
	#' This function returns a dataframe or json object of ExpenseReimbursements
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given ExpenseReimbursements. Defaults to FALSE for all return fields which, for convenience, returns all fields for the ExpenseReimbursements.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object listed as text. Run \code{\link{getSchemaForObjects}}('ExpenseReimbursement') to get more field paths.
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
	#' @concept Accounts Payable
	#' @return A list of ExpenseReimbursements
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	listExpenseReimbursements <- function(searchConditionsList = NULL, ExpenseReimbursementID = F, FiscalYearID = F, Status = F, TransactionStartDate = F, TransactionEndDate = F, NumberOfTransactionDays = F, EntryAmount = F, BaseCurrencyAmount = F, CurrencyIDEntry = F, Description = F, AttachmentCount = F, NameIDReimbursementFor = F, ExpenseReimbursementGroupID = F, CanSubmit = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, InvoiceID = F, AssignmentIDReimbursementFor = F, IsDataComplete = F, BaseCurrencyAmountByAccount = F, CurrentUserHasExpenseReimbursementGroupAccess = F, RenderDeleteButton = F, RenderResubmitButton = F, RenderExpenseReimbursementDetailBrowseButtons = F, fieldPaths = NULL, searchConditionsGroupType = "And", searchSortFieldNamesList = NULL, searchSortFieldNamesDescendingList = NULL, entityId = 1, schoolYearId = NULL, page = 1, pageSize = 100000, flatten = T, returnResponse = F){

		params <- as.list(environment())

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		listSkyObjects(module = "AccountsPayable", objectName = "ExpenseReimbursement", searchFields = searchFields %>% append(fieldPaths), page = page, pageSize = pageSize, SearchConditionsList = searchConditionsList, SearchConditionsGroupType = searchConditionsGroupType, SearchSortFieldNamesList = searchSortFieldNamesList, SearchSortFieldNamesDescendingList = searchSortFieldNamesDescendingList, entityId = entityId, schoolYearId = schoolYearId, flatten = flatten, returnResponse = returnResponse)
	}

	#' Get an ExpenseReimbursement
	#'
	#' This function returns a dataframe or json object of an ExpenseReimbursement
	#' @param ExpenseReimbursementID The ID of the ExpenseReimbursement to return.
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given ExpenseReimbursement. Defaults to FALSE for all return fields which, for convenience, returns all fields for the ExpenseReimbursement.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object listed as text. Run \code{\link{getSchemaForObjects}}('ExpenseReimbursement') to get more field paths.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{listSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Accounts Payable
	#' @return A dataframe or of ExpenseReimbursement
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	getExpenseReimbursement <- function(ExpenseReimbursementID, FiscalYearID = F, Status = F, TransactionStartDate = F, TransactionEndDate = F, NumberOfTransactionDays = F, EntryAmount = F, BaseCurrencyAmount = F, CurrencyIDEntry = F, Description = F, AttachmentCount = F, NameIDReimbursementFor = F, ExpenseReimbursementGroupID = F, CanSubmit = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, InvoiceID = F, AssignmentIDReimbursementFor = F, IsDataComplete = F, BaseCurrencyAmountByAccount = F, CurrentUserHasExpenseReimbursementGroupAccess = F, RenderDeleteButton = F, RenderResubmitButton = F, RenderExpenseReimbursementDetailBrowseButtons = F, fieldPaths = NULL, entityId = 1, schoolYearId = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment()) %>% purrr::keep(names(.) != "ExpenseReimbursementID")

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		getSkyObject(module = "AccountsPayable", objectName = "ExpenseReimbursement", objectId = ExpenseReimbursementID, searchFields = searchFields, entityId = entityId, flatten = flatten, returnResponse = returnResponse)
	}

	#' Delete an ExpenseReimbursement
	#'
	#' This function deletes an ExpenseReimbursement
	#' @param ExpenseReimbursementID The ID of the ExpenseReimbursement to delete
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{listSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Accounts Payable
	#' @return The ExpenseReimbursementID of the deleted ExpenseReimbursement.
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	deleteExpenseReimbursement <- function(ExpenseReimbursementID, ignoreWarnings = F, entityId = 1, schoolYearId = NULL, flatten = T, returnResponse = F){

		deleteSkyObject(module = "AccountsPayable", objectName = "ExpenseReimbursement", objectId = ExpenseReimbursementID, ignoreWarnings = ignoreWarnings, entityId = entityId, flatten = flatten, returnResponse = returnResponse)
	}

	#' Create an ExpenseReimbursement
	#'
	#' This function creates an ExpenseReimbursement
	#' @param fieldNames The field values to give the created ExpenseReimbursement. Each defaults to NULL.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{listSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Accounts Payable
	#' @return A newly created ExpenseReimbursement
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	createExpenseReimbursement <- function(FiscalYearID = NULL, Status = NULL, TransactionStartDate = NULL, TransactionEndDate = NULL, CurrencyIDEntry = NULL, Description = NULL, NameIDReimbursementFor = NULL, ExpenseReimbursementGroupID = NULL, InvoiceID = NULL, AssignmentIDReimbursementFor = NULL, entityId = 1, schoolYearId = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment())

		body <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper()) %>% compact()

		createSkyObject(module = "AccountsPayable", objectName = "ExpenseReimbursement", body = list(DataObject = body), searchFields = append("ExpenseReimbursementID", body %>% names()), entityId = entityId, schoolYearId = schoolYearId, flatten = flatten, returnResponse = returnResponse)
	}

	#' Modify an ExpenseReimbursement
	#'
	#' This function modifies an ExpenseReimbursement
	#' @param fieldNames The field values to give the modified ExpenseReimbursement. Each defaults to NULL.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{listSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Accounts Payable
	#' @return The modified ExpenseReimbursement
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	modifyExpenseReimbursement <- function(ExpenseReimbursementID, FiscalYearID = NULL, Status = NULL, TransactionStartDate = NULL, TransactionEndDate = NULL, CurrencyIDEntry = NULL, Description = NULL, NameIDReimbursementFor = NULL, ExpenseReimbursementGroupID = NULL, InvoiceID = NULL, AssignmentIDReimbursementFor = NULL, entityId = 1, schoolYearId = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment())

		body <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper()) %>% compact()

		modifySkyObject(module = "AccountsPayable", objectName = "ExpenseReimbursement", objectId = ExpenseReimbursementID, body = list(DataObject = body), searchFields = append("ExpenseReimbursementID", body %>% names()), entityId = entityId, schoolYearId = schoolYearId, flatten = flatten, returnResponse = returnResponse)
	}

	#' List ExpenseReimbursementDetails
	#'
	#' This function returns a dataframe or json object of ExpenseReimbursementDetails
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given ExpenseReimbursementDetails. Defaults to FALSE for all return fields which, for convenience, returns all fields for the ExpenseReimbursementDetails.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object listed as text. Run \code{\link{getSchemaForObjects}}('ExpenseReimbursementDetail') to get more field paths.
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
	#' @concept Accounts Payable
	#' @return A list of ExpenseReimbursementDetails
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	listExpenseReimbursementDetails <- function(searchConditionsList = NULL, ExpenseReimbursementDetailID = F, ExpenseReimbursementTypeID = F, ExpenseReimbursementID = F, Reimburse = F, DisplayOrder = F, Description = F, Quantity = F, UnitCost = F, EntryAmount = F, BaseCurrencyAmount = F, ExpenseReimbursementTotalLessEntryAmount = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, Date = F, QuantityAndLabel = F, RateAndLabel = F, CurrentUserHasExpenseReimbursementGroupAccess = F, fieldPaths = NULL, searchConditionsGroupType = "And", searchSortFieldNamesList = NULL, searchSortFieldNamesDescendingList = NULL, entityId = 1, schoolYearId = NULL, page = 1, pageSize = 100000, flatten = T, returnResponse = F){

		params <- as.list(environment())

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		listSkyObjects(module = "AccountsPayable", objectName = "ExpenseReimbursementDetail", searchFields = searchFields %>% append(fieldPaths), page = page, pageSize = pageSize, SearchConditionsList = searchConditionsList, SearchConditionsGroupType = searchConditionsGroupType, SearchSortFieldNamesList = searchSortFieldNamesList, SearchSortFieldNamesDescendingList = searchSortFieldNamesDescendingList, entityId = entityId, schoolYearId = schoolYearId, flatten = flatten, returnResponse = returnResponse)
	}

	#' Get an ExpenseReimbursementDetail
	#'
	#' This function returns a dataframe or json object of an ExpenseReimbursementDetail
	#' @param ExpenseReimbursementDetailID The ID of the ExpenseReimbursementDetail to return.
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given ExpenseReimbursementDetail. Defaults to FALSE for all return fields which, for convenience, returns all fields for the ExpenseReimbursementDetail.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object listed as text. Run \code{\link{getSchemaForObjects}}('ExpenseReimbursementDetail') to get more field paths.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{listSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Accounts Payable
	#' @return A dataframe or of ExpenseReimbursementDetail
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	getExpenseReimbursementDetail <- function(ExpenseReimbursementDetailID, ExpenseReimbursementTypeID = F, ExpenseReimbursementID = F, Reimburse = F, DisplayOrder = F, Description = F, Quantity = F, UnitCost = F, EntryAmount = F, BaseCurrencyAmount = F, ExpenseReimbursementTotalLessEntryAmount = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, Date = F, QuantityAndLabel = F, RateAndLabel = F, CurrentUserHasExpenseReimbursementGroupAccess = F, fieldPaths = NULL, entityId = 1, schoolYearId = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment()) %>% purrr::keep(names(.) != "ExpenseReimbursementDetailID")

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		getSkyObject(module = "AccountsPayable", objectName = "ExpenseReimbursementDetail", objectId = ExpenseReimbursementDetailID, searchFields = searchFields, entityId = entityId, flatten = flatten, returnResponse = returnResponse)
	}

	#' Delete an ExpenseReimbursementDetail
	#'
	#' This function deletes an ExpenseReimbursementDetail
	#' @param ExpenseReimbursementDetailID The ID of the ExpenseReimbursementDetail to delete
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{listSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Accounts Payable
	#' @return The ExpenseReimbursementDetailID of the deleted ExpenseReimbursementDetail.
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	deleteExpenseReimbursementDetail <- function(ExpenseReimbursementDetailID, ignoreWarnings = F, entityId = 1, schoolYearId = NULL, flatten = T, returnResponse = F){

		deleteSkyObject(module = "AccountsPayable", objectName = "ExpenseReimbursementDetail", objectId = ExpenseReimbursementDetailID, ignoreWarnings = ignoreWarnings, entityId = entityId, flatten = flatten, returnResponse = returnResponse)
	}

	#' Create an ExpenseReimbursementDetail
	#'
	#' This function creates an ExpenseReimbursementDetail
	#' @param fieldNames The field values to give the created ExpenseReimbursementDetail. Each defaults to NULL.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{listSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Accounts Payable
	#' @return A newly created ExpenseReimbursementDetail
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	createExpenseReimbursementDetail <- function(ExpenseReimbursementTypeID = NULL, ExpenseReimbursementID = NULL, Reimburse = NULL, DisplayOrder = NULL, Description = NULL, Quantity = NULL, UnitCost = NULL, EntryAmount = NULL, BaseCurrencyAmount = NULL, Date = NULL, entityId = 1, schoolYearId = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment())

		body <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper()) %>% compact()

		createSkyObject(module = "AccountsPayable", objectName = "ExpenseReimbursementDetail", body = list(DataObject = body), searchFields = append("ExpenseReimbursementDetailID", body %>% names()), entityId = entityId, schoolYearId = schoolYearId, flatten = flatten, returnResponse = returnResponse)
	}

	#' Modify an ExpenseReimbursementDetail
	#'
	#' This function modifies an ExpenseReimbursementDetail
	#' @param fieldNames The field values to give the modified ExpenseReimbursementDetail. Each defaults to NULL.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{listSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Accounts Payable
	#' @return The modified ExpenseReimbursementDetail
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	modifyExpenseReimbursementDetail <- function(ExpenseReimbursementDetailID, ExpenseReimbursementTypeID = NULL, ExpenseReimbursementID = NULL, Reimburse = NULL, DisplayOrder = NULL, Description = NULL, Quantity = NULL, UnitCost = NULL, EntryAmount = NULL, BaseCurrencyAmount = NULL, Date = NULL, entityId = 1, schoolYearId = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment())

		body <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper()) %>% compact()

		modifySkyObject(module = "AccountsPayable", objectName = "ExpenseReimbursementDetail", objectId = ExpenseReimbursementDetailID, body = list(DataObject = body), searchFields = append("ExpenseReimbursementDetailID", body %>% names()), entityId = entityId, schoolYearId = schoolYearId, flatten = flatten, returnResponse = returnResponse)
	}

	#' List ExpenseReimbursementAccountings
	#'
	#' This function returns a dataframe or json object of ExpenseReimbursementAccountings
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given ExpenseReimbursementAccountings. Defaults to FALSE for all return fields which, for convenience, returns all fields for the ExpenseReimbursementAccountings.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object listed as text. Run \code{\link{getSchemaForObjects}}('ExpenseReimbursementAccounting') to get more field paths.
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
	#' @concept Accounts Payable
	#' @return A list of ExpenseReimbursementAccountings
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	listExpenseReimbursementAccountings <- function(searchConditionsList = NULL, ExpenseReimbursementAccountingID = F, ExpenseReimbursementDetailID = F, EntryAmount = F, BaseCurrencyAmount = F, BaseCurrencyPercent = F, AccountID = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, CurrentUserHasAccountGroupAccess = F, CurrentUserHasExpenseReimbursementGroupAccess = F, GrantID = F, ProjectID = F, fieldPaths = NULL, searchConditionsGroupType = "And", searchSortFieldNamesList = NULL, searchSortFieldNamesDescendingList = NULL, entityId = 1, schoolYearId = NULL, page = 1, pageSize = 100000, flatten = T, returnResponse = F){

		params <- as.list(environment())

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		listSkyObjects(module = "AccountsPayable", objectName = "ExpenseReimbursementAccounting", searchFields = searchFields %>% append(fieldPaths), page = page, pageSize = pageSize, SearchConditionsList = searchConditionsList, SearchConditionsGroupType = searchConditionsGroupType, SearchSortFieldNamesList = searchSortFieldNamesList, SearchSortFieldNamesDescendingList = searchSortFieldNamesDescendingList, entityId = entityId, schoolYearId = schoolYearId, flatten = flatten, returnResponse = returnResponse)
	}

	#' Get an ExpenseReimbursementAccounting
	#'
	#' This function returns a dataframe or json object of an ExpenseReimbursementAccounting
	#' @param ExpenseReimbursementAccountingID The ID of the ExpenseReimbursementAccounting to return.
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given ExpenseReimbursementAccounting. Defaults to FALSE for all return fields which, for convenience, returns all fields for the ExpenseReimbursementAccounting.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object listed as text. Run \code{\link{getSchemaForObjects}}('ExpenseReimbursementAccounting') to get more field paths.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{listSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Accounts Payable
	#' @return A dataframe or of ExpenseReimbursementAccounting
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	getExpenseReimbursementAccounting <- function(ExpenseReimbursementAccountingID, ExpenseReimbursementDetailID = F, EntryAmount = F, BaseCurrencyAmount = F, BaseCurrencyPercent = F, AccountID = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, CurrentUserHasAccountGroupAccess = F, CurrentUserHasExpenseReimbursementGroupAccess = F, GrantID = F, ProjectID = F, fieldPaths = NULL, entityId = 1, schoolYearId = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment()) %>% purrr::keep(names(.) != "ExpenseReimbursementAccountingID")

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		getSkyObject(module = "AccountsPayable", objectName = "ExpenseReimbursementAccounting", objectId = ExpenseReimbursementAccountingID, searchFields = searchFields, entityId = entityId, flatten = flatten, returnResponse = returnResponse)
	}

	#' Delete an ExpenseReimbursementAccounting
	#'
	#' This function deletes an ExpenseReimbursementAccounting
	#' @param ExpenseReimbursementAccountingID The ID of the ExpenseReimbursementAccounting to delete
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{listSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Accounts Payable
	#' @return The ExpenseReimbursementAccountingID of the deleted ExpenseReimbursementAccounting.
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	deleteExpenseReimbursementAccounting <- function(ExpenseReimbursementAccountingID, ignoreWarnings = F, entityId = 1, schoolYearId = NULL, flatten = T, returnResponse = F){

		deleteSkyObject(module = "AccountsPayable", objectName = "ExpenseReimbursementAccounting", objectId = ExpenseReimbursementAccountingID, ignoreWarnings = ignoreWarnings, entityId = entityId, flatten = flatten, returnResponse = returnResponse)
	}

	#' Create an ExpenseReimbursementAccounting
	#'
	#' This function creates an ExpenseReimbursementAccounting
	#' @param fieldNames The field values to give the created ExpenseReimbursementAccounting. Each defaults to NULL.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{listSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Accounts Payable
	#' @return A newly created ExpenseReimbursementAccounting
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	createExpenseReimbursementAccounting <- function(ExpenseReimbursementDetailID = NULL, EntryAmount = NULL, BaseCurrencyAmount = NULL, AccountID = NULL, GrantID = NULL, ProjectID = NULL, entityId = 1, schoolYearId = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment())

		body <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper()) %>% compact()

		createSkyObject(module = "AccountsPayable", objectName = "ExpenseReimbursementAccounting", body = list(DataObject = body), searchFields = append("ExpenseReimbursementAccountingID", body %>% names()), entityId = entityId, schoolYearId = schoolYearId, flatten = flatten, returnResponse = returnResponse)
	}

	#' Modify an ExpenseReimbursementAccounting
	#'
	#' This function modifies an ExpenseReimbursementAccounting
	#' @param fieldNames The field values to give the modified ExpenseReimbursementAccounting. Each defaults to NULL.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{listSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Accounts Payable
	#' @return The modified ExpenseReimbursementAccounting
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	modifyExpenseReimbursementAccounting <- function(ExpenseReimbursementAccountingID, ExpenseReimbursementDetailID = NULL, EntryAmount = NULL, BaseCurrencyAmount = NULL, AccountID = NULL, GrantID = NULL, ProjectID = NULL, entityId = 1, schoolYearId = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment())

		body <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper()) %>% compact()

		modifySkyObject(module = "AccountsPayable", objectName = "ExpenseReimbursementAccounting", objectId = ExpenseReimbursementAccountingID, body = list(DataObject = body), searchFields = append("ExpenseReimbursementAccountingID", body %>% names()), entityId = entityId, schoolYearId = schoolYearId, flatten = flatten, returnResponse = returnResponse)
	}

	#' List ExpenseReimbursementGroups
	#'
	#' This function returns a dataframe or json object of ExpenseReimbursementGroups
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given ExpenseReimbursementGroups. Defaults to FALSE for all return fields which, for convenience, returns all fields for the ExpenseReimbursementGroups.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object listed as text. Run \code{\link{getSchemaForObjects}}('ExpenseReimbursementGroup') to get more field paths.
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
	#' @concept Accounts Payable
	#' @return A list of ExpenseReimbursementGroups
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	listExpenseReimbursementGroups <- function(searchConditionsList = NULL, ExpenseReimbursementGroupID = F, DistrictID = F, GroupIDAccount = F, Code = F, Description = F, CodeDescription = F, IsActive = F, IsPreApprovalWorkflowUpdated = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, IsPostApprovalWorkflowUpdated = F, CurrentUserHasExpenseReimbursementGroupAccess = F, fieldPaths = NULL, searchConditionsGroupType = "And", searchSortFieldNamesList = NULL, searchSortFieldNamesDescendingList = NULL, entityId = 1, schoolYearId = NULL, page = 1, pageSize = 100000, flatten = T, returnResponse = F){

		params <- as.list(environment())

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		listSkyObjects(module = "AccountsPayable", objectName = "ExpenseReimbursementGroup", searchFields = searchFields %>% append(fieldPaths), page = page, pageSize = pageSize, SearchConditionsList = searchConditionsList, SearchConditionsGroupType = searchConditionsGroupType, SearchSortFieldNamesList = searchSortFieldNamesList, SearchSortFieldNamesDescendingList = searchSortFieldNamesDescendingList, entityId = entityId, schoolYearId = schoolYearId, flatten = flatten, returnResponse = returnResponse)
	}

	#' Get an ExpenseReimbursementGroup
	#'
	#' This function returns a dataframe or json object of an ExpenseReimbursementGroup
	#' @param ExpenseReimbursementGroupID The ID of the ExpenseReimbursementGroup to return.
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given ExpenseReimbursementGroup. Defaults to FALSE for all return fields which, for convenience, returns all fields for the ExpenseReimbursementGroup.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object listed as text. Run \code{\link{getSchemaForObjects}}('ExpenseReimbursementGroup') to get more field paths.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{listSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Accounts Payable
	#' @return A dataframe or of ExpenseReimbursementGroup
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	getExpenseReimbursementGroup <- function(ExpenseReimbursementGroupID, DistrictID = F, GroupIDAccount = F, Code = F, Description = F, CodeDescription = F, IsActive = F, IsPreApprovalWorkflowUpdated = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, IsPostApprovalWorkflowUpdated = F, CurrentUserHasExpenseReimbursementGroupAccess = F, fieldPaths = NULL, entityId = 1, schoolYearId = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment()) %>% purrr::keep(names(.) != "ExpenseReimbursementGroupID")

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		getSkyObject(module = "AccountsPayable", objectName = "ExpenseReimbursementGroup", objectId = ExpenseReimbursementGroupID, searchFields = searchFields, entityId = entityId, flatten = flatten, returnResponse = returnResponse)
	}

	#' Delete an ExpenseReimbursementGroup
	#'
	#' This function deletes an ExpenseReimbursementGroup
	#' @param ExpenseReimbursementGroupID The ID of the ExpenseReimbursementGroup to delete
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{listSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Accounts Payable
	#' @return The ExpenseReimbursementGroupID of the deleted ExpenseReimbursementGroup.
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	deleteExpenseReimbursementGroup <- function(ExpenseReimbursementGroupID, ignoreWarnings = F, entityId = 1, schoolYearId = NULL, flatten = T, returnResponse = F){

		deleteSkyObject(module = "AccountsPayable", objectName = "ExpenseReimbursementGroup", objectId = ExpenseReimbursementGroupID, ignoreWarnings = ignoreWarnings, entityId = entityId, flatten = flatten, returnResponse = returnResponse)
	}

	#' Create an ExpenseReimbursementGroup
	#'
	#' This function creates an ExpenseReimbursementGroup
	#' @param fieldNames The field values to give the created ExpenseReimbursementGroup. Each defaults to NULL.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{listSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Accounts Payable
	#' @return A newly created ExpenseReimbursementGroup
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	createExpenseReimbursementGroup <- function(DistrictID = NULL, GroupIDAccount = NULL, Code = NULL, Description = NULL, IsActive = NULL, entityId = 1, schoolYearId = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment())

		body <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper()) %>% compact()

		createSkyObject(module = "AccountsPayable", objectName = "ExpenseReimbursementGroup", body = list(DataObject = body), searchFields = append("ExpenseReimbursementGroupID", body %>% names()), entityId = entityId, schoolYearId = schoolYearId, flatten = flatten, returnResponse = returnResponse)
	}

	#' Modify an ExpenseReimbursementGroup
	#'
	#' This function modifies an ExpenseReimbursementGroup
	#' @param fieldNames The field values to give the modified ExpenseReimbursementGroup. Each defaults to NULL.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{listSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Accounts Payable
	#' @return The modified ExpenseReimbursementGroup
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	modifyExpenseReimbursementGroup <- function(ExpenseReimbursementGroupID, DistrictID = NULL, GroupIDAccount = NULL, Code = NULL, Description = NULL, IsActive = NULL, entityId = 1, schoolYearId = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment())

		body <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper()) %>% compact()

		modifySkyObject(module = "AccountsPayable", objectName = "ExpenseReimbursementGroup", objectId = ExpenseReimbursementGroupID, body = list(DataObject = body), searchFields = append("ExpenseReimbursementGroupID", body %>% names()), entityId = entityId, schoolYearId = schoolYearId, flatten = flatten, returnResponse = returnResponse)
	}

	#' List ExpenseReimbursementGroupClearances
	#'
	#' This function returns a dataframe or json object of ExpenseReimbursementGroupClearances
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given ExpenseReimbursementGroupClearances. Defaults to FALSE for all return fields which, for convenience, returns all fields for the ExpenseReimbursementGroupClearances.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object listed as text. Run \code{\link{getSchemaForObjects}}('ExpenseReimbursementGroupClearance') to get more field paths.
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
	#' @concept Accounts Payable
	#' @return A list of ExpenseReimbursementGroupClearances
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	listExpenseReimbursementGroupClearances <- function(searchConditionsList = NULL, ExpenseReimbursementGroupClearanceID = F, ExpenseReimbursementGroupID = F, GroupIDSecurity = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, fieldPaths = NULL, searchConditionsGroupType = "And", searchSortFieldNamesList = NULL, searchSortFieldNamesDescendingList = NULL, entityId = 1, schoolYearId = NULL, page = 1, pageSize = 100000, flatten = T, returnResponse = F){

		params <- as.list(environment())

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		listSkyObjects(module = "AccountsPayable", objectName = "ExpenseReimbursementGroupClearance", searchFields = searchFields %>% append(fieldPaths), page = page, pageSize = pageSize, SearchConditionsList = searchConditionsList, SearchConditionsGroupType = searchConditionsGroupType, SearchSortFieldNamesList = searchSortFieldNamesList, SearchSortFieldNamesDescendingList = searchSortFieldNamesDescendingList, entityId = entityId, schoolYearId = schoolYearId, flatten = flatten, returnResponse = returnResponse)
	}

	#' Get an ExpenseReimbursementGroupClearance
	#'
	#' This function returns a dataframe or json object of an ExpenseReimbursementGroupClearance
	#' @param ExpenseReimbursementGroupClearanceID The ID of the ExpenseReimbursementGroupClearance to return.
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given ExpenseReimbursementGroupClearance. Defaults to FALSE for all return fields which, for convenience, returns all fields for the ExpenseReimbursementGroupClearance.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object listed as text. Run \code{\link{getSchemaForObjects}}('ExpenseReimbursementGroupClearance') to get more field paths.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{listSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Accounts Payable
	#' @return A dataframe or of ExpenseReimbursementGroupClearance
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	getExpenseReimbursementGroupClearance <- function(ExpenseReimbursementGroupClearanceID, ExpenseReimbursementGroupID = F, GroupIDSecurity = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, fieldPaths = NULL, entityId = 1, schoolYearId = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment()) %>% purrr::keep(names(.) != "ExpenseReimbursementGroupClearanceID")

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		getSkyObject(module = "AccountsPayable", objectName = "ExpenseReimbursementGroupClearance", objectId = ExpenseReimbursementGroupClearanceID, searchFields = searchFields, entityId = entityId, flatten = flatten, returnResponse = returnResponse)
	}

	#' Delete an ExpenseReimbursementGroupClearance
	#'
	#' This function deletes an ExpenseReimbursementGroupClearance
	#' @param ExpenseReimbursementGroupClearanceID The ID of the ExpenseReimbursementGroupClearance to delete
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{listSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Accounts Payable
	#' @return The ExpenseReimbursementGroupClearanceID of the deleted ExpenseReimbursementGroupClearance.
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	deleteExpenseReimbursementGroupClearance <- function(ExpenseReimbursementGroupClearanceID, ignoreWarnings = F, entityId = 1, schoolYearId = NULL, flatten = T, returnResponse = F){

		deleteSkyObject(module = "AccountsPayable", objectName = "ExpenseReimbursementGroupClearance", objectId = ExpenseReimbursementGroupClearanceID, ignoreWarnings = ignoreWarnings, entityId = entityId, flatten = flatten, returnResponse = returnResponse)
	}

	#' Create an ExpenseReimbursementGroupClearance
	#'
	#' This function creates an ExpenseReimbursementGroupClearance
	#' @param fieldNames The field values to give the created ExpenseReimbursementGroupClearance. Each defaults to NULL.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{listSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Accounts Payable
	#' @return A newly created ExpenseReimbursementGroupClearance
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	createExpenseReimbursementGroupClearance <- function(ExpenseReimbursementGroupID = NULL, GroupIDSecurity = NULL, entityId = 1, schoolYearId = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment())

		body <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper()) %>% compact()

		createSkyObject(module = "AccountsPayable", objectName = "ExpenseReimbursementGroupClearance", body = list(DataObject = body), searchFields = append("ExpenseReimbursementGroupClearanceID", body %>% names()), entityId = entityId, schoolYearId = schoolYearId, flatten = flatten, returnResponse = returnResponse)
	}

	#' Modify an ExpenseReimbursementGroupClearance
	#'
	#' This function modifies an ExpenseReimbursementGroupClearance
	#' @param fieldNames The field values to give the modified ExpenseReimbursementGroupClearance. Each defaults to NULL.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{listSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Accounts Payable
	#' @return The modified ExpenseReimbursementGroupClearance
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	modifyExpenseReimbursementGroupClearance <- function(ExpenseReimbursementGroupClearanceID, ExpenseReimbursementGroupID = NULL, GroupIDSecurity = NULL, entityId = 1, schoolYearId = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment())

		body <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper()) %>% compact()

		modifySkyObject(module = "AccountsPayable", objectName = "ExpenseReimbursementGroupClearance", objectId = ExpenseReimbursementGroupClearanceID, body = list(DataObject = body), searchFields = append("ExpenseReimbursementGroupClearanceID", body %>% names()), entityId = entityId, schoolYearId = schoolYearId, flatten = flatten, returnResponse = returnResponse)
	}

	#' List ExpenseReimbursementTypes
	#'
	#' This function returns a dataframe or json object of ExpenseReimbursementTypes
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given ExpenseReimbursementTypes. Defaults to FALSE for all return fields which, for convenience, returns all fields for the ExpenseReimbursementTypes.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object listed as text. Run \code{\link{getSchemaForObjects}}('ExpenseReimbursementType') to get more field paths.
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
	#' @concept Accounts Payable
	#' @return A list of ExpenseReimbursementTypes
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	listExpenseReimbursementTypes <- function(searchConditionsList = NULL, ExpenseReimbursementTypeID = F, ExpenseReimbursementGroupID = F, Code = F, Description = F, CodeDescription = F, TransactionStartDate = F, TransactionEndDate = F, QuantityLabel = F, QuantityPrecision = F, QuantityDefaultValue = F, IsQuantityReadOnly = F, RateLabel = F, RatePrecision = F, RateDefaultValue = F, MaximumRate = F, IsRateReadOnly = F, AllowEmployeeAccess = F, XMLFilter = F, SecurityStandardFilterData = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, FormattedQuantityDefaultValue = F, FormattedRateDefaultValue = F, FormattedMaximumRate = F, CurrentUserHasExpenseReimbursementGroupAccess = F, fieldPaths = NULL, searchConditionsGroupType = "And", searchSortFieldNamesList = NULL, searchSortFieldNamesDescendingList = NULL, entityId = 1, schoolYearId = NULL, page = 1, pageSize = 100000, flatten = T, returnResponse = F){

		params <- as.list(environment())

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		listSkyObjects(module = "AccountsPayable", objectName = "ExpenseReimbursementType", searchFields = searchFields %>% append(fieldPaths), page = page, pageSize = pageSize, SearchConditionsList = searchConditionsList, SearchConditionsGroupType = searchConditionsGroupType, SearchSortFieldNamesList = searchSortFieldNamesList, SearchSortFieldNamesDescendingList = searchSortFieldNamesDescendingList, entityId = entityId, schoolYearId = schoolYearId, flatten = flatten, returnResponse = returnResponse)
	}

	#' Get an ExpenseReimbursementType
	#'
	#' This function returns a dataframe or json object of an ExpenseReimbursementType
	#' @param ExpenseReimbursementTypeID The ID of the ExpenseReimbursementType to return.
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given ExpenseReimbursementType. Defaults to FALSE for all return fields which, for convenience, returns all fields for the ExpenseReimbursementType.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object listed as text. Run \code{\link{getSchemaForObjects}}('ExpenseReimbursementType') to get more field paths.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{listSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Accounts Payable
	#' @return A dataframe or of ExpenseReimbursementType
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	getExpenseReimbursementType <- function(ExpenseReimbursementTypeID, ExpenseReimbursementGroupID = F, Code = F, Description = F, CodeDescription = F, TransactionStartDate = F, TransactionEndDate = F, QuantityLabel = F, QuantityPrecision = F, QuantityDefaultValue = F, IsQuantityReadOnly = F, RateLabel = F, RatePrecision = F, RateDefaultValue = F, MaximumRate = F, IsRateReadOnly = F, AllowEmployeeAccess = F, XMLFilter = F, SecurityStandardFilterData = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, FormattedQuantityDefaultValue = F, FormattedRateDefaultValue = F, FormattedMaximumRate = F, CurrentUserHasExpenseReimbursementGroupAccess = F, fieldPaths = NULL, entityId = 1, schoolYearId = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment()) %>% purrr::keep(names(.) != "ExpenseReimbursementTypeID")

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		getSkyObject(module = "AccountsPayable", objectName = "ExpenseReimbursementType", objectId = ExpenseReimbursementTypeID, searchFields = searchFields, entityId = entityId, flatten = flatten, returnResponse = returnResponse)
	}

	#' Delete an ExpenseReimbursementType
	#'
	#' This function deletes an ExpenseReimbursementType
	#' @param ExpenseReimbursementTypeID The ID of the ExpenseReimbursementType to delete
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{listSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Accounts Payable
	#' @return The ExpenseReimbursementTypeID of the deleted ExpenseReimbursementType.
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	deleteExpenseReimbursementType <- function(ExpenseReimbursementTypeID, ignoreWarnings = F, entityId = 1, schoolYearId = NULL, flatten = T, returnResponse = F){

		deleteSkyObject(module = "AccountsPayable", objectName = "ExpenseReimbursementType", objectId = ExpenseReimbursementTypeID, ignoreWarnings = ignoreWarnings, entityId = entityId, flatten = flatten, returnResponse = returnResponse)
	}

	#' Create an ExpenseReimbursementType
	#'
	#' This function creates an ExpenseReimbursementType
	#' @param fieldNames The field values to give the created ExpenseReimbursementType. Each defaults to NULL.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{listSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Accounts Payable
	#' @return A newly created ExpenseReimbursementType
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	createExpenseReimbursementType <- function(ExpenseReimbursementGroupID = NULL, Code = NULL, Description = NULL, TransactionStartDate = NULL, TransactionEndDate = NULL, QuantityLabel = NULL, QuantityPrecision = NULL, IsQuantityReadOnly = NULL, RateLabel = NULL, RatePrecision = NULL, IsRateReadOnly = NULL, AllowEmployeeAccess = NULL, entityId = 1, schoolYearId = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment())

		body <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper()) %>% compact()

		createSkyObject(module = "AccountsPayable", objectName = "ExpenseReimbursementType", body = list(DataObject = body), searchFields = append("ExpenseReimbursementTypeID", body %>% names()), entityId = entityId, schoolYearId = schoolYearId, flatten = flatten, returnResponse = returnResponse)
	}

	#' Modify an ExpenseReimbursementType
	#'
	#' This function modifies an ExpenseReimbursementType
	#' @param fieldNames The field values to give the modified ExpenseReimbursementType. Each defaults to NULL.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{listSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Accounts Payable
	#' @return The modified ExpenseReimbursementType
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	modifyExpenseReimbursementType <- function(ExpenseReimbursementTypeID, ExpenseReimbursementGroupID = NULL, Code = NULL, Description = NULL, TransactionStartDate = NULL, TransactionEndDate = NULL, QuantityLabel = NULL, QuantityPrecision = NULL, IsQuantityReadOnly = NULL, RateLabel = NULL, RatePrecision = NULL, IsRateReadOnly = NULL, AllowEmployeeAccess = NULL, entityId = 1, schoolYearId = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment())

		body <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper()) %>% compact()

		modifySkyObject(module = "AccountsPayable", objectName = "ExpenseReimbursementType", objectId = ExpenseReimbursementTypeID, body = list(DataObject = body), searchFields = append("ExpenseReimbursementTypeID", body %>% names()), entityId = entityId, schoolYearId = schoolYearId, flatten = flatten, returnResponse = returnResponse)
	}

	#' List InvoiceApprovals
	#'
	#' This function returns a dataframe or json object of InvoiceApprovals
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given InvoiceApprovals. Defaults to FALSE for all return fields which, for convenience, returns all fields for the InvoiceApprovals.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object listed as text. Run \code{\link{getSchemaForObjects}}('InvoiceApproval') to get more field paths.
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
	#' @concept Accounts Payable
	#' @return A list of InvoiceApprovals
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	listInvoiceApprovals <- function(searchConditionsList = NULL, InvoiceApprovalID = F, InvoiceID = F, UserIDApprover = F, Comment = F, TaskInstanceID = F, Status = F, Level = F, LevelDescription = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, ApprovalActionTime = F, fieldPaths = NULL, searchConditionsGroupType = "And", searchSortFieldNamesList = NULL, searchSortFieldNamesDescendingList = NULL, entityId = 1, schoolYearId = NULL, page = 1, pageSize = 100000, flatten = T, returnResponse = F){

		params <- as.list(environment())

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		listSkyObjects(module = "AccountsPayable", objectName = "InvoiceApproval", searchFields = searchFields %>% append(fieldPaths), page = page, pageSize = pageSize, SearchConditionsList = searchConditionsList, SearchConditionsGroupType = searchConditionsGroupType, SearchSortFieldNamesList = searchSortFieldNamesList, SearchSortFieldNamesDescendingList = searchSortFieldNamesDescendingList, entityId = entityId, schoolYearId = schoolYearId, flatten = flatten, returnResponse = returnResponse)
	}

	#' Get an InvoiceApproval
	#'
	#' This function returns a dataframe or json object of an InvoiceApproval
	#' @param InvoiceApprovalID The ID of the InvoiceApproval to return.
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given InvoiceApproval. Defaults to FALSE for all return fields which, for convenience, returns all fields for the InvoiceApproval.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object listed as text. Run \code{\link{getSchemaForObjects}}('InvoiceApproval') to get more field paths.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{listSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Accounts Payable
	#' @return A dataframe or of InvoiceApproval
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	getInvoiceApproval <- function(InvoiceApprovalID, InvoiceID = F, UserIDApprover = F, Comment = F, TaskInstanceID = F, Status = F, Level = F, LevelDescription = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, ApprovalActionTime = F, fieldPaths = NULL, entityId = 1, schoolYearId = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment()) %>% purrr::keep(names(.) != "InvoiceApprovalID")

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		getSkyObject(module = "AccountsPayable", objectName = "InvoiceApproval", objectId = InvoiceApprovalID, searchFields = searchFields, entityId = entityId, flatten = flatten, returnResponse = returnResponse)
	}

	#' Delete an InvoiceApproval
	#'
	#' This function deletes an InvoiceApproval
	#' @param InvoiceApprovalID The ID of the InvoiceApproval to delete
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{listSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Accounts Payable
	#' @return The InvoiceApprovalID of the deleted InvoiceApproval.
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	deleteInvoiceApproval <- function(InvoiceApprovalID, ignoreWarnings = F, entityId = 1, schoolYearId = NULL, flatten = T, returnResponse = F){

		deleteSkyObject(module = "AccountsPayable", objectName = "InvoiceApproval", objectId = InvoiceApprovalID, ignoreWarnings = ignoreWarnings, entityId = entityId, flatten = flatten, returnResponse = returnResponse)
	}

	#' Create an InvoiceApproval
	#'
	#' This function creates an InvoiceApproval
	#' @param fieldNames The field values to give the created InvoiceApproval. Each defaults to NULL.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{listSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Accounts Payable
	#' @return A newly created InvoiceApproval
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	createInvoiceApproval <- function(InvoiceID = NULL, UserIDApprover = NULL, Comment = NULL, TaskInstanceID = NULL, Status = NULL, Level = NULL, LevelDescription = NULL, ApprovalActionTime = NULL, entityId = 1, schoolYearId = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment())

		body <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper()) %>% compact()

		createSkyObject(module = "AccountsPayable", objectName = "InvoiceApproval", body = list(DataObject = body), searchFields = append("InvoiceApprovalID", body %>% names()), entityId = entityId, schoolYearId = schoolYearId, flatten = flatten, returnResponse = returnResponse)
	}

	#' Modify an InvoiceApproval
	#'
	#' This function modifies an InvoiceApproval
	#' @param fieldNames The field values to give the modified InvoiceApproval. Each defaults to NULL.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{listSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Accounts Payable
	#' @return The modified InvoiceApproval
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	modifyInvoiceApproval <- function(InvoiceApprovalID, InvoiceID = NULL, UserIDApprover = NULL, Comment = NULL, TaskInstanceID = NULL, Status = NULL, Level = NULL, LevelDescription = NULL, ApprovalActionTime = NULL, entityId = 1, schoolYearId = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment())

		body <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper()) %>% compact()

		modifySkyObject(module = "AccountsPayable", objectName = "InvoiceApproval", objectId = InvoiceApprovalID, body = list(DataObject = body), searchFields = append("InvoiceApprovalID", body %>% names()), entityId = entityId, schoolYearId = schoolYearId, flatten = flatten, returnResponse = returnResponse)
	}

	#' List AccountsPayableInvoiceGroups
	#'
	#' This function returns a dataframe or json object of AccountsPayableInvoiceGroups
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given AccountsPayableInvoiceGroups. Defaults to FALSE for all return fields which, for convenience, returns all fields for the AccountsPayableInvoiceGroups.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object listed as text. Run \code{\link{getSchemaForObjects}}('AccountsPayableInvoiceGroup') to get more field paths.
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
	#' @concept Accounts Payable
	#' @return A list of AccountsPayableInvoiceGroups
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	listAccountsPayableInvoiceGroups <- function(searchConditionsList = NULL, InvoiceGroupID = F, DistrictID = F, GroupIDAccount = F, Code = F, Description = F, IsActive = F, CodeDescription = F, IsApprovalWorkflowUpdated = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, CurrentUserHasInvoiceGroupAccess = F, ActivityAccountingGroupID = F, IsActivityAccounting = F, fieldPaths = NULL, searchConditionsGroupType = "And", searchSortFieldNamesList = NULL, searchSortFieldNamesDescendingList = NULL, entityId = 1, schoolYearId = NULL, page = 1, pageSize = 100000, flatten = T, returnResponse = F){

		params <- as.list(environment())

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		listSkyObjects(module = "AccountsPayable", objectName = "InvoiceGroup", searchFields = searchFields %>% append(fieldPaths), page = page, pageSize = pageSize, SearchConditionsList = searchConditionsList, SearchConditionsGroupType = searchConditionsGroupType, SearchSortFieldNamesList = searchSortFieldNamesList, SearchSortFieldNamesDescendingList = searchSortFieldNamesDescendingList, entityId = entityId, schoolYearId = schoolYearId, flatten = flatten, returnResponse = returnResponse)
	}

	#' Get an AccountsPayableInvoiceGroup
	#'
	#' This function returns a dataframe or json object of an AccountsPayableInvoiceGroup
	#' @param AccountsPayableInvoiceGroupID The ID of the AccountsPayableInvoiceGroup to return.
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given AccountsPayableInvoiceGroup. Defaults to FALSE for all return fields which, for convenience, returns all fields for the AccountsPayableInvoiceGroup.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object listed as text. Run \code{\link{getSchemaForObjects}}('AccountsPayableInvoiceGroup') to get more field paths.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{listSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Accounts Payable
	#' @return A dataframe or of AccountsPayableInvoiceGroup
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	getAccountsPayableInvoiceGroup <- function(AccountsPayableInvoiceGroupID, InvoiceGroupID = F, DistrictID = F, GroupIDAccount = F, Code = F, Description = F, IsActive = F, CodeDescription = F, IsApprovalWorkflowUpdated = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, CurrentUserHasInvoiceGroupAccess = F, ActivityAccountingGroupID = F, IsActivityAccounting = F, fieldPaths = NULL, entityId = 1, schoolYearId = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment()) %>% purrr::keep(names(.) != "AccountsPayableInvoiceGroupID")

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		getSkyObject(module = "AccountsPayable", objectName = "InvoiceGroup", objectId = AccountsPayableInvoiceGroupID, searchFields = searchFields, entityId = entityId, flatten = flatten, returnResponse = returnResponse)
	}

	#' Delete an AccountsPayableInvoiceGroup
	#'
	#' This function deletes an AccountsPayableInvoiceGroup
	#' @param AccountsPayableInvoiceGroupID The ID of the AccountsPayableInvoiceGroup to delete
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{listSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Accounts Payable
	#' @return The AccountsPayableInvoiceGroupID of the deleted AccountsPayableInvoiceGroup.
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	deleteAccountsPayableInvoiceGroup <- function(AccountsPayableInvoiceGroupID, ignoreWarnings = F, entityId = 1, schoolYearId = NULL, flatten = T, returnResponse = F){

		deleteSkyObject(module = "AccountsPayable", objectName = "InvoiceGroup", objectId = AccountsPayableInvoiceGroupID, ignoreWarnings = ignoreWarnings, entityId = entityId, flatten = flatten, returnResponse = returnResponse)
	}

	#' Create an AccountsPayableInvoiceGroup
	#'
	#' This function creates an AccountsPayableInvoiceGroup
	#' @param fieldNames The field values to give the created AccountsPayableInvoiceGroup. Each defaults to NULL.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{listSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Accounts Payable
	#' @return A newly created AccountsPayableInvoiceGroup
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	createAccountsPayableInvoiceGroup <- function(DistrictID = NULL, GroupIDAccount = NULL, Code = NULL, Description = NULL, IsActive = NULL, ActivityAccountingGroupID = NULL, entityId = 1, schoolYearId = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment())

		body <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper()) %>% compact()

		createSkyObject(module = "AccountsPayable", objectName = "InvoiceGroup", body = list(DataObject = body), searchFields = append("InvoiceGroupID", body %>% names()), entityId = entityId, schoolYearId = schoolYearId, flatten = flatten, returnResponse = returnResponse)
	}

	#' Modify an AccountsPayableInvoiceGroup
	#'
	#' This function modifies an AccountsPayableInvoiceGroup
	#' @param fieldNames The field values to give the modified AccountsPayableInvoiceGroup. Each defaults to NULL.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{listSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Accounts Payable
	#' @return The modified AccountsPayableInvoiceGroup
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	modifyAccountsPayableInvoiceGroup <- function(InvoiceGroupID, DistrictID = NULL, GroupIDAccount = NULL, Code = NULL, Description = NULL, IsActive = NULL, ActivityAccountingGroupID = NULL, entityId = 1, schoolYearId = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment())

		body <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper()) %>% compact()

		modifySkyObject(module = "AccountsPayable", objectName = "InvoiceGroup", objectId = InvoiceGroupID, body = list(DataObject = body), searchFields = append("InvoiceGroupID", body %>% names()), entityId = entityId, schoolYearId = schoolYearId, flatten = flatten, returnResponse = returnResponse)
	}

	#' List InvoiceGroupApprovalTasks
	#'
	#' This function returns a dataframe or json object of InvoiceGroupApprovalTasks
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given InvoiceGroupApprovalTasks. Defaults to FALSE for all return fields which, for convenience, returns all fields for the InvoiceGroupApprovalTasks.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object listed as text. Run \code{\link{getSchemaForObjects}}('InvoiceGroupApprovalTask') to get more field paths.
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
	#' @concept Accounts Payable
	#' @return A list of InvoiceGroupApprovalTasks
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	listInvoiceGroupApprovalTasks <- function(searchConditionsList = NULL, InvoiceGroupApprovalTaskID = F, InvoiceGroupID = F, IsConditional = F, Level = F, Description = F, XMLFilter = F, SecurityStandardFilterData = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, fieldPaths = NULL, searchConditionsGroupType = "And", searchSortFieldNamesList = NULL, searchSortFieldNamesDescendingList = NULL, entityId = 1, schoolYearId = NULL, page = 1, pageSize = 100000, flatten = T, returnResponse = F){

		params <- as.list(environment())

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		listSkyObjects(module = "AccountsPayable", objectName = "InvoiceGroupApprovalTask", searchFields = searchFields %>% append(fieldPaths), page = page, pageSize = pageSize, SearchConditionsList = searchConditionsList, SearchConditionsGroupType = searchConditionsGroupType, SearchSortFieldNamesList = searchSortFieldNamesList, SearchSortFieldNamesDescendingList = searchSortFieldNamesDescendingList, entityId = entityId, schoolYearId = schoolYearId, flatten = flatten, returnResponse = returnResponse)
	}

	#' Get an InvoiceGroupApprovalTask
	#'
	#' This function returns a dataframe or json object of an InvoiceGroupApprovalTask
	#' @param InvoiceGroupApprovalTaskID The ID of the InvoiceGroupApprovalTask to return.
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given InvoiceGroupApprovalTask. Defaults to FALSE for all return fields which, for convenience, returns all fields for the InvoiceGroupApprovalTask.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object listed as text. Run \code{\link{getSchemaForObjects}}('InvoiceGroupApprovalTask') to get more field paths.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{listSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Accounts Payable
	#' @return A dataframe or of InvoiceGroupApprovalTask
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	getInvoiceGroupApprovalTask <- function(InvoiceGroupApprovalTaskID, InvoiceGroupID = F, IsConditional = F, Level = F, Description = F, XMLFilter = F, SecurityStandardFilterData = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, fieldPaths = NULL, entityId = 1, schoolYearId = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment()) %>% purrr::keep(names(.) != "InvoiceGroupApprovalTaskID")

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		getSkyObject(module = "AccountsPayable", objectName = "InvoiceGroupApprovalTask", objectId = InvoiceGroupApprovalTaskID, searchFields = searchFields, entityId = entityId, flatten = flatten, returnResponse = returnResponse)
	}

	#' Delete an InvoiceGroupApprovalTask
	#'
	#' This function deletes an InvoiceGroupApprovalTask
	#' @param InvoiceGroupApprovalTaskID The ID of the InvoiceGroupApprovalTask to delete
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{listSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Accounts Payable
	#' @return The InvoiceGroupApprovalTaskID of the deleted InvoiceGroupApprovalTask.
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	deleteInvoiceGroupApprovalTask <- function(InvoiceGroupApprovalTaskID, ignoreWarnings = F, entityId = 1, schoolYearId = NULL, flatten = T, returnResponse = F){

		deleteSkyObject(module = "AccountsPayable", objectName = "InvoiceGroupApprovalTask", objectId = InvoiceGroupApprovalTaskID, ignoreWarnings = ignoreWarnings, entityId = entityId, flatten = flatten, returnResponse = returnResponse)
	}

	#' Create an InvoiceGroupApprovalTask
	#'
	#' This function creates an InvoiceGroupApprovalTask
	#' @param fieldNames The field values to give the created InvoiceGroupApprovalTask. Each defaults to NULL.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{listSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Accounts Payable
	#' @return A newly created InvoiceGroupApprovalTask
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	createInvoiceGroupApprovalTask <- function(InvoiceGroupID = NULL, IsConditional = NULL, Level = NULL, Description = NULL, XMLFilter = NULL, entityId = 1, schoolYearId = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment())

		body <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper()) %>% compact()

		createSkyObject(module = "AccountsPayable", objectName = "InvoiceGroupApprovalTask", body = list(DataObject = body), searchFields = append("InvoiceGroupApprovalTaskID", body %>% names()), entityId = entityId, schoolYearId = schoolYearId, flatten = flatten, returnResponse = returnResponse)
	}

	#' Modify an InvoiceGroupApprovalTask
	#'
	#' This function modifies an InvoiceGroupApprovalTask
	#' @param fieldNames The field values to give the modified InvoiceGroupApprovalTask. Each defaults to NULL.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{listSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Accounts Payable
	#' @return The modified InvoiceGroupApprovalTask
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	modifyInvoiceGroupApprovalTask <- function(InvoiceGroupApprovalTaskID, InvoiceGroupID = NULL, IsConditional = NULL, Level = NULL, Description = NULL, XMLFilter = NULL, entityId = 1, schoolYearId = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment())

		body <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper()) %>% compact()

		modifySkyObject(module = "AccountsPayable", objectName = "InvoiceGroupApprovalTask", objectId = InvoiceGroupApprovalTaskID, body = list(DataObject = body), searchFields = append("InvoiceGroupApprovalTaskID", body %>% names()), entityId = entityId, schoolYearId = schoolYearId, flatten = flatten, returnResponse = returnResponse)
	}

	#' List InvoiceGroupApprovalTaskSecurityGroups
	#'
	#' This function returns a dataframe or json object of InvoiceGroupApprovalTaskSecurityGroups
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given InvoiceGroupApprovalTaskSecurityGroups. Defaults to FALSE for all return fields which, for convenience, returns all fields for the InvoiceGroupApprovalTaskSecurityGroups.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object listed as text. Run \code{\link{getSchemaForObjects}}('InvoiceGroupApprovalTaskSecurityGroup') to get more field paths.
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
	#' @concept Accounts Payable
	#' @return A list of InvoiceGroupApprovalTaskSecurityGroups
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	listInvoiceGroupApprovalTaskSecurityGroups <- function(searchConditionsList = NULL, InvoiceGroupApprovalTaskSecurityGroupID = F, InvoiceGroupApprovalTaskID = F, GroupIDSecurity = F, IsAlternate = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, fieldPaths = NULL, searchConditionsGroupType = "And", searchSortFieldNamesList = NULL, searchSortFieldNamesDescendingList = NULL, entityId = 1, schoolYearId = NULL, page = 1, pageSize = 100000, flatten = T, returnResponse = F){

		params <- as.list(environment())

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		listSkyObjects(module = "AccountsPayable", objectName = "InvoiceGroupApprovalTaskSecurityGroup", searchFields = searchFields %>% append(fieldPaths), page = page, pageSize = pageSize, SearchConditionsList = searchConditionsList, SearchConditionsGroupType = searchConditionsGroupType, SearchSortFieldNamesList = searchSortFieldNamesList, SearchSortFieldNamesDescendingList = searchSortFieldNamesDescendingList, entityId = entityId, schoolYearId = schoolYearId, flatten = flatten, returnResponse = returnResponse)
	}

	#' Get an InvoiceGroupApprovalTaskSecurityGroup
	#'
	#' This function returns a dataframe or json object of an InvoiceGroupApprovalTaskSecurityGroup
	#' @param InvoiceGroupApprovalTaskSecurityGroupID The ID of the InvoiceGroupApprovalTaskSecurityGroup to return.
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given InvoiceGroupApprovalTaskSecurityGroup. Defaults to FALSE for all return fields which, for convenience, returns all fields for the InvoiceGroupApprovalTaskSecurityGroup.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object listed as text. Run \code{\link{getSchemaForObjects}}('InvoiceGroupApprovalTaskSecurityGroup') to get more field paths.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{listSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Accounts Payable
	#' @return A dataframe or of InvoiceGroupApprovalTaskSecurityGroup
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	getInvoiceGroupApprovalTaskSecurityGroup <- function(InvoiceGroupApprovalTaskSecurityGroupID, InvoiceGroupApprovalTaskID = F, GroupIDSecurity = F, IsAlternate = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, fieldPaths = NULL, entityId = 1, schoolYearId = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment()) %>% purrr::keep(names(.) != "InvoiceGroupApprovalTaskSecurityGroupID")

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		getSkyObject(module = "AccountsPayable", objectName = "InvoiceGroupApprovalTaskSecurityGroup", objectId = InvoiceGroupApprovalTaskSecurityGroupID, searchFields = searchFields, entityId = entityId, flatten = flatten, returnResponse = returnResponse)
	}

	#' Delete an InvoiceGroupApprovalTaskSecurityGroup
	#'
	#' This function deletes an InvoiceGroupApprovalTaskSecurityGroup
	#' @param InvoiceGroupApprovalTaskSecurityGroupID The ID of the InvoiceGroupApprovalTaskSecurityGroup to delete
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{listSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Accounts Payable
	#' @return The InvoiceGroupApprovalTaskSecurityGroupID of the deleted InvoiceGroupApprovalTaskSecurityGroup.
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	deleteInvoiceGroupApprovalTaskSecurityGroup <- function(InvoiceGroupApprovalTaskSecurityGroupID, ignoreWarnings = F, entityId = 1, schoolYearId = NULL, flatten = T, returnResponse = F){

		deleteSkyObject(module = "AccountsPayable", objectName = "InvoiceGroupApprovalTaskSecurityGroup", objectId = InvoiceGroupApprovalTaskSecurityGroupID, ignoreWarnings = ignoreWarnings, entityId = entityId, flatten = flatten, returnResponse = returnResponse)
	}

	#' Create an InvoiceGroupApprovalTaskSecurityGroup
	#'
	#' This function creates an InvoiceGroupApprovalTaskSecurityGroup
	#' @param fieldNames The field values to give the created InvoiceGroupApprovalTaskSecurityGroup. Each defaults to NULL.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{listSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Accounts Payable
	#' @return A newly created InvoiceGroupApprovalTaskSecurityGroup
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	createInvoiceGroupApprovalTaskSecurityGroup <- function(InvoiceGroupApprovalTaskID = NULL, GroupIDSecurity = NULL, IsAlternate = NULL, entityId = 1, schoolYearId = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment())

		body <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper()) %>% compact()

		createSkyObject(module = "AccountsPayable", objectName = "InvoiceGroupApprovalTaskSecurityGroup", body = list(DataObject = body), searchFields = append("InvoiceGroupApprovalTaskSecurityGroupID", body %>% names()), entityId = entityId, schoolYearId = schoolYearId, flatten = flatten, returnResponse = returnResponse)
	}

	#' Modify an InvoiceGroupApprovalTaskSecurityGroup
	#'
	#' This function modifies an InvoiceGroupApprovalTaskSecurityGroup
	#' @param fieldNames The field values to give the modified InvoiceGroupApprovalTaskSecurityGroup. Each defaults to NULL.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{listSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Accounts Payable
	#' @return The modified InvoiceGroupApprovalTaskSecurityGroup
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	modifyInvoiceGroupApprovalTaskSecurityGroup <- function(InvoiceGroupApprovalTaskSecurityGroupID, InvoiceGroupApprovalTaskID = NULL, GroupIDSecurity = NULL, IsAlternate = NULL, entityId = 1, schoolYearId = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment())

		body <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper()) %>% compact()

		modifySkyObject(module = "AccountsPayable", objectName = "InvoiceGroupApprovalTaskSecurityGroup", objectId = InvoiceGroupApprovalTaskSecurityGroupID, body = list(DataObject = body), searchFields = append("InvoiceGroupApprovalTaskSecurityGroupID", body %>% names()), entityId = entityId, schoolYearId = schoolYearId, flatten = flatten, returnResponse = returnResponse)
	}

	#' List InvoiceGroupBankAccounts
	#'
	#' This function returns a dataframe or json object of InvoiceGroupBankAccounts
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given InvoiceGroupBankAccounts. Defaults to FALSE for all return fields which, for convenience, returns all fields for the InvoiceGroupBankAccounts.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object listed as text. Run \code{\link{getSchemaForObjects}}('InvoiceGroupBankAccount') to get more field paths.
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
	#' @concept Accounts Payable
	#' @return A list of InvoiceGroupBankAccounts
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	listInvoiceGroupBankAccounts <- function(searchConditionsList = NULL, InvoiceGroupBankAccountID = F, InvoiceGroupID = F, BankAccountID = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, fieldPaths = NULL, searchConditionsGroupType = "And", searchSortFieldNamesList = NULL, searchSortFieldNamesDescendingList = NULL, entityId = 1, schoolYearId = NULL, page = 1, pageSize = 100000, flatten = T, returnResponse = F){

		params <- as.list(environment())

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		listSkyObjects(module = "AccountsPayable", objectName = "InvoiceGroupBankAccount", searchFields = searchFields %>% append(fieldPaths), page = page, pageSize = pageSize, SearchConditionsList = searchConditionsList, SearchConditionsGroupType = searchConditionsGroupType, SearchSortFieldNamesList = searchSortFieldNamesList, SearchSortFieldNamesDescendingList = searchSortFieldNamesDescendingList, entityId = entityId, schoolYearId = schoolYearId, flatten = flatten, returnResponse = returnResponse)
	}

	#' Get an InvoiceGroupBankAccount
	#'
	#' This function returns a dataframe or json object of an InvoiceGroupBankAccount
	#' @param InvoiceGroupBankAccountID The ID of the InvoiceGroupBankAccount to return.
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given InvoiceGroupBankAccount. Defaults to FALSE for all return fields which, for convenience, returns all fields for the InvoiceGroupBankAccount.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object listed as text. Run \code{\link{getSchemaForObjects}}('InvoiceGroupBankAccount') to get more field paths.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{listSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Accounts Payable
	#' @return A dataframe or of InvoiceGroupBankAccount
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	getInvoiceGroupBankAccount <- function(InvoiceGroupBankAccountID, InvoiceGroupID = F, BankAccountID = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, fieldPaths = NULL, entityId = 1, schoolYearId = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment()) %>% purrr::keep(names(.) != "InvoiceGroupBankAccountID")

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		getSkyObject(module = "AccountsPayable", objectName = "InvoiceGroupBankAccount", objectId = InvoiceGroupBankAccountID, searchFields = searchFields, entityId = entityId, flatten = flatten, returnResponse = returnResponse)
	}

	#' Delete an InvoiceGroupBankAccount
	#'
	#' This function deletes an InvoiceGroupBankAccount
	#' @param InvoiceGroupBankAccountID The ID of the InvoiceGroupBankAccount to delete
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{listSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Accounts Payable
	#' @return The InvoiceGroupBankAccountID of the deleted InvoiceGroupBankAccount.
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	deleteInvoiceGroupBankAccount <- function(InvoiceGroupBankAccountID, ignoreWarnings = F, entityId = 1, schoolYearId = NULL, flatten = T, returnResponse = F){

		deleteSkyObject(module = "AccountsPayable", objectName = "InvoiceGroupBankAccount", objectId = InvoiceGroupBankAccountID, ignoreWarnings = ignoreWarnings, entityId = entityId, flatten = flatten, returnResponse = returnResponse)
	}

	#' Create an InvoiceGroupBankAccount
	#'
	#' This function creates an InvoiceGroupBankAccount
	#' @param fieldNames The field values to give the created InvoiceGroupBankAccount. Each defaults to NULL.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{listSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Accounts Payable
	#' @return A newly created InvoiceGroupBankAccount
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	createInvoiceGroupBankAccount <- function(InvoiceGroupID = NULL, BankAccountID = NULL, entityId = 1, schoolYearId = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment())

		body <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper()) %>% compact()

		createSkyObject(module = "AccountsPayable", objectName = "InvoiceGroupBankAccount", body = list(DataObject = body), searchFields = append("InvoiceGroupBankAccountID", body %>% names()), entityId = entityId, schoolYearId = schoolYearId, flatten = flatten, returnResponse = returnResponse)
	}

	#' Modify an InvoiceGroupBankAccount
	#'
	#' This function modifies an InvoiceGroupBankAccount
	#' @param fieldNames The field values to give the modified InvoiceGroupBankAccount. Each defaults to NULL.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{listSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Accounts Payable
	#' @return The modified InvoiceGroupBankAccount
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	modifyInvoiceGroupBankAccount <- function(InvoiceGroupBankAccountID, InvoiceGroupID = NULL, BankAccountID = NULL, entityId = 1, schoolYearId = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment())

		body <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper()) %>% compact()

		modifySkyObject(module = "AccountsPayable", objectName = "InvoiceGroupBankAccount", objectId = InvoiceGroupBankAccountID, body = list(DataObject = body), searchFields = append("InvoiceGroupBankAccountID", body %>% names()), entityId = entityId, schoolYearId = schoolYearId, flatten = flatten, returnResponse = returnResponse)
	}

	#' List AccountsPayableInvoiceGroupClearances
	#'
	#' This function returns a dataframe or json object of AccountsPayableInvoiceGroupClearances
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given AccountsPayableInvoiceGroupClearances. Defaults to FALSE for all return fields which, for convenience, returns all fields for the AccountsPayableInvoiceGroupClearances.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object listed as text. Run \code{\link{getSchemaForObjects}}('AccountsPayableInvoiceGroupClearance') to get more field paths.
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
	#' @concept Accounts Payable
	#' @return A list of AccountsPayableInvoiceGroupClearances
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	listAccountsPayableInvoiceGroupClearances <- function(searchConditionsList = NULL, InvoiceGroupClearanceID = F, InvoiceGroupID = F, GroupIDSecurity = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, fieldPaths = NULL, searchConditionsGroupType = "And", searchSortFieldNamesList = NULL, searchSortFieldNamesDescendingList = NULL, entityId = 1, schoolYearId = NULL, page = 1, pageSize = 100000, flatten = T, returnResponse = F){

		params <- as.list(environment())

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		listSkyObjects(module = "AccountsPayable", objectName = "InvoiceGroupClearance", searchFields = searchFields %>% append(fieldPaths), page = page, pageSize = pageSize, SearchConditionsList = searchConditionsList, SearchConditionsGroupType = searchConditionsGroupType, SearchSortFieldNamesList = searchSortFieldNamesList, SearchSortFieldNamesDescendingList = searchSortFieldNamesDescendingList, entityId = entityId, schoolYearId = schoolYearId, flatten = flatten, returnResponse = returnResponse)
	}

	#' Get an AccountsPayableInvoiceGroupClearance
	#'
	#' This function returns a dataframe or json object of an AccountsPayableInvoiceGroupClearance
	#' @param AccountsPayableInvoiceGroupClearanceID The ID of the AccountsPayableInvoiceGroupClearance to return.
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given AccountsPayableInvoiceGroupClearance. Defaults to FALSE for all return fields which, for convenience, returns all fields for the AccountsPayableInvoiceGroupClearance.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object listed as text. Run \code{\link{getSchemaForObjects}}('AccountsPayableInvoiceGroupClearance') to get more field paths.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{listSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Accounts Payable
	#' @return A dataframe or of AccountsPayableInvoiceGroupClearance
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	getAccountsPayableInvoiceGroupClearance <- function(AccountsPayableInvoiceGroupClearanceID, InvoiceGroupClearanceID = F, InvoiceGroupID = F, GroupIDSecurity = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, fieldPaths = NULL, entityId = 1, schoolYearId = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment()) %>% purrr::keep(names(.) != "AccountsPayableInvoiceGroupClearanceID")

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		getSkyObject(module = "AccountsPayable", objectName = "InvoiceGroupClearance", objectId = AccountsPayableInvoiceGroupClearanceID, searchFields = searchFields, entityId = entityId, flatten = flatten, returnResponse = returnResponse)
	}

	#' Delete an AccountsPayableInvoiceGroupClearance
	#'
	#' This function deletes an AccountsPayableInvoiceGroupClearance
	#' @param AccountsPayableInvoiceGroupClearanceID The ID of the AccountsPayableInvoiceGroupClearance to delete
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{listSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Accounts Payable
	#' @return The AccountsPayableInvoiceGroupClearanceID of the deleted AccountsPayableInvoiceGroupClearance.
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	deleteAccountsPayableInvoiceGroupClearance <- function(AccountsPayableInvoiceGroupClearanceID, ignoreWarnings = F, entityId = 1, schoolYearId = NULL, flatten = T, returnResponse = F){

		deleteSkyObject(module = "AccountsPayable", objectName = "InvoiceGroupClearance", objectId = AccountsPayableInvoiceGroupClearanceID, ignoreWarnings = ignoreWarnings, entityId = entityId, flatten = flatten, returnResponse = returnResponse)
	}

	#' Create an AccountsPayableInvoiceGroupClearance
	#'
	#' This function creates an AccountsPayableInvoiceGroupClearance
	#' @param fieldNames The field values to give the created AccountsPayableInvoiceGroupClearance. Each defaults to NULL.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{listSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Accounts Payable
	#' @return A newly created AccountsPayableInvoiceGroupClearance
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	createAccountsPayableInvoiceGroupClearance <- function(InvoiceGroupID = NULL, GroupIDSecurity = NULL, entityId = 1, schoolYearId = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment())

		body <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper()) %>% compact()

		createSkyObject(module = "AccountsPayable", objectName = "InvoiceGroupClearance", body = list(DataObject = body), searchFields = append("InvoiceGroupClearanceID", body %>% names()), entityId = entityId, schoolYearId = schoolYearId, flatten = flatten, returnResponse = returnResponse)
	}

	#' Modify an AccountsPayableInvoiceGroupClearance
	#'
	#' This function modifies an AccountsPayableInvoiceGroupClearance
	#' @param fieldNames The field values to give the modified AccountsPayableInvoiceGroupClearance. Each defaults to NULL.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{listSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Accounts Payable
	#' @return The modified AccountsPayableInvoiceGroupClearance
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	modifyAccountsPayableInvoiceGroupClearance <- function(InvoiceGroupClearanceID, InvoiceGroupID = NULL, GroupIDSecurity = NULL, entityId = 1, schoolYearId = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment())

		body <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper()) %>% compact()

		modifySkyObject(module = "AccountsPayable", objectName = "InvoiceGroupClearance", objectId = InvoiceGroupClearanceID, body = list(DataObject = body), searchFields = append("InvoiceGroupClearanceID", body %>% names()), entityId = entityId, schoolYearId = schoolYearId, flatten = flatten, returnResponse = returnResponse)
	}

	#' List CreditCardTransactionApprovals
	#'
	#' This function returns a dataframe or json object of CreditCardTransactionApprovals
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given CreditCardTransactionApprovals. Defaults to FALSE for all return fields which, for convenience, returns all fields for the CreditCardTransactionApprovals.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object listed as text. Run \code{\link{getSchemaForObjects}}('CreditCardTransactionApproval') to get more field paths.
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
	#' @concept Accounts Payable
	#' @return A list of CreditCardTransactionApprovals
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	listCreditCardTransactionApprovals <- function(searchConditionsList = NULL, CreditCardTransactionApprovalID = F, CreditCardTransactionID = F, UserIDApprover = F, Comment = F, TaskInstanceID = F, Status = F, Level = F, Type = F, LevelDescription = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, ApprovalActionTime = F, fieldPaths = NULL, searchConditionsGroupType = "And", searchSortFieldNamesList = NULL, searchSortFieldNamesDescendingList = NULL, entityId = 1, schoolYearId = NULL, page = 1, pageSize = 100000, flatten = T, returnResponse = F){

		params <- as.list(environment())

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		listSkyObjects(module = "AccountsPayable", objectName = "CreditCardTransactionApproval", searchFields = searchFields %>% append(fieldPaths), page = page, pageSize = pageSize, SearchConditionsList = searchConditionsList, SearchConditionsGroupType = searchConditionsGroupType, SearchSortFieldNamesList = searchSortFieldNamesList, SearchSortFieldNamesDescendingList = searchSortFieldNamesDescendingList, entityId = entityId, schoolYearId = schoolYearId, flatten = flatten, returnResponse = returnResponse)
	}

	#' Get a CreditCardTransactionApproval
	#'
	#' This function returns a dataframe or json object of a CreditCardTransactionApproval
	#' @param CreditCardTransactionApprovalID The ID of the CreditCardTransactionApproval to return.
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given CreditCardTransactionApproval. Defaults to FALSE for all return fields which, for convenience, returns all fields for the CreditCardTransactionApproval.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object listed as text. Run \code{\link{getSchemaForObjects}}('CreditCardTransactionApproval') to get more field paths.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{listSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Accounts Payable
	#' @return A dataframe or of CreditCardTransactionApproval
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	getCreditCardTransactionApproval <- function(CreditCardTransactionApprovalID, CreditCardTransactionID = F, UserIDApprover = F, Comment = F, TaskInstanceID = F, Status = F, Level = F, Type = F, LevelDescription = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, ApprovalActionTime = F, fieldPaths = NULL, entityId = 1, schoolYearId = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment()) %>% purrr::keep(names(.) != "CreditCardTransactionApprovalID")

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		getSkyObject(module = "AccountsPayable", objectName = "CreditCardTransactionApproval", objectId = CreditCardTransactionApprovalID, searchFields = searchFields, entityId = entityId, flatten = flatten, returnResponse = returnResponse)
	}

	#' Delete a CreditCardTransactionApproval
	#'
	#' This function deletes a CreditCardTransactionApproval
	#' @param CreditCardTransactionApprovalID The ID of the CreditCardTransactionApproval to delete
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{listSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Accounts Payable
	#' @return The CreditCardTransactionApprovalID of the deleted CreditCardTransactionApproval.
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	deleteCreditCardTransactionApproval <- function(CreditCardTransactionApprovalID, ignoreWarnings = F, entityId = 1, schoolYearId = NULL, flatten = T, returnResponse = F){

		deleteSkyObject(module = "AccountsPayable", objectName = "CreditCardTransactionApproval", objectId = CreditCardTransactionApprovalID, ignoreWarnings = ignoreWarnings, entityId = entityId, flatten = flatten, returnResponse = returnResponse)
	}

	#' Create a CreditCardTransactionApproval
	#'
	#' This function creates a CreditCardTransactionApproval
	#' @param fieldNames The field values to give the created CreditCardTransactionApproval. Each defaults to NULL.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{listSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Accounts Payable
	#' @return A newly created CreditCardTransactionApproval
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	createCreditCardTransactionApproval <- function(CreditCardTransactionID = NULL, UserIDApprover = NULL, Comment = NULL, TaskInstanceID = NULL, Status = NULL, Level = NULL, Type = NULL, LevelDescription = NULL, ApprovalActionTime = NULL, entityId = 1, schoolYearId = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment())

		body <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper()) %>% compact()

		createSkyObject(module = "AccountsPayable", objectName = "CreditCardTransactionApproval", body = list(DataObject = body), searchFields = append("CreditCardTransactionApprovalID", body %>% names()), entityId = entityId, schoolYearId = schoolYearId, flatten = flatten, returnResponse = returnResponse)
	}

	#' Modify a CreditCardTransactionApproval
	#'
	#' This function modifies a CreditCardTransactionApproval
	#' @param fieldNames The field values to give the modified CreditCardTransactionApproval. Each defaults to NULL.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{listSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Accounts Payable
	#' @return The modified CreditCardTransactionApproval
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	modifyCreditCardTransactionApproval <- function(CreditCardTransactionApprovalID, CreditCardTransactionID = NULL, UserIDApprover = NULL, Comment = NULL, TaskInstanceID = NULL, Status = NULL, Level = NULL, Type = NULL, LevelDescription = NULL, ApprovalActionTime = NULL, entityId = 1, schoolYearId = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment())

		body <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper()) %>% compact()

		modifySkyObject(module = "AccountsPayable", objectName = "CreditCardTransactionApproval", objectId = CreditCardTransactionApprovalID, body = list(DataObject = body), searchFields = append("CreditCardTransactionApprovalID", body %>% names()), entityId = entityId, schoolYearId = schoolYearId, flatten = flatten, returnResponse = returnResponse)
	}

	#' List CreditCardGroupApprovalTasks
	#'
	#' This function returns a dataframe or json object of CreditCardGroupApprovalTasks
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given CreditCardGroupApprovalTasks. Defaults to FALSE for all return fields which, for convenience, returns all fields for the CreditCardGroupApprovalTasks.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object listed as text. Run \code{\link{getSchemaForObjects}}('CreditCardGroupApprovalTask') to get more field paths.
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
	#' @concept Accounts Payable
	#' @return A list of CreditCardGroupApprovalTasks
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	listCreditCardGroupApprovalTasks <- function(searchConditionsList = NULL, CreditCardGroupApprovalTaskID = F, CreditCardGroupID = F, IsConditional = F, Level = F, Description = F, IncludeCreditCardCheckoutUser = F, XMLFilter = F, SecurityStandardFilterData = F, Type = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, fieldPaths = NULL, searchConditionsGroupType = "And", searchSortFieldNamesList = NULL, searchSortFieldNamesDescendingList = NULL, entityId = 1, schoolYearId = NULL, page = 1, pageSize = 100000, flatten = T, returnResponse = F){

		params <- as.list(environment())

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		listSkyObjects(module = "AccountsPayable", objectName = "CreditCardGroupApprovalTask", searchFields = searchFields %>% append(fieldPaths), page = page, pageSize = pageSize, SearchConditionsList = searchConditionsList, SearchConditionsGroupType = searchConditionsGroupType, SearchSortFieldNamesList = searchSortFieldNamesList, SearchSortFieldNamesDescendingList = searchSortFieldNamesDescendingList, entityId = entityId, schoolYearId = schoolYearId, flatten = flatten, returnResponse = returnResponse)
	}

	#' Get a CreditCardGroupApprovalTask
	#'
	#' This function returns a dataframe or json object of a CreditCardGroupApprovalTask
	#' @param CreditCardGroupApprovalTaskID The ID of the CreditCardGroupApprovalTask to return.
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given CreditCardGroupApprovalTask. Defaults to FALSE for all return fields which, for convenience, returns all fields for the CreditCardGroupApprovalTask.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object listed as text. Run \code{\link{getSchemaForObjects}}('CreditCardGroupApprovalTask') to get more field paths.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{listSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Accounts Payable
	#' @return A dataframe or of CreditCardGroupApprovalTask
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	getCreditCardGroupApprovalTask <- function(CreditCardGroupApprovalTaskID, CreditCardGroupID = F, IsConditional = F, Level = F, Description = F, IncludeCreditCardCheckoutUser = F, XMLFilter = F, SecurityStandardFilterData = F, Type = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, fieldPaths = NULL, entityId = 1, schoolYearId = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment()) %>% purrr::keep(names(.) != "CreditCardGroupApprovalTaskID")

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		getSkyObject(module = "AccountsPayable", objectName = "CreditCardGroupApprovalTask", objectId = CreditCardGroupApprovalTaskID, searchFields = searchFields, entityId = entityId, flatten = flatten, returnResponse = returnResponse)
	}

	#' Delete a CreditCardGroupApprovalTask
	#'
	#' This function deletes a CreditCardGroupApprovalTask
	#' @param CreditCardGroupApprovalTaskID The ID of the CreditCardGroupApprovalTask to delete
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{listSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Accounts Payable
	#' @return The CreditCardGroupApprovalTaskID of the deleted CreditCardGroupApprovalTask.
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	deleteCreditCardGroupApprovalTask <- function(CreditCardGroupApprovalTaskID, ignoreWarnings = F, entityId = 1, schoolYearId = NULL, flatten = T, returnResponse = F){

		deleteSkyObject(module = "AccountsPayable", objectName = "CreditCardGroupApprovalTask", objectId = CreditCardGroupApprovalTaskID, ignoreWarnings = ignoreWarnings, entityId = entityId, flatten = flatten, returnResponse = returnResponse)
	}

	#' Create a CreditCardGroupApprovalTask
	#'
	#' This function creates a CreditCardGroupApprovalTask
	#' @param fieldNames The field values to give the created CreditCardGroupApprovalTask. Each defaults to NULL.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{listSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Accounts Payable
	#' @return A newly created CreditCardGroupApprovalTask
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	createCreditCardGroupApprovalTask <- function(CreditCardGroupID = NULL, IsConditional = NULL, Level = NULL, Description = NULL, IncludeCreditCardCheckoutUser = NULL, XMLFilter = NULL, Type = NULL, entityId = 1, schoolYearId = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment())

		body <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper()) %>% compact()

		createSkyObject(module = "AccountsPayable", objectName = "CreditCardGroupApprovalTask", body = list(DataObject = body), searchFields = append("CreditCardGroupApprovalTaskID", body %>% names()), entityId = entityId, schoolYearId = schoolYearId, flatten = flatten, returnResponse = returnResponse)
	}

	#' Modify a CreditCardGroupApprovalTask
	#'
	#' This function modifies a CreditCardGroupApprovalTask
	#' @param fieldNames The field values to give the modified CreditCardGroupApprovalTask. Each defaults to NULL.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{listSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Accounts Payable
	#' @return The modified CreditCardGroupApprovalTask
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	modifyCreditCardGroupApprovalTask <- function(CreditCardGroupApprovalTaskID, CreditCardGroupID = NULL, IsConditional = NULL, Level = NULL, Description = NULL, IncludeCreditCardCheckoutUser = NULL, XMLFilter = NULL, Type = NULL, entityId = 1, schoolYearId = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment())

		body <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper()) %>% compact()

		modifySkyObject(module = "AccountsPayable", objectName = "CreditCardGroupApprovalTask", objectId = CreditCardGroupApprovalTaskID, body = list(DataObject = body), searchFields = append("CreditCardGroupApprovalTaskID", body %>% names()), entityId = entityId, schoolYearId = schoolYearId, flatten = flatten, returnResponse = returnResponse)
	}

	#' List CreditCardGroupApprovalTaskSecurityGroups
	#'
	#' This function returns a dataframe or json object of CreditCardGroupApprovalTaskSecurityGroups
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given CreditCardGroupApprovalTaskSecurityGroups. Defaults to FALSE for all return fields which, for convenience, returns all fields for the CreditCardGroupApprovalTaskSecurityGroups.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object listed as text. Run \code{\link{getSchemaForObjects}}('CreditCardGroupApprovalTaskSecurityGroup') to get more field paths.
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
	#' @concept Accounts Payable
	#' @return A list of CreditCardGroupApprovalTaskSecurityGroups
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	listCreditCardGroupApprovalTaskSecurityGroups <- function(searchConditionsList = NULL, CreditCardGroupApprovalTaskSecurityGroupID = F, CreditCardGroupApprovalTaskID = F, GroupIDSecurity = F, IsAlternate = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, fieldPaths = NULL, searchConditionsGroupType = "And", searchSortFieldNamesList = NULL, searchSortFieldNamesDescendingList = NULL, entityId = 1, schoolYearId = NULL, page = 1, pageSize = 100000, flatten = T, returnResponse = F){

		params <- as.list(environment())

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		listSkyObjects(module = "AccountsPayable", objectName = "CreditCardGroupApprovalTaskSecurityGroup", searchFields = searchFields %>% append(fieldPaths), page = page, pageSize = pageSize, SearchConditionsList = searchConditionsList, SearchConditionsGroupType = searchConditionsGroupType, SearchSortFieldNamesList = searchSortFieldNamesList, SearchSortFieldNamesDescendingList = searchSortFieldNamesDescendingList, entityId = entityId, schoolYearId = schoolYearId, flatten = flatten, returnResponse = returnResponse)
	}

	#' Get a CreditCardGroupApprovalTaskSecurityGroup
	#'
	#' This function returns a dataframe or json object of a CreditCardGroupApprovalTaskSecurityGroup
	#' @param CreditCardGroupApprovalTaskSecurityGroupID The ID of the CreditCardGroupApprovalTaskSecurityGroup to return.
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given CreditCardGroupApprovalTaskSecurityGroup. Defaults to FALSE for all return fields which, for convenience, returns all fields for the CreditCardGroupApprovalTaskSecurityGroup.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object listed as text. Run \code{\link{getSchemaForObjects}}('CreditCardGroupApprovalTaskSecurityGroup') to get more field paths.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{listSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Accounts Payable
	#' @return A dataframe or of CreditCardGroupApprovalTaskSecurityGroup
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	getCreditCardGroupApprovalTaskSecurityGroup <- function(CreditCardGroupApprovalTaskSecurityGroupID, CreditCardGroupApprovalTaskID = F, GroupIDSecurity = F, IsAlternate = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, fieldPaths = NULL, entityId = 1, schoolYearId = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment()) %>% purrr::keep(names(.) != "CreditCardGroupApprovalTaskSecurityGroupID")

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		getSkyObject(module = "AccountsPayable", objectName = "CreditCardGroupApprovalTaskSecurityGroup", objectId = CreditCardGroupApprovalTaskSecurityGroupID, searchFields = searchFields, entityId = entityId, flatten = flatten, returnResponse = returnResponse)
	}

	#' Delete a CreditCardGroupApprovalTaskSecurityGroup
	#'
	#' This function deletes a CreditCardGroupApprovalTaskSecurityGroup
	#' @param CreditCardGroupApprovalTaskSecurityGroupID The ID of the CreditCardGroupApprovalTaskSecurityGroup to delete
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{listSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Accounts Payable
	#' @return The CreditCardGroupApprovalTaskSecurityGroupID of the deleted CreditCardGroupApprovalTaskSecurityGroup.
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	deleteCreditCardGroupApprovalTaskSecurityGroup <- function(CreditCardGroupApprovalTaskSecurityGroupID, ignoreWarnings = F, entityId = 1, schoolYearId = NULL, flatten = T, returnResponse = F){

		deleteSkyObject(module = "AccountsPayable", objectName = "CreditCardGroupApprovalTaskSecurityGroup", objectId = CreditCardGroupApprovalTaskSecurityGroupID, ignoreWarnings = ignoreWarnings, entityId = entityId, flatten = flatten, returnResponse = returnResponse)
	}

	#' Create a CreditCardGroupApprovalTaskSecurityGroup
	#'
	#' This function creates a CreditCardGroupApprovalTaskSecurityGroup
	#' @param fieldNames The field values to give the created CreditCardGroupApprovalTaskSecurityGroup. Each defaults to NULL.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{listSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Accounts Payable
	#' @return A newly created CreditCardGroupApprovalTaskSecurityGroup
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	createCreditCardGroupApprovalTaskSecurityGroup <- function(CreditCardGroupApprovalTaskID = NULL, GroupIDSecurity = NULL, IsAlternate = NULL, entityId = 1, schoolYearId = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment())

		body <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper()) %>% compact()

		createSkyObject(module = "AccountsPayable", objectName = "CreditCardGroupApprovalTaskSecurityGroup", body = list(DataObject = body), searchFields = append("CreditCardGroupApprovalTaskSecurityGroupID", body %>% names()), entityId = entityId, schoolYearId = schoolYearId, flatten = flatten, returnResponse = returnResponse)
	}

	#' Modify a CreditCardGroupApprovalTaskSecurityGroup
	#'
	#' This function modifies a CreditCardGroupApprovalTaskSecurityGroup
	#' @param fieldNames The field values to give the modified CreditCardGroupApprovalTaskSecurityGroup. Each defaults to NULL.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{listSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Accounts Payable
	#' @return The modified CreditCardGroupApprovalTaskSecurityGroup
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	modifyCreditCardGroupApprovalTaskSecurityGroup <- function(CreditCardGroupApprovalTaskSecurityGroupID, CreditCardGroupApprovalTaskID = NULL, GroupIDSecurity = NULL, IsAlternate = NULL, entityId = 1, schoolYearId = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment())

		body <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper()) %>% compact()

		modifySkyObject(module = "AccountsPayable", objectName = "CreditCardGroupApprovalTaskSecurityGroup", objectId = CreditCardGroupApprovalTaskSecurityGroupID, body = list(DataObject = body), searchFields = append("CreditCardGroupApprovalTaskSecurityGroupID", body %>% names()), entityId = entityId, schoolYearId = schoolYearId, flatten = flatten, returnResponse = returnResponse)
	}

	#' List TempInvoiceSubmitErrors
	#'
	#' This function returns a dataframe or json object of TempInvoiceSubmitErrors
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given TempInvoiceSubmitErrors. Defaults to FALSE for all return fields which, for convenience, returns all fields for the TempInvoiceSubmitErrors.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object listed as text. Run \code{\link{getSchemaForObjects}}('TempInvoiceSubmitError') to get more field paths.
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
	#' @concept Accounts Payable
	#' @return A list of TempInvoiceSubmitErrors
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	listTempInvoiceSubmitErrors <- function(searchConditionsList = NULL, TempInvoiceSubmitErrorID = F, VendorID = F, VendorName = F, Description = F, InvoiceNumber = F, Batch = F, Amount = F, FiscalYear = F, PurchaseOrderNumber = F, InvoiceGroup = F, ErrorMessage = F, Severity = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, fieldPaths = NULL, searchConditionsGroupType = "And", searchSortFieldNamesList = NULL, searchSortFieldNamesDescendingList = NULL, entityId = 1, schoolYearId = NULL, page = 1, pageSize = 100000, flatten = T, returnResponse = F){

		params <- as.list(environment())

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		listSkyObjects(module = "AccountsPayable", objectName = "TempInvoiceSubmitError", searchFields = searchFields %>% append(fieldPaths), page = page, pageSize = pageSize, SearchConditionsList = searchConditionsList, SearchConditionsGroupType = searchConditionsGroupType, SearchSortFieldNamesList = searchSortFieldNamesList, SearchSortFieldNamesDescendingList = searchSortFieldNamesDescendingList, entityId = entityId, schoolYearId = schoolYearId, flatten = flatten, returnResponse = returnResponse)
	}

	#' Get a TempInvoiceSubmitError
	#'
	#' This function returns a dataframe or json object of a TempInvoiceSubmitError
	#' @param TempInvoiceSubmitErrorID The ID of the TempInvoiceSubmitError to return.
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given TempInvoiceSubmitError. Defaults to FALSE for all return fields which, for convenience, returns all fields for the TempInvoiceSubmitError.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object listed as text. Run \code{\link{getSchemaForObjects}}('TempInvoiceSubmitError') to get more field paths.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{listSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Accounts Payable
	#' @return A dataframe or of TempInvoiceSubmitError
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	getTempInvoiceSubmitError <- function(TempInvoiceSubmitErrorID, VendorID = F, VendorName = F, Description = F, InvoiceNumber = F, Batch = F, Amount = F, FiscalYear = F, PurchaseOrderNumber = F, InvoiceGroup = F, ErrorMessage = F, Severity = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, fieldPaths = NULL, entityId = 1, schoolYearId = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment()) %>% purrr::keep(names(.) != "TempInvoiceSubmitErrorID")

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		getSkyObject(module = "AccountsPayable", objectName = "TempInvoiceSubmitError", objectId = TempInvoiceSubmitErrorID, searchFields = searchFields, entityId = entityId, flatten = flatten, returnResponse = returnResponse)
	}

	#' Delete a TempInvoiceSubmitError
	#'
	#' This function deletes a TempInvoiceSubmitError
	#' @param TempInvoiceSubmitErrorID The ID of the TempInvoiceSubmitError to delete
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{listSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Accounts Payable
	#' @return The TempInvoiceSubmitErrorID of the deleted TempInvoiceSubmitError.
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	deleteTempInvoiceSubmitError <- function(TempInvoiceSubmitErrorID, ignoreWarnings = F, entityId = 1, schoolYearId = NULL, flatten = T, returnResponse = F){

		deleteSkyObject(module = "AccountsPayable", objectName = "TempInvoiceSubmitError", objectId = TempInvoiceSubmitErrorID, ignoreWarnings = ignoreWarnings, entityId = entityId, flatten = flatten, returnResponse = returnResponse)
	}

	#' Create a TempInvoiceSubmitError
	#'
	#' This function creates a TempInvoiceSubmitError
	#' @param fieldNames The field values to give the created TempInvoiceSubmitError. Each defaults to NULL.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{listSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Accounts Payable
	#' @return A newly created TempInvoiceSubmitError
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	createTempInvoiceSubmitError <- function(VendorID = NULL, VendorName = NULL, Description = NULL, InvoiceNumber = NULL, Batch = NULL, Amount = NULL, FiscalYear = NULL, PurchaseOrderNumber = NULL, InvoiceGroup = NULL, ErrorMessage = NULL, Severity = NULL, entityId = 1, schoolYearId = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment())

		body <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper()) %>% compact()

		createSkyObject(module = "AccountsPayable", objectName = "TempInvoiceSubmitError", body = list(DataObject = body), searchFields = append("TempInvoiceSubmitErrorID", body %>% names()), entityId = entityId, schoolYearId = schoolYearId, flatten = flatten, returnResponse = returnResponse)
	}

	#' Modify a TempInvoiceSubmitError
	#'
	#' This function modifies a TempInvoiceSubmitError
	#' @param fieldNames The field values to give the modified TempInvoiceSubmitError. Each defaults to NULL.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{listSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Accounts Payable
	#' @return The modified TempInvoiceSubmitError
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	modifyTempInvoiceSubmitError <- function(TempInvoiceSubmitErrorID, VendorID = NULL, VendorName = NULL, Description = NULL, InvoiceNumber = NULL, Batch = NULL, Amount = NULL, FiscalYear = NULL, PurchaseOrderNumber = NULL, InvoiceGroup = NULL, ErrorMessage = NULL, Severity = NULL, entityId = 1, schoolYearId = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment())

		body <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper()) %>% compact()

		modifySkyObject(module = "AccountsPayable", objectName = "TempInvoiceSubmitError", objectId = TempInvoiceSubmitErrorID, body = list(DataObject = body), searchFields = append("TempInvoiceSubmitErrorID", body %>% names()), entityId = entityId, schoolYearId = schoolYearId, flatten = flatten, returnResponse = returnResponse)
	}

	#' List TempInvoiceBatches
	#'
	#' This function returns a dataframe or json object of TempInvoiceBatches
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given TempInvoiceBatches. Defaults to FALSE for all return fields which, for convenience, returns all fields for the TempInvoiceBatches.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object listed as text. Run \code{\link{getSchemaForObjects}}('TempInvoiceBatch') to get more field paths.
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
	#' @concept Accounts Payable
	#' @return A list of TempInvoiceBatches
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	listTempInvoiceBatches <- function(searchConditionsList = NULL, TempInvoiceBatchID = F, BankAccountID = F, Batch = F, RegularAmount = F, WireTransferAmount = F, ACHAmount = F, VoidAmount = F, InvoiceCount = F, FiscalYearDescription = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, ManualCheckAmount = F, EpayableCheckAmount = F, fieldPaths = NULL, searchConditionsGroupType = "And", searchSortFieldNamesList = NULL, searchSortFieldNamesDescendingList = NULL, entityId = 1, schoolYearId = NULL, page = 1, pageSize = 100000, flatten = T, returnResponse = F){

		params <- as.list(environment())

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		listSkyObjects(module = "AccountsPayable", objectName = "TempInvoiceBatch", searchFields = searchFields %>% append(fieldPaths), page = page, pageSize = pageSize, SearchConditionsList = searchConditionsList, SearchConditionsGroupType = searchConditionsGroupType, SearchSortFieldNamesList = searchSortFieldNamesList, SearchSortFieldNamesDescendingList = searchSortFieldNamesDescendingList, entityId = entityId, schoolYearId = schoolYearId, flatten = flatten, returnResponse = returnResponse)
	}

	#' Get a TempInvoiceBatch
	#'
	#' This function returns a dataframe or json object of a TempInvoiceBatch
	#' @param TempInvoiceBatchID The ID of the TempInvoiceBatch to return.
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given TempInvoiceBatch. Defaults to FALSE for all return fields which, for convenience, returns all fields for the TempInvoiceBatch.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object listed as text. Run \code{\link{getSchemaForObjects}}('TempInvoiceBatch') to get more field paths.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{listSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Accounts Payable
	#' @return A dataframe or of TempInvoiceBatch
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	getTempInvoiceBatch <- function(TempInvoiceBatchID, BankAccountID = F, Batch = F, RegularAmount = F, WireTransferAmount = F, ACHAmount = F, VoidAmount = F, InvoiceCount = F, FiscalYearDescription = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, ManualCheckAmount = F, EpayableCheckAmount = F, fieldPaths = NULL, entityId = 1, schoolYearId = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment()) %>% purrr::keep(names(.) != "TempInvoiceBatchID")

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		getSkyObject(module = "AccountsPayable", objectName = "TempInvoiceBatch", objectId = TempInvoiceBatchID, searchFields = searchFields, entityId = entityId, flatten = flatten, returnResponse = returnResponse)
	}

	#' Delete a TempInvoiceBatch
	#'
	#' This function deletes a TempInvoiceBatch
	#' @param TempInvoiceBatchID The ID of the TempInvoiceBatch to delete
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{listSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Accounts Payable
	#' @return The TempInvoiceBatchID of the deleted TempInvoiceBatch.
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	deleteTempInvoiceBatch <- function(TempInvoiceBatchID, ignoreWarnings = F, entityId = 1, schoolYearId = NULL, flatten = T, returnResponse = F){

		deleteSkyObject(module = "AccountsPayable", objectName = "TempInvoiceBatch", objectId = TempInvoiceBatchID, ignoreWarnings = ignoreWarnings, entityId = entityId, flatten = flatten, returnResponse = returnResponse)
	}

	#' Create a TempInvoiceBatch
	#'
	#' This function creates a TempInvoiceBatch
	#' @param fieldNames The field values to give the created TempInvoiceBatch. Each defaults to NULL.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{listSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Accounts Payable
	#' @return A newly created TempInvoiceBatch
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	createTempInvoiceBatch <- function(BankAccountID = NULL, Batch = NULL, RegularAmount = NULL, WireTransferAmount = NULL, ACHAmount = NULL, VoidAmount = NULL, InvoiceCount = NULL, FiscalYearDescription = NULL, ManualCheckAmount = NULL, EpayableCheckAmount = NULL, entityId = 1, schoolYearId = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment())

		body <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper()) %>% compact()

		createSkyObject(module = "AccountsPayable", objectName = "TempInvoiceBatch", body = list(DataObject = body), searchFields = append("TempInvoiceBatchID", body %>% names()), entityId = entityId, schoolYearId = schoolYearId, flatten = flatten, returnResponse = returnResponse)
	}

	#' Modify a TempInvoiceBatch
	#'
	#' This function modifies a TempInvoiceBatch
	#' @param fieldNames The field values to give the modified TempInvoiceBatch. Each defaults to NULL.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{listSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Accounts Payable
	#' @return The modified TempInvoiceBatch
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	modifyTempInvoiceBatch <- function(TempInvoiceBatchID, BankAccountID = NULL, Batch = NULL, RegularAmount = NULL, WireTransferAmount = NULL, ACHAmount = NULL, VoidAmount = NULL, InvoiceCount = NULL, FiscalYearDescription = NULL, ManualCheckAmount = NULL, EpayableCheckAmount = NULL, entityId = 1, schoolYearId = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment())

		body <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper()) %>% compact()

		modifySkyObject(module = "AccountsPayable", objectName = "TempInvoiceBatch", objectId = TempInvoiceBatchID, body = list(DataObject = body), searchFields = append("TempInvoiceBatchID", body %>% names()), entityId = entityId, schoolYearId = schoolYearId, flatten = flatten, returnResponse = returnResponse)
	}

	#' List TempInvoiceJuryDutyErrors
	#'
	#' This function returns a dataframe or json object of TempInvoiceJuryDutyErrors
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given TempInvoiceJuryDutyErrors. Defaults to FALSE for all return fields which, for convenience, returns all fields for the TempInvoiceJuryDutyErrors.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object listed as text. Run \code{\link{getSchemaForObjects}}('TempInvoiceJuryDutyError') to get more field paths.
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
	#' @concept Accounts Payable
	#' @return A list of TempInvoiceJuryDutyErrors
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	listTempInvoiceJuryDutyErrors <- function(searchConditionsList = NULL, TempInvoiceJuryDutyErrorID = F, FullName = F, ObjectName = F, ErrorMessage = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, fieldPaths = NULL, searchConditionsGroupType = "And", searchSortFieldNamesList = NULL, searchSortFieldNamesDescendingList = NULL, entityId = 1, schoolYearId = NULL, page = 1, pageSize = 100000, flatten = T, returnResponse = F){

		params <- as.list(environment())

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		listSkyObjects(module = "AccountsPayable", objectName = "TempInvoiceJuryDutyError", searchFields = searchFields %>% append(fieldPaths), page = page, pageSize = pageSize, SearchConditionsList = searchConditionsList, SearchConditionsGroupType = searchConditionsGroupType, SearchSortFieldNamesList = searchSortFieldNamesList, SearchSortFieldNamesDescendingList = searchSortFieldNamesDescendingList, entityId = entityId, schoolYearId = schoolYearId, flatten = flatten, returnResponse = returnResponse)
	}

	#' Get a TempInvoiceJuryDutyError
	#'
	#' This function returns a dataframe or json object of a TempInvoiceJuryDutyError
	#' @param TempInvoiceJuryDutyErrorID The ID of the TempInvoiceJuryDutyError to return.
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given TempInvoiceJuryDutyError. Defaults to FALSE for all return fields which, for convenience, returns all fields for the TempInvoiceJuryDutyError.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object listed as text. Run \code{\link{getSchemaForObjects}}('TempInvoiceJuryDutyError') to get more field paths.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{listSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Accounts Payable
	#' @return A dataframe or of TempInvoiceJuryDutyError
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	getTempInvoiceJuryDutyError <- function(TempInvoiceJuryDutyErrorID, FullName = F, ObjectName = F, ErrorMessage = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, fieldPaths = NULL, entityId = 1, schoolYearId = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment()) %>% purrr::keep(names(.) != "TempInvoiceJuryDutyErrorID")

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		getSkyObject(module = "AccountsPayable", objectName = "TempInvoiceJuryDutyError", objectId = TempInvoiceJuryDutyErrorID, searchFields = searchFields, entityId = entityId, flatten = flatten, returnResponse = returnResponse)
	}

	#' Delete a TempInvoiceJuryDutyError
	#'
	#' This function deletes a TempInvoiceJuryDutyError
	#' @param TempInvoiceJuryDutyErrorID The ID of the TempInvoiceJuryDutyError to delete
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{listSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Accounts Payable
	#' @return The TempInvoiceJuryDutyErrorID of the deleted TempInvoiceJuryDutyError.
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	deleteTempInvoiceJuryDutyError <- function(TempInvoiceJuryDutyErrorID, ignoreWarnings = F, entityId = 1, schoolYearId = NULL, flatten = T, returnResponse = F){

		deleteSkyObject(module = "AccountsPayable", objectName = "TempInvoiceJuryDutyError", objectId = TempInvoiceJuryDutyErrorID, ignoreWarnings = ignoreWarnings, entityId = entityId, flatten = flatten, returnResponse = returnResponse)
	}

	#' Create a TempInvoiceJuryDutyError
	#'
	#' This function creates a TempInvoiceJuryDutyError
	#' @param fieldNames The field values to give the created TempInvoiceJuryDutyError. Each defaults to NULL.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{listSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Accounts Payable
	#' @return A newly created TempInvoiceJuryDutyError
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	createTempInvoiceJuryDutyError <- function(FullName = NULL, ObjectName = NULL, ErrorMessage = NULL, entityId = 1, schoolYearId = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment())

		body <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper()) %>% compact()

		createSkyObject(module = "AccountsPayable", objectName = "TempInvoiceJuryDutyError", body = list(DataObject = body), searchFields = append("TempInvoiceJuryDutyErrorID", body %>% names()), entityId = entityId, schoolYearId = schoolYearId, flatten = flatten, returnResponse = returnResponse)
	}

	#' Modify a TempInvoiceJuryDutyError
	#'
	#' This function modifies a TempInvoiceJuryDutyError
	#' @param fieldNames The field values to give the modified TempInvoiceJuryDutyError. Each defaults to NULL.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{listSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Accounts Payable
	#' @return The modified TempInvoiceJuryDutyError
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	modifyTempInvoiceJuryDutyError <- function(TempInvoiceJuryDutyErrorID, FullName = NULL, ObjectName = NULL, ErrorMessage = NULL, entityId = 1, schoolYearId = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment())

		body <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper()) %>% compact()

		modifySkyObject(module = "AccountsPayable", objectName = "TempInvoiceJuryDutyError", objectId = TempInvoiceJuryDutyErrorID, body = list(DataObject = body), searchFields = append("TempInvoiceJuryDutyErrorID", body %>% names()), entityId = entityId, schoolYearId = schoolYearId, flatten = flatten, returnResponse = returnResponse)
	}

	#' List TempInvoiceJuryDuties
	#'
	#' This function returns a dataframe or json object of TempInvoiceJuryDuties
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given TempInvoiceJuryDuties. Defaults to FALSE for all return fields which, for convenience, returns all fields for the TempInvoiceJuryDuties.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object listed as text. Run \code{\link{getSchemaForObjects}}('TempInvoiceJuryDuty') to get more field paths.
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
	#' @concept Accounts Payable
	#' @return A list of TempInvoiceJuryDuties
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	listTempInvoiceJuryDuties <- function(searchConditionsList = NULL, TempInvoiceJuryDutyID = F, ExistingVendorText = F, ReadOnlyDemographicInfo = F, ReadOnlyVendorFields = F, VendorID = F, VendorNumber = F, NameID = F, FirstName = F, MiddleName = F, LastName = F, AddressLine1 = F, AddressLine2 = F, City = F, StateCode = F, ZipCode = F, Amount = F, SourceData = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, fieldPaths = NULL, searchConditionsGroupType = "And", searchSortFieldNamesList = NULL, searchSortFieldNamesDescendingList = NULL, entityId = 1, schoolYearId = NULL, page = 1, pageSize = 100000, flatten = T, returnResponse = F){

		params <- as.list(environment())

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		listSkyObjects(module = "AccountsPayable", objectName = "TempInvoiceJuryDuty", searchFields = searchFields %>% append(fieldPaths), page = page, pageSize = pageSize, SearchConditionsList = searchConditionsList, SearchConditionsGroupType = searchConditionsGroupType, SearchSortFieldNamesList = searchSortFieldNamesList, SearchSortFieldNamesDescendingList = searchSortFieldNamesDescendingList, entityId = entityId, schoolYearId = schoolYearId, flatten = flatten, returnResponse = returnResponse)
	}

	#' Get a TempInvoiceJuryDuty
	#'
	#' This function returns a dataframe or json object of a TempInvoiceJuryDuty
	#' @param TempInvoiceJuryDutyID The ID of the TempInvoiceJuryDuty to return.
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given TempInvoiceJuryDuty. Defaults to FALSE for all return fields which, for convenience, returns all fields for the TempInvoiceJuryDuty.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object listed as text. Run \code{\link{getSchemaForObjects}}('TempInvoiceJuryDuty') to get more field paths.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{listSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Accounts Payable
	#' @return A dataframe or of TempInvoiceJuryDuty
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	getTempInvoiceJuryDuty <- function(TempInvoiceJuryDutyID, ExistingVendorText = F, ReadOnlyDemographicInfo = F, ReadOnlyVendorFields = F, VendorID = F, VendorNumber = F, NameID = F, FirstName = F, MiddleName = F, LastName = F, AddressLine1 = F, AddressLine2 = F, City = F, StateCode = F, ZipCode = F, Amount = F, SourceData = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, fieldPaths = NULL, entityId = 1, schoolYearId = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment()) %>% purrr::keep(names(.) != "TempInvoiceJuryDutyID")

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		getSkyObject(module = "AccountsPayable", objectName = "TempInvoiceJuryDuty", objectId = TempInvoiceJuryDutyID, searchFields = searchFields, entityId = entityId, flatten = flatten, returnResponse = returnResponse)
	}

	#' Delete a TempInvoiceJuryDuty
	#'
	#' This function deletes a TempInvoiceJuryDuty
	#' @param TempInvoiceJuryDutyID The ID of the TempInvoiceJuryDuty to delete
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{listSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Accounts Payable
	#' @return The TempInvoiceJuryDutyID of the deleted TempInvoiceJuryDuty.
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	deleteTempInvoiceJuryDuty <- function(TempInvoiceJuryDutyID, ignoreWarnings = F, entityId = 1, schoolYearId = NULL, flatten = T, returnResponse = F){

		deleteSkyObject(module = "AccountsPayable", objectName = "TempInvoiceJuryDuty", objectId = TempInvoiceJuryDutyID, ignoreWarnings = ignoreWarnings, entityId = entityId, flatten = flatten, returnResponse = returnResponse)
	}

	#' Create a TempInvoiceJuryDuty
	#'
	#' This function creates a TempInvoiceJuryDuty
	#' @param fieldNames The field values to give the created TempInvoiceJuryDuty. Each defaults to NULL.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{listSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Accounts Payable
	#' @return A newly created TempInvoiceJuryDuty
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	createTempInvoiceJuryDuty <- function(ExistingVendorText = NULL, VendorNumber = NULL, FirstName = NULL, MiddleName = NULL, LastName = NULL, AddressLine1 = NULL, AddressLine2 = NULL, City = NULL, StateCode = NULL, ZipCode = NULL, Amount = NULL, SourceData = NULL, entityId = 1, schoolYearId = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment())

		body <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper()) %>% compact()

		createSkyObject(module = "AccountsPayable", objectName = "TempInvoiceJuryDuty", body = list(DataObject = body), searchFields = append("TempInvoiceJuryDutyID", body %>% names()), entityId = entityId, schoolYearId = schoolYearId, flatten = flatten, returnResponse = returnResponse)
	}

	#' Modify a TempInvoiceJuryDuty
	#'
	#' This function modifies a TempInvoiceJuryDuty
	#' @param fieldNames The field values to give the modified TempInvoiceJuryDuty. Each defaults to NULL.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{listSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Accounts Payable
	#' @return The modified TempInvoiceJuryDuty
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	modifyTempInvoiceJuryDuty <- function(TempInvoiceJuryDutyID, ExistingVendorText = NULL, VendorNumber = NULL, FirstName = NULL, MiddleName = NULL, LastName = NULL, AddressLine1 = NULL, AddressLine2 = NULL, City = NULL, StateCode = NULL, ZipCode = NULL, Amount = NULL, SourceData = NULL, entityId = 1, schoolYearId = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment())

		body <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper()) %>% compact()

		modifySkyObject(module = "AccountsPayable", objectName = "TempInvoiceJuryDuty", objectId = TempInvoiceJuryDutyID, body = list(DataObject = body), searchFields = append("TempInvoiceJuryDutyID", body %>% names()), entityId = entityId, schoolYearId = schoolYearId, flatten = flatten, returnResponse = returnResponse)
	}

	#' List AccountsPayableInvoices
	#'
	#' This function returns a dataframe or json object of AccountsPayableInvoices
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given AccountsPayableInvoices. Defaults to FALSE for all return fields which, for convenience, returns all fields for the AccountsPayableInvoices.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object listed as text. Run \code{\link{getSchemaForObjects}}('AccountsPayableInvoice') to get more field paths.
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
	#' @concept Accounts Payable
	#' @return A list of AccountsPayableInvoices
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	listAccountsPayableInvoices <- function(searchConditionsList = NULL, InvoiceID = F, Batch = F, InvoiceDate = F, Description = F, InvoiceNumber = F, AccountingUpdateIDExpense = F, AccountingUpdateIDCash = F, ContractID = F, CommodityID = F, PurchaseOrderID = F, DistrictID = F, VendorID = F, FiscalYearID = F, CheckTransactionID = F, BankAccountID = F, CurrencyIDEntry = F, AccountsPayableRunID = F, PayrollRunID = F, CurrencyConversionRate = F, InvoiceGroupID = F, PendingReceiving = F, Status = F, PaymentType = F, SourceType = F, AccountsPayableRunType = F, EntryAmount = F, BaseCurrencyAmount = F, BaseCurrencyAmountByAccount = F, CanChangeAccount = F, CanClone = F, CanSubmit = F, ShippingAmount = F, EntryTotal = F, RenderInvoiceUpdateVoid = F, AttachmentCount = F, CurrentUserHasInvoiceGroupAccess = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, DueDate = F, CanDelete = F, CanDeleteInvoiceDetail = F, CanReSubmit = F, PaymentTermsID = F, DiscountPercentApplied = F, PaymentTermsDate = F, DiscountAmount = F, fieldPaths = NULL, searchConditionsGroupType = "And", searchSortFieldNamesList = NULL, searchSortFieldNamesDescendingList = NULL, entityId = 1, schoolYearId = NULL, page = 1, pageSize = 100000, flatten = T, returnResponse = F){

		params <- as.list(environment())

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		listSkyObjects(module = "AccountsPayable", objectName = "Invoice", searchFields = searchFields %>% append(fieldPaths), page = page, pageSize = pageSize, SearchConditionsList = searchConditionsList, SearchConditionsGroupType = searchConditionsGroupType, SearchSortFieldNamesList = searchSortFieldNamesList, SearchSortFieldNamesDescendingList = searchSortFieldNamesDescendingList, entityId = entityId, schoolYearId = schoolYearId, flatten = flatten, returnResponse = returnResponse)
	}

	#' Get an AccountsPayableInvoice
	#'
	#' This function returns a dataframe or json object of an AccountsPayableInvoice
	#' @param AccountsPayableInvoiceID The ID of the AccountsPayableInvoice to return.
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given AccountsPayableInvoice. Defaults to FALSE for all return fields which, for convenience, returns all fields for the AccountsPayableInvoice.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object listed as text. Run \code{\link{getSchemaForObjects}}('AccountsPayableInvoice') to get more field paths.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{listSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Accounts Payable
	#' @return A dataframe or of AccountsPayableInvoice
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	getAccountsPayableInvoice <- function(AccountsPayableInvoiceID, InvoiceID = F, Batch = F, InvoiceDate = F, Description = F, InvoiceNumber = F, AccountingUpdateIDExpense = F, AccountingUpdateIDCash = F, ContractID = F, CommodityID = F, PurchaseOrderID = F, DistrictID = F, VendorID = F, FiscalYearID = F, CheckTransactionID = F, BankAccountID = F, CurrencyIDEntry = F, AccountsPayableRunID = F, PayrollRunID = F, CurrencyConversionRate = F, InvoiceGroupID = F, PendingReceiving = F, Status = F, PaymentType = F, SourceType = F, AccountsPayableRunType = F, EntryAmount = F, BaseCurrencyAmount = F, BaseCurrencyAmountByAccount = F, CanChangeAccount = F, CanClone = F, CanSubmit = F, ShippingAmount = F, EntryTotal = F, RenderInvoiceUpdateVoid = F, AttachmentCount = F, CurrentUserHasInvoiceGroupAccess = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, DueDate = F, CanDelete = F, CanDeleteInvoiceDetail = F, CanReSubmit = F, PaymentTermsID = F, DiscountPercentApplied = F, PaymentTermsDate = F, DiscountAmount = F, fieldPaths = NULL, entityId = 1, schoolYearId = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment()) %>% purrr::keep(names(.) != "AccountsPayableInvoiceID")

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		getSkyObject(module = "AccountsPayable", objectName = "Invoice", objectId = AccountsPayableInvoiceID, searchFields = searchFields, entityId = entityId, flatten = flatten, returnResponse = returnResponse)
	}

	#' Delete an AccountsPayableInvoice
	#'
	#' This function deletes an AccountsPayableInvoice
	#' @param AccountsPayableInvoiceID The ID of the AccountsPayableInvoice to delete
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{listSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Accounts Payable
	#' @return The AccountsPayableInvoiceID of the deleted AccountsPayableInvoice.
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	deleteAccountsPayableInvoice <- function(AccountsPayableInvoiceID, ignoreWarnings = F, entityId = 1, schoolYearId = NULL, flatten = T, returnResponse = F){

		deleteSkyObject(module = "AccountsPayable", objectName = "Invoice", objectId = AccountsPayableInvoiceID, ignoreWarnings = ignoreWarnings, entityId = entityId, flatten = flatten, returnResponse = returnResponse)
	}

	#' Create an AccountsPayableInvoice
	#'
	#' This function creates an AccountsPayableInvoice
	#' @param fieldNames The field values to give the created AccountsPayableInvoice. Each defaults to NULL.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{listSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Accounts Payable
	#' @return A newly created AccountsPayableInvoice
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	createAccountsPayableInvoice <- function(Batch = NULL, InvoiceDate = NULL, Description = NULL, InvoiceNumber = NULL, AccountingUpdateIDExpense = NULL, AccountingUpdateIDCash = NULL, ContractID = NULL, CommodityID = NULL, PurchaseOrderID = NULL, DistrictID = NULL, VendorID = NULL, FiscalYearID = NULL, CheckTransactionID = NULL, BankAccountID = NULL, CurrencyIDEntry = NULL, AccountsPayableRunID = NULL, PayrollRunID = NULL, CurrencyConversionRate = NULL, InvoiceGroupID = NULL, Status = NULL, PaymentType = NULL, SourceType = NULL, AccountsPayableRunType = NULL, DueDate = NULL, PaymentTermsID = NULL, DiscountPercentApplied = NULL, entityId = 1, schoolYearId = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment())

		body <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper()) %>% compact()

		createSkyObject(module = "AccountsPayable", objectName = "Invoice", body = list(DataObject = body), searchFields = append("InvoiceID", body %>% names()), entityId = entityId, schoolYearId = schoolYearId, flatten = flatten, returnResponse = returnResponse)
	}

	#' Modify an AccountsPayableInvoice
	#'
	#' This function modifies an AccountsPayableInvoice
	#' @param fieldNames The field values to give the modified AccountsPayableInvoice. Each defaults to NULL.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{listSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Accounts Payable
	#' @return The modified AccountsPayableInvoice
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	modifyAccountsPayableInvoice <- function(InvoiceID, Batch = NULL, InvoiceDate = NULL, Description = NULL, InvoiceNumber = NULL, AccountingUpdateIDExpense = NULL, AccountingUpdateIDCash = NULL, ContractID = NULL, CommodityID = NULL, PurchaseOrderID = NULL, DistrictID = NULL, VendorID = NULL, FiscalYearID = NULL, CheckTransactionID = NULL, BankAccountID = NULL, CurrencyIDEntry = NULL, AccountsPayableRunID = NULL, PayrollRunID = NULL, CurrencyConversionRate = NULL, InvoiceGroupID = NULL, Status = NULL, PaymentType = NULL, SourceType = NULL, AccountsPayableRunType = NULL, DueDate = NULL, PaymentTermsID = NULL, DiscountPercentApplied = NULL, entityId = 1, schoolYearId = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment())

		body <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper()) %>% compact()

		modifySkyObject(module = "AccountsPayable", objectName = "Invoice", objectId = InvoiceID, body = list(DataObject = body), searchFields = append("InvoiceID", body %>% names()), entityId = entityId, schoolYearId = schoolYearId, flatten = flatten, returnResponse = returnResponse)
	}

	#' List AccountsPayableInvoiceAccountings
	#'
	#' This function returns a dataframe or json object of AccountsPayableInvoiceAccountings
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given AccountsPayableInvoiceAccountings. Defaults to FALSE for all return fields which, for convenience, returns all fields for the AccountsPayableInvoiceAccountings.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object listed as text. Run \code{\link{getSchemaForObjects}}('AccountsPayableInvoiceAccounting') to get more field paths.
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
	#' @concept Accounts Payable
	#' @return A list of AccountsPayableInvoiceAccountings
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	listAccountsPayableInvoiceAccountings <- function(searchConditionsList = NULL, InvoiceAccountingID = F, InvoiceDetailID = F, AccountID = F, EntryAmount = F, BaseCurrencyAmount = F, Percent = F, BaseCurrencyPercent = F, BaseCurrencyAmountByAccount = F, CurrentUserHasInvoiceGroupAccess = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, CurrentUserHasAccountGroupAccess = F, GrantID = F, ProjectID = F, fieldPaths = NULL, searchConditionsGroupType = "And", searchSortFieldNamesList = NULL, searchSortFieldNamesDescendingList = NULL, entityId = 1, schoolYearId = NULL, page = 1, pageSize = 100000, flatten = T, returnResponse = F){

		params <- as.list(environment())

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		listSkyObjects(module = "AccountsPayable", objectName = "InvoiceAccounting", searchFields = searchFields %>% append(fieldPaths), page = page, pageSize = pageSize, SearchConditionsList = searchConditionsList, SearchConditionsGroupType = searchConditionsGroupType, SearchSortFieldNamesList = searchSortFieldNamesList, SearchSortFieldNamesDescendingList = searchSortFieldNamesDescendingList, entityId = entityId, schoolYearId = schoolYearId, flatten = flatten, returnResponse = returnResponse)
	}

	#' Get an AccountsPayableInvoiceAccounting
	#'
	#' This function returns a dataframe or json object of an AccountsPayableInvoiceAccounting
	#' @param AccountsPayableInvoiceAccountingID The ID of the AccountsPayableInvoiceAccounting to return.
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given AccountsPayableInvoiceAccounting. Defaults to FALSE for all return fields which, for convenience, returns all fields for the AccountsPayableInvoiceAccounting.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object listed as text. Run \code{\link{getSchemaForObjects}}('AccountsPayableInvoiceAccounting') to get more field paths.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{listSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Accounts Payable
	#' @return A dataframe or of AccountsPayableInvoiceAccounting
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	getAccountsPayableInvoiceAccounting <- function(AccountsPayableInvoiceAccountingID, InvoiceAccountingID = F, InvoiceDetailID = F, AccountID = F, EntryAmount = F, BaseCurrencyAmount = F, Percent = F, BaseCurrencyPercent = F, BaseCurrencyAmountByAccount = F, CurrentUserHasInvoiceGroupAccess = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, CurrentUserHasAccountGroupAccess = F, GrantID = F, ProjectID = F, fieldPaths = NULL, entityId = 1, schoolYearId = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment()) %>% purrr::keep(names(.) != "AccountsPayableInvoiceAccountingID")

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		getSkyObject(module = "AccountsPayable", objectName = "InvoiceAccounting", objectId = AccountsPayableInvoiceAccountingID, searchFields = searchFields, entityId = entityId, flatten = flatten, returnResponse = returnResponse)
	}

	#' Delete an AccountsPayableInvoiceAccounting
	#'
	#' This function deletes an AccountsPayableInvoiceAccounting
	#' @param AccountsPayableInvoiceAccountingID The ID of the AccountsPayableInvoiceAccounting to delete
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{listSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Accounts Payable
	#' @return The AccountsPayableInvoiceAccountingID of the deleted AccountsPayableInvoiceAccounting.
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	deleteAccountsPayableInvoiceAccounting <- function(AccountsPayableInvoiceAccountingID, ignoreWarnings = F, entityId = 1, schoolYearId = NULL, flatten = T, returnResponse = F){

		deleteSkyObject(module = "AccountsPayable", objectName = "InvoiceAccounting", objectId = AccountsPayableInvoiceAccountingID, ignoreWarnings = ignoreWarnings, entityId = entityId, flatten = flatten, returnResponse = returnResponse)
	}

	#' Create an AccountsPayableInvoiceAccounting
	#'
	#' This function creates an AccountsPayableInvoiceAccounting
	#' @param fieldNames The field values to give the created AccountsPayableInvoiceAccounting. Each defaults to NULL.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{listSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Accounts Payable
	#' @return A newly created AccountsPayableInvoiceAccounting
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	createAccountsPayableInvoiceAccounting <- function(InvoiceDetailID = NULL, AccountID = NULL, EntryAmount = NULL, BaseCurrencyAmount = NULL, GrantID = NULL, ProjectID = NULL, entityId = 1, schoolYearId = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment())

		body <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper()) %>% compact()

		createSkyObject(module = "AccountsPayable", objectName = "InvoiceAccounting", body = list(DataObject = body), searchFields = append("InvoiceAccountingID", body %>% names()), entityId = entityId, schoolYearId = schoolYearId, flatten = flatten, returnResponse = returnResponse)
	}

	#' Modify an AccountsPayableInvoiceAccounting
	#'
	#' This function modifies an AccountsPayableInvoiceAccounting
	#' @param fieldNames The field values to give the modified AccountsPayableInvoiceAccounting. Each defaults to NULL.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{listSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Accounts Payable
	#' @return The modified AccountsPayableInvoiceAccounting
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	modifyAccountsPayableInvoiceAccounting <- function(InvoiceAccountingID, InvoiceDetailID = NULL, AccountID = NULL, EntryAmount = NULL, BaseCurrencyAmount = NULL, GrantID = NULL, ProjectID = NULL, entityId = 1, schoolYearId = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment())

		body <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper()) %>% compact()

		modifySkyObject(module = "AccountsPayable", objectName = "InvoiceAccounting", objectId = InvoiceAccountingID, body = list(DataObject = body), searchFields = append("InvoiceAccountingID", body %>% names()), entityId = entityId, schoolYearId = schoolYearId, flatten = flatten, returnResponse = returnResponse)
	}

	#' List AccountsPayableInvoiceDetails
	#'
	#' This function returns a dataframe or json object of AccountsPayableInvoiceDetails
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given AccountsPayableInvoiceDetails. Defaults to FALSE for all return fields which, for convenience, returns all fields for the AccountsPayableInvoiceDetails.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object listed as text. Run \code{\link{getSchemaForObjects}}('AccountsPayableInvoiceDetail') to get more field paths.
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
	#' @concept Accounts Payable
	#' @return A list of AccountsPayableInvoiceDetails
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	listAccountsPayableInvoiceDetails <- function(searchConditionsList = NULL, InvoiceDetailID = F, InvoiceID = F, DisplayOrder = F, Quantity = F, UnitCost = F, EntryAmount = F, BaseCurrencyAmount = F, CommodityID = F, CatalogItem = F, Description = F, PurchaseOrderDetailID = F, UnitOfMeasureID = F, InvoiceDetailIDDeletedHistory = F, EntryShippingAmount = F, IsOnPurchaseOrder = F, CurrentUserHasInvoiceGroupAccess = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, IsAsset = F, InvoiceTotalLessEntryAmount = F, EntryDiscountAmount = F, DiscountAmount = F, Form1099TypeID = F, Form1099TypeBoxNumberDescription = F, Form1099TypeBoxNumber = F, fieldPaths = NULL, searchConditionsGroupType = "And", searchSortFieldNamesList = NULL, searchSortFieldNamesDescendingList = NULL, entityId = 1, schoolYearId = NULL, page = 1, pageSize = 100000, flatten = T, returnResponse = F){

		params <- as.list(environment())

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		listSkyObjects(module = "AccountsPayable", objectName = "InvoiceDetail", searchFields = searchFields %>% append(fieldPaths), page = page, pageSize = pageSize, SearchConditionsList = searchConditionsList, SearchConditionsGroupType = searchConditionsGroupType, SearchSortFieldNamesList = searchSortFieldNamesList, SearchSortFieldNamesDescendingList = searchSortFieldNamesDescendingList, entityId = entityId, schoolYearId = schoolYearId, flatten = flatten, returnResponse = returnResponse)
	}

	#' Get an AccountsPayableInvoiceDetail
	#'
	#' This function returns a dataframe or json object of an AccountsPayableInvoiceDetail
	#' @param AccountsPayableInvoiceDetailID The ID of the AccountsPayableInvoiceDetail to return.
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given AccountsPayableInvoiceDetail. Defaults to FALSE for all return fields which, for convenience, returns all fields for the AccountsPayableInvoiceDetail.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object listed as text. Run \code{\link{getSchemaForObjects}}('AccountsPayableInvoiceDetail') to get more field paths.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{listSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Accounts Payable
	#' @return A dataframe or of AccountsPayableInvoiceDetail
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	getAccountsPayableInvoiceDetail <- function(AccountsPayableInvoiceDetailID, InvoiceDetailID = F, InvoiceID = F, DisplayOrder = F, Quantity = F, UnitCost = F, EntryAmount = F, BaseCurrencyAmount = F, CommodityID = F, CatalogItem = F, Description = F, PurchaseOrderDetailID = F, UnitOfMeasureID = F, InvoiceDetailIDDeletedHistory = F, EntryShippingAmount = F, IsOnPurchaseOrder = F, CurrentUserHasInvoiceGroupAccess = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, IsAsset = F, InvoiceTotalLessEntryAmount = F, EntryDiscountAmount = F, DiscountAmount = F, Form1099TypeID = F, Form1099TypeBoxNumberDescription = F, Form1099TypeBoxNumber = F, fieldPaths = NULL, entityId = 1, schoolYearId = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment()) %>% purrr::keep(names(.) != "AccountsPayableInvoiceDetailID")

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		getSkyObject(module = "AccountsPayable", objectName = "InvoiceDetail", objectId = AccountsPayableInvoiceDetailID, searchFields = searchFields, entityId = entityId, flatten = flatten, returnResponse = returnResponse)
	}

	#' Delete an AccountsPayableInvoiceDetail
	#'
	#' This function deletes an AccountsPayableInvoiceDetail
	#' @param AccountsPayableInvoiceDetailID The ID of the AccountsPayableInvoiceDetail to delete
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{listSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Accounts Payable
	#' @return The AccountsPayableInvoiceDetailID of the deleted AccountsPayableInvoiceDetail.
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	deleteAccountsPayableInvoiceDetail <- function(AccountsPayableInvoiceDetailID, ignoreWarnings = F, entityId = 1, schoolYearId = NULL, flatten = T, returnResponse = F){

		deleteSkyObject(module = "AccountsPayable", objectName = "InvoiceDetail", objectId = AccountsPayableInvoiceDetailID, ignoreWarnings = ignoreWarnings, entityId = entityId, flatten = flatten, returnResponse = returnResponse)
	}

	#' Create an AccountsPayableInvoiceDetail
	#'
	#' This function creates an AccountsPayableInvoiceDetail
	#' @param fieldNames The field values to give the created AccountsPayableInvoiceDetail. Each defaults to NULL.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{listSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Accounts Payable
	#' @return A newly created AccountsPayableInvoiceDetail
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	createAccountsPayableInvoiceDetail <- function(InvoiceID = NULL, DisplayOrder = NULL, Quantity = NULL, UnitCost = NULL, EntryAmount = NULL, BaseCurrencyAmount = NULL, CommodityID = NULL, CatalogItem = NULL, Description = NULL, PurchaseOrderDetailID = NULL, UnitOfMeasureID = NULL, InvoiceDetailIDDeletedHistory = NULL, EntryShippingAmount = NULL, IsAsset = NULL, Form1099TypeID = NULL, entityId = 1, schoolYearId = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment())

		body <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper()) %>% compact()

		createSkyObject(module = "AccountsPayable", objectName = "InvoiceDetail", body = list(DataObject = body), searchFields = append("InvoiceDetailID", body %>% names()), entityId = entityId, schoolYearId = schoolYearId, flatten = flatten, returnResponse = returnResponse)
	}

	#' Modify an AccountsPayableInvoiceDetail
	#'
	#' This function modifies an AccountsPayableInvoiceDetail
	#' @param fieldNames The field values to give the modified AccountsPayableInvoiceDetail. Each defaults to NULL.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{listSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Accounts Payable
	#' @return The modified AccountsPayableInvoiceDetail
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	modifyAccountsPayableInvoiceDetail <- function(InvoiceDetailID, InvoiceID = NULL, DisplayOrder = NULL, Quantity = NULL, UnitCost = NULL, EntryAmount = NULL, BaseCurrencyAmount = NULL, CommodityID = NULL, CatalogItem = NULL, Description = NULL, PurchaseOrderDetailID = NULL, UnitOfMeasureID = NULL, InvoiceDetailIDDeletedHistory = NULL, EntryShippingAmount = NULL, IsAsset = NULL, Form1099TypeID = NULL, entityId = 1, schoolYearId = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment())

		body <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper()) %>% compact()

		modifySkyObject(module = "AccountsPayable", objectName = "InvoiceDetail", objectId = InvoiceDetailID, body = list(DataObject = body), searchFields = append("InvoiceDetailID", body %>% names()), entityId = entityId, schoolYearId = schoolYearId, flatten = flatten, returnResponse = returnResponse)
	}

	#' List TempCheckTransactions
	#'
	#' This function returns a dataframe or json object of TempCheckTransactions
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given TempCheckTransactions. Defaults to FALSE for all return fields which, for convenience, returns all fields for the TempCheckTransactions.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object listed as text. Run \code{\link{getSchemaForObjects}}('TempCheckTransaction') to get more field paths.
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
	#' @concept Accounts Payable
	#' @return A list of TempCheckTransactions
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	listTempCheckTransactions <- function(searchConditionsList = NULL, TempCheckTransactionID = F, VendorName = F, PaymentType = F, NetCheckAmount = F, AccountsPayableContact = F, RemitToAddress = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, DisplayAccountsPayableContact = F, PrintAPContact = F, fieldPaths = NULL, searchConditionsGroupType = "And", searchSortFieldNamesList = NULL, searchSortFieldNamesDescendingList = NULL, entityId = 1, schoolYearId = NULL, page = 1, pageSize = 100000, flatten = T, returnResponse = F){

		params <- as.list(environment())

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		listSkyObjects(module = "AccountsPayable", objectName = "TempCheckTransaction", searchFields = searchFields %>% append(fieldPaths), page = page, pageSize = pageSize, SearchConditionsList = searchConditionsList, SearchConditionsGroupType = searchConditionsGroupType, SearchSortFieldNamesList = searchSortFieldNamesList, SearchSortFieldNamesDescendingList = searchSortFieldNamesDescendingList, entityId = entityId, schoolYearId = schoolYearId, flatten = flatten, returnResponse = returnResponse)
	}

	#' Get a TempCheckTransaction
	#'
	#' This function returns a dataframe or json object of a TempCheckTransaction
	#' @param TempCheckTransactionID The ID of the TempCheckTransaction to return.
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given TempCheckTransaction. Defaults to FALSE for all return fields which, for convenience, returns all fields for the TempCheckTransaction.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object listed as text. Run \code{\link{getSchemaForObjects}}('TempCheckTransaction') to get more field paths.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{listSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Accounts Payable
	#' @return A dataframe or of TempCheckTransaction
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	getTempCheckTransaction <- function(TempCheckTransactionID, VendorName = F, PaymentType = F, NetCheckAmount = F, AccountsPayableContact = F, RemitToAddress = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, DisplayAccountsPayableContact = F, PrintAPContact = F, fieldPaths = NULL, entityId = 1, schoolYearId = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment()) %>% purrr::keep(names(.) != "TempCheckTransactionID")

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		getSkyObject(module = "AccountsPayable", objectName = "TempCheckTransaction", objectId = TempCheckTransactionID, searchFields = searchFields, entityId = entityId, flatten = flatten, returnResponse = returnResponse)
	}

	#' Delete a TempCheckTransaction
	#'
	#' This function deletes a TempCheckTransaction
	#' @param TempCheckTransactionID The ID of the TempCheckTransaction to delete
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{listSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Accounts Payable
	#' @return The TempCheckTransactionID of the deleted TempCheckTransaction.
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	deleteTempCheckTransaction <- function(TempCheckTransactionID, ignoreWarnings = F, entityId = 1, schoolYearId = NULL, flatten = T, returnResponse = F){

		deleteSkyObject(module = "AccountsPayable", objectName = "TempCheckTransaction", objectId = TempCheckTransactionID, ignoreWarnings = ignoreWarnings, entityId = entityId, flatten = flatten, returnResponse = returnResponse)
	}

	#' Create a TempCheckTransaction
	#'
	#' This function creates a TempCheckTransaction
	#' @param fieldNames The field values to give the created TempCheckTransaction. Each defaults to NULL.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{listSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Accounts Payable
	#' @return A newly created TempCheckTransaction
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	createTempCheckTransaction <- function(VendorName = NULL, PaymentType = NULL, NetCheckAmount = NULL, AccountsPayableContact = NULL, RemitToAddress = NULL, DisplayAccountsPayableContact = NULL, PrintAPContact = NULL, entityId = 1, schoolYearId = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment())

		body <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper()) %>% compact()

		createSkyObject(module = "AccountsPayable", objectName = "TempCheckTransaction", body = list(DataObject = body), searchFields = append("TempCheckTransactionID", body %>% names()), entityId = entityId, schoolYearId = schoolYearId, flatten = flatten, returnResponse = returnResponse)
	}

	#' Modify a TempCheckTransaction
	#'
	#' This function modifies a TempCheckTransaction
	#' @param fieldNames The field values to give the modified TempCheckTransaction. Each defaults to NULL.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{listSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Accounts Payable
	#' @return The modified TempCheckTransaction
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	modifyTempCheckTransaction <- function(TempCheckTransactionID, VendorName = NULL, PaymentType = NULL, NetCheckAmount = NULL, AccountsPayableContact = NULL, RemitToAddress = NULL, DisplayAccountsPayableContact = NULL, PrintAPContact = NULL, entityId = 1, schoolYearId = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment())

		body <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper()) %>% compact()

		modifySkyObject(module = "AccountsPayable", objectName = "TempCheckTransaction", objectId = TempCheckTransactionID, body = list(DataObject = body), searchFields = append("TempCheckTransactionID", body %>% names()), entityId = entityId, schoolYearId = schoolYearId, flatten = flatten, returnResponse = returnResponse)
	}

	#' List TempInvoiceAccountingErrors
	#'
	#' This function returns a dataframe or json object of TempInvoiceAccountingErrors
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given TempInvoiceAccountingErrors. Defaults to FALSE for all return fields which, for convenience, returns all fields for the TempInvoiceAccountingErrors.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object listed as text. Run \code{\link{getSchemaForObjects}}('TempInvoiceAccountingError') to get more field paths.
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
	#' @concept Accounts Payable
	#' @return A list of TempInvoiceAccountingErrors
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	listTempInvoiceAccountingErrors <- function(searchConditionsList = NULL, TempInvoiceAccountingErrorID = F, Type = F, Code = F, VendorFullNameLFM = F, DisplayAccount = F, Error = F, InvoiceDescription = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, fieldPaths = NULL, searchConditionsGroupType = "And", searchSortFieldNamesList = NULL, searchSortFieldNamesDescendingList = NULL, entityId = 1, schoolYearId = NULL, page = 1, pageSize = 100000, flatten = T, returnResponse = F){

		params <- as.list(environment())

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		listSkyObjects(module = "AccountsPayable", objectName = "TempInvoiceAccountingError", searchFields = searchFields %>% append(fieldPaths), page = page, pageSize = pageSize, SearchConditionsList = searchConditionsList, SearchConditionsGroupType = searchConditionsGroupType, SearchSortFieldNamesList = searchSortFieldNamesList, SearchSortFieldNamesDescendingList = searchSortFieldNamesDescendingList, entityId = entityId, schoolYearId = schoolYearId, flatten = flatten, returnResponse = returnResponse)
	}

	#' Get a TempInvoiceAccountingError
	#'
	#' This function returns a dataframe or json object of a TempInvoiceAccountingError
	#' @param TempInvoiceAccountingErrorID The ID of the TempInvoiceAccountingError to return.
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given TempInvoiceAccountingError. Defaults to FALSE for all return fields which, for convenience, returns all fields for the TempInvoiceAccountingError.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object listed as text. Run \code{\link{getSchemaForObjects}}('TempInvoiceAccountingError') to get more field paths.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{listSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Accounts Payable
	#' @return A dataframe or of TempInvoiceAccountingError
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	getTempInvoiceAccountingError <- function(TempInvoiceAccountingErrorID, Type = F, Code = F, VendorFullNameLFM = F, DisplayAccount = F, Error = F, InvoiceDescription = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, fieldPaths = NULL, entityId = 1, schoolYearId = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment()) %>% purrr::keep(names(.) != "TempInvoiceAccountingErrorID")

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		getSkyObject(module = "AccountsPayable", objectName = "TempInvoiceAccountingError", objectId = TempInvoiceAccountingErrorID, searchFields = searchFields, entityId = entityId, flatten = flatten, returnResponse = returnResponse)
	}

	#' Delete a TempInvoiceAccountingError
	#'
	#' This function deletes a TempInvoiceAccountingError
	#' @param TempInvoiceAccountingErrorID The ID of the TempInvoiceAccountingError to delete
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{listSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Accounts Payable
	#' @return The TempInvoiceAccountingErrorID of the deleted TempInvoiceAccountingError.
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	deleteTempInvoiceAccountingError <- function(TempInvoiceAccountingErrorID, ignoreWarnings = F, entityId = 1, schoolYearId = NULL, flatten = T, returnResponse = F){

		deleteSkyObject(module = "AccountsPayable", objectName = "TempInvoiceAccountingError", objectId = TempInvoiceAccountingErrorID, ignoreWarnings = ignoreWarnings, entityId = entityId, flatten = flatten, returnResponse = returnResponse)
	}

	#' Create a TempInvoiceAccountingError
	#'
	#' This function creates a TempInvoiceAccountingError
	#' @param fieldNames The field values to give the created TempInvoiceAccountingError. Each defaults to NULL.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{listSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Accounts Payable
	#' @return A newly created TempInvoiceAccountingError
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	createTempInvoiceAccountingError <- function(Type = NULL, Code = NULL, VendorFullNameLFM = NULL, DisplayAccount = NULL, Error = NULL, InvoiceDescription = NULL, entityId = 1, schoolYearId = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment())

		body <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper()) %>% compact()

		createSkyObject(module = "AccountsPayable", objectName = "TempInvoiceAccountingError", body = list(DataObject = body), searchFields = append("TempInvoiceAccountingErrorID", body %>% names()), entityId = entityId, schoolYearId = schoolYearId, flatten = flatten, returnResponse = returnResponse)
	}

	#' Modify a TempInvoiceAccountingError
	#'
	#' This function modifies a TempInvoiceAccountingError
	#' @param fieldNames The field values to give the modified TempInvoiceAccountingError. Each defaults to NULL.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{listSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Accounts Payable
	#' @return The modified TempInvoiceAccountingError
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	modifyTempInvoiceAccountingError <- function(TempInvoiceAccountingErrorID, Type = NULL, Code = NULL, VendorFullNameLFM = NULL, DisplayAccount = NULL, Error = NULL, InvoiceDescription = NULL, entityId = 1, schoolYearId = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment())

		body <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper()) %>% compact()

		modifySkyObject(module = "AccountsPayable", objectName = "TempInvoiceAccountingError", objectId = TempInvoiceAccountingErrorID, body = list(DataObject = body), searchFields = append("TempInvoiceAccountingErrorID", body %>% names()), entityId = entityId, schoolYearId = schoolYearId, flatten = flatten, returnResponse = returnResponse)
	}

	#' List AccountsPayableTempInvoices
	#'
	#' This function returns a dataframe or json object of AccountsPayableTempInvoices
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given AccountsPayableTempInvoices. Defaults to FALSE for all return fields which, for convenience, returns all fields for the AccountsPayableTempInvoices.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object listed as text. Run \code{\link{getSchemaForObjects}}('AccountsPayableTempInvoice') to get more field paths.
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
	#' @concept Accounts Payable
	#' @return A list of AccountsPayableTempInvoices
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	listAccountsPayableTempInvoices <- function(searchConditionsList = NULL, TempInvoiceID = F, VendorID = F, VendorName = F, Description = F, InvoiceNumber = F, Batch = F, TotalAmount = F, DeductionID = F, BenefitID = F, PayrollRunID = F, PaymentTypeCode = F, SourceType = F, SourceCode = F, SourceDisplayAccount = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, HasError = F, InvoiceGroupID = F, BankAccountID = F, FundDimensionID = F, InvoiceGroupDescription = F, InvoiceDataObjectVariableIdentifier = F, Fund = F, FiscalYear = F, BillID = F, InvoiceDate = F, DueDate = F, AccountID = F, BaseCurrencyAmount = F, fieldPaths = NULL, searchConditionsGroupType = "And", searchSortFieldNamesList = NULL, searchSortFieldNamesDescendingList = NULL, entityId = 1, schoolYearId = NULL, page = 1, pageSize = 100000, flatten = T, returnResponse = F){

		params <- as.list(environment())

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		listSkyObjects(module = "AccountsPayable", objectName = "TempInvoice", searchFields = searchFields %>% append(fieldPaths), page = page, pageSize = pageSize, SearchConditionsList = searchConditionsList, SearchConditionsGroupType = searchConditionsGroupType, SearchSortFieldNamesList = searchSortFieldNamesList, SearchSortFieldNamesDescendingList = searchSortFieldNamesDescendingList, entityId = entityId, schoolYearId = schoolYearId, flatten = flatten, returnResponse = returnResponse)
	}

	#' Get an AccountsPayableTempInvoice
	#'
	#' This function returns a dataframe or json object of an AccountsPayableTempInvoice
	#' @param AccountsPayableTempInvoiceID The ID of the AccountsPayableTempInvoice to return.
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given AccountsPayableTempInvoice. Defaults to FALSE for all return fields which, for convenience, returns all fields for the AccountsPayableTempInvoice.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object listed as text. Run \code{\link{getSchemaForObjects}}('AccountsPayableTempInvoice') to get more field paths.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{listSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Accounts Payable
	#' @return A dataframe or of AccountsPayableTempInvoice
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	getAccountsPayableTempInvoice <- function(AccountsPayableTempInvoiceID, TempInvoiceID = F, VendorID = F, VendorName = F, Description = F, InvoiceNumber = F, Batch = F, TotalAmount = F, DeductionID = F, BenefitID = F, PayrollRunID = F, PaymentTypeCode = F, SourceType = F, SourceCode = F, SourceDisplayAccount = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, HasError = F, InvoiceGroupID = F, BankAccountID = F, FundDimensionID = F, InvoiceGroupDescription = F, InvoiceDataObjectVariableIdentifier = F, Fund = F, FiscalYear = F, BillID = F, InvoiceDate = F, DueDate = F, AccountID = F, BaseCurrencyAmount = F, fieldPaths = NULL, entityId = 1, schoolYearId = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment()) %>% purrr::keep(names(.) != "AccountsPayableTempInvoiceID")

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		getSkyObject(module = "AccountsPayable", objectName = "TempInvoice", objectId = AccountsPayableTempInvoiceID, searchFields = searchFields, entityId = entityId, flatten = flatten, returnResponse = returnResponse)
	}

	#' Delete an AccountsPayableTempInvoice
	#'
	#' This function deletes an AccountsPayableTempInvoice
	#' @param AccountsPayableTempInvoiceID The ID of the AccountsPayableTempInvoice to delete
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{listSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Accounts Payable
	#' @return The AccountsPayableTempInvoiceID of the deleted AccountsPayableTempInvoice.
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	deleteAccountsPayableTempInvoice <- function(AccountsPayableTempInvoiceID, ignoreWarnings = F, entityId = 1, schoolYearId = NULL, flatten = T, returnResponse = F){

		deleteSkyObject(module = "AccountsPayable", objectName = "TempInvoice", objectId = AccountsPayableTempInvoiceID, ignoreWarnings = ignoreWarnings, entityId = entityId, flatten = flatten, returnResponse = returnResponse)
	}

	#' Create an AccountsPayableTempInvoice
	#'
	#' This function creates an AccountsPayableTempInvoice
	#' @param fieldNames The field values to give the created AccountsPayableTempInvoice. Each defaults to NULL.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{listSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Accounts Payable
	#' @return A newly created AccountsPayableTempInvoice
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	createAccountsPayableTempInvoice <- function(VendorID = NULL, VendorName = NULL, Description = NULL, InvoiceNumber = NULL, Batch = NULL, TotalAmount = NULL, DeductionID = NULL, BenefitID = NULL, PayrollRunID = NULL, PaymentTypeCode = NULL, SourceType = NULL, SourceCode = NULL, SourceDisplayAccount = NULL, HasError = NULL, InvoiceGroupID = NULL, BankAccountID = NULL, FundDimensionID = NULL, InvoiceGroupDescription = NULL, InvoiceDataObjectVariableIdentifier = NULL, Fund = NULL, FiscalYear = NULL, BillID = NULL, InvoiceDate = NULL, DueDate = NULL, AccountID = NULL, BaseCurrencyAmount = NULL, entityId = 1, schoolYearId = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment())

		body <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper()) %>% compact()

		createSkyObject(module = "AccountsPayable", objectName = "TempInvoice", body = list(DataObject = body), searchFields = append("TempInvoiceID", body %>% names()), entityId = entityId, schoolYearId = schoolYearId, flatten = flatten, returnResponse = returnResponse)
	}

	#' Modify an AccountsPayableTempInvoice
	#'
	#' This function modifies an AccountsPayableTempInvoice
	#' @param fieldNames The field values to give the modified AccountsPayableTempInvoice. Each defaults to NULL.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{listSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Accounts Payable
	#' @return The modified AccountsPayableTempInvoice
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	modifyAccountsPayableTempInvoice <- function(TempInvoiceID, VendorID = NULL, VendorName = NULL, Description = NULL, InvoiceNumber = NULL, Batch = NULL, TotalAmount = NULL, DeductionID = NULL, BenefitID = NULL, PayrollRunID = NULL, PaymentTypeCode = NULL, SourceType = NULL, SourceCode = NULL, SourceDisplayAccount = NULL, HasError = NULL, InvoiceGroupID = NULL, BankAccountID = NULL, FundDimensionID = NULL, InvoiceGroupDescription = NULL, InvoiceDataObjectVariableIdentifier = NULL, Fund = NULL, FiscalYear = NULL, BillID = NULL, InvoiceDate = NULL, DueDate = NULL, AccountID = NULL, BaseCurrencyAmount = NULL, entityId = 1, schoolYearId = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment())

		body <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper()) %>% compact()

		modifySkyObject(module = "AccountsPayable", objectName = "TempInvoice", objectId = TempInvoiceID, body = list(DataObject = body), searchFields = append("TempInvoiceID", body %>% names()), entityId = entityId, schoolYearId = schoolYearId, flatten = flatten, returnResponse = returnResponse)
	}

	#' List CreditCards
	#'
	#' This function returns a dataframe or json object of CreditCards
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given CreditCards. Defaults to FALSE for all return fields which, for convenience, returns all fields for the CreditCards.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object listed as text. Run \code{\link{getSchemaForObjects}}('CreditCard') to get more field paths.
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
	#' @concept Accounts Payable
	#' @return A list of CreditCards
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	listCreditCards <- function(searchConditionsList = NULL, CreditCardID = F, DistrictID = F, Description = F, Number = F, NumberForDisplay = F, SelectedNumberForDisplay = F, Alias = F, ExpirationDate = F, CreditCardGroupID = F, VendorID = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, MaximumAmount = F, IsActive = F, CurrentUserHasCreditCardGroupAccess = F, fieldPaths = NULL, searchConditionsGroupType = "And", searchSortFieldNamesList = NULL, searchSortFieldNamesDescendingList = NULL, entityId = 1, schoolYearId = NULL, page = 1, pageSize = 100000, flatten = T, returnResponse = F){

		params <- as.list(environment())

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		listSkyObjects(module = "AccountsPayable", objectName = "CreditCard", searchFields = searchFields %>% append(fieldPaths), page = page, pageSize = pageSize, SearchConditionsList = searchConditionsList, SearchConditionsGroupType = searchConditionsGroupType, SearchSortFieldNamesList = searchSortFieldNamesList, SearchSortFieldNamesDescendingList = searchSortFieldNamesDescendingList, entityId = entityId, schoolYearId = schoolYearId, flatten = flatten, returnResponse = returnResponse)
	}

	#' Get a CreditCard
	#'
	#' This function returns a dataframe or json object of a CreditCard
	#' @param CreditCardID The ID of the CreditCard to return.
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given CreditCard. Defaults to FALSE for all return fields which, for convenience, returns all fields for the CreditCard.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object listed as text. Run \code{\link{getSchemaForObjects}}('CreditCard') to get more field paths.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{listSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Accounts Payable
	#' @return A dataframe or of CreditCard
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	getCreditCard <- function(CreditCardID, DistrictID = F, Description = F, Number = F, NumberForDisplay = F, SelectedNumberForDisplay = F, Alias = F, ExpirationDate = F, CreditCardGroupID = F, VendorID = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, MaximumAmount = F, IsActive = F, CurrentUserHasCreditCardGroupAccess = F, fieldPaths = NULL, entityId = 1, schoolYearId = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment()) %>% purrr::keep(names(.) != "CreditCardID")

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		getSkyObject(module = "AccountsPayable", objectName = "CreditCard", objectId = CreditCardID, searchFields = searchFields, entityId = entityId, flatten = flatten, returnResponse = returnResponse)
	}

	#' Delete a CreditCard
	#'
	#' This function deletes a CreditCard
	#' @param CreditCardID The ID of the CreditCard to delete
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{listSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Accounts Payable
	#' @return The CreditCardID of the deleted CreditCard.
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	deleteCreditCard <- function(CreditCardID, ignoreWarnings = F, entityId = 1, schoolYearId = NULL, flatten = T, returnResponse = F){

		deleteSkyObject(module = "AccountsPayable", objectName = "CreditCard", objectId = CreditCardID, ignoreWarnings = ignoreWarnings, entityId = entityId, flatten = flatten, returnResponse = returnResponse)
	}

	#' Create a CreditCard
	#'
	#' This function creates a CreditCard
	#' @param fieldNames The field values to give the created CreditCard. Each defaults to NULL.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{listSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Accounts Payable
	#' @return A newly created CreditCard
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	createCreditCard <- function(DistrictID = NULL, Description = NULL, Number = NULL, Alias = NULL, ExpirationDate = NULL, CreditCardGroupID = NULL, VendorID = NULL, MaximumAmount = NULL, IsActive = NULL, entityId = 1, schoolYearId = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment())

		body <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper()) %>% compact()

		createSkyObject(module = "AccountsPayable", objectName = "CreditCard", body = list(DataObject = body), searchFields = append("CreditCardID", body %>% names()), entityId = entityId, schoolYearId = schoolYearId, flatten = flatten, returnResponse = returnResponse)
	}

	#' Modify a CreditCard
	#'
	#' This function modifies a CreditCard
	#' @param fieldNames The field values to give the modified CreditCard. Each defaults to NULL.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{listSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Accounts Payable
	#' @return The modified CreditCard
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	modifyCreditCard <- function(CreditCardID, DistrictID = NULL, Description = NULL, Number = NULL, Alias = NULL, ExpirationDate = NULL, CreditCardGroupID = NULL, VendorID = NULL, MaximumAmount = NULL, IsActive = NULL, entityId = 1, schoolYearId = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment())

		body <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper()) %>% compact()

		modifySkyObject(module = "AccountsPayable", objectName = "CreditCard", objectId = CreditCardID, body = list(DataObject = body), searchFields = append("CreditCardID", body %>% names()), entityId = entityId, schoolYearId = schoolYearId, flatten = flatten, returnResponse = returnResponse)
	}

	#' List CreditCardGroups
	#'
	#' This function returns a dataframe or json object of CreditCardGroups
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given CreditCardGroups. Defaults to FALSE for all return fields which, for convenience, returns all fields for the CreditCardGroups.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object listed as text. Run \code{\link{getSchemaForObjects}}('CreditCardGroup') to get more field paths.
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
	#' @concept Accounts Payable
	#' @return A list of CreditCardGroups
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	listCreditCardGroups <- function(searchConditionsList = NULL, CreditCardGroupID = F, DistrictID = F, GroupIDAccount = F, Code = F, Description = F, CodeDescription = F, IsActive = F, IsPreApprovalWorkflowUpdated = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, IsPostApprovalWorkflowUpdated = F, CurrentUserHasCreditCardGroupAccess = F, ActivityAccountingGroupID = F, IsActivityAccounting = F, fieldPaths = NULL, searchConditionsGroupType = "And", searchSortFieldNamesList = NULL, searchSortFieldNamesDescendingList = NULL, entityId = 1, schoolYearId = NULL, page = 1, pageSize = 100000, flatten = T, returnResponse = F){

		params <- as.list(environment())

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		listSkyObjects(module = "AccountsPayable", objectName = "CreditCardGroup", searchFields = searchFields %>% append(fieldPaths), page = page, pageSize = pageSize, SearchConditionsList = searchConditionsList, SearchConditionsGroupType = searchConditionsGroupType, SearchSortFieldNamesList = searchSortFieldNamesList, SearchSortFieldNamesDescendingList = searchSortFieldNamesDescendingList, entityId = entityId, schoolYearId = schoolYearId, flatten = flatten, returnResponse = returnResponse)
	}

	#' Get a CreditCardGroup
	#'
	#' This function returns a dataframe or json object of a CreditCardGroup
	#' @param CreditCardGroupID The ID of the CreditCardGroup to return.
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given CreditCardGroup. Defaults to FALSE for all return fields which, for convenience, returns all fields for the CreditCardGroup.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object listed as text. Run \code{\link{getSchemaForObjects}}('CreditCardGroup') to get more field paths.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{listSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Accounts Payable
	#' @return A dataframe or of CreditCardGroup
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	getCreditCardGroup <- function(CreditCardGroupID, DistrictID = F, GroupIDAccount = F, Code = F, Description = F, CodeDescription = F, IsActive = F, IsPreApprovalWorkflowUpdated = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, IsPostApprovalWorkflowUpdated = F, CurrentUserHasCreditCardGroupAccess = F, ActivityAccountingGroupID = F, IsActivityAccounting = F, fieldPaths = NULL, entityId = 1, schoolYearId = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment()) %>% purrr::keep(names(.) != "CreditCardGroupID")

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		getSkyObject(module = "AccountsPayable", objectName = "CreditCardGroup", objectId = CreditCardGroupID, searchFields = searchFields, entityId = entityId, flatten = flatten, returnResponse = returnResponse)
	}

	#' Delete a CreditCardGroup
	#'
	#' This function deletes a CreditCardGroup
	#' @param CreditCardGroupID The ID of the CreditCardGroup to delete
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{listSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Accounts Payable
	#' @return The CreditCardGroupID of the deleted CreditCardGroup.
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	deleteCreditCardGroup <- function(CreditCardGroupID, ignoreWarnings = F, entityId = 1, schoolYearId = NULL, flatten = T, returnResponse = F){

		deleteSkyObject(module = "AccountsPayable", objectName = "CreditCardGroup", objectId = CreditCardGroupID, ignoreWarnings = ignoreWarnings, entityId = entityId, flatten = flatten, returnResponse = returnResponse)
	}

	#' Create a CreditCardGroup
	#'
	#' This function creates a CreditCardGroup
	#' @param fieldNames The field values to give the created CreditCardGroup. Each defaults to NULL.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{listSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Accounts Payable
	#' @return A newly created CreditCardGroup
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	createCreditCardGroup <- function(DistrictID = NULL, GroupIDAccount = NULL, Code = NULL, Description = NULL, IsActive = NULL, ActivityAccountingGroupID = NULL, entityId = 1, schoolYearId = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment())

		body <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper()) %>% compact()

		createSkyObject(module = "AccountsPayable", objectName = "CreditCardGroup", body = list(DataObject = body), searchFields = append("CreditCardGroupID", body %>% names()), entityId = entityId, schoolYearId = schoolYearId, flatten = flatten, returnResponse = returnResponse)
	}

	#' Modify a CreditCardGroup
	#'
	#' This function modifies a CreditCardGroup
	#' @param fieldNames The field values to give the modified CreditCardGroup. Each defaults to NULL.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{listSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Accounts Payable
	#' @return The modified CreditCardGroup
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	modifyCreditCardGroup <- function(CreditCardGroupID, DistrictID = NULL, GroupIDAccount = NULL, Code = NULL, Description = NULL, IsActive = NULL, ActivityAccountingGroupID = NULL, entityId = 1, schoolYearId = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment())

		body <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper()) %>% compact()

		modifySkyObject(module = "AccountsPayable", objectName = "CreditCardGroup", objectId = CreditCardGroupID, body = list(DataObject = body), searchFields = append("CreditCardGroupID", body %>% names()), entityId = entityId, schoolYearId = schoolYearId, flatten = flatten, returnResponse = returnResponse)
	}

	#' List CreditCardGroupClearances
	#'
	#' This function returns a dataframe or json object of CreditCardGroupClearances
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given CreditCardGroupClearances. Defaults to FALSE for all return fields which, for convenience, returns all fields for the CreditCardGroupClearances.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object listed as text. Run \code{\link{getSchemaForObjects}}('CreditCardGroupClearance') to get more field paths.
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
	#' @concept Accounts Payable
	#' @return A list of CreditCardGroupClearances
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	listCreditCardGroupClearances <- function(searchConditionsList = NULL, CreditCardGroupClearanceID = F, CreditCardGroupID = F, GroupIDSecurity = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, fieldPaths = NULL, searchConditionsGroupType = "And", searchSortFieldNamesList = NULL, searchSortFieldNamesDescendingList = NULL, entityId = 1, schoolYearId = NULL, page = 1, pageSize = 100000, flatten = T, returnResponse = F){

		params <- as.list(environment())

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		listSkyObjects(module = "AccountsPayable", objectName = "CreditCardGroupClearance", searchFields = searchFields %>% append(fieldPaths), page = page, pageSize = pageSize, SearchConditionsList = searchConditionsList, SearchConditionsGroupType = searchConditionsGroupType, SearchSortFieldNamesList = searchSortFieldNamesList, SearchSortFieldNamesDescendingList = searchSortFieldNamesDescendingList, entityId = entityId, schoolYearId = schoolYearId, flatten = flatten, returnResponse = returnResponse)
	}

	#' Get a CreditCardGroupClearance
	#'
	#' This function returns a dataframe or json object of a CreditCardGroupClearance
	#' @param CreditCardGroupClearanceID The ID of the CreditCardGroupClearance to return.
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given CreditCardGroupClearance. Defaults to FALSE for all return fields which, for convenience, returns all fields for the CreditCardGroupClearance.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object listed as text. Run \code{\link{getSchemaForObjects}}('CreditCardGroupClearance') to get more field paths.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{listSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Accounts Payable
	#' @return A dataframe or of CreditCardGroupClearance
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	getCreditCardGroupClearance <- function(CreditCardGroupClearanceID, CreditCardGroupID = F, GroupIDSecurity = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, fieldPaths = NULL, entityId = 1, schoolYearId = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment()) %>% purrr::keep(names(.) != "CreditCardGroupClearanceID")

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		getSkyObject(module = "AccountsPayable", objectName = "CreditCardGroupClearance", objectId = CreditCardGroupClearanceID, searchFields = searchFields, entityId = entityId, flatten = flatten, returnResponse = returnResponse)
	}

	#' Delete a CreditCardGroupClearance
	#'
	#' This function deletes a CreditCardGroupClearance
	#' @param CreditCardGroupClearanceID The ID of the CreditCardGroupClearance to delete
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{listSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Accounts Payable
	#' @return The CreditCardGroupClearanceID of the deleted CreditCardGroupClearance.
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	deleteCreditCardGroupClearance <- function(CreditCardGroupClearanceID, ignoreWarnings = F, entityId = 1, schoolYearId = NULL, flatten = T, returnResponse = F){

		deleteSkyObject(module = "AccountsPayable", objectName = "CreditCardGroupClearance", objectId = CreditCardGroupClearanceID, ignoreWarnings = ignoreWarnings, entityId = entityId, flatten = flatten, returnResponse = returnResponse)
	}

	#' Create a CreditCardGroupClearance
	#'
	#' This function creates a CreditCardGroupClearance
	#' @param fieldNames The field values to give the created CreditCardGroupClearance. Each defaults to NULL.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{listSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Accounts Payable
	#' @return A newly created CreditCardGroupClearance
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	createCreditCardGroupClearance <- function(CreditCardGroupID = NULL, GroupIDSecurity = NULL, entityId = 1, schoolYearId = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment())

		body <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper()) %>% compact()

		createSkyObject(module = "AccountsPayable", objectName = "CreditCardGroupClearance", body = list(DataObject = body), searchFields = append("CreditCardGroupClearanceID", body %>% names()), entityId = entityId, schoolYearId = schoolYearId, flatten = flatten, returnResponse = returnResponse)
	}

	#' Modify a CreditCardGroupClearance
	#'
	#' This function modifies a CreditCardGroupClearance
	#' @param fieldNames The field values to give the modified CreditCardGroupClearance. Each defaults to NULL.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{listSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Accounts Payable
	#' @return The modified CreditCardGroupClearance
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	modifyCreditCardGroupClearance <- function(CreditCardGroupClearanceID, CreditCardGroupID = NULL, GroupIDSecurity = NULL, entityId = 1, schoolYearId = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment())

		body <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper()) %>% compact()

		modifySkyObject(module = "AccountsPayable", objectName = "CreditCardGroupClearance", objectId = CreditCardGroupClearanceID, body = list(DataObject = body), searchFields = append("CreditCardGroupClearanceID", body %>% names()), entityId = entityId, schoolYearId = schoolYearId, flatten = flatten, returnResponse = returnResponse)
	}

	#' List CreditCardTransactions
	#'
	#' This function returns a dataframe or json object of CreditCardTransactions
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given CreditCardTransactions. Defaults to FALSE for all return fields which, for convenience, returns all fields for the CreditCardTransactions.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object listed as text. Run \code{\link{getSchemaForObjects}}('CreditCardTransaction') to get more field paths.
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
	#' @concept Accounts Payable
	#' @return A list of CreditCardTransactions
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	listCreditCardTransactions <- function(searchConditionsList = NULL, CreditCardTransactionID = F, CreditCardID = F, FiscalYearID = F, InvoiceDetailID = F, VendorID = F, VendorDescription = F, Status = F, Type = F, TransactionTime = F, BaseCurrencyAmount = F, EntryAmount = F, CurrencyIDEntry = F, Description = F, CreditCardTransactionIDOriginal = F, UserIDCreditCardUsedBy = F, BaseCurrencyAmountByAccount = F, AttachmentCount = F, IsPreApproved = F, CanSubmit = F, IsDataComplete = F, RenderCreditCardTransactionDetailBrowseButtons = F, CanClone = F, CanChangeAccount = F, VendorDisplayName = F, SelectedValueDisplay = F, CurrentUserHasCreditCardGroupAccess = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, CreditCardTransactionThirdPartyImportID = F, CanDelete = F, CanResubmit = F, CanSkipImport = F, fieldPaths = NULL, searchConditionsGroupType = "And", searchSortFieldNamesList = NULL, searchSortFieldNamesDescendingList = NULL, entityId = 1, schoolYearId = NULL, page = 1, pageSize = 100000, flatten = T, returnResponse = F){

		params <- as.list(environment())

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		listSkyObjects(module = "AccountsPayable", objectName = "CreditCardTransaction", searchFields = searchFields %>% append(fieldPaths), page = page, pageSize = pageSize, SearchConditionsList = searchConditionsList, SearchConditionsGroupType = searchConditionsGroupType, SearchSortFieldNamesList = searchSortFieldNamesList, SearchSortFieldNamesDescendingList = searchSortFieldNamesDescendingList, entityId = entityId, schoolYearId = schoolYearId, flatten = flatten, returnResponse = returnResponse)
	}

	#' Get a CreditCardTransaction
	#'
	#' This function returns a dataframe or json object of a CreditCardTransaction
	#' @param CreditCardTransactionID The ID of the CreditCardTransaction to return.
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given CreditCardTransaction. Defaults to FALSE for all return fields which, for convenience, returns all fields for the CreditCardTransaction.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object listed as text. Run \code{\link{getSchemaForObjects}}('CreditCardTransaction') to get more field paths.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{listSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Accounts Payable
	#' @return A dataframe or of CreditCardTransaction
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	getCreditCardTransaction <- function(CreditCardTransactionID, CreditCardID = F, FiscalYearID = F, InvoiceDetailID = F, VendorID = F, VendorDescription = F, Status = F, Type = F, TransactionTime = F, BaseCurrencyAmount = F, EntryAmount = F, CurrencyIDEntry = F, Description = F, CreditCardTransactionIDOriginal = F, UserIDCreditCardUsedBy = F, BaseCurrencyAmountByAccount = F, AttachmentCount = F, IsPreApproved = F, CanSubmit = F, IsDataComplete = F, RenderCreditCardTransactionDetailBrowseButtons = F, CanClone = F, CanChangeAccount = F, VendorDisplayName = F, SelectedValueDisplay = F, CurrentUserHasCreditCardGroupAccess = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, CreditCardTransactionThirdPartyImportID = F, CanDelete = F, CanResubmit = F, CanSkipImport = F, fieldPaths = NULL, entityId = 1, schoolYearId = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment()) %>% purrr::keep(names(.) != "CreditCardTransactionID")

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		getSkyObject(module = "AccountsPayable", objectName = "CreditCardTransaction", objectId = CreditCardTransactionID, searchFields = searchFields, entityId = entityId, flatten = flatten, returnResponse = returnResponse)
	}

	#' Delete a CreditCardTransaction
	#'
	#' This function deletes a CreditCardTransaction
	#' @param CreditCardTransactionID The ID of the CreditCardTransaction to delete
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{listSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Accounts Payable
	#' @return The CreditCardTransactionID of the deleted CreditCardTransaction.
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	deleteCreditCardTransaction <- function(CreditCardTransactionID, ignoreWarnings = F, entityId = 1, schoolYearId = NULL, flatten = T, returnResponse = F){

		deleteSkyObject(module = "AccountsPayable", objectName = "CreditCardTransaction", objectId = CreditCardTransactionID, ignoreWarnings = ignoreWarnings, entityId = entityId, flatten = flatten, returnResponse = returnResponse)
	}

	#' Create a CreditCardTransaction
	#'
	#' This function creates a CreditCardTransaction
	#' @param fieldNames The field values to give the created CreditCardTransaction. Each defaults to NULL.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{listSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Accounts Payable
	#' @return A newly created CreditCardTransaction
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	createCreditCardTransaction <- function(CreditCardID = NULL, FiscalYearID = NULL, InvoiceDetailID = NULL, VendorID = NULL, VendorDescription = NULL, Status = NULL, Type = NULL, TransactionTime = NULL, CurrencyIDEntry = NULL, Description = NULL, CreditCardTransactionIDOriginal = NULL, UserIDCreditCardUsedBy = NULL, IsPreApproved = NULL, CreditCardTransactionThirdPartyImportID = NULL, entityId = 1, schoolYearId = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment())

		body <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper()) %>% compact()

		createSkyObject(module = "AccountsPayable", objectName = "CreditCardTransaction", body = list(DataObject = body), searchFields = append("CreditCardTransactionID", body %>% names()), entityId = entityId, schoolYearId = schoolYearId, flatten = flatten, returnResponse = returnResponse)
	}

	#' Modify a CreditCardTransaction
	#'
	#' This function modifies a CreditCardTransaction
	#' @param fieldNames The field values to give the modified CreditCardTransaction. Each defaults to NULL.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{listSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Accounts Payable
	#' @return The modified CreditCardTransaction
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	modifyCreditCardTransaction <- function(CreditCardTransactionID, CreditCardID = NULL, FiscalYearID = NULL, InvoiceDetailID = NULL, VendorID = NULL, VendorDescription = NULL, Status = NULL, Type = NULL, TransactionTime = NULL, CurrencyIDEntry = NULL, Description = NULL, CreditCardTransactionIDOriginal = NULL, UserIDCreditCardUsedBy = NULL, IsPreApproved = NULL, CreditCardTransactionThirdPartyImportID = NULL, entityId = 1, schoolYearId = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment())

		body <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper()) %>% compact()

		modifySkyObject(module = "AccountsPayable", objectName = "CreditCardTransaction", objectId = CreditCardTransactionID, body = list(DataObject = body), searchFields = append("CreditCardTransactionID", body %>% names()), entityId = entityId, schoolYearId = schoolYearId, flatten = flatten, returnResponse = returnResponse)
	}

	#' List CreditCardTransactionDetails
	#'
	#' This function returns a dataframe or json object of CreditCardTransactionDetails
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given CreditCardTransactionDetails. Defaults to FALSE for all return fields which, for convenience, returns all fields for the CreditCardTransactionDetails.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object listed as text. Run \code{\link{getSchemaForObjects}}('CreditCardTransactionDetail') to get more field paths.
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
	#' @concept Accounts Payable
	#' @return A list of CreditCardTransactionDetails
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	listCreditCardTransactionDetails <- function(searchConditionsList = NULL, CreditCardTransactionDetailID = F, CreditCardTransactionID = F, Description = F, EntryAmount = F, BaseCurrencyAmount = F, Quantity = F, UnitCost = F, DisplayOrder = F, CreditCardTransactionTotalLessEntryAmount = F, CurrentUserHasCreditCardGroupAccess = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, fieldPaths = NULL, searchConditionsGroupType = "And", searchSortFieldNamesList = NULL, searchSortFieldNamesDescendingList = NULL, entityId = 1, schoolYearId = NULL, page = 1, pageSize = 100000, flatten = T, returnResponse = F){

		params <- as.list(environment())

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		listSkyObjects(module = "AccountsPayable", objectName = "CreditCardTransactionDetail", searchFields = searchFields %>% append(fieldPaths), page = page, pageSize = pageSize, SearchConditionsList = searchConditionsList, SearchConditionsGroupType = searchConditionsGroupType, SearchSortFieldNamesList = searchSortFieldNamesList, SearchSortFieldNamesDescendingList = searchSortFieldNamesDescendingList, entityId = entityId, schoolYearId = schoolYearId, flatten = flatten, returnResponse = returnResponse)
	}

	#' Get a CreditCardTransactionDetail
	#'
	#' This function returns a dataframe or json object of a CreditCardTransactionDetail
	#' @param CreditCardTransactionDetailID The ID of the CreditCardTransactionDetail to return.
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given CreditCardTransactionDetail. Defaults to FALSE for all return fields which, for convenience, returns all fields for the CreditCardTransactionDetail.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object listed as text. Run \code{\link{getSchemaForObjects}}('CreditCardTransactionDetail') to get more field paths.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{listSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Accounts Payable
	#' @return A dataframe or of CreditCardTransactionDetail
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	getCreditCardTransactionDetail <- function(CreditCardTransactionDetailID, CreditCardTransactionID = F, Description = F, EntryAmount = F, BaseCurrencyAmount = F, Quantity = F, UnitCost = F, DisplayOrder = F, CreditCardTransactionTotalLessEntryAmount = F, CurrentUserHasCreditCardGroupAccess = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, fieldPaths = NULL, entityId = 1, schoolYearId = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment()) %>% purrr::keep(names(.) != "CreditCardTransactionDetailID")

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		getSkyObject(module = "AccountsPayable", objectName = "CreditCardTransactionDetail", objectId = CreditCardTransactionDetailID, searchFields = searchFields, entityId = entityId, flatten = flatten, returnResponse = returnResponse)
	}

	#' Delete a CreditCardTransactionDetail
	#'
	#' This function deletes a CreditCardTransactionDetail
	#' @param CreditCardTransactionDetailID The ID of the CreditCardTransactionDetail to delete
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{listSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Accounts Payable
	#' @return The CreditCardTransactionDetailID of the deleted CreditCardTransactionDetail.
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	deleteCreditCardTransactionDetail <- function(CreditCardTransactionDetailID, ignoreWarnings = F, entityId = 1, schoolYearId = NULL, flatten = T, returnResponse = F){

		deleteSkyObject(module = "AccountsPayable", objectName = "CreditCardTransactionDetail", objectId = CreditCardTransactionDetailID, ignoreWarnings = ignoreWarnings, entityId = entityId, flatten = flatten, returnResponse = returnResponse)
	}

	#' Create a CreditCardTransactionDetail
	#'
	#' This function creates a CreditCardTransactionDetail
	#' @param fieldNames The field values to give the created CreditCardTransactionDetail. Each defaults to NULL.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{listSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Accounts Payable
	#' @return A newly created CreditCardTransactionDetail
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	createCreditCardTransactionDetail <- function(CreditCardTransactionID = NULL, Description = NULL, EntryAmount = NULL, BaseCurrencyAmount = NULL, Quantity = NULL, UnitCost = NULL, DisplayOrder = NULL, entityId = 1, schoolYearId = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment())

		body <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper()) %>% compact()

		createSkyObject(module = "AccountsPayable", objectName = "CreditCardTransactionDetail", body = list(DataObject = body), searchFields = append("CreditCardTransactionDetailID", body %>% names()), entityId = entityId, schoolYearId = schoolYearId, flatten = flatten, returnResponse = returnResponse)
	}

	#' Modify a CreditCardTransactionDetail
	#'
	#' This function modifies a CreditCardTransactionDetail
	#' @param fieldNames The field values to give the modified CreditCardTransactionDetail. Each defaults to NULL.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{listSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Accounts Payable
	#' @return The modified CreditCardTransactionDetail
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	modifyCreditCardTransactionDetail <- function(CreditCardTransactionDetailID, CreditCardTransactionID = NULL, Description = NULL, EntryAmount = NULL, BaseCurrencyAmount = NULL, Quantity = NULL, UnitCost = NULL, DisplayOrder = NULL, entityId = 1, schoolYearId = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment())

		body <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper()) %>% compact()

		modifySkyObject(module = "AccountsPayable", objectName = "CreditCardTransactionDetail", objectId = CreditCardTransactionDetailID, body = list(DataObject = body), searchFields = append("CreditCardTransactionDetailID", body %>% names()), entityId = entityId, schoolYearId = schoolYearId, flatten = flatten, returnResponse = returnResponse)
	}

	#' List CreditCardTransactionDetailAccountings
	#'
	#' This function returns a dataframe or json object of CreditCardTransactionDetailAccountings
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given CreditCardTransactionDetailAccountings. Defaults to FALSE for all return fields which, for convenience, returns all fields for the CreditCardTransactionDetailAccountings.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object listed as text. Run \code{\link{getSchemaForObjects}}('CreditCardTransactionDetailAccounting') to get more field paths.
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
	#' @concept Accounts Payable
	#' @return A list of CreditCardTransactionDetailAccountings
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	listCreditCardTransactionDetailAccountings <- function(searchConditionsList = NULL, CreditCardTransactionDetailAccountingID = F, CreditCardTransactionDetailID = F, EntryAmount = F, BaseCurrencyAmount = F, AccountID = F, BaseCurrencyPercent = F, CurrentUserHasCreditCardGroupAccess = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, CurrentUserHasAccountGroupAccess = F, GrantID = F, ProjectID = F, fieldPaths = NULL, searchConditionsGroupType = "And", searchSortFieldNamesList = NULL, searchSortFieldNamesDescendingList = NULL, entityId = 1, schoolYearId = NULL, page = 1, pageSize = 100000, flatten = T, returnResponse = F){

		params <- as.list(environment())

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		listSkyObjects(module = "AccountsPayable", objectName = "CreditCardTransactionDetailAccounting", searchFields = searchFields %>% append(fieldPaths), page = page, pageSize = pageSize, SearchConditionsList = searchConditionsList, SearchConditionsGroupType = searchConditionsGroupType, SearchSortFieldNamesList = searchSortFieldNamesList, SearchSortFieldNamesDescendingList = searchSortFieldNamesDescendingList, entityId = entityId, schoolYearId = schoolYearId, flatten = flatten, returnResponse = returnResponse)
	}

	#' Get a CreditCardTransactionDetailAccounting
	#'
	#' This function returns a dataframe or json object of a CreditCardTransactionDetailAccounting
	#' @param CreditCardTransactionDetailAccountingID The ID of the CreditCardTransactionDetailAccounting to return.
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given CreditCardTransactionDetailAccounting. Defaults to FALSE for all return fields which, for convenience, returns all fields for the CreditCardTransactionDetailAccounting.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object listed as text. Run \code{\link{getSchemaForObjects}}('CreditCardTransactionDetailAccounting') to get more field paths.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{listSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Accounts Payable
	#' @return A dataframe or of CreditCardTransactionDetailAccounting
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	getCreditCardTransactionDetailAccounting <- function(CreditCardTransactionDetailAccountingID, CreditCardTransactionDetailID = F, EntryAmount = F, BaseCurrencyAmount = F, AccountID = F, BaseCurrencyPercent = F, CurrentUserHasCreditCardGroupAccess = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, CurrentUserHasAccountGroupAccess = F, GrantID = F, ProjectID = F, fieldPaths = NULL, entityId = 1, schoolYearId = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment()) %>% purrr::keep(names(.) != "CreditCardTransactionDetailAccountingID")

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		getSkyObject(module = "AccountsPayable", objectName = "CreditCardTransactionDetailAccounting", objectId = CreditCardTransactionDetailAccountingID, searchFields = searchFields, entityId = entityId, flatten = flatten, returnResponse = returnResponse)
	}

	#' Delete a CreditCardTransactionDetailAccounting
	#'
	#' This function deletes a CreditCardTransactionDetailAccounting
	#' @param CreditCardTransactionDetailAccountingID The ID of the CreditCardTransactionDetailAccounting to delete
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{listSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Accounts Payable
	#' @return The CreditCardTransactionDetailAccountingID of the deleted CreditCardTransactionDetailAccounting.
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	deleteCreditCardTransactionDetailAccounting <- function(CreditCardTransactionDetailAccountingID, ignoreWarnings = F, entityId = 1, schoolYearId = NULL, flatten = T, returnResponse = F){

		deleteSkyObject(module = "AccountsPayable", objectName = "CreditCardTransactionDetailAccounting", objectId = CreditCardTransactionDetailAccountingID, ignoreWarnings = ignoreWarnings, entityId = entityId, flatten = flatten, returnResponse = returnResponse)
	}

	#' Create a CreditCardTransactionDetailAccounting
	#'
	#' This function creates a CreditCardTransactionDetailAccounting
	#' @param fieldNames The field values to give the created CreditCardTransactionDetailAccounting. Each defaults to NULL.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{listSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Accounts Payable
	#' @return A newly created CreditCardTransactionDetailAccounting
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	createCreditCardTransactionDetailAccounting <- function(CreditCardTransactionDetailID = NULL, EntryAmount = NULL, BaseCurrencyAmount = NULL, AccountID = NULL, GrantID = NULL, ProjectID = NULL, entityId = 1, schoolYearId = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment())

		body <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper()) %>% compact()

		createSkyObject(module = "AccountsPayable", objectName = "CreditCardTransactionDetailAccounting", body = list(DataObject = body), searchFields = append("CreditCardTransactionDetailAccountingID", body %>% names()), entityId = entityId, schoolYearId = schoolYearId, flatten = flatten, returnResponse = returnResponse)
	}

	#' Modify a CreditCardTransactionDetailAccounting
	#'
	#' This function modifies a CreditCardTransactionDetailAccounting
	#' @param fieldNames The field values to give the modified CreditCardTransactionDetailAccounting. Each defaults to NULL.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{listSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Accounts Payable
	#' @return The modified CreditCardTransactionDetailAccounting
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	modifyCreditCardTransactionDetailAccounting <- function(CreditCardTransactionDetailAccountingID, CreditCardTransactionDetailID = NULL, EntryAmount = NULL, BaseCurrencyAmount = NULL, AccountID = NULL, GrantID = NULL, ProjectID = NULL, entityId = 1, schoolYearId = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment())

		body <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper()) %>% compact()

		modifySkyObject(module = "AccountsPayable", objectName = "CreditCardTransactionDetailAccounting", objectId = CreditCardTransactionDetailAccountingID, body = list(DataObject = body), searchFields = append("CreditCardTransactionDetailAccountingID", body %>% names()), entityId = entityId, schoolYearId = schoolYearId, flatten = flatten, returnResponse = returnResponse)
	}

	#' List ExpenseReimbursementApprovals
	#'
	#' This function returns a dataframe or json object of ExpenseReimbursementApprovals
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given ExpenseReimbursementApprovals. Defaults to FALSE for all return fields which, for convenience, returns all fields for the ExpenseReimbursementApprovals.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object listed as text. Run \code{\link{getSchemaForObjects}}('ExpenseReimbursementApproval') to get more field paths.
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
	#' @concept Accounts Payable
	#' @return A list of ExpenseReimbursementApprovals
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	listExpenseReimbursementApprovals <- function(searchConditionsList = NULL, ExpenseReimbursementApprovalID = F, ExpenseReimbursementID = F, UserIDApprover = F, Comment = F, TaskInstanceID = F, Status = F, Level = F, Type = F, LevelDescription = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, OrganizationChartRelationshipID = F, ApprovalActionTime = F, fieldPaths = NULL, searchConditionsGroupType = "And", searchSortFieldNamesList = NULL, searchSortFieldNamesDescendingList = NULL, entityId = 1, schoolYearId = NULL, page = 1, pageSize = 100000, flatten = T, returnResponse = F){

		params <- as.list(environment())

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		listSkyObjects(module = "AccountsPayable", objectName = "ExpenseReimbursementApproval", searchFields = searchFields %>% append(fieldPaths), page = page, pageSize = pageSize, SearchConditionsList = searchConditionsList, SearchConditionsGroupType = searchConditionsGroupType, SearchSortFieldNamesList = searchSortFieldNamesList, SearchSortFieldNamesDescendingList = searchSortFieldNamesDescendingList, entityId = entityId, schoolYearId = schoolYearId, flatten = flatten, returnResponse = returnResponse)
	}

	#' Get an ExpenseReimbursementApproval
	#'
	#' This function returns a dataframe or json object of an ExpenseReimbursementApproval
	#' @param ExpenseReimbursementApprovalID The ID of the ExpenseReimbursementApproval to return.
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given ExpenseReimbursementApproval. Defaults to FALSE for all return fields which, for convenience, returns all fields for the ExpenseReimbursementApproval.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object listed as text. Run \code{\link{getSchemaForObjects}}('ExpenseReimbursementApproval') to get more field paths.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{listSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Accounts Payable
	#' @return A dataframe or of ExpenseReimbursementApproval
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	getExpenseReimbursementApproval <- function(ExpenseReimbursementApprovalID, ExpenseReimbursementID = F, UserIDApprover = F, Comment = F, TaskInstanceID = F, Status = F, Level = F, Type = F, LevelDescription = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, OrganizationChartRelationshipID = F, ApprovalActionTime = F, fieldPaths = NULL, entityId = 1, schoolYearId = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment()) %>% purrr::keep(names(.) != "ExpenseReimbursementApprovalID")

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		getSkyObject(module = "AccountsPayable", objectName = "ExpenseReimbursementApproval", objectId = ExpenseReimbursementApprovalID, searchFields = searchFields, entityId = entityId, flatten = flatten, returnResponse = returnResponse)
	}

	#' Delete an ExpenseReimbursementApproval
	#'
	#' This function deletes an ExpenseReimbursementApproval
	#' @param ExpenseReimbursementApprovalID The ID of the ExpenseReimbursementApproval to delete
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{listSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Accounts Payable
	#' @return The ExpenseReimbursementApprovalID of the deleted ExpenseReimbursementApproval.
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	deleteExpenseReimbursementApproval <- function(ExpenseReimbursementApprovalID, ignoreWarnings = F, entityId = 1, schoolYearId = NULL, flatten = T, returnResponse = F){

		deleteSkyObject(module = "AccountsPayable", objectName = "ExpenseReimbursementApproval", objectId = ExpenseReimbursementApprovalID, ignoreWarnings = ignoreWarnings, entityId = entityId, flatten = flatten, returnResponse = returnResponse)
	}

	#' Create an ExpenseReimbursementApproval
	#'
	#' This function creates an ExpenseReimbursementApproval
	#' @param fieldNames The field values to give the created ExpenseReimbursementApproval. Each defaults to NULL.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{listSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Accounts Payable
	#' @return A newly created ExpenseReimbursementApproval
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	createExpenseReimbursementApproval <- function(ExpenseReimbursementID = NULL, UserIDApprover = NULL, Comment = NULL, TaskInstanceID = NULL, Status = NULL, Level = NULL, Type = NULL, LevelDescription = NULL, OrganizationChartRelationshipID = NULL, ApprovalActionTime = NULL, entityId = 1, schoolYearId = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment())

		body <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper()) %>% compact()

		createSkyObject(module = "AccountsPayable", objectName = "ExpenseReimbursementApproval", body = list(DataObject = body), searchFields = append("ExpenseReimbursementApprovalID", body %>% names()), entityId = entityId, schoolYearId = schoolYearId, flatten = flatten, returnResponse = returnResponse)
	}

	#' Modify an ExpenseReimbursementApproval
	#'
	#' This function modifies an ExpenseReimbursementApproval
	#' @param fieldNames The field values to give the modified ExpenseReimbursementApproval. Each defaults to NULL.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{listSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Accounts Payable
	#' @return The modified ExpenseReimbursementApproval
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	modifyExpenseReimbursementApproval <- function(ExpenseReimbursementApprovalID, ExpenseReimbursementID = NULL, UserIDApprover = NULL, Comment = NULL, TaskInstanceID = NULL, Status = NULL, Level = NULL, Type = NULL, LevelDescription = NULL, OrganizationChartRelationshipID = NULL, ApprovalActionTime = NULL, entityId = 1, schoolYearId = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment())

		body <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper()) %>% compact()

		modifySkyObject(module = "AccountsPayable", objectName = "ExpenseReimbursementApproval", objectId = ExpenseReimbursementApprovalID, body = list(DataObject = body), searchFields = append("ExpenseReimbursementApprovalID", body %>% names()), entityId = entityId, schoolYearId = schoolYearId, flatten = flatten, returnResponse = returnResponse)
	}

	#' List ExpenseReimbursementGroupApprovalTasks
	#'
	#' This function returns a dataframe or json object of ExpenseReimbursementGroupApprovalTasks
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given ExpenseReimbursementGroupApprovalTasks. Defaults to FALSE for all return fields which, for convenience, returns all fields for the ExpenseReimbursementGroupApprovalTasks.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object listed as text. Run \code{\link{getSchemaForObjects}}('ExpenseReimbursementGroupApprovalTask') to get more field paths.
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
	#' @concept Accounts Payable
	#' @return A list of ExpenseReimbursementGroupApprovalTasks
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	listExpenseReimbursementGroupApprovalTasks <- function(searchConditionsList = NULL, ExpenseReimbursementGroupApprovalTaskID = F, ExpenseReimbursementGroupID = F, IsConditional = F, Level = F, Description = F, XMLFilter = F, SecurityStandardFilterData = F, Type = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, UseOrganizationChart = F, fieldPaths = NULL, searchConditionsGroupType = "And", searchSortFieldNamesList = NULL, searchSortFieldNamesDescendingList = NULL, entityId = 1, schoolYearId = NULL, page = 1, pageSize = 100000, flatten = T, returnResponse = F){

		params <- as.list(environment())

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		listSkyObjects(module = "AccountsPayable", objectName = "ExpenseReimbursementGroupApprovalTask", searchFields = searchFields %>% append(fieldPaths), page = page, pageSize = pageSize, SearchConditionsList = searchConditionsList, SearchConditionsGroupType = searchConditionsGroupType, SearchSortFieldNamesList = searchSortFieldNamesList, SearchSortFieldNamesDescendingList = searchSortFieldNamesDescendingList, entityId = entityId, schoolYearId = schoolYearId, flatten = flatten, returnResponse = returnResponse)
	}

	#' Get an ExpenseReimbursementGroupApprovalTask
	#'
	#' This function returns a dataframe or json object of an ExpenseReimbursementGroupApprovalTask
	#' @param ExpenseReimbursementGroupApprovalTaskID The ID of the ExpenseReimbursementGroupApprovalTask to return.
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given ExpenseReimbursementGroupApprovalTask. Defaults to FALSE for all return fields which, for convenience, returns all fields for the ExpenseReimbursementGroupApprovalTask.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object listed as text. Run \code{\link{getSchemaForObjects}}('ExpenseReimbursementGroupApprovalTask') to get more field paths.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{listSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Accounts Payable
	#' @return A dataframe or of ExpenseReimbursementGroupApprovalTask
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	getExpenseReimbursementGroupApprovalTask <- function(ExpenseReimbursementGroupApprovalTaskID, ExpenseReimbursementGroupID = F, IsConditional = F, Level = F, Description = F, XMLFilter = F, SecurityStandardFilterData = F, Type = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, UseOrganizationChart = F, fieldPaths = NULL, entityId = 1, schoolYearId = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment()) %>% purrr::keep(names(.) != "ExpenseReimbursementGroupApprovalTaskID")

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		getSkyObject(module = "AccountsPayable", objectName = "ExpenseReimbursementGroupApprovalTask", objectId = ExpenseReimbursementGroupApprovalTaskID, searchFields = searchFields, entityId = entityId, flatten = flatten, returnResponse = returnResponse)
	}

	#' Delete an ExpenseReimbursementGroupApprovalTask
	#'
	#' This function deletes an ExpenseReimbursementGroupApprovalTask
	#' @param ExpenseReimbursementGroupApprovalTaskID The ID of the ExpenseReimbursementGroupApprovalTask to delete
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{listSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Accounts Payable
	#' @return The ExpenseReimbursementGroupApprovalTaskID of the deleted ExpenseReimbursementGroupApprovalTask.
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	deleteExpenseReimbursementGroupApprovalTask <- function(ExpenseReimbursementGroupApprovalTaskID, ignoreWarnings = F, entityId = 1, schoolYearId = NULL, flatten = T, returnResponse = F){

		deleteSkyObject(module = "AccountsPayable", objectName = "ExpenseReimbursementGroupApprovalTask", objectId = ExpenseReimbursementGroupApprovalTaskID, ignoreWarnings = ignoreWarnings, entityId = entityId, flatten = flatten, returnResponse = returnResponse)
	}

	#' Create an ExpenseReimbursementGroupApprovalTask
	#'
	#' This function creates an ExpenseReimbursementGroupApprovalTask
	#' @param fieldNames The field values to give the created ExpenseReimbursementGroupApprovalTask. Each defaults to NULL.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{listSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Accounts Payable
	#' @return A newly created ExpenseReimbursementGroupApprovalTask
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	createExpenseReimbursementGroupApprovalTask <- function(ExpenseReimbursementGroupID = NULL, IsConditional = NULL, Level = NULL, Description = NULL, XMLFilter = NULL, Type = NULL, UseOrganizationChart = NULL, entityId = 1, schoolYearId = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment())

		body <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper()) %>% compact()

		createSkyObject(module = "AccountsPayable", objectName = "ExpenseReimbursementGroupApprovalTask", body = list(DataObject = body), searchFields = append("ExpenseReimbursementGroupApprovalTaskID", body %>% names()), entityId = entityId, schoolYearId = schoolYearId, flatten = flatten, returnResponse = returnResponse)
	}

	#' Modify an ExpenseReimbursementGroupApprovalTask
	#'
	#' This function modifies an ExpenseReimbursementGroupApprovalTask
	#' @param fieldNames The field values to give the modified ExpenseReimbursementGroupApprovalTask. Each defaults to NULL.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{listSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Accounts Payable
	#' @return The modified ExpenseReimbursementGroupApprovalTask
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	modifyExpenseReimbursementGroupApprovalTask <- function(ExpenseReimbursementGroupApprovalTaskID, ExpenseReimbursementGroupID = NULL, IsConditional = NULL, Level = NULL, Description = NULL, XMLFilter = NULL, Type = NULL, UseOrganizationChart = NULL, entityId = 1, schoolYearId = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment())

		body <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper()) %>% compact()

		modifySkyObject(module = "AccountsPayable", objectName = "ExpenseReimbursementGroupApprovalTask", objectId = ExpenseReimbursementGroupApprovalTaskID, body = list(DataObject = body), searchFields = append("ExpenseReimbursementGroupApprovalTaskID", body %>% names()), entityId = entityId, schoolYearId = schoolYearId, flatten = flatten, returnResponse = returnResponse)
	}

	#' List ExpenseReimbursementGroupApprovalTaskSecurityGroups
	#'
	#' This function returns a dataframe or json object of ExpenseReimbursementGroupApprovalTaskSecurityGroups
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given ExpenseReimbursementGroupApprovalTaskSecurityGroups. Defaults to FALSE for all return fields which, for convenience, returns all fields for the ExpenseReimbursementGroupApprovalTaskSecurityGroups.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object listed as text. Run \code{\link{getSchemaForObjects}}('ExpenseReimbursementGroupApprovalTaskSecurityGroup') to get more field paths.
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
	#' @concept Accounts Payable
	#' @return A list of ExpenseReimbursementGroupApprovalTaskSecurityGroups
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	listExpenseReimbursementGroupApprovalTaskSecurityGroups <- function(searchConditionsList = NULL, ExpenseReimbursementGroupApprovalTaskSecurityGroupID = F, ExpenseReimbursementGroupApprovalTaskID = F, GroupIDSecurity = F, IsAlternate = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, fieldPaths = NULL, searchConditionsGroupType = "And", searchSortFieldNamesList = NULL, searchSortFieldNamesDescendingList = NULL, entityId = 1, schoolYearId = NULL, page = 1, pageSize = 100000, flatten = T, returnResponse = F){

		params <- as.list(environment())

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		listSkyObjects(module = "AccountsPayable", objectName = "ExpenseReimbursementGroupApprovalTaskSecurityGroup", searchFields = searchFields %>% append(fieldPaths), page = page, pageSize = pageSize, SearchConditionsList = searchConditionsList, SearchConditionsGroupType = searchConditionsGroupType, SearchSortFieldNamesList = searchSortFieldNamesList, SearchSortFieldNamesDescendingList = searchSortFieldNamesDescendingList, entityId = entityId, schoolYearId = schoolYearId, flatten = flatten, returnResponse = returnResponse)
	}

	#' Get an ExpenseReimbursementGroupApprovalTaskSecurityGroup
	#'
	#' This function returns a dataframe or json object of an ExpenseReimbursementGroupApprovalTaskSecurityGroup
	#' @param ExpenseReimbursementGroupApprovalTaskSecurityGroupID The ID of the ExpenseReimbursementGroupApprovalTaskSecurityGroup to return.
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given ExpenseReimbursementGroupApprovalTaskSecurityGroup. Defaults to FALSE for all return fields which, for convenience, returns all fields for the ExpenseReimbursementGroupApprovalTaskSecurityGroup.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object listed as text. Run \code{\link{getSchemaForObjects}}('ExpenseReimbursementGroupApprovalTaskSecurityGroup') to get more field paths.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{listSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Accounts Payable
	#' @return A dataframe or of ExpenseReimbursementGroupApprovalTaskSecurityGroup
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	getExpenseReimbursementGroupApprovalTaskSecurityGroup <- function(ExpenseReimbursementGroupApprovalTaskSecurityGroupID, ExpenseReimbursementGroupApprovalTaskID = F, GroupIDSecurity = F, IsAlternate = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, fieldPaths = NULL, entityId = 1, schoolYearId = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment()) %>% purrr::keep(names(.) != "ExpenseReimbursementGroupApprovalTaskSecurityGroupID")

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		getSkyObject(module = "AccountsPayable", objectName = "ExpenseReimbursementGroupApprovalTaskSecurityGroup", objectId = ExpenseReimbursementGroupApprovalTaskSecurityGroupID, searchFields = searchFields, entityId = entityId, flatten = flatten, returnResponse = returnResponse)
	}

	#' Delete an ExpenseReimbursementGroupApprovalTaskSecurityGroup
	#'
	#' This function deletes an ExpenseReimbursementGroupApprovalTaskSecurityGroup
	#' @param ExpenseReimbursementGroupApprovalTaskSecurityGroupID The ID of the ExpenseReimbursementGroupApprovalTaskSecurityGroup to delete
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{listSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Accounts Payable
	#' @return The ExpenseReimbursementGroupApprovalTaskSecurityGroupID of the deleted ExpenseReimbursementGroupApprovalTaskSecurityGroup.
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	deleteExpenseReimbursementGroupApprovalTaskSecurityGroup <- function(ExpenseReimbursementGroupApprovalTaskSecurityGroupID, ignoreWarnings = F, entityId = 1, schoolYearId = NULL, flatten = T, returnResponse = F){

		deleteSkyObject(module = "AccountsPayable", objectName = "ExpenseReimbursementGroupApprovalTaskSecurityGroup", objectId = ExpenseReimbursementGroupApprovalTaskSecurityGroupID, ignoreWarnings = ignoreWarnings, entityId = entityId, flatten = flatten, returnResponse = returnResponse)
	}

	#' Create an ExpenseReimbursementGroupApprovalTaskSecurityGroup
	#'
	#' This function creates an ExpenseReimbursementGroupApprovalTaskSecurityGroup
	#' @param fieldNames The field values to give the created ExpenseReimbursementGroupApprovalTaskSecurityGroup. Each defaults to NULL.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{listSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Accounts Payable
	#' @return A newly created ExpenseReimbursementGroupApprovalTaskSecurityGroup
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	createExpenseReimbursementGroupApprovalTaskSecurityGroup <- function(ExpenseReimbursementGroupApprovalTaskID = NULL, GroupIDSecurity = NULL, IsAlternate = NULL, entityId = 1, schoolYearId = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment())

		body <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper()) %>% compact()

		createSkyObject(module = "AccountsPayable", objectName = "ExpenseReimbursementGroupApprovalTaskSecurityGroup", body = list(DataObject = body), searchFields = append("ExpenseReimbursementGroupApprovalTaskSecurityGroupID", body %>% names()), entityId = entityId, schoolYearId = schoolYearId, flatten = flatten, returnResponse = returnResponse)
	}

	#' Modify an ExpenseReimbursementGroupApprovalTaskSecurityGroup
	#'
	#' This function modifies an ExpenseReimbursementGroupApprovalTaskSecurityGroup
	#' @param fieldNames The field values to give the modified ExpenseReimbursementGroupApprovalTaskSecurityGroup. Each defaults to NULL.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{listSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Accounts Payable
	#' @return The modified ExpenseReimbursementGroupApprovalTaskSecurityGroup
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	modifyExpenseReimbursementGroupApprovalTaskSecurityGroup <- function(ExpenseReimbursementGroupApprovalTaskSecurityGroupID, ExpenseReimbursementGroupApprovalTaskID = NULL, GroupIDSecurity = NULL, IsAlternate = NULL, entityId = 1, schoolYearId = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment())

		body <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper()) %>% compact()

		modifySkyObject(module = "AccountsPayable", objectName = "ExpenseReimbursementGroupApprovalTaskSecurityGroup", objectId = ExpenseReimbursementGroupApprovalTaskSecurityGroupID, body = list(DataObject = body), searchFields = append("ExpenseReimbursementGroupApprovalTaskSecurityGroupID", body %>% names()), entityId = entityId, schoolYearId = schoolYearId, flatten = flatten, returnResponse = returnResponse)
	}

	#' List AccountsPayableConfigFiscalYears
	#'
	#' This function returns a dataframe or json object of AccountsPayableConfigFiscalYears
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given AccountsPayableConfigFiscalYears. Defaults to FALSE for all return fields which, for convenience, returns all fields for the AccountsPayableConfigFiscalYears.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object listed as text. Run \code{\link{getSchemaForObjects}}('AccountsPayableConfigFiscalYear') to get more field paths.
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
	#' @concept Accounts Payable
	#' @return A list of AccountsPayableConfigFiscalYears
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	listAccountsPayableConfigFiscalYears <- function(searchConditionsList = NULL, ConfigFiscalYearID = F, FiscalYearID = F, DistrictID = F, OrganizationChartIDExpenseReimbursementApproval = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, fieldPaths = NULL, searchConditionsGroupType = "And", searchSortFieldNamesList = NULL, searchSortFieldNamesDescendingList = NULL, entityId = 1, schoolYearId = NULL, page = 1, pageSize = 100000, flatten = T, returnResponse = F){

		params <- as.list(environment())

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		listSkyObjects(module = "AccountsPayable", objectName = "ConfigFiscalYear", searchFields = searchFields %>% append(fieldPaths), page = page, pageSize = pageSize, SearchConditionsList = searchConditionsList, SearchConditionsGroupType = searchConditionsGroupType, SearchSortFieldNamesList = searchSortFieldNamesList, SearchSortFieldNamesDescendingList = searchSortFieldNamesDescendingList, entityId = entityId, schoolYearId = schoolYearId, flatten = flatten, returnResponse = returnResponse)
	}

	#' Get an AccountsPayableConfigFiscalYear
	#'
	#' This function returns a dataframe or json object of an AccountsPayableConfigFiscalYear
	#' @param AccountsPayableConfigFiscalYearID The ID of the AccountsPayableConfigFiscalYear to return.
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given AccountsPayableConfigFiscalYear. Defaults to FALSE for all return fields which, for convenience, returns all fields for the AccountsPayableConfigFiscalYear.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object listed as text. Run \code{\link{getSchemaForObjects}}('AccountsPayableConfigFiscalYear') to get more field paths.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{listSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Accounts Payable
	#' @return A dataframe or of AccountsPayableConfigFiscalYear
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	getAccountsPayableConfigFiscalYear <- function(AccountsPayableConfigFiscalYearID, ConfigFiscalYearID = F, FiscalYearID = F, DistrictID = F, OrganizationChartIDExpenseReimbursementApproval = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, fieldPaths = NULL, entityId = 1, schoolYearId = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment()) %>% purrr::keep(names(.) != "AccountsPayableConfigFiscalYearID")

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		getSkyObject(module = "AccountsPayable", objectName = "ConfigFiscalYear", objectId = AccountsPayableConfigFiscalYearID, searchFields = searchFields, entityId = entityId, flatten = flatten, returnResponse = returnResponse)
	}

	#' Delete an AccountsPayableConfigFiscalYear
	#'
	#' This function deletes an AccountsPayableConfigFiscalYear
	#' @param AccountsPayableConfigFiscalYearID The ID of the AccountsPayableConfigFiscalYear to delete
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{listSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Accounts Payable
	#' @return The AccountsPayableConfigFiscalYearID of the deleted AccountsPayableConfigFiscalYear.
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	deleteAccountsPayableConfigFiscalYear <- function(AccountsPayableConfigFiscalYearID, ignoreWarnings = F, entityId = 1, schoolYearId = NULL, flatten = T, returnResponse = F){

		deleteSkyObject(module = "AccountsPayable", objectName = "ConfigFiscalYear", objectId = AccountsPayableConfigFiscalYearID, ignoreWarnings = ignoreWarnings, entityId = entityId, flatten = flatten, returnResponse = returnResponse)
	}

	#' Create an AccountsPayableConfigFiscalYear
	#'
	#' This function creates an AccountsPayableConfigFiscalYear
	#' @param fieldNames The field values to give the created AccountsPayableConfigFiscalYear. Each defaults to NULL.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{listSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Accounts Payable
	#' @return A newly created AccountsPayableConfigFiscalYear
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	createAccountsPayableConfigFiscalYear <- function(FiscalYearID = NULL, DistrictID = NULL, OrganizationChartIDExpenseReimbursementApproval = NULL, entityId = 1, schoolYearId = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment())

		body <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper()) %>% compact()

		createSkyObject(module = "AccountsPayable", objectName = "ConfigFiscalYear", body = list(DataObject = body), searchFields = append("ConfigFiscalYearID", body %>% names()), entityId = entityId, schoolYearId = schoolYearId, flatten = flatten, returnResponse = returnResponse)
	}

	#' Modify an AccountsPayableConfigFiscalYear
	#'
	#' This function modifies an AccountsPayableConfigFiscalYear
	#' @param fieldNames The field values to give the modified AccountsPayableConfigFiscalYear. Each defaults to NULL.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{listSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Accounts Payable
	#' @return The modified AccountsPayableConfigFiscalYear
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	modifyAccountsPayableConfigFiscalYear <- function(ConfigFiscalYearID, FiscalYearID = NULL, DistrictID = NULL, OrganizationChartIDExpenseReimbursementApproval = NULL, entityId = 1, schoolYearId = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment())

		body <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper()) %>% compact()

		modifySkyObject(module = "AccountsPayable", objectName = "ConfigFiscalYear", objectId = ConfigFiscalYearID, body = list(DataObject = body), searchFields = append("ConfigFiscalYearID", body %>% names()), entityId = entityId, schoolYearId = schoolYearId, flatten = flatten, returnResponse = returnResponse)
	}

	#' List TempCreditCardTransactions
	#'
	#' This function returns a dataframe or json object of TempCreditCardTransactions
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given TempCreditCardTransactions. Defaults to FALSE for all return fields which, for convenience, returns all fields for the TempCreditCardTransactions.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object listed as text. Run \code{\link{getSchemaForObjects}}('TempCreditCardTransaction') to get more field paths.
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
	#' @concept Accounts Payable
	#' @return A list of TempCreditCardTransactions
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	listTempCreditCardTransactions <- function(searchConditionsList = NULL, TempCreditCardTransactionID = F, TransactionTime = F, CreditCardGroup = F, CreditCard = F, VendorID = F, VendorName = F, Status = F, Description = F, Amount = F, FiscalYear = F, ErrorMessage = F, Severity = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, CreditCardTransactionID = F, ErrorCount = F, VendorDescription = F, UserIDCreditCardUsedBy = F, UserCreditCardUsedBy = F, CreditCardID = F, FiscalYearID = F, HasErrors = F, fieldPaths = NULL, searchConditionsGroupType = "And", searchSortFieldNamesList = NULL, searchSortFieldNamesDescendingList = NULL, entityId = 1, schoolYearId = NULL, page = 1, pageSize = 100000, flatten = T, returnResponse = F){

		params <- as.list(environment())

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		listSkyObjects(module = "AccountsPayable", objectName = "TempCreditCardTransaction", searchFields = searchFields %>% append(fieldPaths), page = page, pageSize = pageSize, SearchConditionsList = searchConditionsList, SearchConditionsGroupType = searchConditionsGroupType, SearchSortFieldNamesList = searchSortFieldNamesList, SearchSortFieldNamesDescendingList = searchSortFieldNamesDescendingList, entityId = entityId, schoolYearId = schoolYearId, flatten = flatten, returnResponse = returnResponse)
	}

	#' Get a TempCreditCardTransaction
	#'
	#' This function returns a dataframe or json object of a TempCreditCardTransaction
	#' @param TempCreditCardTransactionID The ID of the TempCreditCardTransaction to return.
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given TempCreditCardTransaction. Defaults to FALSE for all return fields which, for convenience, returns all fields for the TempCreditCardTransaction.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object listed as text. Run \code{\link{getSchemaForObjects}}('TempCreditCardTransaction') to get more field paths.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{listSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Accounts Payable
	#' @return A dataframe or of TempCreditCardTransaction
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	getTempCreditCardTransaction <- function(TempCreditCardTransactionID, TransactionTime = F, CreditCardGroup = F, CreditCard = F, VendorID = F, VendorName = F, Status = F, Description = F, Amount = F, FiscalYear = F, ErrorMessage = F, Severity = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, CreditCardTransactionID = F, ErrorCount = F, VendorDescription = F, UserIDCreditCardUsedBy = F, UserCreditCardUsedBy = F, CreditCardID = F, FiscalYearID = F, HasErrors = F, fieldPaths = NULL, entityId = 1, schoolYearId = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment()) %>% purrr::keep(names(.) != "TempCreditCardTransactionID")

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		getSkyObject(module = "AccountsPayable", objectName = "TempCreditCardTransaction", objectId = TempCreditCardTransactionID, searchFields = searchFields, entityId = entityId, flatten = flatten, returnResponse = returnResponse)
	}

	#' Delete a TempCreditCardTransaction
	#'
	#' This function deletes a TempCreditCardTransaction
	#' @param TempCreditCardTransactionID The ID of the TempCreditCardTransaction to delete
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{listSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Accounts Payable
	#' @return The TempCreditCardTransactionID of the deleted TempCreditCardTransaction.
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	deleteTempCreditCardTransaction <- function(TempCreditCardTransactionID, ignoreWarnings = F, entityId = 1, schoolYearId = NULL, flatten = T, returnResponse = F){

		deleteSkyObject(module = "AccountsPayable", objectName = "TempCreditCardTransaction", objectId = TempCreditCardTransactionID, ignoreWarnings = ignoreWarnings, entityId = entityId, flatten = flatten, returnResponse = returnResponse)
	}

	#' Create a TempCreditCardTransaction
	#'
	#' This function creates a TempCreditCardTransaction
	#' @param fieldNames The field values to give the created TempCreditCardTransaction. Each defaults to NULL.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{listSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Accounts Payable
	#' @return A newly created TempCreditCardTransaction
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	createTempCreditCardTransaction <- function(TransactionTime = NULL, CreditCardGroup = NULL, CreditCard = NULL, VendorID = NULL, VendorName = NULL, Status = NULL, Description = NULL, Amount = NULL, FiscalYear = NULL, ErrorMessage = NULL, Severity = NULL, CreditCardTransactionID = NULL, ErrorCount = NULL, VendorDescription = NULL, UserIDCreditCardUsedBy = NULL, UserCreditCardUsedBy = NULL, CreditCardID = NULL, FiscalYearID = NULL, HasErrors = NULL, entityId = 1, schoolYearId = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment())

		body <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper()) %>% compact()

		createSkyObject(module = "AccountsPayable", objectName = "TempCreditCardTransaction", body = list(DataObject = body), searchFields = append("TempCreditCardTransactionID", body %>% names()), entityId = entityId, schoolYearId = schoolYearId, flatten = flatten, returnResponse = returnResponse)
	}

	#' Modify a TempCreditCardTransaction
	#'
	#' This function modifies a TempCreditCardTransaction
	#' @param fieldNames The field values to give the modified TempCreditCardTransaction. Each defaults to NULL.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{listSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Accounts Payable
	#' @return The modified TempCreditCardTransaction
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	modifyTempCreditCardTransaction <- function(TempCreditCardTransactionID, TransactionTime = NULL, CreditCardGroup = NULL, CreditCard = NULL, VendorID = NULL, VendorName = NULL, Status = NULL, Description = NULL, Amount = NULL, FiscalYear = NULL, ErrorMessage = NULL, Severity = NULL, CreditCardTransactionID = NULL, ErrorCount = NULL, VendorDescription = NULL, UserIDCreditCardUsedBy = NULL, UserCreditCardUsedBy = NULL, CreditCardID = NULL, FiscalYearID = NULL, HasErrors = NULL, entityId = 1, schoolYearId = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment())

		body <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper()) %>% compact()

		modifySkyObject(module = "AccountsPayable", objectName = "TempCreditCardTransaction", objectId = TempCreditCardTransactionID, body = list(DataObject = body), searchFields = append("TempCreditCardTransactionID", body %>% names()), entityId = entityId, schoolYearId = schoolYearId, flatten = flatten, returnResponse = returnResponse)
	}

	#' List TempCreditCardTransactionExceptionMessages
	#'
	#' This function returns a dataframe or json object of TempCreditCardTransactionExceptionMessages
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given TempCreditCardTransactionExceptionMessages. Defaults to FALSE for all return fields which, for convenience, returns all fields for the TempCreditCardTransactionExceptionMessages.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object listed as text. Run \code{\link{getSchemaForObjects}}('TempCreditCardTransactionExceptionMessage') to get more field paths.
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
	#' @concept Accounts Payable
	#' @return A list of TempCreditCardTransactionExceptionMessages
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	listTempCreditCardTransactionExceptionMessages <- function(searchConditionsList = NULL, TempCreditCardTransactionExceptionMessageID = F, TempCreditCardTransactionID = F, Error = F, ErrorDetail = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, fieldPaths = NULL, searchConditionsGroupType = "And", searchSortFieldNamesList = NULL, searchSortFieldNamesDescendingList = NULL, entityId = 1, schoolYearId = NULL, page = 1, pageSize = 100000, flatten = T, returnResponse = F){

		params <- as.list(environment())

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		listSkyObjects(module = "AccountsPayable", objectName = "TempCreditCardTransactionExceptionMessage", searchFields = searchFields %>% append(fieldPaths), page = page, pageSize = pageSize, SearchConditionsList = searchConditionsList, SearchConditionsGroupType = searchConditionsGroupType, SearchSortFieldNamesList = searchSortFieldNamesList, SearchSortFieldNamesDescendingList = searchSortFieldNamesDescendingList, entityId = entityId, schoolYearId = schoolYearId, flatten = flatten, returnResponse = returnResponse)
	}

	#' Get a TempCreditCardTransactionExceptionMessage
	#'
	#' This function returns a dataframe or json object of a TempCreditCardTransactionExceptionMessage
	#' @param TempCreditCardTransactionExceptionMessageID The ID of the TempCreditCardTransactionExceptionMessage to return.
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given TempCreditCardTransactionExceptionMessage. Defaults to FALSE for all return fields which, for convenience, returns all fields for the TempCreditCardTransactionExceptionMessage.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object listed as text. Run \code{\link{getSchemaForObjects}}('TempCreditCardTransactionExceptionMessage') to get more field paths.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{listSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Accounts Payable
	#' @return A dataframe or of TempCreditCardTransactionExceptionMessage
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	getTempCreditCardTransactionExceptionMessage <- function(TempCreditCardTransactionExceptionMessageID, TempCreditCardTransactionID = F, Error = F, ErrorDetail = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, fieldPaths = NULL, entityId = 1, schoolYearId = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment()) %>% purrr::keep(names(.) != "TempCreditCardTransactionExceptionMessageID")

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		getSkyObject(module = "AccountsPayable", objectName = "TempCreditCardTransactionExceptionMessage", objectId = TempCreditCardTransactionExceptionMessageID, searchFields = searchFields, entityId = entityId, flatten = flatten, returnResponse = returnResponse)
	}

	#' Delete a TempCreditCardTransactionExceptionMessage
	#'
	#' This function deletes a TempCreditCardTransactionExceptionMessage
	#' @param TempCreditCardTransactionExceptionMessageID The ID of the TempCreditCardTransactionExceptionMessage to delete
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{listSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Accounts Payable
	#' @return The TempCreditCardTransactionExceptionMessageID of the deleted TempCreditCardTransactionExceptionMessage.
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	deleteTempCreditCardTransactionExceptionMessage <- function(TempCreditCardTransactionExceptionMessageID, ignoreWarnings = F, entityId = 1, schoolYearId = NULL, flatten = T, returnResponse = F){

		deleteSkyObject(module = "AccountsPayable", objectName = "TempCreditCardTransactionExceptionMessage", objectId = TempCreditCardTransactionExceptionMessageID, ignoreWarnings = ignoreWarnings, entityId = entityId, flatten = flatten, returnResponse = returnResponse)
	}

	#' Create a TempCreditCardTransactionExceptionMessage
	#'
	#' This function creates a TempCreditCardTransactionExceptionMessage
	#' @param fieldNames The field values to give the created TempCreditCardTransactionExceptionMessage. Each defaults to NULL.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{listSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Accounts Payable
	#' @return A newly created TempCreditCardTransactionExceptionMessage
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	createTempCreditCardTransactionExceptionMessage <- function(TempCreditCardTransactionID = NULL, Error = NULL, ErrorDetail = NULL, entityId = 1, schoolYearId = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment())

		body <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper()) %>% compact()

		createSkyObject(module = "AccountsPayable", objectName = "TempCreditCardTransactionExceptionMessage", body = list(DataObject = body), searchFields = append("TempCreditCardTransactionExceptionMessageID", body %>% names()), entityId = entityId, schoolYearId = schoolYearId, flatten = flatten, returnResponse = returnResponse)
	}

	#' Modify a TempCreditCardTransactionExceptionMessage
	#'
	#' This function modifies a TempCreditCardTransactionExceptionMessage
	#' @param fieldNames The field values to give the modified TempCreditCardTransactionExceptionMessage. Each defaults to NULL.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{listSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Accounts Payable
	#' @return The modified TempCreditCardTransactionExceptionMessage
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	modifyTempCreditCardTransactionExceptionMessage <- function(TempCreditCardTransactionExceptionMessageID, TempCreditCardTransactionID = NULL, Error = NULL, ErrorDetail = NULL, entityId = 1, schoolYearId = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment())

		body <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper()) %>% compact()

		modifySkyObject(module = "AccountsPayable", objectName = "TempCreditCardTransactionExceptionMessage", objectId = TempCreditCardTransactionExceptionMessageID, body = list(DataObject = body), searchFields = append("TempCreditCardTransactionExceptionMessageID", body %>% names()), entityId = entityId, schoolYearId = schoolYearId, flatten = flatten, returnResponse = returnResponse)
	}

	#' List TempInvoiceErrors
	#'
	#' This function returns a dataframe or json object of TempInvoiceErrors
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given TempInvoiceErrors. Defaults to FALSE for all return fields which, for convenience, returns all fields for the TempInvoiceErrors.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object listed as text. Run \code{\link{getSchemaForObjects}}('TempInvoiceError') to get more field paths.
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
	#' @concept Accounts Payable
	#' @return A list of TempInvoiceErrors
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	listTempInvoiceErrors <- function(searchConditionsList = NULL, TempInvoiceErrorID = F, TempInvoiceID = F, Error = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, fieldPaths = NULL, searchConditionsGroupType = "And", searchSortFieldNamesList = NULL, searchSortFieldNamesDescendingList = NULL, entityId = 1, schoolYearId = NULL, page = 1, pageSize = 100000, flatten = T, returnResponse = F){

		params <- as.list(environment())

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		listSkyObjects(module = "AccountsPayable", objectName = "TempInvoiceError", searchFields = searchFields %>% append(fieldPaths), page = page, pageSize = pageSize, SearchConditionsList = searchConditionsList, SearchConditionsGroupType = searchConditionsGroupType, SearchSortFieldNamesList = searchSortFieldNamesList, SearchSortFieldNamesDescendingList = searchSortFieldNamesDescendingList, entityId = entityId, schoolYearId = schoolYearId, flatten = flatten, returnResponse = returnResponse)
	}

	#' Get a TempInvoiceError
	#'
	#' This function returns a dataframe or json object of a TempInvoiceError
	#' @param TempInvoiceErrorID The ID of the TempInvoiceError to return.
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given TempInvoiceError. Defaults to FALSE for all return fields which, for convenience, returns all fields for the TempInvoiceError.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object listed as text. Run \code{\link{getSchemaForObjects}}('TempInvoiceError') to get more field paths.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{listSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Accounts Payable
	#' @return A dataframe or of TempInvoiceError
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	getTempInvoiceError <- function(TempInvoiceErrorID, TempInvoiceID = F, Error = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, fieldPaths = NULL, entityId = 1, schoolYearId = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment()) %>% purrr::keep(names(.) != "TempInvoiceErrorID")

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		getSkyObject(module = "AccountsPayable", objectName = "TempInvoiceError", objectId = TempInvoiceErrorID, searchFields = searchFields, entityId = entityId, flatten = flatten, returnResponse = returnResponse)
	}

	#' Delete a TempInvoiceError
	#'
	#' This function deletes a TempInvoiceError
	#' @param TempInvoiceErrorID The ID of the TempInvoiceError to delete
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{listSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Accounts Payable
	#' @return The TempInvoiceErrorID of the deleted TempInvoiceError.
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	deleteTempInvoiceError <- function(TempInvoiceErrorID, ignoreWarnings = F, entityId = 1, schoolYearId = NULL, flatten = T, returnResponse = F){

		deleteSkyObject(module = "AccountsPayable", objectName = "TempInvoiceError", objectId = TempInvoiceErrorID, ignoreWarnings = ignoreWarnings, entityId = entityId, flatten = flatten, returnResponse = returnResponse)
	}

	#' Create a TempInvoiceError
	#'
	#' This function creates a TempInvoiceError
	#' @param fieldNames The field values to give the created TempInvoiceError. Each defaults to NULL.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{listSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Accounts Payable
	#' @return A newly created TempInvoiceError
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	createTempInvoiceError <- function(TempInvoiceID = NULL, Error = NULL, entityId = 1, schoolYearId = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment())

		body <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper()) %>% compact()

		createSkyObject(module = "AccountsPayable", objectName = "TempInvoiceError", body = list(DataObject = body), searchFields = append("TempInvoiceErrorID", body %>% names()), entityId = entityId, schoolYearId = schoolYearId, flatten = flatten, returnResponse = returnResponse)
	}

	#' Modify a TempInvoiceError
	#'
	#' This function modifies a TempInvoiceError
	#' @param fieldNames The field values to give the modified TempInvoiceError. Each defaults to NULL.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{listSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Accounts Payable
	#' @return The modified TempInvoiceError
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	modifyTempInvoiceError <- function(TempInvoiceErrorID, TempInvoiceID = NULL, Error = NULL, entityId = 1, schoolYearId = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment())

		body <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper()) %>% compact()

		modifySkyObject(module = "AccountsPayable", objectName = "TempInvoiceError", objectId = TempInvoiceErrorID, body = list(DataObject = body), searchFields = append("TempInvoiceErrorID", body %>% names()), entityId = entityId, schoolYearId = schoolYearId, flatten = flatten, returnResponse = returnResponse)
	}

	#' List TempCreditCardTransactionDetailAccountings
	#'
	#' This function returns a dataframe or json object of TempCreditCardTransactionDetailAccountings
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given TempCreditCardTransactionDetailAccountings. Defaults to FALSE for all return fields which, for convenience, returns all fields for the TempCreditCardTransactionDetailAccountings.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object listed as text. Run \code{\link{getSchemaForObjects}}('TempCreditCardTransactionDetailAccounting') to get more field paths.
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
	#' @concept Accounts Payable
	#' @return A list of TempCreditCardTransactionDetailAccountings
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	listTempCreditCardTransactionDetailAccountings <- function(searchConditionsList = NULL, TempCreditCardTransactionDetailAccountingID = F, Error = F, DisplayAccount = F, CreditCardTransaction = F, CreditCardTransactionDetail = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, Severity = F, fieldPaths = NULL, searchConditionsGroupType = "And", searchSortFieldNamesList = NULL, searchSortFieldNamesDescendingList = NULL, entityId = 1, schoolYearId = NULL, page = 1, pageSize = 100000, flatten = T, returnResponse = F){

		params <- as.list(environment())

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		listSkyObjects(module = "AccountsPayable", objectName = "TempCreditCardTransactionDetailAccounting", searchFields = searchFields %>% append(fieldPaths), page = page, pageSize = pageSize, SearchConditionsList = searchConditionsList, SearchConditionsGroupType = searchConditionsGroupType, SearchSortFieldNamesList = searchSortFieldNamesList, SearchSortFieldNamesDescendingList = searchSortFieldNamesDescendingList, entityId = entityId, schoolYearId = schoolYearId, flatten = flatten, returnResponse = returnResponse)
	}

	#' Get a TempCreditCardTransactionDetailAccounting
	#'
	#' This function returns a dataframe or json object of a TempCreditCardTransactionDetailAccounting
	#' @param TempCreditCardTransactionDetailAccountingID The ID of the TempCreditCardTransactionDetailAccounting to return.
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given TempCreditCardTransactionDetailAccounting. Defaults to FALSE for all return fields which, for convenience, returns all fields for the TempCreditCardTransactionDetailAccounting.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object listed as text. Run \code{\link{getSchemaForObjects}}('TempCreditCardTransactionDetailAccounting') to get more field paths.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{listSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Accounts Payable
	#' @return A dataframe or of TempCreditCardTransactionDetailAccounting
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	getTempCreditCardTransactionDetailAccounting <- function(TempCreditCardTransactionDetailAccountingID, Error = F, DisplayAccount = F, CreditCardTransaction = F, CreditCardTransactionDetail = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, Severity = F, fieldPaths = NULL, entityId = 1, schoolYearId = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment()) %>% purrr::keep(names(.) != "TempCreditCardTransactionDetailAccountingID")

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		getSkyObject(module = "AccountsPayable", objectName = "TempCreditCardTransactionDetailAccounting", objectId = TempCreditCardTransactionDetailAccountingID, searchFields = searchFields, entityId = entityId, flatten = flatten, returnResponse = returnResponse)
	}

	#' Delete a TempCreditCardTransactionDetailAccounting
	#'
	#' This function deletes a TempCreditCardTransactionDetailAccounting
	#' @param TempCreditCardTransactionDetailAccountingID The ID of the TempCreditCardTransactionDetailAccounting to delete
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{listSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Accounts Payable
	#' @return The TempCreditCardTransactionDetailAccountingID of the deleted TempCreditCardTransactionDetailAccounting.
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	deleteTempCreditCardTransactionDetailAccounting <- function(TempCreditCardTransactionDetailAccountingID, ignoreWarnings = F, entityId = 1, schoolYearId = NULL, flatten = T, returnResponse = F){

		deleteSkyObject(module = "AccountsPayable", objectName = "TempCreditCardTransactionDetailAccounting", objectId = TempCreditCardTransactionDetailAccountingID, ignoreWarnings = ignoreWarnings, entityId = entityId, flatten = flatten, returnResponse = returnResponse)
	}

	#' Create a TempCreditCardTransactionDetailAccounting
	#'
	#' This function creates a TempCreditCardTransactionDetailAccounting
	#' @param fieldNames The field values to give the created TempCreditCardTransactionDetailAccounting. Each defaults to NULL.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{listSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Accounts Payable
	#' @return A newly created TempCreditCardTransactionDetailAccounting
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	createTempCreditCardTransactionDetailAccounting <- function(Error = NULL, DisplayAccount = NULL, CreditCardTransaction = NULL, CreditCardTransactionDetail = NULL, Severity = NULL, entityId = 1, schoolYearId = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment())

		body <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper()) %>% compact()

		createSkyObject(module = "AccountsPayable", objectName = "TempCreditCardTransactionDetailAccounting", body = list(DataObject = body), searchFields = append("TempCreditCardTransactionDetailAccountingID", body %>% names()), entityId = entityId, schoolYearId = schoolYearId, flatten = flatten, returnResponse = returnResponse)
	}

	#' Modify a TempCreditCardTransactionDetailAccounting
	#'
	#' This function modifies a TempCreditCardTransactionDetailAccounting
	#' @param fieldNames The field values to give the modified TempCreditCardTransactionDetailAccounting. Each defaults to NULL.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{listSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Accounts Payable
	#' @return The modified TempCreditCardTransactionDetailAccounting
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	modifyTempCreditCardTransactionDetailAccounting <- function(TempCreditCardTransactionDetailAccountingID, Error = NULL, DisplayAccount = NULL, CreditCardTransaction = NULL, CreditCardTransactionDetail = NULL, Severity = NULL, entityId = 1, schoolYearId = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment())

		body <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper()) %>% compact()

		modifySkyObject(module = "AccountsPayable", objectName = "TempCreditCardTransactionDetailAccounting", objectId = TempCreditCardTransactionDetailAccountingID, body = list(DataObject = body), searchFields = append("TempCreditCardTransactionDetailAccountingID", body %>% names()), entityId = entityId, schoolYearId = schoolYearId, flatten = flatten, returnResponse = returnResponse)
	}

	#' List TempExpenseReimbursementAccountings
	#'
	#' This function returns a dataframe or json object of TempExpenseReimbursementAccountings
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given TempExpenseReimbursementAccountings. Defaults to FALSE for all return fields which, for convenience, returns all fields for the TempExpenseReimbursementAccountings.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object listed as text. Run \code{\link{getSchemaForObjects}}('TempExpenseReimbursementAccounting') to get more field paths.
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
	#' @concept Accounts Payable
	#' @return A list of TempExpenseReimbursementAccountings
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	listTempExpenseReimbursementAccountings <- function(searchConditionsList = NULL, TempExpenseReimbursementAccountingID = F, Error = F, DisplayAccount = F, ExpenseReimbursement = F, ExpenseReimbursementDetail = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, fieldPaths = NULL, searchConditionsGroupType = "And", searchSortFieldNamesList = NULL, searchSortFieldNamesDescendingList = NULL, entityId = 1, schoolYearId = NULL, page = 1, pageSize = 100000, flatten = T, returnResponse = F){

		params <- as.list(environment())

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		listSkyObjects(module = "AccountsPayable", objectName = "TempExpenseReimbursementAccounting", searchFields = searchFields %>% append(fieldPaths), page = page, pageSize = pageSize, SearchConditionsList = searchConditionsList, SearchConditionsGroupType = searchConditionsGroupType, SearchSortFieldNamesList = searchSortFieldNamesList, SearchSortFieldNamesDescendingList = searchSortFieldNamesDescendingList, entityId = entityId, schoolYearId = schoolYearId, flatten = flatten, returnResponse = returnResponse)
	}

	#' Get a TempExpenseReimbursementAccounting
	#'
	#' This function returns a dataframe or json object of a TempExpenseReimbursementAccounting
	#' @param TempExpenseReimbursementAccountingID The ID of the TempExpenseReimbursementAccounting to return.
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given TempExpenseReimbursementAccounting. Defaults to FALSE for all return fields which, for convenience, returns all fields for the TempExpenseReimbursementAccounting.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object listed as text. Run \code{\link{getSchemaForObjects}}('TempExpenseReimbursementAccounting') to get more field paths.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{listSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Accounts Payable
	#' @return A dataframe or of TempExpenseReimbursementAccounting
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	getTempExpenseReimbursementAccounting <- function(TempExpenseReimbursementAccountingID, Error = F, DisplayAccount = F, ExpenseReimbursement = F, ExpenseReimbursementDetail = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, fieldPaths = NULL, entityId = 1, schoolYearId = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment()) %>% purrr::keep(names(.) != "TempExpenseReimbursementAccountingID")

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		getSkyObject(module = "AccountsPayable", objectName = "TempExpenseReimbursementAccounting", objectId = TempExpenseReimbursementAccountingID, searchFields = searchFields, entityId = entityId, flatten = flatten, returnResponse = returnResponse)
	}

	#' Delete a TempExpenseReimbursementAccounting
	#'
	#' This function deletes a TempExpenseReimbursementAccounting
	#' @param TempExpenseReimbursementAccountingID The ID of the TempExpenseReimbursementAccounting to delete
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{listSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Accounts Payable
	#' @return The TempExpenseReimbursementAccountingID of the deleted TempExpenseReimbursementAccounting.
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	deleteTempExpenseReimbursementAccounting <- function(TempExpenseReimbursementAccountingID, ignoreWarnings = F, entityId = 1, schoolYearId = NULL, flatten = T, returnResponse = F){

		deleteSkyObject(module = "AccountsPayable", objectName = "TempExpenseReimbursementAccounting", objectId = TempExpenseReimbursementAccountingID, ignoreWarnings = ignoreWarnings, entityId = entityId, flatten = flatten, returnResponse = returnResponse)
	}

	#' Create a TempExpenseReimbursementAccounting
	#'
	#' This function creates a TempExpenseReimbursementAccounting
	#' @param fieldNames The field values to give the created TempExpenseReimbursementAccounting. Each defaults to NULL.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{listSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Accounts Payable
	#' @return A newly created TempExpenseReimbursementAccounting
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	createTempExpenseReimbursementAccounting <- function(Error = NULL, DisplayAccount = NULL, ExpenseReimbursement = NULL, ExpenseReimbursementDetail = NULL, entityId = 1, schoolYearId = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment())

		body <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper()) %>% compact()

		createSkyObject(module = "AccountsPayable", objectName = "TempExpenseReimbursementAccounting", body = list(DataObject = body), searchFields = append("TempExpenseReimbursementAccountingID", body %>% names()), entityId = entityId, schoolYearId = schoolYearId, flatten = flatten, returnResponse = returnResponse)
	}

	#' Modify a TempExpenseReimbursementAccounting
	#'
	#' This function modifies a TempExpenseReimbursementAccounting
	#' @param fieldNames The field values to give the modified TempExpenseReimbursementAccounting. Each defaults to NULL.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{listSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Accounts Payable
	#' @return The modified TempExpenseReimbursementAccounting
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	modifyTempExpenseReimbursementAccounting <- function(TempExpenseReimbursementAccountingID, Error = NULL, DisplayAccount = NULL, ExpenseReimbursement = NULL, ExpenseReimbursementDetail = NULL, entityId = 1, schoolYearId = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment())

		body <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper()) %>% compact()

		modifySkyObject(module = "AccountsPayable", objectName = "TempExpenseReimbursementAccounting", objectId = TempExpenseReimbursementAccountingID, body = list(DataObject = body), searchFields = append("TempExpenseReimbursementAccountingID", body %>% names()), entityId = entityId, schoolYearId = schoolYearId, flatten = flatten, returnResponse = returnResponse)
	}

	#' List AccountsPayableTempInvoiceDetails
	#'
	#' This function returns a dataframe or json object of AccountsPayableTempInvoiceDetails
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given AccountsPayableTempInvoiceDetails. Defaults to FALSE for all return fields which, for convenience, returns all fields for the AccountsPayableTempInvoiceDetails.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object listed as text. Run \code{\link{getSchemaForObjects}}('AccountsPayableTempInvoiceDetail') to get more field paths.
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
	#' @concept Accounts Payable
	#' @return A list of AccountsPayableTempInvoiceDetails
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	listAccountsPayableTempInvoiceDetails <- function(searchConditionsList = NULL, TempInvoiceDetailID = F, TempInvoiceID = F, Quantity = F, UnitCost = F, BaseCurrencyAmount = F, EntryAmount = F, Description = F, DisplayOrder = F, FirstAccountDistributionAccountID = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, fieldPaths = NULL, searchConditionsGroupType = "And", searchSortFieldNamesList = NULL, searchSortFieldNamesDescendingList = NULL, entityId = 1, schoolYearId = NULL, page = 1, pageSize = 100000, flatten = T, returnResponse = F){

		params <- as.list(environment())

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		listSkyObjects(module = "AccountsPayable", objectName = "TempInvoiceDetail", searchFields = searchFields %>% append(fieldPaths), page = page, pageSize = pageSize, SearchConditionsList = searchConditionsList, SearchConditionsGroupType = searchConditionsGroupType, SearchSortFieldNamesList = searchSortFieldNamesList, SearchSortFieldNamesDescendingList = searchSortFieldNamesDescendingList, entityId = entityId, schoolYearId = schoolYearId, flatten = flatten, returnResponse = returnResponse)
	}

	#' Get an AccountsPayableTempInvoiceDetail
	#'
	#' This function returns a dataframe or json object of an AccountsPayableTempInvoiceDetail
	#' @param AccountsPayableTempInvoiceDetailID The ID of the AccountsPayableTempInvoiceDetail to return.
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given AccountsPayableTempInvoiceDetail. Defaults to FALSE for all return fields which, for convenience, returns all fields for the AccountsPayableTempInvoiceDetail.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object listed as text. Run \code{\link{getSchemaForObjects}}('AccountsPayableTempInvoiceDetail') to get more field paths.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{listSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Accounts Payable
	#' @return A dataframe or of AccountsPayableTempInvoiceDetail
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	getAccountsPayableTempInvoiceDetail <- function(AccountsPayableTempInvoiceDetailID, TempInvoiceDetailID = F, TempInvoiceID = F, Quantity = F, UnitCost = F, BaseCurrencyAmount = F, EntryAmount = F, Description = F, DisplayOrder = F, FirstAccountDistributionAccountID = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, fieldPaths = NULL, entityId = 1, schoolYearId = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment()) %>% purrr::keep(names(.) != "AccountsPayableTempInvoiceDetailID")

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		getSkyObject(module = "AccountsPayable", objectName = "TempInvoiceDetail", objectId = AccountsPayableTempInvoiceDetailID, searchFields = searchFields, entityId = entityId, flatten = flatten, returnResponse = returnResponse)
	}

	#' Delete an AccountsPayableTempInvoiceDetail
	#'
	#' This function deletes an AccountsPayableTempInvoiceDetail
	#' @param AccountsPayableTempInvoiceDetailID The ID of the AccountsPayableTempInvoiceDetail to delete
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{listSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Accounts Payable
	#' @return The AccountsPayableTempInvoiceDetailID of the deleted AccountsPayableTempInvoiceDetail.
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	deleteAccountsPayableTempInvoiceDetail <- function(AccountsPayableTempInvoiceDetailID, ignoreWarnings = F, entityId = 1, schoolYearId = NULL, flatten = T, returnResponse = F){

		deleteSkyObject(module = "AccountsPayable", objectName = "TempInvoiceDetail", objectId = AccountsPayableTempInvoiceDetailID, ignoreWarnings = ignoreWarnings, entityId = entityId, flatten = flatten, returnResponse = returnResponse)
	}

	#' Create an AccountsPayableTempInvoiceDetail
	#'
	#' This function creates an AccountsPayableTempInvoiceDetail
	#' @param fieldNames The field values to give the created AccountsPayableTempInvoiceDetail. Each defaults to NULL.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{listSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Accounts Payable
	#' @return A newly created AccountsPayableTempInvoiceDetail
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	createAccountsPayableTempInvoiceDetail <- function(TempInvoiceID = NULL, Quantity = NULL, UnitCost = NULL, BaseCurrencyAmount = NULL, EntryAmount = NULL, Description = NULL, DisplayOrder = NULL, FirstAccountDistributionAccountID = NULL, entityId = 1, schoolYearId = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment())

		body <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper()) %>% compact()

		createSkyObject(module = "AccountsPayable", objectName = "TempInvoiceDetail", body = list(DataObject = body), searchFields = append("TempInvoiceDetailID", body %>% names()), entityId = entityId, schoolYearId = schoolYearId, flatten = flatten, returnResponse = returnResponse)
	}

	#' Modify an AccountsPayableTempInvoiceDetail
	#'
	#' This function modifies an AccountsPayableTempInvoiceDetail
	#' @param fieldNames The field values to give the modified AccountsPayableTempInvoiceDetail. Each defaults to NULL.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{listSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Accounts Payable
	#' @return The modified AccountsPayableTempInvoiceDetail
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	modifyAccountsPayableTempInvoiceDetail <- function(TempInvoiceDetailID, TempInvoiceID = NULL, Quantity = NULL, UnitCost = NULL, BaseCurrencyAmount = NULL, EntryAmount = NULL, Description = NULL, DisplayOrder = NULL, FirstAccountDistributionAccountID = NULL, entityId = 1, schoolYearId = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment())

		body <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper()) %>% compact()

		modifySkyObject(module = "AccountsPayable", objectName = "TempInvoiceDetail", objectId = TempInvoiceDetailID, body = list(DataObject = body), searchFields = append("TempInvoiceDetailID", body %>% names()), entityId = entityId, schoolYearId = schoolYearId, flatten = flatten, returnResponse = returnResponse)
	}

	#' List CreditCardTransactionDelimitedFileFormats
	#'
	#' This function returns a dataframe or json object of CreditCardTransactionDelimitedFileFormats
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given CreditCardTransactionDelimitedFileFormats. Defaults to FALSE for all return fields which, for convenience, returns all fields for the CreditCardTransactionDelimitedFileFormats.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object listed as text. Run \code{\link{getSchemaForObjects}}('CreditCardTransactionDelimitedFileFormat') to get more field paths.
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
	#' @concept Accounts Payable
	#' @return A list of CreditCardTransactionDelimitedFileFormats
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	listCreditCardTransactionDelimitedFileFormats <- function(searchConditionsList = NULL, CreditCardTransactionDelimitedFileFormatID = F, SkywardID = F, SkywardHash = F, CreditCardTransactionThirdPartyFormatID = F, NumberOfHeaderRows = F, DelimiterType = F, DelimiterCharacter = F, TransactionImportIDColumnNumber = F, SequenceNumberColumnNumber = F, UniqueCardIdentifierColumnNumber = F, TransactionDateColumnNumber = F, TransactionAmountColumnNumber = F, SplitAmountColumnNumber = F, GeneralDefaultCodeOneColumnNumber = F, GeneralDefaultCodeTwoColumnNumber = F, GeneralDefaultCodeThreeColumnNumber = F, GeneralDefaultCodeFourColumnNumber = F, GeneralDefaultCodeFiveColumnNumber = F, GeneralDefaultCodeSixColumnNumber = F, GeneralDefaultCodeSevenColumnNumber = F, GeneralDefaultCodeEightColumnNumber = F, GeneralDefaultCodeNineColumnNumber = F, GeneralDefaultCodeTenColumnNumber = F, GeneralDefaultCodeElevenColumnNumber = F, GeneralDefaultCodeTwelveColumnNumber = F, VendorNameColumnNumber = F, VendorCityColumnNumber = F, VendorStateProvinceColumnNumber = F, VendorZipCodeColumnNumber = F, VendorCountryColumnNumber = F, TransactionDescriptionColumnNumber = F, SplitDescriptionColumnNumber = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, UniqueAccountIdentifierColumnNumber = F, fieldPaths = NULL, searchConditionsGroupType = "And", searchSortFieldNamesList = NULL, searchSortFieldNamesDescendingList = NULL, entityId = 1, schoolYearId = NULL, page = 1, pageSize = 100000, flatten = T, returnResponse = F){

		params <- as.list(environment())

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		listSkyObjects(module = "AccountsPayable", objectName = "CreditCardTransactionDelimitedFileFormat", searchFields = searchFields %>% append(fieldPaths), page = page, pageSize = pageSize, SearchConditionsList = searchConditionsList, SearchConditionsGroupType = searchConditionsGroupType, SearchSortFieldNamesList = searchSortFieldNamesList, SearchSortFieldNamesDescendingList = searchSortFieldNamesDescendingList, entityId = entityId, schoolYearId = schoolYearId, flatten = flatten, returnResponse = returnResponse)
	}

	#' Get a CreditCardTransactionDelimitedFileFormat
	#'
	#' This function returns a dataframe or json object of a CreditCardTransactionDelimitedFileFormat
	#' @param CreditCardTransactionDelimitedFileFormatID The ID of the CreditCardTransactionDelimitedFileFormat to return.
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given CreditCardTransactionDelimitedFileFormat. Defaults to FALSE for all return fields which, for convenience, returns all fields for the CreditCardTransactionDelimitedFileFormat.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object listed as text. Run \code{\link{getSchemaForObjects}}('CreditCardTransactionDelimitedFileFormat') to get more field paths.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{listSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Accounts Payable
	#' @return A dataframe or of CreditCardTransactionDelimitedFileFormat
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	getCreditCardTransactionDelimitedFileFormat <- function(CreditCardTransactionDelimitedFileFormatID, SkywardID = F, SkywardHash = F, CreditCardTransactionThirdPartyFormatID = F, NumberOfHeaderRows = F, DelimiterType = F, DelimiterCharacter = F, TransactionImportIDColumnNumber = F, SequenceNumberColumnNumber = F, UniqueCardIdentifierColumnNumber = F, TransactionDateColumnNumber = F, TransactionAmountColumnNumber = F, SplitAmountColumnNumber = F, GeneralDefaultCodeOneColumnNumber = F, GeneralDefaultCodeTwoColumnNumber = F, GeneralDefaultCodeThreeColumnNumber = F, GeneralDefaultCodeFourColumnNumber = F, GeneralDefaultCodeFiveColumnNumber = F, GeneralDefaultCodeSixColumnNumber = F, GeneralDefaultCodeSevenColumnNumber = F, GeneralDefaultCodeEightColumnNumber = F, GeneralDefaultCodeNineColumnNumber = F, GeneralDefaultCodeTenColumnNumber = F, GeneralDefaultCodeElevenColumnNumber = F, GeneralDefaultCodeTwelveColumnNumber = F, VendorNameColumnNumber = F, VendorCityColumnNumber = F, VendorStateProvinceColumnNumber = F, VendorZipCodeColumnNumber = F, VendorCountryColumnNumber = F, TransactionDescriptionColumnNumber = F, SplitDescriptionColumnNumber = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, UniqueAccountIdentifierColumnNumber = F, fieldPaths = NULL, entityId = 1, schoolYearId = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment()) %>% purrr::keep(names(.) != "CreditCardTransactionDelimitedFileFormatID")

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		getSkyObject(module = "AccountsPayable", objectName = "CreditCardTransactionDelimitedFileFormat", objectId = CreditCardTransactionDelimitedFileFormatID, searchFields = searchFields, entityId = entityId, flatten = flatten, returnResponse = returnResponse)
	}

	#' Delete a CreditCardTransactionDelimitedFileFormat
	#'
	#' This function deletes a CreditCardTransactionDelimitedFileFormat
	#' @param CreditCardTransactionDelimitedFileFormatID The ID of the CreditCardTransactionDelimitedFileFormat to delete
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{listSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Accounts Payable
	#' @return The CreditCardTransactionDelimitedFileFormatID of the deleted CreditCardTransactionDelimitedFileFormat.
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	deleteCreditCardTransactionDelimitedFileFormat <- function(CreditCardTransactionDelimitedFileFormatID, ignoreWarnings = F, entityId = 1, schoolYearId = NULL, flatten = T, returnResponse = F){

		deleteSkyObject(module = "AccountsPayable", objectName = "CreditCardTransactionDelimitedFileFormat", objectId = CreditCardTransactionDelimitedFileFormatID, ignoreWarnings = ignoreWarnings, entityId = entityId, flatten = flatten, returnResponse = returnResponse)
	}

	#' Create a CreditCardTransactionDelimitedFileFormat
	#'
	#' This function creates a CreditCardTransactionDelimitedFileFormat
	#' @param fieldNames The field values to give the created CreditCardTransactionDelimitedFileFormat. Each defaults to NULL.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{listSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Accounts Payable
	#' @return A newly created CreditCardTransactionDelimitedFileFormat
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	createCreditCardTransactionDelimitedFileFormat <- function(CreditCardTransactionThirdPartyFormatID = NULL, NumberOfHeaderRows = NULL, DelimiterType = NULL, DelimiterCharacter = NULL, TransactionImportIDColumnNumber = NULL, SequenceNumberColumnNumber = NULL, UniqueCardIdentifierColumnNumber = NULL, TransactionDateColumnNumber = NULL, TransactionAmountColumnNumber = NULL, SplitAmountColumnNumber = NULL, GeneralDefaultCodeOneColumnNumber = NULL, GeneralDefaultCodeTwoColumnNumber = NULL, GeneralDefaultCodeThreeColumnNumber = NULL, GeneralDefaultCodeFourColumnNumber = NULL, GeneralDefaultCodeFiveColumnNumber = NULL, GeneralDefaultCodeSixColumnNumber = NULL, GeneralDefaultCodeSevenColumnNumber = NULL, GeneralDefaultCodeEightColumnNumber = NULL, GeneralDefaultCodeNineColumnNumber = NULL, GeneralDefaultCodeTenColumnNumber = NULL, GeneralDefaultCodeElevenColumnNumber = NULL, GeneralDefaultCodeTwelveColumnNumber = NULL, VendorNameColumnNumber = NULL, VendorCityColumnNumber = NULL, VendorStateProvinceColumnNumber = NULL, VendorZipCodeColumnNumber = NULL, VendorCountryColumnNumber = NULL, TransactionDescriptionColumnNumber = NULL, SplitDescriptionColumnNumber = NULL, UniqueAccountIdentifierColumnNumber = NULL, entityId = 1, schoolYearId = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment())

		body <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper()) %>% compact()

		createSkyObject(module = "AccountsPayable", objectName = "CreditCardTransactionDelimitedFileFormat", body = list(DataObject = body), searchFields = append("CreditCardTransactionDelimitedFileFormatID", body %>% names()), entityId = entityId, schoolYearId = schoolYearId, flatten = flatten, returnResponse = returnResponse)
	}

	#' Modify a CreditCardTransactionDelimitedFileFormat
	#'
	#' This function modifies a CreditCardTransactionDelimitedFileFormat
	#' @param fieldNames The field values to give the modified CreditCardTransactionDelimitedFileFormat. Each defaults to NULL.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{listSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Accounts Payable
	#' @return The modified CreditCardTransactionDelimitedFileFormat
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	modifyCreditCardTransactionDelimitedFileFormat <- function(CreditCardTransactionDelimitedFileFormatID, CreditCardTransactionThirdPartyFormatID = NULL, NumberOfHeaderRows = NULL, DelimiterType = NULL, DelimiterCharacter = NULL, TransactionImportIDColumnNumber = NULL, SequenceNumberColumnNumber = NULL, UniqueCardIdentifierColumnNumber = NULL, TransactionDateColumnNumber = NULL, TransactionAmountColumnNumber = NULL, SplitAmountColumnNumber = NULL, GeneralDefaultCodeOneColumnNumber = NULL, GeneralDefaultCodeTwoColumnNumber = NULL, GeneralDefaultCodeThreeColumnNumber = NULL, GeneralDefaultCodeFourColumnNumber = NULL, GeneralDefaultCodeFiveColumnNumber = NULL, GeneralDefaultCodeSixColumnNumber = NULL, GeneralDefaultCodeSevenColumnNumber = NULL, GeneralDefaultCodeEightColumnNumber = NULL, GeneralDefaultCodeNineColumnNumber = NULL, GeneralDefaultCodeTenColumnNumber = NULL, GeneralDefaultCodeElevenColumnNumber = NULL, GeneralDefaultCodeTwelveColumnNumber = NULL, VendorNameColumnNumber = NULL, VendorCityColumnNumber = NULL, VendorStateProvinceColumnNumber = NULL, VendorZipCodeColumnNumber = NULL, VendorCountryColumnNumber = NULL, TransactionDescriptionColumnNumber = NULL, SplitDescriptionColumnNumber = NULL, UniqueAccountIdentifierColumnNumber = NULL, entityId = 1, schoolYearId = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment())

		body <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper()) %>% compact()

		modifySkyObject(module = "AccountsPayable", objectName = "CreditCardTransactionDelimitedFileFormat", objectId = CreditCardTransactionDelimitedFileFormatID, body = list(DataObject = body), searchFields = append("CreditCardTransactionDelimitedFileFormatID", body %>% names()), entityId = entityId, schoolYearId = schoolYearId, flatten = flatten, returnResponse = returnResponse)
	}

	#' List CreditCardTransactionFixedLengthFileFormats
	#'
	#' This function returns a dataframe or json object of CreditCardTransactionFixedLengthFileFormats
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given CreditCardTransactionFixedLengthFileFormats. Defaults to FALSE for all return fields which, for convenience, returns all fields for the CreditCardTransactionFixedLengthFileFormats.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object listed as text. Run \code{\link{getSchemaForObjects}}('CreditCardTransactionFixedLengthFileFormat') to get more field paths.
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
	#' @concept Accounts Payable
	#' @return A list of CreditCardTransactionFixedLengthFileFormats
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	listCreditCardTransactionFixedLengthFileFormats <- function(searchConditionsList = NULL, CreditCardTransactionFixedLengthFileFormatID = F, SkywardID = F, SkywardHash = F, CreditCardTransactionThirdPartyFormatID = F, NumberOfHeaderRows = F, TransactionImportIDStartPosition = F, SequenceNumberStartPosition = F, UniqueCardIdentifierStartPosition = F, TransactionDateStartPosition = F, TransactionAmountStartPosition = F, SplitAmountStartPosition = F, GeneralDefaultCodeOneStartPosition = F, GeneralDefaultCodeTwoStartPosition = F, GeneralDefaultCodeThreeStartPosition = F, GeneralDefaultCodeFourStartPosition = F, GeneralDefaultCodeFiveStartPosition = F, GeneralDefaultCodeSixStartPosition = F, GeneralDefaultCodeSevenStartPosition = F, GeneralDefaultCodeEightStartPosition = F, GeneralDefaultCodeNineStartPosition = F, GeneralDefaultCodeTenStartPosition = F, GeneralDefaultCodeElevenStartPosition = F, GeneralDefaultCodeTwelveStartPosition = F, VendorNameStartPosition = F, VendorCityStartPosition = F, VendorStateProvinceStartPosition = F, VendorZipCodeStartPosition = F, VendorCountryStartPosition = F, TransactionDescriptionStartPosition = F, SplitDescriptionStartPosition = F, TransactionImportIDLength = F, SequenceNumberLength = F, UniqueCardIdentifierLength = F, TransactionDateLength = F, TransactionAmountLength = F, SplitAmountLength = F, GeneralDefaultCodeOneLength = F, GeneralDefaultCodeTwoLength = F, GeneralDefaultCodeThreeLength = F, GeneralDefaultCodeFourLength = F, GeneralDefaultCodeFiveLength = F, GeneralDefaultCodeSixLength = F, GeneralDefaultCodeSevenLength = F, GeneralDefaultCodeEightLength = F, GeneralDefaultCodeNineLength = F, GeneralDefaultCodeTenLength = F, GeneralDefaultCodeElevenLength = F, GeneralDefaultCodeTwelveLength = F, VendorNameLength = F, VendorCityLength = F, VendorStateProvinceLength = F, VendorZipCodeLength = F, VendorCountryLength = F, TransactionDescriptionLength = F, SplitDescriptionLength = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, UniqueAccountIdentifierStartPosition = F, UniqueAccountIdentifierLength = F, fieldPaths = NULL, searchConditionsGroupType = "And", searchSortFieldNamesList = NULL, searchSortFieldNamesDescendingList = NULL, entityId = 1, schoolYearId = NULL, page = 1, pageSize = 100000, flatten = T, returnResponse = F){

		params <- as.list(environment())

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		listSkyObjects(module = "AccountsPayable", objectName = "CreditCardTransactionFixedLengthFileFormat", searchFields = searchFields %>% append(fieldPaths), page = page, pageSize = pageSize, SearchConditionsList = searchConditionsList, SearchConditionsGroupType = searchConditionsGroupType, SearchSortFieldNamesList = searchSortFieldNamesList, SearchSortFieldNamesDescendingList = searchSortFieldNamesDescendingList, entityId = entityId, schoolYearId = schoolYearId, flatten = flatten, returnResponse = returnResponse)
	}

	#' Get a CreditCardTransactionFixedLengthFileFormat
	#'
	#' This function returns a dataframe or json object of a CreditCardTransactionFixedLengthFileFormat
	#' @param CreditCardTransactionFixedLengthFileFormatID The ID of the CreditCardTransactionFixedLengthFileFormat to return.
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given CreditCardTransactionFixedLengthFileFormat. Defaults to FALSE for all return fields which, for convenience, returns all fields for the CreditCardTransactionFixedLengthFileFormat.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object listed as text. Run \code{\link{getSchemaForObjects}}('CreditCardTransactionFixedLengthFileFormat') to get more field paths.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{listSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Accounts Payable
	#' @return A dataframe or of CreditCardTransactionFixedLengthFileFormat
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	getCreditCardTransactionFixedLengthFileFormat <- function(CreditCardTransactionFixedLengthFileFormatID, SkywardID = F, SkywardHash = F, CreditCardTransactionThirdPartyFormatID = F, NumberOfHeaderRows = F, TransactionImportIDStartPosition = F, SequenceNumberStartPosition = F, UniqueCardIdentifierStartPosition = F, TransactionDateStartPosition = F, TransactionAmountStartPosition = F, SplitAmountStartPosition = F, GeneralDefaultCodeOneStartPosition = F, GeneralDefaultCodeTwoStartPosition = F, GeneralDefaultCodeThreeStartPosition = F, GeneralDefaultCodeFourStartPosition = F, GeneralDefaultCodeFiveStartPosition = F, GeneralDefaultCodeSixStartPosition = F, GeneralDefaultCodeSevenStartPosition = F, GeneralDefaultCodeEightStartPosition = F, GeneralDefaultCodeNineStartPosition = F, GeneralDefaultCodeTenStartPosition = F, GeneralDefaultCodeElevenStartPosition = F, GeneralDefaultCodeTwelveStartPosition = F, VendorNameStartPosition = F, VendorCityStartPosition = F, VendorStateProvinceStartPosition = F, VendorZipCodeStartPosition = F, VendorCountryStartPosition = F, TransactionDescriptionStartPosition = F, SplitDescriptionStartPosition = F, TransactionImportIDLength = F, SequenceNumberLength = F, UniqueCardIdentifierLength = F, TransactionDateLength = F, TransactionAmountLength = F, SplitAmountLength = F, GeneralDefaultCodeOneLength = F, GeneralDefaultCodeTwoLength = F, GeneralDefaultCodeThreeLength = F, GeneralDefaultCodeFourLength = F, GeneralDefaultCodeFiveLength = F, GeneralDefaultCodeSixLength = F, GeneralDefaultCodeSevenLength = F, GeneralDefaultCodeEightLength = F, GeneralDefaultCodeNineLength = F, GeneralDefaultCodeTenLength = F, GeneralDefaultCodeElevenLength = F, GeneralDefaultCodeTwelveLength = F, VendorNameLength = F, VendorCityLength = F, VendorStateProvinceLength = F, VendorZipCodeLength = F, VendorCountryLength = F, TransactionDescriptionLength = F, SplitDescriptionLength = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, UniqueAccountIdentifierStartPosition = F, UniqueAccountIdentifierLength = F, fieldPaths = NULL, entityId = 1, schoolYearId = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment()) %>% purrr::keep(names(.) != "CreditCardTransactionFixedLengthFileFormatID")

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		getSkyObject(module = "AccountsPayable", objectName = "CreditCardTransactionFixedLengthFileFormat", objectId = CreditCardTransactionFixedLengthFileFormatID, searchFields = searchFields, entityId = entityId, flatten = flatten, returnResponse = returnResponse)
	}

	#' Delete a CreditCardTransactionFixedLengthFileFormat
	#'
	#' This function deletes a CreditCardTransactionFixedLengthFileFormat
	#' @param CreditCardTransactionFixedLengthFileFormatID The ID of the CreditCardTransactionFixedLengthFileFormat to delete
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{listSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Accounts Payable
	#' @return The CreditCardTransactionFixedLengthFileFormatID of the deleted CreditCardTransactionFixedLengthFileFormat.
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	deleteCreditCardTransactionFixedLengthFileFormat <- function(CreditCardTransactionFixedLengthFileFormatID, ignoreWarnings = F, entityId = 1, schoolYearId = NULL, flatten = T, returnResponse = F){

		deleteSkyObject(module = "AccountsPayable", objectName = "CreditCardTransactionFixedLengthFileFormat", objectId = CreditCardTransactionFixedLengthFileFormatID, ignoreWarnings = ignoreWarnings, entityId = entityId, flatten = flatten, returnResponse = returnResponse)
	}

	#' Create a CreditCardTransactionFixedLengthFileFormat
	#'
	#' This function creates a CreditCardTransactionFixedLengthFileFormat
	#' @param fieldNames The field values to give the created CreditCardTransactionFixedLengthFileFormat. Each defaults to NULL.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{listSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Accounts Payable
	#' @return A newly created CreditCardTransactionFixedLengthFileFormat
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	createCreditCardTransactionFixedLengthFileFormat <- function(CreditCardTransactionThirdPartyFormatID = NULL, NumberOfHeaderRows = NULL, TransactionImportIDStartPosition = NULL, SequenceNumberStartPosition = NULL, UniqueCardIdentifierStartPosition = NULL, TransactionDateStartPosition = NULL, TransactionAmountStartPosition = NULL, SplitAmountStartPosition = NULL, GeneralDefaultCodeOneStartPosition = NULL, GeneralDefaultCodeTwoStartPosition = NULL, GeneralDefaultCodeThreeStartPosition = NULL, GeneralDefaultCodeFourStartPosition = NULL, GeneralDefaultCodeFiveStartPosition = NULL, GeneralDefaultCodeSixStartPosition = NULL, GeneralDefaultCodeSevenStartPosition = NULL, GeneralDefaultCodeEightStartPosition = NULL, GeneralDefaultCodeNineStartPosition = NULL, GeneralDefaultCodeTenStartPosition = NULL, GeneralDefaultCodeElevenStartPosition = NULL, GeneralDefaultCodeTwelveStartPosition = NULL, VendorNameStartPosition = NULL, VendorCityStartPosition = NULL, VendorStateProvinceStartPosition = NULL, VendorZipCodeStartPosition = NULL, VendorCountryStartPosition = NULL, TransactionDescriptionStartPosition = NULL, SplitDescriptionStartPosition = NULL, TransactionImportIDLength = NULL, SequenceNumberLength = NULL, UniqueCardIdentifierLength = NULL, TransactionDateLength = NULL, TransactionAmountLength = NULL, SplitAmountLength = NULL, GeneralDefaultCodeOneLength = NULL, GeneralDefaultCodeTwoLength = NULL, GeneralDefaultCodeThreeLength = NULL, GeneralDefaultCodeFourLength = NULL, GeneralDefaultCodeFiveLength = NULL, GeneralDefaultCodeSixLength = NULL, GeneralDefaultCodeSevenLength = NULL, GeneralDefaultCodeEightLength = NULL, GeneralDefaultCodeNineLength = NULL, GeneralDefaultCodeTenLength = NULL, GeneralDefaultCodeElevenLength = NULL, GeneralDefaultCodeTwelveLength = NULL, VendorNameLength = NULL, VendorCityLength = NULL, VendorStateProvinceLength = NULL, VendorZipCodeLength = NULL, VendorCountryLength = NULL, TransactionDescriptionLength = NULL, SplitDescriptionLength = NULL, UniqueAccountIdentifierStartPosition = NULL, UniqueAccountIdentifierLength = NULL, entityId = 1, schoolYearId = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment())

		body <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper()) %>% compact()

		createSkyObject(module = "AccountsPayable", objectName = "CreditCardTransactionFixedLengthFileFormat", body = list(DataObject = body), searchFields = append("CreditCardTransactionFixedLengthFileFormatID", body %>% names()), entityId = entityId, schoolYearId = schoolYearId, flatten = flatten, returnResponse = returnResponse)
	}

	#' Modify a CreditCardTransactionFixedLengthFileFormat
	#'
	#' This function modifies a CreditCardTransactionFixedLengthFileFormat
	#' @param fieldNames The field values to give the modified CreditCardTransactionFixedLengthFileFormat. Each defaults to NULL.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{listSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Accounts Payable
	#' @return The modified CreditCardTransactionFixedLengthFileFormat
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	modifyCreditCardTransactionFixedLengthFileFormat <- function(CreditCardTransactionFixedLengthFileFormatID, CreditCardTransactionThirdPartyFormatID = NULL, NumberOfHeaderRows = NULL, TransactionImportIDStartPosition = NULL, SequenceNumberStartPosition = NULL, UniqueCardIdentifierStartPosition = NULL, TransactionDateStartPosition = NULL, TransactionAmountStartPosition = NULL, SplitAmountStartPosition = NULL, GeneralDefaultCodeOneStartPosition = NULL, GeneralDefaultCodeTwoStartPosition = NULL, GeneralDefaultCodeThreeStartPosition = NULL, GeneralDefaultCodeFourStartPosition = NULL, GeneralDefaultCodeFiveStartPosition = NULL, GeneralDefaultCodeSixStartPosition = NULL, GeneralDefaultCodeSevenStartPosition = NULL, GeneralDefaultCodeEightStartPosition = NULL, GeneralDefaultCodeNineStartPosition = NULL, GeneralDefaultCodeTenStartPosition = NULL, GeneralDefaultCodeElevenStartPosition = NULL, GeneralDefaultCodeTwelveStartPosition = NULL, VendorNameStartPosition = NULL, VendorCityStartPosition = NULL, VendorStateProvinceStartPosition = NULL, VendorZipCodeStartPosition = NULL, VendorCountryStartPosition = NULL, TransactionDescriptionStartPosition = NULL, SplitDescriptionStartPosition = NULL, TransactionImportIDLength = NULL, SequenceNumberLength = NULL, UniqueCardIdentifierLength = NULL, TransactionDateLength = NULL, TransactionAmountLength = NULL, SplitAmountLength = NULL, GeneralDefaultCodeOneLength = NULL, GeneralDefaultCodeTwoLength = NULL, GeneralDefaultCodeThreeLength = NULL, GeneralDefaultCodeFourLength = NULL, GeneralDefaultCodeFiveLength = NULL, GeneralDefaultCodeSixLength = NULL, GeneralDefaultCodeSevenLength = NULL, GeneralDefaultCodeEightLength = NULL, GeneralDefaultCodeNineLength = NULL, GeneralDefaultCodeTenLength = NULL, GeneralDefaultCodeElevenLength = NULL, GeneralDefaultCodeTwelveLength = NULL, VendorNameLength = NULL, VendorCityLength = NULL, VendorStateProvinceLength = NULL, VendorZipCodeLength = NULL, VendorCountryLength = NULL, TransactionDescriptionLength = NULL, SplitDescriptionLength = NULL, UniqueAccountIdentifierStartPosition = NULL, UniqueAccountIdentifierLength = NULL, entityId = 1, schoolYearId = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment())

		body <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper()) %>% compact()

		modifySkyObject(module = "AccountsPayable", objectName = "CreditCardTransactionFixedLengthFileFormat", objectId = CreditCardTransactionFixedLengthFileFormatID, body = list(DataObject = body), searchFields = append("CreditCardTransactionFixedLengthFileFormatID", body %>% names()), entityId = entityId, schoolYearId = schoolYearId, flatten = flatten, returnResponse = returnResponse)
	}

	#' List CreditCardTransactionThirdPartyFormats
	#'
	#' This function returns a dataframe or json object of CreditCardTransactionThirdPartyFormats
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given CreditCardTransactionThirdPartyFormats. Defaults to FALSE for all return fields which, for convenience, returns all fields for the CreditCardTransactionThirdPartyFormats.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object listed as text. Run \code{\link{getSchemaForObjects}}('CreditCardTransactionThirdPartyFormat') to get more field paths.
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
	#' @concept Accounts Payable
	#' @return A list of CreditCardTransactionThirdPartyFormats
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	listCreditCardTransactionThirdPartyFormats <- function(searchConditionsList = NULL, CreditCardTransactionThirdPartyFormatID = F, SkywardID = F, SkywardHash = F, SkywardIDClonedFrom = F, DistrictID = F, Code = F, Description = F, CodeDescription = F, IsSystemLoaded = F, ImportType = F, DateFormat = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, DefaultCreditCardTransactionDetailDescription = F, UniqueCardIdentifierType = F, UniqueAccountIdentifierType = F, fieldPaths = NULL, searchConditionsGroupType = "And", searchSortFieldNamesList = NULL, searchSortFieldNamesDescendingList = NULL, entityId = 1, schoolYearId = NULL, page = 1, pageSize = 100000, flatten = T, returnResponse = F){

		params <- as.list(environment())

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		listSkyObjects(module = "AccountsPayable", objectName = "CreditCardTransactionThirdPartyFormat", searchFields = searchFields %>% append(fieldPaths), page = page, pageSize = pageSize, SearchConditionsList = searchConditionsList, SearchConditionsGroupType = searchConditionsGroupType, SearchSortFieldNamesList = searchSortFieldNamesList, SearchSortFieldNamesDescendingList = searchSortFieldNamesDescendingList, entityId = entityId, schoolYearId = schoolYearId, flatten = flatten, returnResponse = returnResponse)
	}

	#' Get a CreditCardTransactionThirdPartyFormat
	#'
	#' This function returns a dataframe or json object of a CreditCardTransactionThirdPartyFormat
	#' @param CreditCardTransactionThirdPartyFormatID The ID of the CreditCardTransactionThirdPartyFormat to return.
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given CreditCardTransactionThirdPartyFormat. Defaults to FALSE for all return fields which, for convenience, returns all fields for the CreditCardTransactionThirdPartyFormat.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object listed as text. Run \code{\link{getSchemaForObjects}}('CreditCardTransactionThirdPartyFormat') to get more field paths.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{listSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Accounts Payable
	#' @return A dataframe or of CreditCardTransactionThirdPartyFormat
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	getCreditCardTransactionThirdPartyFormat <- function(CreditCardTransactionThirdPartyFormatID, SkywardID = F, SkywardHash = F, SkywardIDClonedFrom = F, DistrictID = F, Code = F, Description = F, CodeDescription = F, IsSystemLoaded = F, ImportType = F, DateFormat = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, DefaultCreditCardTransactionDetailDescription = F, UniqueCardIdentifierType = F, UniqueAccountIdentifierType = F, fieldPaths = NULL, entityId = 1, schoolYearId = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment()) %>% purrr::keep(names(.) != "CreditCardTransactionThirdPartyFormatID")

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		getSkyObject(module = "AccountsPayable", objectName = "CreditCardTransactionThirdPartyFormat", objectId = CreditCardTransactionThirdPartyFormatID, searchFields = searchFields, entityId = entityId, flatten = flatten, returnResponse = returnResponse)
	}

	#' Delete a CreditCardTransactionThirdPartyFormat
	#'
	#' This function deletes a CreditCardTransactionThirdPartyFormat
	#' @param CreditCardTransactionThirdPartyFormatID The ID of the CreditCardTransactionThirdPartyFormat to delete
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{listSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Accounts Payable
	#' @return The CreditCardTransactionThirdPartyFormatID of the deleted CreditCardTransactionThirdPartyFormat.
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	deleteCreditCardTransactionThirdPartyFormat <- function(CreditCardTransactionThirdPartyFormatID, ignoreWarnings = F, entityId = 1, schoolYearId = NULL, flatten = T, returnResponse = F){

		deleteSkyObject(module = "AccountsPayable", objectName = "CreditCardTransactionThirdPartyFormat", objectId = CreditCardTransactionThirdPartyFormatID, ignoreWarnings = ignoreWarnings, entityId = entityId, flatten = flatten, returnResponse = returnResponse)
	}

	#' Create a CreditCardTransactionThirdPartyFormat
	#'
	#' This function creates a CreditCardTransactionThirdPartyFormat
	#' @param fieldNames The field values to give the created CreditCardTransactionThirdPartyFormat. Each defaults to NULL.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{listSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Accounts Payable
	#' @return A newly created CreditCardTransactionThirdPartyFormat
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	createCreditCardTransactionThirdPartyFormat <- function(SkywardIDClonedFrom = NULL, DistrictID = NULL, Code = NULL, Description = NULL, ImportType = NULL, DateFormat = NULL, DefaultCreditCardTransactionDetailDescription = NULL, UniqueCardIdentifierType = NULL, UniqueAccountIdentifierType = NULL, entityId = 1, schoolYearId = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment())

		body <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper()) %>% compact()

		createSkyObject(module = "AccountsPayable", objectName = "CreditCardTransactionThirdPartyFormat", body = list(DataObject = body), searchFields = append("CreditCardTransactionThirdPartyFormatID", body %>% names()), entityId = entityId, schoolYearId = schoolYearId, flatten = flatten, returnResponse = returnResponse)
	}

	#' Modify a CreditCardTransactionThirdPartyFormat
	#'
	#' This function modifies a CreditCardTransactionThirdPartyFormat
	#' @param fieldNames The field values to give the modified CreditCardTransactionThirdPartyFormat. Each defaults to NULL.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{listSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Accounts Payable
	#' @return The modified CreditCardTransactionThirdPartyFormat
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	modifyCreditCardTransactionThirdPartyFormat <- function(CreditCardTransactionThirdPartyFormatID, SkywardIDClonedFrom = NULL, DistrictID = NULL, Code = NULL, Description = NULL, ImportType = NULL, DateFormat = NULL, DefaultCreditCardTransactionDetailDescription = NULL, UniqueCardIdentifierType = NULL, UniqueAccountIdentifierType = NULL, entityId = 1, schoolYearId = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment())

		body <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper()) %>% compact()

		modifySkyObject(module = "AccountsPayable", objectName = "CreditCardTransactionThirdPartyFormat", objectId = CreditCardTransactionThirdPartyFormatID, body = list(DataObject = body), searchFields = append("CreditCardTransactionThirdPartyFormatID", body %>% names()), entityId = entityId, schoolYearId = schoolYearId, flatten = flatten, returnResponse = returnResponse)
	}

	#' List CreditCardTransactionThirdPartyImports
	#'
	#' This function returns a dataframe or json object of CreditCardTransactionThirdPartyImports
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given CreditCardTransactionThirdPartyImports. Defaults to FALSE for all return fields which, for convenience, returns all fields for the CreditCardTransactionThirdPartyImports.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object listed as text. Run \code{\link{getSchemaForObjects}}('CreditCardTransactionThirdPartyImport') to get more field paths.
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
	#' @concept Accounts Payable
	#' @return A list of CreditCardTransactionThirdPartyImports
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	listCreditCardTransactionThirdPartyImports <- function(searchConditionsList = NULL, CreditCardTransactionThirdPartyImportID = F, CreditCardTransactionThirdPartyFormatID = F, ImportTime = F, ImportData = F, MediaID = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, fieldPaths = NULL, searchConditionsGroupType = "And", searchSortFieldNamesList = NULL, searchSortFieldNamesDescendingList = NULL, entityId = 1, schoolYearId = NULL, page = 1, pageSize = 100000, flatten = T, returnResponse = F){

		params <- as.list(environment())

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		listSkyObjects(module = "AccountsPayable", objectName = "CreditCardTransactionThirdPartyImport", searchFields = searchFields %>% append(fieldPaths), page = page, pageSize = pageSize, SearchConditionsList = searchConditionsList, SearchConditionsGroupType = searchConditionsGroupType, SearchSortFieldNamesList = searchSortFieldNamesList, SearchSortFieldNamesDescendingList = searchSortFieldNamesDescendingList, entityId = entityId, schoolYearId = schoolYearId, flatten = flatten, returnResponse = returnResponse)
	}

	#' Get a CreditCardTransactionThirdPartyImport
	#'
	#' This function returns a dataframe or json object of a CreditCardTransactionThirdPartyImport
	#' @param CreditCardTransactionThirdPartyImportID The ID of the CreditCardTransactionThirdPartyImport to return.
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given CreditCardTransactionThirdPartyImport. Defaults to FALSE for all return fields which, for convenience, returns all fields for the CreditCardTransactionThirdPartyImport.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object listed as text. Run \code{\link{getSchemaForObjects}}('CreditCardTransactionThirdPartyImport') to get more field paths.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{listSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Accounts Payable
	#' @return A dataframe or of CreditCardTransactionThirdPartyImport
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	getCreditCardTransactionThirdPartyImport <- function(CreditCardTransactionThirdPartyImportID, CreditCardTransactionThirdPartyFormatID = F, ImportTime = F, ImportData = F, MediaID = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, fieldPaths = NULL, entityId = 1, schoolYearId = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment()) %>% purrr::keep(names(.) != "CreditCardTransactionThirdPartyImportID")

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		getSkyObject(module = "AccountsPayable", objectName = "CreditCardTransactionThirdPartyImport", objectId = CreditCardTransactionThirdPartyImportID, searchFields = searchFields, entityId = entityId, flatten = flatten, returnResponse = returnResponse)
	}

	#' Delete a CreditCardTransactionThirdPartyImport
	#'
	#' This function deletes a CreditCardTransactionThirdPartyImport
	#' @param CreditCardTransactionThirdPartyImportID The ID of the CreditCardTransactionThirdPartyImport to delete
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{listSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Accounts Payable
	#' @return The CreditCardTransactionThirdPartyImportID of the deleted CreditCardTransactionThirdPartyImport.
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	deleteCreditCardTransactionThirdPartyImport <- function(CreditCardTransactionThirdPartyImportID, ignoreWarnings = F, entityId = 1, schoolYearId = NULL, flatten = T, returnResponse = F){

		deleteSkyObject(module = "AccountsPayable", objectName = "CreditCardTransactionThirdPartyImport", objectId = CreditCardTransactionThirdPartyImportID, ignoreWarnings = ignoreWarnings, entityId = entityId, flatten = flatten, returnResponse = returnResponse)
	}

	#' Create a CreditCardTransactionThirdPartyImport
	#'
	#' This function creates a CreditCardTransactionThirdPartyImport
	#' @param fieldNames The field values to give the created CreditCardTransactionThirdPartyImport. Each defaults to NULL.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{listSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Accounts Payable
	#' @return A newly created CreditCardTransactionThirdPartyImport
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	createCreditCardTransactionThirdPartyImport <- function(CreditCardTransactionThirdPartyFormatID = NULL, ImportTime = NULL, ImportData = NULL, MediaID = NULL, entityId = 1, schoolYearId = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment())

		body <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper()) %>% compact()

		createSkyObject(module = "AccountsPayable", objectName = "CreditCardTransactionThirdPartyImport", body = list(DataObject = body), searchFields = append("CreditCardTransactionThirdPartyImportID", body %>% names()), entityId = entityId, schoolYearId = schoolYearId, flatten = flatten, returnResponse = returnResponse)
	}

	#' Modify a CreditCardTransactionThirdPartyImport
	#'
	#' This function modifies a CreditCardTransactionThirdPartyImport
	#' @param fieldNames The field values to give the modified CreditCardTransactionThirdPartyImport. Each defaults to NULL.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{listSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Accounts Payable
	#' @return The modified CreditCardTransactionThirdPartyImport
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	modifyCreditCardTransactionThirdPartyImport <- function(CreditCardTransactionThirdPartyImportID, CreditCardTransactionThirdPartyFormatID = NULL, ImportTime = NULL, ImportData = NULL, MediaID = NULL, entityId = 1, schoolYearId = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment())

		body <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper()) %>% compact()

		modifySkyObject(module = "AccountsPayable", objectName = "CreditCardTransactionThirdPartyImport", objectId = CreditCardTransactionThirdPartyImportID, body = list(DataObject = body), searchFields = append("CreditCardTransactionThirdPartyImportID", body %>% names()), entityId = entityId, schoolYearId = schoolYearId, flatten = flatten, returnResponse = returnResponse)
	}

	#' List CreditCardTransactionThirdPartyFormatVendors
	#'
	#' This function returns a dataframe or json object of CreditCardTransactionThirdPartyFormatVendors
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given CreditCardTransactionThirdPartyFormatVendors. Defaults to FALSE for all return fields which, for convenience, returns all fields for the CreditCardTransactionThirdPartyFormatVendors.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object listed as text. Run \code{\link{getSchemaForObjects}}('CreditCardTransactionThirdPartyFormatVendor') to get more field paths.
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
	#' @concept Accounts Payable
	#' @return A list of CreditCardTransactionThirdPartyFormatVendors
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	listCreditCardTransactionThirdPartyFormatVendors <- function(searchConditionsList = NULL, CreditCardTransactionThirdPartyFormatVendorID = F, VendorID = F, ImportValue = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, CreditCardTransactionThirdPartyFormatID = F, fieldPaths = NULL, searchConditionsGroupType = "And", searchSortFieldNamesList = NULL, searchSortFieldNamesDescendingList = NULL, entityId = 1, schoolYearId = NULL, page = 1, pageSize = 100000, flatten = T, returnResponse = F){

		params <- as.list(environment())

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		listSkyObjects(module = "AccountsPayable", objectName = "CreditCardTransactionThirdPartyFormatVendor", searchFields = searchFields %>% append(fieldPaths), page = page, pageSize = pageSize, SearchConditionsList = searchConditionsList, SearchConditionsGroupType = searchConditionsGroupType, SearchSortFieldNamesList = searchSortFieldNamesList, SearchSortFieldNamesDescendingList = searchSortFieldNamesDescendingList, entityId = entityId, schoolYearId = schoolYearId, flatten = flatten, returnResponse = returnResponse)
	}

	#' Get a CreditCardTransactionThirdPartyFormatVendor
	#'
	#' This function returns a dataframe or json object of a CreditCardTransactionThirdPartyFormatVendor
	#' @param CreditCardTransactionThirdPartyFormatVendorID The ID of the CreditCardTransactionThirdPartyFormatVendor to return.
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given CreditCardTransactionThirdPartyFormatVendor. Defaults to FALSE for all return fields which, for convenience, returns all fields for the CreditCardTransactionThirdPartyFormatVendor.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object listed as text. Run \code{\link{getSchemaForObjects}}('CreditCardTransactionThirdPartyFormatVendor') to get more field paths.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{listSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Accounts Payable
	#' @return A dataframe or of CreditCardTransactionThirdPartyFormatVendor
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	getCreditCardTransactionThirdPartyFormatVendor <- function(CreditCardTransactionThirdPartyFormatVendorID, VendorID = F, ImportValue = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, CreditCardTransactionThirdPartyFormatID = F, fieldPaths = NULL, entityId = 1, schoolYearId = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment()) %>% purrr::keep(names(.) != "CreditCardTransactionThirdPartyFormatVendorID")

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		getSkyObject(module = "AccountsPayable", objectName = "CreditCardTransactionThirdPartyFormatVendor", objectId = CreditCardTransactionThirdPartyFormatVendorID, searchFields = searchFields, entityId = entityId, flatten = flatten, returnResponse = returnResponse)
	}

	#' Delete a CreditCardTransactionThirdPartyFormatVendor
	#'
	#' This function deletes a CreditCardTransactionThirdPartyFormatVendor
	#' @param CreditCardTransactionThirdPartyFormatVendorID The ID of the CreditCardTransactionThirdPartyFormatVendor to delete
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{listSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Accounts Payable
	#' @return The CreditCardTransactionThirdPartyFormatVendorID of the deleted CreditCardTransactionThirdPartyFormatVendor.
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	deleteCreditCardTransactionThirdPartyFormatVendor <- function(CreditCardTransactionThirdPartyFormatVendorID, ignoreWarnings = F, entityId = 1, schoolYearId = NULL, flatten = T, returnResponse = F){

		deleteSkyObject(module = "AccountsPayable", objectName = "CreditCardTransactionThirdPartyFormatVendor", objectId = CreditCardTransactionThirdPartyFormatVendorID, ignoreWarnings = ignoreWarnings, entityId = entityId, flatten = flatten, returnResponse = returnResponse)
	}

	#' Create a CreditCardTransactionThirdPartyFormatVendor
	#'
	#' This function creates a CreditCardTransactionThirdPartyFormatVendor
	#' @param fieldNames The field values to give the created CreditCardTransactionThirdPartyFormatVendor. Each defaults to NULL.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{listSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Accounts Payable
	#' @return A newly created CreditCardTransactionThirdPartyFormatVendor
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	createCreditCardTransactionThirdPartyFormatVendor <- function(VendorID = NULL, ImportValue = NULL, CreditCardTransactionThirdPartyFormatID = NULL, entityId = 1, schoolYearId = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment())

		body <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper()) %>% compact()

		createSkyObject(module = "AccountsPayable", objectName = "CreditCardTransactionThirdPartyFormatVendor", body = list(DataObject = body), searchFields = append("CreditCardTransactionThirdPartyFormatVendorID", body %>% names()), entityId = entityId, schoolYearId = schoolYearId, flatten = flatten, returnResponse = returnResponse)
	}

	#' Modify a CreditCardTransactionThirdPartyFormatVendor
	#'
	#' This function modifies a CreditCardTransactionThirdPartyFormatVendor
	#' @param fieldNames The field values to give the modified CreditCardTransactionThirdPartyFormatVendor. Each defaults to NULL.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{listSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Accounts Payable
	#' @return The modified CreditCardTransactionThirdPartyFormatVendor
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	modifyCreditCardTransactionThirdPartyFormatVendor <- function(CreditCardTransactionThirdPartyFormatVendorID, VendorID = NULL, ImportValue = NULL, CreditCardTransactionThirdPartyFormatID = NULL, entityId = 1, schoolYearId = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment())

		body <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper()) %>% compact()

		modifySkyObject(module = "AccountsPayable", objectName = "CreditCardTransactionThirdPartyFormatVendor", objectId = CreditCardTransactionThirdPartyFormatVendorID, body = list(DataObject = body), searchFields = append("CreditCardTransactionThirdPartyFormatVendorID", body %>% names()), entityId = entityId, schoolYearId = schoolYearId, flatten = flatten, returnResponse = returnResponse)
	}

	#' List TempCreditCardTransactionDetails
	#'
	#' This function returns a dataframe or json object of TempCreditCardTransactionDetails
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given TempCreditCardTransactionDetails. Defaults to FALSE for all return fields which, for convenience, returns all fields for the TempCreditCardTransactionDetails.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object listed as text. Run \code{\link{getSchemaForObjects}}('TempCreditCardTransactionDetail') to get more field paths.
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
	#' @concept Accounts Payable
	#' @return A list of TempCreditCardTransactionDetails
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	listTempCreditCardTransactionDetails <- function(searchConditionsList = NULL, TempCreditCardTransactionDetailID = F, TempCreditCardTransactionID = F, CreditCardTransactionID = F, Description = F, EntryAmount = F, BaseCurrencyAmount = F, Quantity = F, UnitCost = F, AccountID = F, AccountDistribution = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, HasTax = F, SiteWAID = F, SiteCodeDescription = F, State = F, fieldPaths = NULL, searchConditionsGroupType = "And", searchSortFieldNamesList = NULL, searchSortFieldNamesDescendingList = NULL, entityId = 1, schoolYearId = NULL, page = 1, pageSize = 100000, flatten = T, returnResponse = F){

		params <- as.list(environment())

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		listSkyObjects(module = "AccountsPayable", objectName = "TempCreditCardTransactionDetail", searchFields = searchFields %>% append(fieldPaths), page = page, pageSize = pageSize, SearchConditionsList = searchConditionsList, SearchConditionsGroupType = searchConditionsGroupType, SearchSortFieldNamesList = searchSortFieldNamesList, SearchSortFieldNamesDescendingList = searchSortFieldNamesDescendingList, entityId = entityId, schoolYearId = schoolYearId, flatten = flatten, returnResponse = returnResponse)
	}

	#' Get a TempCreditCardTransactionDetail
	#'
	#' This function returns a dataframe or json object of a TempCreditCardTransactionDetail
	#' @param TempCreditCardTransactionDetailID The ID of the TempCreditCardTransactionDetail to return.
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given TempCreditCardTransactionDetail. Defaults to FALSE for all return fields which, for convenience, returns all fields for the TempCreditCardTransactionDetail.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object listed as text. Run \code{\link{getSchemaForObjects}}('TempCreditCardTransactionDetail') to get more field paths.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{listSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Accounts Payable
	#' @return A dataframe or of TempCreditCardTransactionDetail
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	getTempCreditCardTransactionDetail <- function(TempCreditCardTransactionDetailID, TempCreditCardTransactionID = F, CreditCardTransactionID = F, Description = F, EntryAmount = F, BaseCurrencyAmount = F, Quantity = F, UnitCost = F, AccountID = F, AccountDistribution = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, HasTax = F, SiteWAID = F, SiteCodeDescription = F, State = F, fieldPaths = NULL, entityId = 1, schoolYearId = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment()) %>% purrr::keep(names(.) != "TempCreditCardTransactionDetailID")

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		getSkyObject(module = "AccountsPayable", objectName = "TempCreditCardTransactionDetail", objectId = TempCreditCardTransactionDetailID, searchFields = searchFields, entityId = entityId, flatten = flatten, returnResponse = returnResponse)
	}

	#' Delete a TempCreditCardTransactionDetail
	#'
	#' This function deletes a TempCreditCardTransactionDetail
	#' @param TempCreditCardTransactionDetailID The ID of the TempCreditCardTransactionDetail to delete
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{listSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Accounts Payable
	#' @return The TempCreditCardTransactionDetailID of the deleted TempCreditCardTransactionDetail.
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	deleteTempCreditCardTransactionDetail <- function(TempCreditCardTransactionDetailID, ignoreWarnings = F, entityId = 1, schoolYearId = NULL, flatten = T, returnResponse = F){

		deleteSkyObject(module = "AccountsPayable", objectName = "TempCreditCardTransactionDetail", objectId = TempCreditCardTransactionDetailID, ignoreWarnings = ignoreWarnings, entityId = entityId, flatten = flatten, returnResponse = returnResponse)
	}

	#' Create a TempCreditCardTransactionDetail
	#'
	#' This function creates a TempCreditCardTransactionDetail
	#' @param fieldNames The field values to give the created TempCreditCardTransactionDetail. Each defaults to NULL.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{listSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Accounts Payable
	#' @return A newly created TempCreditCardTransactionDetail
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	createTempCreditCardTransactionDetail <- function(TempCreditCardTransactionID = NULL, CreditCardTransactionID = NULL, Description = NULL, EntryAmount = NULL, BaseCurrencyAmount = NULL, Quantity = NULL, UnitCost = NULL, AccountID = NULL, AccountDistribution = NULL, HasTax = NULL, SiteWAID = NULL, SiteCodeDescription = NULL, State = NULL, entityId = 1, schoolYearId = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment())

		body <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper()) %>% compact()

		createSkyObject(module = "AccountsPayable", objectName = "TempCreditCardTransactionDetail", body = list(DataObject = body), searchFields = append("TempCreditCardTransactionDetailID", body %>% names()), entityId = entityId, schoolYearId = schoolYearId, flatten = flatten, returnResponse = returnResponse)
	}

	#' Modify a TempCreditCardTransactionDetail
	#'
	#' This function modifies a TempCreditCardTransactionDetail
	#' @param fieldNames The field values to give the modified TempCreditCardTransactionDetail. Each defaults to NULL.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{listSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Accounts Payable
	#' @return The modified TempCreditCardTransactionDetail
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	modifyTempCreditCardTransactionDetail <- function(TempCreditCardTransactionDetailID, TempCreditCardTransactionID = NULL, CreditCardTransactionID = NULL, Description = NULL, EntryAmount = NULL, BaseCurrencyAmount = NULL, Quantity = NULL, UnitCost = NULL, AccountID = NULL, AccountDistribution = NULL, HasTax = NULL, SiteWAID = NULL, SiteCodeDescription = NULL, State = NULL, entityId = 1, schoolYearId = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment())

		body <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper()) %>% compact()

		modifySkyObject(module = "AccountsPayable", objectName = "TempCreditCardTransactionDetail", objectId = TempCreditCardTransactionDetailID, body = list(DataObject = body), searchFields = append("TempCreditCardTransactionDetailID", body %>% names()), entityId = entityId, schoolYearId = schoolYearId, flatten = flatten, returnResponse = returnResponse)
	}

	#' List AccountsPayableTempErrors
	#'
	#' This function returns a dataframe or json object of AccountsPayableTempErrors
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given AccountsPayableTempErrors. Defaults to FALSE for all return fields which, for convenience, returns all fields for the AccountsPayableTempErrors.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object listed as text. Run \code{\link{getSchemaForObjects}}('AccountsPayableTempError') to get more field paths.
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
	#' @concept Accounts Payable
	#' @return A list of AccountsPayableTempErrors
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	listAccountsPayableTempErrors <- function(searchConditionsList = NULL, TempErrorID = F, TempCreditCardTransactionID = F, Error = F, ErrorDetail = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, fieldPaths = NULL, searchConditionsGroupType = "And", searchSortFieldNamesList = NULL, searchSortFieldNamesDescendingList = NULL, entityId = 1, schoolYearId = NULL, page = 1, pageSize = 100000, flatten = T, returnResponse = F){

		params <- as.list(environment())

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		listSkyObjects(module = "AccountsPayable", objectName = "TempError", searchFields = searchFields %>% append(fieldPaths), page = page, pageSize = pageSize, SearchConditionsList = searchConditionsList, SearchConditionsGroupType = searchConditionsGroupType, SearchSortFieldNamesList = searchSortFieldNamesList, SearchSortFieldNamesDescendingList = searchSortFieldNamesDescendingList, entityId = entityId, schoolYearId = schoolYearId, flatten = flatten, returnResponse = returnResponse)
	}

	#' Get an AccountsPayableTempError
	#'
	#' This function returns a dataframe or json object of an AccountsPayableTempError
	#' @param AccountsPayableTempErrorID The ID of the AccountsPayableTempError to return.
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given AccountsPayableTempError. Defaults to FALSE for all return fields which, for convenience, returns all fields for the AccountsPayableTempError.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object listed as text. Run \code{\link{getSchemaForObjects}}('AccountsPayableTempError') to get more field paths.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{listSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Accounts Payable
	#' @return A dataframe or of AccountsPayableTempError
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	getAccountsPayableTempError <- function(AccountsPayableTempErrorID, TempErrorID = F, TempCreditCardTransactionID = F, Error = F, ErrorDetail = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, fieldPaths = NULL, entityId = 1, schoolYearId = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment()) %>% purrr::keep(names(.) != "AccountsPayableTempErrorID")

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		getSkyObject(module = "AccountsPayable", objectName = "TempError", objectId = AccountsPayableTempErrorID, searchFields = searchFields, entityId = entityId, flatten = flatten, returnResponse = returnResponse)
	}

	#' Delete an AccountsPayableTempError
	#'
	#' This function deletes an AccountsPayableTempError
	#' @param AccountsPayableTempErrorID The ID of the AccountsPayableTempError to delete
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{listSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Accounts Payable
	#' @return The AccountsPayableTempErrorID of the deleted AccountsPayableTempError.
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	deleteAccountsPayableTempError <- function(AccountsPayableTempErrorID, ignoreWarnings = F, entityId = 1, schoolYearId = NULL, flatten = T, returnResponse = F){

		deleteSkyObject(module = "AccountsPayable", objectName = "TempError", objectId = AccountsPayableTempErrorID, ignoreWarnings = ignoreWarnings, entityId = entityId, flatten = flatten, returnResponse = returnResponse)
	}

	#' Create an AccountsPayableTempError
	#'
	#' This function creates an AccountsPayableTempError
	#' @param fieldNames The field values to give the created AccountsPayableTempError. Each defaults to NULL.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{listSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Accounts Payable
	#' @return A newly created AccountsPayableTempError
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	createAccountsPayableTempError <- function(TempCreditCardTransactionID = NULL, Error = NULL, ErrorDetail = NULL, entityId = 1, schoolYearId = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment())

		body <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper()) %>% compact()

		createSkyObject(module = "AccountsPayable", objectName = "TempError", body = list(DataObject = body), searchFields = append("TempErrorID", body %>% names()), entityId = entityId, schoolYearId = schoolYearId, flatten = flatten, returnResponse = returnResponse)
	}

	#' Modify an AccountsPayableTempError
	#'
	#' This function modifies an AccountsPayableTempError
	#' @param fieldNames The field values to give the modified AccountsPayableTempError. Each defaults to NULL.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{listSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Accounts Payable
	#' @return The modified AccountsPayableTempError
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	modifyAccountsPayableTempError <- function(TempErrorID, TempCreditCardTransactionID = NULL, Error = NULL, ErrorDetail = NULL, entityId = 1, schoolYearId = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment())

		body <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper()) %>% compact()

		modifySkyObject(module = "AccountsPayable", objectName = "TempError", objectId = TempErrorID, body = list(DataObject = body), searchFields = append("TempErrorID", body %>% names()), entityId = entityId, schoolYearId = schoolYearId, flatten = flatten, returnResponse = returnResponse)
	}

	#' List AccountsPayableTempExceptions
	#'
	#' This function returns a dataframe or json object of AccountsPayableTempExceptions
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given AccountsPayableTempExceptions. Defaults to FALSE for all return fields which, for convenience, returns all fields for the AccountsPayableTempExceptions.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object listed as text. Run \code{\link{getSchemaForObjects}}('AccountsPayableTempException') to get more field paths.
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
	#' @concept Accounts Payable
	#' @return A list of AccountsPayableTempExceptions
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	listAccountsPayableTempExceptions <- function(searchConditionsList = NULL, TempExceptionID = F, Message = F, LineNumber = F, IsFatal = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, fieldPaths = NULL, searchConditionsGroupType = "And", searchSortFieldNamesList = NULL, searchSortFieldNamesDescendingList = NULL, entityId = 1, schoolYearId = NULL, page = 1, pageSize = 100000, flatten = T, returnResponse = F){

		params <- as.list(environment())

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		listSkyObjects(module = "AccountsPayable", objectName = "TempException", searchFields = searchFields %>% append(fieldPaths), page = page, pageSize = pageSize, SearchConditionsList = searchConditionsList, SearchConditionsGroupType = searchConditionsGroupType, SearchSortFieldNamesList = searchSortFieldNamesList, SearchSortFieldNamesDescendingList = searchSortFieldNamesDescendingList, entityId = entityId, schoolYearId = schoolYearId, flatten = flatten, returnResponse = returnResponse)
	}

	#' Get an AccountsPayableTempException
	#'
	#' This function returns a dataframe or json object of an AccountsPayableTempException
	#' @param AccountsPayableTempExceptionID The ID of the AccountsPayableTempException to return.
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given AccountsPayableTempException. Defaults to FALSE for all return fields which, for convenience, returns all fields for the AccountsPayableTempException.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object listed as text. Run \code{\link{getSchemaForObjects}}('AccountsPayableTempException') to get more field paths.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{listSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Accounts Payable
	#' @return A dataframe or of AccountsPayableTempException
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	getAccountsPayableTempException <- function(AccountsPayableTempExceptionID, TempExceptionID = F, Message = F, LineNumber = F, IsFatal = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, fieldPaths = NULL, entityId = 1, schoolYearId = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment()) %>% purrr::keep(names(.) != "AccountsPayableTempExceptionID")

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		getSkyObject(module = "AccountsPayable", objectName = "TempException", objectId = AccountsPayableTempExceptionID, searchFields = searchFields, entityId = entityId, flatten = flatten, returnResponse = returnResponse)
	}

	#' Delete an AccountsPayableTempException
	#'
	#' This function deletes an AccountsPayableTempException
	#' @param AccountsPayableTempExceptionID The ID of the AccountsPayableTempException to delete
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{listSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Accounts Payable
	#' @return The AccountsPayableTempExceptionID of the deleted AccountsPayableTempException.
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	deleteAccountsPayableTempException <- function(AccountsPayableTempExceptionID, ignoreWarnings = F, entityId = 1, schoolYearId = NULL, flatten = T, returnResponse = F){

		deleteSkyObject(module = "AccountsPayable", objectName = "TempException", objectId = AccountsPayableTempExceptionID, ignoreWarnings = ignoreWarnings, entityId = entityId, flatten = flatten, returnResponse = returnResponse)
	}

	#' Create an AccountsPayableTempException
	#'
	#' This function creates an AccountsPayableTempException
	#' @param fieldNames The field values to give the created AccountsPayableTempException. Each defaults to NULL.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{listSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Accounts Payable
	#' @return A newly created AccountsPayableTempException
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	createAccountsPayableTempException <- function(Message = NULL, LineNumber = NULL, IsFatal = NULL, entityId = 1, schoolYearId = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment())

		body <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper()) %>% compact()

		createSkyObject(module = "AccountsPayable", objectName = "TempException", body = list(DataObject = body), searchFields = append("TempExceptionID", body %>% names()), entityId = entityId, schoolYearId = schoolYearId, flatten = flatten, returnResponse = returnResponse)
	}

	#' Modify an AccountsPayableTempException
	#'
	#' This function modifies an AccountsPayableTempException
	#' @param fieldNames The field values to give the modified AccountsPayableTempException. Each defaults to NULL.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{listSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Accounts Payable
	#' @return The modified AccountsPayableTempException
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	modifyAccountsPayableTempException <- function(TempExceptionID, Message = NULL, LineNumber = NULL, IsFatal = NULL, entityId = 1, schoolYearId = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment())

		body <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper()) %>% compact()

		modifySkyObject(module = "AccountsPayable", objectName = "TempException", objectId = TempExceptionID, body = list(DataObject = body), searchFields = append("TempExceptionID", body %>% names()), entityId = entityId, schoolYearId = schoolYearId, flatten = flatten, returnResponse = returnResponse)
	}

	#' List VendorProfileCalendarYearTotals
	#'
	#' This function returns a dataframe or json object of VendorProfileCalendarYearTotals
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given VendorProfileCalendarYearTotals. Defaults to FALSE for all return fields which, for convenience, returns all fields for the VendorProfileCalendarYearTotals.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object listed as text. Run \code{\link{getSchemaForObjects}}('VendorProfileCalendarYearTotal') to get more field paths.
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
	#' @concept Accounts Payable
	#' @return A list of VendorProfileCalendarYearTotals
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	listVendorProfileCalendarYearTotals <- function(searchConditionsList = NULL, CheckTransactionIDFirst = F, VendorID = F, DistrictID = F, CalendarYear = F, PaymentsAmountTotal = F, PaymentsAmount1099 = F, fieldPaths = NULL, searchConditionsGroupType = "And", searchSortFieldNamesList = NULL, searchSortFieldNamesDescendingList = NULL, entityId = 1, schoolYearId = NULL, page = 1, pageSize = 100000, flatten = T, returnResponse = F){

		params <- as.list(environment())

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		listSkyObjects(module = "AccountsPayable", objectName = "VendorProfileCalendarYearTotal", searchFields = searchFields %>% append(fieldPaths), page = page, pageSize = pageSize, SearchConditionsList = searchConditionsList, SearchConditionsGroupType = searchConditionsGroupType, SearchSortFieldNamesList = searchSortFieldNamesList, SearchSortFieldNamesDescendingList = searchSortFieldNamesDescendingList, entityId = entityId, schoolYearId = schoolYearId, flatten = flatten, returnResponse = returnResponse)
	}

	#' Get a VendorProfileCalendarYearTotal
	#'
	#' This function returns a dataframe or json object of a VendorProfileCalendarYearTotal
	#' @param VendorProfileCalendarYearTotalID The ID of the VendorProfileCalendarYearTotal to return.
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given VendorProfileCalendarYearTotal. Defaults to FALSE for all return fields which, for convenience, returns all fields for the VendorProfileCalendarYearTotal.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object listed as text. Run \code{\link{getSchemaForObjects}}('VendorProfileCalendarYearTotal') to get more field paths.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{listSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Accounts Payable
	#' @return A dataframe or of VendorProfileCalendarYearTotal
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	getVendorProfileCalendarYearTotal <- function(VendorProfileCalendarYearTotalID, CheckTransactionIDFirst = F, VendorID = F, DistrictID = F, CalendarYear = F, PaymentsAmountTotal = F, PaymentsAmount1099 = F, fieldPaths = NULL, entityId = 1, schoolYearId = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment()) %>% purrr::keep(names(.) != "VendorProfileCalendarYearTotalID")

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		getSkyObject(module = "AccountsPayable", objectName = "VendorProfileCalendarYearTotal", objectId = VendorProfileCalendarYearTotalID, searchFields = searchFields, entityId = entityId, flatten = flatten, returnResponse = returnResponse)
	}

	#' Delete a VendorProfileCalendarYearTotal
	#'
	#' This function deletes a VendorProfileCalendarYearTotal
	#' @param VendorProfileCalendarYearTotalID The ID of the VendorProfileCalendarYearTotal to delete
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{listSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Accounts Payable
	#' @return The VendorProfileCalendarYearTotalID of the deleted VendorProfileCalendarYearTotal.
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	deleteVendorProfileCalendarYearTotal <- function(VendorProfileCalendarYearTotalID, ignoreWarnings = F, entityId = 1, schoolYearId = NULL, flatten = T, returnResponse = F){

		deleteSkyObject(module = "AccountsPayable", objectName = "VendorProfileCalendarYearTotal", objectId = VendorProfileCalendarYearTotalID, ignoreWarnings = ignoreWarnings, entityId = entityId, flatten = flatten, returnResponse = returnResponse)
	}

	#' Create a VendorProfileCalendarYearTotal
	#'
	#' This function creates a VendorProfileCalendarYearTotal
	#' @param fieldNames The field values to give the created VendorProfileCalendarYearTotal. Each defaults to NULL.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{listSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Accounts Payable
	#' @return A newly created VendorProfileCalendarYearTotal
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	createVendorProfileCalendarYearTotal <- function(VendorID = NULL, DistrictID = NULL, CalendarYear = NULL, PaymentsAmountTotal = NULL, PaymentsAmount1099 = NULL, entityId = 1, schoolYearId = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment())

		body <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper()) %>% compact()

		createSkyObject(module = "AccountsPayable", objectName = "VendorProfileCalendarYearTotal", body = list(DataObject = body), searchFields = append("VendorProfileCalendarYearTotalID", body %>% names()), entityId = entityId, schoolYearId = schoolYearId, flatten = flatten, returnResponse = returnResponse)
	}

	#' Modify a VendorProfileCalendarYearTotal
	#'
	#' This function modifies a VendorProfileCalendarYearTotal
	#' @param fieldNames The field values to give the modified VendorProfileCalendarYearTotal. Each defaults to NULL.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{listSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Accounts Payable
	#' @return The modified VendorProfileCalendarYearTotal
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	modifyVendorProfileCalendarYearTotal <- function(VendorProfileCalendarYearTotalID, VendorID = NULL, DistrictID = NULL, CalendarYear = NULL, PaymentsAmountTotal = NULL, PaymentsAmount1099 = NULL, entityId = 1, schoolYearId = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment())

		body <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper()) %>% compact()

		modifySkyObject(module = "AccountsPayable", objectName = "VendorProfileCalendarYearTotal", objectId = VendorProfileCalendarYearTotalID, body = list(DataObject = body), searchFields = append("VendorProfileCalendarYearTotalID", body %>% names()), entityId = entityId, schoolYearId = schoolYearId, flatten = flatten, returnResponse = returnResponse)
	}

	#' List TempInvoiceAccountings
	#'
	#' This function returns a dataframe or json object of TempInvoiceAccountings
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given TempInvoiceAccountings. Defaults to FALSE for all return fields which, for convenience, returns all fields for the TempInvoiceAccountings.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object listed as text. Run \code{\link{getSchemaForObjects}}('TempInvoiceAccounting') to get more field paths.
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
	#' @concept Accounts Payable
	#' @return A list of TempInvoiceAccountings
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	listTempInvoiceAccountings <- function(searchConditionsList = NULL, TempInvoiceAccountingID = F, TempInvoiceID = F, AccountID = F, Amount = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, fieldPaths = NULL, searchConditionsGroupType = "And", searchSortFieldNamesList = NULL, searchSortFieldNamesDescendingList = NULL, entityId = 1, schoolYearId = NULL, page = 1, pageSize = 100000, flatten = T, returnResponse = F){

		params <- as.list(environment())

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		listSkyObjects(module = "AccountsPayable", objectName = "TempInvoiceAccounting", searchFields = searchFields %>% append(fieldPaths), page = page, pageSize = pageSize, SearchConditionsList = searchConditionsList, SearchConditionsGroupType = searchConditionsGroupType, SearchSortFieldNamesList = searchSortFieldNamesList, SearchSortFieldNamesDescendingList = searchSortFieldNamesDescendingList, entityId = entityId, schoolYearId = schoolYearId, flatten = flatten, returnResponse = returnResponse)
	}

	#' Get a TempInvoiceAccounting
	#'
	#' This function returns a dataframe or json object of a TempInvoiceAccounting
	#' @param TempInvoiceAccountingID The ID of the TempInvoiceAccounting to return.
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given TempInvoiceAccounting. Defaults to FALSE for all return fields which, for convenience, returns all fields for the TempInvoiceAccounting.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object listed as text. Run \code{\link{getSchemaForObjects}}('TempInvoiceAccounting') to get more field paths.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{listSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Accounts Payable
	#' @return A dataframe or of TempInvoiceAccounting
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	getTempInvoiceAccounting <- function(TempInvoiceAccountingID, TempInvoiceID = F, AccountID = F, Amount = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, fieldPaths = NULL, entityId = 1, schoolYearId = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment()) %>% purrr::keep(names(.) != "TempInvoiceAccountingID")

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		getSkyObject(module = "AccountsPayable", objectName = "TempInvoiceAccounting", objectId = TempInvoiceAccountingID, searchFields = searchFields, entityId = entityId, flatten = flatten, returnResponse = returnResponse)
	}

	#' Delete a TempInvoiceAccounting
	#'
	#' This function deletes a TempInvoiceAccounting
	#' @param TempInvoiceAccountingID The ID of the TempInvoiceAccounting to delete
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{listSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Accounts Payable
	#' @return The TempInvoiceAccountingID of the deleted TempInvoiceAccounting.
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	deleteTempInvoiceAccounting <- function(TempInvoiceAccountingID, ignoreWarnings = F, entityId = 1, schoolYearId = NULL, flatten = T, returnResponse = F){

		deleteSkyObject(module = "AccountsPayable", objectName = "TempInvoiceAccounting", objectId = TempInvoiceAccountingID, ignoreWarnings = ignoreWarnings, entityId = entityId, flatten = flatten, returnResponse = returnResponse)
	}

	#' Create a TempInvoiceAccounting
	#'
	#' This function creates a TempInvoiceAccounting
	#' @param fieldNames The field values to give the created TempInvoiceAccounting. Each defaults to NULL.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{listSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Accounts Payable
	#' @return A newly created TempInvoiceAccounting
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	createTempInvoiceAccounting <- function(TempInvoiceID = NULL, AccountID = NULL, Amount = NULL, entityId = 1, schoolYearId = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment())

		body <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper()) %>% compact()

		createSkyObject(module = "AccountsPayable", objectName = "TempInvoiceAccounting", body = list(DataObject = body), searchFields = append("TempInvoiceAccountingID", body %>% names()), entityId = entityId, schoolYearId = schoolYearId, flatten = flatten, returnResponse = returnResponse)
	}

	#' Modify a TempInvoiceAccounting
	#'
	#' This function modifies a TempInvoiceAccounting
	#' @param fieldNames The field values to give the modified TempInvoiceAccounting. Each defaults to NULL.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{listSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Accounts Payable
	#' @return The modified TempInvoiceAccounting
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	modifyTempInvoiceAccounting <- function(TempInvoiceAccountingID, TempInvoiceID = NULL, AccountID = NULL, Amount = NULL, entityId = 1, schoolYearId = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment())

		body <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper()) %>% compact()

		modifySkyObject(module = "AccountsPayable", objectName = "TempInvoiceAccounting", objectId = TempInvoiceAccountingID, body = list(DataObject = body), searchFields = append("TempInvoiceAccountingID", body %>% names()), entityId = entityId, schoolYearId = schoolYearId, flatten = flatten, returnResponse = returnResponse)
	}

	#' List PaymentTerms
	#'
	#' This function returns a dataframe or json object of PaymentTerms
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given PaymentTerms. Defaults to FALSE for all return fields which, for convenience, returns all fields for the PaymentTerms.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object listed as text. Run \code{\link{getSchemaForObjects}}('PaymentTerms') to get more field paths.
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
	#' @concept Accounts Payable
	#' @return A list of PaymentTerms
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	listPaymentTerms <- function(searchConditionsList = NULL, PaymentTermsID = F, DistrictID = F, Code = F, Description = F, CodeDescription = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, DaysDue = F, BestDiscount = F, fieldPaths = NULL, searchConditionsGroupType = "And", searchSortFieldNamesList = NULL, searchSortFieldNamesDescendingList = NULL, entityId = 1, schoolYearId = NULL, page = 1, pageSize = 100000, flatten = T, returnResponse = F){

		params <- as.list(environment())

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		listSkyObjects(module = "AccountsPayable", objectName = "PaymentTerms", searchFields = searchFields %>% append(fieldPaths), page = page, pageSize = pageSize, SearchConditionsList = searchConditionsList, SearchConditionsGroupType = searchConditionsGroupType, SearchSortFieldNamesList = searchSortFieldNamesList, SearchSortFieldNamesDescendingList = searchSortFieldNamesDescendingList, entityId = entityId, schoolYearId = schoolYearId, flatten = flatten, returnResponse = returnResponse)
	}

	#' Get a PaymentTerms
	#'
	#' This function returns a dataframe or json object of a PaymentTerms
	#' @param PaymentTermsID The ID of the PaymentTerms to return.
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given PaymentTerms. Defaults to FALSE for all return fields which, for convenience, returns all fields for the PaymentTerms.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object listed as text. Run \code{\link{getSchemaForObjects}}('PaymentTerms') to get more field paths.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{listSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Accounts Payable
	#' @return A dataframe or of PaymentTerms
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	getPaymentTerms <- function(PaymentTermsID, DistrictID = F, Code = F, Description = F, CodeDescription = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, DaysDue = F, BestDiscount = F, fieldPaths = NULL, entityId = 1, schoolYearId = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment()) %>% purrr::keep(names(.) != "PaymentTermsID")

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		getSkyObject(module = "AccountsPayable", objectName = "PaymentTerms", objectId = PaymentTermsID, searchFields = searchFields, entityId = entityId, flatten = flatten, returnResponse = returnResponse)
	}

	#' Delete a PaymentTerms
	#'
	#' This function deletes a PaymentTerms
	#' @param PaymentTermsID The ID of the PaymentTerms to delete
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{listSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Accounts Payable
	#' @return The PaymentTermsID of the deleted PaymentTerms.
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	deletePaymentTerms <- function(PaymentTermsID, ignoreWarnings = F, entityId = 1, schoolYearId = NULL, flatten = T, returnResponse = F){

		deleteSkyObject(module = "AccountsPayable", objectName = "PaymentTerms", objectId = PaymentTermsID, ignoreWarnings = ignoreWarnings, entityId = entityId, flatten = flatten, returnResponse = returnResponse)
	}

	#' Create a PaymentTerms
	#'
	#' This function creates a PaymentTerms
	#' @param fieldNames The field values to give the created PaymentTerms. Each defaults to NULL.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{listSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Accounts Payable
	#' @return A newly created PaymentTerms
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	createPaymentTerms <- function(DistrictID = NULL, Code = NULL, Description = NULL, DaysDue = NULL, entityId = 1, schoolYearId = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment())

		body <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper()) %>% compact()

		createSkyObject(module = "AccountsPayable", objectName = "PaymentTerms", body = list(DataObject = body), searchFields = append("PaymentTermsID", body %>% names()), entityId = entityId, schoolYearId = schoolYearId, flatten = flatten, returnResponse = returnResponse)
	}

	#' Modify a PaymentTerms
	#'
	#' This function modifies a PaymentTerms
	#' @param fieldNames The field values to give the modified PaymentTerms. Each defaults to NULL.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{listSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Accounts Payable
	#' @return The modified PaymentTerms
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	modifyPaymentTerms <- function(PaymentTermsID, DistrictID = NULL, Code = NULL, Description = NULL, DaysDue = NULL, entityId = 1, schoolYearId = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment())

		body <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper()) %>% compact()

		modifySkyObject(module = "AccountsPayable", objectName = "PaymentTerms", objectId = PaymentTermsID, body = list(DataObject = body), searchFields = append("PaymentTermsID", body %>% names()), entityId = entityId, schoolYearId = schoolYearId, flatten = flatten, returnResponse = returnResponse)
	}

	#' List PaymentTermsDetails
	#'
	#' This function returns a dataframe or json object of PaymentTermsDetails
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given PaymentTermsDetails. Defaults to FALSE for all return fields which, for convenience, returns all fields for the PaymentTermsDetails.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object listed as text. Run \code{\link{getSchemaForObjects}}('PaymentTermsDetail') to get more field paths.
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
	#' @concept Accounts Payable
	#' @return A list of PaymentTermsDetails
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	listPaymentTermsDetails <- function(searchConditionsList = NULL, PaymentTermsDetailID = F, PaymentTermsID = F, DiscountDaysDue = F, DiscountPercent = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, fieldPaths = NULL, searchConditionsGroupType = "And", searchSortFieldNamesList = NULL, searchSortFieldNamesDescendingList = NULL, entityId = 1, schoolYearId = NULL, page = 1, pageSize = 100000, flatten = T, returnResponse = F){

		params <- as.list(environment())

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		listSkyObjects(module = "AccountsPayable", objectName = "PaymentTermsDetail", searchFields = searchFields %>% append(fieldPaths), page = page, pageSize = pageSize, SearchConditionsList = searchConditionsList, SearchConditionsGroupType = searchConditionsGroupType, SearchSortFieldNamesList = searchSortFieldNamesList, SearchSortFieldNamesDescendingList = searchSortFieldNamesDescendingList, entityId = entityId, schoolYearId = schoolYearId, flatten = flatten, returnResponse = returnResponse)
	}

	#' Get a PaymentTermsDetail
	#'
	#' This function returns a dataframe or json object of a PaymentTermsDetail
	#' @param PaymentTermsDetailID The ID of the PaymentTermsDetail to return.
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given PaymentTermsDetail. Defaults to FALSE for all return fields which, for convenience, returns all fields for the PaymentTermsDetail.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object listed as text. Run \code{\link{getSchemaForObjects}}('PaymentTermsDetail') to get more field paths.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{listSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Accounts Payable
	#' @return A dataframe or of PaymentTermsDetail
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	getPaymentTermsDetail <- function(PaymentTermsDetailID, PaymentTermsID = F, DiscountDaysDue = F, DiscountPercent = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, fieldPaths = NULL, entityId = 1, schoolYearId = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment()) %>% purrr::keep(names(.) != "PaymentTermsDetailID")

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		getSkyObject(module = "AccountsPayable", objectName = "PaymentTermsDetail", objectId = PaymentTermsDetailID, searchFields = searchFields, entityId = entityId, flatten = flatten, returnResponse = returnResponse)
	}

	#' Delete a PaymentTermsDetail
	#'
	#' This function deletes a PaymentTermsDetail
	#' @param PaymentTermsDetailID The ID of the PaymentTermsDetail to delete
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{listSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Accounts Payable
	#' @return The PaymentTermsDetailID of the deleted PaymentTermsDetail.
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	deletePaymentTermsDetail <- function(PaymentTermsDetailID, ignoreWarnings = F, entityId = 1, schoolYearId = NULL, flatten = T, returnResponse = F){

		deleteSkyObject(module = "AccountsPayable", objectName = "PaymentTermsDetail", objectId = PaymentTermsDetailID, ignoreWarnings = ignoreWarnings, entityId = entityId, flatten = flatten, returnResponse = returnResponse)
	}

	#' Create a PaymentTermsDetail
	#'
	#' This function creates a PaymentTermsDetail
	#' @param fieldNames The field values to give the created PaymentTermsDetail. Each defaults to NULL.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{listSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Accounts Payable
	#' @return A newly created PaymentTermsDetail
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	createPaymentTermsDetail <- function(PaymentTermsID = NULL, DiscountDaysDue = NULL, DiscountPercent = NULL, entityId = 1, schoolYearId = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment())

		body <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper()) %>% compact()

		createSkyObject(module = "AccountsPayable", objectName = "PaymentTermsDetail", body = list(DataObject = body), searchFields = append("PaymentTermsDetailID", body %>% names()), entityId = entityId, schoolYearId = schoolYearId, flatten = flatten, returnResponse = returnResponse)
	}

	#' Modify a PaymentTermsDetail
	#'
	#' This function modifies a PaymentTermsDetail
	#' @param fieldNames The field values to give the modified PaymentTermsDetail. Each defaults to NULL.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{listSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Accounts Payable
	#' @return The modified PaymentTermsDetail
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	modifyPaymentTermsDetail <- function(PaymentTermsDetailID, PaymentTermsID = NULL, DiscountDaysDue = NULL, DiscountPercent = NULL, entityId = 1, schoolYearId = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment())

		body <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper()) %>% compact()

		modifySkyObject(module = "AccountsPayable", objectName = "PaymentTermsDetail", objectId = PaymentTermsDetailID, body = list(DataObject = body), searchFields = append("PaymentTermsDetailID", body %>% names()), entityId = entityId, schoolYearId = schoolYearId, flatten = flatten, returnResponse = returnResponse)
	}

	#' List AccountsPayableReports
	#'
	#' This function returns a dataframe or json object of AccountsPayableReports
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given AccountsPayableReports. Defaults to FALSE for all return fields which, for convenience, returns all fields for the AccountsPayableReports.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object listed as text. Run \code{\link{getSchemaForObjects}}('AccountsPayableReport') to get more field paths.
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
	#' @concept Accounts Payable
	#' @return A list of AccountsPayableReports
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	listAccountsPayableReports <- function(searchConditionsList = NULL, AccountsPayableReportID = F, DistrictID = F, ReportRunInfoID = F, DisplayOrder = F, Section = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, fieldPaths = NULL, searchConditionsGroupType = "And", searchSortFieldNamesList = NULL, searchSortFieldNamesDescendingList = NULL, entityId = 1, schoolYearId = NULL, page = 1, pageSize = 100000, flatten = T, returnResponse = F){

		params <- as.list(environment())

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		listSkyObjects(module = "AccountsPayable", objectName = "AccountsPayableReport", searchFields = searchFields %>% append(fieldPaths), page = page, pageSize = pageSize, SearchConditionsList = searchConditionsList, SearchConditionsGroupType = searchConditionsGroupType, SearchSortFieldNamesList = searchSortFieldNamesList, SearchSortFieldNamesDescendingList = searchSortFieldNamesDescendingList, entityId = entityId, schoolYearId = schoolYearId, flatten = flatten, returnResponse = returnResponse)
	}

	#' Get an AccountsPayableReport
	#'
	#' This function returns a dataframe or json object of an AccountsPayableReport
	#' @param AccountsPayableReportID The ID of the AccountsPayableReport to return.
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given AccountsPayableReport. Defaults to FALSE for all return fields which, for convenience, returns all fields for the AccountsPayableReport.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object listed as text. Run \code{\link{getSchemaForObjects}}('AccountsPayableReport') to get more field paths.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{listSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Accounts Payable
	#' @return A dataframe or of AccountsPayableReport
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	getAccountsPayableReport <- function(AccountsPayableReportID, DistrictID = F, ReportRunInfoID = F, DisplayOrder = F, Section = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, fieldPaths = NULL, entityId = 1, schoolYearId = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment()) %>% purrr::keep(names(.) != "AccountsPayableReportID")

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		getSkyObject(module = "AccountsPayable", objectName = "AccountsPayableReport", objectId = AccountsPayableReportID, searchFields = searchFields, entityId = entityId, flatten = flatten, returnResponse = returnResponse)
	}

	#' Delete an AccountsPayableReport
	#'
	#' This function deletes an AccountsPayableReport
	#' @param AccountsPayableReportID The ID of the AccountsPayableReport to delete
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{listSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Accounts Payable
	#' @return The AccountsPayableReportID of the deleted AccountsPayableReport.
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	deleteAccountsPayableReport <- function(AccountsPayableReportID, ignoreWarnings = F, entityId = 1, schoolYearId = NULL, flatten = T, returnResponse = F){

		deleteSkyObject(module = "AccountsPayable", objectName = "AccountsPayableReport", objectId = AccountsPayableReportID, ignoreWarnings = ignoreWarnings, entityId = entityId, flatten = flatten, returnResponse = returnResponse)
	}

	#' Create an AccountsPayableReport
	#'
	#' This function creates an AccountsPayableReport
	#' @param fieldNames The field values to give the created AccountsPayableReport. Each defaults to NULL.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{listSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Accounts Payable
	#' @return A newly created AccountsPayableReport
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	createAccountsPayableReport <- function(DistrictID = NULL, ReportRunInfoID = NULL, DisplayOrder = NULL, Section = NULL, entityId = 1, schoolYearId = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment())

		body <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper()) %>% compact()

		createSkyObject(module = "AccountsPayable", objectName = "AccountsPayableReport", body = list(DataObject = body), searchFields = append("AccountsPayableReportID", body %>% names()), entityId = entityId, schoolYearId = schoolYearId, flatten = flatten, returnResponse = returnResponse)
	}

	#' Modify an AccountsPayableReport
	#'
	#' This function modifies an AccountsPayableReport
	#' @param fieldNames The field values to give the modified AccountsPayableReport. Each defaults to NULL.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{listSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Accounts Payable
	#' @return The modified AccountsPayableReport
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	modifyAccountsPayableReport <- function(AccountsPayableReportID, DistrictID = NULL, ReportRunInfoID = NULL, DisplayOrder = NULL, Section = NULL, entityId = 1, schoolYearId = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment())

		body <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper()) %>% compact()

		modifySkyObject(module = "AccountsPayable", objectName = "AccountsPayableReport", objectId = AccountsPayableReportID, body = list(DataObject = body), searchFields = append("AccountsPayableReportID", body %>% names()), entityId = entityId, schoolYearId = schoolYearId, flatten = flatten, returnResponse = returnResponse)
	}

	#' List CreditCardTransactionThirdPartyFormatAccounts
	#'
	#' This function returns a dataframe or json object of CreditCardTransactionThirdPartyFormatAccounts
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given CreditCardTransactionThirdPartyFormatAccounts. Defaults to FALSE for all return fields which, for convenience, returns all fields for the CreditCardTransactionThirdPartyFormatAccounts.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object listed as text. Run \code{\link{getSchemaForObjects}}('CreditCardTransactionThirdPartyFormatAccount') to get more field paths.
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
	#' @concept Accounts Payable
	#' @return A list of CreditCardTransactionThirdPartyFormatAccounts
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	listCreditCardTransactionThirdPartyFormatAccounts <- function(searchConditionsList = NULL, CreditCardTransactionThirdPartyFormatAccountID = F, CreditCardTransactionThirdPartyFormatID = F, AccountID = F, ImportValue = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, fieldPaths = NULL, searchConditionsGroupType = "And", searchSortFieldNamesList = NULL, searchSortFieldNamesDescendingList = NULL, entityId = 1, schoolYearId = NULL, page = 1, pageSize = 100000, flatten = T, returnResponse = F){

		params <- as.list(environment())

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		listSkyObjects(module = "AccountsPayable", objectName = "CreditCardTransactionThirdPartyFormatAccount", searchFields = searchFields %>% append(fieldPaths), page = page, pageSize = pageSize, SearchConditionsList = searchConditionsList, SearchConditionsGroupType = searchConditionsGroupType, SearchSortFieldNamesList = searchSortFieldNamesList, SearchSortFieldNamesDescendingList = searchSortFieldNamesDescendingList, entityId = entityId, schoolYearId = schoolYearId, flatten = flatten, returnResponse = returnResponse)
	}

	#' Get a CreditCardTransactionThirdPartyFormatAccount
	#'
	#' This function returns a dataframe or json object of a CreditCardTransactionThirdPartyFormatAccount
	#' @param CreditCardTransactionThirdPartyFormatAccountID The ID of the CreditCardTransactionThirdPartyFormatAccount to return.
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given CreditCardTransactionThirdPartyFormatAccount. Defaults to FALSE for all return fields which, for convenience, returns all fields for the CreditCardTransactionThirdPartyFormatAccount.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object listed as text. Run \code{\link{getSchemaForObjects}}('CreditCardTransactionThirdPartyFormatAccount') to get more field paths.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{listSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Accounts Payable
	#' @return A dataframe or of CreditCardTransactionThirdPartyFormatAccount
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	getCreditCardTransactionThirdPartyFormatAccount <- function(CreditCardTransactionThirdPartyFormatAccountID, CreditCardTransactionThirdPartyFormatID = F, AccountID = F, ImportValue = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, fieldPaths = NULL, entityId = 1, schoolYearId = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment()) %>% purrr::keep(names(.) != "CreditCardTransactionThirdPartyFormatAccountID")

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		getSkyObject(module = "AccountsPayable", objectName = "CreditCardTransactionThirdPartyFormatAccount", objectId = CreditCardTransactionThirdPartyFormatAccountID, searchFields = searchFields, entityId = entityId, flatten = flatten, returnResponse = returnResponse)
	}

	#' Delete a CreditCardTransactionThirdPartyFormatAccount
	#'
	#' This function deletes a CreditCardTransactionThirdPartyFormatAccount
	#' @param CreditCardTransactionThirdPartyFormatAccountID The ID of the CreditCardTransactionThirdPartyFormatAccount to delete
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{listSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Accounts Payable
	#' @return The CreditCardTransactionThirdPartyFormatAccountID of the deleted CreditCardTransactionThirdPartyFormatAccount.
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	deleteCreditCardTransactionThirdPartyFormatAccount <- function(CreditCardTransactionThirdPartyFormatAccountID, ignoreWarnings = F, entityId = 1, schoolYearId = NULL, flatten = T, returnResponse = F){

		deleteSkyObject(module = "AccountsPayable", objectName = "CreditCardTransactionThirdPartyFormatAccount", objectId = CreditCardTransactionThirdPartyFormatAccountID, ignoreWarnings = ignoreWarnings, entityId = entityId, flatten = flatten, returnResponse = returnResponse)
	}

	#' Create a CreditCardTransactionThirdPartyFormatAccount
	#'
	#' This function creates a CreditCardTransactionThirdPartyFormatAccount
	#' @param fieldNames The field values to give the created CreditCardTransactionThirdPartyFormatAccount. Each defaults to NULL.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{listSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Accounts Payable
	#' @return A newly created CreditCardTransactionThirdPartyFormatAccount
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	createCreditCardTransactionThirdPartyFormatAccount <- function(CreditCardTransactionThirdPartyFormatID = NULL, AccountID = NULL, ImportValue = NULL, entityId = 1, schoolYearId = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment())

		body <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper()) %>% compact()

		createSkyObject(module = "AccountsPayable", objectName = "CreditCardTransactionThirdPartyFormatAccount", body = list(DataObject = body), searchFields = append("CreditCardTransactionThirdPartyFormatAccountID", body %>% names()), entityId = entityId, schoolYearId = schoolYearId, flatten = flatten, returnResponse = returnResponse)
	}

	#' Modify a CreditCardTransactionThirdPartyFormatAccount
	#'
	#' This function modifies a CreditCardTransactionThirdPartyFormatAccount
	#' @param fieldNames The field values to give the modified CreditCardTransactionThirdPartyFormatAccount. Each defaults to NULL.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{listSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Accounts Payable
	#' @return The modified CreditCardTransactionThirdPartyFormatAccount
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	modifyCreditCardTransactionThirdPartyFormatAccount <- function(CreditCardTransactionThirdPartyFormatAccountID, CreditCardTransactionThirdPartyFormatID = NULL, AccountID = NULL, ImportValue = NULL, entityId = 1, schoolYearId = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment())

		body <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper()) %>% compact()

		modifySkyObject(module = "AccountsPayable", objectName = "CreditCardTransactionThirdPartyFormatAccount", objectId = CreditCardTransactionThirdPartyFormatAccountID, body = list(DataObject = body), searchFields = append("CreditCardTransactionThirdPartyFormatAccountID", body %>% names()), entityId = entityId, schoolYearId = schoolYearId, flatten = flatten, returnResponse = returnResponse)
	}
