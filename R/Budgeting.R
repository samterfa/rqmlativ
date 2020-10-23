
	#' List TempMassChangeBudgetVersionAccounts
	#'
	#' This function returns a dataframe or json object of TempMassChangeBudgetVersionAccounts
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given TempMassChangeBudgetVersionAccounts. Defaults to FALSE for all return fields which, for convenience, returns all fields for the TempMassChangeBudgetVersionAccounts.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object listed as text. Run \code{\link{getSchemaForObjects}}('TempMassChangeBudgetVersionAccount') to get more field paths.
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
	#' @concept Budgeting
	#' @return A list of TempMassChangeBudgetVersionAccounts
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	listTempMassChangeBudgetVersionAccounts <- function(searchConditionsList = NULL, TempMassChangeBudgetVersionAccountID = F, BudgetVersionAccountID = F, DisplayAccount = F, BudgetedAmount = F, ProposedAmount = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, BudgetVersionAccountDetailIDToUpdate = F, BudgetVersionAccountDetailIDToDelete = F, HasError = F, fieldPaths = NULL, searchConditionsGroupType = "And", searchSortFieldNamesList = NULL, searchSortFieldNamesDescendingList = NULL, entityId = 1, schoolYearId = NULL, page = 1, pageSize = 100000, flatten = T, returnResponse = F){

		params <- as.list(environment())

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		listSkyObjects(module = "Budgeting", objectName = "TempMassChangeBudgetVersionAccount", searchFields = searchFields %>% append(fieldPaths), page = page, pageSize = pageSize, SearchConditionsList = searchConditionsList, SearchConditionsGroupType = searchConditionsGroupType, SearchSortFieldNamesList = searchSortFieldNamesList, SearchSortFieldNamesDescendingList = searchSortFieldNamesDescendingList, entityId = entityId, schoolYearId = schoolYearId, flatten = flatten, returnResponse = returnResponse)
	}

	#' Get a TempMassChangeBudgetVersionAccount
	#'
	#' This function returns a dataframe or json object of a TempMassChangeBudgetVersionAccount
	#' @param TempMassChangeBudgetVersionAccountID The ID of the TempMassChangeBudgetVersionAccount to return.
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given TempMassChangeBudgetVersionAccount. Defaults to FALSE for all return fields which, for convenience, returns all fields for the TempMassChangeBudgetVersionAccount.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object listed as text. Run \code{\link{getSchemaForObjects}}('TempMassChangeBudgetVersionAccount') to get more field paths.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{listSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Budgeting
	#' @return A dataframe or of TempMassChangeBudgetVersionAccount
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	getTempMassChangeBudgetVersionAccount <- function(TempMassChangeBudgetVersionAccountID, BudgetVersionAccountID = F, DisplayAccount = F, BudgetedAmount = F, ProposedAmount = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, BudgetVersionAccountDetailIDToUpdate = F, BudgetVersionAccountDetailIDToDelete = F, HasError = F, fieldPaths = NULL, entityId = 1, schoolYearId = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment()) %>% purrr::keep(names(.) != "TempMassChangeBudgetVersionAccountID")

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		getSkyObject(module = "Budgeting", objectName = "TempMassChangeBudgetVersionAccount", objectId = TempMassChangeBudgetVersionAccountID, searchFields = searchFields, entityId = entityId, flatten = flatten, returnResponse = returnResponse)
	}

	#' Delete a TempMassChangeBudgetVersionAccount
	#'
	#' This function deletes a TempMassChangeBudgetVersionAccount
	#' @param TempMassChangeBudgetVersionAccountID The ID of the TempMassChangeBudgetVersionAccount to delete
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{listSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Budgeting
	#' @return The TempMassChangeBudgetVersionAccountID of the deleted TempMassChangeBudgetVersionAccount.
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	deleteTempMassChangeBudgetVersionAccount <- function(TempMassChangeBudgetVersionAccountID, ignoreWarnings = F, entityId = 1, schoolYearId = NULL, flatten = T, returnResponse = F){

		deleteSkyObject(module = "Budgeting", objectName = "TempMassChangeBudgetVersionAccount", objectId = TempMassChangeBudgetVersionAccountID, ignoreWarnings = ignoreWarnings, entityId = entityId, flatten = flatten, returnResponse = returnResponse)
	}

	#' Create a TempMassChangeBudgetVersionAccount
	#'
	#' This function creates a TempMassChangeBudgetVersionAccount
	#' @param fieldNames The field values to give the created TempMassChangeBudgetVersionAccount. Each defaults to NULL.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{listSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Budgeting
	#' @return A newly created TempMassChangeBudgetVersionAccount
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	createTempMassChangeBudgetVersionAccount <- function(BudgetVersionAccountID = NULL, DisplayAccount = NULL, BudgetedAmount = NULL, ProposedAmount = NULL, BudgetVersionAccountDetailIDToUpdate = NULL, BudgetVersionAccountDetailIDToDelete = NULL, HasError = NULL, entityId = 1, schoolYearId = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment())

		body <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper()) %>% purrr::compact()

		createSkyObject(module = "Budgeting", objectName = "TempMassChangeBudgetVersionAccount", body = list(DataObject = body), searchFields = append("TempMassChangeBudgetVersionAccountID", body %>% names()), entityId = entityId, schoolYearId = schoolYearId, flatten = flatten, returnResponse = returnResponse)
	}

	#' Modify a TempMassChangeBudgetVersionAccount
	#'
	#' This function modifies a TempMassChangeBudgetVersionAccount
	#' @param fieldNames The field values to give the modified TempMassChangeBudgetVersionAccount. Each defaults to NULL.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{listSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Budgeting
	#' @return The modified TempMassChangeBudgetVersionAccount
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	modifyTempMassChangeBudgetVersionAccount <- function(TempMassChangeBudgetVersionAccountID, BudgetVersionAccountID = NULL, DisplayAccount = NULL, BudgetedAmount = NULL, ProposedAmount = NULL, BudgetVersionAccountDetailIDToUpdate = NULL, BudgetVersionAccountDetailIDToDelete = NULL, HasError = NULL, entityId = 1, schoolYearId = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment())

		body <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper()) %>% purrr::compact()

		modifySkyObject(module = "Budgeting", objectName = "TempMassChangeBudgetVersionAccount", objectId = TempMassChangeBudgetVersionAccountID, body = list(DataObject = body), searchFields = append("TempMassChangeBudgetVersionAccountID", body %>% names()), entityId = entityId, schoolYearId = schoolYearId, flatten = flatten, returnResponse = returnResponse)
	}

	#' List BudgetAmendmentApprovals
	#'
	#' This function returns a dataframe or json object of BudgetAmendmentApprovals
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given BudgetAmendmentApprovals. Defaults to FALSE for all return fields which, for convenience, returns all fields for the BudgetAmendmentApprovals.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object listed as text. Run \code{\link{getSchemaForObjects}}('BudgetAmendmentApproval') to get more field paths.
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
	#' @concept Budgeting
	#' @return A list of BudgetAmendmentApprovals
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	listBudgetAmendmentApprovals <- function(searchConditionsList = NULL, BudgetAmendmentApprovalID = F, BudgetAmendmentID = F, UserIDApprover = F, Comment = F, TaskInstanceID = F, Status = F, Level = F, LevelDescription = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, ApprovalActionTime = F, fieldPaths = NULL, searchConditionsGroupType = "And", searchSortFieldNamesList = NULL, searchSortFieldNamesDescendingList = NULL, entityId = 1, schoolYearId = NULL, page = 1, pageSize = 100000, flatten = T, returnResponse = F){

		params <- as.list(environment())

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		listSkyObjects(module = "Budgeting", objectName = "BudgetAmendmentApproval", searchFields = searchFields %>% append(fieldPaths), page = page, pageSize = pageSize, SearchConditionsList = searchConditionsList, SearchConditionsGroupType = searchConditionsGroupType, SearchSortFieldNamesList = searchSortFieldNamesList, SearchSortFieldNamesDescendingList = searchSortFieldNamesDescendingList, entityId = entityId, schoolYearId = schoolYearId, flatten = flatten, returnResponse = returnResponse)
	}

	#' Get a BudgetAmendmentApproval
	#'
	#' This function returns a dataframe or json object of a BudgetAmendmentApproval
	#' @param BudgetAmendmentApprovalID The ID of the BudgetAmendmentApproval to return.
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given BudgetAmendmentApproval. Defaults to FALSE for all return fields which, for convenience, returns all fields for the BudgetAmendmentApproval.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object listed as text. Run \code{\link{getSchemaForObjects}}('BudgetAmendmentApproval') to get more field paths.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{listSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Budgeting
	#' @return A dataframe or of BudgetAmendmentApproval
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	getBudgetAmendmentApproval <- function(BudgetAmendmentApprovalID, BudgetAmendmentID = F, UserIDApprover = F, Comment = F, TaskInstanceID = F, Status = F, Level = F, LevelDescription = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, ApprovalActionTime = F, fieldPaths = NULL, entityId = 1, schoolYearId = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment()) %>% purrr::keep(names(.) != "BudgetAmendmentApprovalID")

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		getSkyObject(module = "Budgeting", objectName = "BudgetAmendmentApproval", objectId = BudgetAmendmentApprovalID, searchFields = searchFields, entityId = entityId, flatten = flatten, returnResponse = returnResponse)
	}

	#' Delete a BudgetAmendmentApproval
	#'
	#' This function deletes a BudgetAmendmentApproval
	#' @param BudgetAmendmentApprovalID The ID of the BudgetAmendmentApproval to delete
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{listSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Budgeting
	#' @return The BudgetAmendmentApprovalID of the deleted BudgetAmendmentApproval.
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	deleteBudgetAmendmentApproval <- function(BudgetAmendmentApprovalID, ignoreWarnings = F, entityId = 1, schoolYearId = NULL, flatten = T, returnResponse = F){

		deleteSkyObject(module = "Budgeting", objectName = "BudgetAmendmentApproval", objectId = BudgetAmendmentApprovalID, ignoreWarnings = ignoreWarnings, entityId = entityId, flatten = flatten, returnResponse = returnResponse)
	}

	#' Create a BudgetAmendmentApproval
	#'
	#' This function creates a BudgetAmendmentApproval
	#' @param fieldNames The field values to give the created BudgetAmendmentApproval. Each defaults to NULL.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{listSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Budgeting
	#' @return A newly created BudgetAmendmentApproval
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	createBudgetAmendmentApproval <- function(BudgetAmendmentID = NULL, UserIDApprover = NULL, Comment = NULL, TaskInstanceID = NULL, Status = NULL, Level = NULL, LevelDescription = NULL, ApprovalActionTime = NULL, entityId = 1, schoolYearId = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment())

		body <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper()) %>% purrr::compact()

		createSkyObject(module = "Budgeting", objectName = "BudgetAmendmentApproval", body = list(DataObject = body), searchFields = append("BudgetAmendmentApprovalID", body %>% names()), entityId = entityId, schoolYearId = schoolYearId, flatten = flatten, returnResponse = returnResponse)
	}

	#' Modify a BudgetAmendmentApproval
	#'
	#' This function modifies a BudgetAmendmentApproval
	#' @param fieldNames The field values to give the modified BudgetAmendmentApproval. Each defaults to NULL.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{listSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Budgeting
	#' @return The modified BudgetAmendmentApproval
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	modifyBudgetAmendmentApproval <- function(BudgetAmendmentApprovalID, BudgetAmendmentID = NULL, UserIDApprover = NULL, Comment = NULL, TaskInstanceID = NULL, Status = NULL, Level = NULL, LevelDescription = NULL, ApprovalActionTime = NULL, entityId = 1, schoolYearId = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment())

		body <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper()) %>% purrr::compact()

		modifySkyObject(module = "Budgeting", objectName = "BudgetAmendmentApproval", objectId = BudgetAmendmentApprovalID, body = list(DataObject = body), searchFields = append("BudgetAmendmentApprovalID", body %>% names()), entityId = entityId, schoolYearId = schoolYearId, flatten = flatten, returnResponse = returnResponse)
	}

	#' List BudgetAmendmentGroupApprovalTasks
	#'
	#' This function returns a dataframe or json object of BudgetAmendmentGroupApprovalTasks
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given BudgetAmendmentGroupApprovalTasks. Defaults to FALSE for all return fields which, for convenience, returns all fields for the BudgetAmendmentGroupApprovalTasks.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object listed as text. Run \code{\link{getSchemaForObjects}}('BudgetAmendmentGroupApprovalTask') to get more field paths.
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
	#' @concept Budgeting
	#' @return A list of BudgetAmendmentGroupApprovalTasks
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	listBudgetAmendmentGroupApprovalTasks <- function(searchConditionsList = NULL, BudgetAmendmentGroupApprovalTaskID = F, BudgetAmendmentGroupID = F, IsConditional = F, Level = F, Description = F, XMLFilter = F, SecurityStandardFilterData = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, fieldPaths = NULL, searchConditionsGroupType = "And", searchSortFieldNamesList = NULL, searchSortFieldNamesDescendingList = NULL, entityId = 1, schoolYearId = NULL, page = 1, pageSize = 100000, flatten = T, returnResponse = F){

		params <- as.list(environment())

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		listSkyObjects(module = "Budgeting", objectName = "BudgetAmendmentGroupApprovalTask", searchFields = searchFields %>% append(fieldPaths), page = page, pageSize = pageSize, SearchConditionsList = searchConditionsList, SearchConditionsGroupType = searchConditionsGroupType, SearchSortFieldNamesList = searchSortFieldNamesList, SearchSortFieldNamesDescendingList = searchSortFieldNamesDescendingList, entityId = entityId, schoolYearId = schoolYearId, flatten = flatten, returnResponse = returnResponse)
	}

	#' Get a BudgetAmendmentGroupApprovalTask
	#'
	#' This function returns a dataframe or json object of a BudgetAmendmentGroupApprovalTask
	#' @param BudgetAmendmentGroupApprovalTaskID The ID of the BudgetAmendmentGroupApprovalTask to return.
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given BudgetAmendmentGroupApprovalTask. Defaults to FALSE for all return fields which, for convenience, returns all fields for the BudgetAmendmentGroupApprovalTask.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object listed as text. Run \code{\link{getSchemaForObjects}}('BudgetAmendmentGroupApprovalTask') to get more field paths.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{listSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Budgeting
	#' @return A dataframe or of BudgetAmendmentGroupApprovalTask
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	getBudgetAmendmentGroupApprovalTask <- function(BudgetAmendmentGroupApprovalTaskID, BudgetAmendmentGroupID = F, IsConditional = F, Level = F, Description = F, XMLFilter = F, SecurityStandardFilterData = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, fieldPaths = NULL, entityId = 1, schoolYearId = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment()) %>% purrr::keep(names(.) != "BudgetAmendmentGroupApprovalTaskID")

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		getSkyObject(module = "Budgeting", objectName = "BudgetAmendmentGroupApprovalTask", objectId = BudgetAmendmentGroupApprovalTaskID, searchFields = searchFields, entityId = entityId, flatten = flatten, returnResponse = returnResponse)
	}

	#' Delete a BudgetAmendmentGroupApprovalTask
	#'
	#' This function deletes a BudgetAmendmentGroupApprovalTask
	#' @param BudgetAmendmentGroupApprovalTaskID The ID of the BudgetAmendmentGroupApprovalTask to delete
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{listSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Budgeting
	#' @return The BudgetAmendmentGroupApprovalTaskID of the deleted BudgetAmendmentGroupApprovalTask.
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	deleteBudgetAmendmentGroupApprovalTask <- function(BudgetAmendmentGroupApprovalTaskID, ignoreWarnings = F, entityId = 1, schoolYearId = NULL, flatten = T, returnResponse = F){

		deleteSkyObject(module = "Budgeting", objectName = "BudgetAmendmentGroupApprovalTask", objectId = BudgetAmendmentGroupApprovalTaskID, ignoreWarnings = ignoreWarnings, entityId = entityId, flatten = flatten, returnResponse = returnResponse)
	}

	#' Create a BudgetAmendmentGroupApprovalTask
	#'
	#' This function creates a BudgetAmendmentGroupApprovalTask
	#' @param fieldNames The field values to give the created BudgetAmendmentGroupApprovalTask. Each defaults to NULL.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{listSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Budgeting
	#' @return A newly created BudgetAmendmentGroupApprovalTask
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	createBudgetAmendmentGroupApprovalTask <- function(BudgetAmendmentGroupID = NULL, IsConditional = NULL, Level = NULL, Description = NULL, XMLFilter = NULL, entityId = 1, schoolYearId = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment())

		body <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper()) %>% purrr::compact()

		createSkyObject(module = "Budgeting", objectName = "BudgetAmendmentGroupApprovalTask", body = list(DataObject = body), searchFields = append("BudgetAmendmentGroupApprovalTaskID", body %>% names()), entityId = entityId, schoolYearId = schoolYearId, flatten = flatten, returnResponse = returnResponse)
	}

	#' Modify a BudgetAmendmentGroupApprovalTask
	#'
	#' This function modifies a BudgetAmendmentGroupApprovalTask
	#' @param fieldNames The field values to give the modified BudgetAmendmentGroupApprovalTask. Each defaults to NULL.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{listSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Budgeting
	#' @return The modified BudgetAmendmentGroupApprovalTask
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	modifyBudgetAmendmentGroupApprovalTask <- function(BudgetAmendmentGroupApprovalTaskID, BudgetAmendmentGroupID = NULL, IsConditional = NULL, Level = NULL, Description = NULL, XMLFilter = NULL, entityId = 1, schoolYearId = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment())

		body <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper()) %>% purrr::compact()

		modifySkyObject(module = "Budgeting", objectName = "BudgetAmendmentGroupApprovalTask", objectId = BudgetAmendmentGroupApprovalTaskID, body = list(DataObject = body), searchFields = append("BudgetAmendmentGroupApprovalTaskID", body %>% names()), entityId = entityId, schoolYearId = schoolYearId, flatten = flatten, returnResponse = returnResponse)
	}

	#' List BudgetAmendmentGroupApprovalTaskSecurityGroups
	#'
	#' This function returns a dataframe or json object of BudgetAmendmentGroupApprovalTaskSecurityGroups
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given BudgetAmendmentGroupApprovalTaskSecurityGroups. Defaults to FALSE for all return fields which, for convenience, returns all fields for the BudgetAmendmentGroupApprovalTaskSecurityGroups.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object listed as text. Run \code{\link{getSchemaForObjects}}('BudgetAmendmentGroupApprovalTaskSecurityGroup') to get more field paths.
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
	#' @concept Budgeting
	#' @return A list of BudgetAmendmentGroupApprovalTaskSecurityGroups
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	listBudgetAmendmentGroupApprovalTaskSecurityGroups <- function(searchConditionsList = NULL, BudgetAmendmentGroupApprovalTaskSecurityGroupID = F, BudgetAmendmentGroupApprovalTaskID = F, GroupIDSecurity = F, IsAlternate = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, fieldPaths = NULL, searchConditionsGroupType = "And", searchSortFieldNamesList = NULL, searchSortFieldNamesDescendingList = NULL, entityId = 1, schoolYearId = NULL, page = 1, pageSize = 100000, flatten = T, returnResponse = F){

		params <- as.list(environment())

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		listSkyObjects(module = "Budgeting", objectName = "BudgetAmendmentGroupApprovalTaskSecurityGroup", searchFields = searchFields %>% append(fieldPaths), page = page, pageSize = pageSize, SearchConditionsList = searchConditionsList, SearchConditionsGroupType = searchConditionsGroupType, SearchSortFieldNamesList = searchSortFieldNamesList, SearchSortFieldNamesDescendingList = searchSortFieldNamesDescendingList, entityId = entityId, schoolYearId = schoolYearId, flatten = flatten, returnResponse = returnResponse)
	}

	#' Get a BudgetAmendmentGroupApprovalTaskSecurityGroup
	#'
	#' This function returns a dataframe or json object of a BudgetAmendmentGroupApprovalTaskSecurityGroup
	#' @param BudgetAmendmentGroupApprovalTaskSecurityGroupID The ID of the BudgetAmendmentGroupApprovalTaskSecurityGroup to return.
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given BudgetAmendmentGroupApprovalTaskSecurityGroup. Defaults to FALSE for all return fields which, for convenience, returns all fields for the BudgetAmendmentGroupApprovalTaskSecurityGroup.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object listed as text. Run \code{\link{getSchemaForObjects}}('BudgetAmendmentGroupApprovalTaskSecurityGroup') to get more field paths.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{listSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Budgeting
	#' @return A dataframe or of BudgetAmendmentGroupApprovalTaskSecurityGroup
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	getBudgetAmendmentGroupApprovalTaskSecurityGroup <- function(BudgetAmendmentGroupApprovalTaskSecurityGroupID, BudgetAmendmentGroupApprovalTaskID = F, GroupIDSecurity = F, IsAlternate = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, fieldPaths = NULL, entityId = 1, schoolYearId = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment()) %>% purrr::keep(names(.) != "BudgetAmendmentGroupApprovalTaskSecurityGroupID")

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		getSkyObject(module = "Budgeting", objectName = "BudgetAmendmentGroupApprovalTaskSecurityGroup", objectId = BudgetAmendmentGroupApprovalTaskSecurityGroupID, searchFields = searchFields, entityId = entityId, flatten = flatten, returnResponse = returnResponse)
	}

	#' Delete a BudgetAmendmentGroupApprovalTaskSecurityGroup
	#'
	#' This function deletes a BudgetAmendmentGroupApprovalTaskSecurityGroup
	#' @param BudgetAmendmentGroupApprovalTaskSecurityGroupID The ID of the BudgetAmendmentGroupApprovalTaskSecurityGroup to delete
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{listSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Budgeting
	#' @return The BudgetAmendmentGroupApprovalTaskSecurityGroupID of the deleted BudgetAmendmentGroupApprovalTaskSecurityGroup.
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	deleteBudgetAmendmentGroupApprovalTaskSecurityGroup <- function(BudgetAmendmentGroupApprovalTaskSecurityGroupID, ignoreWarnings = F, entityId = 1, schoolYearId = NULL, flatten = T, returnResponse = F){

		deleteSkyObject(module = "Budgeting", objectName = "BudgetAmendmentGroupApprovalTaskSecurityGroup", objectId = BudgetAmendmentGroupApprovalTaskSecurityGroupID, ignoreWarnings = ignoreWarnings, entityId = entityId, flatten = flatten, returnResponse = returnResponse)
	}

	#' Create a BudgetAmendmentGroupApprovalTaskSecurityGroup
	#'
	#' This function creates a BudgetAmendmentGroupApprovalTaskSecurityGroup
	#' @param fieldNames The field values to give the created BudgetAmendmentGroupApprovalTaskSecurityGroup. Each defaults to NULL.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{listSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Budgeting
	#' @return A newly created BudgetAmendmentGroupApprovalTaskSecurityGroup
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	createBudgetAmendmentGroupApprovalTaskSecurityGroup <- function(BudgetAmendmentGroupApprovalTaskID = NULL, GroupIDSecurity = NULL, IsAlternate = NULL, entityId = 1, schoolYearId = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment())

		body <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper()) %>% purrr::compact()

		createSkyObject(module = "Budgeting", objectName = "BudgetAmendmentGroupApprovalTaskSecurityGroup", body = list(DataObject = body), searchFields = append("BudgetAmendmentGroupApprovalTaskSecurityGroupID", body %>% names()), entityId = entityId, schoolYearId = schoolYearId, flatten = flatten, returnResponse = returnResponse)
	}

	#' Modify a BudgetAmendmentGroupApprovalTaskSecurityGroup
	#'
	#' This function modifies a BudgetAmendmentGroupApprovalTaskSecurityGroup
	#' @param fieldNames The field values to give the modified BudgetAmendmentGroupApprovalTaskSecurityGroup. Each defaults to NULL.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{listSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Budgeting
	#' @return The modified BudgetAmendmentGroupApprovalTaskSecurityGroup
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	modifyBudgetAmendmentGroupApprovalTaskSecurityGroup <- function(BudgetAmendmentGroupApprovalTaskSecurityGroupID, BudgetAmendmentGroupApprovalTaskID = NULL, GroupIDSecurity = NULL, IsAlternate = NULL, entityId = 1, schoolYearId = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment())

		body <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper()) %>% purrr::compact()

		modifySkyObject(module = "Budgeting", objectName = "BudgetAmendmentGroupApprovalTaskSecurityGroup", objectId = BudgetAmendmentGroupApprovalTaskSecurityGroupID, body = list(DataObject = body), searchFields = append("BudgetAmendmentGroupApprovalTaskSecurityGroupID", body %>% names()), entityId = entityId, schoolYearId = schoolYearId, flatten = flatten, returnResponse = returnResponse)
	}

	#' List TempAdvanceBudgetVersions
	#'
	#' This function returns a dataframe or json object of TempAdvanceBudgetVersions
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given TempAdvanceBudgetVersions. Defaults to FALSE for all return fields which, for convenience, returns all fields for the TempAdvanceBudgetVersions.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object listed as text. Run \code{\link{getSchemaForObjects}}('TempAdvanceBudgetVersion') to get more field paths.
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
	#' @concept Budgeting
	#' @return A list of TempAdvanceBudgetVersions
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	listTempAdvanceBudgetVersions <- function(searchConditionsList = NULL, TempAdvanceBudgetVersionID = F, BudgetVersionAccountID = F, AccountID = F, DisplayAccount = F, Amount = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, DependentStaffPlanningAccount = F, ExceptionMessage = F, Status = F, fieldPaths = NULL, searchConditionsGroupType = "And", searchSortFieldNamesList = NULL, searchSortFieldNamesDescendingList = NULL, entityId = 1, schoolYearId = NULL, page = 1, pageSize = 100000, flatten = T, returnResponse = F){

		params <- as.list(environment())

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		listSkyObjects(module = "Budgeting", objectName = "TempAdvanceBudgetVersion", searchFields = searchFields %>% append(fieldPaths), page = page, pageSize = pageSize, SearchConditionsList = searchConditionsList, SearchConditionsGroupType = searchConditionsGroupType, SearchSortFieldNamesList = searchSortFieldNamesList, SearchSortFieldNamesDescendingList = searchSortFieldNamesDescendingList, entityId = entityId, schoolYearId = schoolYearId, flatten = flatten, returnResponse = returnResponse)
	}

	#' Get a TempAdvanceBudgetVersion
	#'
	#' This function returns a dataframe or json object of a TempAdvanceBudgetVersion
	#' @param TempAdvanceBudgetVersionID The ID of the TempAdvanceBudgetVersion to return.
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given TempAdvanceBudgetVersion. Defaults to FALSE for all return fields which, for convenience, returns all fields for the TempAdvanceBudgetVersion.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object listed as text. Run \code{\link{getSchemaForObjects}}('TempAdvanceBudgetVersion') to get more field paths.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{listSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Budgeting
	#' @return A dataframe or of TempAdvanceBudgetVersion
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	getTempAdvanceBudgetVersion <- function(TempAdvanceBudgetVersionID, BudgetVersionAccountID = F, AccountID = F, DisplayAccount = F, Amount = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, DependentStaffPlanningAccount = F, ExceptionMessage = F, Status = F, fieldPaths = NULL, entityId = 1, schoolYearId = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment()) %>% purrr::keep(names(.) != "TempAdvanceBudgetVersionID")

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		getSkyObject(module = "Budgeting", objectName = "TempAdvanceBudgetVersion", objectId = TempAdvanceBudgetVersionID, searchFields = searchFields, entityId = entityId, flatten = flatten, returnResponse = returnResponse)
	}

	#' Delete a TempAdvanceBudgetVersion
	#'
	#' This function deletes a TempAdvanceBudgetVersion
	#' @param TempAdvanceBudgetVersionID The ID of the TempAdvanceBudgetVersion to delete
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{listSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Budgeting
	#' @return The TempAdvanceBudgetVersionID of the deleted TempAdvanceBudgetVersion.
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	deleteTempAdvanceBudgetVersion <- function(TempAdvanceBudgetVersionID, ignoreWarnings = F, entityId = 1, schoolYearId = NULL, flatten = T, returnResponse = F){

		deleteSkyObject(module = "Budgeting", objectName = "TempAdvanceBudgetVersion", objectId = TempAdvanceBudgetVersionID, ignoreWarnings = ignoreWarnings, entityId = entityId, flatten = flatten, returnResponse = returnResponse)
	}

	#' Create a TempAdvanceBudgetVersion
	#'
	#' This function creates a TempAdvanceBudgetVersion
	#' @param fieldNames The field values to give the created TempAdvanceBudgetVersion. Each defaults to NULL.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{listSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Budgeting
	#' @return A newly created TempAdvanceBudgetVersion
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	createTempAdvanceBudgetVersion <- function(BudgetVersionAccountID = NULL, AccountID = NULL, DisplayAccount = NULL, Amount = NULL, DependentStaffPlanningAccount = NULL, ExceptionMessage = NULL, Status = NULL, entityId = 1, schoolYearId = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment())

		body <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper()) %>% purrr::compact()

		createSkyObject(module = "Budgeting", objectName = "TempAdvanceBudgetVersion", body = list(DataObject = body), searchFields = append("TempAdvanceBudgetVersionID", body %>% names()), entityId = entityId, schoolYearId = schoolYearId, flatten = flatten, returnResponse = returnResponse)
	}

	#' Modify a TempAdvanceBudgetVersion
	#'
	#' This function modifies a TempAdvanceBudgetVersion
	#' @param fieldNames The field values to give the modified TempAdvanceBudgetVersion. Each defaults to NULL.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{listSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Budgeting
	#' @return The modified TempAdvanceBudgetVersion
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	modifyTempAdvanceBudgetVersion <- function(TempAdvanceBudgetVersionID, BudgetVersionAccountID = NULL, AccountID = NULL, DisplayAccount = NULL, Amount = NULL, DependentStaffPlanningAccount = NULL, ExceptionMessage = NULL, Status = NULL, entityId = 1, schoolYearId = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment())

		body <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper()) %>% purrr::compact()

		modifySkyObject(module = "Budgeting", objectName = "TempAdvanceBudgetVersion", objectId = TempAdvanceBudgetVersionID, body = list(DataObject = body), searchFields = append("TempAdvanceBudgetVersionID", body %>% names()), entityId = entityId, schoolYearId = schoolYearId, flatten = flatten, returnResponse = returnResponse)
	}

	#' List BudgetVersionAccounts
	#'
	#' This function returns a dataframe or json object of BudgetVersionAccounts
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given BudgetVersionAccounts. Defaults to FALSE for all return fields which, for convenience, returns all fields for the BudgetVersionAccounts.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object listed as text. Run \code{\link{getSchemaForObjects}}('BudgetVersionAccount') to get more field paths.
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
	#' @concept Budgeting
	#' @return A list of BudgetVersionAccounts
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	listBudgetVersionAccounts <- function(searchConditionsList = NULL, BudgetVersionAccountID = F, BudgetVersionID = F, AccountID = F, BudgetVersionAccountAmount = F, IsOpenAccount = F, IsNotOpenAccount = F, IsAdoptedAccount = F, CurrentUserHasBudgetVersionClearance = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, Comment = F, fieldPaths = NULL, searchConditionsGroupType = "And", searchSortFieldNamesList = NULL, searchSortFieldNamesDescendingList = NULL, entityId = 1, schoolYearId = NULL, page = 1, pageSize = 100000, flatten = T, returnResponse = F){

		params <- as.list(environment())

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		listSkyObjects(module = "Budgeting", objectName = "BudgetVersionAccount", searchFields = searchFields %>% append(fieldPaths), page = page, pageSize = pageSize, SearchConditionsList = searchConditionsList, SearchConditionsGroupType = searchConditionsGroupType, SearchSortFieldNamesList = searchSortFieldNamesList, SearchSortFieldNamesDescendingList = searchSortFieldNamesDescendingList, entityId = entityId, schoolYearId = schoolYearId, flatten = flatten, returnResponse = returnResponse)
	}

	#' Get a BudgetVersionAccount
	#'
	#' This function returns a dataframe or json object of a BudgetVersionAccount
	#' @param BudgetVersionAccountID The ID of the BudgetVersionAccount to return.
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given BudgetVersionAccount. Defaults to FALSE for all return fields which, for convenience, returns all fields for the BudgetVersionAccount.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object listed as text. Run \code{\link{getSchemaForObjects}}('BudgetVersionAccount') to get more field paths.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{listSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Budgeting
	#' @return A dataframe or of BudgetVersionAccount
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	getBudgetVersionAccount <- function(BudgetVersionAccountID, BudgetVersionID = F, AccountID = F, BudgetVersionAccountAmount = F, IsOpenAccount = F, IsNotOpenAccount = F, IsAdoptedAccount = F, CurrentUserHasBudgetVersionClearance = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, Comment = F, fieldPaths = NULL, entityId = 1, schoolYearId = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment()) %>% purrr::keep(names(.) != "BudgetVersionAccountID")

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		getSkyObject(module = "Budgeting", objectName = "BudgetVersionAccount", objectId = BudgetVersionAccountID, searchFields = searchFields, entityId = entityId, flatten = flatten, returnResponse = returnResponse)
	}

	#' Delete a BudgetVersionAccount
	#'
	#' This function deletes a BudgetVersionAccount
	#' @param BudgetVersionAccountID The ID of the BudgetVersionAccount to delete
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{listSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Budgeting
	#' @return The BudgetVersionAccountID of the deleted BudgetVersionAccount.
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	deleteBudgetVersionAccount <- function(BudgetVersionAccountID, ignoreWarnings = F, entityId = 1, schoolYearId = NULL, flatten = T, returnResponse = F){

		deleteSkyObject(module = "Budgeting", objectName = "BudgetVersionAccount", objectId = BudgetVersionAccountID, ignoreWarnings = ignoreWarnings, entityId = entityId, flatten = flatten, returnResponse = returnResponse)
	}

	#' Create a BudgetVersionAccount
	#'
	#' This function creates a BudgetVersionAccount
	#' @param fieldNames The field values to give the created BudgetVersionAccount. Each defaults to NULL.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{listSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Budgeting
	#' @return A newly created BudgetVersionAccount
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	createBudgetVersionAccount <- function(BudgetVersionID = NULL, AccountID = NULL, Comment = NULL, entityId = 1, schoolYearId = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment())

		body <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper()) %>% purrr::compact()

		createSkyObject(module = "Budgeting", objectName = "BudgetVersionAccount", body = list(DataObject = body), searchFields = append("BudgetVersionAccountID", body %>% names()), entityId = entityId, schoolYearId = schoolYearId, flatten = flatten, returnResponse = returnResponse)
	}

	#' Modify a BudgetVersionAccount
	#'
	#' This function modifies a BudgetVersionAccount
	#' @param fieldNames The field values to give the modified BudgetVersionAccount. Each defaults to NULL.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{listSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Budgeting
	#' @return The modified BudgetVersionAccount
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	modifyBudgetVersionAccount <- function(BudgetVersionAccountID, BudgetVersionID = NULL, AccountID = NULL, Comment = NULL, entityId = 1, schoolYearId = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment())

		body <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper()) %>% purrr::compact()

		modifySkyObject(module = "Budgeting", objectName = "BudgetVersionAccount", objectId = BudgetVersionAccountID, body = list(DataObject = body), searchFields = append("BudgetVersionAccountID", body %>% names()), entityId = entityId, schoolYearId = schoolYearId, flatten = flatten, returnResponse = returnResponse)
	}

	#' List BudgetVersionAccountDetails
	#'
	#' This function returns a dataframe or json object of BudgetVersionAccountDetails
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given BudgetVersionAccountDetails. Defaults to FALSE for all return fields which, for convenience, returns all fields for the BudgetVersionAccountDetails.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object listed as text. Run \code{\link{getSchemaForObjects}}('BudgetVersionAccountDetail') to get more field paths.
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
	#' @concept Budgeting
	#' @return A list of BudgetVersionAccountDetails
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	listBudgetVersionAccountDetails <- function(searchConditionsList = NULL, BudgetVersionAccountDetailID = F, BudgetVersionAccountID = F, Amount = F, Status = F, Description = F, BudgetVersionAccountDetailIDClonedFrom = F, CurrentUserHasBudgetVersionClearance = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, PlanID = F, fieldPaths = NULL, searchConditionsGroupType = "And", searchSortFieldNamesList = NULL, searchSortFieldNamesDescendingList = NULL, entityId = 1, schoolYearId = NULL, page = 1, pageSize = 100000, flatten = T, returnResponse = F){

		params <- as.list(environment())

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		listSkyObjects(module = "Budgeting", objectName = "BudgetVersionAccountDetail", searchFields = searchFields %>% append(fieldPaths), page = page, pageSize = pageSize, SearchConditionsList = searchConditionsList, SearchConditionsGroupType = searchConditionsGroupType, SearchSortFieldNamesList = searchSortFieldNamesList, SearchSortFieldNamesDescendingList = searchSortFieldNamesDescendingList, entityId = entityId, schoolYearId = schoolYearId, flatten = flatten, returnResponse = returnResponse)
	}

	#' Get a BudgetVersionAccountDetail
	#'
	#' This function returns a dataframe or json object of a BudgetVersionAccountDetail
	#' @param BudgetVersionAccountDetailID The ID of the BudgetVersionAccountDetail to return.
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given BudgetVersionAccountDetail. Defaults to FALSE for all return fields which, for convenience, returns all fields for the BudgetVersionAccountDetail.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object listed as text. Run \code{\link{getSchemaForObjects}}('BudgetVersionAccountDetail') to get more field paths.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{listSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Budgeting
	#' @return A dataframe or of BudgetVersionAccountDetail
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	getBudgetVersionAccountDetail <- function(BudgetVersionAccountDetailID, BudgetVersionAccountID = F, Amount = F, Status = F, Description = F, BudgetVersionAccountDetailIDClonedFrom = F, CurrentUserHasBudgetVersionClearance = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, PlanID = F, fieldPaths = NULL, entityId = 1, schoolYearId = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment()) %>% purrr::keep(names(.) != "BudgetVersionAccountDetailID")

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		getSkyObject(module = "Budgeting", objectName = "BudgetVersionAccountDetail", objectId = BudgetVersionAccountDetailID, searchFields = searchFields, entityId = entityId, flatten = flatten, returnResponse = returnResponse)
	}

	#' Delete a BudgetVersionAccountDetail
	#'
	#' This function deletes a BudgetVersionAccountDetail
	#' @param BudgetVersionAccountDetailID The ID of the BudgetVersionAccountDetail to delete
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{listSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Budgeting
	#' @return The BudgetVersionAccountDetailID of the deleted BudgetVersionAccountDetail.
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	deleteBudgetVersionAccountDetail <- function(BudgetVersionAccountDetailID, ignoreWarnings = F, entityId = 1, schoolYearId = NULL, flatten = T, returnResponse = F){

		deleteSkyObject(module = "Budgeting", objectName = "BudgetVersionAccountDetail", objectId = BudgetVersionAccountDetailID, ignoreWarnings = ignoreWarnings, entityId = entityId, flatten = flatten, returnResponse = returnResponse)
	}

	#' Create a BudgetVersionAccountDetail
	#'
	#' This function creates a BudgetVersionAccountDetail
	#' @param fieldNames The field values to give the created BudgetVersionAccountDetail. Each defaults to NULL.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{listSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Budgeting
	#' @return A newly created BudgetVersionAccountDetail
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	createBudgetVersionAccountDetail <- function(BudgetVersionAccountID = NULL, Amount = NULL, Status = NULL, Description = NULL, BudgetVersionAccountDetailIDClonedFrom = NULL, PlanID = NULL, entityId = 1, schoolYearId = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment())

		body <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper()) %>% purrr::compact()

		createSkyObject(module = "Budgeting", objectName = "BudgetVersionAccountDetail", body = list(DataObject = body), searchFields = append("BudgetVersionAccountDetailID", body %>% names()), entityId = entityId, schoolYearId = schoolYearId, flatten = flatten, returnResponse = returnResponse)
	}

	#' Modify a BudgetVersionAccountDetail
	#'
	#' This function modifies a BudgetVersionAccountDetail
	#' @param fieldNames The field values to give the modified BudgetVersionAccountDetail. Each defaults to NULL.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{listSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Budgeting
	#' @return The modified BudgetVersionAccountDetail
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	modifyBudgetVersionAccountDetail <- function(BudgetVersionAccountDetailID, BudgetVersionAccountID = NULL, Amount = NULL, Status = NULL, Description = NULL, BudgetVersionAccountDetailIDClonedFrom = NULL, PlanID = NULL, entityId = 1, schoolYearId = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment())

		body <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper()) %>% purrr::compact()

		modifySkyObject(module = "Budgeting", objectName = "BudgetVersionAccountDetail", objectId = BudgetVersionAccountDetailID, body = list(DataObject = body), searchFields = append("BudgetVersionAccountDetailID", body %>% names()), entityId = entityId, schoolYearId = schoolYearId, flatten = flatten, returnResponse = returnResponse)
	}

	#' List BudgetVersions
	#'
	#' This function returns a dataframe or json object of BudgetVersions
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given BudgetVersions. Defaults to FALSE for all return fields which, for convenience, returns all fields for the BudgetVersions.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object listed as text. Run \code{\link{getSchemaForObjects}}('BudgetVersion') to get more field paths.
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
	#' @concept Budgeting
	#' @return A list of BudgetVersions
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	listBudgetVersions <- function(searchConditionsList = NULL, BudgetVersionID = F, DistrictID = F, Code = F, Description = F, FiscalYearID = F, Rank = F, CodeDescription = F, BudgetAmount = F, AccountsAtThisBudgetVersionRankCount = F, AdoptedAccountsCount = F, MaxRank = F, MinRank = F, AttachmentCount = F, CanAdvance = F, CanRollBack = F, CurrentUserHasBudgetVersionClearance = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, PlanAmount = F, StaffPlanningTotal = F, NonStaffPlanningTotal = F, fieldPaths = NULL, searchConditionsGroupType = "And", searchSortFieldNamesList = NULL, searchSortFieldNamesDescendingList = NULL, entityId = 1, schoolYearId = NULL, page = 1, pageSize = 100000, flatten = T, returnResponse = F){

		params <- as.list(environment())

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		listSkyObjects(module = "Budgeting", objectName = "BudgetVersion", searchFields = searchFields %>% append(fieldPaths), page = page, pageSize = pageSize, SearchConditionsList = searchConditionsList, SearchConditionsGroupType = searchConditionsGroupType, SearchSortFieldNamesList = searchSortFieldNamesList, SearchSortFieldNamesDescendingList = searchSortFieldNamesDescendingList, entityId = entityId, schoolYearId = schoolYearId, flatten = flatten, returnResponse = returnResponse)
	}

	#' Get a BudgetVersion
	#'
	#' This function returns a dataframe or json object of a BudgetVersion
	#' @param BudgetVersionID The ID of the BudgetVersion to return.
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given BudgetVersion. Defaults to FALSE for all return fields which, for convenience, returns all fields for the BudgetVersion.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object listed as text. Run \code{\link{getSchemaForObjects}}('BudgetVersion') to get more field paths.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{listSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Budgeting
	#' @return A dataframe or of BudgetVersion
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	getBudgetVersion <- function(BudgetVersionID, DistrictID = F, Code = F, Description = F, FiscalYearID = F, Rank = F, CodeDescription = F, BudgetAmount = F, AccountsAtThisBudgetVersionRankCount = F, AdoptedAccountsCount = F, MaxRank = F, MinRank = F, AttachmentCount = F, CanAdvance = F, CanRollBack = F, CurrentUserHasBudgetVersionClearance = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, PlanAmount = F, StaffPlanningTotal = F, NonStaffPlanningTotal = F, fieldPaths = NULL, entityId = 1, schoolYearId = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment()) %>% purrr::keep(names(.) != "BudgetVersionID")

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		getSkyObject(module = "Budgeting", objectName = "BudgetVersion", objectId = BudgetVersionID, searchFields = searchFields, entityId = entityId, flatten = flatten, returnResponse = returnResponse)
	}

	#' Delete a BudgetVersion
	#'
	#' This function deletes a BudgetVersion
	#' @param BudgetVersionID The ID of the BudgetVersion to delete
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{listSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Budgeting
	#' @return The BudgetVersionID of the deleted BudgetVersion.
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	deleteBudgetVersion <- function(BudgetVersionID, ignoreWarnings = F, entityId = 1, schoolYearId = NULL, flatten = T, returnResponse = F){

		deleteSkyObject(module = "Budgeting", objectName = "BudgetVersion", objectId = BudgetVersionID, ignoreWarnings = ignoreWarnings, entityId = entityId, flatten = flatten, returnResponse = returnResponse)
	}

	#' Create a BudgetVersion
	#'
	#' This function creates a BudgetVersion
	#' @param fieldNames The field values to give the created BudgetVersion. Each defaults to NULL.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{listSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Budgeting
	#' @return A newly created BudgetVersion
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	createBudgetVersion <- function(DistrictID = NULL, Code = NULL, Description = NULL, FiscalYearID = NULL, Rank = NULL, entityId = 1, schoolYearId = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment())

		body <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper()) %>% purrr::compact()

		createSkyObject(module = "Budgeting", objectName = "BudgetVersion", body = list(DataObject = body), searchFields = append("BudgetVersionID", body %>% names()), entityId = entityId, schoolYearId = schoolYearId, flatten = flatten, returnResponse = returnResponse)
	}

	#' Modify a BudgetVersion
	#'
	#' This function modifies a BudgetVersion
	#' @param fieldNames The field values to give the modified BudgetVersion. Each defaults to NULL.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{listSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Budgeting
	#' @return The modified BudgetVersion
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	modifyBudgetVersion <- function(BudgetVersionID, DistrictID = NULL, Code = NULL, Description = NULL, FiscalYearID = NULL, Rank = NULL, entityId = 1, schoolYearId = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment())

		body <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper()) %>% purrr::compact()

		modifySkyObject(module = "Budgeting", objectName = "BudgetVersion", objectId = BudgetVersionID, body = list(DataObject = body), searchFields = append("BudgetVersionID", body %>% names()), entityId = entityId, schoolYearId = schoolYearId, flatten = flatten, returnResponse = returnResponse)
	}

	#' List BudgetVersionClearances
	#'
	#' This function returns a dataframe or json object of BudgetVersionClearances
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given BudgetVersionClearances. Defaults to FALSE for all return fields which, for convenience, returns all fields for the BudgetVersionClearances.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object listed as text. Run \code{\link{getSchemaForObjects}}('BudgetVersionClearance') to get more field paths.
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
	#' @concept Budgeting
	#' @return A list of BudgetVersionClearances
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	listBudgetVersionClearances <- function(searchConditionsList = NULL, BudgetVersionClearanceID = F, BudgetVersionID = F, GroupIDSecurity = F, GroupIDAccount = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, fieldPaths = NULL, searchConditionsGroupType = "And", searchSortFieldNamesList = NULL, searchSortFieldNamesDescendingList = NULL, entityId = 1, schoolYearId = NULL, page = 1, pageSize = 100000, flatten = T, returnResponse = F){

		params <- as.list(environment())

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		listSkyObjects(module = "Budgeting", objectName = "BudgetVersionClearance", searchFields = searchFields %>% append(fieldPaths), page = page, pageSize = pageSize, SearchConditionsList = searchConditionsList, SearchConditionsGroupType = searchConditionsGroupType, SearchSortFieldNamesList = searchSortFieldNamesList, SearchSortFieldNamesDescendingList = searchSortFieldNamesDescendingList, entityId = entityId, schoolYearId = schoolYearId, flatten = flatten, returnResponse = returnResponse)
	}

	#' Get a BudgetVersionClearance
	#'
	#' This function returns a dataframe or json object of a BudgetVersionClearance
	#' @param BudgetVersionClearanceID The ID of the BudgetVersionClearance to return.
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given BudgetVersionClearance. Defaults to FALSE for all return fields which, for convenience, returns all fields for the BudgetVersionClearance.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object listed as text. Run \code{\link{getSchemaForObjects}}('BudgetVersionClearance') to get more field paths.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{listSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Budgeting
	#' @return A dataframe or of BudgetVersionClearance
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	getBudgetVersionClearance <- function(BudgetVersionClearanceID, BudgetVersionID = F, GroupIDSecurity = F, GroupIDAccount = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, fieldPaths = NULL, entityId = 1, schoolYearId = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment()) %>% purrr::keep(names(.) != "BudgetVersionClearanceID")

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		getSkyObject(module = "Budgeting", objectName = "BudgetVersionClearance", objectId = BudgetVersionClearanceID, searchFields = searchFields, entityId = entityId, flatten = flatten, returnResponse = returnResponse)
	}

	#' Delete a BudgetVersionClearance
	#'
	#' This function deletes a BudgetVersionClearance
	#' @param BudgetVersionClearanceID The ID of the BudgetVersionClearance to delete
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{listSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Budgeting
	#' @return The BudgetVersionClearanceID of the deleted BudgetVersionClearance.
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	deleteBudgetVersionClearance <- function(BudgetVersionClearanceID, ignoreWarnings = F, entityId = 1, schoolYearId = NULL, flatten = T, returnResponse = F){

		deleteSkyObject(module = "Budgeting", objectName = "BudgetVersionClearance", objectId = BudgetVersionClearanceID, ignoreWarnings = ignoreWarnings, entityId = entityId, flatten = flatten, returnResponse = returnResponse)
	}

	#' Create a BudgetVersionClearance
	#'
	#' This function creates a BudgetVersionClearance
	#' @param fieldNames The field values to give the created BudgetVersionClearance. Each defaults to NULL.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{listSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Budgeting
	#' @return A newly created BudgetVersionClearance
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	createBudgetVersionClearance <- function(BudgetVersionID = NULL, GroupIDSecurity = NULL, GroupIDAccount = NULL, entityId = 1, schoolYearId = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment())

		body <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper()) %>% purrr::compact()

		createSkyObject(module = "Budgeting", objectName = "BudgetVersionClearance", body = list(DataObject = body), searchFields = append("BudgetVersionClearanceID", body %>% names()), entityId = entityId, schoolYearId = schoolYearId, flatten = flatten, returnResponse = returnResponse)
	}

	#' Modify a BudgetVersionClearance
	#'
	#' This function modifies a BudgetVersionClearance
	#' @param fieldNames The field values to give the modified BudgetVersionClearance. Each defaults to NULL.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{listSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Budgeting
	#' @return The modified BudgetVersionClearance
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	modifyBudgetVersionClearance <- function(BudgetVersionClearanceID, BudgetVersionID = NULL, GroupIDSecurity = NULL, GroupIDAccount = NULL, entityId = 1, schoolYearId = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment())

		body <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper()) %>% purrr::compact()

		modifySkyObject(module = "Budgeting", objectName = "BudgetVersionClearance", objectId = BudgetVersionClearanceID, body = list(DataObject = body), searchFields = append("BudgetVersionClearanceID", body %>% names()), entityId = entityId, schoolYearId = schoolYearId, flatten = flatten, returnResponse = returnResponse)
	}

	#' List BudgetingConfigDistricts
	#'
	#' This function returns a dataframe or json object of BudgetingConfigDistricts
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given BudgetingConfigDistricts. Defaults to FALSE for all return fields which, for convenience, returns all fields for the BudgetingConfigDistricts.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object listed as text. Run \code{\link{getSchemaForObjects}}('BudgetingConfigDistrict') to get more field paths.
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
	#' @concept Budgeting
	#' @return A list of BudgetingConfigDistricts
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	listBudgetingConfigDistricts <- function(searchConditionsList = NULL, ConfigDistrictID = F, DistrictID = F, SendApprovedMessage = F, BudgetAmendmentRequestApprovedMessageSubject = F, BudgetAmendmentRequestApprovedMessageContent = F, SendDeniedMessage = F, BudgetAmendmentRequestDeniedMessageSubject = F, BudgetAmendmentRequestDeniedMessageContent = F, SendWaitingMessage = F, BudgetAmendmentRequestWaitingMessageSubject = F, BudgetAmendmentRequestWaitingMessageContent = F, UseAvailableFundsWarning = F, AvailableFundsWarningXMLFilter = F, SecurityStandardWarningFilterData = F, UseAvailableFundsError = F, AvailableFundsErrorXMLFilter = F, SecurityStandardErrorFilterData = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, fieldPaths = NULL, searchConditionsGroupType = "And", searchSortFieldNamesList = NULL, searchSortFieldNamesDescendingList = NULL, entityId = 1, schoolYearId = NULL, page = 1, pageSize = 100000, flatten = T, returnResponse = F){

		params <- as.list(environment())

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		listSkyObjects(module = "Budgeting", objectName = "ConfigDistrict", searchFields = searchFields %>% append(fieldPaths), page = page, pageSize = pageSize, SearchConditionsList = searchConditionsList, SearchConditionsGroupType = searchConditionsGroupType, SearchSortFieldNamesList = searchSortFieldNamesList, SearchSortFieldNamesDescendingList = searchSortFieldNamesDescendingList, entityId = entityId, schoolYearId = schoolYearId, flatten = flatten, returnResponse = returnResponse)
	}

	#' Get a BudgetingConfigDistrict
	#'
	#' This function returns a dataframe or json object of a BudgetingConfigDistrict
	#' @param BudgetingConfigDistrictID The ID of the BudgetingConfigDistrict to return.
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given BudgetingConfigDistrict. Defaults to FALSE for all return fields which, for convenience, returns all fields for the BudgetingConfigDistrict.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object listed as text. Run \code{\link{getSchemaForObjects}}('BudgetingConfigDistrict') to get more field paths.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{listSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Budgeting
	#' @return A dataframe or of BudgetingConfigDistrict
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	getBudgetingConfigDistrict <- function(BudgetingConfigDistrictID, ConfigDistrictID = F, DistrictID = F, SendApprovedMessage = F, BudgetAmendmentRequestApprovedMessageSubject = F, BudgetAmendmentRequestApprovedMessageContent = F, SendDeniedMessage = F, BudgetAmendmentRequestDeniedMessageSubject = F, BudgetAmendmentRequestDeniedMessageContent = F, SendWaitingMessage = F, BudgetAmendmentRequestWaitingMessageSubject = F, BudgetAmendmentRequestWaitingMessageContent = F, UseAvailableFundsWarning = F, AvailableFundsWarningXMLFilter = F, SecurityStandardWarningFilterData = F, UseAvailableFundsError = F, AvailableFundsErrorXMLFilter = F, SecurityStandardErrorFilterData = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, fieldPaths = NULL, entityId = 1, schoolYearId = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment()) %>% purrr::keep(names(.) != "BudgetingConfigDistrictID")

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		getSkyObject(module = "Budgeting", objectName = "ConfigDistrict", objectId = BudgetingConfigDistrictID, searchFields = searchFields, entityId = entityId, flatten = flatten, returnResponse = returnResponse)
	}

	#' Delete a BudgetingConfigDistrict
	#'
	#' This function deletes a BudgetingConfigDistrict
	#' @param BudgetingConfigDistrictID The ID of the BudgetingConfigDistrict to delete
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{listSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Budgeting
	#' @return The BudgetingConfigDistrictID of the deleted BudgetingConfigDistrict.
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	deleteBudgetingConfigDistrict <- function(BudgetingConfigDistrictID, ignoreWarnings = F, entityId = 1, schoolYearId = NULL, flatten = T, returnResponse = F){

		deleteSkyObject(module = "Budgeting", objectName = "ConfigDistrict", objectId = BudgetingConfigDistrictID, ignoreWarnings = ignoreWarnings, entityId = entityId, flatten = flatten, returnResponse = returnResponse)
	}

	#' Create a BudgetingConfigDistrict
	#'
	#' This function creates a BudgetingConfigDistrict
	#' @param fieldNames The field values to give the created BudgetingConfigDistrict. Each defaults to NULL.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{listSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Budgeting
	#' @return A newly created BudgetingConfigDistrict
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	createBudgetingConfigDistrict <- function(DistrictID = NULL, SendApprovedMessage = NULL, BudgetAmendmentRequestApprovedMessageSubject = NULL, BudgetAmendmentRequestApprovedMessageContent = NULL, SendDeniedMessage = NULL, BudgetAmendmentRequestDeniedMessageSubject = NULL, BudgetAmendmentRequestDeniedMessageContent = NULL, SendWaitingMessage = NULL, BudgetAmendmentRequestWaitingMessageSubject = NULL, BudgetAmendmentRequestWaitingMessageContent = NULL, UseAvailableFundsWarning = NULL, UseAvailableFundsError = NULL, entityId = 1, schoolYearId = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment())

		body <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper()) %>% purrr::compact()

		createSkyObject(module = "Budgeting", objectName = "ConfigDistrict", body = list(DataObject = body), searchFields = append("ConfigDistrictID", body %>% names()), entityId = entityId, schoolYearId = schoolYearId, flatten = flatten, returnResponse = returnResponse)
	}

	#' Modify a BudgetingConfigDistrict
	#'
	#' This function modifies a BudgetingConfigDistrict
	#' @param fieldNames The field values to give the modified BudgetingConfigDistrict. Each defaults to NULL.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{listSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Budgeting
	#' @return The modified BudgetingConfigDistrict
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	modifyBudgetingConfigDistrict <- function(ConfigDistrictID, DistrictID = NULL, SendApprovedMessage = NULL, BudgetAmendmentRequestApprovedMessageSubject = NULL, BudgetAmendmentRequestApprovedMessageContent = NULL, SendDeniedMessage = NULL, BudgetAmendmentRequestDeniedMessageSubject = NULL, BudgetAmendmentRequestDeniedMessageContent = NULL, SendWaitingMessage = NULL, BudgetAmendmentRequestWaitingMessageSubject = NULL, BudgetAmendmentRequestWaitingMessageContent = NULL, UseAvailableFundsWarning = NULL, UseAvailableFundsError = NULL, entityId = 1, schoolYearId = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment())

		body <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper()) %>% purrr::compact()

		modifySkyObject(module = "Budgeting", objectName = "ConfigDistrict", objectId = ConfigDistrictID, body = list(DataObject = body), searchFields = append("ConfigDistrictID", body %>% names()), entityId = entityId, schoolYearId = schoolYearId, flatten = flatten, returnResponse = returnResponse)
	}

	#' List TempBudgetAmendmentAudits
	#'
	#' This function returns a dataframe or json object of TempBudgetAmendmentAudits
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given TempBudgetAmendmentAudits. Defaults to FALSE for all return fields which, for convenience, returns all fields for the TempBudgetAmendmentAudits.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object listed as text. Run \code{\link{getSchemaForObjects}}('TempBudgetAmendmentAudit') to get more field paths.
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
	#' @concept Budgeting
	#' @return A list of TempBudgetAmendmentAudits
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	listTempBudgetAmendmentAudits <- function(searchConditionsList = NULL, TempBudgetAmendmentAuditID = F, AccountID = F, DisplayAccount = F, FiscalYear = F, Month = F, CalculatedAmount = F, RecordedAmount = F, AmountDifference = F, IsFiscalYear = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, fieldPaths = NULL, searchConditionsGroupType = "And", searchSortFieldNamesList = NULL, searchSortFieldNamesDescendingList = NULL, entityId = 1, schoolYearId = NULL, page = 1, pageSize = 100000, flatten = T, returnResponse = F){

		params <- as.list(environment())

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		listSkyObjects(module = "Budgeting", objectName = "TempBudgetAmendmentAudit", searchFields = searchFields %>% append(fieldPaths), page = page, pageSize = pageSize, SearchConditionsList = searchConditionsList, SearchConditionsGroupType = searchConditionsGroupType, SearchSortFieldNamesList = searchSortFieldNamesList, SearchSortFieldNamesDescendingList = searchSortFieldNamesDescendingList, entityId = entityId, schoolYearId = schoolYearId, flatten = flatten, returnResponse = returnResponse)
	}

	#' Get a TempBudgetAmendmentAudit
	#'
	#' This function returns a dataframe or json object of a TempBudgetAmendmentAudit
	#' @param TempBudgetAmendmentAuditID The ID of the TempBudgetAmendmentAudit to return.
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given TempBudgetAmendmentAudit. Defaults to FALSE for all return fields which, for convenience, returns all fields for the TempBudgetAmendmentAudit.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object listed as text. Run \code{\link{getSchemaForObjects}}('TempBudgetAmendmentAudit') to get more field paths.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{listSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Budgeting
	#' @return A dataframe or of TempBudgetAmendmentAudit
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	getTempBudgetAmendmentAudit <- function(TempBudgetAmendmentAuditID, AccountID = F, DisplayAccount = F, FiscalYear = F, Month = F, CalculatedAmount = F, RecordedAmount = F, AmountDifference = F, IsFiscalYear = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, fieldPaths = NULL, entityId = 1, schoolYearId = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment()) %>% purrr::keep(names(.) != "TempBudgetAmendmentAuditID")

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		getSkyObject(module = "Budgeting", objectName = "TempBudgetAmendmentAudit", objectId = TempBudgetAmendmentAuditID, searchFields = searchFields, entityId = entityId, flatten = flatten, returnResponse = returnResponse)
	}

	#' Delete a TempBudgetAmendmentAudit
	#'
	#' This function deletes a TempBudgetAmendmentAudit
	#' @param TempBudgetAmendmentAuditID The ID of the TempBudgetAmendmentAudit to delete
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{listSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Budgeting
	#' @return The TempBudgetAmendmentAuditID of the deleted TempBudgetAmendmentAudit.
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	deleteTempBudgetAmendmentAudit <- function(TempBudgetAmendmentAuditID, ignoreWarnings = F, entityId = 1, schoolYearId = NULL, flatten = T, returnResponse = F){

		deleteSkyObject(module = "Budgeting", objectName = "TempBudgetAmendmentAudit", objectId = TempBudgetAmendmentAuditID, ignoreWarnings = ignoreWarnings, entityId = entityId, flatten = flatten, returnResponse = returnResponse)
	}

	#' Create a TempBudgetAmendmentAudit
	#'
	#' This function creates a TempBudgetAmendmentAudit
	#' @param fieldNames The field values to give the created TempBudgetAmendmentAudit. Each defaults to NULL.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{listSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Budgeting
	#' @return A newly created TempBudgetAmendmentAudit
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	createTempBudgetAmendmentAudit <- function(AccountID = NULL, DisplayAccount = NULL, FiscalYear = NULL, Month = NULL, CalculatedAmount = NULL, RecordedAmount = NULL, AmountDifference = NULL, entityId = 1, schoolYearId = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment())

		body <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper()) %>% purrr::compact()

		createSkyObject(module = "Budgeting", objectName = "TempBudgetAmendmentAudit", body = list(DataObject = body), searchFields = append("TempBudgetAmendmentAuditID", body %>% names()), entityId = entityId, schoolYearId = schoolYearId, flatten = flatten, returnResponse = returnResponse)
	}

	#' Modify a TempBudgetAmendmentAudit
	#'
	#' This function modifies a TempBudgetAmendmentAudit
	#' @param fieldNames The field values to give the modified TempBudgetAmendmentAudit. Each defaults to NULL.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{listSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Budgeting
	#' @return The modified TempBudgetAmendmentAudit
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	modifyTempBudgetAmendmentAudit <- function(TempBudgetAmendmentAuditID, AccountID = NULL, DisplayAccount = NULL, FiscalYear = NULL, Month = NULL, CalculatedAmount = NULL, RecordedAmount = NULL, AmountDifference = NULL, entityId = 1, schoolYearId = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment())

		body <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper()) %>% purrr::compact()

		modifySkyObject(module = "Budgeting", objectName = "TempBudgetAmendmentAudit", objectId = TempBudgetAmendmentAuditID, body = list(DataObject = body), searchFields = append("TempBudgetAmendmentAuditID", body %>% names()), entityId = entityId, schoolYearId = schoolYearId, flatten = flatten, returnResponse = returnResponse)
	}

	#' List BudgetAmendments
	#'
	#' This function returns a dataframe or json object of BudgetAmendments
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given BudgetAmendments. Defaults to FALSE for all return fields which, for convenience, returns all fields for the BudgetAmendments.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object listed as text. Run \code{\link{getSchemaForObjects}}('BudgetAmendment') to get more field paths.
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
	#' @concept Budgeting
	#' @return A list of BudgetAmendments
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	listBudgetAmendments <- function(searchConditionsList = NULL, BudgetAmendmentID = F, BudgetAmendmentGroupID = F, FiscalYearID = F, Batch = F, Description = F, PostDate = F, Status = F, Type = F, CanSubmit = F, BudgetAmountIn = F, BudgetAmountOut = F, AttachmentCount = F, CurrentUserHasBudgetAmendmentGroupAccess = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, CanDelete = F, RenderBudgetAmendmentDetailBrowseButtons = F, CanResubmit = F, BudgetAmountNetTotal = F, fieldPaths = NULL, searchConditionsGroupType = "And", searchSortFieldNamesList = NULL, searchSortFieldNamesDescendingList = NULL, entityId = 1, schoolYearId = NULL, page = 1, pageSize = 100000, flatten = T, returnResponse = F){

		params <- as.list(environment())

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		listSkyObjects(module = "Budgeting", objectName = "BudgetAmendment", searchFields = searchFields %>% append(fieldPaths), page = page, pageSize = pageSize, SearchConditionsList = searchConditionsList, SearchConditionsGroupType = searchConditionsGroupType, SearchSortFieldNamesList = searchSortFieldNamesList, SearchSortFieldNamesDescendingList = searchSortFieldNamesDescendingList, entityId = entityId, schoolYearId = schoolYearId, flatten = flatten, returnResponse = returnResponse)
	}

	#' Get a BudgetAmendment
	#'
	#' This function returns a dataframe or json object of a BudgetAmendment
	#' @param BudgetAmendmentID The ID of the BudgetAmendment to return.
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given BudgetAmendment. Defaults to FALSE for all return fields which, for convenience, returns all fields for the BudgetAmendment.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object listed as text. Run \code{\link{getSchemaForObjects}}('BudgetAmendment') to get more field paths.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{listSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Budgeting
	#' @return A dataframe or of BudgetAmendment
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	getBudgetAmendment <- function(BudgetAmendmentID, BudgetAmendmentGroupID = F, FiscalYearID = F, Batch = F, Description = F, PostDate = F, Status = F, Type = F, CanSubmit = F, BudgetAmountIn = F, BudgetAmountOut = F, AttachmentCount = F, CurrentUserHasBudgetAmendmentGroupAccess = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, CanDelete = F, RenderBudgetAmendmentDetailBrowseButtons = F, CanResubmit = F, BudgetAmountNetTotal = F, fieldPaths = NULL, entityId = 1, schoolYearId = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment()) %>% purrr::keep(names(.) != "BudgetAmendmentID")

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		getSkyObject(module = "Budgeting", objectName = "BudgetAmendment", objectId = BudgetAmendmentID, searchFields = searchFields, entityId = entityId, flatten = flatten, returnResponse = returnResponse)
	}

	#' Delete a BudgetAmendment
	#'
	#' This function deletes a BudgetAmendment
	#' @param BudgetAmendmentID The ID of the BudgetAmendment to delete
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{listSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Budgeting
	#' @return The BudgetAmendmentID of the deleted BudgetAmendment.
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	deleteBudgetAmendment <- function(BudgetAmendmentID, ignoreWarnings = F, entityId = 1, schoolYearId = NULL, flatten = T, returnResponse = F){

		deleteSkyObject(module = "Budgeting", objectName = "BudgetAmendment", objectId = BudgetAmendmentID, ignoreWarnings = ignoreWarnings, entityId = entityId, flatten = flatten, returnResponse = returnResponse)
	}

	#' Create a BudgetAmendment
	#'
	#' This function creates a BudgetAmendment
	#' @param fieldNames The field values to give the created BudgetAmendment. Each defaults to NULL.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{listSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Budgeting
	#' @return A newly created BudgetAmendment
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	createBudgetAmendment <- function(BudgetAmendmentGroupID = NULL, FiscalYearID = NULL, Batch = NULL, Description = NULL, PostDate = NULL, Status = NULL, Type = NULL, entityId = 1, schoolYearId = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment())

		body <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper()) %>% purrr::compact()

		createSkyObject(module = "Budgeting", objectName = "BudgetAmendment", body = list(DataObject = body), searchFields = append("BudgetAmendmentID", body %>% names()), entityId = entityId, schoolYearId = schoolYearId, flatten = flatten, returnResponse = returnResponse)
	}

	#' Modify a BudgetAmendment
	#'
	#' This function modifies a BudgetAmendment
	#' @param fieldNames The field values to give the modified BudgetAmendment. Each defaults to NULL.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{listSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Budgeting
	#' @return The modified BudgetAmendment
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	modifyBudgetAmendment <- function(BudgetAmendmentID, BudgetAmendmentGroupID = NULL, FiscalYearID = NULL, Batch = NULL, Description = NULL, PostDate = NULL, Status = NULL, Type = NULL, entityId = 1, schoolYearId = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment())

		body <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper()) %>% purrr::compact()

		modifySkyObject(module = "Budgeting", objectName = "BudgetAmendment", objectId = BudgetAmendmentID, body = list(DataObject = body), searchFields = append("BudgetAmendmentID", body %>% names()), entityId = entityId, schoolYearId = schoolYearId, flatten = flatten, returnResponse = returnResponse)
	}

	#' List BudgetAmendmentDetails
	#'
	#' This function returns a dataframe or json object of BudgetAmendmentDetails
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given BudgetAmendmentDetails. Defaults to FALSE for all return fields which, for convenience, returns all fields for the BudgetAmendmentDetails.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object listed as text. Run \code{\link{getSchemaForObjects}}('BudgetAmendmentDetail') to get more field paths.
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
	#' @concept Budgeting
	#' @return A list of BudgetAmendmentDetails
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	listBudgetAmendmentDetails <- function(searchConditionsList = NULL, BudgetAmendmentDetailID = F, DisplayOrder = F, BudgetAmendmentID = F, AccountID = F, Description = F, BudgetAmountIn = F, BudgetAmountOut = F, PostDateMonth = F, CurrentUserHasBudgetAmendmentGroupAccess = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, fieldPaths = NULL, searchConditionsGroupType = "And", searchSortFieldNamesList = NULL, searchSortFieldNamesDescendingList = NULL, entityId = 1, schoolYearId = NULL, page = 1, pageSize = 100000, flatten = T, returnResponse = F){

		params <- as.list(environment())

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		listSkyObjects(module = "Budgeting", objectName = "BudgetAmendmentDetail", searchFields = searchFields %>% append(fieldPaths), page = page, pageSize = pageSize, SearchConditionsList = searchConditionsList, SearchConditionsGroupType = searchConditionsGroupType, SearchSortFieldNamesList = searchSortFieldNamesList, SearchSortFieldNamesDescendingList = searchSortFieldNamesDescendingList, entityId = entityId, schoolYearId = schoolYearId, flatten = flatten, returnResponse = returnResponse)
	}

	#' Get a BudgetAmendmentDetail
	#'
	#' This function returns a dataframe or json object of a BudgetAmendmentDetail
	#' @param BudgetAmendmentDetailID The ID of the BudgetAmendmentDetail to return.
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given BudgetAmendmentDetail. Defaults to FALSE for all return fields which, for convenience, returns all fields for the BudgetAmendmentDetail.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object listed as text. Run \code{\link{getSchemaForObjects}}('BudgetAmendmentDetail') to get more field paths.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{listSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Budgeting
	#' @return A dataframe or of BudgetAmendmentDetail
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	getBudgetAmendmentDetail <- function(BudgetAmendmentDetailID, DisplayOrder = F, BudgetAmendmentID = F, AccountID = F, Description = F, BudgetAmountIn = F, BudgetAmountOut = F, PostDateMonth = F, CurrentUserHasBudgetAmendmentGroupAccess = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, fieldPaths = NULL, entityId = 1, schoolYearId = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment()) %>% purrr::keep(names(.) != "BudgetAmendmentDetailID")

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		getSkyObject(module = "Budgeting", objectName = "BudgetAmendmentDetail", objectId = BudgetAmendmentDetailID, searchFields = searchFields, entityId = entityId, flatten = flatten, returnResponse = returnResponse)
	}

	#' Delete a BudgetAmendmentDetail
	#'
	#' This function deletes a BudgetAmendmentDetail
	#' @param BudgetAmendmentDetailID The ID of the BudgetAmendmentDetail to delete
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{listSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Budgeting
	#' @return The BudgetAmendmentDetailID of the deleted BudgetAmendmentDetail.
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	deleteBudgetAmendmentDetail <- function(BudgetAmendmentDetailID, ignoreWarnings = F, entityId = 1, schoolYearId = NULL, flatten = T, returnResponse = F){

		deleteSkyObject(module = "Budgeting", objectName = "BudgetAmendmentDetail", objectId = BudgetAmendmentDetailID, ignoreWarnings = ignoreWarnings, entityId = entityId, flatten = flatten, returnResponse = returnResponse)
	}

	#' Create a BudgetAmendmentDetail
	#'
	#' This function creates a BudgetAmendmentDetail
	#' @param fieldNames The field values to give the created BudgetAmendmentDetail. Each defaults to NULL.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{listSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Budgeting
	#' @return A newly created BudgetAmendmentDetail
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	createBudgetAmendmentDetail <- function(DisplayOrder = NULL, BudgetAmendmentID = NULL, AccountID = NULL, Description = NULL, BudgetAmountIn = NULL, BudgetAmountOut = NULL, entityId = 1, schoolYearId = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment())

		body <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper()) %>% purrr::compact()

		createSkyObject(module = "Budgeting", objectName = "BudgetAmendmentDetail", body = list(DataObject = body), searchFields = append("BudgetAmendmentDetailID", body %>% names()), entityId = entityId, schoolYearId = schoolYearId, flatten = flatten, returnResponse = returnResponse)
	}

	#' Modify a BudgetAmendmentDetail
	#'
	#' This function modifies a BudgetAmendmentDetail
	#' @param fieldNames The field values to give the modified BudgetAmendmentDetail. Each defaults to NULL.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{listSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Budgeting
	#' @return The modified BudgetAmendmentDetail
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	modifyBudgetAmendmentDetail <- function(BudgetAmendmentDetailID, DisplayOrder = NULL, BudgetAmendmentID = NULL, AccountID = NULL, Description = NULL, BudgetAmountIn = NULL, BudgetAmountOut = NULL, entityId = 1, schoolYearId = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment())

		body <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper()) %>% purrr::compact()

		modifySkyObject(module = "Budgeting", objectName = "BudgetAmendmentDetail", objectId = BudgetAmendmentDetailID, body = list(DataObject = body), searchFields = append("BudgetAmendmentDetailID", body %>% names()), entityId = entityId, schoolYearId = schoolYearId, flatten = flatten, returnResponse = returnResponse)
	}

	#' List BudgetAmendmentGroupClearances
	#'
	#' This function returns a dataframe or json object of BudgetAmendmentGroupClearances
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given BudgetAmendmentGroupClearances. Defaults to FALSE for all return fields which, for convenience, returns all fields for the BudgetAmendmentGroupClearances.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object listed as text. Run \code{\link{getSchemaForObjects}}('BudgetAmendmentGroupClearance') to get more field paths.
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
	#' @concept Budgeting
	#' @return A list of BudgetAmendmentGroupClearances
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	listBudgetAmendmentGroupClearances <- function(searchConditionsList = NULL, BudgetAmendmentGroupClearanceID = F, GroupIDSecurity = F, BudgetAmendmentGroupID = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, fieldPaths = NULL, searchConditionsGroupType = "And", searchSortFieldNamesList = NULL, searchSortFieldNamesDescendingList = NULL, entityId = 1, schoolYearId = NULL, page = 1, pageSize = 100000, flatten = T, returnResponse = F){

		params <- as.list(environment())

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		listSkyObjects(module = "Budgeting", objectName = "BudgetAmendmentGroupClearance", searchFields = searchFields %>% append(fieldPaths), page = page, pageSize = pageSize, SearchConditionsList = searchConditionsList, SearchConditionsGroupType = searchConditionsGroupType, SearchSortFieldNamesList = searchSortFieldNamesList, SearchSortFieldNamesDescendingList = searchSortFieldNamesDescendingList, entityId = entityId, schoolYearId = schoolYearId, flatten = flatten, returnResponse = returnResponse)
	}

	#' Get a BudgetAmendmentGroupClearance
	#'
	#' This function returns a dataframe or json object of a BudgetAmendmentGroupClearance
	#' @param BudgetAmendmentGroupClearanceID The ID of the BudgetAmendmentGroupClearance to return.
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given BudgetAmendmentGroupClearance. Defaults to FALSE for all return fields which, for convenience, returns all fields for the BudgetAmendmentGroupClearance.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object listed as text. Run \code{\link{getSchemaForObjects}}('BudgetAmendmentGroupClearance') to get more field paths.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{listSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Budgeting
	#' @return A dataframe or of BudgetAmendmentGroupClearance
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	getBudgetAmendmentGroupClearance <- function(BudgetAmendmentGroupClearanceID, GroupIDSecurity = F, BudgetAmendmentGroupID = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, fieldPaths = NULL, entityId = 1, schoolYearId = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment()) %>% purrr::keep(names(.) != "BudgetAmendmentGroupClearanceID")

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		getSkyObject(module = "Budgeting", objectName = "BudgetAmendmentGroupClearance", objectId = BudgetAmendmentGroupClearanceID, searchFields = searchFields, entityId = entityId, flatten = flatten, returnResponse = returnResponse)
	}

	#' Delete a BudgetAmendmentGroupClearance
	#'
	#' This function deletes a BudgetAmendmentGroupClearance
	#' @param BudgetAmendmentGroupClearanceID The ID of the BudgetAmendmentGroupClearance to delete
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{listSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Budgeting
	#' @return The BudgetAmendmentGroupClearanceID of the deleted BudgetAmendmentGroupClearance.
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	deleteBudgetAmendmentGroupClearance <- function(BudgetAmendmentGroupClearanceID, ignoreWarnings = F, entityId = 1, schoolYearId = NULL, flatten = T, returnResponse = F){

		deleteSkyObject(module = "Budgeting", objectName = "BudgetAmendmentGroupClearance", objectId = BudgetAmendmentGroupClearanceID, ignoreWarnings = ignoreWarnings, entityId = entityId, flatten = flatten, returnResponse = returnResponse)
	}

	#' Create a BudgetAmendmentGroupClearance
	#'
	#' This function creates a BudgetAmendmentGroupClearance
	#' @param fieldNames The field values to give the created BudgetAmendmentGroupClearance. Each defaults to NULL.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{listSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Budgeting
	#' @return A newly created BudgetAmendmentGroupClearance
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	createBudgetAmendmentGroupClearance <- function(GroupIDSecurity = NULL, BudgetAmendmentGroupID = NULL, entityId = 1, schoolYearId = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment())

		body <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper()) %>% purrr::compact()

		createSkyObject(module = "Budgeting", objectName = "BudgetAmendmentGroupClearance", body = list(DataObject = body), searchFields = append("BudgetAmendmentGroupClearanceID", body %>% names()), entityId = entityId, schoolYearId = schoolYearId, flatten = flatten, returnResponse = returnResponse)
	}

	#' Modify a BudgetAmendmentGroupClearance
	#'
	#' This function modifies a BudgetAmendmentGroupClearance
	#' @param fieldNames The field values to give the modified BudgetAmendmentGroupClearance. Each defaults to NULL.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{listSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Budgeting
	#' @return The modified BudgetAmendmentGroupClearance
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	modifyBudgetAmendmentGroupClearance <- function(BudgetAmendmentGroupClearanceID, GroupIDSecurity = NULL, BudgetAmendmentGroupID = NULL, entityId = 1, schoolYearId = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment())

		body <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper()) %>% purrr::compact()

		modifySkyObject(module = "Budgeting", objectName = "BudgetAmendmentGroupClearance", objectId = BudgetAmendmentGroupClearanceID, body = list(DataObject = body), searchFields = append("BudgetAmendmentGroupClearanceID", body %>% names()), entityId = entityId, schoolYearId = schoolYearId, flatten = flatten, returnResponse = returnResponse)
	}

	#' List BudgetAmendmentGroups
	#'
	#' This function returns a dataframe or json object of BudgetAmendmentGroups
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given BudgetAmendmentGroups. Defaults to FALSE for all return fields which, for convenience, returns all fields for the BudgetAmendmentGroups.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object listed as text. Run \code{\link{getSchemaForObjects}}('BudgetAmendmentGroup') to get more field paths.
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
	#' @concept Budgeting
	#' @return A list of BudgetAmendmentGroups
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	listBudgetAmendmentGroups <- function(searchConditionsList = NULL, BudgetAmendmentGroupID = F, Code = F, Description = F, DistrictID = F, AllowUnbalancedRevision = F, IsActive = F, IsApprovalWorkflowUpdated = F, GroupIDAccount = F, CodeDescription = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, CurrentUserHasBudgetAmendmentGroupAccess = F, fieldPaths = NULL, searchConditionsGroupType = "And", searchSortFieldNamesList = NULL, searchSortFieldNamesDescendingList = NULL, entityId = 1, schoolYearId = NULL, page = 1, pageSize = 100000, flatten = T, returnResponse = F){

		params <- as.list(environment())

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		listSkyObjects(module = "Budgeting", objectName = "BudgetAmendmentGroup", searchFields = searchFields %>% append(fieldPaths), page = page, pageSize = pageSize, SearchConditionsList = searchConditionsList, SearchConditionsGroupType = searchConditionsGroupType, SearchSortFieldNamesList = searchSortFieldNamesList, SearchSortFieldNamesDescendingList = searchSortFieldNamesDescendingList, entityId = entityId, schoolYearId = schoolYearId, flatten = flatten, returnResponse = returnResponse)
	}

	#' Get a BudgetAmendmentGroup
	#'
	#' This function returns a dataframe or json object of a BudgetAmendmentGroup
	#' @param BudgetAmendmentGroupID The ID of the BudgetAmendmentGroup to return.
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given BudgetAmendmentGroup. Defaults to FALSE for all return fields which, for convenience, returns all fields for the BudgetAmendmentGroup.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object listed as text. Run \code{\link{getSchemaForObjects}}('BudgetAmendmentGroup') to get more field paths.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{listSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Budgeting
	#' @return A dataframe or of BudgetAmendmentGroup
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	getBudgetAmendmentGroup <- function(BudgetAmendmentGroupID, Code = F, Description = F, DistrictID = F, AllowUnbalancedRevision = F, IsActive = F, IsApprovalWorkflowUpdated = F, GroupIDAccount = F, CodeDescription = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, CurrentUserHasBudgetAmendmentGroupAccess = F, fieldPaths = NULL, entityId = 1, schoolYearId = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment()) %>% purrr::keep(names(.) != "BudgetAmendmentGroupID")

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		getSkyObject(module = "Budgeting", objectName = "BudgetAmendmentGroup", objectId = BudgetAmendmentGroupID, searchFields = searchFields, entityId = entityId, flatten = flatten, returnResponse = returnResponse)
	}

	#' Delete a BudgetAmendmentGroup
	#'
	#' This function deletes a BudgetAmendmentGroup
	#' @param BudgetAmendmentGroupID The ID of the BudgetAmendmentGroup to delete
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{listSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Budgeting
	#' @return The BudgetAmendmentGroupID of the deleted BudgetAmendmentGroup.
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	deleteBudgetAmendmentGroup <- function(BudgetAmendmentGroupID, ignoreWarnings = F, entityId = 1, schoolYearId = NULL, flatten = T, returnResponse = F){

		deleteSkyObject(module = "Budgeting", objectName = "BudgetAmendmentGroup", objectId = BudgetAmendmentGroupID, ignoreWarnings = ignoreWarnings, entityId = entityId, flatten = flatten, returnResponse = returnResponse)
	}

	#' Create a BudgetAmendmentGroup
	#'
	#' This function creates a BudgetAmendmentGroup
	#' @param fieldNames The field values to give the created BudgetAmendmentGroup. Each defaults to NULL.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{listSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Budgeting
	#' @return A newly created BudgetAmendmentGroup
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	createBudgetAmendmentGroup <- function(Code = NULL, Description = NULL, DistrictID = NULL, AllowUnbalancedRevision = NULL, IsActive = NULL, GroupIDAccount = NULL, entityId = 1, schoolYearId = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment())

		body <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper()) %>% purrr::compact()

		createSkyObject(module = "Budgeting", objectName = "BudgetAmendmentGroup", body = list(DataObject = body), searchFields = append("BudgetAmendmentGroupID", body %>% names()), entityId = entityId, schoolYearId = schoolYearId, flatten = flatten, returnResponse = returnResponse)
	}

	#' Modify a BudgetAmendmentGroup
	#'
	#' This function modifies a BudgetAmendmentGroup
	#' @param fieldNames The field values to give the modified BudgetAmendmentGroup. Each defaults to NULL.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{listSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Budgeting
	#' @return The modified BudgetAmendmentGroup
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	modifyBudgetAmendmentGroup <- function(BudgetAmendmentGroupID, Code = NULL, Description = NULL, DistrictID = NULL, AllowUnbalancedRevision = NULL, IsActive = NULL, GroupIDAccount = NULL, entityId = 1, schoolYearId = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment())

		body <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper()) %>% purrr::compact()

		modifySkyObject(module = "Budgeting", objectName = "BudgetAmendmentGroup", objectId = BudgetAmendmentGroupID, body = list(DataObject = body), searchFields = append("BudgetAmendmentGroupID", body %>% names()), entityId = entityId, schoolYearId = schoolYearId, flatten = flatten, returnResponse = returnResponse)
	}

	#' List TempBudgetVersionAccounts
	#'
	#' This function returns a dataframe or json object of TempBudgetVersionAccounts
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given TempBudgetVersionAccounts. Defaults to FALSE for all return fields which, for convenience, returns all fields for the TempBudgetVersionAccounts.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object listed as text. Run \code{\link{getSchemaForObjects}}('TempBudgetVersionAccount') to get more field paths.
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
	#' @concept Budgeting
	#' @return A list of TempBudgetVersionAccounts
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	listTempBudgetVersionAccounts <- function(searchConditionsList = NULL, TempBudgetVersionAccountID = F, AccountID = F, BudgetVersionID = F, FiscalYearDescription = F, BudgetDescription = F, AccountDisplay = F, AccountDescription = F, IsError = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, OldPlanAmount = F, NewPlanAmount = F, OldAmount = F, NewAmount = F, BudgetVersionAccountDetailID = F, BudgetVersionAccountAuditType = F, BudgetVersionAccountDetailDescription = F, fieldPaths = NULL, searchConditionsGroupType = "And", searchSortFieldNamesList = NULL, searchSortFieldNamesDescendingList = NULL, entityId = 1, schoolYearId = NULL, page = 1, pageSize = 100000, flatten = T, returnResponse = F){

		params <- as.list(environment())

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		listSkyObjects(module = "Budgeting", objectName = "TempBudgetVersionAccount", searchFields = searchFields %>% append(fieldPaths), page = page, pageSize = pageSize, SearchConditionsList = searchConditionsList, SearchConditionsGroupType = searchConditionsGroupType, SearchSortFieldNamesList = searchSortFieldNamesList, SearchSortFieldNamesDescendingList = searchSortFieldNamesDescendingList, entityId = entityId, schoolYearId = schoolYearId, flatten = flatten, returnResponse = returnResponse)
	}

	#' Get a TempBudgetVersionAccount
	#'
	#' This function returns a dataframe or json object of a TempBudgetVersionAccount
	#' @param TempBudgetVersionAccountID The ID of the TempBudgetVersionAccount to return.
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given TempBudgetVersionAccount. Defaults to FALSE for all return fields which, for convenience, returns all fields for the TempBudgetVersionAccount.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object listed as text. Run \code{\link{getSchemaForObjects}}('TempBudgetVersionAccount') to get more field paths.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{listSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Budgeting
	#' @return A dataframe or of TempBudgetVersionAccount
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	getTempBudgetVersionAccount <- function(TempBudgetVersionAccountID, AccountID = F, BudgetVersionID = F, FiscalYearDescription = F, BudgetDescription = F, AccountDisplay = F, AccountDescription = F, IsError = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, OldPlanAmount = F, NewPlanAmount = F, OldAmount = F, NewAmount = F, BudgetVersionAccountDetailID = F, BudgetVersionAccountAuditType = F, BudgetVersionAccountDetailDescription = F, fieldPaths = NULL, entityId = 1, schoolYearId = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment()) %>% purrr::keep(names(.) != "TempBudgetVersionAccountID")

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		getSkyObject(module = "Budgeting", objectName = "TempBudgetVersionAccount", objectId = TempBudgetVersionAccountID, searchFields = searchFields, entityId = entityId, flatten = flatten, returnResponse = returnResponse)
	}

	#' Delete a TempBudgetVersionAccount
	#'
	#' This function deletes a TempBudgetVersionAccount
	#' @param TempBudgetVersionAccountID The ID of the TempBudgetVersionAccount to delete
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{listSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Budgeting
	#' @return The TempBudgetVersionAccountID of the deleted TempBudgetVersionAccount.
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	deleteTempBudgetVersionAccount <- function(TempBudgetVersionAccountID, ignoreWarnings = F, entityId = 1, schoolYearId = NULL, flatten = T, returnResponse = F){

		deleteSkyObject(module = "Budgeting", objectName = "TempBudgetVersionAccount", objectId = TempBudgetVersionAccountID, ignoreWarnings = ignoreWarnings, entityId = entityId, flatten = flatten, returnResponse = returnResponse)
	}

	#' Create a TempBudgetVersionAccount
	#'
	#' This function creates a TempBudgetVersionAccount
	#' @param fieldNames The field values to give the created TempBudgetVersionAccount. Each defaults to NULL.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{listSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Budgeting
	#' @return A newly created TempBudgetVersionAccount
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	createTempBudgetVersionAccount <- function(AccountID = NULL, BudgetVersionID = NULL, FiscalYearDescription = NULL, BudgetDescription = NULL, AccountDisplay = NULL, AccountDescription = NULL, OldPlanAmount = NULL, NewPlanAmount = NULL, OldAmount = NULL, NewAmount = NULL, BudgetVersionAccountDetailID = NULL, BudgetVersionAccountAuditType = NULL, BudgetVersionAccountDetailDescription = NULL, entityId = 1, schoolYearId = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment())

		body <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper()) %>% purrr::compact()

		createSkyObject(module = "Budgeting", objectName = "TempBudgetVersionAccount", body = list(DataObject = body), searchFields = append("TempBudgetVersionAccountID", body %>% names()), entityId = entityId, schoolYearId = schoolYearId, flatten = flatten, returnResponse = returnResponse)
	}

	#' Modify a TempBudgetVersionAccount
	#'
	#' This function modifies a TempBudgetVersionAccount
	#' @param fieldNames The field values to give the modified TempBudgetVersionAccount. Each defaults to NULL.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{listSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Budgeting
	#' @return The modified TempBudgetVersionAccount
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	modifyTempBudgetVersionAccount <- function(TempBudgetVersionAccountID, AccountID = NULL, BudgetVersionID = NULL, FiscalYearDescription = NULL, BudgetDescription = NULL, AccountDisplay = NULL, AccountDescription = NULL, OldPlanAmount = NULL, NewPlanAmount = NULL, OldAmount = NULL, NewAmount = NULL, BudgetVersionAccountDetailID = NULL, BudgetVersionAccountAuditType = NULL, BudgetVersionAccountDetailDescription = NULL, entityId = 1, schoolYearId = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment())

		body <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper()) %>% purrr::compact()

		modifySkyObject(module = "Budgeting", objectName = "TempBudgetVersionAccount", objectId = TempBudgetVersionAccountID, body = list(DataObject = body), searchFields = append("TempBudgetVersionAccountID", body %>% names()), entityId = entityId, schoolYearId = schoolYearId, flatten = flatten, returnResponse = returnResponse)
	}

	#' List TempBudgetVersionAccountExceptions
	#'
	#' This function returns a dataframe or json object of TempBudgetVersionAccountExceptions
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given TempBudgetVersionAccountExceptions. Defaults to FALSE for all return fields which, for convenience, returns all fields for the TempBudgetVersionAccountExceptions.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object listed as text. Run \code{\link{getSchemaForObjects}}('TempBudgetVersionAccountException') to get more field paths.
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
	#' @concept Budgeting
	#' @return A list of TempBudgetVersionAccountExceptions
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	listTempBudgetVersionAccountExceptions <- function(searchConditionsList = NULL, TempBudgetVersionAccountExceptionID = F, TempBudgetVersionAccountID = F, Error = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, DisplayAccount = F, fieldPaths = NULL, searchConditionsGroupType = "And", searchSortFieldNamesList = NULL, searchSortFieldNamesDescendingList = NULL, entityId = 1, schoolYearId = NULL, page = 1, pageSize = 100000, flatten = T, returnResponse = F){

		params <- as.list(environment())

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		listSkyObjects(module = "Budgeting", objectName = "TempBudgetVersionAccountException", searchFields = searchFields %>% append(fieldPaths), page = page, pageSize = pageSize, SearchConditionsList = searchConditionsList, SearchConditionsGroupType = searchConditionsGroupType, SearchSortFieldNamesList = searchSortFieldNamesList, SearchSortFieldNamesDescendingList = searchSortFieldNamesDescendingList, entityId = entityId, schoolYearId = schoolYearId, flatten = flatten, returnResponse = returnResponse)
	}

	#' Get a TempBudgetVersionAccountException
	#'
	#' This function returns a dataframe or json object of a TempBudgetVersionAccountException
	#' @param TempBudgetVersionAccountExceptionID The ID of the TempBudgetVersionAccountException to return.
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given TempBudgetVersionAccountException. Defaults to FALSE for all return fields which, for convenience, returns all fields for the TempBudgetVersionAccountException.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object listed as text. Run \code{\link{getSchemaForObjects}}('TempBudgetVersionAccountException') to get more field paths.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{listSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Budgeting
	#' @return A dataframe or of TempBudgetVersionAccountException
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	getTempBudgetVersionAccountException <- function(TempBudgetVersionAccountExceptionID, TempBudgetVersionAccountID = F, Error = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, DisplayAccount = F, fieldPaths = NULL, entityId = 1, schoolYearId = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment()) %>% purrr::keep(names(.) != "TempBudgetVersionAccountExceptionID")

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		getSkyObject(module = "Budgeting", objectName = "TempBudgetVersionAccountException", objectId = TempBudgetVersionAccountExceptionID, searchFields = searchFields, entityId = entityId, flatten = flatten, returnResponse = returnResponse)
	}

	#' Delete a TempBudgetVersionAccountException
	#'
	#' This function deletes a TempBudgetVersionAccountException
	#' @param TempBudgetVersionAccountExceptionID The ID of the TempBudgetVersionAccountException to delete
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{listSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Budgeting
	#' @return The TempBudgetVersionAccountExceptionID of the deleted TempBudgetVersionAccountException.
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	deleteTempBudgetVersionAccountException <- function(TempBudgetVersionAccountExceptionID, ignoreWarnings = F, entityId = 1, schoolYearId = NULL, flatten = T, returnResponse = F){

		deleteSkyObject(module = "Budgeting", objectName = "TempBudgetVersionAccountException", objectId = TempBudgetVersionAccountExceptionID, ignoreWarnings = ignoreWarnings, entityId = entityId, flatten = flatten, returnResponse = returnResponse)
	}

	#' Create a TempBudgetVersionAccountException
	#'
	#' This function creates a TempBudgetVersionAccountException
	#' @param fieldNames The field values to give the created TempBudgetVersionAccountException. Each defaults to NULL.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{listSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Budgeting
	#' @return A newly created TempBudgetVersionAccountException
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	createTempBudgetVersionAccountException <- function(TempBudgetVersionAccountID = NULL, Error = NULL, DisplayAccount = NULL, entityId = 1, schoolYearId = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment())

		body <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper()) %>% purrr::compact()

		createSkyObject(module = "Budgeting", objectName = "TempBudgetVersionAccountException", body = list(DataObject = body), searchFields = append("TempBudgetVersionAccountExceptionID", body %>% names()), entityId = entityId, schoolYearId = schoolYearId, flatten = flatten, returnResponse = returnResponse)
	}

	#' Modify a TempBudgetVersionAccountException
	#'
	#' This function modifies a TempBudgetVersionAccountException
	#' @param fieldNames The field values to give the modified TempBudgetVersionAccountException. Each defaults to NULL.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{listSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Budgeting
	#' @return The modified TempBudgetVersionAccountException
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	modifyTempBudgetVersionAccountException <- function(TempBudgetVersionAccountExceptionID, TempBudgetVersionAccountID = NULL, Error = NULL, DisplayAccount = NULL, entityId = 1, schoolYearId = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment())

		body <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper()) %>% purrr::compact()

		modifySkyObject(module = "Budgeting", objectName = "TempBudgetVersionAccountException", objectId = TempBudgetVersionAccountExceptionID, body = list(DataObject = body), searchFields = append("TempBudgetVersionAccountExceptionID", body %>% names()), entityId = entityId, schoolYearId = schoolYearId, flatten = flatten, returnResponse = returnResponse)
	}

	#' List TempBudgetVersionAccountDetails
	#'
	#' This function returns a dataframe or json object of TempBudgetVersionAccountDetails
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given TempBudgetVersionAccountDetails. Defaults to FALSE for all return fields which, for convenience, returns all fields for the TempBudgetVersionAccountDetails.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object listed as text. Run \code{\link{getSchemaForObjects}}('TempBudgetVersionAccountDetail') to get more field paths.
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
	#' @concept Budgeting
	#' @return A list of TempBudgetVersionAccountDetails
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	listTempBudgetVersionAccountDetails <- function(searchConditionsList = NULL, TempBudgetVersionAccountDetailID = F, BudgetVersionAccountID = F, DisplayAccount = F, Amount = F, Description = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, fieldPaths = NULL, searchConditionsGroupType = "And", searchSortFieldNamesList = NULL, searchSortFieldNamesDescendingList = NULL, entityId = 1, schoolYearId = NULL, page = 1, pageSize = 100000, flatten = T, returnResponse = F){

		params <- as.list(environment())

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		listSkyObjects(module = "Budgeting", objectName = "TempBudgetVersionAccountDetail", searchFields = searchFields %>% append(fieldPaths), page = page, pageSize = pageSize, SearchConditionsList = searchConditionsList, SearchConditionsGroupType = searchConditionsGroupType, SearchSortFieldNamesList = searchSortFieldNamesList, SearchSortFieldNamesDescendingList = searchSortFieldNamesDescendingList, entityId = entityId, schoolYearId = schoolYearId, flatten = flatten, returnResponse = returnResponse)
	}

	#' Get a TempBudgetVersionAccountDetail
	#'
	#' This function returns a dataframe or json object of a TempBudgetVersionAccountDetail
	#' @param TempBudgetVersionAccountDetailID The ID of the TempBudgetVersionAccountDetail to return.
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given TempBudgetVersionAccountDetail. Defaults to FALSE for all return fields which, for convenience, returns all fields for the TempBudgetVersionAccountDetail.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object listed as text. Run \code{\link{getSchemaForObjects}}('TempBudgetVersionAccountDetail') to get more field paths.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{listSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Budgeting
	#' @return A dataframe or of TempBudgetVersionAccountDetail
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	getTempBudgetVersionAccountDetail <- function(TempBudgetVersionAccountDetailID, BudgetVersionAccountID = F, DisplayAccount = F, Amount = F, Description = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, fieldPaths = NULL, entityId = 1, schoolYearId = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment()) %>% purrr::keep(names(.) != "TempBudgetVersionAccountDetailID")

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		getSkyObject(module = "Budgeting", objectName = "TempBudgetVersionAccountDetail", objectId = TempBudgetVersionAccountDetailID, searchFields = searchFields, entityId = entityId, flatten = flatten, returnResponse = returnResponse)
	}

	#' Delete a TempBudgetVersionAccountDetail
	#'
	#' This function deletes a TempBudgetVersionAccountDetail
	#' @param TempBudgetVersionAccountDetailID The ID of the TempBudgetVersionAccountDetail to delete
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{listSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Budgeting
	#' @return The TempBudgetVersionAccountDetailID of the deleted TempBudgetVersionAccountDetail.
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	deleteTempBudgetVersionAccountDetail <- function(TempBudgetVersionAccountDetailID, ignoreWarnings = F, entityId = 1, schoolYearId = NULL, flatten = T, returnResponse = F){

		deleteSkyObject(module = "Budgeting", objectName = "TempBudgetVersionAccountDetail", objectId = TempBudgetVersionAccountDetailID, ignoreWarnings = ignoreWarnings, entityId = entityId, flatten = flatten, returnResponse = returnResponse)
	}

	#' Create a TempBudgetVersionAccountDetail
	#'
	#' This function creates a TempBudgetVersionAccountDetail
	#' @param fieldNames The field values to give the created TempBudgetVersionAccountDetail. Each defaults to NULL.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{listSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Budgeting
	#' @return A newly created TempBudgetVersionAccountDetail
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	createTempBudgetVersionAccountDetail <- function(BudgetVersionAccountID = NULL, DisplayAccount = NULL, Amount = NULL, Description = NULL, entityId = 1, schoolYearId = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment())

		body <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper()) %>% purrr::compact()

		createSkyObject(module = "Budgeting", objectName = "TempBudgetVersionAccountDetail", body = list(DataObject = body), searchFields = append("TempBudgetVersionAccountDetailID", body %>% names()), entityId = entityId, schoolYearId = schoolYearId, flatten = flatten, returnResponse = returnResponse)
	}

	#' Modify a TempBudgetVersionAccountDetail
	#'
	#' This function modifies a TempBudgetVersionAccountDetail
	#' @param fieldNames The field values to give the modified TempBudgetVersionAccountDetail. Each defaults to NULL.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{listSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Budgeting
	#' @return The modified TempBudgetVersionAccountDetail
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	modifyTempBudgetVersionAccountDetail <- function(TempBudgetVersionAccountDetailID, BudgetVersionAccountID = NULL, DisplayAccount = NULL, Amount = NULL, Description = NULL, entityId = 1, schoolYearId = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment())

		body <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper()) %>% purrr::compact()

		modifySkyObject(module = "Budgeting", objectName = "TempBudgetVersionAccountDetail", objectId = TempBudgetVersionAccountDetailID, body = list(DataObject = body), searchFields = append("TempBudgetVersionAccountDetailID", body %>% names()), entityId = entityId, schoolYearId = schoolYearId, flatten = flatten, returnResponse = returnResponse)
	}

	#' List TempBudgetAmendments
	#'
	#' This function returns a dataframe or json object of TempBudgetAmendments
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given TempBudgetAmendments. Defaults to FALSE for all return fields which, for convenience, returns all fields for the TempBudgetAmendments.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object listed as text. Run \code{\link{getSchemaForObjects}}('TempBudgetAmendment') to get more field paths.
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
	#' @concept Budgeting
	#' @return A list of TempBudgetAmendments
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	listTempBudgetAmendments <- function(searchConditionsList = NULL, TempBudgetAmendmentID = F, BudgetAmendmentID = F, Batch = F, Status = F, Type = F, Description = F, BudgetAmendmentGroup = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, fieldPaths = NULL, searchConditionsGroupType = "And", searchSortFieldNamesList = NULL, searchSortFieldNamesDescendingList = NULL, entityId = 1, schoolYearId = NULL, page = 1, pageSize = 100000, flatten = T, returnResponse = F){

		params <- as.list(environment())

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		listSkyObjects(module = "Budgeting", objectName = "TempBudgetAmendment", searchFields = searchFields %>% append(fieldPaths), page = page, pageSize = pageSize, SearchConditionsList = searchConditionsList, SearchConditionsGroupType = searchConditionsGroupType, SearchSortFieldNamesList = searchSortFieldNamesList, SearchSortFieldNamesDescendingList = searchSortFieldNamesDescendingList, entityId = entityId, schoolYearId = schoolYearId, flatten = flatten, returnResponse = returnResponse)
	}

	#' Get a TempBudgetAmendment
	#'
	#' This function returns a dataframe or json object of a TempBudgetAmendment
	#' @param TempBudgetAmendmentID The ID of the TempBudgetAmendment to return.
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given TempBudgetAmendment. Defaults to FALSE for all return fields which, for convenience, returns all fields for the TempBudgetAmendment.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object listed as text. Run \code{\link{getSchemaForObjects}}('TempBudgetAmendment') to get more field paths.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{listSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Budgeting
	#' @return A dataframe or of TempBudgetAmendment
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	getTempBudgetAmendment <- function(TempBudgetAmendmentID, BudgetAmendmentID = F, Batch = F, Status = F, Type = F, Description = F, BudgetAmendmentGroup = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, fieldPaths = NULL, entityId = 1, schoolYearId = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment()) %>% purrr::keep(names(.) != "TempBudgetAmendmentID")

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		getSkyObject(module = "Budgeting", objectName = "TempBudgetAmendment", objectId = TempBudgetAmendmentID, searchFields = searchFields, entityId = entityId, flatten = flatten, returnResponse = returnResponse)
	}

	#' Delete a TempBudgetAmendment
	#'
	#' This function deletes a TempBudgetAmendment
	#' @param TempBudgetAmendmentID The ID of the TempBudgetAmendment to delete
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{listSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Budgeting
	#' @return The TempBudgetAmendmentID of the deleted TempBudgetAmendment.
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	deleteTempBudgetAmendment <- function(TempBudgetAmendmentID, ignoreWarnings = F, entityId = 1, schoolYearId = NULL, flatten = T, returnResponse = F){

		deleteSkyObject(module = "Budgeting", objectName = "TempBudgetAmendment", objectId = TempBudgetAmendmentID, ignoreWarnings = ignoreWarnings, entityId = entityId, flatten = flatten, returnResponse = returnResponse)
	}

	#' Create a TempBudgetAmendment
	#'
	#' This function creates a TempBudgetAmendment
	#' @param fieldNames The field values to give the created TempBudgetAmendment. Each defaults to NULL.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{listSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Budgeting
	#' @return A newly created TempBudgetAmendment
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	createTempBudgetAmendment <- function(BudgetAmendmentID = NULL, Batch = NULL, Status = NULL, Type = NULL, Description = NULL, BudgetAmendmentGroup = NULL, entityId = 1, schoolYearId = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment())

		body <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper()) %>% purrr::compact()

		createSkyObject(module = "Budgeting", objectName = "TempBudgetAmendment", body = list(DataObject = body), searchFields = append("TempBudgetAmendmentID", body %>% names()), entityId = entityId, schoolYearId = schoolYearId, flatten = flatten, returnResponse = returnResponse)
	}

	#' Modify a TempBudgetAmendment
	#'
	#' This function modifies a TempBudgetAmendment
	#' @param fieldNames The field values to give the modified TempBudgetAmendment. Each defaults to NULL.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{listSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Budgeting
	#' @return The modified TempBudgetAmendment
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	modifyTempBudgetAmendment <- function(TempBudgetAmendmentID, BudgetAmendmentID = NULL, Batch = NULL, Status = NULL, Type = NULL, Description = NULL, BudgetAmendmentGroup = NULL, entityId = 1, schoolYearId = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment())

		body <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper()) %>% purrr::compact()

		modifySkyObject(module = "Budgeting", objectName = "TempBudgetAmendment", objectId = TempBudgetAmendmentID, body = list(DataObject = body), searchFields = append("TempBudgetAmendmentID", body %>% names()), entityId = entityId, schoolYearId = schoolYearId, flatten = flatten, returnResponse = returnResponse)
	}
