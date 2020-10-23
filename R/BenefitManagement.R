
	#' List BenefitGroups
	#'
	#' This function returns a dataframe or json object of BenefitGroups
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given BenefitGroups. Defaults to FALSE for all return fields which, for convenience, returns all fields for the BenefitGroups.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object listed as text. Run \code{\link{getSchemaForObjects}}('BenefitGroup') to get more field paths.
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
	#' @concept Benefit Management
	#' @return A list of BenefitGroups
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	listBenefitGroups <- function(searchConditionsList = NULL, BenefitGroupID = F, DistrictID = F, Code = F, Description = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, CodeDescription = F, fieldPaths = NULL, searchConditionsGroupType = "And", searchSortFieldNamesList = NULL, searchSortFieldNamesDescendingList = NULL, entityId = 1, schoolYearId = NULL, page = 1, pageSize = 100000, flatten = T, returnResponse = F){

		params <- as.list(environment())

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		listSkyObjects(module = "BenefitManagement", objectName = "BenefitGroup", searchFields = searchFields %>% append(fieldPaths), page = page, pageSize = pageSize, SearchConditionsList = searchConditionsList, SearchConditionsGroupType = searchConditionsGroupType, SearchSortFieldNamesList = searchSortFieldNamesList, SearchSortFieldNamesDescendingList = searchSortFieldNamesDescendingList, entityId = entityId, schoolYearId = schoolYearId, flatten = flatten, returnResponse = returnResponse)
	}

	#' Get a BenefitGroup
	#'
	#' This function returns a dataframe or json object of a BenefitGroup
	#' @param BenefitGroupID The ID of the BenefitGroup to return.
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given BenefitGroup. Defaults to FALSE for all return fields which, for convenience, returns all fields for the BenefitGroup.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object listed as text. Run \code{\link{getSchemaForObjects}}('BenefitGroup') to get more field paths.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{listSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Benefit Management
	#' @return A dataframe or of BenefitGroup
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	getBenefitGroup <- function(BenefitGroupID, DistrictID = F, Code = F, Description = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, CodeDescription = F, fieldPaths = NULL, entityId = 1, schoolYearId = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment()) %>% purrr::keep(names(.) != "BenefitGroupID")

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		getSkyObject(module = "BenefitManagement", objectName = "BenefitGroup", objectId = BenefitGroupID, searchFields = searchFields, entityId = entityId, flatten = flatten, returnResponse = returnResponse)
	}

	#' Delete a BenefitGroup
	#'
	#' This function deletes a BenefitGroup
	#' @param BenefitGroupID The ID of the BenefitGroup to delete
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{listSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Benefit Management
	#' @return The BenefitGroupID of the deleted BenefitGroup.
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	deleteBenefitGroup <- function(BenefitGroupID, ignoreWarnings = F, entityId = 1, schoolYearId = NULL, flatten = T, returnResponse = F){

		deleteSkyObject(module = "BenefitManagement", objectName = "BenefitGroup", objectId = BenefitGroupID, ignoreWarnings = ignoreWarnings, entityId = entityId, flatten = flatten, returnResponse = returnResponse)
	}

	#' Create a BenefitGroup
	#'
	#' This function creates a BenefitGroup
	#' @param fieldNames The field values to give the created BenefitGroup. Each defaults to NULL.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{listSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Benefit Management
	#' @return A newly created BenefitGroup
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	createBenefitGroup <- function(DistrictID = NULL, Code = NULL, Description = NULL, entityId = 1, schoolYearId = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment())

		body <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper()) %>% purrr::compact()

		createSkyObject(module = "BenefitManagement", objectName = "BenefitGroup", body = list(DataObject = body), searchFields = append("BenefitGroupID", body %>% names()), entityId = entityId, schoolYearId = schoolYearId, flatten = flatten, returnResponse = returnResponse)
	}

	#' Modify a BenefitGroup
	#'
	#' This function modifies a BenefitGroup
	#' @param fieldNames The field values to give the modified BenefitGroup. Each defaults to NULL.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{listSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Benefit Management
	#' @return The modified BenefitGroup
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	modifyBenefitGroup <- function(BenefitGroupID, DistrictID = NULL, Code = NULL, Description = NULL, entityId = 1, schoolYearId = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment())

		body <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper()) %>% purrr::compact()

		modifySkyObject(module = "BenefitManagement", objectName = "BenefitGroup", objectId = BenefitGroupID, body = list(DataObject = body), searchFields = append("BenefitGroupID", body %>% names()), entityId = entityId, schoolYearId = schoolYearId, flatten = flatten, returnResponse = returnResponse)
	}

	#' List Coverages
	#'
	#' This function returns a dataframe or json object of Coverages
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given Coverages. Defaults to FALSE for all return fields which, for convenience, returns all fields for the Coverages.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object listed as text. Run \code{\link{getSchemaForObjects}}('Coverage') to get more field paths.
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
	#' @concept Benefit Management
	#' @return A list of Coverages
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	listCoverages <- function(searchConditionsList = NULL, CoverageID = F, PlanID = F, Description = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, AllowedDependentsXML = F, DependentMessage = F, MaximumDependentCount = F, AllowDependents = F, AllowDependentCount = F, HasAgeBasedRates = F, AgeBasedCoverageAmount = F, AgeAsOfMonthDay = F, AgeAsOfMonthNumber = F, AgeAsOfMonth = F, AgeAsOfDay = F, PlanBenefitIDDefaultOverride = F, PlanDeductionIDDefaultOverride = F, fieldPaths = NULL, searchConditionsGroupType = "And", searchSortFieldNamesList = NULL, searchSortFieldNamesDescendingList = NULL, entityId = 1, schoolYearId = NULL, page = 1, pageSize = 100000, flatten = T, returnResponse = F){

		params <- as.list(environment())

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		listSkyObjects(module = "BenefitManagement", objectName = "Coverage", searchFields = searchFields %>% append(fieldPaths), page = page, pageSize = pageSize, SearchConditionsList = searchConditionsList, SearchConditionsGroupType = searchConditionsGroupType, SearchSortFieldNamesList = searchSortFieldNamesList, SearchSortFieldNamesDescendingList = searchSortFieldNamesDescendingList, entityId = entityId, schoolYearId = schoolYearId, flatten = flatten, returnResponse = returnResponse)
	}

	#' Get a Coverage
	#'
	#' This function returns a dataframe or json object of a Coverage
	#' @param CoverageID The ID of the Coverage to return.
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given Coverage. Defaults to FALSE for all return fields which, for convenience, returns all fields for the Coverage.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object listed as text. Run \code{\link{getSchemaForObjects}}('Coverage') to get more field paths.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{listSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Benefit Management
	#' @return A dataframe or of Coverage
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	getCoverage <- function(CoverageID, PlanID = F, Description = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, AllowedDependentsXML = F, DependentMessage = F, MaximumDependentCount = F, AllowDependents = F, AllowDependentCount = F, HasAgeBasedRates = F, AgeBasedCoverageAmount = F, AgeAsOfMonthDay = F, AgeAsOfMonthNumber = F, AgeAsOfMonth = F, AgeAsOfDay = F, PlanBenefitIDDefaultOverride = F, PlanDeductionIDDefaultOverride = F, fieldPaths = NULL, entityId = 1, schoolYearId = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment()) %>% purrr::keep(names(.) != "CoverageID")

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		getSkyObject(module = "BenefitManagement", objectName = "Coverage", objectId = CoverageID, searchFields = searchFields, entityId = entityId, flatten = flatten, returnResponse = returnResponse)
	}

	#' Delete a Coverage
	#'
	#' This function deletes a Coverage
	#' @param CoverageID The ID of the Coverage to delete
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{listSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Benefit Management
	#' @return The CoverageID of the deleted Coverage.
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	deleteCoverage <- function(CoverageID, ignoreWarnings = F, entityId = 1, schoolYearId = NULL, flatten = T, returnResponse = F){

		deleteSkyObject(module = "BenefitManagement", objectName = "Coverage", objectId = CoverageID, ignoreWarnings = ignoreWarnings, entityId = entityId, flatten = flatten, returnResponse = returnResponse)
	}

	#' Create a Coverage
	#'
	#' This function creates a Coverage
	#' @param fieldNames The field values to give the created Coverage. Each defaults to NULL.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{listSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Benefit Management
	#' @return A newly created Coverage
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	createCoverage <- function(PlanID = NULL, Description = NULL, DependentMessage = NULL, MaximumDependentCount = NULL, HasAgeBasedRates = NULL, AgeBasedCoverageAmount = NULL, AgeAsOfMonthNumber = NULL, AgeAsOfDay = NULL, PlanBenefitIDDefaultOverride = NULL, PlanDeductionIDDefaultOverride = NULL, entityId = 1, schoolYearId = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment())

		body <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper()) %>% purrr::compact()

		createSkyObject(module = "BenefitManagement", objectName = "Coverage", body = list(DataObject = body), searchFields = append("CoverageID", body %>% names()), entityId = entityId, schoolYearId = schoolYearId, flatten = flatten, returnResponse = returnResponse)
	}

	#' Modify a Coverage
	#'
	#' This function modifies a Coverage
	#' @param fieldNames The field values to give the modified Coverage. Each defaults to NULL.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{listSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Benefit Management
	#' @return The modified Coverage
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	modifyCoverage <- function(CoverageID, PlanID = NULL, Description = NULL, DependentMessage = NULL, MaximumDependentCount = NULL, HasAgeBasedRates = NULL, AgeBasedCoverageAmount = NULL, AgeAsOfMonthNumber = NULL, AgeAsOfDay = NULL, PlanBenefitIDDefaultOverride = NULL, PlanDeductionIDDefaultOverride = NULL, entityId = 1, schoolYearId = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment())

		body <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper()) %>% purrr::compact()

		modifySkyObject(module = "BenefitManagement", objectName = "Coverage", objectId = CoverageID, body = list(DataObject = body), searchFields = append("CoverageID", body %>% names()), entityId = entityId, schoolYearId = schoolYearId, flatten = flatten, returnResponse = returnResponse)
	}

	#' List BenefitManagementPlans
	#'
	#' This function returns a dataframe or json object of BenefitManagementPlans
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given BenefitManagementPlans. Defaults to FALSE for all return fields which, for convenience, returns all fields for the BenefitManagementPlans.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object listed as text. Run \code{\link{getSchemaForObjects}}('BenefitManagementPlan') to get more field paths.
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
	#' @concept Benefit Management
	#' @return A list of BenefitManagementPlans
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	listBenefitManagementPlans <- function(searchConditionsList = NULL, PlanID = F, DistrictID = F, VendorID = F, Code = F, Description = F, PaymentStartDate = F, PaymentEndDate = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, CodeDescription = F, AccountIDAccrual = F, BenefitIDDefault = F, DeductionIDDefault = F, IsPaymentOneMonthBeforeCoverage = F, fieldPaths = NULL, searchConditionsGroupType = "And", searchSortFieldNamesList = NULL, searchSortFieldNamesDescendingList = NULL, entityId = 1, schoolYearId = NULL, page = 1, pageSize = 100000, flatten = T, returnResponse = F){

		params <- as.list(environment())

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		listSkyObjects(module = "BenefitManagement", objectName = "Plan", searchFields = searchFields %>% append(fieldPaths), page = page, pageSize = pageSize, SearchConditionsList = searchConditionsList, SearchConditionsGroupType = searchConditionsGroupType, SearchSortFieldNamesList = searchSortFieldNamesList, SearchSortFieldNamesDescendingList = searchSortFieldNamesDescendingList, entityId = entityId, schoolYearId = schoolYearId, flatten = flatten, returnResponse = returnResponse)
	}

	#' Get a BenefitManagementPlan
	#'
	#' This function returns a dataframe or json object of a BenefitManagementPlan
	#' @param BenefitManagementPlanID The ID of the BenefitManagementPlan to return.
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given BenefitManagementPlan. Defaults to FALSE for all return fields which, for convenience, returns all fields for the BenefitManagementPlan.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object listed as text. Run \code{\link{getSchemaForObjects}}('BenefitManagementPlan') to get more field paths.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{listSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Benefit Management
	#' @return A dataframe or of BenefitManagementPlan
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	getBenefitManagementPlan <- function(BenefitManagementPlanID, PlanID = F, DistrictID = F, VendorID = F, Code = F, Description = F, PaymentStartDate = F, PaymentEndDate = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, CodeDescription = F, AccountIDAccrual = F, BenefitIDDefault = F, DeductionIDDefault = F, IsPaymentOneMonthBeforeCoverage = F, fieldPaths = NULL, entityId = 1, schoolYearId = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment()) %>% purrr::keep(names(.) != "BenefitManagementPlanID")

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		getSkyObject(module = "BenefitManagement", objectName = "Plan", objectId = BenefitManagementPlanID, searchFields = searchFields, entityId = entityId, flatten = flatten, returnResponse = returnResponse)
	}

	#' Delete a BenefitManagementPlan
	#'
	#' This function deletes a BenefitManagementPlan
	#' @param BenefitManagementPlanID The ID of the BenefitManagementPlan to delete
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{listSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Benefit Management
	#' @return The BenefitManagementPlanID of the deleted BenefitManagementPlan.
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	deleteBenefitManagementPlan <- function(BenefitManagementPlanID, ignoreWarnings = F, entityId = 1, schoolYearId = NULL, flatten = T, returnResponse = F){

		deleteSkyObject(module = "BenefitManagement", objectName = "Plan", objectId = BenefitManagementPlanID, ignoreWarnings = ignoreWarnings, entityId = entityId, flatten = flatten, returnResponse = returnResponse)
	}

	#' Create a BenefitManagementPlan
	#'
	#' This function creates a BenefitManagementPlan
	#' @param fieldNames The field values to give the created BenefitManagementPlan. Each defaults to NULL.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{listSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Benefit Management
	#' @return A newly created BenefitManagementPlan
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	createBenefitManagementPlan <- function(DistrictID = NULL, VendorID = NULL, Code = NULL, Description = NULL, PaymentStartDate = NULL, PaymentEndDate = NULL, AccountIDAccrual = NULL, BenefitIDDefault = NULL, DeductionIDDefault = NULL, IsPaymentOneMonthBeforeCoverage = NULL, entityId = 1, schoolYearId = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment())

		body <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper()) %>% purrr::compact()

		createSkyObject(module = "BenefitManagement", objectName = "Plan", body = list(DataObject = body), searchFields = append("PlanID", body %>% names()), entityId = entityId, schoolYearId = schoolYearId, flatten = flatten, returnResponse = returnResponse)
	}

	#' Modify a BenefitManagementPlan
	#'
	#' This function modifies a BenefitManagementPlan
	#' @param fieldNames The field values to give the modified BenefitManagementPlan. Each defaults to NULL.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{listSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Benefit Management
	#' @return The modified BenefitManagementPlan
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	modifyBenefitManagementPlan <- function(PlanID, DistrictID = NULL, VendorID = NULL, Code = NULL, Description = NULL, PaymentStartDate = NULL, PaymentEndDate = NULL, AccountIDAccrual = NULL, BenefitIDDefault = NULL, DeductionIDDefault = NULL, IsPaymentOneMonthBeforeCoverage = NULL, entityId = 1, schoolYearId = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment())

		body <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper()) %>% purrr::compact()

		modifySkyObject(module = "BenefitManagement", objectName = "Plan", objectId = PlanID, body = list(DataObject = body), searchFields = append("PlanID", body %>% names()), entityId = entityId, schoolYearId = schoolYearId, flatten = flatten, returnResponse = returnResponse)
	}

	#' List SubPlans
	#'
	#' This function returns a dataframe or json object of SubPlans
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given SubPlans. Defaults to FALSE for all return fields which, for convenience, returns all fields for the SubPlans.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object listed as text. Run \code{\link{getSchemaForObjects}}('SubPlan') to get more field paths.
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
	#' @concept Benefit Management
	#' @return A list of SubPlans
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	listSubPlans <- function(searchConditionsList = NULL, SubPlanID = F, BenefitGroupID = F, CoverageID = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, SubPlanIdentifier = F, RateType = F, BenefitFTERangeID = F, SalaryMultiplier = F, SalaryCoverageAmountMaximum = F, fieldPaths = NULL, searchConditionsGroupType = "And", searchSortFieldNamesList = NULL, searchSortFieldNamesDescendingList = NULL, entityId = 1, schoolYearId = NULL, page = 1, pageSize = 100000, flatten = T, returnResponse = F){

		params <- as.list(environment())

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		listSkyObjects(module = "BenefitManagement", objectName = "SubPlan", searchFields = searchFields %>% append(fieldPaths), page = page, pageSize = pageSize, SearchConditionsList = searchConditionsList, SearchConditionsGroupType = searchConditionsGroupType, SearchSortFieldNamesList = searchSortFieldNamesList, SearchSortFieldNamesDescendingList = searchSortFieldNamesDescendingList, entityId = entityId, schoolYearId = schoolYearId, flatten = flatten, returnResponse = returnResponse)
	}

	#' Get a SubPlan
	#'
	#' This function returns a dataframe or json object of a SubPlan
	#' @param SubPlanID The ID of the SubPlan to return.
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given SubPlan. Defaults to FALSE for all return fields which, for convenience, returns all fields for the SubPlan.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object listed as text. Run \code{\link{getSchemaForObjects}}('SubPlan') to get more field paths.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{listSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Benefit Management
	#' @return A dataframe or of SubPlan
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	getSubPlan <- function(SubPlanID, BenefitGroupID = F, CoverageID = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, SubPlanIdentifier = F, RateType = F, BenefitFTERangeID = F, SalaryMultiplier = F, SalaryCoverageAmountMaximum = F, fieldPaths = NULL, entityId = 1, schoolYearId = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment()) %>% purrr::keep(names(.) != "SubPlanID")

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		getSkyObject(module = "BenefitManagement", objectName = "SubPlan", objectId = SubPlanID, searchFields = searchFields, entityId = entityId, flatten = flatten, returnResponse = returnResponse)
	}

	#' Delete a SubPlan
	#'
	#' This function deletes a SubPlan
	#' @param SubPlanID The ID of the SubPlan to delete
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{listSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Benefit Management
	#' @return The SubPlanID of the deleted SubPlan.
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	deleteSubPlan <- function(SubPlanID, ignoreWarnings = F, entityId = 1, schoolYearId = NULL, flatten = T, returnResponse = F){

		deleteSkyObject(module = "BenefitManagement", objectName = "SubPlan", objectId = SubPlanID, ignoreWarnings = ignoreWarnings, entityId = entityId, flatten = flatten, returnResponse = returnResponse)
	}

	#' Create a SubPlan
	#'
	#' This function creates a SubPlan
	#' @param fieldNames The field values to give the created SubPlan. Each defaults to NULL.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{listSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Benefit Management
	#' @return A newly created SubPlan
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	createSubPlan <- function(BenefitGroupID = NULL, CoverageID = NULL, RateType = NULL, BenefitFTERangeID = NULL, SalaryMultiplier = NULL, SalaryCoverageAmountMaximum = NULL, entityId = 1, schoolYearId = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment())

		body <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper()) %>% purrr::compact()

		createSkyObject(module = "BenefitManagement", objectName = "SubPlan", body = list(DataObject = body), searchFields = append("SubPlanID", body %>% names()), entityId = entityId, schoolYearId = schoolYearId, flatten = flatten, returnResponse = returnResponse)
	}

	#' Modify a SubPlan
	#'
	#' This function modifies a SubPlan
	#' @param fieldNames The field values to give the modified SubPlan. Each defaults to NULL.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{listSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Benefit Management
	#' @return The modified SubPlan
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	modifySubPlan <- function(SubPlanID, BenefitGroupID = NULL, CoverageID = NULL, RateType = NULL, BenefitFTERangeID = NULL, SalaryMultiplier = NULL, SalaryCoverageAmountMaximum = NULL, entityId = 1, schoolYearId = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment())

		body <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper()) %>% purrr::compact()

		modifySkyObject(module = "BenefitManagement", objectName = "SubPlan", objectId = SubPlanID, body = list(DataObject = body), searchFields = append("SubPlanID", body %>% names()), entityId = entityId, schoolYearId = schoolYearId, flatten = flatten, returnResponse = returnResponse)
	}

	#' List SubPlanRates
	#'
	#' This function returns a dataframe or json object of SubPlanRates
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given SubPlanRates. Defaults to FALSE for all return fields which, for convenience, returns all fields for the SubPlanRates.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object listed as text. Run \code{\link{getSchemaForObjects}}('SubPlanRate') to get more field paths.
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
	#' @concept Benefit Management
	#' @return A list of SubPlanRates
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	listSubPlanRates <- function(searchConditionsList = NULL, SubPlanRateID = F, SubPlanID = F, CoverageStartDate = F, CoverageEndDate = F, DeductionAmount = F, BenefitAmount = F, PremiumAmount = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, BenefitCostPerThousand = F, DeductionCostPerThousand = F, CalculateWithFTE = F, UsesFTE = F, SubPlanRateIDClonedFrom = F, CoverageAgeRangeID = F, BenefitPercentage = F, DeductionPercentage = F, AgeCalculatedRateBase = F, fieldPaths = NULL, searchConditionsGroupType = "And", searchSortFieldNamesList = NULL, searchSortFieldNamesDescendingList = NULL, entityId = 1, schoolYearId = NULL, page = 1, pageSize = 100000, flatten = T, returnResponse = F){

		params <- as.list(environment())

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		listSkyObjects(module = "BenefitManagement", objectName = "SubPlanRate", searchFields = searchFields %>% append(fieldPaths), page = page, pageSize = pageSize, SearchConditionsList = searchConditionsList, SearchConditionsGroupType = searchConditionsGroupType, SearchSortFieldNamesList = searchSortFieldNamesList, SearchSortFieldNamesDescendingList = searchSortFieldNamesDescendingList, entityId = entityId, schoolYearId = schoolYearId, flatten = flatten, returnResponse = returnResponse)
	}

	#' Get a SubPlanRate
	#'
	#' This function returns a dataframe or json object of a SubPlanRate
	#' @param SubPlanRateID The ID of the SubPlanRate to return.
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given SubPlanRate. Defaults to FALSE for all return fields which, for convenience, returns all fields for the SubPlanRate.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object listed as text. Run \code{\link{getSchemaForObjects}}('SubPlanRate') to get more field paths.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{listSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Benefit Management
	#' @return A dataframe or of SubPlanRate
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	getSubPlanRate <- function(SubPlanRateID, SubPlanID = F, CoverageStartDate = F, CoverageEndDate = F, DeductionAmount = F, BenefitAmount = F, PremiumAmount = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, BenefitCostPerThousand = F, DeductionCostPerThousand = F, CalculateWithFTE = F, UsesFTE = F, SubPlanRateIDClonedFrom = F, CoverageAgeRangeID = F, BenefitPercentage = F, DeductionPercentage = F, AgeCalculatedRateBase = F, fieldPaths = NULL, entityId = 1, schoolYearId = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment()) %>% purrr::keep(names(.) != "SubPlanRateID")

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		getSkyObject(module = "BenefitManagement", objectName = "SubPlanRate", objectId = SubPlanRateID, searchFields = searchFields, entityId = entityId, flatten = flatten, returnResponse = returnResponse)
	}

	#' Delete a SubPlanRate
	#'
	#' This function deletes a SubPlanRate
	#' @param SubPlanRateID The ID of the SubPlanRate to delete
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{listSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Benefit Management
	#' @return The SubPlanRateID of the deleted SubPlanRate.
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	deleteSubPlanRate <- function(SubPlanRateID, ignoreWarnings = F, entityId = 1, schoolYearId = NULL, flatten = T, returnResponse = F){

		deleteSkyObject(module = "BenefitManagement", objectName = "SubPlanRate", objectId = SubPlanRateID, ignoreWarnings = ignoreWarnings, entityId = entityId, flatten = flatten, returnResponse = returnResponse)
	}

	#' Create a SubPlanRate
	#'
	#' This function creates a SubPlanRate
	#' @param fieldNames The field values to give the created SubPlanRate. Each defaults to NULL.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{listSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Benefit Management
	#' @return A newly created SubPlanRate
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	createSubPlanRate <- function(SubPlanID = NULL, CoverageStartDate = NULL, CoverageEndDate = NULL, DeductionAmount = NULL, BenefitAmount = NULL, PremiumAmount = NULL, BenefitCostPerThousand = NULL, DeductionCostPerThousand = NULL, CalculateWithFTE = NULL, SubPlanRateIDClonedFrom = NULL, CoverageAgeRangeID = NULL, BenefitPercentage = NULL, DeductionPercentage = NULL, AgeCalculatedRateBase = NULL, entityId = 1, schoolYearId = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment())

		body <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper()) %>% purrr::compact()

		createSkyObject(module = "BenefitManagement", objectName = "SubPlanRate", body = list(DataObject = body), searchFields = append("SubPlanRateID", body %>% names()), entityId = entityId, schoolYearId = schoolYearId, flatten = flatten, returnResponse = returnResponse)
	}

	#' Modify a SubPlanRate
	#'
	#' This function modifies a SubPlanRate
	#' @param fieldNames The field values to give the modified SubPlanRate. Each defaults to NULL.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{listSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Benefit Management
	#' @return The modified SubPlanRate
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	modifySubPlanRate <- function(SubPlanRateID, SubPlanID = NULL, CoverageStartDate = NULL, CoverageEndDate = NULL, DeductionAmount = NULL, BenefitAmount = NULL, PremiumAmount = NULL, BenefitCostPerThousand = NULL, DeductionCostPerThousand = NULL, CalculateWithFTE = NULL, SubPlanRateIDClonedFrom = NULL, CoverageAgeRangeID = NULL, BenefitPercentage = NULL, DeductionPercentage = NULL, AgeCalculatedRateBase = NULL, entityId = 1, schoolYearId = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment())

		body <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper()) %>% purrr::compact()

		modifySkyObject(module = "BenefitManagement", objectName = "SubPlanRate", objectId = SubPlanRateID, body = list(DataObject = body), searchFields = append("SubPlanRateID", body %>% names()), entityId = entityId, schoolYearId = schoolYearId, flatten = flatten, returnResponse = returnResponse)
	}

	#' List EmployeePlanEnrollments
	#'
	#' This function returns a dataframe or json object of EmployeePlanEnrollments
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given EmployeePlanEnrollments. Defaults to FALSE for all return fields which, for convenience, returns all fields for the EmployeePlanEnrollments.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object listed as text. Run \code{\link{getSchemaForObjects}}('EmployeePlanEnrollment') to get more field paths.
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
	#' @concept Benefit Management
	#' @return A list of EmployeePlanEnrollments
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	listEmployeePlanEnrollments <- function(searchConditionsList = NULL, EmployeePlanEnrollmentID = F, EmployeeID = F, PlanID = F, PaymentStartDate = F, PaymentEndDate = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, CoverageStartDate = F, CoverageEndDate = F, DeductionBenefitScheduleID = F, TotalPremiums = F, TotalPremiumsCollected = F, RemainingPays = F, CurrentPerPayrollPremium = F, EmployeePlanEnrollmentIDClonedFrom = F, TotalPremiumsUncollected = F, BenefitsPaidToDate = F, DeductionsPaidToDate = F, CashReceiptAccountDistributionsToDate = F, TotalPremiumsToDate = F, TotalPremiumsCollectedToDate = F, CurrentAmountDue = F, BilledAmount = F, PaymentDateRange = F, fieldPaths = NULL, searchConditionsGroupType = "And", searchSortFieldNamesList = NULL, searchSortFieldNamesDescendingList = NULL, entityId = 1, schoolYearId = NULL, page = 1, pageSize = 100000, flatten = T, returnResponse = F){

		params <- as.list(environment())

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		listSkyObjects(module = "BenefitManagement", objectName = "EmployeePlanEnrollment", searchFields = searchFields %>% append(fieldPaths), page = page, pageSize = pageSize, SearchConditionsList = searchConditionsList, SearchConditionsGroupType = searchConditionsGroupType, SearchSortFieldNamesList = searchSortFieldNamesList, SearchSortFieldNamesDescendingList = searchSortFieldNamesDescendingList, entityId = entityId, schoolYearId = schoolYearId, flatten = flatten, returnResponse = returnResponse)
	}

	#' Get an EmployeePlanEnrollment
	#'
	#' This function returns a dataframe or json object of an EmployeePlanEnrollment
	#' @param EmployeePlanEnrollmentID The ID of the EmployeePlanEnrollment to return.
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given EmployeePlanEnrollment. Defaults to FALSE for all return fields which, for convenience, returns all fields for the EmployeePlanEnrollment.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object listed as text. Run \code{\link{getSchemaForObjects}}('EmployeePlanEnrollment') to get more field paths.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{listSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Benefit Management
	#' @return A dataframe or of EmployeePlanEnrollment
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	getEmployeePlanEnrollment <- function(EmployeePlanEnrollmentID, EmployeeID = F, PlanID = F, PaymentStartDate = F, PaymentEndDate = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, CoverageStartDate = F, CoverageEndDate = F, DeductionBenefitScheduleID = F, TotalPremiums = F, TotalPremiumsCollected = F, RemainingPays = F, CurrentPerPayrollPremium = F, EmployeePlanEnrollmentIDClonedFrom = F, TotalPremiumsUncollected = F, BenefitsPaidToDate = F, DeductionsPaidToDate = F, CashReceiptAccountDistributionsToDate = F, TotalPremiumsToDate = F, TotalPremiumsCollectedToDate = F, CurrentAmountDue = F, BilledAmount = F, PaymentDateRange = F, fieldPaths = NULL, entityId = 1, schoolYearId = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment()) %>% purrr::keep(names(.) != "EmployeePlanEnrollmentID")

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		getSkyObject(module = "BenefitManagement", objectName = "EmployeePlanEnrollment", objectId = EmployeePlanEnrollmentID, searchFields = searchFields, entityId = entityId, flatten = flatten, returnResponse = returnResponse)
	}

	#' Delete an EmployeePlanEnrollment
	#'
	#' This function deletes an EmployeePlanEnrollment
	#' @param EmployeePlanEnrollmentID The ID of the EmployeePlanEnrollment to delete
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{listSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Benefit Management
	#' @return The EmployeePlanEnrollmentID of the deleted EmployeePlanEnrollment.
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	deleteEmployeePlanEnrollment <- function(EmployeePlanEnrollmentID, ignoreWarnings = F, entityId = 1, schoolYearId = NULL, flatten = T, returnResponse = F){

		deleteSkyObject(module = "BenefitManagement", objectName = "EmployeePlanEnrollment", objectId = EmployeePlanEnrollmentID, ignoreWarnings = ignoreWarnings, entityId = entityId, flatten = flatten, returnResponse = returnResponse)
	}

	#' Create an EmployeePlanEnrollment
	#'
	#' This function creates an EmployeePlanEnrollment
	#' @param fieldNames The field values to give the created EmployeePlanEnrollment. Each defaults to NULL.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{listSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Benefit Management
	#' @return A newly created EmployeePlanEnrollment
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	createEmployeePlanEnrollment <- function(EmployeeID = NULL, PlanID = NULL, PaymentStartDate = NULL, PaymentEndDate = NULL, CoverageStartDate = NULL, CoverageEndDate = NULL, DeductionBenefitScheduleID = NULL, EmployeePlanEnrollmentIDClonedFrom = NULL, entityId = 1, schoolYearId = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment())

		body <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper()) %>% purrr::compact()

		createSkyObject(module = "BenefitManagement", objectName = "EmployeePlanEnrollment", body = list(DataObject = body), searchFields = append("EmployeePlanEnrollmentID", body %>% names()), entityId = entityId, schoolYearId = schoolYearId, flatten = flatten, returnResponse = returnResponse)
	}

	#' Modify an EmployeePlanEnrollment
	#'
	#' This function modifies an EmployeePlanEnrollment
	#' @param fieldNames The field values to give the modified EmployeePlanEnrollment. Each defaults to NULL.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{listSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Benefit Management
	#' @return The modified EmployeePlanEnrollment
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	modifyEmployeePlanEnrollment <- function(EmployeePlanEnrollmentID, EmployeeID = NULL, PlanID = NULL, PaymentStartDate = NULL, PaymentEndDate = NULL, CoverageStartDate = NULL, CoverageEndDate = NULL, DeductionBenefitScheduleID = NULL, EmployeePlanEnrollmentIDClonedFrom = NULL, entityId = 1, schoolYearId = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment())

		body <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper()) %>% purrr::compact()

		modifySkyObject(module = "BenefitManagement", objectName = "EmployeePlanEnrollment", objectId = EmployeePlanEnrollmentID, body = list(DataObject = body), searchFields = append("EmployeePlanEnrollmentID", body %>% names()), entityId = entityId, schoolYearId = schoolYearId, flatten = flatten, returnResponse = returnResponse)
	}

	#' List CoverageMonths
	#'
	#' This function returns a dataframe or json object of CoverageMonths
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given CoverageMonths. Defaults to FALSE for all return fields which, for convenience, returns all fields for the CoverageMonths.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object listed as text. Run \code{\link{getSchemaForObjects}}('CoverageMonth') to get more field paths.
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
	#' @concept Benefit Management
	#' @return A list of CoverageMonths
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	listCoverageMonths <- function(searchConditionsList = NULL, CoverageMonthID = F, EmployeePlanEnrollmentID = F, SubPlanID = F, Month = F, CalendarYear = F, DeductionAmount = F, BenefitAmount = F, PremiumAmount = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, MonthName = F, CoverageMonthDate = F, CoverageAmount = F, CoverageAmountReadOnly = F, BenefitFTEOverride = F, BenefitFTESource = F, BenefitPercentage = F, BenefitDeductionAmountReadOnly = F, fieldPaths = NULL, searchConditionsGroupType = "And", searchSortFieldNamesList = NULL, searchSortFieldNamesDescendingList = NULL, entityId = 1, schoolYearId = NULL, page = 1, pageSize = 100000, flatten = T, returnResponse = F){

		params <- as.list(environment())

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		listSkyObjects(module = "BenefitManagement", objectName = "CoverageMonth", searchFields = searchFields %>% append(fieldPaths), page = page, pageSize = pageSize, SearchConditionsList = searchConditionsList, SearchConditionsGroupType = searchConditionsGroupType, SearchSortFieldNamesList = searchSortFieldNamesList, SearchSortFieldNamesDescendingList = searchSortFieldNamesDescendingList, entityId = entityId, schoolYearId = schoolYearId, flatten = flatten, returnResponse = returnResponse)
	}

	#' Get a CoverageMonth
	#'
	#' This function returns a dataframe or json object of a CoverageMonth
	#' @param CoverageMonthID The ID of the CoverageMonth to return.
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given CoverageMonth. Defaults to FALSE for all return fields which, for convenience, returns all fields for the CoverageMonth.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object listed as text. Run \code{\link{getSchemaForObjects}}('CoverageMonth') to get more field paths.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{listSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Benefit Management
	#' @return A dataframe or of CoverageMonth
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	getCoverageMonth <- function(CoverageMonthID, EmployeePlanEnrollmentID = F, SubPlanID = F, Month = F, CalendarYear = F, DeductionAmount = F, BenefitAmount = F, PremiumAmount = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, MonthName = F, CoverageMonthDate = F, CoverageAmount = F, CoverageAmountReadOnly = F, BenefitFTEOverride = F, BenefitFTESource = F, BenefitPercentage = F, BenefitDeductionAmountReadOnly = F, fieldPaths = NULL, entityId = 1, schoolYearId = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment()) %>% purrr::keep(names(.) != "CoverageMonthID")

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		getSkyObject(module = "BenefitManagement", objectName = "CoverageMonth", objectId = CoverageMonthID, searchFields = searchFields, entityId = entityId, flatten = flatten, returnResponse = returnResponse)
	}

	#' Delete a CoverageMonth
	#'
	#' This function deletes a CoverageMonth
	#' @param CoverageMonthID The ID of the CoverageMonth to delete
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{listSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Benefit Management
	#' @return The CoverageMonthID of the deleted CoverageMonth.
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	deleteCoverageMonth <- function(CoverageMonthID, ignoreWarnings = F, entityId = 1, schoolYearId = NULL, flatten = T, returnResponse = F){

		deleteSkyObject(module = "BenefitManagement", objectName = "CoverageMonth", objectId = CoverageMonthID, ignoreWarnings = ignoreWarnings, entityId = entityId, flatten = flatten, returnResponse = returnResponse)
	}

	#' Create a CoverageMonth
	#'
	#' This function creates a CoverageMonth
	#' @param fieldNames The field values to give the created CoverageMonth. Each defaults to NULL.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{listSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Benefit Management
	#' @return A newly created CoverageMonth
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	createCoverageMonth <- function(EmployeePlanEnrollmentID = NULL, SubPlanID = NULL, Month = NULL, CalendarYear = NULL, DeductionAmount = NULL, BenefitAmount = NULL, PremiumAmount = NULL, CoverageAmount = NULL, BenefitFTEOverride = NULL, entityId = 1, schoolYearId = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment())

		body <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper()) %>% purrr::compact()

		createSkyObject(module = "BenefitManagement", objectName = "CoverageMonth", body = list(DataObject = body), searchFields = append("CoverageMonthID", body %>% names()), entityId = entityId, schoolYearId = schoolYearId, flatten = flatten, returnResponse = returnResponse)
	}

	#' Modify a CoverageMonth
	#'
	#' This function modifies a CoverageMonth
	#' @param fieldNames The field values to give the modified CoverageMonth. Each defaults to NULL.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{listSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Benefit Management
	#' @return The modified CoverageMonth
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	modifyCoverageMonth <- function(CoverageMonthID, EmployeePlanEnrollmentID = NULL, SubPlanID = NULL, Month = NULL, CalendarYear = NULL, DeductionAmount = NULL, BenefitAmount = NULL, PremiumAmount = NULL, CoverageAmount = NULL, BenefitFTEOverride = NULL, entityId = 1, schoolYearId = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment())

		body <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper()) %>% purrr::compact()

		modifySkyObject(module = "BenefitManagement", objectName = "CoverageMonth", objectId = CoverageMonthID, body = list(DataObject = body), searchFields = append("CoverageMonthID", body %>% names()), entityId = entityId, schoolYearId = schoolYearId, flatten = flatten, returnResponse = returnResponse)
	}

	#' List PlanBenefits
	#'
	#' This function returns a dataframe or json object of PlanBenefits
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given PlanBenefits. Defaults to FALSE for all return fields which, for convenience, returns all fields for the PlanBenefits.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object listed as text. Run \code{\link{getSchemaForObjects}}('PlanBenefit') to get more field paths.
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
	#' @concept Benefit Management
	#' @return A list of PlanBenefits
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	listPlanBenefits <- function(searchConditionsList = NULL, PlanBenefitID = F, PlanID = F, BenefitID = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, fieldPaths = NULL, searchConditionsGroupType = "And", searchSortFieldNamesList = NULL, searchSortFieldNamesDescendingList = NULL, entityId = 1, schoolYearId = NULL, page = 1, pageSize = 100000, flatten = T, returnResponse = F){

		params <- as.list(environment())

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		listSkyObjects(module = "BenefitManagement", objectName = "PlanBenefit", searchFields = searchFields %>% append(fieldPaths), page = page, pageSize = pageSize, SearchConditionsList = searchConditionsList, SearchConditionsGroupType = searchConditionsGroupType, SearchSortFieldNamesList = searchSortFieldNamesList, SearchSortFieldNamesDescendingList = searchSortFieldNamesDescendingList, entityId = entityId, schoolYearId = schoolYearId, flatten = flatten, returnResponse = returnResponse)
	}

	#' Get a PlanBenefit
	#'
	#' This function returns a dataframe or json object of a PlanBenefit
	#' @param PlanBenefitID The ID of the PlanBenefit to return.
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given PlanBenefit. Defaults to FALSE for all return fields which, for convenience, returns all fields for the PlanBenefit.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object listed as text. Run \code{\link{getSchemaForObjects}}('PlanBenefit') to get more field paths.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{listSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Benefit Management
	#' @return A dataframe or of PlanBenefit
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	getPlanBenefit <- function(PlanBenefitID, PlanID = F, BenefitID = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, fieldPaths = NULL, entityId = 1, schoolYearId = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment()) %>% purrr::keep(names(.) != "PlanBenefitID")

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		getSkyObject(module = "BenefitManagement", objectName = "PlanBenefit", objectId = PlanBenefitID, searchFields = searchFields, entityId = entityId, flatten = flatten, returnResponse = returnResponse)
	}

	#' Delete a PlanBenefit
	#'
	#' This function deletes a PlanBenefit
	#' @param PlanBenefitID The ID of the PlanBenefit to delete
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{listSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Benefit Management
	#' @return The PlanBenefitID of the deleted PlanBenefit.
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	deletePlanBenefit <- function(PlanBenefitID, ignoreWarnings = F, entityId = 1, schoolYearId = NULL, flatten = T, returnResponse = F){

		deleteSkyObject(module = "BenefitManagement", objectName = "PlanBenefit", objectId = PlanBenefitID, ignoreWarnings = ignoreWarnings, entityId = entityId, flatten = flatten, returnResponse = returnResponse)
	}

	#' Create a PlanBenefit
	#'
	#' This function creates a PlanBenefit
	#' @param fieldNames The field values to give the created PlanBenefit. Each defaults to NULL.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{listSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Benefit Management
	#' @return A newly created PlanBenefit
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	createPlanBenefit <- function(PlanID = NULL, BenefitID = NULL, entityId = 1, schoolYearId = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment())

		body <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper()) %>% purrr::compact()

		createSkyObject(module = "BenefitManagement", objectName = "PlanBenefit", body = list(DataObject = body), searchFields = append("PlanBenefitID", body %>% names()), entityId = entityId, schoolYearId = schoolYearId, flatten = flatten, returnResponse = returnResponse)
	}

	#' Modify a PlanBenefit
	#'
	#' This function modifies a PlanBenefit
	#' @param fieldNames The field values to give the modified PlanBenefit. Each defaults to NULL.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{listSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Benefit Management
	#' @return The modified PlanBenefit
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	modifyPlanBenefit <- function(PlanBenefitID, PlanID = NULL, BenefitID = NULL, entityId = 1, schoolYearId = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment())

		body <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper()) %>% purrr::compact()

		modifySkyObject(module = "BenefitManagement", objectName = "PlanBenefit", objectId = PlanBenefitID, body = list(DataObject = body), searchFields = append("PlanBenefitID", body %>% names()), entityId = entityId, schoolYearId = schoolYearId, flatten = flatten, returnResponse = returnResponse)
	}

	#' List PlanDeductions
	#'
	#' This function returns a dataframe or json object of PlanDeductions
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given PlanDeductions. Defaults to FALSE for all return fields which, for convenience, returns all fields for the PlanDeductions.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object listed as text. Run \code{\link{getSchemaForObjects}}('PlanDeduction') to get more field paths.
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
	#' @concept Benefit Management
	#' @return A list of PlanDeductions
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	listPlanDeductions <- function(searchConditionsList = NULL, PlanDeductionID = F, PlanID = F, DeductionID = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, fieldPaths = NULL, searchConditionsGroupType = "And", searchSortFieldNamesList = NULL, searchSortFieldNamesDescendingList = NULL, entityId = 1, schoolYearId = NULL, page = 1, pageSize = 100000, flatten = T, returnResponse = F){

		params <- as.list(environment())

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		listSkyObjects(module = "BenefitManagement", objectName = "PlanDeduction", searchFields = searchFields %>% append(fieldPaths), page = page, pageSize = pageSize, SearchConditionsList = searchConditionsList, SearchConditionsGroupType = searchConditionsGroupType, SearchSortFieldNamesList = searchSortFieldNamesList, SearchSortFieldNamesDescendingList = searchSortFieldNamesDescendingList, entityId = entityId, schoolYearId = schoolYearId, flatten = flatten, returnResponse = returnResponse)
	}

	#' Get a PlanDeduction
	#'
	#' This function returns a dataframe or json object of a PlanDeduction
	#' @param PlanDeductionID The ID of the PlanDeduction to return.
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given PlanDeduction. Defaults to FALSE for all return fields which, for convenience, returns all fields for the PlanDeduction.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object listed as text. Run \code{\link{getSchemaForObjects}}('PlanDeduction') to get more field paths.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{listSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Benefit Management
	#' @return A dataframe or of PlanDeduction
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	getPlanDeduction <- function(PlanDeductionID, PlanID = F, DeductionID = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, fieldPaths = NULL, entityId = 1, schoolYearId = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment()) %>% purrr::keep(names(.) != "PlanDeductionID")

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		getSkyObject(module = "BenefitManagement", objectName = "PlanDeduction", objectId = PlanDeductionID, searchFields = searchFields, entityId = entityId, flatten = flatten, returnResponse = returnResponse)
	}

	#' Delete a PlanDeduction
	#'
	#' This function deletes a PlanDeduction
	#' @param PlanDeductionID The ID of the PlanDeduction to delete
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{listSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Benefit Management
	#' @return The PlanDeductionID of the deleted PlanDeduction.
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	deletePlanDeduction <- function(PlanDeductionID, ignoreWarnings = F, entityId = 1, schoolYearId = NULL, flatten = T, returnResponse = F){

		deleteSkyObject(module = "BenefitManagement", objectName = "PlanDeduction", objectId = PlanDeductionID, ignoreWarnings = ignoreWarnings, entityId = entityId, flatten = flatten, returnResponse = returnResponse)
	}

	#' Create a PlanDeduction
	#'
	#' This function creates a PlanDeduction
	#' @param fieldNames The field values to give the created PlanDeduction. Each defaults to NULL.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{listSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Benefit Management
	#' @return A newly created PlanDeduction
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	createPlanDeduction <- function(PlanID = NULL, DeductionID = NULL, entityId = 1, schoolYearId = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment())

		body <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper()) %>% purrr::compact()

		createSkyObject(module = "BenefitManagement", objectName = "PlanDeduction", body = list(DataObject = body), searchFields = append("PlanDeductionID", body %>% names()), entityId = entityId, schoolYearId = schoolYearId, flatten = flatten, returnResponse = returnResponse)
	}

	#' Modify a PlanDeduction
	#'
	#' This function modifies a PlanDeduction
	#' @param fieldNames The field values to give the modified PlanDeduction. Each defaults to NULL.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{listSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Benefit Management
	#' @return The modified PlanDeduction
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	modifyPlanDeduction <- function(PlanDeductionID, PlanID = NULL, DeductionID = NULL, entityId = 1, schoolYearId = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment())

		body <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper()) %>% purrr::compact()

		modifySkyObject(module = "BenefitManagement", objectName = "PlanDeduction", objectId = PlanDeductionID, body = list(DataObject = body), searchFields = append("PlanDeductionID", body %>% names()), entityId = entityId, schoolYearId = schoolYearId, flatten = flatten, returnResponse = returnResponse)
	}

	#' List BenefitPaids
	#'
	#' This function returns a dataframe or json object of BenefitPaids
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given BenefitPaids. Defaults to FALSE for all return fields which, for convenience, returns all fields for the BenefitPaids.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object listed as text. Run \code{\link{getSchemaForObjects}}('BenefitPaid') to get more field paths.
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
	#' @concept Benefit Management
	#' @return A list of BenefitPaids
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	listBenefitPaids <- function(searchConditionsList = NULL, BenefitPaidID = F, BenefitTransactionID = F, EmployeePlanEnrollmentID = F, Amount = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, fieldPaths = NULL, searchConditionsGroupType = "And", searchSortFieldNamesList = NULL, searchSortFieldNamesDescendingList = NULL, entityId = 1, schoolYearId = NULL, page = 1, pageSize = 100000, flatten = T, returnResponse = F){

		params <- as.list(environment())

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		listSkyObjects(module = "BenefitManagement", objectName = "BenefitPaid", searchFields = searchFields %>% append(fieldPaths), page = page, pageSize = pageSize, SearchConditionsList = searchConditionsList, SearchConditionsGroupType = searchConditionsGroupType, SearchSortFieldNamesList = searchSortFieldNamesList, SearchSortFieldNamesDescendingList = searchSortFieldNamesDescendingList, entityId = entityId, schoolYearId = schoolYearId, flatten = flatten, returnResponse = returnResponse)
	}

	#' Get a BenefitPaid
	#'
	#' This function returns a dataframe or json object of a BenefitPaid
	#' @param BenefitPaidID The ID of the BenefitPaid to return.
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given BenefitPaid. Defaults to FALSE for all return fields which, for convenience, returns all fields for the BenefitPaid.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object listed as text. Run \code{\link{getSchemaForObjects}}('BenefitPaid') to get more field paths.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{listSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Benefit Management
	#' @return A dataframe or of BenefitPaid
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	getBenefitPaid <- function(BenefitPaidID, BenefitTransactionID = F, EmployeePlanEnrollmentID = F, Amount = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, fieldPaths = NULL, entityId = 1, schoolYearId = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment()) %>% purrr::keep(names(.) != "BenefitPaidID")

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		getSkyObject(module = "BenefitManagement", objectName = "BenefitPaid", objectId = BenefitPaidID, searchFields = searchFields, entityId = entityId, flatten = flatten, returnResponse = returnResponse)
	}

	#' Delete a BenefitPaid
	#'
	#' This function deletes a BenefitPaid
	#' @param BenefitPaidID The ID of the BenefitPaid to delete
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{listSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Benefit Management
	#' @return The BenefitPaidID of the deleted BenefitPaid.
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	deleteBenefitPaid <- function(BenefitPaidID, ignoreWarnings = F, entityId = 1, schoolYearId = NULL, flatten = T, returnResponse = F){

		deleteSkyObject(module = "BenefitManagement", objectName = "BenefitPaid", objectId = BenefitPaidID, ignoreWarnings = ignoreWarnings, entityId = entityId, flatten = flatten, returnResponse = returnResponse)
	}

	#' Create a BenefitPaid
	#'
	#' This function creates a BenefitPaid
	#' @param fieldNames The field values to give the created BenefitPaid. Each defaults to NULL.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{listSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Benefit Management
	#' @return A newly created BenefitPaid
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	createBenefitPaid <- function(BenefitTransactionID = NULL, EmployeePlanEnrollmentID = NULL, Amount = NULL, entityId = 1, schoolYearId = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment())

		body <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper()) %>% purrr::compact()

		createSkyObject(module = "BenefitManagement", objectName = "BenefitPaid", body = list(DataObject = body), searchFields = append("BenefitPaidID", body %>% names()), entityId = entityId, schoolYearId = schoolYearId, flatten = flatten, returnResponse = returnResponse)
	}

	#' Modify a BenefitPaid
	#'
	#' This function modifies a BenefitPaid
	#' @param fieldNames The field values to give the modified BenefitPaid. Each defaults to NULL.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{listSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Benefit Management
	#' @return The modified BenefitPaid
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	modifyBenefitPaid <- function(BenefitPaidID, BenefitTransactionID = NULL, EmployeePlanEnrollmentID = NULL, Amount = NULL, entityId = 1, schoolYearId = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment())

		body <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper()) %>% purrr::compact()

		modifySkyObject(module = "BenefitManagement", objectName = "BenefitPaid", objectId = BenefitPaidID, body = list(DataObject = body), searchFields = append("BenefitPaidID", body %>% names()), entityId = entityId, schoolYearId = schoolYearId, flatten = flatten, returnResponse = returnResponse)
	}

	#' List DeductionPaids
	#'
	#' This function returns a dataframe or json object of DeductionPaids
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given DeductionPaids. Defaults to FALSE for all return fields which, for convenience, returns all fields for the DeductionPaids.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object listed as text. Run \code{\link{getSchemaForObjects}}('DeductionPaid') to get more field paths.
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
	#' @concept Benefit Management
	#' @return A list of DeductionPaids
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	listDeductionPaids <- function(searchConditionsList = NULL, DeductionPaidID = F, DeductionTransactionID = F, EmployeePlanEnrollmentID = F, Amount = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, fieldPaths = NULL, searchConditionsGroupType = "And", searchSortFieldNamesList = NULL, searchSortFieldNamesDescendingList = NULL, entityId = 1, schoolYearId = NULL, page = 1, pageSize = 100000, flatten = T, returnResponse = F){

		params <- as.list(environment())

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		listSkyObjects(module = "BenefitManagement", objectName = "DeductionPaid", searchFields = searchFields %>% append(fieldPaths), page = page, pageSize = pageSize, SearchConditionsList = searchConditionsList, SearchConditionsGroupType = searchConditionsGroupType, SearchSortFieldNamesList = searchSortFieldNamesList, SearchSortFieldNamesDescendingList = searchSortFieldNamesDescendingList, entityId = entityId, schoolYearId = schoolYearId, flatten = flatten, returnResponse = returnResponse)
	}

	#' Get a DeductionPaid
	#'
	#' This function returns a dataframe or json object of a DeductionPaid
	#' @param DeductionPaidID The ID of the DeductionPaid to return.
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given DeductionPaid. Defaults to FALSE for all return fields which, for convenience, returns all fields for the DeductionPaid.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object listed as text. Run \code{\link{getSchemaForObjects}}('DeductionPaid') to get more field paths.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{listSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Benefit Management
	#' @return A dataframe or of DeductionPaid
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	getDeductionPaid <- function(DeductionPaidID, DeductionTransactionID = F, EmployeePlanEnrollmentID = F, Amount = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, fieldPaths = NULL, entityId = 1, schoolYearId = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment()) %>% purrr::keep(names(.) != "DeductionPaidID")

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		getSkyObject(module = "BenefitManagement", objectName = "DeductionPaid", objectId = DeductionPaidID, searchFields = searchFields, entityId = entityId, flatten = flatten, returnResponse = returnResponse)
	}

	#' Delete a DeductionPaid
	#'
	#' This function deletes a DeductionPaid
	#' @param DeductionPaidID The ID of the DeductionPaid to delete
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{listSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Benefit Management
	#' @return The DeductionPaidID of the deleted DeductionPaid.
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	deleteDeductionPaid <- function(DeductionPaidID, ignoreWarnings = F, entityId = 1, schoolYearId = NULL, flatten = T, returnResponse = F){

		deleteSkyObject(module = "BenefitManagement", objectName = "DeductionPaid", objectId = DeductionPaidID, ignoreWarnings = ignoreWarnings, entityId = entityId, flatten = flatten, returnResponse = returnResponse)
	}

	#' Create a DeductionPaid
	#'
	#' This function creates a DeductionPaid
	#' @param fieldNames The field values to give the created DeductionPaid. Each defaults to NULL.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{listSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Benefit Management
	#' @return A newly created DeductionPaid
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	createDeductionPaid <- function(DeductionTransactionID = NULL, EmployeePlanEnrollmentID = NULL, Amount = NULL, entityId = 1, schoolYearId = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment())

		body <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper()) %>% purrr::compact()

		createSkyObject(module = "BenefitManagement", objectName = "DeductionPaid", body = list(DataObject = body), searchFields = append("DeductionPaidID", body %>% names()), entityId = entityId, schoolYearId = schoolYearId, flatten = flatten, returnResponse = returnResponse)
	}

	#' Modify a DeductionPaid
	#'
	#' This function modifies a DeductionPaid
	#' @param fieldNames The field values to give the modified DeductionPaid. Each defaults to NULL.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{listSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Benefit Management
	#' @return The modified DeductionPaid
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	modifyDeductionPaid <- function(DeductionPaidID, DeductionTransactionID = NULL, EmployeePlanEnrollmentID = NULL, Amount = NULL, entityId = 1, schoolYearId = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment())

		body <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper()) %>% purrr::compact()

		modifySkyObject(module = "BenefitManagement", objectName = "DeductionPaid", objectId = DeductionPaidID, body = list(DataObject = body), searchFields = append("DeductionPaidID", body %>% names()), entityId = entityId, schoolYearId = schoolYearId, flatten = flatten, returnResponse = returnResponse)
	}

	#' List TempDeductionBenefitPaids
	#'
	#' This function returns a dataframe or json object of TempDeductionBenefitPaids
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given TempDeductionBenefitPaids. Defaults to FALSE for all return fields which, for convenience, returns all fields for the TempDeductionBenefitPaids.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object listed as text. Run \code{\link{getSchemaForObjects}}('TempDeductionBenefitPaid') to get more field paths.
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
	#' @concept Benefit Management
	#' @return A list of TempDeductionBenefitPaids
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	listTempDeductionBenefitPaids <- function(searchConditionsList = NULL, TempDeductionBenefitPaidID = F, TransactionID = F, EmployeePlanEnrollmentID = F, OldEmployeePlanEnrollmentInformation = F, NewEmployeePlanEnrollmentInformation = F, Origin = F, OriginInformation = F, ActionReason = F, OldAmount = F, NewAmount = F, PreProcessingError = F, HasProcessingError = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, fieldPaths = NULL, searchConditionsGroupType = "And", searchSortFieldNamesList = NULL, searchSortFieldNamesDescendingList = NULL, entityId = 1, schoolYearId = NULL, page = 1, pageSize = 100000, flatten = T, returnResponse = F){

		params <- as.list(environment())

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		listSkyObjects(module = "BenefitManagement", objectName = "TempDeductionBenefitPaid", searchFields = searchFields %>% append(fieldPaths), page = page, pageSize = pageSize, SearchConditionsList = searchConditionsList, SearchConditionsGroupType = searchConditionsGroupType, SearchSortFieldNamesList = searchSortFieldNamesList, SearchSortFieldNamesDescendingList = searchSortFieldNamesDescendingList, entityId = entityId, schoolYearId = schoolYearId, flatten = flatten, returnResponse = returnResponse)
	}

	#' Get a TempDeductionBenefitPaid
	#'
	#' This function returns a dataframe or json object of a TempDeductionBenefitPaid
	#' @param TempDeductionBenefitPaidID The ID of the TempDeductionBenefitPaid to return.
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given TempDeductionBenefitPaid. Defaults to FALSE for all return fields which, for convenience, returns all fields for the TempDeductionBenefitPaid.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object listed as text. Run \code{\link{getSchemaForObjects}}('TempDeductionBenefitPaid') to get more field paths.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{listSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Benefit Management
	#' @return A dataframe or of TempDeductionBenefitPaid
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	getTempDeductionBenefitPaid <- function(TempDeductionBenefitPaidID, TransactionID = F, EmployeePlanEnrollmentID = F, OldEmployeePlanEnrollmentInformation = F, NewEmployeePlanEnrollmentInformation = F, Origin = F, OriginInformation = F, ActionReason = F, OldAmount = F, NewAmount = F, PreProcessingError = F, HasProcessingError = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, fieldPaths = NULL, entityId = 1, schoolYearId = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment()) %>% purrr::keep(names(.) != "TempDeductionBenefitPaidID")

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		getSkyObject(module = "BenefitManagement", objectName = "TempDeductionBenefitPaid", objectId = TempDeductionBenefitPaidID, searchFields = searchFields, entityId = entityId, flatten = flatten, returnResponse = returnResponse)
	}

	#' Delete a TempDeductionBenefitPaid
	#'
	#' This function deletes a TempDeductionBenefitPaid
	#' @param TempDeductionBenefitPaidID The ID of the TempDeductionBenefitPaid to delete
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{listSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Benefit Management
	#' @return The TempDeductionBenefitPaidID of the deleted TempDeductionBenefitPaid.
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	deleteTempDeductionBenefitPaid <- function(TempDeductionBenefitPaidID, ignoreWarnings = F, entityId = 1, schoolYearId = NULL, flatten = T, returnResponse = F){

		deleteSkyObject(module = "BenefitManagement", objectName = "TempDeductionBenefitPaid", objectId = TempDeductionBenefitPaidID, ignoreWarnings = ignoreWarnings, entityId = entityId, flatten = flatten, returnResponse = returnResponse)
	}

	#' Create a TempDeductionBenefitPaid
	#'
	#' This function creates a TempDeductionBenefitPaid
	#' @param fieldNames The field values to give the created TempDeductionBenefitPaid. Each defaults to NULL.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{listSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Benefit Management
	#' @return A newly created TempDeductionBenefitPaid
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	createTempDeductionBenefitPaid <- function(TransactionID = NULL, EmployeePlanEnrollmentID = NULL, OldEmployeePlanEnrollmentInformation = NULL, NewEmployeePlanEnrollmentInformation = NULL, Origin = NULL, OriginInformation = NULL, ActionReason = NULL, OldAmount = NULL, NewAmount = NULL, PreProcessingError = NULL, HasProcessingError = NULL, entityId = 1, schoolYearId = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment())

		body <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper()) %>% purrr::compact()

		createSkyObject(module = "BenefitManagement", objectName = "TempDeductionBenefitPaid", body = list(DataObject = body), searchFields = append("TempDeductionBenefitPaidID", body %>% names()), entityId = entityId, schoolYearId = schoolYearId, flatten = flatten, returnResponse = returnResponse)
	}

	#' Modify a TempDeductionBenefitPaid
	#'
	#' This function modifies a TempDeductionBenefitPaid
	#' @param fieldNames The field values to give the modified TempDeductionBenefitPaid. Each defaults to NULL.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{listSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Benefit Management
	#' @return The modified TempDeductionBenefitPaid
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	modifyTempDeductionBenefitPaid <- function(TempDeductionBenefitPaidID, TransactionID = NULL, EmployeePlanEnrollmentID = NULL, OldEmployeePlanEnrollmentInformation = NULL, NewEmployeePlanEnrollmentInformation = NULL, Origin = NULL, OriginInformation = NULL, ActionReason = NULL, OldAmount = NULL, NewAmount = NULL, PreProcessingError = NULL, HasProcessingError = NULL, entityId = 1, schoolYearId = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment())

		body <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper()) %>% purrr::compact()

		modifySkyObject(module = "BenefitManagement", objectName = "TempDeductionBenefitPaid", objectId = TempDeductionBenefitPaidID, body = list(DataObject = body), searchFields = append("TempDeductionBenefitPaidID", body %>% names()), entityId = entityId, schoolYearId = schoolYearId, flatten = flatten, returnResponse = returnResponse)
	}

	#' List TempDeductionBenefitPaidErrors
	#'
	#' This function returns a dataframe or json object of TempDeductionBenefitPaidErrors
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given TempDeductionBenefitPaidErrors. Defaults to FALSE for all return fields which, for convenience, returns all fields for the TempDeductionBenefitPaidErrors.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object listed as text. Run \code{\link{getSchemaForObjects}}('TempDeductionBenefitPaidError') to get more field paths.
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
	#' @concept Benefit Management
	#' @return A list of TempDeductionBenefitPaidErrors
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	listTempDeductionBenefitPaidErrors <- function(searchConditionsList = NULL, TempDeductionBenefitPaidErrorID = F, TempDeductionBenefitPaidID = F, Error = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, fieldPaths = NULL, searchConditionsGroupType = "And", searchSortFieldNamesList = NULL, searchSortFieldNamesDescendingList = NULL, entityId = 1, schoolYearId = NULL, page = 1, pageSize = 100000, flatten = T, returnResponse = F){

		params <- as.list(environment())

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		listSkyObjects(module = "BenefitManagement", objectName = "TempDeductionBenefitPaidError", searchFields = searchFields %>% append(fieldPaths), page = page, pageSize = pageSize, SearchConditionsList = searchConditionsList, SearchConditionsGroupType = searchConditionsGroupType, SearchSortFieldNamesList = searchSortFieldNamesList, SearchSortFieldNamesDescendingList = searchSortFieldNamesDescendingList, entityId = entityId, schoolYearId = schoolYearId, flatten = flatten, returnResponse = returnResponse)
	}

	#' Get a TempDeductionBenefitPaidError
	#'
	#' This function returns a dataframe or json object of a TempDeductionBenefitPaidError
	#' @param TempDeductionBenefitPaidErrorID The ID of the TempDeductionBenefitPaidError to return.
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given TempDeductionBenefitPaidError. Defaults to FALSE for all return fields which, for convenience, returns all fields for the TempDeductionBenefitPaidError.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object listed as text. Run \code{\link{getSchemaForObjects}}('TempDeductionBenefitPaidError') to get more field paths.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{listSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Benefit Management
	#' @return A dataframe or of TempDeductionBenefitPaidError
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	getTempDeductionBenefitPaidError <- function(TempDeductionBenefitPaidErrorID, TempDeductionBenefitPaidID = F, Error = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, fieldPaths = NULL, entityId = 1, schoolYearId = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment()) %>% purrr::keep(names(.) != "TempDeductionBenefitPaidErrorID")

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		getSkyObject(module = "BenefitManagement", objectName = "TempDeductionBenefitPaidError", objectId = TempDeductionBenefitPaidErrorID, searchFields = searchFields, entityId = entityId, flatten = flatten, returnResponse = returnResponse)
	}

	#' Delete a TempDeductionBenefitPaidError
	#'
	#' This function deletes a TempDeductionBenefitPaidError
	#' @param TempDeductionBenefitPaidErrorID The ID of the TempDeductionBenefitPaidError to delete
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{listSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Benefit Management
	#' @return The TempDeductionBenefitPaidErrorID of the deleted TempDeductionBenefitPaidError.
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	deleteTempDeductionBenefitPaidError <- function(TempDeductionBenefitPaidErrorID, ignoreWarnings = F, entityId = 1, schoolYearId = NULL, flatten = T, returnResponse = F){

		deleteSkyObject(module = "BenefitManagement", objectName = "TempDeductionBenefitPaidError", objectId = TempDeductionBenefitPaidErrorID, ignoreWarnings = ignoreWarnings, entityId = entityId, flatten = flatten, returnResponse = returnResponse)
	}

	#' Create a TempDeductionBenefitPaidError
	#'
	#' This function creates a TempDeductionBenefitPaidError
	#' @param fieldNames The field values to give the created TempDeductionBenefitPaidError. Each defaults to NULL.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{listSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Benefit Management
	#' @return A newly created TempDeductionBenefitPaidError
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	createTempDeductionBenefitPaidError <- function(TempDeductionBenefitPaidID = NULL, Error = NULL, entityId = 1, schoolYearId = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment())

		body <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper()) %>% purrr::compact()

		createSkyObject(module = "BenefitManagement", objectName = "TempDeductionBenefitPaidError", body = list(DataObject = body), searchFields = append("TempDeductionBenefitPaidErrorID", body %>% names()), entityId = entityId, schoolYearId = schoolYearId, flatten = flatten, returnResponse = returnResponse)
	}

	#' Modify a TempDeductionBenefitPaidError
	#'
	#' This function modifies a TempDeductionBenefitPaidError
	#' @param fieldNames The field values to give the modified TempDeductionBenefitPaidError. Each defaults to NULL.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{listSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Benefit Management
	#' @return The modified TempDeductionBenefitPaidError
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	modifyTempDeductionBenefitPaidError <- function(TempDeductionBenefitPaidErrorID, TempDeductionBenefitPaidID = NULL, Error = NULL, entityId = 1, schoolYearId = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment())

		body <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper()) %>% purrr::compact()

		modifySkyObject(module = "BenefitManagement", objectName = "TempDeductionBenefitPaidError", objectId = TempDeductionBenefitPaidErrorID, body = list(DataObject = body), searchFields = append("TempDeductionBenefitPaidErrorID", body %>% names()), entityId = entityId, schoolYearId = schoolYearId, flatten = flatten, returnResponse = returnResponse)
	}

	#' List DependentCoverages
	#'
	#' This function returns a dataframe or json object of DependentCoverages
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given DependentCoverages. Defaults to FALSE for all return fields which, for convenience, returns all fields for the DependentCoverages.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object listed as text. Run \code{\link{getSchemaForObjects}}('DependentCoverage') to get more field paths.
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
	#' @concept Benefit Management
	#' @return A list of DependentCoverages
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	listDependentCoverages <- function(searchConditionsList = NULL, DependentCoverageID = F, CoverageMonthID = F, DependentID = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, fieldPaths = NULL, searchConditionsGroupType = "And", searchSortFieldNamesList = NULL, searchSortFieldNamesDescendingList = NULL, entityId = 1, schoolYearId = NULL, page = 1, pageSize = 100000, flatten = T, returnResponse = F){

		params <- as.list(environment())

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		listSkyObjects(module = "BenefitManagement", objectName = "DependentCoverage", searchFields = searchFields %>% append(fieldPaths), page = page, pageSize = pageSize, SearchConditionsList = searchConditionsList, SearchConditionsGroupType = searchConditionsGroupType, SearchSortFieldNamesList = searchSortFieldNamesList, SearchSortFieldNamesDescendingList = searchSortFieldNamesDescendingList, entityId = entityId, schoolYearId = schoolYearId, flatten = flatten, returnResponse = returnResponse)
	}

	#' Get a DependentCoverage
	#'
	#' This function returns a dataframe or json object of a DependentCoverage
	#' @param DependentCoverageID The ID of the DependentCoverage to return.
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given DependentCoverage. Defaults to FALSE for all return fields which, for convenience, returns all fields for the DependentCoverage.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object listed as text. Run \code{\link{getSchemaForObjects}}('DependentCoverage') to get more field paths.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{listSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Benefit Management
	#' @return A dataframe or of DependentCoverage
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	getDependentCoverage <- function(DependentCoverageID, CoverageMonthID = F, DependentID = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, fieldPaths = NULL, entityId = 1, schoolYearId = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment()) %>% purrr::keep(names(.) != "DependentCoverageID")

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		getSkyObject(module = "BenefitManagement", objectName = "DependentCoverage", objectId = DependentCoverageID, searchFields = searchFields, entityId = entityId, flatten = flatten, returnResponse = returnResponse)
	}

	#' Delete a DependentCoverage
	#'
	#' This function deletes a DependentCoverage
	#' @param DependentCoverageID The ID of the DependentCoverage to delete
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{listSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Benefit Management
	#' @return The DependentCoverageID of the deleted DependentCoverage.
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	deleteDependentCoverage <- function(DependentCoverageID, ignoreWarnings = F, entityId = 1, schoolYearId = NULL, flatten = T, returnResponse = F){

		deleteSkyObject(module = "BenefitManagement", objectName = "DependentCoverage", objectId = DependentCoverageID, ignoreWarnings = ignoreWarnings, entityId = entityId, flatten = flatten, returnResponse = returnResponse)
	}

	#' Create a DependentCoverage
	#'
	#' This function creates a DependentCoverage
	#' @param fieldNames The field values to give the created DependentCoverage. Each defaults to NULL.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{listSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Benefit Management
	#' @return A newly created DependentCoverage
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	createDependentCoverage <- function(CoverageMonthID = NULL, DependentID = NULL, entityId = 1, schoolYearId = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment())

		body <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper()) %>% purrr::compact()

		createSkyObject(module = "BenefitManagement", objectName = "DependentCoverage", body = list(DataObject = body), searchFields = append("DependentCoverageID", body %>% names()), entityId = entityId, schoolYearId = schoolYearId, flatten = flatten, returnResponse = returnResponse)
	}

	#' Modify a DependentCoverage
	#'
	#' This function modifies a DependentCoverage
	#' @param fieldNames The field values to give the modified DependentCoverage. Each defaults to NULL.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{listSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Benefit Management
	#' @return The modified DependentCoverage
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	modifyDependentCoverage <- function(DependentCoverageID, CoverageMonthID = NULL, DependentID = NULL, entityId = 1, schoolYearId = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment())

		body <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper()) %>% purrr::compact()

		modifySkyObject(module = "BenefitManagement", objectName = "DependentCoverage", objectId = DependentCoverageID, body = list(DataObject = body), searchFields = append("DependentCoverageID", body %>% names()), entityId = entityId, schoolYearId = schoolYearId, flatten = flatten, returnResponse = returnResponse)
	}

	#' List TempCoverageMonths
	#'
	#' This function returns a dataframe or json object of TempCoverageMonths
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given TempCoverageMonths. Defaults to FALSE for all return fields which, for convenience, returns all fields for the TempCoverageMonths.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object listed as text. Run \code{\link{getSchemaForObjects}}('TempCoverageMonth') to get more field paths.
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
	#' @concept Benefit Management
	#' @return A list of TempCoverageMonths
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	listTempCoverageMonths <- function(searchConditionsList = NULL, TempCoverageMonthID = F, CoverageMonthID = F, CoverageMonthInfo = F, OldDeductionAmount = F, OldBenefitAmount = F, OldPremiumAmount = F, NewDeductionAmount = F, NewBenefitAmount = F, NewPremiumAmount = F, HasError = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, EmployeePlanEnrollmentID = F, EmployeeFullNameLFM = F, PlanCodeDescription = F, CoverageMonthDate = F, OldCoverageAmount = F, NewCoverageAmount = F, RateType = F, fieldPaths = NULL, searchConditionsGroupType = "And", searchSortFieldNamesList = NULL, searchSortFieldNamesDescendingList = NULL, entityId = 1, schoolYearId = NULL, page = 1, pageSize = 100000, flatten = T, returnResponse = F){

		params <- as.list(environment())

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		listSkyObjects(module = "BenefitManagement", objectName = "TempCoverageMonth", searchFields = searchFields %>% append(fieldPaths), page = page, pageSize = pageSize, SearchConditionsList = searchConditionsList, SearchConditionsGroupType = searchConditionsGroupType, SearchSortFieldNamesList = searchSortFieldNamesList, SearchSortFieldNamesDescendingList = searchSortFieldNamesDescendingList, entityId = entityId, schoolYearId = schoolYearId, flatten = flatten, returnResponse = returnResponse)
	}

	#' Get a TempCoverageMonth
	#'
	#' This function returns a dataframe or json object of a TempCoverageMonth
	#' @param TempCoverageMonthID The ID of the TempCoverageMonth to return.
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given TempCoverageMonth. Defaults to FALSE for all return fields which, for convenience, returns all fields for the TempCoverageMonth.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object listed as text. Run \code{\link{getSchemaForObjects}}('TempCoverageMonth') to get more field paths.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{listSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Benefit Management
	#' @return A dataframe or of TempCoverageMonth
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	getTempCoverageMonth <- function(TempCoverageMonthID, CoverageMonthID = F, CoverageMonthInfo = F, OldDeductionAmount = F, OldBenefitAmount = F, OldPremiumAmount = F, NewDeductionAmount = F, NewBenefitAmount = F, NewPremiumAmount = F, HasError = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, EmployeePlanEnrollmentID = F, EmployeeFullNameLFM = F, PlanCodeDescription = F, CoverageMonthDate = F, OldCoverageAmount = F, NewCoverageAmount = F, RateType = F, fieldPaths = NULL, entityId = 1, schoolYearId = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment()) %>% purrr::keep(names(.) != "TempCoverageMonthID")

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		getSkyObject(module = "BenefitManagement", objectName = "TempCoverageMonth", objectId = TempCoverageMonthID, searchFields = searchFields, entityId = entityId, flatten = flatten, returnResponse = returnResponse)
	}

	#' Delete a TempCoverageMonth
	#'
	#' This function deletes a TempCoverageMonth
	#' @param TempCoverageMonthID The ID of the TempCoverageMonth to delete
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{listSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Benefit Management
	#' @return The TempCoverageMonthID of the deleted TempCoverageMonth.
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	deleteTempCoverageMonth <- function(TempCoverageMonthID, ignoreWarnings = F, entityId = 1, schoolYearId = NULL, flatten = T, returnResponse = F){

		deleteSkyObject(module = "BenefitManagement", objectName = "TempCoverageMonth", objectId = TempCoverageMonthID, ignoreWarnings = ignoreWarnings, entityId = entityId, flatten = flatten, returnResponse = returnResponse)
	}

	#' Create a TempCoverageMonth
	#'
	#' This function creates a TempCoverageMonth
	#' @param fieldNames The field values to give the created TempCoverageMonth. Each defaults to NULL.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{listSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Benefit Management
	#' @return A newly created TempCoverageMonth
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	createTempCoverageMonth <- function(CoverageMonthID = NULL, CoverageMonthInfo = NULL, OldDeductionAmount = NULL, OldBenefitAmount = NULL, OldPremiumAmount = NULL, NewDeductionAmount = NULL, NewBenefitAmount = NULL, NewPremiumAmount = NULL, HasError = NULL, EmployeePlanEnrollmentID = NULL, EmployeeFullNameLFM = NULL, PlanCodeDescription = NULL, CoverageMonthDate = NULL, OldCoverageAmount = NULL, NewCoverageAmount = NULL, RateType = NULL, entityId = 1, schoolYearId = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment())

		body <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper()) %>% purrr::compact()

		createSkyObject(module = "BenefitManagement", objectName = "TempCoverageMonth", body = list(DataObject = body), searchFields = append("TempCoverageMonthID", body %>% names()), entityId = entityId, schoolYearId = schoolYearId, flatten = flatten, returnResponse = returnResponse)
	}

	#' Modify a TempCoverageMonth
	#'
	#' This function modifies a TempCoverageMonth
	#' @param fieldNames The field values to give the modified TempCoverageMonth. Each defaults to NULL.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{listSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Benefit Management
	#' @return The modified TempCoverageMonth
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	modifyTempCoverageMonth <- function(TempCoverageMonthID, CoverageMonthID = NULL, CoverageMonthInfo = NULL, OldDeductionAmount = NULL, OldBenefitAmount = NULL, OldPremiumAmount = NULL, NewDeductionAmount = NULL, NewBenefitAmount = NULL, NewPremiumAmount = NULL, HasError = NULL, EmployeePlanEnrollmentID = NULL, EmployeeFullNameLFM = NULL, PlanCodeDescription = NULL, CoverageMonthDate = NULL, OldCoverageAmount = NULL, NewCoverageAmount = NULL, RateType = NULL, entityId = 1, schoolYearId = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment())

		body <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper()) %>% purrr::compact()

		modifySkyObject(module = "BenefitManagement", objectName = "TempCoverageMonth", objectId = TempCoverageMonthID, body = list(DataObject = body), searchFields = append("TempCoverageMonthID", body %>% names()), entityId = entityId, schoolYearId = schoolYearId, flatten = flatten, returnResponse = returnResponse)
	}

	#' List TempCoverageMonthErrors
	#'
	#' This function returns a dataframe or json object of TempCoverageMonthErrors
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given TempCoverageMonthErrors. Defaults to FALSE for all return fields which, for convenience, returns all fields for the TempCoverageMonthErrors.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object listed as text. Run \code{\link{getSchemaForObjects}}('TempCoverageMonthError') to get more field paths.
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
	#' @concept Benefit Management
	#' @return A list of TempCoverageMonthErrors
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	listTempCoverageMonthErrors <- function(searchConditionsList = NULL, TempCoverageMonthErrorID = F, TempCoverageMonthID = F, Error = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, fieldPaths = NULL, searchConditionsGroupType = "And", searchSortFieldNamesList = NULL, searchSortFieldNamesDescendingList = NULL, entityId = 1, schoolYearId = NULL, page = 1, pageSize = 100000, flatten = T, returnResponse = F){

		params <- as.list(environment())

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		listSkyObjects(module = "BenefitManagement", objectName = "TempCoverageMonthError", searchFields = searchFields %>% append(fieldPaths), page = page, pageSize = pageSize, SearchConditionsList = searchConditionsList, SearchConditionsGroupType = searchConditionsGroupType, SearchSortFieldNamesList = searchSortFieldNamesList, SearchSortFieldNamesDescendingList = searchSortFieldNamesDescendingList, entityId = entityId, schoolYearId = schoolYearId, flatten = flatten, returnResponse = returnResponse)
	}

	#' Get a TempCoverageMonthError
	#'
	#' This function returns a dataframe or json object of a TempCoverageMonthError
	#' @param TempCoverageMonthErrorID The ID of the TempCoverageMonthError to return.
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given TempCoverageMonthError. Defaults to FALSE for all return fields which, for convenience, returns all fields for the TempCoverageMonthError.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object listed as text. Run \code{\link{getSchemaForObjects}}('TempCoverageMonthError') to get more field paths.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{listSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Benefit Management
	#' @return A dataframe or of TempCoverageMonthError
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	getTempCoverageMonthError <- function(TempCoverageMonthErrorID, TempCoverageMonthID = F, Error = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, fieldPaths = NULL, entityId = 1, schoolYearId = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment()) %>% purrr::keep(names(.) != "TempCoverageMonthErrorID")

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		getSkyObject(module = "BenefitManagement", objectName = "TempCoverageMonthError", objectId = TempCoverageMonthErrorID, searchFields = searchFields, entityId = entityId, flatten = flatten, returnResponse = returnResponse)
	}

	#' Delete a TempCoverageMonthError
	#'
	#' This function deletes a TempCoverageMonthError
	#' @param TempCoverageMonthErrorID The ID of the TempCoverageMonthError to delete
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{listSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Benefit Management
	#' @return The TempCoverageMonthErrorID of the deleted TempCoverageMonthError.
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	deleteTempCoverageMonthError <- function(TempCoverageMonthErrorID, ignoreWarnings = F, entityId = 1, schoolYearId = NULL, flatten = T, returnResponse = F){

		deleteSkyObject(module = "BenefitManagement", objectName = "TempCoverageMonthError", objectId = TempCoverageMonthErrorID, ignoreWarnings = ignoreWarnings, entityId = entityId, flatten = flatten, returnResponse = returnResponse)
	}

	#' Create a TempCoverageMonthError
	#'
	#' This function creates a TempCoverageMonthError
	#' @param fieldNames The field values to give the created TempCoverageMonthError. Each defaults to NULL.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{listSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Benefit Management
	#' @return A newly created TempCoverageMonthError
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	createTempCoverageMonthError <- function(TempCoverageMonthID = NULL, Error = NULL, entityId = 1, schoolYearId = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment())

		body <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper()) %>% purrr::compact()

		createSkyObject(module = "BenefitManagement", objectName = "TempCoverageMonthError", body = list(DataObject = body), searchFields = append("TempCoverageMonthErrorID", body %>% names()), entityId = entityId, schoolYearId = schoolYearId, flatten = flatten, returnResponse = returnResponse)
	}

	#' Modify a TempCoverageMonthError
	#'
	#' This function modifies a TempCoverageMonthError
	#' @param fieldNames The field values to give the modified TempCoverageMonthError. Each defaults to NULL.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{listSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Benefit Management
	#' @return The modified TempCoverageMonthError
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	modifyTempCoverageMonthError <- function(TempCoverageMonthErrorID, TempCoverageMonthID = NULL, Error = NULL, entityId = 1, schoolYearId = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment())

		body <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper()) %>% purrr::compact()

		modifySkyObject(module = "BenefitManagement", objectName = "TempCoverageMonthError", objectId = TempCoverageMonthErrorID, body = list(DataObject = body), searchFields = append("TempCoverageMonthErrorID", body %>% names()), entityId = entityId, schoolYearId = schoolYearId, flatten = flatten, returnResponse = returnResponse)
	}

	#' List DeductionBenefitSchedules
	#'
	#' This function returns a dataframe or json object of DeductionBenefitSchedules
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given DeductionBenefitSchedules. Defaults to FALSE for all return fields which, for convenience, returns all fields for the DeductionBenefitSchedules.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object listed as text. Run \code{\link{getSchemaForObjects}}('DeductionBenefitSchedule') to get more field paths.
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
	#' @concept Benefit Management
	#' @return A list of DeductionBenefitSchedules
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	listDeductionBenefitSchedules <- function(searchConditionsList = NULL, DeductionBenefitScheduleID = F, DistrictID = F, Code = F, Description = F, CodeDescription = F, HasDeductionBenefitScheduleDetails = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, FirstCheckDate = F, FinalCheckDate = F, InsuranceYearCoverageMonthCount = F, InsuranceYearPayCount = F, fieldPaths = NULL, searchConditionsGroupType = "And", searchSortFieldNamesList = NULL, searchSortFieldNamesDescendingList = NULL, entityId = 1, schoolYearId = NULL, page = 1, pageSize = 100000, flatten = T, returnResponse = F){

		params <- as.list(environment())

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		listSkyObjects(module = "BenefitManagement", objectName = "DeductionBenefitSchedule", searchFields = searchFields %>% append(fieldPaths), page = page, pageSize = pageSize, SearchConditionsList = searchConditionsList, SearchConditionsGroupType = searchConditionsGroupType, SearchSortFieldNamesList = searchSortFieldNamesList, SearchSortFieldNamesDescendingList = searchSortFieldNamesDescendingList, entityId = entityId, schoolYearId = schoolYearId, flatten = flatten, returnResponse = returnResponse)
	}

	#' Get a DeductionBenefitSchedule
	#'
	#' This function returns a dataframe or json object of a DeductionBenefitSchedule
	#' @param DeductionBenefitScheduleID The ID of the DeductionBenefitSchedule to return.
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given DeductionBenefitSchedule. Defaults to FALSE for all return fields which, for convenience, returns all fields for the DeductionBenefitSchedule.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object listed as text. Run \code{\link{getSchemaForObjects}}('DeductionBenefitSchedule') to get more field paths.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{listSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Benefit Management
	#' @return A dataframe or of DeductionBenefitSchedule
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	getDeductionBenefitSchedule <- function(DeductionBenefitScheduleID, DistrictID = F, Code = F, Description = F, CodeDescription = F, HasDeductionBenefitScheduleDetails = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, FirstCheckDate = F, FinalCheckDate = F, InsuranceYearCoverageMonthCount = F, InsuranceYearPayCount = F, fieldPaths = NULL, entityId = 1, schoolYearId = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment()) %>% purrr::keep(names(.) != "DeductionBenefitScheduleID")

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		getSkyObject(module = "BenefitManagement", objectName = "DeductionBenefitSchedule", objectId = DeductionBenefitScheduleID, searchFields = searchFields, entityId = entityId, flatten = flatten, returnResponse = returnResponse)
	}

	#' Delete a DeductionBenefitSchedule
	#'
	#' This function deletes a DeductionBenefitSchedule
	#' @param DeductionBenefitScheduleID The ID of the DeductionBenefitSchedule to delete
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{listSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Benefit Management
	#' @return The DeductionBenefitScheduleID of the deleted DeductionBenefitSchedule.
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	deleteDeductionBenefitSchedule <- function(DeductionBenefitScheduleID, ignoreWarnings = F, entityId = 1, schoolYearId = NULL, flatten = T, returnResponse = F){

		deleteSkyObject(module = "BenefitManagement", objectName = "DeductionBenefitSchedule", objectId = DeductionBenefitScheduleID, ignoreWarnings = ignoreWarnings, entityId = entityId, flatten = flatten, returnResponse = returnResponse)
	}

	#' Create a DeductionBenefitSchedule
	#'
	#' This function creates a DeductionBenefitSchedule
	#' @param fieldNames The field values to give the created DeductionBenefitSchedule. Each defaults to NULL.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{listSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Benefit Management
	#' @return A newly created DeductionBenefitSchedule
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	createDeductionBenefitSchedule <- function(DistrictID = NULL, Code = NULL, Description = NULL, InsuranceYearCoverageMonthCount = NULL, InsuranceYearPayCount = NULL, entityId = 1, schoolYearId = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment())

		body <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper()) %>% purrr::compact()

		createSkyObject(module = "BenefitManagement", objectName = "DeductionBenefitSchedule", body = list(DataObject = body), searchFields = append("DeductionBenefitScheduleID", body %>% names()), entityId = entityId, schoolYearId = schoolYearId, flatten = flatten, returnResponse = returnResponse)
	}

	#' Modify a DeductionBenefitSchedule
	#'
	#' This function modifies a DeductionBenefitSchedule
	#' @param fieldNames The field values to give the modified DeductionBenefitSchedule. Each defaults to NULL.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{listSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Benefit Management
	#' @return The modified DeductionBenefitSchedule
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	modifyDeductionBenefitSchedule <- function(DeductionBenefitScheduleID, DistrictID = NULL, Code = NULL, Description = NULL, InsuranceYearCoverageMonthCount = NULL, InsuranceYearPayCount = NULL, entityId = 1, schoolYearId = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment())

		body <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper()) %>% purrr::compact()

		modifySkyObject(module = "BenefitManagement", objectName = "DeductionBenefitSchedule", objectId = DeductionBenefitScheduleID, body = list(DataObject = body), searchFields = append("DeductionBenefitScheduleID", body %>% names()), entityId = entityId, schoolYearId = schoolYearId, flatten = flatten, returnResponse = returnResponse)
	}

	#' List DeductionBenefitScheduleDetails
	#'
	#' This function returns a dataframe or json object of DeductionBenefitScheduleDetails
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given DeductionBenefitScheduleDetails. Defaults to FALSE for all return fields which, for convenience, returns all fields for the DeductionBenefitScheduleDetails.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object listed as text. Run \code{\link{getSchemaForObjects}}('DeductionBenefitScheduleDetail') to get more field paths.
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
	#' @concept Benefit Management
	#' @return A list of DeductionBenefitScheduleDetails
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	listDeductionBenefitScheduleDetails <- function(searchConditionsList = NULL, DeductionBenefitScheduleDetailID = F, DeductionBenefitScheduleID = F, CheckDate = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, fieldPaths = NULL, searchConditionsGroupType = "And", searchSortFieldNamesList = NULL, searchSortFieldNamesDescendingList = NULL, entityId = 1, schoolYearId = NULL, page = 1, pageSize = 100000, flatten = T, returnResponse = F){

		params <- as.list(environment())

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		listSkyObjects(module = "BenefitManagement", objectName = "DeductionBenefitScheduleDetail", searchFields = searchFields %>% append(fieldPaths), page = page, pageSize = pageSize, SearchConditionsList = searchConditionsList, SearchConditionsGroupType = searchConditionsGroupType, SearchSortFieldNamesList = searchSortFieldNamesList, SearchSortFieldNamesDescendingList = searchSortFieldNamesDescendingList, entityId = entityId, schoolYearId = schoolYearId, flatten = flatten, returnResponse = returnResponse)
	}

	#' Get a DeductionBenefitScheduleDetail
	#'
	#' This function returns a dataframe or json object of a DeductionBenefitScheduleDetail
	#' @param DeductionBenefitScheduleDetailID The ID of the DeductionBenefitScheduleDetail to return.
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given DeductionBenefitScheduleDetail. Defaults to FALSE for all return fields which, for convenience, returns all fields for the DeductionBenefitScheduleDetail.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object listed as text. Run \code{\link{getSchemaForObjects}}('DeductionBenefitScheduleDetail') to get more field paths.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{listSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Benefit Management
	#' @return A dataframe or of DeductionBenefitScheduleDetail
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	getDeductionBenefitScheduleDetail <- function(DeductionBenefitScheduleDetailID, DeductionBenefitScheduleID = F, CheckDate = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, fieldPaths = NULL, entityId = 1, schoolYearId = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment()) %>% purrr::keep(names(.) != "DeductionBenefitScheduleDetailID")

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		getSkyObject(module = "BenefitManagement", objectName = "DeductionBenefitScheduleDetail", objectId = DeductionBenefitScheduleDetailID, searchFields = searchFields, entityId = entityId, flatten = flatten, returnResponse = returnResponse)
	}

	#' Delete a DeductionBenefitScheduleDetail
	#'
	#' This function deletes a DeductionBenefitScheduleDetail
	#' @param DeductionBenefitScheduleDetailID The ID of the DeductionBenefitScheduleDetail to delete
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{listSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Benefit Management
	#' @return The DeductionBenefitScheduleDetailID of the deleted DeductionBenefitScheduleDetail.
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	deleteDeductionBenefitScheduleDetail <- function(DeductionBenefitScheduleDetailID, ignoreWarnings = F, entityId = 1, schoolYearId = NULL, flatten = T, returnResponse = F){

		deleteSkyObject(module = "BenefitManagement", objectName = "DeductionBenefitScheduleDetail", objectId = DeductionBenefitScheduleDetailID, ignoreWarnings = ignoreWarnings, entityId = entityId, flatten = flatten, returnResponse = returnResponse)
	}

	#' Create a DeductionBenefitScheduleDetail
	#'
	#' This function creates a DeductionBenefitScheduleDetail
	#' @param fieldNames The field values to give the created DeductionBenefitScheduleDetail. Each defaults to NULL.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{listSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Benefit Management
	#' @return A newly created DeductionBenefitScheduleDetail
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	createDeductionBenefitScheduleDetail <- function(DeductionBenefitScheduleID = NULL, CheckDate = NULL, entityId = 1, schoolYearId = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment())

		body <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper()) %>% purrr::compact()

		createSkyObject(module = "BenefitManagement", objectName = "DeductionBenefitScheduleDetail", body = list(DataObject = body), searchFields = append("DeductionBenefitScheduleDetailID", body %>% names()), entityId = entityId, schoolYearId = schoolYearId, flatten = flatten, returnResponse = returnResponse)
	}

	#' Modify a DeductionBenefitScheduleDetail
	#'
	#' This function modifies a DeductionBenefitScheduleDetail
	#' @param fieldNames The field values to give the modified DeductionBenefitScheduleDetail. Each defaults to NULL.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{listSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Benefit Management
	#' @return The modified DeductionBenefitScheduleDetail
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	modifyDeductionBenefitScheduleDetail <- function(DeductionBenefitScheduleDetailID, DeductionBenefitScheduleID = NULL, CheckDate = NULL, entityId = 1, schoolYearId = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment())

		body <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper()) %>% purrr::compact()

		modifySkyObject(module = "BenefitManagement", objectName = "DeductionBenefitScheduleDetail", objectId = DeductionBenefitScheduleDetailID, body = list(DataObject = body), searchFields = append("DeductionBenefitScheduleDetailID", body %>% names()), entityId = entityId, schoolYearId = schoolYearId, flatten = flatten, returnResponse = returnResponse)
	}

	#' List TempDeductionBenefitScheduleDetails
	#'
	#' This function returns a dataframe or json object of TempDeductionBenefitScheduleDetails
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given TempDeductionBenefitScheduleDetails. Defaults to FALSE for all return fields which, for convenience, returns all fields for the TempDeductionBenefitScheduleDetails.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object listed as text. Run \code{\link{getSchemaForObjects}}('TempDeductionBenefitScheduleDetail') to get more field paths.
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
	#' @concept Benefit Management
	#' @return A list of TempDeductionBenefitScheduleDetails
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	listTempDeductionBenefitScheduleDetails <- function(searchConditionsList = NULL, TempDeductionBenefitScheduleDetailID = F, CheckDate = F, HasError = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, DeductionBenefitScheduleID = F, DeductionBenefitScheduleCodeDescription = F, OldCheckDate = F, fieldPaths = NULL, searchConditionsGroupType = "And", searchSortFieldNamesList = NULL, searchSortFieldNamesDescendingList = NULL, entityId = 1, schoolYearId = NULL, page = 1, pageSize = 100000, flatten = T, returnResponse = F){

		params <- as.list(environment())

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		listSkyObjects(module = "BenefitManagement", objectName = "TempDeductionBenefitScheduleDetail", searchFields = searchFields %>% append(fieldPaths), page = page, pageSize = pageSize, SearchConditionsList = searchConditionsList, SearchConditionsGroupType = searchConditionsGroupType, SearchSortFieldNamesList = searchSortFieldNamesList, SearchSortFieldNamesDescendingList = searchSortFieldNamesDescendingList, entityId = entityId, schoolYearId = schoolYearId, flatten = flatten, returnResponse = returnResponse)
	}

	#' Get a TempDeductionBenefitScheduleDetail
	#'
	#' This function returns a dataframe or json object of a TempDeductionBenefitScheduleDetail
	#' @param TempDeductionBenefitScheduleDetailID The ID of the TempDeductionBenefitScheduleDetail to return.
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given TempDeductionBenefitScheduleDetail. Defaults to FALSE for all return fields which, for convenience, returns all fields for the TempDeductionBenefitScheduleDetail.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object listed as text. Run \code{\link{getSchemaForObjects}}('TempDeductionBenefitScheduleDetail') to get more field paths.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{listSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Benefit Management
	#' @return A dataframe or of TempDeductionBenefitScheduleDetail
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	getTempDeductionBenefitScheduleDetail <- function(TempDeductionBenefitScheduleDetailID, CheckDate = F, HasError = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, DeductionBenefitScheduleID = F, DeductionBenefitScheduleCodeDescription = F, OldCheckDate = F, fieldPaths = NULL, entityId = 1, schoolYearId = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment()) %>% purrr::keep(names(.) != "TempDeductionBenefitScheduleDetailID")

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		getSkyObject(module = "BenefitManagement", objectName = "TempDeductionBenefitScheduleDetail", objectId = TempDeductionBenefitScheduleDetailID, searchFields = searchFields, entityId = entityId, flatten = flatten, returnResponse = returnResponse)
	}

	#' Delete a TempDeductionBenefitScheduleDetail
	#'
	#' This function deletes a TempDeductionBenefitScheduleDetail
	#' @param TempDeductionBenefitScheduleDetailID The ID of the TempDeductionBenefitScheduleDetail to delete
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{listSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Benefit Management
	#' @return The TempDeductionBenefitScheduleDetailID of the deleted TempDeductionBenefitScheduleDetail.
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	deleteTempDeductionBenefitScheduleDetail <- function(TempDeductionBenefitScheduleDetailID, ignoreWarnings = F, entityId = 1, schoolYearId = NULL, flatten = T, returnResponse = F){

		deleteSkyObject(module = "BenefitManagement", objectName = "TempDeductionBenefitScheduleDetail", objectId = TempDeductionBenefitScheduleDetailID, ignoreWarnings = ignoreWarnings, entityId = entityId, flatten = flatten, returnResponse = returnResponse)
	}

	#' Create a TempDeductionBenefitScheduleDetail
	#'
	#' This function creates a TempDeductionBenefitScheduleDetail
	#' @param fieldNames The field values to give the created TempDeductionBenefitScheduleDetail. Each defaults to NULL.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{listSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Benefit Management
	#' @return A newly created TempDeductionBenefitScheduleDetail
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	createTempDeductionBenefitScheduleDetail <- function(CheckDate = NULL, HasError = NULL, DeductionBenefitScheduleID = NULL, DeductionBenefitScheduleCodeDescription = NULL, OldCheckDate = NULL, entityId = 1, schoolYearId = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment())

		body <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper()) %>% purrr::compact()

		createSkyObject(module = "BenefitManagement", objectName = "TempDeductionBenefitScheduleDetail", body = list(DataObject = body), searchFields = append("TempDeductionBenefitScheduleDetailID", body %>% names()), entityId = entityId, schoolYearId = schoolYearId, flatten = flatten, returnResponse = returnResponse)
	}

	#' Modify a TempDeductionBenefitScheduleDetail
	#'
	#' This function modifies a TempDeductionBenefitScheduleDetail
	#' @param fieldNames The field values to give the modified TempDeductionBenefitScheduleDetail. Each defaults to NULL.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{listSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Benefit Management
	#' @return The modified TempDeductionBenefitScheduleDetail
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	modifyTempDeductionBenefitScheduleDetail <- function(TempDeductionBenefitScheduleDetailID, CheckDate = NULL, HasError = NULL, DeductionBenefitScheduleID = NULL, DeductionBenefitScheduleCodeDescription = NULL, OldCheckDate = NULL, entityId = 1, schoolYearId = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment())

		body <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper()) %>% purrr::compact()

		modifySkyObject(module = "BenefitManagement", objectName = "TempDeductionBenefitScheduleDetail", objectId = TempDeductionBenefitScheduleDetailID, body = list(DataObject = body), searchFields = append("TempDeductionBenefitScheduleDetailID", body %>% names()), entityId = entityId, schoolYearId = schoolYearId, flatten = flatten, returnResponse = returnResponse)
	}

	#' List TempDeductionBenefitScheduleDetailErrors
	#'
	#' This function returns a dataframe or json object of TempDeductionBenefitScheduleDetailErrors
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given TempDeductionBenefitScheduleDetailErrors. Defaults to FALSE for all return fields which, for convenience, returns all fields for the TempDeductionBenefitScheduleDetailErrors.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object listed as text. Run \code{\link{getSchemaForObjects}}('TempDeductionBenefitScheduleDetailError') to get more field paths.
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
	#' @concept Benefit Management
	#' @return A list of TempDeductionBenefitScheduleDetailErrors
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	listTempDeductionBenefitScheduleDetailErrors <- function(searchConditionsList = NULL, TempDeductionBenefitScheduleDetailErrorID = F, TempDeductionBenefitScheduleDetailID = F, Error = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, fieldPaths = NULL, searchConditionsGroupType = "And", searchSortFieldNamesList = NULL, searchSortFieldNamesDescendingList = NULL, entityId = 1, schoolYearId = NULL, page = 1, pageSize = 100000, flatten = T, returnResponse = F){

		params <- as.list(environment())

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		listSkyObjects(module = "BenefitManagement", objectName = "TempDeductionBenefitScheduleDetailError", searchFields = searchFields %>% append(fieldPaths), page = page, pageSize = pageSize, SearchConditionsList = searchConditionsList, SearchConditionsGroupType = searchConditionsGroupType, SearchSortFieldNamesList = searchSortFieldNamesList, SearchSortFieldNamesDescendingList = searchSortFieldNamesDescendingList, entityId = entityId, schoolYearId = schoolYearId, flatten = flatten, returnResponse = returnResponse)
	}

	#' Get a TempDeductionBenefitScheduleDetailError
	#'
	#' This function returns a dataframe or json object of a TempDeductionBenefitScheduleDetailError
	#' @param TempDeductionBenefitScheduleDetailErrorID The ID of the TempDeductionBenefitScheduleDetailError to return.
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given TempDeductionBenefitScheduleDetailError. Defaults to FALSE for all return fields which, for convenience, returns all fields for the TempDeductionBenefitScheduleDetailError.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object listed as text. Run \code{\link{getSchemaForObjects}}('TempDeductionBenefitScheduleDetailError') to get more field paths.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{listSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Benefit Management
	#' @return A dataframe or of TempDeductionBenefitScheduleDetailError
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	getTempDeductionBenefitScheduleDetailError <- function(TempDeductionBenefitScheduleDetailErrorID, TempDeductionBenefitScheduleDetailID = F, Error = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, fieldPaths = NULL, entityId = 1, schoolYearId = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment()) %>% purrr::keep(names(.) != "TempDeductionBenefitScheduleDetailErrorID")

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		getSkyObject(module = "BenefitManagement", objectName = "TempDeductionBenefitScheduleDetailError", objectId = TempDeductionBenefitScheduleDetailErrorID, searchFields = searchFields, entityId = entityId, flatten = flatten, returnResponse = returnResponse)
	}

	#' Delete a TempDeductionBenefitScheduleDetailError
	#'
	#' This function deletes a TempDeductionBenefitScheduleDetailError
	#' @param TempDeductionBenefitScheduleDetailErrorID The ID of the TempDeductionBenefitScheduleDetailError to delete
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{listSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Benefit Management
	#' @return The TempDeductionBenefitScheduleDetailErrorID of the deleted TempDeductionBenefitScheduleDetailError.
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	deleteTempDeductionBenefitScheduleDetailError <- function(TempDeductionBenefitScheduleDetailErrorID, ignoreWarnings = F, entityId = 1, schoolYearId = NULL, flatten = T, returnResponse = F){

		deleteSkyObject(module = "BenefitManagement", objectName = "TempDeductionBenefitScheduleDetailError", objectId = TempDeductionBenefitScheduleDetailErrorID, ignoreWarnings = ignoreWarnings, entityId = entityId, flatten = flatten, returnResponse = returnResponse)
	}

	#' Create a TempDeductionBenefitScheduleDetailError
	#'
	#' This function creates a TempDeductionBenefitScheduleDetailError
	#' @param fieldNames The field values to give the created TempDeductionBenefitScheduleDetailError. Each defaults to NULL.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{listSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Benefit Management
	#' @return A newly created TempDeductionBenefitScheduleDetailError
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	createTempDeductionBenefitScheduleDetailError <- function(TempDeductionBenefitScheduleDetailID = NULL, Error = NULL, entityId = 1, schoolYearId = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment())

		body <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper()) %>% purrr::compact()

		createSkyObject(module = "BenefitManagement", objectName = "TempDeductionBenefitScheduleDetailError", body = list(DataObject = body), searchFields = append("TempDeductionBenefitScheduleDetailErrorID", body %>% names()), entityId = entityId, schoolYearId = schoolYearId, flatten = flatten, returnResponse = returnResponse)
	}

	#' Modify a TempDeductionBenefitScheduleDetailError
	#'
	#' This function modifies a TempDeductionBenefitScheduleDetailError
	#' @param fieldNames The field values to give the modified TempDeductionBenefitScheduleDetailError. Each defaults to NULL.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{listSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Benefit Management
	#' @return The modified TempDeductionBenefitScheduleDetailError
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	modifyTempDeductionBenefitScheduleDetailError <- function(TempDeductionBenefitScheduleDetailErrorID, TempDeductionBenefitScheduleDetailID = NULL, Error = NULL, entityId = 1, schoolYearId = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment())

		body <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper()) %>% purrr::compact()

		modifySkyObject(module = "BenefitManagement", objectName = "TempDeductionBenefitScheduleDetailError", objectId = TempDeductionBenefitScheduleDetailErrorID, body = list(DataObject = body), searchFields = append("TempDeductionBenefitScheduleDetailErrorID", body %>% names()), entityId = entityId, schoolYearId = schoolYearId, flatten = flatten, returnResponse = returnResponse)
	}

	#' List Bills
	#'
	#' This function returns a dataframe or json object of Bills
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given Bills. Defaults to FALSE for all return fields which, for convenience, returns all fields for the Bills.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object listed as text. Run \code{\link{getSchemaForObjects}}('Bill') to get more field paths.
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
	#' @concept Benefit Management
	#' @return A list of Bills
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	listBills <- function(searchConditionsList = NULL, BillID = F, PlanID = F, Month = F, MonthName = F, Year = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, InvoiceID = F, BillIdentifier = F, fieldPaths = NULL, searchConditionsGroupType = "And", searchSortFieldNamesList = NULL, searchSortFieldNamesDescendingList = NULL, entityId = 1, schoolYearId = NULL, page = 1, pageSize = 100000, flatten = T, returnResponse = F){

		params <- as.list(environment())

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		listSkyObjects(module = "BenefitManagement", objectName = "Bill", searchFields = searchFields %>% append(fieldPaths), page = page, pageSize = pageSize, SearchConditionsList = searchConditionsList, SearchConditionsGroupType = searchConditionsGroupType, SearchSortFieldNamesList = searchSortFieldNamesList, SearchSortFieldNamesDescendingList = searchSortFieldNamesDescendingList, entityId = entityId, schoolYearId = schoolYearId, flatten = flatten, returnResponse = returnResponse)
	}

	#' Get a Bill
	#'
	#' This function returns a dataframe or json object of a Bill
	#' @param BillID The ID of the Bill to return.
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given Bill. Defaults to FALSE for all return fields which, for convenience, returns all fields for the Bill.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object listed as text. Run \code{\link{getSchemaForObjects}}('Bill') to get more field paths.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{listSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Benefit Management
	#' @return A dataframe or of Bill
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	getBill <- function(BillID, PlanID = F, Month = F, MonthName = F, Year = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, InvoiceID = F, BillIdentifier = F, fieldPaths = NULL, entityId = 1, schoolYearId = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment()) %>% purrr::keep(names(.) != "BillID")

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		getSkyObject(module = "BenefitManagement", objectName = "Bill", objectId = BillID, searchFields = searchFields, entityId = entityId, flatten = flatten, returnResponse = returnResponse)
	}

	#' Delete a Bill
	#'
	#' This function deletes a Bill
	#' @param BillID The ID of the Bill to delete
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{listSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Benefit Management
	#' @return The BillID of the deleted Bill.
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	deleteBill <- function(BillID, ignoreWarnings = F, entityId = 1, schoolYearId = NULL, flatten = T, returnResponse = F){

		deleteSkyObject(module = "BenefitManagement", objectName = "Bill", objectId = BillID, ignoreWarnings = ignoreWarnings, entityId = entityId, flatten = flatten, returnResponse = returnResponse)
	}

	#' Create a Bill
	#'
	#' This function creates a Bill
	#' @param fieldNames The field values to give the created Bill. Each defaults to NULL.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{listSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Benefit Management
	#' @return A newly created Bill
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	createBill <- function(PlanID = NULL, Month = NULL, Year = NULL, InvoiceID = NULL, entityId = 1, schoolYearId = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment())

		body <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper()) %>% purrr::compact()

		createSkyObject(module = "BenefitManagement", objectName = "Bill", body = list(DataObject = body), searchFields = append("BillID", body %>% names()), entityId = entityId, schoolYearId = schoolYearId, flatten = flatten, returnResponse = returnResponse)
	}

	#' Modify a Bill
	#'
	#' This function modifies a Bill
	#' @param fieldNames The field values to give the modified Bill. Each defaults to NULL.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{listSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Benefit Management
	#' @return The modified Bill
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	modifyBill <- function(BillID, PlanID = NULL, Month = NULL, Year = NULL, InvoiceID = NULL, entityId = 1, schoolYearId = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment())

		body <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper()) %>% purrr::compact()

		modifySkyObject(module = "BenefitManagement", objectName = "Bill", objectId = BillID, body = list(DataObject = body), searchFields = append("BillID", body %>% names()), entityId = entityId, schoolYearId = schoolYearId, flatten = flatten, returnResponse = returnResponse)
	}

	#' List BillDetails
	#'
	#' This function returns a dataframe or json object of BillDetails
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given BillDetails. Defaults to FALSE for all return fields which, for convenience, returns all fields for the BillDetails.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object listed as text. Run \code{\link{getSchemaForObjects}}('BillDetail') to get more field paths.
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
	#' @concept Benefit Management
	#' @return A list of BillDetails
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	listBillDetails <- function(searchConditionsList = NULL, BillDetailID = F, BillID = F, EmployeeID = F, BaseCurrencyAmount = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, HasOneEmployeePlanEnrollmentBillDetail = F, fieldPaths = NULL, searchConditionsGroupType = "And", searchSortFieldNamesList = NULL, searchSortFieldNamesDescendingList = NULL, entityId = 1, schoolYearId = NULL, page = 1, pageSize = 100000, flatten = T, returnResponse = F){

		params <- as.list(environment())

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		listSkyObjects(module = "BenefitManagement", objectName = "BillDetail", searchFields = searchFields %>% append(fieldPaths), page = page, pageSize = pageSize, SearchConditionsList = searchConditionsList, SearchConditionsGroupType = searchConditionsGroupType, SearchSortFieldNamesList = searchSortFieldNamesList, SearchSortFieldNamesDescendingList = searchSortFieldNamesDescendingList, entityId = entityId, schoolYearId = schoolYearId, flatten = flatten, returnResponse = returnResponse)
	}

	#' Get a BillDetail
	#'
	#' This function returns a dataframe or json object of a BillDetail
	#' @param BillDetailID The ID of the BillDetail to return.
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given BillDetail. Defaults to FALSE for all return fields which, for convenience, returns all fields for the BillDetail.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object listed as text. Run \code{\link{getSchemaForObjects}}('BillDetail') to get more field paths.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{listSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Benefit Management
	#' @return A dataframe or of BillDetail
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	getBillDetail <- function(BillDetailID, BillID = F, EmployeeID = F, BaseCurrencyAmount = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, HasOneEmployeePlanEnrollmentBillDetail = F, fieldPaths = NULL, entityId = 1, schoolYearId = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment()) %>% purrr::keep(names(.) != "BillDetailID")

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		getSkyObject(module = "BenefitManagement", objectName = "BillDetail", objectId = BillDetailID, searchFields = searchFields, entityId = entityId, flatten = flatten, returnResponse = returnResponse)
	}

	#' Delete a BillDetail
	#'
	#' This function deletes a BillDetail
	#' @param BillDetailID The ID of the BillDetail to delete
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{listSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Benefit Management
	#' @return The BillDetailID of the deleted BillDetail.
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	deleteBillDetail <- function(BillDetailID, ignoreWarnings = F, entityId = 1, schoolYearId = NULL, flatten = T, returnResponse = F){

		deleteSkyObject(module = "BenefitManagement", objectName = "BillDetail", objectId = BillDetailID, ignoreWarnings = ignoreWarnings, entityId = entityId, flatten = flatten, returnResponse = returnResponse)
	}

	#' Create a BillDetail
	#'
	#' This function creates a BillDetail
	#' @param fieldNames The field values to give the created BillDetail. Each defaults to NULL.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{listSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Benefit Management
	#' @return A newly created BillDetail
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	createBillDetail <- function(BillID = NULL, EmployeeID = NULL, entityId = 1, schoolYearId = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment())

		body <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper()) %>% purrr::compact()

		createSkyObject(module = "BenefitManagement", objectName = "BillDetail", body = list(DataObject = body), searchFields = append("BillDetailID", body %>% names()), entityId = entityId, schoolYearId = schoolYearId, flatten = flatten, returnResponse = returnResponse)
	}

	#' Modify a BillDetail
	#'
	#' This function modifies a BillDetail
	#' @param fieldNames The field values to give the modified BillDetail. Each defaults to NULL.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{listSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Benefit Management
	#' @return The modified BillDetail
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	modifyBillDetail <- function(BillDetailID, BillID = NULL, EmployeeID = NULL, entityId = 1, schoolYearId = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment())

		body <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper()) %>% purrr::compact()

		modifySkyObject(module = "BenefitManagement", objectName = "BillDetail", objectId = BillDetailID, body = list(DataObject = body), searchFields = append("BillDetailID", body %>% names()), entityId = entityId, schoolYearId = schoolYearId, flatten = flatten, returnResponse = returnResponse)
	}

	#' List TempUpdatePayrollDataInfos
	#'
	#' This function returns a dataframe or json object of TempUpdatePayrollDataInfos
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given TempUpdatePayrollDataInfos. Defaults to FALSE for all return fields which, for convenience, returns all fields for the TempUpdatePayrollDataInfos.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object listed as text. Run \code{\link{getSchemaForObjects}}('TempUpdatePayrollDataInfo') to get more field paths.
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
	#' @concept Benefit Management
	#' @return A list of TempUpdatePayrollDataInfos
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	listTempUpdatePayrollDataInfos <- function(searchConditionsList = NULL, TempUpdatePayrollDataInfoID = F, Employee = F, IsPreProcessException = F, PreProcessException = F, EmployeeBenefitID = F, EmployeeDeductionID = F, OldBenefitAmount = F, NewBenefitAmount = F, OldDeductionAmount = F, NewDeductionAmount = F, TotalBenefitAmount = F, TotalDeductionAmount = F, TotalPremiumAmount = F, TotalBenefitPaid = F, TotalDeductionPaid = F, RemainingPays = F, HasProcessError = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, EmployeeNumber = F, Plan = F, Benefit = F, Deduction = F, EmployeePlanEnrollmentID = F, CalculationDateCoverage = F, CalculationDateBenefitGroup = F, CalculationDateBenefitPercentage = F, fieldPaths = NULL, searchConditionsGroupType = "And", searchSortFieldNamesList = NULL, searchSortFieldNamesDescendingList = NULL, entityId = 1, schoolYearId = NULL, page = 1, pageSize = 100000, flatten = T, returnResponse = F){

		params <- as.list(environment())

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		listSkyObjects(module = "BenefitManagement", objectName = "TempUpdatePayrollDataInfo", searchFields = searchFields %>% append(fieldPaths), page = page, pageSize = pageSize, SearchConditionsList = searchConditionsList, SearchConditionsGroupType = searchConditionsGroupType, SearchSortFieldNamesList = searchSortFieldNamesList, SearchSortFieldNamesDescendingList = searchSortFieldNamesDescendingList, entityId = entityId, schoolYearId = schoolYearId, flatten = flatten, returnResponse = returnResponse)
	}

	#' Get a TempUpdatePayrollDataInfo
	#'
	#' This function returns a dataframe or json object of a TempUpdatePayrollDataInfo
	#' @param TempUpdatePayrollDataInfoID The ID of the TempUpdatePayrollDataInfo to return.
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given TempUpdatePayrollDataInfo. Defaults to FALSE for all return fields which, for convenience, returns all fields for the TempUpdatePayrollDataInfo.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object listed as text. Run \code{\link{getSchemaForObjects}}('TempUpdatePayrollDataInfo') to get more field paths.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{listSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Benefit Management
	#' @return A dataframe or of TempUpdatePayrollDataInfo
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	getTempUpdatePayrollDataInfo <- function(TempUpdatePayrollDataInfoID, Employee = F, IsPreProcessException = F, PreProcessException = F, EmployeeBenefitID = F, EmployeeDeductionID = F, OldBenefitAmount = F, NewBenefitAmount = F, OldDeductionAmount = F, NewDeductionAmount = F, TotalBenefitAmount = F, TotalDeductionAmount = F, TotalPremiumAmount = F, TotalBenefitPaid = F, TotalDeductionPaid = F, RemainingPays = F, HasProcessError = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, EmployeeNumber = F, Plan = F, Benefit = F, Deduction = F, EmployeePlanEnrollmentID = F, CalculationDateCoverage = F, CalculationDateBenefitGroup = F, CalculationDateBenefitPercentage = F, fieldPaths = NULL, entityId = 1, schoolYearId = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment()) %>% purrr::keep(names(.) != "TempUpdatePayrollDataInfoID")

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		getSkyObject(module = "BenefitManagement", objectName = "TempUpdatePayrollDataInfo", objectId = TempUpdatePayrollDataInfoID, searchFields = searchFields, entityId = entityId, flatten = flatten, returnResponse = returnResponse)
	}

	#' Delete a TempUpdatePayrollDataInfo
	#'
	#' This function deletes a TempUpdatePayrollDataInfo
	#' @param TempUpdatePayrollDataInfoID The ID of the TempUpdatePayrollDataInfo to delete
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{listSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Benefit Management
	#' @return The TempUpdatePayrollDataInfoID of the deleted TempUpdatePayrollDataInfo.
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	deleteTempUpdatePayrollDataInfo <- function(TempUpdatePayrollDataInfoID, ignoreWarnings = F, entityId = 1, schoolYearId = NULL, flatten = T, returnResponse = F){

		deleteSkyObject(module = "BenefitManagement", objectName = "TempUpdatePayrollDataInfo", objectId = TempUpdatePayrollDataInfoID, ignoreWarnings = ignoreWarnings, entityId = entityId, flatten = flatten, returnResponse = returnResponse)
	}

	#' Create a TempUpdatePayrollDataInfo
	#'
	#' This function creates a TempUpdatePayrollDataInfo
	#' @param fieldNames The field values to give the created TempUpdatePayrollDataInfo. Each defaults to NULL.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{listSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Benefit Management
	#' @return A newly created TempUpdatePayrollDataInfo
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	createTempUpdatePayrollDataInfo <- function(Employee = NULL, IsPreProcessException = NULL, PreProcessException = NULL, EmployeeBenefitID = NULL, EmployeeDeductionID = NULL, OldBenefitAmount = NULL, NewBenefitAmount = NULL, OldDeductionAmount = NULL, NewDeductionAmount = NULL, TotalBenefitAmount = NULL, TotalDeductionAmount = NULL, TotalPremiumAmount = NULL, TotalBenefitPaid = NULL, TotalDeductionPaid = NULL, RemainingPays = NULL, HasProcessError = NULL, EmployeeNumber = NULL, Plan = NULL, Benefit = NULL, Deduction = NULL, EmployeePlanEnrollmentID = NULL, CalculationDateCoverage = NULL, CalculationDateBenefitGroup = NULL, CalculationDateBenefitPercentage = NULL, entityId = 1, schoolYearId = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment())

		body <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper()) %>% purrr::compact()

		createSkyObject(module = "BenefitManagement", objectName = "TempUpdatePayrollDataInfo", body = list(DataObject = body), searchFields = append("TempUpdatePayrollDataInfoID", body %>% names()), entityId = entityId, schoolYearId = schoolYearId, flatten = flatten, returnResponse = returnResponse)
	}

	#' Modify a TempUpdatePayrollDataInfo
	#'
	#' This function modifies a TempUpdatePayrollDataInfo
	#' @param fieldNames The field values to give the modified TempUpdatePayrollDataInfo. Each defaults to NULL.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{listSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Benefit Management
	#' @return The modified TempUpdatePayrollDataInfo
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	modifyTempUpdatePayrollDataInfo <- function(TempUpdatePayrollDataInfoID, Employee = NULL, IsPreProcessException = NULL, PreProcessException = NULL, EmployeeBenefitID = NULL, EmployeeDeductionID = NULL, OldBenefitAmount = NULL, NewBenefitAmount = NULL, OldDeductionAmount = NULL, NewDeductionAmount = NULL, TotalBenefitAmount = NULL, TotalDeductionAmount = NULL, TotalPremiumAmount = NULL, TotalBenefitPaid = NULL, TotalDeductionPaid = NULL, RemainingPays = NULL, HasProcessError = NULL, EmployeeNumber = NULL, Plan = NULL, Benefit = NULL, Deduction = NULL, EmployeePlanEnrollmentID = NULL, CalculationDateCoverage = NULL, CalculationDateBenefitGroup = NULL, CalculationDateBenefitPercentage = NULL, entityId = 1, schoolYearId = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment())

		body <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper()) %>% purrr::compact()

		modifySkyObject(module = "BenefitManagement", objectName = "TempUpdatePayrollDataInfo", objectId = TempUpdatePayrollDataInfoID, body = list(DataObject = body), searchFields = append("TempUpdatePayrollDataInfoID", body %>% names()), entityId = entityId, schoolYearId = schoolYearId, flatten = flatten, returnResponse = returnResponse)
	}

	#' List TempUpdatePayrollDataInfoExceptions
	#'
	#' This function returns a dataframe or json object of TempUpdatePayrollDataInfoExceptions
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given TempUpdatePayrollDataInfoExceptions. Defaults to FALSE for all return fields which, for convenience, returns all fields for the TempUpdatePayrollDataInfoExceptions.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object listed as text. Run \code{\link{getSchemaForObjects}}('TempUpdatePayrollDataInfoException') to get more field paths.
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
	#' @concept Benefit Management
	#' @return A list of TempUpdatePayrollDataInfoExceptions
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	listTempUpdatePayrollDataInfoExceptions <- function(searchConditionsList = NULL, TempUpdatePayrollDataInfoExceptionID = F, TempUpdatePayrollDataInfoID = F, Error = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, fieldPaths = NULL, searchConditionsGroupType = "And", searchSortFieldNamesList = NULL, searchSortFieldNamesDescendingList = NULL, entityId = 1, schoolYearId = NULL, page = 1, pageSize = 100000, flatten = T, returnResponse = F){

		params <- as.list(environment())

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		listSkyObjects(module = "BenefitManagement", objectName = "TempUpdatePayrollDataInfoException", searchFields = searchFields %>% append(fieldPaths), page = page, pageSize = pageSize, SearchConditionsList = searchConditionsList, SearchConditionsGroupType = searchConditionsGroupType, SearchSortFieldNamesList = searchSortFieldNamesList, SearchSortFieldNamesDescendingList = searchSortFieldNamesDescendingList, entityId = entityId, schoolYearId = schoolYearId, flatten = flatten, returnResponse = returnResponse)
	}

	#' Get a TempUpdatePayrollDataInfoException
	#'
	#' This function returns a dataframe or json object of a TempUpdatePayrollDataInfoException
	#' @param TempUpdatePayrollDataInfoExceptionID The ID of the TempUpdatePayrollDataInfoException to return.
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given TempUpdatePayrollDataInfoException. Defaults to FALSE for all return fields which, for convenience, returns all fields for the TempUpdatePayrollDataInfoException.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object listed as text. Run \code{\link{getSchemaForObjects}}('TempUpdatePayrollDataInfoException') to get more field paths.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{listSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Benefit Management
	#' @return A dataframe or of TempUpdatePayrollDataInfoException
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	getTempUpdatePayrollDataInfoException <- function(TempUpdatePayrollDataInfoExceptionID, TempUpdatePayrollDataInfoID = F, Error = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, fieldPaths = NULL, entityId = 1, schoolYearId = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment()) %>% purrr::keep(names(.) != "TempUpdatePayrollDataInfoExceptionID")

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		getSkyObject(module = "BenefitManagement", objectName = "TempUpdatePayrollDataInfoException", objectId = TempUpdatePayrollDataInfoExceptionID, searchFields = searchFields, entityId = entityId, flatten = flatten, returnResponse = returnResponse)
	}

	#' Delete a TempUpdatePayrollDataInfoException
	#'
	#' This function deletes a TempUpdatePayrollDataInfoException
	#' @param TempUpdatePayrollDataInfoExceptionID The ID of the TempUpdatePayrollDataInfoException to delete
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{listSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Benefit Management
	#' @return The TempUpdatePayrollDataInfoExceptionID of the deleted TempUpdatePayrollDataInfoException.
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	deleteTempUpdatePayrollDataInfoException <- function(TempUpdatePayrollDataInfoExceptionID, ignoreWarnings = F, entityId = 1, schoolYearId = NULL, flatten = T, returnResponse = F){

		deleteSkyObject(module = "BenefitManagement", objectName = "TempUpdatePayrollDataInfoException", objectId = TempUpdatePayrollDataInfoExceptionID, ignoreWarnings = ignoreWarnings, entityId = entityId, flatten = flatten, returnResponse = returnResponse)
	}

	#' Create a TempUpdatePayrollDataInfoException
	#'
	#' This function creates a TempUpdatePayrollDataInfoException
	#' @param fieldNames The field values to give the created TempUpdatePayrollDataInfoException. Each defaults to NULL.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{listSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Benefit Management
	#' @return A newly created TempUpdatePayrollDataInfoException
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	createTempUpdatePayrollDataInfoException <- function(TempUpdatePayrollDataInfoID = NULL, Error = NULL, entityId = 1, schoolYearId = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment())

		body <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper()) %>% purrr::compact()

		createSkyObject(module = "BenefitManagement", objectName = "TempUpdatePayrollDataInfoException", body = list(DataObject = body), searchFields = append("TempUpdatePayrollDataInfoExceptionID", body %>% names()), entityId = entityId, schoolYearId = schoolYearId, flatten = flatten, returnResponse = returnResponse)
	}

	#' Modify a TempUpdatePayrollDataInfoException
	#'
	#' This function modifies a TempUpdatePayrollDataInfoException
	#' @param fieldNames The field values to give the modified TempUpdatePayrollDataInfoException. Each defaults to NULL.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{listSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Benefit Management
	#' @return The modified TempUpdatePayrollDataInfoException
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	modifyTempUpdatePayrollDataInfoException <- function(TempUpdatePayrollDataInfoExceptionID, TempUpdatePayrollDataInfoID = NULL, Error = NULL, entityId = 1, schoolYearId = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment())

		body <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper()) %>% purrr::compact()

		modifySkyObject(module = "BenefitManagement", objectName = "TempUpdatePayrollDataInfoException", objectId = TempUpdatePayrollDataInfoExceptionID, body = list(DataObject = body), searchFields = append("TempUpdatePayrollDataInfoExceptionID", body %>% names()), entityId = entityId, schoolYearId = schoolYearId, flatten = flatten, returnResponse = returnResponse)
	}

	#' List TempEmployeePlanEnrollments
	#'
	#' This function returns a dataframe or json object of TempEmployeePlanEnrollments
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given TempEmployeePlanEnrollments. Defaults to FALSE for all return fields which, for convenience, returns all fields for the TempEmployeePlanEnrollments.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object listed as text. Run \code{\link{getSchemaForObjects}}('TempEmployeePlanEnrollment') to get more field paths.
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
	#' @concept Benefit Management
	#' @return A list of TempEmployeePlanEnrollments
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	listTempEmployeePlanEnrollments <- function(searchConditionsList = NULL, TempEmployeePlanEnrollmentID = F, EmployeeID = F, PlanID = F, EmployeeFullNameLFM = F, SubPlanID = F, SubPlanIdentifier = F, DeductionBenefitDescription = F, DeductionBenefitValue = F, EnteredDeductionAmount = F, EnteredBenefitAmount = F, PremiumAmount = F, CoverageStartDate = F, CoverageEndDate = F, PaymentStartDate = F, PaymentEndDate = F, HasError = F, ErrorNumber = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, CoverageAmount = F, CalculateWithFTE = F, DeductionBenefitScheduleID = F, EmployeeNumber = F, BenefitFTEOverride = F, BenefitFTESource = F, EmployeePlanEnrollmentIDClonedFrom = F, PlanCode = F, OldCoverageStartDate = F, OldCoverageEndDate = F, OldPaymentStartDate = F, OldPaymentEndDate = F, OldFinalCoverageMonthID = F, OldFinalBenefitID = F, OldFinalBenefitValueOverride = F, OldFinalBenefitMaximumValueOverride = F, OldFinalDeductionID = F, OldFinalDeductionValueOverride = F, OldFinalDeductionMaximumValueOverride = F, EmployeePlanEnrollmentIdentifier = F, DistrictID = F, CoverageMonthCount = F, CurrentCoverageMonthBenefitAmount = F, CurrentCoverageMonthDeductionAmount = F, PlanCodeDescription = F, CalculationDateCoverage = F, CalculationDateBenefitGroup = F, CalculationDateBenefitPercentage = F, InsuranceYearCoverageMonthCount = F, InsuranceYearPayCount = F, fieldPaths = NULL, searchConditionsGroupType = "And", searchSortFieldNamesList = NULL, searchSortFieldNamesDescendingList = NULL, entityId = 1, schoolYearId = NULL, page = 1, pageSize = 100000, flatten = T, returnResponse = F){

		params <- as.list(environment())

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		listSkyObjects(module = "BenefitManagement", objectName = "TempEmployeePlanEnrollment", searchFields = searchFields %>% append(fieldPaths), page = page, pageSize = pageSize, SearchConditionsList = searchConditionsList, SearchConditionsGroupType = searchConditionsGroupType, SearchSortFieldNamesList = searchSortFieldNamesList, SearchSortFieldNamesDescendingList = searchSortFieldNamesDescendingList, entityId = entityId, schoolYearId = schoolYearId, flatten = flatten, returnResponse = returnResponse)
	}

	#' Get a TempEmployeePlanEnrollment
	#'
	#' This function returns a dataframe or json object of a TempEmployeePlanEnrollment
	#' @param TempEmployeePlanEnrollmentID The ID of the TempEmployeePlanEnrollment to return.
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given TempEmployeePlanEnrollment. Defaults to FALSE for all return fields which, for convenience, returns all fields for the TempEmployeePlanEnrollment.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object listed as text. Run \code{\link{getSchemaForObjects}}('TempEmployeePlanEnrollment') to get more field paths.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{listSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Benefit Management
	#' @return A dataframe or of TempEmployeePlanEnrollment
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	getTempEmployeePlanEnrollment <- function(TempEmployeePlanEnrollmentID, EmployeeID = F, PlanID = F, EmployeeFullNameLFM = F, SubPlanID = F, SubPlanIdentifier = F, DeductionBenefitDescription = F, DeductionBenefitValue = F, EnteredDeductionAmount = F, EnteredBenefitAmount = F, PremiumAmount = F, CoverageStartDate = F, CoverageEndDate = F, PaymentStartDate = F, PaymentEndDate = F, HasError = F, ErrorNumber = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, CoverageAmount = F, CalculateWithFTE = F, DeductionBenefitScheduleID = F, EmployeeNumber = F, BenefitFTEOverride = F, BenefitFTESource = F, EmployeePlanEnrollmentIDClonedFrom = F, PlanCode = F, OldCoverageStartDate = F, OldCoverageEndDate = F, OldPaymentStartDate = F, OldPaymentEndDate = F, OldFinalCoverageMonthID = F, OldFinalBenefitID = F, OldFinalBenefitValueOverride = F, OldFinalBenefitMaximumValueOverride = F, OldFinalDeductionID = F, OldFinalDeductionValueOverride = F, OldFinalDeductionMaximumValueOverride = F, EmployeePlanEnrollmentIdentifier = F, DistrictID = F, CoverageMonthCount = F, CurrentCoverageMonthBenefitAmount = F, CurrentCoverageMonthDeductionAmount = F, PlanCodeDescription = F, CalculationDateCoverage = F, CalculationDateBenefitGroup = F, CalculationDateBenefitPercentage = F, InsuranceYearCoverageMonthCount = F, InsuranceYearPayCount = F, fieldPaths = NULL, entityId = 1, schoolYearId = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment()) %>% purrr::keep(names(.) != "TempEmployeePlanEnrollmentID")

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		getSkyObject(module = "BenefitManagement", objectName = "TempEmployeePlanEnrollment", objectId = TempEmployeePlanEnrollmentID, searchFields = searchFields, entityId = entityId, flatten = flatten, returnResponse = returnResponse)
	}

	#' Delete a TempEmployeePlanEnrollment
	#'
	#' This function deletes a TempEmployeePlanEnrollment
	#' @param TempEmployeePlanEnrollmentID The ID of the TempEmployeePlanEnrollment to delete
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{listSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Benefit Management
	#' @return The TempEmployeePlanEnrollmentID of the deleted TempEmployeePlanEnrollment.
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	deleteTempEmployeePlanEnrollment <- function(TempEmployeePlanEnrollmentID, ignoreWarnings = F, entityId = 1, schoolYearId = NULL, flatten = T, returnResponse = F){

		deleteSkyObject(module = "BenefitManagement", objectName = "TempEmployeePlanEnrollment", objectId = TempEmployeePlanEnrollmentID, ignoreWarnings = ignoreWarnings, entityId = entityId, flatten = flatten, returnResponse = returnResponse)
	}

	#' Create a TempEmployeePlanEnrollment
	#'
	#' This function creates a TempEmployeePlanEnrollment
	#' @param fieldNames The field values to give the created TempEmployeePlanEnrollment. Each defaults to NULL.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{listSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Benefit Management
	#' @return A newly created TempEmployeePlanEnrollment
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	createTempEmployeePlanEnrollment <- function(EmployeeID = NULL, PlanID = NULL, EmployeeFullNameLFM = NULL, SubPlanID = NULL, SubPlanIdentifier = NULL, DeductionBenefitDescription = NULL, DeductionBenefitValue = NULL, EnteredDeductionAmount = NULL, EnteredBenefitAmount = NULL, PremiumAmount = NULL, CoverageStartDate = NULL, CoverageEndDate = NULL, PaymentStartDate = NULL, PaymentEndDate = NULL, HasError = NULL, ErrorNumber = NULL, CoverageAmount = NULL, CalculateWithFTE = NULL, DeductionBenefitScheduleID = NULL, EmployeeNumber = NULL, BenefitFTEOverride = NULL, BenefitFTESource = NULL, EmployeePlanEnrollmentIDClonedFrom = NULL, PlanCode = NULL, OldCoverageStartDate = NULL, OldCoverageEndDate = NULL, OldPaymentStartDate = NULL, OldPaymentEndDate = NULL, OldFinalCoverageMonthID = NULL, OldFinalBenefitID = NULL, OldFinalBenefitValueOverride = NULL, OldFinalBenefitMaximumValueOverride = NULL, OldFinalDeductionID = NULL, OldFinalDeductionValueOverride = NULL, OldFinalDeductionMaximumValueOverride = NULL, EmployeePlanEnrollmentIdentifier = NULL, DistrictID = NULL, CoverageMonthCount = NULL, CurrentCoverageMonthBenefitAmount = NULL, CurrentCoverageMonthDeductionAmount = NULL, PlanCodeDescription = NULL, CalculationDateCoverage = NULL, CalculationDateBenefitGroup = NULL, CalculationDateBenefitPercentage = NULL, InsuranceYearCoverageMonthCount = NULL, InsuranceYearPayCount = NULL, entityId = 1, schoolYearId = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment())

		body <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper()) %>% purrr::compact()

		createSkyObject(module = "BenefitManagement", objectName = "TempEmployeePlanEnrollment", body = list(DataObject = body), searchFields = append("TempEmployeePlanEnrollmentID", body %>% names()), entityId = entityId, schoolYearId = schoolYearId, flatten = flatten, returnResponse = returnResponse)
	}

	#' Modify a TempEmployeePlanEnrollment
	#'
	#' This function modifies a TempEmployeePlanEnrollment
	#' @param fieldNames The field values to give the modified TempEmployeePlanEnrollment. Each defaults to NULL.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{listSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Benefit Management
	#' @return The modified TempEmployeePlanEnrollment
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	modifyTempEmployeePlanEnrollment <- function(TempEmployeePlanEnrollmentID, EmployeeID = NULL, PlanID = NULL, EmployeeFullNameLFM = NULL, SubPlanID = NULL, SubPlanIdentifier = NULL, DeductionBenefitDescription = NULL, DeductionBenefitValue = NULL, EnteredDeductionAmount = NULL, EnteredBenefitAmount = NULL, PremiumAmount = NULL, CoverageStartDate = NULL, CoverageEndDate = NULL, PaymentStartDate = NULL, PaymentEndDate = NULL, HasError = NULL, ErrorNumber = NULL, CoverageAmount = NULL, CalculateWithFTE = NULL, DeductionBenefitScheduleID = NULL, EmployeeNumber = NULL, BenefitFTEOverride = NULL, BenefitFTESource = NULL, EmployeePlanEnrollmentIDClonedFrom = NULL, PlanCode = NULL, OldCoverageStartDate = NULL, OldCoverageEndDate = NULL, OldPaymentStartDate = NULL, OldPaymentEndDate = NULL, OldFinalCoverageMonthID = NULL, OldFinalBenefitID = NULL, OldFinalBenefitValueOverride = NULL, OldFinalBenefitMaximumValueOverride = NULL, OldFinalDeductionID = NULL, OldFinalDeductionValueOverride = NULL, OldFinalDeductionMaximumValueOverride = NULL, EmployeePlanEnrollmentIdentifier = NULL, DistrictID = NULL, CoverageMonthCount = NULL, CurrentCoverageMonthBenefitAmount = NULL, CurrentCoverageMonthDeductionAmount = NULL, PlanCodeDescription = NULL, CalculationDateCoverage = NULL, CalculationDateBenefitGroup = NULL, CalculationDateBenefitPercentage = NULL, InsuranceYearCoverageMonthCount = NULL, InsuranceYearPayCount = NULL, entityId = 1, schoolYearId = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment())

		body <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper()) %>% purrr::compact()

		modifySkyObject(module = "BenefitManagement", objectName = "TempEmployeePlanEnrollment", objectId = TempEmployeePlanEnrollmentID, body = list(DataObject = body), searchFields = append("TempEmployeePlanEnrollmentID", body %>% names()), entityId = entityId, schoolYearId = schoolYearId, flatten = flatten, returnResponse = returnResponse)
	}

	#' List TempSubPlans
	#'
	#' This function returns a dataframe or json object of TempSubPlans
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given TempSubPlans. Defaults to FALSE for all return fields which, for convenience, returns all fields for the TempSubPlans.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object listed as text. Run \code{\link{getSchemaForObjects}}('TempSubPlan') to get more field paths.
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
	#' @concept Benefit Management
	#' @return A list of TempSubPlans
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	listTempSubPlans <- function(searchConditionsList = NULL, TempSubPlanID = F, BenefitGroupID = F, BenefitGroupCodeDescription = F, PlanID = F, PlanCodeDescription = F, CoverageID = F, CoverageDescription = F, EnteredDeductionAmount = F, EnteredBenefitAmount = F, PremiumAmount = F, CoverageStartDate = F, CoverageEndDate = F, HasError = F, ErrorNumber = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, BenefitCostPerThousand = F, DeductionCostPerThousand = F, CalculateWithFTE = F, BenefitFTERangeID = F, SalaryMultiplier = F, SalaryCoverageAmountMaximum = F, HasAgeBasedRates = F, CoverageAmount = F, CoverageAgeRangeID = F, AgeCalculatedRateBaseCode = F, BenefitPercentage = F, DeductionPercentage = F, fieldPaths = NULL, searchConditionsGroupType = "And", searchSortFieldNamesList = NULL, searchSortFieldNamesDescendingList = NULL, entityId = 1, schoolYearId = NULL, page = 1, pageSize = 100000, flatten = T, returnResponse = F){

		params <- as.list(environment())

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		listSkyObjects(module = "BenefitManagement", objectName = "TempSubPlan", searchFields = searchFields %>% append(fieldPaths), page = page, pageSize = pageSize, SearchConditionsList = searchConditionsList, SearchConditionsGroupType = searchConditionsGroupType, SearchSortFieldNamesList = searchSortFieldNamesList, SearchSortFieldNamesDescendingList = searchSortFieldNamesDescendingList, entityId = entityId, schoolYearId = schoolYearId, flatten = flatten, returnResponse = returnResponse)
	}

	#' Get a TempSubPlan
	#'
	#' This function returns a dataframe or json object of a TempSubPlan
	#' @param TempSubPlanID The ID of the TempSubPlan to return.
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given TempSubPlan. Defaults to FALSE for all return fields which, for convenience, returns all fields for the TempSubPlan.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object listed as text. Run \code{\link{getSchemaForObjects}}('TempSubPlan') to get more field paths.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{listSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Benefit Management
	#' @return A dataframe or of TempSubPlan
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	getTempSubPlan <- function(TempSubPlanID, BenefitGroupID = F, BenefitGroupCodeDescription = F, PlanID = F, PlanCodeDescription = F, CoverageID = F, CoverageDescription = F, EnteredDeductionAmount = F, EnteredBenefitAmount = F, PremiumAmount = F, CoverageStartDate = F, CoverageEndDate = F, HasError = F, ErrorNumber = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, BenefitCostPerThousand = F, DeductionCostPerThousand = F, CalculateWithFTE = F, BenefitFTERangeID = F, SalaryMultiplier = F, SalaryCoverageAmountMaximum = F, HasAgeBasedRates = F, CoverageAmount = F, CoverageAgeRangeID = F, AgeCalculatedRateBaseCode = F, BenefitPercentage = F, DeductionPercentage = F, fieldPaths = NULL, entityId = 1, schoolYearId = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment()) %>% purrr::keep(names(.) != "TempSubPlanID")

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		getSkyObject(module = "BenefitManagement", objectName = "TempSubPlan", objectId = TempSubPlanID, searchFields = searchFields, entityId = entityId, flatten = flatten, returnResponse = returnResponse)
	}

	#' Delete a TempSubPlan
	#'
	#' This function deletes a TempSubPlan
	#' @param TempSubPlanID The ID of the TempSubPlan to delete
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{listSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Benefit Management
	#' @return The TempSubPlanID of the deleted TempSubPlan.
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	deleteTempSubPlan <- function(TempSubPlanID, ignoreWarnings = F, entityId = 1, schoolYearId = NULL, flatten = T, returnResponse = F){

		deleteSkyObject(module = "BenefitManagement", objectName = "TempSubPlan", objectId = TempSubPlanID, ignoreWarnings = ignoreWarnings, entityId = entityId, flatten = flatten, returnResponse = returnResponse)
	}

	#' Create a TempSubPlan
	#'
	#' This function creates a TempSubPlan
	#' @param fieldNames The field values to give the created TempSubPlan. Each defaults to NULL.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{listSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Benefit Management
	#' @return A newly created TempSubPlan
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	createTempSubPlan <- function(BenefitGroupID = NULL, BenefitGroupCodeDescription = NULL, PlanID = NULL, PlanCodeDescription = NULL, CoverageID = NULL, CoverageDescription = NULL, EnteredDeductionAmount = NULL, EnteredBenefitAmount = NULL, PremiumAmount = NULL, CoverageStartDate = NULL, CoverageEndDate = NULL, HasError = NULL, ErrorNumber = NULL, BenefitCostPerThousand = NULL, DeductionCostPerThousand = NULL, CalculateWithFTE = NULL, BenefitFTERangeID = NULL, SalaryMultiplier = NULL, SalaryCoverageAmountMaximum = NULL, HasAgeBasedRates = NULL, CoverageAmount = NULL, CoverageAgeRangeID = NULL, AgeCalculatedRateBaseCode = NULL, BenefitPercentage = NULL, DeductionPercentage = NULL, entityId = 1, schoolYearId = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment())

		body <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper()) %>% purrr::compact()

		createSkyObject(module = "BenefitManagement", objectName = "TempSubPlan", body = list(DataObject = body), searchFields = append("TempSubPlanID", body %>% names()), entityId = entityId, schoolYearId = schoolYearId, flatten = flatten, returnResponse = returnResponse)
	}

	#' Modify a TempSubPlan
	#'
	#' This function modifies a TempSubPlan
	#' @param fieldNames The field values to give the modified TempSubPlan. Each defaults to NULL.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{listSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Benefit Management
	#' @return The modified TempSubPlan
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	modifyTempSubPlan <- function(TempSubPlanID, BenefitGroupID = NULL, BenefitGroupCodeDescription = NULL, PlanID = NULL, PlanCodeDescription = NULL, CoverageID = NULL, CoverageDescription = NULL, EnteredDeductionAmount = NULL, EnteredBenefitAmount = NULL, PremiumAmount = NULL, CoverageStartDate = NULL, CoverageEndDate = NULL, HasError = NULL, ErrorNumber = NULL, BenefitCostPerThousand = NULL, DeductionCostPerThousand = NULL, CalculateWithFTE = NULL, BenefitFTERangeID = NULL, SalaryMultiplier = NULL, SalaryCoverageAmountMaximum = NULL, HasAgeBasedRates = NULL, CoverageAmount = NULL, CoverageAgeRangeID = NULL, AgeCalculatedRateBaseCode = NULL, BenefitPercentage = NULL, DeductionPercentage = NULL, entityId = 1, schoolYearId = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment())

		body <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper()) %>% purrr::compact()

		modifySkyObject(module = "BenefitManagement", objectName = "TempSubPlan", objectId = TempSubPlanID, body = list(DataObject = body), searchFields = append("TempSubPlanID", body %>% names()), entityId = entityId, schoolYearId = schoolYearId, flatten = flatten, returnResponse = returnResponse)
	}

	#' List BenefitManagementConfigDistricts
	#'
	#' This function returns a dataframe or json object of BenefitManagementConfigDistricts
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given BenefitManagementConfigDistricts. Defaults to FALSE for all return fields which, for convenience, returns all fields for the BenefitManagementConfigDistricts.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object listed as text. Run \code{\link{getSchemaForObjects}}('BenefitManagementConfigDistrict') to get more field paths.
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
	#' @concept Benefit Management
	#' @return A list of BenefitManagementConfigDistricts
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	listBenefitManagementConfigDistricts <- function(searchConditionsList = NULL, ConfigDistrictID = F, DistrictID = F, EmployeeDeductionBenefitCalculationType = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, fieldPaths = NULL, searchConditionsGroupType = "And", searchSortFieldNamesList = NULL, searchSortFieldNamesDescendingList = NULL, entityId = 1, schoolYearId = NULL, page = 1, pageSize = 100000, flatten = T, returnResponse = F){

		params <- as.list(environment())

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		listSkyObjects(module = "BenefitManagement", objectName = "ConfigDistrict", searchFields = searchFields %>% append(fieldPaths), page = page, pageSize = pageSize, SearchConditionsList = searchConditionsList, SearchConditionsGroupType = searchConditionsGroupType, SearchSortFieldNamesList = searchSortFieldNamesList, SearchSortFieldNamesDescendingList = searchSortFieldNamesDescendingList, entityId = entityId, schoolYearId = schoolYearId, flatten = flatten, returnResponse = returnResponse)
	}

	#' Get a BenefitManagementConfigDistrict
	#'
	#' This function returns a dataframe or json object of a BenefitManagementConfigDistrict
	#' @param BenefitManagementConfigDistrictID The ID of the BenefitManagementConfigDistrict to return.
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given BenefitManagementConfigDistrict. Defaults to FALSE for all return fields which, for convenience, returns all fields for the BenefitManagementConfigDistrict.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object listed as text. Run \code{\link{getSchemaForObjects}}('BenefitManagementConfigDistrict') to get more field paths.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{listSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Benefit Management
	#' @return A dataframe or of BenefitManagementConfigDistrict
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	getBenefitManagementConfigDistrict <- function(BenefitManagementConfigDistrictID, ConfigDistrictID = F, DistrictID = F, EmployeeDeductionBenefitCalculationType = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, fieldPaths = NULL, entityId = 1, schoolYearId = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment()) %>% purrr::keep(names(.) != "BenefitManagementConfigDistrictID")

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		getSkyObject(module = "BenefitManagement", objectName = "ConfigDistrict", objectId = BenefitManagementConfigDistrictID, searchFields = searchFields, entityId = entityId, flatten = flatten, returnResponse = returnResponse)
	}

	#' Delete a BenefitManagementConfigDistrict
	#'
	#' This function deletes a BenefitManagementConfigDistrict
	#' @param BenefitManagementConfigDistrictID The ID of the BenefitManagementConfigDistrict to delete
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{listSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Benefit Management
	#' @return The BenefitManagementConfigDistrictID of the deleted BenefitManagementConfigDistrict.
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	deleteBenefitManagementConfigDistrict <- function(BenefitManagementConfigDistrictID, ignoreWarnings = F, entityId = 1, schoolYearId = NULL, flatten = T, returnResponse = F){

		deleteSkyObject(module = "BenefitManagement", objectName = "ConfigDistrict", objectId = BenefitManagementConfigDistrictID, ignoreWarnings = ignoreWarnings, entityId = entityId, flatten = flatten, returnResponse = returnResponse)
	}

	#' Create a BenefitManagementConfigDistrict
	#'
	#' This function creates a BenefitManagementConfigDistrict
	#' @param fieldNames The field values to give the created BenefitManagementConfigDistrict. Each defaults to NULL.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{listSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Benefit Management
	#' @return A newly created BenefitManagementConfigDistrict
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	createBenefitManagementConfigDistrict <- function(DistrictID = NULL, EmployeeDeductionBenefitCalculationType = NULL, entityId = 1, schoolYearId = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment())

		body <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper()) %>% purrr::compact()

		createSkyObject(module = "BenefitManagement", objectName = "ConfigDistrict", body = list(DataObject = body), searchFields = append("ConfigDistrictID", body %>% names()), entityId = entityId, schoolYearId = schoolYearId, flatten = flatten, returnResponse = returnResponse)
	}

	#' Modify a BenefitManagementConfigDistrict
	#'
	#' This function modifies a BenefitManagementConfigDistrict
	#' @param fieldNames The field values to give the modified BenefitManagementConfigDistrict. Each defaults to NULL.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{listSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Benefit Management
	#' @return The modified BenefitManagementConfigDistrict
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	modifyBenefitManagementConfigDistrict <- function(ConfigDistrictID, DistrictID = NULL, EmployeeDeductionBenefitCalculationType = NULL, entityId = 1, schoolYearId = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment())

		body <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper()) %>% purrr::compact()

		modifySkyObject(module = "BenefitManagement", objectName = "ConfigDistrict", objectId = ConfigDistrictID, body = list(DataObject = body), searchFields = append("ConfigDistrictID", body %>% names()), entityId = entityId, schoolYearId = schoolYearId, flatten = flatten, returnResponse = returnResponse)
	}

	#' List TempBenefitManagementCodeSummaries
	#'
	#' This function returns a dataframe or json object of TempBenefitManagementCodeSummaries
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given TempBenefitManagementCodeSummaries. Defaults to FALSE for all return fields which, for convenience, returns all fields for the TempBenefitManagementCodeSummaries.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object listed as text. Run \code{\link{getSchemaForObjects}}('TempBenefitManagementCodeSummary') to get more field paths.
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
	#' @concept Benefit Management
	#' @return A list of TempBenefitManagementCodeSummaries
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	listTempBenefitManagementCodeSummaries <- function(searchConditionsList = NULL, TempBenefitManagementCodeSummaryID = F, DisplayName = F, SourceRecordsFound = F, TargetRecordsFound = F, RecordsToProcess = F, SuccessCount = F, FailCount = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, fieldPaths = NULL, searchConditionsGroupType = "And", searchSortFieldNamesList = NULL, searchSortFieldNamesDescendingList = NULL, entityId = 1, schoolYearId = NULL, page = 1, pageSize = 100000, flatten = T, returnResponse = F){

		params <- as.list(environment())

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		listSkyObjects(module = "BenefitManagement", objectName = "TempBenefitManagementCodeSummary", searchFields = searchFields %>% append(fieldPaths), page = page, pageSize = pageSize, SearchConditionsList = searchConditionsList, SearchConditionsGroupType = searchConditionsGroupType, SearchSortFieldNamesList = searchSortFieldNamesList, SearchSortFieldNamesDescendingList = searchSortFieldNamesDescendingList, entityId = entityId, schoolYearId = schoolYearId, flatten = flatten, returnResponse = returnResponse)
	}

	#' Get a TempBenefitManagementCodeSummary
	#'
	#' This function returns a dataframe or json object of a TempBenefitManagementCodeSummary
	#' @param TempBenefitManagementCodeSummaryID The ID of the TempBenefitManagementCodeSummary to return.
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given TempBenefitManagementCodeSummary. Defaults to FALSE for all return fields which, for convenience, returns all fields for the TempBenefitManagementCodeSummary.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object listed as text. Run \code{\link{getSchemaForObjects}}('TempBenefitManagementCodeSummary') to get more field paths.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{listSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Benefit Management
	#' @return A dataframe or of TempBenefitManagementCodeSummary
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	getTempBenefitManagementCodeSummary <- function(TempBenefitManagementCodeSummaryID, DisplayName = F, SourceRecordsFound = F, TargetRecordsFound = F, RecordsToProcess = F, SuccessCount = F, FailCount = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, fieldPaths = NULL, entityId = 1, schoolYearId = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment()) %>% purrr::keep(names(.) != "TempBenefitManagementCodeSummaryID")

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		getSkyObject(module = "BenefitManagement", objectName = "TempBenefitManagementCodeSummary", objectId = TempBenefitManagementCodeSummaryID, searchFields = searchFields, entityId = entityId, flatten = flatten, returnResponse = returnResponse)
	}

	#' Delete a TempBenefitManagementCodeSummary
	#'
	#' This function deletes a TempBenefitManagementCodeSummary
	#' @param TempBenefitManagementCodeSummaryID The ID of the TempBenefitManagementCodeSummary to delete
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{listSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Benefit Management
	#' @return The TempBenefitManagementCodeSummaryID of the deleted TempBenefitManagementCodeSummary.
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	deleteTempBenefitManagementCodeSummary <- function(TempBenefitManagementCodeSummaryID, ignoreWarnings = F, entityId = 1, schoolYearId = NULL, flatten = T, returnResponse = F){

		deleteSkyObject(module = "BenefitManagement", objectName = "TempBenefitManagementCodeSummary", objectId = TempBenefitManagementCodeSummaryID, ignoreWarnings = ignoreWarnings, entityId = entityId, flatten = flatten, returnResponse = returnResponse)
	}

	#' Create a TempBenefitManagementCodeSummary
	#'
	#' This function creates a TempBenefitManagementCodeSummary
	#' @param fieldNames The field values to give the created TempBenefitManagementCodeSummary. Each defaults to NULL.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{listSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Benefit Management
	#' @return A newly created TempBenefitManagementCodeSummary
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	createTempBenefitManagementCodeSummary <- function(DisplayName = NULL, SourceRecordsFound = NULL, TargetRecordsFound = NULL, RecordsToProcess = NULL, SuccessCount = NULL, FailCount = NULL, entityId = 1, schoolYearId = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment())

		body <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper()) %>% purrr::compact()

		createSkyObject(module = "BenefitManagement", objectName = "TempBenefitManagementCodeSummary", body = list(DataObject = body), searchFields = append("TempBenefitManagementCodeSummaryID", body %>% names()), entityId = entityId, schoolYearId = schoolYearId, flatten = flatten, returnResponse = returnResponse)
	}

	#' Modify a TempBenefitManagementCodeSummary
	#'
	#' This function modifies a TempBenefitManagementCodeSummary
	#' @param fieldNames The field values to give the modified TempBenefitManagementCodeSummary. Each defaults to NULL.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{listSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Benefit Management
	#' @return The modified TempBenefitManagementCodeSummary
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	modifyTempBenefitManagementCodeSummary <- function(TempBenefitManagementCodeSummaryID, DisplayName = NULL, SourceRecordsFound = NULL, TargetRecordsFound = NULL, RecordsToProcess = NULL, SuccessCount = NULL, FailCount = NULL, entityId = 1, schoolYearId = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment())

		body <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper()) %>% purrr::compact()

		modifySkyObject(module = "BenefitManagement", objectName = "TempBenefitManagementCodeSummary", objectId = TempBenefitManagementCodeSummaryID, body = list(DataObject = body), searchFields = append("TempBenefitManagementCodeSummaryID", body %>% names()), entityId = entityId, schoolYearId = schoolYearId, flatten = flatten, returnResponse = returnResponse)
	}

	#' List TempSubPlanRates
	#'
	#' This function returns a dataframe or json object of TempSubPlanRates
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given TempSubPlanRates. Defaults to FALSE for all return fields which, for convenience, returns all fields for the TempSubPlanRates.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object listed as text. Run \code{\link{getSchemaForObjects}}('TempSubPlanRate') to get more field paths.
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
	#' @concept Benefit Management
	#' @return A list of TempSubPlanRates
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	listTempSubPlanRates <- function(searchConditionsList = NULL, TempSubPlanRateID = F, SubPlanRateIDClonedFrom = F, SubPlanID = F, SubPlanIdentifier = F, CoverageStartDate = F, CoverageEndDate = F, OldCoverageStartDate = F, OldCoverageEndDate = F, DeductionAmount = F, BenefitAmount = F, PremiumAmount = F, BenefitCostPerThousand = F, DeductionCostPerThousand = F, CalculateWithFTE = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, HasError = F, ErrorNumber = F, CoverageAgeRangeID = F, AgeCalculatedRateBaseCode = F, BenefitPercentage = F, DeductionPercentage = F, fieldPaths = NULL, searchConditionsGroupType = "And", searchSortFieldNamesList = NULL, searchSortFieldNamesDescendingList = NULL, entityId = 1, schoolYearId = NULL, page = 1, pageSize = 100000, flatten = T, returnResponse = F){

		params <- as.list(environment())

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		listSkyObjects(module = "BenefitManagement", objectName = "TempSubPlanRate", searchFields = searchFields %>% append(fieldPaths), page = page, pageSize = pageSize, SearchConditionsList = searchConditionsList, SearchConditionsGroupType = searchConditionsGroupType, SearchSortFieldNamesList = searchSortFieldNamesList, SearchSortFieldNamesDescendingList = searchSortFieldNamesDescendingList, entityId = entityId, schoolYearId = schoolYearId, flatten = flatten, returnResponse = returnResponse)
	}

	#' Get a TempSubPlanRate
	#'
	#' This function returns a dataframe or json object of a TempSubPlanRate
	#' @param TempSubPlanRateID The ID of the TempSubPlanRate to return.
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given TempSubPlanRate. Defaults to FALSE for all return fields which, for convenience, returns all fields for the TempSubPlanRate.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object listed as text. Run \code{\link{getSchemaForObjects}}('TempSubPlanRate') to get more field paths.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{listSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Benefit Management
	#' @return A dataframe or of TempSubPlanRate
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	getTempSubPlanRate <- function(TempSubPlanRateID, SubPlanRateIDClonedFrom = F, SubPlanID = F, SubPlanIdentifier = F, CoverageStartDate = F, CoverageEndDate = F, OldCoverageStartDate = F, OldCoverageEndDate = F, DeductionAmount = F, BenefitAmount = F, PremiumAmount = F, BenefitCostPerThousand = F, DeductionCostPerThousand = F, CalculateWithFTE = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, HasError = F, ErrorNumber = F, CoverageAgeRangeID = F, AgeCalculatedRateBaseCode = F, BenefitPercentage = F, DeductionPercentage = F, fieldPaths = NULL, entityId = 1, schoolYearId = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment()) %>% purrr::keep(names(.) != "TempSubPlanRateID")

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		getSkyObject(module = "BenefitManagement", objectName = "TempSubPlanRate", objectId = TempSubPlanRateID, searchFields = searchFields, entityId = entityId, flatten = flatten, returnResponse = returnResponse)
	}

	#' Delete a TempSubPlanRate
	#'
	#' This function deletes a TempSubPlanRate
	#' @param TempSubPlanRateID The ID of the TempSubPlanRate to delete
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{listSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Benefit Management
	#' @return The TempSubPlanRateID of the deleted TempSubPlanRate.
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	deleteTempSubPlanRate <- function(TempSubPlanRateID, ignoreWarnings = F, entityId = 1, schoolYearId = NULL, flatten = T, returnResponse = F){

		deleteSkyObject(module = "BenefitManagement", objectName = "TempSubPlanRate", objectId = TempSubPlanRateID, ignoreWarnings = ignoreWarnings, entityId = entityId, flatten = flatten, returnResponse = returnResponse)
	}

	#' Create a TempSubPlanRate
	#'
	#' This function creates a TempSubPlanRate
	#' @param fieldNames The field values to give the created TempSubPlanRate. Each defaults to NULL.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{listSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Benefit Management
	#' @return A newly created TempSubPlanRate
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	createTempSubPlanRate <- function(SubPlanRateIDClonedFrom = NULL, SubPlanID = NULL, SubPlanIdentifier = NULL, CoverageStartDate = NULL, CoverageEndDate = NULL, OldCoverageStartDate = NULL, OldCoverageEndDate = NULL, DeductionAmount = NULL, BenefitAmount = NULL, PremiumAmount = NULL, BenefitCostPerThousand = NULL, DeductionCostPerThousand = NULL, CalculateWithFTE = NULL, HasError = NULL, ErrorNumber = NULL, CoverageAgeRangeID = NULL, AgeCalculatedRateBaseCode = NULL, BenefitPercentage = NULL, DeductionPercentage = NULL, entityId = 1, schoolYearId = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment())

		body <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper()) %>% purrr::compact()

		createSkyObject(module = "BenefitManagement", objectName = "TempSubPlanRate", body = list(DataObject = body), searchFields = append("TempSubPlanRateID", body %>% names()), entityId = entityId, schoolYearId = schoolYearId, flatten = flatten, returnResponse = returnResponse)
	}

	#' Modify a TempSubPlanRate
	#'
	#' This function modifies a TempSubPlanRate
	#' @param fieldNames The field values to give the modified TempSubPlanRate. Each defaults to NULL.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{listSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Benefit Management
	#' @return The modified TempSubPlanRate
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	modifyTempSubPlanRate <- function(TempSubPlanRateID, SubPlanRateIDClonedFrom = NULL, SubPlanID = NULL, SubPlanIdentifier = NULL, CoverageStartDate = NULL, CoverageEndDate = NULL, OldCoverageStartDate = NULL, OldCoverageEndDate = NULL, DeductionAmount = NULL, BenefitAmount = NULL, PremiumAmount = NULL, BenefitCostPerThousand = NULL, DeductionCostPerThousand = NULL, CalculateWithFTE = NULL, HasError = NULL, ErrorNumber = NULL, CoverageAgeRangeID = NULL, AgeCalculatedRateBaseCode = NULL, BenefitPercentage = NULL, DeductionPercentage = NULL, entityId = 1, schoolYearId = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment())

		body <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper()) %>% purrr::compact()

		modifySkyObject(module = "BenefitManagement", objectName = "TempSubPlanRate", objectId = TempSubPlanRateID, body = list(DataObject = body), searchFields = append("TempSubPlanRateID", body %>% names()), entityId = entityId, schoolYearId = schoolYearId, flatten = flatten, returnResponse = returnResponse)
	}

	#' List TempBenefitManagementCodeErrors
	#'
	#' This function returns a dataframe or json object of TempBenefitManagementCodeErrors
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given TempBenefitManagementCodeErrors. Defaults to FALSE for all return fields which, for convenience, returns all fields for the TempBenefitManagementCodeErrors.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object listed as text. Run \code{\link{getSchemaForObjects}}('TempBenefitManagementCodeError') to get more field paths.
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
	#' @concept Benefit Management
	#' @return A list of TempBenefitManagementCodeErrors
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	listTempBenefitManagementCodeErrors <- function(searchConditionsList = NULL, TempBenefitManagementCodeErrorID = F, TempBenefitManagementCodeSummaryID = F, CodeIdentifier = F, ErrorDetail = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, fieldPaths = NULL, searchConditionsGroupType = "And", searchSortFieldNamesList = NULL, searchSortFieldNamesDescendingList = NULL, entityId = 1, schoolYearId = NULL, page = 1, pageSize = 100000, flatten = T, returnResponse = F){

		params <- as.list(environment())

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		listSkyObjects(module = "BenefitManagement", objectName = "TempBenefitManagementCodeError", searchFields = searchFields %>% append(fieldPaths), page = page, pageSize = pageSize, SearchConditionsList = searchConditionsList, SearchConditionsGroupType = searchConditionsGroupType, SearchSortFieldNamesList = searchSortFieldNamesList, SearchSortFieldNamesDescendingList = searchSortFieldNamesDescendingList, entityId = entityId, schoolYearId = schoolYearId, flatten = flatten, returnResponse = returnResponse)
	}

	#' Get a TempBenefitManagementCodeError
	#'
	#' This function returns a dataframe or json object of a TempBenefitManagementCodeError
	#' @param TempBenefitManagementCodeErrorID The ID of the TempBenefitManagementCodeError to return.
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given TempBenefitManagementCodeError. Defaults to FALSE for all return fields which, for convenience, returns all fields for the TempBenefitManagementCodeError.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object listed as text. Run \code{\link{getSchemaForObjects}}('TempBenefitManagementCodeError') to get more field paths.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{listSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Benefit Management
	#' @return A dataframe or of TempBenefitManagementCodeError
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	getTempBenefitManagementCodeError <- function(TempBenefitManagementCodeErrorID, TempBenefitManagementCodeSummaryID = F, CodeIdentifier = F, ErrorDetail = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, fieldPaths = NULL, entityId = 1, schoolYearId = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment()) %>% purrr::keep(names(.) != "TempBenefitManagementCodeErrorID")

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		getSkyObject(module = "BenefitManagement", objectName = "TempBenefitManagementCodeError", objectId = TempBenefitManagementCodeErrorID, searchFields = searchFields, entityId = entityId, flatten = flatten, returnResponse = returnResponse)
	}

	#' Delete a TempBenefitManagementCodeError
	#'
	#' This function deletes a TempBenefitManagementCodeError
	#' @param TempBenefitManagementCodeErrorID The ID of the TempBenefitManagementCodeError to delete
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{listSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Benefit Management
	#' @return The TempBenefitManagementCodeErrorID of the deleted TempBenefitManagementCodeError.
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	deleteTempBenefitManagementCodeError <- function(TempBenefitManagementCodeErrorID, ignoreWarnings = F, entityId = 1, schoolYearId = NULL, flatten = T, returnResponse = F){

		deleteSkyObject(module = "BenefitManagement", objectName = "TempBenefitManagementCodeError", objectId = TempBenefitManagementCodeErrorID, ignoreWarnings = ignoreWarnings, entityId = entityId, flatten = flatten, returnResponse = returnResponse)
	}

	#' Create a TempBenefitManagementCodeError
	#'
	#' This function creates a TempBenefitManagementCodeError
	#' @param fieldNames The field values to give the created TempBenefitManagementCodeError. Each defaults to NULL.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{listSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Benefit Management
	#' @return A newly created TempBenefitManagementCodeError
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	createTempBenefitManagementCodeError <- function(TempBenefitManagementCodeSummaryID = NULL, CodeIdentifier = NULL, ErrorDetail = NULL, entityId = 1, schoolYearId = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment())

		body <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper()) %>% purrr::compact()

		createSkyObject(module = "BenefitManagement", objectName = "TempBenefitManagementCodeError", body = list(DataObject = body), searchFields = append("TempBenefitManagementCodeErrorID", body %>% names()), entityId = entityId, schoolYearId = schoolYearId, flatten = flatten, returnResponse = returnResponse)
	}

	#' Modify a TempBenefitManagementCodeError
	#'
	#' This function modifies a TempBenefitManagementCodeError
	#' @param fieldNames The field values to give the modified TempBenefitManagementCodeError. Each defaults to NULL.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{listSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Benefit Management
	#' @return The modified TempBenefitManagementCodeError
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	modifyTempBenefitManagementCodeError <- function(TempBenefitManagementCodeErrorID, TempBenefitManagementCodeSummaryID = NULL, CodeIdentifier = NULL, ErrorDetail = NULL, entityId = 1, schoolYearId = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment())

		body <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper()) %>% purrr::compact()

		modifySkyObject(module = "BenefitManagement", objectName = "TempBenefitManagementCodeError", objectId = TempBenefitManagementCodeErrorID, body = list(DataObject = body), searchFields = append("TempBenefitManagementCodeErrorID", body %>% names()), entityId = entityId, schoolYearId = schoolYearId, flatten = flatten, returnResponse = returnResponse)
	}

	#' List BenefitFTERanges
	#'
	#' This function returns a dataframe or json object of BenefitFTERanges
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given BenefitFTERanges. Defaults to FALSE for all return fields which, for convenience, returns all fields for the BenefitFTERanges.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object listed as text. Run \code{\link{getSchemaForObjects}}('BenefitFTERange') to get more field paths.
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
	#' @concept Benefit Management
	#' @return A list of BenefitFTERanges
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	listBenefitFTERanges <- function(searchConditionsList = NULL, BenefitFTERangeID = F, DistrictID = F, Code = F, Description = F, CodeDescription = F, PercentageToUse = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, fieldPaths = NULL, searchConditionsGroupType = "And", searchSortFieldNamesList = NULL, searchSortFieldNamesDescendingList = NULL, entityId = 1, schoolYearId = NULL, page = 1, pageSize = 100000, flatten = T, returnResponse = F){

		params <- as.list(environment())

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		listSkyObjects(module = "BenefitManagement", objectName = "BenefitFTERange", searchFields = searchFields %>% append(fieldPaths), page = page, pageSize = pageSize, SearchConditionsList = searchConditionsList, SearchConditionsGroupType = searchConditionsGroupType, SearchSortFieldNamesList = searchSortFieldNamesList, SearchSortFieldNamesDescendingList = searchSortFieldNamesDescendingList, entityId = entityId, schoolYearId = schoolYearId, flatten = flatten, returnResponse = returnResponse)
	}

	#' Get a BenefitFTERange
	#'
	#' This function returns a dataframe or json object of a BenefitFTERange
	#' @param BenefitFTERangeID The ID of the BenefitFTERange to return.
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given BenefitFTERange. Defaults to FALSE for all return fields which, for convenience, returns all fields for the BenefitFTERange.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object listed as text. Run \code{\link{getSchemaForObjects}}('BenefitFTERange') to get more field paths.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{listSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Benefit Management
	#' @return A dataframe or of BenefitFTERange
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	getBenefitFTERange <- function(BenefitFTERangeID, DistrictID = F, Code = F, Description = F, CodeDescription = F, PercentageToUse = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, fieldPaths = NULL, entityId = 1, schoolYearId = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment()) %>% purrr::keep(names(.) != "BenefitFTERangeID")

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		getSkyObject(module = "BenefitManagement", objectName = "BenefitFTERange", objectId = BenefitFTERangeID, searchFields = searchFields, entityId = entityId, flatten = flatten, returnResponse = returnResponse)
	}

	#' Delete a BenefitFTERange
	#'
	#' This function deletes a BenefitFTERange
	#' @param BenefitFTERangeID The ID of the BenefitFTERange to delete
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{listSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Benefit Management
	#' @return The BenefitFTERangeID of the deleted BenefitFTERange.
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	deleteBenefitFTERange <- function(BenefitFTERangeID, ignoreWarnings = F, entityId = 1, schoolYearId = NULL, flatten = T, returnResponse = F){

		deleteSkyObject(module = "BenefitManagement", objectName = "BenefitFTERange", objectId = BenefitFTERangeID, ignoreWarnings = ignoreWarnings, entityId = entityId, flatten = flatten, returnResponse = returnResponse)
	}

	#' Create a BenefitFTERange
	#'
	#' This function creates a BenefitFTERange
	#' @param fieldNames The field values to give the created BenefitFTERange. Each defaults to NULL.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{listSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Benefit Management
	#' @return A newly created BenefitFTERange
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	createBenefitFTERange <- function(DistrictID = NULL, Code = NULL, Description = NULL, entityId = 1, schoolYearId = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment())

		body <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper()) %>% purrr::compact()

		createSkyObject(module = "BenefitManagement", objectName = "BenefitFTERange", body = list(DataObject = body), searchFields = append("BenefitFTERangeID", body %>% names()), entityId = entityId, schoolYearId = schoolYearId, flatten = flatten, returnResponse = returnResponse)
	}

	#' Modify a BenefitFTERange
	#'
	#' This function modifies a BenefitFTERange
	#' @param fieldNames The field values to give the modified BenefitFTERange. Each defaults to NULL.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{listSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Benefit Management
	#' @return The modified BenefitFTERange
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	modifyBenefitFTERange <- function(BenefitFTERangeID, DistrictID = NULL, Code = NULL, Description = NULL, entityId = 1, schoolYearId = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment())

		body <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper()) %>% purrr::compact()

		modifySkyObject(module = "BenefitManagement", objectName = "BenefitFTERange", objectId = BenefitFTERangeID, body = list(DataObject = body), searchFields = append("BenefitFTERangeID", body %>% names()), entityId = entityId, schoolYearId = schoolYearId, flatten = flatten, returnResponse = returnResponse)
	}

	#' List BenefitFTERangeDetails
	#'
	#' This function returns a dataframe or json object of BenefitFTERangeDetails
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given BenefitFTERangeDetails. Defaults to FALSE for all return fields which, for convenience, returns all fields for the BenefitFTERangeDetails.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object listed as text. Run \code{\link{getSchemaForObjects}}('BenefitFTERangeDetail') to get more field paths.
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
	#' @concept Benefit Management
	#' @return A list of BenefitFTERangeDetails
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	listBenefitFTERangeDetails <- function(searchConditionsList = NULL, BenefitFTERangeDetailID = F, BenefitFTERangeID = F, FTEThreshold = F, BenefitPercentage = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, fieldPaths = NULL, searchConditionsGroupType = "And", searchSortFieldNamesList = NULL, searchSortFieldNamesDescendingList = NULL, entityId = 1, schoolYearId = NULL, page = 1, pageSize = 100000, flatten = T, returnResponse = F){

		params <- as.list(environment())

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		listSkyObjects(module = "BenefitManagement", objectName = "BenefitFTERangeDetail", searchFields = searchFields %>% append(fieldPaths), page = page, pageSize = pageSize, SearchConditionsList = searchConditionsList, SearchConditionsGroupType = searchConditionsGroupType, SearchSortFieldNamesList = searchSortFieldNamesList, SearchSortFieldNamesDescendingList = searchSortFieldNamesDescendingList, entityId = entityId, schoolYearId = schoolYearId, flatten = flatten, returnResponse = returnResponse)
	}

	#' Get a BenefitFTERangeDetail
	#'
	#' This function returns a dataframe or json object of a BenefitFTERangeDetail
	#' @param BenefitFTERangeDetailID The ID of the BenefitFTERangeDetail to return.
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given BenefitFTERangeDetail. Defaults to FALSE for all return fields which, for convenience, returns all fields for the BenefitFTERangeDetail.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object listed as text. Run \code{\link{getSchemaForObjects}}('BenefitFTERangeDetail') to get more field paths.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{listSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Benefit Management
	#' @return A dataframe or of BenefitFTERangeDetail
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	getBenefitFTERangeDetail <- function(BenefitFTERangeDetailID, BenefitFTERangeID = F, FTEThreshold = F, BenefitPercentage = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, fieldPaths = NULL, entityId = 1, schoolYearId = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment()) %>% purrr::keep(names(.) != "BenefitFTERangeDetailID")

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		getSkyObject(module = "BenefitManagement", objectName = "BenefitFTERangeDetail", objectId = BenefitFTERangeDetailID, searchFields = searchFields, entityId = entityId, flatten = flatten, returnResponse = returnResponse)
	}

	#' Delete a BenefitFTERangeDetail
	#'
	#' This function deletes a BenefitFTERangeDetail
	#' @param BenefitFTERangeDetailID The ID of the BenefitFTERangeDetail to delete
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{listSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Benefit Management
	#' @return The BenefitFTERangeDetailID of the deleted BenefitFTERangeDetail.
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	deleteBenefitFTERangeDetail <- function(BenefitFTERangeDetailID, ignoreWarnings = F, entityId = 1, schoolYearId = NULL, flatten = T, returnResponse = F){

		deleteSkyObject(module = "BenefitManagement", objectName = "BenefitFTERangeDetail", objectId = BenefitFTERangeDetailID, ignoreWarnings = ignoreWarnings, entityId = entityId, flatten = flatten, returnResponse = returnResponse)
	}

	#' Create a BenefitFTERangeDetail
	#'
	#' This function creates a BenefitFTERangeDetail
	#' @param fieldNames The field values to give the created BenefitFTERangeDetail. Each defaults to NULL.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{listSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Benefit Management
	#' @return A newly created BenefitFTERangeDetail
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	createBenefitFTERangeDetail <- function(BenefitFTERangeID = NULL, FTEThreshold = NULL, BenefitPercentage = NULL, entityId = 1, schoolYearId = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment())

		body <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper()) %>% purrr::compact()

		createSkyObject(module = "BenefitManagement", objectName = "BenefitFTERangeDetail", body = list(DataObject = body), searchFields = append("BenefitFTERangeDetailID", body %>% names()), entityId = entityId, schoolYearId = schoolYearId, flatten = flatten, returnResponse = returnResponse)
	}

	#' Modify a BenefitFTERangeDetail
	#'
	#' This function modifies a BenefitFTERangeDetail
	#' @param fieldNames The field values to give the modified BenefitFTERangeDetail. Each defaults to NULL.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{listSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Benefit Management
	#' @return The modified BenefitFTERangeDetail
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	modifyBenefitFTERangeDetail <- function(BenefitFTERangeDetailID, BenefitFTERangeID = NULL, FTEThreshold = NULL, BenefitPercentage = NULL, entityId = 1, schoolYearId = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment())

		body <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper()) %>% purrr::compact()

		modifySkyObject(module = "BenefitManagement", objectName = "BenefitFTERangeDetail", objectId = BenefitFTERangeDetailID, body = list(DataObject = body), searchFields = append("BenefitFTERangeDetailID", body %>% names()), entityId = entityId, schoolYearId = schoolYearId, flatten = flatten, returnResponse = returnResponse)
	}

	#' List TempBillDetails
	#'
	#' This function returns a dataframe or json object of TempBillDetails
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given TempBillDetails. Defaults to FALSE for all return fields which, for convenience, returns all fields for the TempBillDetails.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object listed as text. Run \code{\link{getSchemaForObjects}}('TempBillDetail') to get more field paths.
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
	#' @concept Benefit Management
	#' @return A list of TempBillDetails
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	listTempBillDetails <- function(searchConditionsList = NULL, TempBillDetailID = F, EmployeeID = F, EmployeeFullNameLFM = F, EmployeeNumber = F, BaseCurrencyAmount = F, ErrorCount = F, HasError = F, IsSelected = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, EmployeePlanEnrollmentID = F, fieldPaths = NULL, searchConditionsGroupType = "And", searchSortFieldNamesList = NULL, searchSortFieldNamesDescendingList = NULL, entityId = 1, schoolYearId = NULL, page = 1, pageSize = 100000, flatten = T, returnResponse = F){

		params <- as.list(environment())

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		listSkyObjects(module = "BenefitManagement", objectName = "TempBillDetail", searchFields = searchFields %>% append(fieldPaths), page = page, pageSize = pageSize, SearchConditionsList = searchConditionsList, SearchConditionsGroupType = searchConditionsGroupType, SearchSortFieldNamesList = searchSortFieldNamesList, SearchSortFieldNamesDescendingList = searchSortFieldNamesDescendingList, entityId = entityId, schoolYearId = schoolYearId, flatten = flatten, returnResponse = returnResponse)
	}

	#' Get a TempBillDetail
	#'
	#' This function returns a dataframe or json object of a TempBillDetail
	#' @param TempBillDetailID The ID of the TempBillDetail to return.
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given TempBillDetail. Defaults to FALSE for all return fields which, for convenience, returns all fields for the TempBillDetail.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object listed as text. Run \code{\link{getSchemaForObjects}}('TempBillDetail') to get more field paths.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{listSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Benefit Management
	#' @return A dataframe or of TempBillDetail
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	getTempBillDetail <- function(TempBillDetailID, EmployeeID = F, EmployeeFullNameLFM = F, EmployeeNumber = F, BaseCurrencyAmount = F, ErrorCount = F, HasError = F, IsSelected = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, EmployeePlanEnrollmentID = F, fieldPaths = NULL, entityId = 1, schoolYearId = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment()) %>% purrr::keep(names(.) != "TempBillDetailID")

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		getSkyObject(module = "BenefitManagement", objectName = "TempBillDetail", objectId = TempBillDetailID, searchFields = searchFields, entityId = entityId, flatten = flatten, returnResponse = returnResponse)
	}

	#' Delete a TempBillDetail
	#'
	#' This function deletes a TempBillDetail
	#' @param TempBillDetailID The ID of the TempBillDetail to delete
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{listSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Benefit Management
	#' @return The TempBillDetailID of the deleted TempBillDetail.
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	deleteTempBillDetail <- function(TempBillDetailID, ignoreWarnings = F, entityId = 1, schoolYearId = NULL, flatten = T, returnResponse = F){

		deleteSkyObject(module = "BenefitManagement", objectName = "TempBillDetail", objectId = TempBillDetailID, ignoreWarnings = ignoreWarnings, entityId = entityId, flatten = flatten, returnResponse = returnResponse)
	}

	#' Create a TempBillDetail
	#'
	#' This function creates a TempBillDetail
	#' @param fieldNames The field values to give the created TempBillDetail. Each defaults to NULL.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{listSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Benefit Management
	#' @return A newly created TempBillDetail
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	createTempBillDetail <- function(EmployeeID = NULL, EmployeeFullNameLFM = NULL, EmployeeNumber = NULL, BaseCurrencyAmount = NULL, ErrorCount = NULL, HasError = NULL, IsSelected = NULL, EmployeePlanEnrollmentID = NULL, entityId = 1, schoolYearId = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment())

		body <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper()) %>% purrr::compact()

		createSkyObject(module = "BenefitManagement", objectName = "TempBillDetail", body = list(DataObject = body), searchFields = append("TempBillDetailID", body %>% names()), entityId = entityId, schoolYearId = schoolYearId, flatten = flatten, returnResponse = returnResponse)
	}

	#' Modify a TempBillDetail
	#'
	#' This function modifies a TempBillDetail
	#' @param fieldNames The field values to give the modified TempBillDetail. Each defaults to NULL.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{listSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Benefit Management
	#' @return The modified TempBillDetail
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	modifyTempBillDetail <- function(TempBillDetailID, EmployeeID = NULL, EmployeeFullNameLFM = NULL, EmployeeNumber = NULL, BaseCurrencyAmount = NULL, ErrorCount = NULL, HasError = NULL, IsSelected = NULL, EmployeePlanEnrollmentID = NULL, entityId = 1, schoolYearId = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment())

		body <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper()) %>% purrr::compact()

		modifySkyObject(module = "BenefitManagement", objectName = "TempBillDetail", objectId = TempBillDetailID, body = list(DataObject = body), searchFields = append("TempBillDetailID", body %>% names()), entityId = entityId, schoolYearId = schoolYearId, flatten = flatten, returnResponse = returnResponse)
	}

	#' List EmployeePlanEnrollmentBillDetails
	#'
	#' This function returns a dataframe or json object of EmployeePlanEnrollmentBillDetails
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given EmployeePlanEnrollmentBillDetails. Defaults to FALSE for all return fields which, for convenience, returns all fields for the EmployeePlanEnrollmentBillDetails.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object listed as text. Run \code{\link{getSchemaForObjects}}('EmployeePlanEnrollmentBillDetail') to get more field paths.
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
	#' @concept Benefit Management
	#' @return A list of EmployeePlanEnrollmentBillDetails
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	listEmployeePlanEnrollmentBillDetails <- function(searchConditionsList = NULL, EmployeePlanEnrollmentBillDetailID = F, EmployeePlanEnrollmentID = F, BillDetailID = F, BaseCurrencyAmount = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, fieldPaths = NULL, searchConditionsGroupType = "And", searchSortFieldNamesList = NULL, searchSortFieldNamesDescendingList = NULL, entityId = 1, schoolYearId = NULL, page = 1, pageSize = 100000, flatten = T, returnResponse = F){

		params <- as.list(environment())

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		listSkyObjects(module = "BenefitManagement", objectName = "EmployeePlanEnrollmentBillDetail", searchFields = searchFields %>% append(fieldPaths), page = page, pageSize = pageSize, SearchConditionsList = searchConditionsList, SearchConditionsGroupType = searchConditionsGroupType, SearchSortFieldNamesList = searchSortFieldNamesList, SearchSortFieldNamesDescendingList = searchSortFieldNamesDescendingList, entityId = entityId, schoolYearId = schoolYearId, flatten = flatten, returnResponse = returnResponse)
	}

	#' Get an EmployeePlanEnrollmentBillDetail
	#'
	#' This function returns a dataframe or json object of an EmployeePlanEnrollmentBillDetail
	#' @param EmployeePlanEnrollmentBillDetailID The ID of the EmployeePlanEnrollmentBillDetail to return.
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given EmployeePlanEnrollmentBillDetail. Defaults to FALSE for all return fields which, for convenience, returns all fields for the EmployeePlanEnrollmentBillDetail.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object listed as text. Run \code{\link{getSchemaForObjects}}('EmployeePlanEnrollmentBillDetail') to get more field paths.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{listSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Benefit Management
	#' @return A dataframe or of EmployeePlanEnrollmentBillDetail
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	getEmployeePlanEnrollmentBillDetail <- function(EmployeePlanEnrollmentBillDetailID, EmployeePlanEnrollmentID = F, BillDetailID = F, BaseCurrencyAmount = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, fieldPaths = NULL, entityId = 1, schoolYearId = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment()) %>% purrr::keep(names(.) != "EmployeePlanEnrollmentBillDetailID")

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		getSkyObject(module = "BenefitManagement", objectName = "EmployeePlanEnrollmentBillDetail", objectId = EmployeePlanEnrollmentBillDetailID, searchFields = searchFields, entityId = entityId, flatten = flatten, returnResponse = returnResponse)
	}

	#' Delete an EmployeePlanEnrollmentBillDetail
	#'
	#' This function deletes an EmployeePlanEnrollmentBillDetail
	#' @param EmployeePlanEnrollmentBillDetailID The ID of the EmployeePlanEnrollmentBillDetail to delete
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{listSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Benefit Management
	#' @return The EmployeePlanEnrollmentBillDetailID of the deleted EmployeePlanEnrollmentBillDetail.
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	deleteEmployeePlanEnrollmentBillDetail <- function(EmployeePlanEnrollmentBillDetailID, ignoreWarnings = F, entityId = 1, schoolYearId = NULL, flatten = T, returnResponse = F){

		deleteSkyObject(module = "BenefitManagement", objectName = "EmployeePlanEnrollmentBillDetail", objectId = EmployeePlanEnrollmentBillDetailID, ignoreWarnings = ignoreWarnings, entityId = entityId, flatten = flatten, returnResponse = returnResponse)
	}

	#' Create an EmployeePlanEnrollmentBillDetail
	#'
	#' This function creates an EmployeePlanEnrollmentBillDetail
	#' @param fieldNames The field values to give the created EmployeePlanEnrollmentBillDetail. Each defaults to NULL.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{listSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Benefit Management
	#' @return A newly created EmployeePlanEnrollmentBillDetail
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	createEmployeePlanEnrollmentBillDetail <- function(EmployeePlanEnrollmentID = NULL, BillDetailID = NULL, BaseCurrencyAmount = NULL, entityId = 1, schoolYearId = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment())

		body <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper()) %>% purrr::compact()

		createSkyObject(module = "BenefitManagement", objectName = "EmployeePlanEnrollmentBillDetail", body = list(DataObject = body), searchFields = append("EmployeePlanEnrollmentBillDetailID", body %>% names()), entityId = entityId, schoolYearId = schoolYearId, flatten = flatten, returnResponse = returnResponse)
	}

	#' Modify an EmployeePlanEnrollmentBillDetail
	#'
	#' This function modifies an EmployeePlanEnrollmentBillDetail
	#' @param fieldNames The field values to give the modified EmployeePlanEnrollmentBillDetail. Each defaults to NULL.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{listSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Benefit Management
	#' @return The modified EmployeePlanEnrollmentBillDetail
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	modifyEmployeePlanEnrollmentBillDetail <- function(EmployeePlanEnrollmentBillDetailID, EmployeePlanEnrollmentID = NULL, BillDetailID = NULL, BaseCurrencyAmount = NULL, entityId = 1, schoolYearId = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment())

		body <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper()) %>% purrr::compact()

		modifySkyObject(module = "BenefitManagement", objectName = "EmployeePlanEnrollmentBillDetail", objectId = EmployeePlanEnrollmentBillDetailID, body = list(DataObject = body), searchFields = append("EmployeePlanEnrollmentBillDetailID", body %>% names()), entityId = entityId, schoolYearId = schoolYearId, flatten = flatten, returnResponse = returnResponse)
	}

	#' List CoverageAgeRanges
	#'
	#' This function returns a dataframe or json object of CoverageAgeRanges
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given CoverageAgeRanges. Defaults to FALSE for all return fields which, for convenience, returns all fields for the CoverageAgeRanges.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object listed as text. Run \code{\link{getSchemaForObjects}}('CoverageAgeRange') to get more field paths.
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
	#' @concept Benefit Management
	#' @return A list of CoverageAgeRanges
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	listCoverageAgeRanges <- function(searchConditionsList = NULL, CoverageAgeRangeID = F, DistrictID = F, Code = F, Description = F, CodeDescription = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, fieldPaths = NULL, searchConditionsGroupType = "And", searchSortFieldNamesList = NULL, searchSortFieldNamesDescendingList = NULL, entityId = 1, schoolYearId = NULL, page = 1, pageSize = 100000, flatten = T, returnResponse = F){

		params <- as.list(environment())

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		listSkyObjects(module = "BenefitManagement", objectName = "CoverageAgeRange", searchFields = searchFields %>% append(fieldPaths), page = page, pageSize = pageSize, SearchConditionsList = searchConditionsList, SearchConditionsGroupType = searchConditionsGroupType, SearchSortFieldNamesList = searchSortFieldNamesList, SearchSortFieldNamesDescendingList = searchSortFieldNamesDescendingList, entityId = entityId, schoolYearId = schoolYearId, flatten = flatten, returnResponse = returnResponse)
	}

	#' Get a CoverageAgeRange
	#'
	#' This function returns a dataframe or json object of a CoverageAgeRange
	#' @param CoverageAgeRangeID The ID of the CoverageAgeRange to return.
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given CoverageAgeRange. Defaults to FALSE for all return fields which, for convenience, returns all fields for the CoverageAgeRange.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object listed as text. Run \code{\link{getSchemaForObjects}}('CoverageAgeRange') to get more field paths.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{listSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Benefit Management
	#' @return A dataframe or of CoverageAgeRange
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	getCoverageAgeRange <- function(CoverageAgeRangeID, DistrictID = F, Code = F, Description = F, CodeDescription = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, fieldPaths = NULL, entityId = 1, schoolYearId = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment()) %>% purrr::keep(names(.) != "CoverageAgeRangeID")

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		getSkyObject(module = "BenefitManagement", objectName = "CoverageAgeRange", objectId = CoverageAgeRangeID, searchFields = searchFields, entityId = entityId, flatten = flatten, returnResponse = returnResponse)
	}

	#' Delete a CoverageAgeRange
	#'
	#' This function deletes a CoverageAgeRange
	#' @param CoverageAgeRangeID The ID of the CoverageAgeRange to delete
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{listSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Benefit Management
	#' @return The CoverageAgeRangeID of the deleted CoverageAgeRange.
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	deleteCoverageAgeRange <- function(CoverageAgeRangeID, ignoreWarnings = F, entityId = 1, schoolYearId = NULL, flatten = T, returnResponse = F){

		deleteSkyObject(module = "BenefitManagement", objectName = "CoverageAgeRange", objectId = CoverageAgeRangeID, ignoreWarnings = ignoreWarnings, entityId = entityId, flatten = flatten, returnResponse = returnResponse)
	}

	#' Create a CoverageAgeRange
	#'
	#' This function creates a CoverageAgeRange
	#' @param fieldNames The field values to give the created CoverageAgeRange. Each defaults to NULL.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{listSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Benefit Management
	#' @return A newly created CoverageAgeRange
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	createCoverageAgeRange <- function(DistrictID = NULL, Code = NULL, Description = NULL, entityId = 1, schoolYearId = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment())

		body <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper()) %>% purrr::compact()

		createSkyObject(module = "BenefitManagement", objectName = "CoverageAgeRange", body = list(DataObject = body), searchFields = append("CoverageAgeRangeID", body %>% names()), entityId = entityId, schoolYearId = schoolYearId, flatten = flatten, returnResponse = returnResponse)
	}

	#' Modify a CoverageAgeRange
	#'
	#' This function modifies a CoverageAgeRange
	#' @param fieldNames The field values to give the modified CoverageAgeRange. Each defaults to NULL.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{listSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Benefit Management
	#' @return The modified CoverageAgeRange
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	modifyCoverageAgeRange <- function(CoverageAgeRangeID, DistrictID = NULL, Code = NULL, Description = NULL, entityId = 1, schoolYearId = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment())

		body <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper()) %>% purrr::compact()

		modifySkyObject(module = "BenefitManagement", objectName = "CoverageAgeRange", objectId = CoverageAgeRangeID, body = list(DataObject = body), searchFields = append("CoverageAgeRangeID", body %>% names()), entityId = entityId, schoolYearId = schoolYearId, flatten = flatten, returnResponse = returnResponse)
	}

	#' List CoverageAgeRangeDetails
	#'
	#' This function returns a dataframe or json object of CoverageAgeRangeDetails
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given CoverageAgeRangeDetails. Defaults to FALSE for all return fields which, for convenience, returns all fields for the CoverageAgeRangeDetails.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object listed as text. Run \code{\link{getSchemaForObjects}}('CoverageAgeRangeDetail') to get more field paths.
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
	#' @concept Benefit Management
	#' @return A list of CoverageAgeRangeDetails
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	listCoverageAgeRangeDetails <- function(searchConditionsList = NULL, CoverageAgeRangeDetailID = F, CoverageAgeRangeID = F, AgeThreshold = F, RatePerThousand = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, fieldPaths = NULL, searchConditionsGroupType = "And", searchSortFieldNamesList = NULL, searchSortFieldNamesDescendingList = NULL, entityId = 1, schoolYearId = NULL, page = 1, pageSize = 100000, flatten = T, returnResponse = F){

		params <- as.list(environment())

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		listSkyObjects(module = "BenefitManagement", objectName = "CoverageAgeRangeDetail", searchFields = searchFields %>% append(fieldPaths), page = page, pageSize = pageSize, SearchConditionsList = searchConditionsList, SearchConditionsGroupType = searchConditionsGroupType, SearchSortFieldNamesList = searchSortFieldNamesList, SearchSortFieldNamesDescendingList = searchSortFieldNamesDescendingList, entityId = entityId, schoolYearId = schoolYearId, flatten = flatten, returnResponse = returnResponse)
	}

	#' Get a CoverageAgeRangeDetail
	#'
	#' This function returns a dataframe or json object of a CoverageAgeRangeDetail
	#' @param CoverageAgeRangeDetailID The ID of the CoverageAgeRangeDetail to return.
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given CoverageAgeRangeDetail. Defaults to FALSE for all return fields which, for convenience, returns all fields for the CoverageAgeRangeDetail.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object listed as text. Run \code{\link{getSchemaForObjects}}('CoverageAgeRangeDetail') to get more field paths.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{listSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Benefit Management
	#' @return A dataframe or of CoverageAgeRangeDetail
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	getCoverageAgeRangeDetail <- function(CoverageAgeRangeDetailID, CoverageAgeRangeID = F, AgeThreshold = F, RatePerThousand = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, fieldPaths = NULL, entityId = 1, schoolYearId = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment()) %>% purrr::keep(names(.) != "CoverageAgeRangeDetailID")

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		getSkyObject(module = "BenefitManagement", objectName = "CoverageAgeRangeDetail", objectId = CoverageAgeRangeDetailID, searchFields = searchFields, entityId = entityId, flatten = flatten, returnResponse = returnResponse)
	}

	#' Delete a CoverageAgeRangeDetail
	#'
	#' This function deletes a CoverageAgeRangeDetail
	#' @param CoverageAgeRangeDetailID The ID of the CoverageAgeRangeDetail to delete
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{listSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Benefit Management
	#' @return The CoverageAgeRangeDetailID of the deleted CoverageAgeRangeDetail.
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	deleteCoverageAgeRangeDetail <- function(CoverageAgeRangeDetailID, ignoreWarnings = F, entityId = 1, schoolYearId = NULL, flatten = T, returnResponse = F){

		deleteSkyObject(module = "BenefitManagement", objectName = "CoverageAgeRangeDetail", objectId = CoverageAgeRangeDetailID, ignoreWarnings = ignoreWarnings, entityId = entityId, flatten = flatten, returnResponse = returnResponse)
	}

	#' Create a CoverageAgeRangeDetail
	#'
	#' This function creates a CoverageAgeRangeDetail
	#' @param fieldNames The field values to give the created CoverageAgeRangeDetail. Each defaults to NULL.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{listSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Benefit Management
	#' @return A newly created CoverageAgeRangeDetail
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	createCoverageAgeRangeDetail <- function(CoverageAgeRangeID = NULL, AgeThreshold = NULL, RatePerThousand = NULL, entityId = 1, schoolYearId = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment())

		body <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper()) %>% purrr::compact()

		createSkyObject(module = "BenefitManagement", objectName = "CoverageAgeRangeDetail", body = list(DataObject = body), searchFields = append("CoverageAgeRangeDetailID", body %>% names()), entityId = entityId, schoolYearId = schoolYearId, flatten = flatten, returnResponse = returnResponse)
	}

	#' Modify a CoverageAgeRangeDetail
	#'
	#' This function modifies a CoverageAgeRangeDetail
	#' @param fieldNames The field values to give the modified CoverageAgeRangeDetail. Each defaults to NULL.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{listSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Benefit Management
	#' @return The modified CoverageAgeRangeDetail
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	modifyCoverageAgeRangeDetail <- function(CoverageAgeRangeDetailID, CoverageAgeRangeID = NULL, AgeThreshold = NULL, RatePerThousand = NULL, entityId = 1, schoolYearId = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment())

		body <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper()) %>% purrr::compact()

		modifySkyObject(module = "BenefitManagement", objectName = "CoverageAgeRangeDetail", objectId = CoverageAgeRangeDetailID, body = list(DataObject = body), searchFields = append("CoverageAgeRangeDetailID", body %>% names()), entityId = entityId, schoolYearId = schoolYearId, flatten = flatten, returnResponse = returnResponse)
	}

	#' List TempCoverageMonthRanges
	#'
	#' This function returns a dataframe or json object of TempCoverageMonthRanges
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given TempCoverageMonthRanges. Defaults to FALSE for all return fields which, for convenience, returns all fields for the TempCoverageMonthRanges.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object listed as text. Run \code{\link{getSchemaForObjects}}('TempCoverageMonthRange') to get more field paths.
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
	#' @concept Benefit Management
	#' @return A list of TempCoverageMonthRanges
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	listTempCoverageMonthRanges <- function(searchConditionsList = NULL, TempCoverageMonthRangeID = F, EmployeePlanEnrollmentID = F, CoverageStartDate = F, CoverageEndDate = F, RangeAmount = F, RangePaid = F, RemainingPays = F, TotalPays = F, EmployeeBenefitDeductionID = F, RangeType = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, fieldPaths = NULL, searchConditionsGroupType = "And", searchSortFieldNamesList = NULL, searchSortFieldNamesDescendingList = NULL, entityId = 1, schoolYearId = NULL, page = 1, pageSize = 100000, flatten = T, returnResponse = F){

		params <- as.list(environment())

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		listSkyObjects(module = "BenefitManagement", objectName = "TempCoverageMonthRange", searchFields = searchFields %>% append(fieldPaths), page = page, pageSize = pageSize, SearchConditionsList = searchConditionsList, SearchConditionsGroupType = searchConditionsGroupType, SearchSortFieldNamesList = searchSortFieldNamesList, SearchSortFieldNamesDescendingList = searchSortFieldNamesDescendingList, entityId = entityId, schoolYearId = schoolYearId, flatten = flatten, returnResponse = returnResponse)
	}

	#' Get a TempCoverageMonthRange
	#'
	#' This function returns a dataframe or json object of a TempCoverageMonthRange
	#' @param TempCoverageMonthRangeID The ID of the TempCoverageMonthRange to return.
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given TempCoverageMonthRange. Defaults to FALSE for all return fields which, for convenience, returns all fields for the TempCoverageMonthRange.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object listed as text. Run \code{\link{getSchemaForObjects}}('TempCoverageMonthRange') to get more field paths.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{listSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Benefit Management
	#' @return A dataframe or of TempCoverageMonthRange
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	getTempCoverageMonthRange <- function(TempCoverageMonthRangeID, EmployeePlanEnrollmentID = F, CoverageStartDate = F, CoverageEndDate = F, RangeAmount = F, RangePaid = F, RemainingPays = F, TotalPays = F, EmployeeBenefitDeductionID = F, RangeType = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, fieldPaths = NULL, entityId = 1, schoolYearId = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment()) %>% purrr::keep(names(.) != "TempCoverageMonthRangeID")

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		getSkyObject(module = "BenefitManagement", objectName = "TempCoverageMonthRange", objectId = TempCoverageMonthRangeID, searchFields = searchFields, entityId = entityId, flatten = flatten, returnResponse = returnResponse)
	}

	#' Delete a TempCoverageMonthRange
	#'
	#' This function deletes a TempCoverageMonthRange
	#' @param TempCoverageMonthRangeID The ID of the TempCoverageMonthRange to delete
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{listSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Benefit Management
	#' @return The TempCoverageMonthRangeID of the deleted TempCoverageMonthRange.
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	deleteTempCoverageMonthRange <- function(TempCoverageMonthRangeID, ignoreWarnings = F, entityId = 1, schoolYearId = NULL, flatten = T, returnResponse = F){

		deleteSkyObject(module = "BenefitManagement", objectName = "TempCoverageMonthRange", objectId = TempCoverageMonthRangeID, ignoreWarnings = ignoreWarnings, entityId = entityId, flatten = flatten, returnResponse = returnResponse)
	}

	#' Create a TempCoverageMonthRange
	#'
	#' This function creates a TempCoverageMonthRange
	#' @param fieldNames The field values to give the created TempCoverageMonthRange. Each defaults to NULL.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{listSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Benefit Management
	#' @return A newly created TempCoverageMonthRange
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	createTempCoverageMonthRange <- function(EmployeePlanEnrollmentID = NULL, CoverageStartDate = NULL, CoverageEndDate = NULL, RangeAmount = NULL, RangePaid = NULL, RemainingPays = NULL, TotalPays = NULL, EmployeeBenefitDeductionID = NULL, RangeType = NULL, entityId = 1, schoolYearId = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment())

		body <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper()) %>% purrr::compact()

		createSkyObject(module = "BenefitManagement", objectName = "TempCoverageMonthRange", body = list(DataObject = body), searchFields = append("TempCoverageMonthRangeID", body %>% names()), entityId = entityId, schoolYearId = schoolYearId, flatten = flatten, returnResponse = returnResponse)
	}

	#' Modify a TempCoverageMonthRange
	#'
	#' This function modifies a TempCoverageMonthRange
	#' @param fieldNames The field values to give the modified TempCoverageMonthRange. Each defaults to NULL.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{listSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Benefit Management
	#' @return The modified TempCoverageMonthRange
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	modifyTempCoverageMonthRange <- function(TempCoverageMonthRangeID, EmployeePlanEnrollmentID = NULL, CoverageStartDate = NULL, CoverageEndDate = NULL, RangeAmount = NULL, RangePaid = NULL, RemainingPays = NULL, TotalPays = NULL, EmployeeBenefitDeductionID = NULL, RangeType = NULL, entityId = 1, schoolYearId = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment())

		body <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper()) %>% purrr::compact()

		modifySkyObject(module = "BenefitManagement", objectName = "TempCoverageMonthRange", objectId = TempCoverageMonthRangeID, body = list(DataObject = body), searchFields = append("TempCoverageMonthRangeID", body %>% names()), entityId = entityId, schoolYearId = schoolYearId, flatten = flatten, returnResponse = returnResponse)
	}
