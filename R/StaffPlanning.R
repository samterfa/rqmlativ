
	#' List PlanGroups
	#'
	#' This function returns a dataframe or json object of PlanGroups
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given PlanGroups. Defaults to FALSE for all return fields which, for convenience, returns all fields for the PlanGroups.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object listed as text. Run \code{\link{getSchemaForObjects}}('PlanGroup') to get more field paths.
	#' @param searchConditionsList A list of search conditions to filter results which are joined by the searchConditionsGroupType. Of the form {FieldName} {ConditionType} {SearchCondition}. For example, c('StudentID LessEqual 500', 'LastName Like Ander\%'). Run \code{\link{listSearchConditionTypes}} for a list of ConditionTypes. Defaults to NULL (unfiltered).
	#' @param searchConditionsGroupType The conjunction which joins multiple searchConditions in the searchConditionsList. Either 'Or' or 'And'. Defaults to 'And'.
	#' @param searchSortFieldNamesList The list of fields sort results by. Defaults to NULL (unsorted).
	#' @param searchSortFieldNamesDescendingList A list of T/F values corresponding to whether to sort each field in searchSortFieldNamesList in descending order. Defaults to F for each FieldName in searchSortFieldNamesList.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{listSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param page Results are paginated. The page of results to return. Default is 1.
	#' @param pageSize Results are paginated. The number of records per page to return. Default is 100,000 (essentially all records for most objects).
	#' @param flatten Whether to flatten results into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Staff Planning
	#' @return A list of PlanGroups
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	listPlanGroups <- function(searchConditionsList = NULL, PlanGroupID = F, Code = F, Description = F, CodeDescription = F, PlanID = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, IsUnassigned = F, BudgetedFTE = F, AssignedFTE = F, AvailableFTE = F, NumberOfVacantPositions = F, CurrentUserHasPlanGroupAccess = F, NumberOfOccupiedPositions = F, fieldPaths = NULL, searchConditionsGroupType = "And", searchSortFieldNamesList = NULL, searchSortFieldNamesDescendingList = NULL, entityId = 1, schoolYearId = NULL, page = 1, pageSize = 100000, flatten = T, returnResponse = F){

		params <- as.list(environment())

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		listSkyObjects(module = "StaffPlanning", objectName = "PlanGroup", searchFields = searchFields %>% append(fieldPaths), page = page, pageSize = pageSize, SearchConditionsList = searchConditionsList, SearchConditionsGroupType = searchConditionsGroupType, SearchSortFieldNamesList = searchSortFieldNamesList, SearchSortFieldNamesDescendingList = searchSortFieldNamesDescendingList, entityId = entityId, schoolYearId = schoolYearId, flatten = flatten, returnResponse = returnResponse)
	}

	#' Get a PlanGroup
	#'
	#' This function returns a dataframe or json object of a PlanGroup
	#' @param PlanGroupID The ID of the PlanGroup to return.
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given PlanGroup. Defaults to FALSE for all return fields which, for convenience, returns all fields for the PlanGroup.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object listed as text. Run \code{\link{getSchemaForObjects}}('PlanGroup') to get more field paths.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{listSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Staff Planning
	#' @return A dataframe or of PlanGroup
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	getPlanGroup <- function(PlanGroupID, Code = F, Description = F, CodeDescription = F, PlanID = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, IsUnassigned = F, BudgetedFTE = F, AssignedFTE = F, AvailableFTE = F, NumberOfVacantPositions = F, CurrentUserHasPlanGroupAccess = F, NumberOfOccupiedPositions = F, fieldPaths = NULL, entityId = 1, schoolYearId = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment()) %>% purrr::keep(names(.) != "PlanGroupID")

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		getSkyObject(module = "StaffPlanning", objectName = "PlanGroup", objectId = PlanGroupID, searchFields = searchFields, entityId = entityId, flatten = flatten, returnResponse = returnResponse)
	}

	#' Delete a PlanGroup
	#'
	#' This function deletes a PlanGroup
	#' @param PlanGroupID The ID of the PlanGroup to delete
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{listSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Staff Planning
	#' @return The PlanGroupID of the deleted PlanGroup.
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	deletePlanGroup <- function(PlanGroupID, ignoreWarnings = F, entityId = 1, schoolYearId = NULL, flatten = T, returnResponse = F){

		deleteSkyObject(module = "StaffPlanning", objectName = "PlanGroup", objectId = PlanGroupID, ignoreWarnings = ignoreWarnings, entityId = entityId, flatten = flatten, returnResponse = returnResponse)
	}

	#' Create a PlanGroup
	#'
	#' This function creates a PlanGroup
	#' @param fieldNames The field values to give the created PlanGroup. Each defaults to NULL.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{listSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Staff Planning
	#' @return A newly created PlanGroup
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	createPlanGroup <- function(Code = NULL, Description = NULL, PlanID = NULL, IsUnassigned = NULL, BudgetedFTE = NULL, entityId = 1, schoolYearId = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment())

		body <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper()) %>% purrr::compact()

		createSkyObject(module = "StaffPlanning", objectName = "PlanGroup", body = list(DataObject = body), searchFields = append("PlanGroupID", body %>% names()), entityId = entityId, schoolYearId = schoolYearId, flatten = flatten, returnResponse = returnResponse)
	}

	#' Modify a PlanGroup
	#'
	#' This function modifies a PlanGroup
	#' @param fieldNames The field values to give the modified PlanGroup. Each defaults to NULL.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{listSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Staff Planning
	#' @return The modified PlanGroup
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	modifyPlanGroup <- function(PlanGroupID, Code = NULL, Description = NULL, PlanID = NULL, IsUnassigned = NULL, BudgetedFTE = NULL, entityId = 1, schoolYearId = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment())

		body <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper()) %>% purrr::compact()

		modifySkyObject(module = "StaffPlanning", objectName = "PlanGroup", objectId = PlanGroupID, body = list(DataObject = body), searchFields = append("PlanGroupID", body %>% names()), entityId = entityId, schoolYearId = schoolYearId, flatten = flatten, returnResponse = returnResponse)
	}

	#' List PlanPositions
	#'
	#' This function returns a dataframe or json object of PlanPositions
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given PlanPositions. Defaults to FALSE for all return fields which, for convenience, returns all fields for the PlanPositions.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object listed as text. Run \code{\link{getSchemaForObjects}}('PlanPosition') to get more field paths.
	#' @param searchConditionsList A list of search conditions to filter results which are joined by the searchConditionsGroupType. Of the form {FieldName} {ConditionType} {SearchCondition}. For example, c('StudentID LessEqual 500', 'LastName Like Ander\%'). Run \code{\link{listSearchConditionTypes}} for a list of ConditionTypes. Defaults to NULL (unfiltered).
	#' @param searchConditionsGroupType The conjunction which joins multiple searchConditions in the searchConditionsList. Either 'Or' or 'And'. Defaults to 'And'.
	#' @param searchSortFieldNamesList The list of fields sort results by. Defaults to NULL (unsorted).
	#' @param searchSortFieldNamesDescendingList A list of T/F values corresponding to whether to sort each field in searchSortFieldNamesList in descending order. Defaults to F for each FieldName in searchSortFieldNamesList.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{listSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param page Results are paginated. The page of results to return. Default is 1.
	#' @param pageSize Results are paginated. The number of records per page to return. Default is 100,000 (essentially all records for most objects).
	#' @param flatten Whether to flatten results into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Staff Planning
	#' @return A list of PlanPositions
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	listPlanPositions <- function(searchConditionsList = NULL, PlanPositionID = F, PlanID = F, PositionNumberID = F, BudgetedFTE = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, PlanEmployeeID = F, Description = F, PositionIdentifier = F, AssignedFTE = F, AvailableFTE = F, MatrixID = F, StepNumber = F, CalendarID = F, FullPaidDays = F, FullPaySecondsPerDay = F, Rate = F, AnnualPay = F, CurrentUserHasPlanGroupAccess = F, PositionGroupID = F, JobTypeID = F, PositionIDClonedFrom = F, CalculationStatus = F, TotalBenefitAmount = F, TotalPayAmount = F, SalaryCalculationMethodID = F, TotalCompensationAmount = F, LaneID = F, RequiredCredits = F, PositionTypeID = F, FormattedFullPaySecondsPerDayDecimal = F, PlanPositionDistributionsBuildingCodes = F, PlanPositionDistributionsBuildingDescriptions = F, PlanPositionDistributionsAssignmentTypeCodes = F, PlanPositionDistributionsAssignmentTypeDescriptions = F, PlacementID = F, PlanPositionDistributionsDepartmentCodes = F, PlanPositionDistributionsDepartmentDescriptions = F, PlanPositionDistributionsFTEGroupCodes = F, PlanPositionDistributionsFTEGroupDescriptions = F, EmployeePlacementDescription = F, fieldPaths = NULL, searchConditionsGroupType = "And", searchSortFieldNamesList = NULL, searchSortFieldNamesDescendingList = NULL, entityId = 1, schoolYearId = NULL, page = 1, pageSize = 100000, flatten = T, returnResponse = F){

		params <- as.list(environment())

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		listSkyObjects(module = "StaffPlanning", objectName = "PlanPosition", searchFields = searchFields %>% append(fieldPaths), page = page, pageSize = pageSize, SearchConditionsList = searchConditionsList, SearchConditionsGroupType = searchConditionsGroupType, SearchSortFieldNamesList = searchSortFieldNamesList, SearchSortFieldNamesDescendingList = searchSortFieldNamesDescendingList, entityId = entityId, schoolYearId = schoolYearId, flatten = flatten, returnResponse = returnResponse)
	}

	#' Get a PlanPosition
	#'
	#' This function returns a dataframe or json object of a PlanPosition
	#' @param PlanPositionID The ID of the PlanPosition to return.
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given PlanPosition. Defaults to FALSE for all return fields which, for convenience, returns all fields for the PlanPosition.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object listed as text. Run \code{\link{getSchemaForObjects}}('PlanPosition') to get more field paths.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{listSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Staff Planning
	#' @return A dataframe or of PlanPosition
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	getPlanPosition <- function(PlanPositionID, PlanID = F, PositionNumberID = F, BudgetedFTE = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, PlanEmployeeID = F, Description = F, PositionIdentifier = F, AssignedFTE = F, AvailableFTE = F, MatrixID = F, StepNumber = F, CalendarID = F, FullPaidDays = F, FullPaySecondsPerDay = F, Rate = F, AnnualPay = F, CurrentUserHasPlanGroupAccess = F, PositionGroupID = F, JobTypeID = F, PositionIDClonedFrom = F, CalculationStatus = F, TotalBenefitAmount = F, TotalPayAmount = F, SalaryCalculationMethodID = F, TotalCompensationAmount = F, LaneID = F, RequiredCredits = F, PositionTypeID = F, FormattedFullPaySecondsPerDayDecimal = F, PlanPositionDistributionsBuildingCodes = F, PlanPositionDistributionsBuildingDescriptions = F, PlanPositionDistributionsAssignmentTypeCodes = F, PlanPositionDistributionsAssignmentTypeDescriptions = F, PlacementID = F, PlanPositionDistributionsDepartmentCodes = F, PlanPositionDistributionsDepartmentDescriptions = F, PlanPositionDistributionsFTEGroupCodes = F, PlanPositionDistributionsFTEGroupDescriptions = F, EmployeePlacementDescription = F, fieldPaths = NULL, entityId = 1, schoolYearId = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment()) %>% purrr::keep(names(.) != "PlanPositionID")

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		getSkyObject(module = "StaffPlanning", objectName = "PlanPosition", objectId = PlanPositionID, searchFields = searchFields, entityId = entityId, flatten = flatten, returnResponse = returnResponse)
	}

	#' Delete a PlanPosition
	#'
	#' This function deletes a PlanPosition
	#' @param PlanPositionID The ID of the PlanPosition to delete
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{listSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Staff Planning
	#' @return The PlanPositionID of the deleted PlanPosition.
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	deletePlanPosition <- function(PlanPositionID, ignoreWarnings = F, entityId = 1, schoolYearId = NULL, flatten = T, returnResponse = F){

		deleteSkyObject(module = "StaffPlanning", objectName = "PlanPosition", objectId = PlanPositionID, ignoreWarnings = ignoreWarnings, entityId = entityId, flatten = flatten, returnResponse = returnResponse)
	}

	#' Create a PlanPosition
	#'
	#' This function creates a PlanPosition
	#' @param fieldNames The field values to give the created PlanPosition. Each defaults to NULL.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{listSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Staff Planning
	#' @return A newly created PlanPosition
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	createPlanPosition <- function(PlanID = NULL, PositionNumberID = NULL, BudgetedFTE = NULL, PlanEmployeeID = NULL, Description = NULL, MatrixID = NULL, StepNumber = NULL, CalendarID = NULL, FullPaidDays = NULL, FullPaySecondsPerDay = NULL, Rate = NULL, AnnualPay = NULL, PositionGroupID = NULL, JobTypeID = NULL, PositionIDClonedFrom = NULL, CalculationStatus = NULL, SalaryCalculationMethodID = NULL, LaneID = NULL, RequiredCredits = NULL, PositionTypeID = NULL, PlacementID = NULL, EmployeePlacementDescription = NULL, entityId = 1, schoolYearId = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment())

		body <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper()) %>% purrr::compact()

		createSkyObject(module = "StaffPlanning", objectName = "PlanPosition", body = list(DataObject = body), searchFields = append("PlanPositionID", body %>% names()), entityId = entityId, schoolYearId = schoolYearId, flatten = flatten, returnResponse = returnResponse)
	}

	#' Modify a PlanPosition
	#'
	#' This function modifies a PlanPosition
	#' @param fieldNames The field values to give the modified PlanPosition. Each defaults to NULL.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{listSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Staff Planning
	#' @return The modified PlanPosition
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	modifyPlanPosition <- function(PlanPositionID, PlanID = NULL, PositionNumberID = NULL, BudgetedFTE = NULL, PlanEmployeeID = NULL, Description = NULL, MatrixID = NULL, StepNumber = NULL, CalendarID = NULL, FullPaidDays = NULL, FullPaySecondsPerDay = NULL, Rate = NULL, AnnualPay = NULL, PositionGroupID = NULL, JobTypeID = NULL, PositionIDClonedFrom = NULL, CalculationStatus = NULL, SalaryCalculationMethodID = NULL, LaneID = NULL, RequiredCredits = NULL, PositionTypeID = NULL, PlacementID = NULL, EmployeePlacementDescription = NULL, entityId = 1, schoolYearId = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment())

		body <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper()) %>% purrr::compact()

		modifySkyObject(module = "StaffPlanning", objectName = "PlanPosition", objectId = PlanPositionID, body = list(DataObject = body), searchFields = append("PlanPositionID", body %>% names()), entityId = entityId, schoolYearId = schoolYearId, flatten = flatten, returnResponse = returnResponse)
	}

	#' List StaffPlanningPlans
	#'
	#' This function returns a dataframe or json object of StaffPlanningPlans
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given StaffPlanningPlans. Defaults to FALSE for all return fields which, for convenience, returns all fields for the StaffPlanningPlans.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object listed as text. Run \code{\link{getSchemaForObjects}}('StaffPlanningPlan') to get more field paths.
	#' @param searchConditionsList A list of search conditions to filter results which are joined by the searchConditionsGroupType. Of the form {FieldName} {ConditionType} {SearchCondition}. For example, c('StudentID LessEqual 500', 'LastName Like Ander\%'). Run \code{\link{listSearchConditionTypes}} for a list of ConditionTypes. Defaults to NULL (unfiltered).
	#' @param searchConditionsGroupType The conjunction which joins multiple searchConditions in the searchConditionsList. Either 'Or' or 'And'. Defaults to 'And'.
	#' @param searchSortFieldNamesList The list of fields sort results by. Defaults to NULL (unsorted).
	#' @param searchSortFieldNamesDescendingList A list of T/F values corresponding to whether to sort each field in searchSortFieldNamesList in descending order. Defaults to F for each FieldName in searchSortFieldNamesList.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{listSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param page Results are paginated. The page of results to return. Default is 1.
	#' @param pageSize Results are paginated. The number of records per page to return. Default is 100,000 (essentially all records for most objects).
	#' @param flatten Whether to flatten results into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Staff Planning
	#' @return A list of StaffPlanningPlans
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	listStaffPlanningPlans <- function(searchConditionsList = NULL, PlanID = F, DistrictID = F, FiscalYearID = F, Description = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, NumberOfOccupiedPositions = F, NumberOfVacantPositions = F, CurrentUserHasPlanGroupAccess = F, CalculationStatusCode = F, TotalBenefitAmount = F, TotalPayAmount = F, TotalCompensationAmount = F, IsBudgeted = F, BudgetVersionAmount = F, BudgetVersionDifference = F, fieldPaths = NULL, searchConditionsGroupType = "And", searchSortFieldNamesList = NULL, searchSortFieldNamesDescendingList = NULL, entityId = 1, schoolYearId = NULL, page = 1, pageSize = 100000, flatten = T, returnResponse = F){

		params <- as.list(environment())

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		listSkyObjects(module = "StaffPlanning", objectName = "Plan", searchFields = searchFields %>% append(fieldPaths), page = page, pageSize = pageSize, SearchConditionsList = searchConditionsList, SearchConditionsGroupType = searchConditionsGroupType, SearchSortFieldNamesList = searchSortFieldNamesList, SearchSortFieldNamesDescendingList = searchSortFieldNamesDescendingList, entityId = entityId, schoolYearId = schoolYearId, flatten = flatten, returnResponse = returnResponse)
	}

	#' Get a StaffPlanningPlan
	#'
	#' This function returns a dataframe or json object of a StaffPlanningPlan
	#' @param StaffPlanningPlanID The ID of the StaffPlanningPlan to return.
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given StaffPlanningPlan. Defaults to FALSE for all return fields which, for convenience, returns all fields for the StaffPlanningPlan.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object listed as text. Run \code{\link{getSchemaForObjects}}('StaffPlanningPlan') to get more field paths.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{listSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Staff Planning
	#' @return A dataframe or of StaffPlanningPlan
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	getStaffPlanningPlan <- function(StaffPlanningPlanID, PlanID = F, DistrictID = F, FiscalYearID = F, Description = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, NumberOfOccupiedPositions = F, NumberOfVacantPositions = F, CurrentUserHasPlanGroupAccess = F, CalculationStatusCode = F, TotalBenefitAmount = F, TotalPayAmount = F, TotalCompensationAmount = F, IsBudgeted = F, BudgetVersionAmount = F, BudgetVersionDifference = F, fieldPaths = NULL, entityId = 1, schoolYearId = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment()) %>% purrr::keep(names(.) != "StaffPlanningPlanID")

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		getSkyObject(module = "StaffPlanning", objectName = "Plan", objectId = StaffPlanningPlanID, searchFields = searchFields, entityId = entityId, flatten = flatten, returnResponse = returnResponse)
	}

	#' Delete a StaffPlanningPlan
	#'
	#' This function deletes a StaffPlanningPlan
	#' @param StaffPlanningPlanID The ID of the StaffPlanningPlan to delete
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{listSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Staff Planning
	#' @return The StaffPlanningPlanID of the deleted StaffPlanningPlan.
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	deleteStaffPlanningPlan <- function(StaffPlanningPlanID, ignoreWarnings = F, entityId = 1, schoolYearId = NULL, flatten = T, returnResponse = F){

		deleteSkyObject(module = "StaffPlanning", objectName = "Plan", objectId = StaffPlanningPlanID, ignoreWarnings = ignoreWarnings, entityId = entityId, flatten = flatten, returnResponse = returnResponse)
	}

	#' Create a StaffPlanningPlan
	#'
	#' This function creates a StaffPlanningPlan
	#' @param fieldNames The field values to give the created StaffPlanningPlan. Each defaults to NULL.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{listSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Staff Planning
	#' @return A newly created StaffPlanningPlan
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	createStaffPlanningPlan <- function(DistrictID = NULL, FiscalYearID = NULL, Description = NULL, entityId = 1, schoolYearId = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment())

		body <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper()) %>% purrr::compact()

		createSkyObject(module = "StaffPlanning", objectName = "Plan", body = list(DataObject = body), searchFields = append("PlanID", body %>% names()), entityId = entityId, schoolYearId = schoolYearId, flatten = flatten, returnResponse = returnResponse)
	}

	#' Modify a StaffPlanningPlan
	#'
	#' This function modifies a StaffPlanningPlan
	#' @param fieldNames The field values to give the modified StaffPlanningPlan. Each defaults to NULL.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{listSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Staff Planning
	#' @return The modified StaffPlanningPlan
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	modifyStaffPlanningPlan <- function(PlanID, DistrictID = NULL, FiscalYearID = NULL, Description = NULL, entityId = 1, schoolYearId = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment())

		body <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper()) %>% purrr::compact()

		modifySkyObject(module = "StaffPlanning", objectName = "Plan", objectId = PlanID, body = list(DataObject = body), searchFields = append("PlanID", body %>% names()), entityId = entityId, schoolYearId = schoolYearId, flatten = flatten, returnResponse = returnResponse)
	}

	#' List PlanPositionDistributions
	#'
	#' This function returns a dataframe or json object of PlanPositionDistributions
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given PlanPositionDistributions. Defaults to FALSE for all return fields which, for convenience, returns all fields for the PlanPositionDistributions.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object listed as text. Run \code{\link{getSchemaForObjects}}('PlanPositionDistribution') to get more field paths.
	#' @param searchConditionsList A list of search conditions to filter results which are joined by the searchConditionsGroupType. Of the form {FieldName} {ConditionType} {SearchCondition}. For example, c('StudentID LessEqual 500', 'LastName Like Ander\%'). Run \code{\link{listSearchConditionTypes}} for a list of ConditionTypes. Defaults to NULL (unfiltered).
	#' @param searchConditionsGroupType The conjunction which joins multiple searchConditions in the searchConditionsList. Either 'Or' or 'And'. Defaults to 'And'.
	#' @param searchSortFieldNamesList The list of fields sort results by. Defaults to NULL (unsorted).
	#' @param searchSortFieldNamesDescendingList A list of T/F values corresponding to whether to sort each field in searchSortFieldNamesList in descending order. Defaults to F for each FieldName in searchSortFieldNamesList.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{listSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param page Results are paginated. The page of results to return. Default is 1.
	#' @param pageSize Results are paginated. The number of records per page to return. Default is 100,000 (essentially all records for most objects).
	#' @param flatten Whether to flatten results into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Staff Planning
	#' @return A list of PlanPositionDistributions
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	listPlanPositionDistributions <- function(searchConditionsList = NULL, PlanPositionDistributionID = F, PlanGroupID = F, PlanPositionID = F, AssignmentTypeID = F, BuildingID = F, FTE = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, CurrentUserHasPlanGroupAccess = F, DepartmentID = F, FTEGroupID = F, fieldPaths = NULL, searchConditionsGroupType = "And", searchSortFieldNamesList = NULL, searchSortFieldNamesDescendingList = NULL, entityId = 1, schoolYearId = NULL, page = 1, pageSize = 100000, flatten = T, returnResponse = F){

		params <- as.list(environment())

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		listSkyObjects(module = "StaffPlanning", objectName = "PlanPositionDistribution", searchFields = searchFields %>% append(fieldPaths), page = page, pageSize = pageSize, SearchConditionsList = searchConditionsList, SearchConditionsGroupType = searchConditionsGroupType, SearchSortFieldNamesList = searchSortFieldNamesList, SearchSortFieldNamesDescendingList = searchSortFieldNamesDescendingList, entityId = entityId, schoolYearId = schoolYearId, flatten = flatten, returnResponse = returnResponse)
	}

	#' Get a PlanPositionDistribution
	#'
	#' This function returns a dataframe or json object of a PlanPositionDistribution
	#' @param PlanPositionDistributionID The ID of the PlanPositionDistribution to return.
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given PlanPositionDistribution. Defaults to FALSE for all return fields which, for convenience, returns all fields for the PlanPositionDistribution.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object listed as text. Run \code{\link{getSchemaForObjects}}('PlanPositionDistribution') to get more field paths.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{listSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Staff Planning
	#' @return A dataframe or of PlanPositionDistribution
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	getPlanPositionDistribution <- function(PlanPositionDistributionID, PlanGroupID = F, PlanPositionID = F, AssignmentTypeID = F, BuildingID = F, FTE = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, CurrentUserHasPlanGroupAccess = F, DepartmentID = F, FTEGroupID = F, fieldPaths = NULL, entityId = 1, schoolYearId = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment()) %>% purrr::keep(names(.) != "PlanPositionDistributionID")

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		getSkyObject(module = "StaffPlanning", objectName = "PlanPositionDistribution", objectId = PlanPositionDistributionID, searchFields = searchFields, entityId = entityId, flatten = flatten, returnResponse = returnResponse)
	}

	#' Delete a PlanPositionDistribution
	#'
	#' This function deletes a PlanPositionDistribution
	#' @param PlanPositionDistributionID The ID of the PlanPositionDistribution to delete
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{listSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Staff Planning
	#' @return The PlanPositionDistributionID of the deleted PlanPositionDistribution.
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	deletePlanPositionDistribution <- function(PlanPositionDistributionID, ignoreWarnings = F, entityId = 1, schoolYearId = NULL, flatten = T, returnResponse = F){

		deleteSkyObject(module = "StaffPlanning", objectName = "PlanPositionDistribution", objectId = PlanPositionDistributionID, ignoreWarnings = ignoreWarnings, entityId = entityId, flatten = flatten, returnResponse = returnResponse)
	}

	#' Create a PlanPositionDistribution
	#'
	#' This function creates a PlanPositionDistribution
	#' @param fieldNames The field values to give the created PlanPositionDistribution. Each defaults to NULL.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{listSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Staff Planning
	#' @return A newly created PlanPositionDistribution
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	createPlanPositionDistribution <- function(PlanGroupID = NULL, PlanPositionID = NULL, AssignmentTypeID = NULL, BuildingID = NULL, FTE = NULL, DepartmentID = NULL, FTEGroupID = NULL, entityId = 1, schoolYearId = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment())

		body <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper()) %>% purrr::compact()

		createSkyObject(module = "StaffPlanning", objectName = "PlanPositionDistribution", body = list(DataObject = body), searchFields = append("PlanPositionDistributionID", body %>% names()), entityId = entityId, schoolYearId = schoolYearId, flatten = flatten, returnResponse = returnResponse)
	}

	#' Modify a PlanPositionDistribution
	#'
	#' This function modifies a PlanPositionDistribution
	#' @param fieldNames The field values to give the modified PlanPositionDistribution. Each defaults to NULL.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{listSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Staff Planning
	#' @return The modified PlanPositionDistribution
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	modifyPlanPositionDistribution <- function(PlanPositionDistributionID, PlanGroupID = NULL, PlanPositionID = NULL, AssignmentTypeID = NULL, BuildingID = NULL, FTE = NULL, DepartmentID = NULL, FTEGroupID = NULL, entityId = 1, schoolYearId = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment())

		body <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper()) %>% purrr::compact()

		modifySkyObject(module = "StaffPlanning", objectName = "PlanPositionDistribution", objectId = PlanPositionDistributionID, body = list(DataObject = body), searchFields = append("PlanPositionDistributionID", body %>% names()), entityId = entityId, schoolYearId = schoolYearId, flatten = flatten, returnResponse = returnResponse)
	}

	#' List PlanGroupClearances
	#'
	#' This function returns a dataframe or json object of PlanGroupClearances
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given PlanGroupClearances. Defaults to FALSE for all return fields which, for convenience, returns all fields for the PlanGroupClearances.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object listed as text. Run \code{\link{getSchemaForObjects}}('PlanGroupClearance') to get more field paths.
	#' @param searchConditionsList A list of search conditions to filter results which are joined by the searchConditionsGroupType. Of the form {FieldName} {ConditionType} {SearchCondition}. For example, c('StudentID LessEqual 500', 'LastName Like Ander\%'). Run \code{\link{listSearchConditionTypes}} for a list of ConditionTypes. Defaults to NULL (unfiltered).
	#' @param searchConditionsGroupType The conjunction which joins multiple searchConditions in the searchConditionsList. Either 'Or' or 'And'. Defaults to 'And'.
	#' @param searchSortFieldNamesList The list of fields sort results by. Defaults to NULL (unsorted).
	#' @param searchSortFieldNamesDescendingList A list of T/F values corresponding to whether to sort each field in searchSortFieldNamesList in descending order. Defaults to F for each FieldName in searchSortFieldNamesList.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{listSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param page Results are paginated. The page of results to return. Default is 1.
	#' @param pageSize Results are paginated. The number of records per page to return. Default is 100,000 (essentially all records for most objects).
	#' @param flatten Whether to flatten results into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Staff Planning
	#' @return A list of PlanGroupClearances
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	listPlanGroupClearances <- function(searchConditionsList = NULL, PlanGroupClearanceID = F, PlanGroupID = F, SecurityGroupID = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, CurrentUserHasPlanGroupAccess = F, fieldPaths = NULL, searchConditionsGroupType = "And", searchSortFieldNamesList = NULL, searchSortFieldNamesDescendingList = NULL, entityId = 1, schoolYearId = NULL, page = 1, pageSize = 100000, flatten = T, returnResponse = F){

		params <- as.list(environment())

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		listSkyObjects(module = "StaffPlanning", objectName = "PlanGroupClearance", searchFields = searchFields %>% append(fieldPaths), page = page, pageSize = pageSize, SearchConditionsList = searchConditionsList, SearchConditionsGroupType = searchConditionsGroupType, SearchSortFieldNamesList = searchSortFieldNamesList, SearchSortFieldNamesDescendingList = searchSortFieldNamesDescendingList, entityId = entityId, schoolYearId = schoolYearId, flatten = flatten, returnResponse = returnResponse)
	}

	#' Get a PlanGroupClearance
	#'
	#' This function returns a dataframe or json object of a PlanGroupClearance
	#' @param PlanGroupClearanceID The ID of the PlanGroupClearance to return.
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given PlanGroupClearance. Defaults to FALSE for all return fields which, for convenience, returns all fields for the PlanGroupClearance.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object listed as text. Run \code{\link{getSchemaForObjects}}('PlanGroupClearance') to get more field paths.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{listSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Staff Planning
	#' @return A dataframe or of PlanGroupClearance
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	getPlanGroupClearance <- function(PlanGroupClearanceID, PlanGroupID = F, SecurityGroupID = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, CurrentUserHasPlanGroupAccess = F, fieldPaths = NULL, entityId = 1, schoolYearId = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment()) %>% purrr::keep(names(.) != "PlanGroupClearanceID")

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		getSkyObject(module = "StaffPlanning", objectName = "PlanGroupClearance", objectId = PlanGroupClearanceID, searchFields = searchFields, entityId = entityId, flatten = flatten, returnResponse = returnResponse)
	}

	#' Delete a PlanGroupClearance
	#'
	#' This function deletes a PlanGroupClearance
	#' @param PlanGroupClearanceID The ID of the PlanGroupClearance to delete
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{listSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Staff Planning
	#' @return The PlanGroupClearanceID of the deleted PlanGroupClearance.
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	deletePlanGroupClearance <- function(PlanGroupClearanceID, ignoreWarnings = F, entityId = 1, schoolYearId = NULL, flatten = T, returnResponse = F){

		deleteSkyObject(module = "StaffPlanning", objectName = "PlanGroupClearance", objectId = PlanGroupClearanceID, ignoreWarnings = ignoreWarnings, entityId = entityId, flatten = flatten, returnResponse = returnResponse)
	}

	#' Create a PlanGroupClearance
	#'
	#' This function creates a PlanGroupClearance
	#' @param fieldNames The field values to give the created PlanGroupClearance. Each defaults to NULL.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{listSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Staff Planning
	#' @return A newly created PlanGroupClearance
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	createPlanGroupClearance <- function(PlanGroupID = NULL, SecurityGroupID = NULL, entityId = 1, schoolYearId = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment())

		body <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper()) %>% purrr::compact()

		createSkyObject(module = "StaffPlanning", objectName = "PlanGroupClearance", body = list(DataObject = body), searchFields = append("PlanGroupClearanceID", body %>% names()), entityId = entityId, schoolYearId = schoolYearId, flatten = flatten, returnResponse = returnResponse)
	}

	#' Modify a PlanGroupClearance
	#'
	#' This function modifies a PlanGroupClearance
	#' @param fieldNames The field values to give the modified PlanGroupClearance. Each defaults to NULL.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{listSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Staff Planning
	#' @return The modified PlanGroupClearance
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	modifyPlanGroupClearance <- function(PlanGroupClearanceID, PlanGroupID = NULL, SecurityGroupID = NULL, entityId = 1, schoolYearId = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment())

		body <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper()) %>% purrr::compact()

		modifySkyObject(module = "StaffPlanning", objectName = "PlanGroupClearance", objectId = PlanGroupClearanceID, body = list(DataObject = body), searchFields = append("PlanGroupClearanceID", body %>% names()), entityId = entityId, schoolYearId = schoolYearId, flatten = flatten, returnResponse = returnResponse)
	}

	#' List PlanEmployees
	#'
	#' This function returns a dataframe or json object of PlanEmployees
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given PlanEmployees. Defaults to FALSE for all return fields which, for convenience, returns all fields for the PlanEmployees.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object listed as text. Run \code{\link{getSchemaForObjects}}('PlanEmployee') to get more field paths.
	#' @param searchConditionsList A list of search conditions to filter results which are joined by the searchConditionsGroupType. Of the form {FieldName} {ConditionType} {SearchCondition}. For example, c('StudentID LessEqual 500', 'LastName Like Ander\%'). Run \code{\link{listSearchConditionTypes}} for a list of ConditionTypes. Defaults to NULL (unfiltered).
	#' @param searchConditionsGroupType The conjunction which joins multiple searchConditions in the searchConditionsList. Either 'Or' or 'And'. Defaults to 'And'.
	#' @param searchSortFieldNamesList The list of fields sort results by. Defaults to NULL (unsorted).
	#' @param searchSortFieldNamesDescendingList A list of T/F values corresponding to whether to sort each field in searchSortFieldNamesList in descending order. Defaults to F for each FieldName in searchSortFieldNamesList.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{listSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param page Results are paginated. The page of results to return. Default is 1.
	#' @param pageSize Results are paginated. The number of records per page to return. Default is 100,000 (essentially all records for most objects).
	#' @param flatten Whether to flatten results into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Staff Planning
	#' @return A list of PlanEmployees
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	listPlanEmployees <- function(searchConditionsList = NULL, PlanEmployeeID = F, EmployeeID = F, PlanID = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, AssignedFTE = F, CurrentUserHasPlanGroupAccess = F, TotalBenefitAmount = F, TotalPayAmount = F, TotalCompensationAmount = F, fieldPaths = NULL, searchConditionsGroupType = "And", searchSortFieldNamesList = NULL, searchSortFieldNamesDescendingList = NULL, entityId = 1, schoolYearId = NULL, page = 1, pageSize = 100000, flatten = T, returnResponse = F){

		params <- as.list(environment())

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		listSkyObjects(module = "StaffPlanning", objectName = "PlanEmployee", searchFields = searchFields %>% append(fieldPaths), page = page, pageSize = pageSize, SearchConditionsList = searchConditionsList, SearchConditionsGroupType = searchConditionsGroupType, SearchSortFieldNamesList = searchSortFieldNamesList, SearchSortFieldNamesDescendingList = searchSortFieldNamesDescendingList, entityId = entityId, schoolYearId = schoolYearId, flatten = flatten, returnResponse = returnResponse)
	}

	#' Get a PlanEmployee
	#'
	#' This function returns a dataframe or json object of a PlanEmployee
	#' @param PlanEmployeeID The ID of the PlanEmployee to return.
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given PlanEmployee. Defaults to FALSE for all return fields which, for convenience, returns all fields for the PlanEmployee.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object listed as text. Run \code{\link{getSchemaForObjects}}('PlanEmployee') to get more field paths.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{listSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Staff Planning
	#' @return A dataframe or of PlanEmployee
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	getPlanEmployee <- function(PlanEmployeeID, EmployeeID = F, PlanID = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, AssignedFTE = F, CurrentUserHasPlanGroupAccess = F, TotalBenefitAmount = F, TotalPayAmount = F, TotalCompensationAmount = F, fieldPaths = NULL, entityId = 1, schoolYearId = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment()) %>% purrr::keep(names(.) != "PlanEmployeeID")

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		getSkyObject(module = "StaffPlanning", objectName = "PlanEmployee", objectId = PlanEmployeeID, searchFields = searchFields, entityId = entityId, flatten = flatten, returnResponse = returnResponse)
	}

	#' Delete a PlanEmployee
	#'
	#' This function deletes a PlanEmployee
	#' @param PlanEmployeeID The ID of the PlanEmployee to delete
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{listSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Staff Planning
	#' @return The PlanEmployeeID of the deleted PlanEmployee.
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	deletePlanEmployee <- function(PlanEmployeeID, ignoreWarnings = F, entityId = 1, schoolYearId = NULL, flatten = T, returnResponse = F){

		deleteSkyObject(module = "StaffPlanning", objectName = "PlanEmployee", objectId = PlanEmployeeID, ignoreWarnings = ignoreWarnings, entityId = entityId, flatten = flatten, returnResponse = returnResponse)
	}

	#' Create a PlanEmployee
	#'
	#' This function creates a PlanEmployee
	#' @param fieldNames The field values to give the created PlanEmployee. Each defaults to NULL.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{listSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Staff Planning
	#' @return A newly created PlanEmployee
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	createPlanEmployee <- function(EmployeeID = NULL, PlanID = NULL, entityId = 1, schoolYearId = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment())

		body <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper()) %>% purrr::compact()

		createSkyObject(module = "StaffPlanning", objectName = "PlanEmployee", body = list(DataObject = body), searchFields = append("PlanEmployeeID", body %>% names()), entityId = entityId, schoolYearId = schoolYearId, flatten = flatten, returnResponse = returnResponse)
	}

	#' Modify a PlanEmployee
	#'
	#' This function modifies a PlanEmployee
	#' @param fieldNames The field values to give the modified PlanEmployee. Each defaults to NULL.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{listSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Staff Planning
	#' @return The modified PlanEmployee
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	modifyPlanEmployee <- function(PlanEmployeeID, EmployeeID = NULL, PlanID = NULL, entityId = 1, schoolYearId = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment())

		body <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper()) %>% purrr::compact()

		modifySkyObject(module = "StaffPlanning", objectName = "PlanEmployee", objectId = PlanEmployeeID, body = list(DataObject = body), searchFields = append("PlanEmployeeID", body %>% names()), entityId = entityId, schoolYearId = schoolYearId, flatten = flatten, returnResponse = returnResponse)
	}

	#' List PlanGroupBuildings
	#'
	#' This function returns a dataframe or json object of PlanGroupBuildings
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given PlanGroupBuildings. Defaults to FALSE for all return fields which, for convenience, returns all fields for the PlanGroupBuildings.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object listed as text. Run \code{\link{getSchemaForObjects}}('PlanGroupBuilding') to get more field paths.
	#' @param searchConditionsList A list of search conditions to filter results which are joined by the searchConditionsGroupType. Of the form {FieldName} {ConditionType} {SearchCondition}. For example, c('StudentID LessEqual 500', 'LastName Like Ander\%'). Run \code{\link{listSearchConditionTypes}} for a list of ConditionTypes. Defaults to NULL (unfiltered).
	#' @param searchConditionsGroupType The conjunction which joins multiple searchConditions in the searchConditionsList. Either 'Or' or 'And'. Defaults to 'And'.
	#' @param searchSortFieldNamesList The list of fields sort results by. Defaults to NULL (unsorted).
	#' @param searchSortFieldNamesDescendingList A list of T/F values corresponding to whether to sort each field in searchSortFieldNamesList in descending order. Defaults to F for each FieldName in searchSortFieldNamesList.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{listSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param page Results are paginated. The page of results to return. Default is 1.
	#' @param pageSize Results are paginated. The number of records per page to return. Default is 100,000 (essentially all records for most objects).
	#' @param flatten Whether to flatten results into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Staff Planning
	#' @return A list of PlanGroupBuildings
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	listPlanGroupBuildings <- function(searchConditionsList = NULL, PlanGroupBuildingID = F, PlanGroupID = F, BuildingID = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, CurrentUserHasPlanGroupAccess = F, fieldPaths = NULL, searchConditionsGroupType = "And", searchSortFieldNamesList = NULL, searchSortFieldNamesDescendingList = NULL, entityId = 1, schoolYearId = NULL, page = 1, pageSize = 100000, flatten = T, returnResponse = F){

		params <- as.list(environment())

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		listSkyObjects(module = "StaffPlanning", objectName = "PlanGroupBuilding", searchFields = searchFields %>% append(fieldPaths), page = page, pageSize = pageSize, SearchConditionsList = searchConditionsList, SearchConditionsGroupType = searchConditionsGroupType, SearchSortFieldNamesList = searchSortFieldNamesList, SearchSortFieldNamesDescendingList = searchSortFieldNamesDescendingList, entityId = entityId, schoolYearId = schoolYearId, flatten = flatten, returnResponse = returnResponse)
	}

	#' Get a PlanGroupBuilding
	#'
	#' This function returns a dataframe or json object of a PlanGroupBuilding
	#' @param PlanGroupBuildingID The ID of the PlanGroupBuilding to return.
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given PlanGroupBuilding. Defaults to FALSE for all return fields which, for convenience, returns all fields for the PlanGroupBuilding.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object listed as text. Run \code{\link{getSchemaForObjects}}('PlanGroupBuilding') to get more field paths.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{listSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Staff Planning
	#' @return A dataframe or of PlanGroupBuilding
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	getPlanGroupBuilding <- function(PlanGroupBuildingID, PlanGroupID = F, BuildingID = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, CurrentUserHasPlanGroupAccess = F, fieldPaths = NULL, entityId = 1, schoolYearId = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment()) %>% purrr::keep(names(.) != "PlanGroupBuildingID")

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		getSkyObject(module = "StaffPlanning", objectName = "PlanGroupBuilding", objectId = PlanGroupBuildingID, searchFields = searchFields, entityId = entityId, flatten = flatten, returnResponse = returnResponse)
	}

	#' Delete a PlanGroupBuilding
	#'
	#' This function deletes a PlanGroupBuilding
	#' @param PlanGroupBuildingID The ID of the PlanGroupBuilding to delete
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{listSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Staff Planning
	#' @return The PlanGroupBuildingID of the deleted PlanGroupBuilding.
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	deletePlanGroupBuilding <- function(PlanGroupBuildingID, ignoreWarnings = F, entityId = 1, schoolYearId = NULL, flatten = T, returnResponse = F){

		deleteSkyObject(module = "StaffPlanning", objectName = "PlanGroupBuilding", objectId = PlanGroupBuildingID, ignoreWarnings = ignoreWarnings, entityId = entityId, flatten = flatten, returnResponse = returnResponse)
	}

	#' Create a PlanGroupBuilding
	#'
	#' This function creates a PlanGroupBuilding
	#' @param fieldNames The field values to give the created PlanGroupBuilding. Each defaults to NULL.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{listSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Staff Planning
	#' @return A newly created PlanGroupBuilding
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	createPlanGroupBuilding <- function(PlanGroupID = NULL, BuildingID = NULL, entityId = 1, schoolYearId = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment())

		body <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper()) %>% purrr::compact()

		createSkyObject(module = "StaffPlanning", objectName = "PlanGroupBuilding", body = list(DataObject = body), searchFields = append("PlanGroupBuildingID", body %>% names()), entityId = entityId, schoolYearId = schoolYearId, flatten = flatten, returnResponse = returnResponse)
	}

	#' Modify a PlanGroupBuilding
	#'
	#' This function modifies a PlanGroupBuilding
	#' @param fieldNames The field values to give the modified PlanGroupBuilding. Each defaults to NULL.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{listSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Staff Planning
	#' @return The modified PlanGroupBuilding
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	modifyPlanGroupBuilding <- function(PlanGroupBuildingID, PlanGroupID = NULL, BuildingID = NULL, entityId = 1, schoolYearId = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment())

		body <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper()) %>% purrr::compact()

		modifySkyObject(module = "StaffPlanning", objectName = "PlanGroupBuilding", objectId = PlanGroupBuildingID, body = list(DataObject = body), searchFields = append("PlanGroupBuildingID", body %>% names()), entityId = entityId, schoolYearId = schoolYearId, flatten = flatten, returnResponse = returnResponse)
	}

	#' List PlanGroupAssignmentTypes
	#'
	#' This function returns a dataframe or json object of PlanGroupAssignmentTypes
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given PlanGroupAssignmentTypes. Defaults to FALSE for all return fields which, for convenience, returns all fields for the PlanGroupAssignmentTypes.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object listed as text. Run \code{\link{getSchemaForObjects}}('PlanGroupAssignmentType') to get more field paths.
	#' @param searchConditionsList A list of search conditions to filter results which are joined by the searchConditionsGroupType. Of the form {FieldName} {ConditionType} {SearchCondition}. For example, c('StudentID LessEqual 500', 'LastName Like Ander\%'). Run \code{\link{listSearchConditionTypes}} for a list of ConditionTypes. Defaults to NULL (unfiltered).
	#' @param searchConditionsGroupType The conjunction which joins multiple searchConditions in the searchConditionsList. Either 'Or' or 'And'. Defaults to 'And'.
	#' @param searchSortFieldNamesList The list of fields sort results by. Defaults to NULL (unsorted).
	#' @param searchSortFieldNamesDescendingList A list of T/F values corresponding to whether to sort each field in searchSortFieldNamesList in descending order. Defaults to F for each FieldName in searchSortFieldNamesList.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{listSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param page Results are paginated. The page of results to return. Default is 1.
	#' @param pageSize Results are paginated. The number of records per page to return. Default is 100,000 (essentially all records for most objects).
	#' @param flatten Whether to flatten results into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Staff Planning
	#' @return A list of PlanGroupAssignmentTypes
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	listPlanGroupAssignmentTypes <- function(searchConditionsList = NULL, PlanGroupAssignmentTypeID = F, PlanGroupID = F, AssignmentTypeID = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, CurrentUserHasPlanGroupAccess = F, fieldPaths = NULL, searchConditionsGroupType = "And", searchSortFieldNamesList = NULL, searchSortFieldNamesDescendingList = NULL, entityId = 1, schoolYearId = NULL, page = 1, pageSize = 100000, flatten = T, returnResponse = F){

		params <- as.list(environment())

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		listSkyObjects(module = "StaffPlanning", objectName = "PlanGroupAssignmentType", searchFields = searchFields %>% append(fieldPaths), page = page, pageSize = pageSize, SearchConditionsList = searchConditionsList, SearchConditionsGroupType = searchConditionsGroupType, SearchSortFieldNamesList = searchSortFieldNamesList, SearchSortFieldNamesDescendingList = searchSortFieldNamesDescendingList, entityId = entityId, schoolYearId = schoolYearId, flatten = flatten, returnResponse = returnResponse)
	}

	#' Get a PlanGroupAssignmentType
	#'
	#' This function returns a dataframe or json object of a PlanGroupAssignmentType
	#' @param PlanGroupAssignmentTypeID The ID of the PlanGroupAssignmentType to return.
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given PlanGroupAssignmentType. Defaults to FALSE for all return fields which, for convenience, returns all fields for the PlanGroupAssignmentType.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object listed as text. Run \code{\link{getSchemaForObjects}}('PlanGroupAssignmentType') to get more field paths.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{listSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Staff Planning
	#' @return A dataframe or of PlanGroupAssignmentType
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	getPlanGroupAssignmentType <- function(PlanGroupAssignmentTypeID, PlanGroupID = F, AssignmentTypeID = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, CurrentUserHasPlanGroupAccess = F, fieldPaths = NULL, entityId = 1, schoolYearId = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment()) %>% purrr::keep(names(.) != "PlanGroupAssignmentTypeID")

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		getSkyObject(module = "StaffPlanning", objectName = "PlanGroupAssignmentType", objectId = PlanGroupAssignmentTypeID, searchFields = searchFields, entityId = entityId, flatten = flatten, returnResponse = returnResponse)
	}

	#' Delete a PlanGroupAssignmentType
	#'
	#' This function deletes a PlanGroupAssignmentType
	#' @param PlanGroupAssignmentTypeID The ID of the PlanGroupAssignmentType to delete
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{listSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Staff Planning
	#' @return The PlanGroupAssignmentTypeID of the deleted PlanGroupAssignmentType.
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	deletePlanGroupAssignmentType <- function(PlanGroupAssignmentTypeID, ignoreWarnings = F, entityId = 1, schoolYearId = NULL, flatten = T, returnResponse = F){

		deleteSkyObject(module = "StaffPlanning", objectName = "PlanGroupAssignmentType", objectId = PlanGroupAssignmentTypeID, ignoreWarnings = ignoreWarnings, entityId = entityId, flatten = flatten, returnResponse = returnResponse)
	}

	#' Create a PlanGroupAssignmentType
	#'
	#' This function creates a PlanGroupAssignmentType
	#' @param fieldNames The field values to give the created PlanGroupAssignmentType. Each defaults to NULL.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{listSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Staff Planning
	#' @return A newly created PlanGroupAssignmentType
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	createPlanGroupAssignmentType <- function(PlanGroupID = NULL, AssignmentTypeID = NULL, entityId = 1, schoolYearId = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment())

		body <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper()) %>% purrr::compact()

		createSkyObject(module = "StaffPlanning", objectName = "PlanGroupAssignmentType", body = list(DataObject = body), searchFields = append("PlanGroupAssignmentTypeID", body %>% names()), entityId = entityId, schoolYearId = schoolYearId, flatten = flatten, returnResponse = returnResponse)
	}

	#' Modify a PlanGroupAssignmentType
	#'
	#' This function modifies a PlanGroupAssignmentType
	#' @param fieldNames The field values to give the modified PlanGroupAssignmentType. Each defaults to NULL.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{listSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Staff Planning
	#' @return The modified PlanGroupAssignmentType
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	modifyPlanGroupAssignmentType <- function(PlanGroupAssignmentTypeID, PlanGroupID = NULL, AssignmentTypeID = NULL, entityId = 1, schoolYearId = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment())

		body <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper()) %>% purrr::compact()

		modifySkyObject(module = "StaffPlanning", objectName = "PlanGroupAssignmentType", objectId = PlanGroupAssignmentTypeID, body = list(DataObject = body), searchFields = append("PlanGroupAssignmentTypeID", body %>% names()), entityId = entityId, schoolYearId = schoolYearId, flatten = flatten, returnResponse = returnResponse)
	}

	#' List PlanGroupPositionTypes
	#'
	#' This function returns a dataframe or json object of PlanGroupPositionTypes
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given PlanGroupPositionTypes. Defaults to FALSE for all return fields which, for convenience, returns all fields for the PlanGroupPositionTypes.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object listed as text. Run \code{\link{getSchemaForObjects}}('PlanGroupPositionType') to get more field paths.
	#' @param searchConditionsList A list of search conditions to filter results which are joined by the searchConditionsGroupType. Of the form {FieldName} {ConditionType} {SearchCondition}. For example, c('StudentID LessEqual 500', 'LastName Like Ander\%'). Run \code{\link{listSearchConditionTypes}} for a list of ConditionTypes. Defaults to NULL (unfiltered).
	#' @param searchConditionsGroupType The conjunction which joins multiple searchConditions in the searchConditionsList. Either 'Or' or 'And'. Defaults to 'And'.
	#' @param searchSortFieldNamesList The list of fields sort results by. Defaults to NULL (unsorted).
	#' @param searchSortFieldNamesDescendingList A list of T/F values corresponding to whether to sort each field in searchSortFieldNamesList in descending order. Defaults to F for each FieldName in searchSortFieldNamesList.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{listSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param page Results are paginated. The page of results to return. Default is 1.
	#' @param pageSize Results are paginated. The number of records per page to return. Default is 100,000 (essentially all records for most objects).
	#' @param flatten Whether to flatten results into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Staff Planning
	#' @return A list of PlanGroupPositionTypes
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	listPlanGroupPositionTypes <- function(searchConditionsList = NULL, PlanGroupPositionTypeID = F, PlanGroupID = F, PositionTypeID = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, CurrentUserHasPlanGroupAccess = F, fieldPaths = NULL, searchConditionsGroupType = "And", searchSortFieldNamesList = NULL, searchSortFieldNamesDescendingList = NULL, entityId = 1, schoolYearId = NULL, page = 1, pageSize = 100000, flatten = T, returnResponse = F){

		params <- as.list(environment())

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		listSkyObjects(module = "StaffPlanning", objectName = "PlanGroupPositionType", searchFields = searchFields %>% append(fieldPaths), page = page, pageSize = pageSize, SearchConditionsList = searchConditionsList, SearchConditionsGroupType = searchConditionsGroupType, SearchSortFieldNamesList = searchSortFieldNamesList, SearchSortFieldNamesDescendingList = searchSortFieldNamesDescendingList, entityId = entityId, schoolYearId = schoolYearId, flatten = flatten, returnResponse = returnResponse)
	}

	#' Get a PlanGroupPositionType
	#'
	#' This function returns a dataframe or json object of a PlanGroupPositionType
	#' @param PlanGroupPositionTypeID The ID of the PlanGroupPositionType to return.
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given PlanGroupPositionType. Defaults to FALSE for all return fields which, for convenience, returns all fields for the PlanGroupPositionType.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object listed as text. Run \code{\link{getSchemaForObjects}}('PlanGroupPositionType') to get more field paths.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{listSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Staff Planning
	#' @return A dataframe or of PlanGroupPositionType
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	getPlanGroupPositionType <- function(PlanGroupPositionTypeID, PlanGroupID = F, PositionTypeID = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, CurrentUserHasPlanGroupAccess = F, fieldPaths = NULL, entityId = 1, schoolYearId = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment()) %>% purrr::keep(names(.) != "PlanGroupPositionTypeID")

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		getSkyObject(module = "StaffPlanning", objectName = "PlanGroupPositionType", objectId = PlanGroupPositionTypeID, searchFields = searchFields, entityId = entityId, flatten = flatten, returnResponse = returnResponse)
	}

	#' Delete a PlanGroupPositionType
	#'
	#' This function deletes a PlanGroupPositionType
	#' @param PlanGroupPositionTypeID The ID of the PlanGroupPositionType to delete
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{listSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Staff Planning
	#' @return The PlanGroupPositionTypeID of the deleted PlanGroupPositionType.
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	deletePlanGroupPositionType <- function(PlanGroupPositionTypeID, ignoreWarnings = F, entityId = 1, schoolYearId = NULL, flatten = T, returnResponse = F){

		deleteSkyObject(module = "StaffPlanning", objectName = "PlanGroupPositionType", objectId = PlanGroupPositionTypeID, ignoreWarnings = ignoreWarnings, entityId = entityId, flatten = flatten, returnResponse = returnResponse)
	}

	#' Create a PlanGroupPositionType
	#'
	#' This function creates a PlanGroupPositionType
	#' @param fieldNames The field values to give the created PlanGroupPositionType. Each defaults to NULL.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{listSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Staff Planning
	#' @return A newly created PlanGroupPositionType
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	createPlanGroupPositionType <- function(PlanGroupID = NULL, PositionTypeID = NULL, entityId = 1, schoolYearId = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment())

		body <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper()) %>% purrr::compact()

		createSkyObject(module = "StaffPlanning", objectName = "PlanGroupPositionType", body = list(DataObject = body), searchFields = append("PlanGroupPositionTypeID", body %>% names()), entityId = entityId, schoolYearId = schoolYearId, flatten = flatten, returnResponse = returnResponse)
	}

	#' Modify a PlanGroupPositionType
	#'
	#' This function modifies a PlanGroupPositionType
	#' @param fieldNames The field values to give the modified PlanGroupPositionType. Each defaults to NULL.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{listSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Staff Planning
	#' @return The modified PlanGroupPositionType
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	modifyPlanGroupPositionType <- function(PlanGroupPositionTypeID, PlanGroupID = NULL, PositionTypeID = NULL, entityId = 1, schoolYearId = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment())

		body <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper()) %>% purrr::compact()

		modifySkyObject(module = "StaffPlanning", objectName = "PlanGroupPositionType", objectId = PlanGroupPositionTypeID, body = list(DataObject = body), searchFields = append("PlanGroupPositionTypeID", body %>% names()), entityId = entityId, schoolYearId = schoolYearId, flatten = flatten, returnResponse = returnResponse)
	}

	#' List TempPlanPositionDistributions
	#'
	#' This function returns a dataframe or json object of TempPlanPositionDistributions
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given TempPlanPositionDistributions. Defaults to FALSE for all return fields which, for convenience, returns all fields for the TempPlanPositionDistributions.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object listed as text. Run \code{\link{getSchemaForObjects}}('TempPlanPositionDistribution') to get more field paths.
	#' @param searchConditionsList A list of search conditions to filter results which are joined by the searchConditionsGroupType. Of the form {FieldName} {ConditionType} {SearchCondition}. For example, c('StudentID LessEqual 500', 'LastName Like Ander\%'). Run \code{\link{listSearchConditionTypes}} for a list of ConditionTypes. Defaults to NULL (unfiltered).
	#' @param searchConditionsGroupType The conjunction which joins multiple searchConditions in the searchConditionsList. Either 'Or' or 'And'. Defaults to 'And'.
	#' @param searchSortFieldNamesList The list of fields sort results by. Defaults to NULL (unsorted).
	#' @param searchSortFieldNamesDescendingList A list of T/F values corresponding to whether to sort each field in searchSortFieldNamesList in descending order. Defaults to F for each FieldName in searchSortFieldNamesList.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{listSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param page Results are paginated. The page of results to return. Default is 1.
	#' @param pageSize Results are paginated. The number of records per page to return. Default is 100,000 (essentially all records for most objects).
	#' @param flatten Whether to flatten results into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Staff Planning
	#' @return A list of TempPlanPositionDistributions
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	listTempPlanPositionDistributions <- function(searchConditionsList = NULL, TempPlanPositionDistributionID = F, TempPlanPositionEmployeeID = F, AssignmentTypeID = F, BuildingID = F, FTE = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, FTEGroupID = F, DepartmentID = F, fieldPaths = NULL, searchConditionsGroupType = "And", searchSortFieldNamesList = NULL, searchSortFieldNamesDescendingList = NULL, entityId = 1, schoolYearId = NULL, page = 1, pageSize = 100000, flatten = T, returnResponse = F){

		params <- as.list(environment())

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		listSkyObjects(module = "StaffPlanning", objectName = "TempPlanPositionDistribution", searchFields = searchFields %>% append(fieldPaths), page = page, pageSize = pageSize, SearchConditionsList = searchConditionsList, SearchConditionsGroupType = searchConditionsGroupType, SearchSortFieldNamesList = searchSortFieldNamesList, SearchSortFieldNamesDescendingList = searchSortFieldNamesDescendingList, entityId = entityId, schoolYearId = schoolYearId, flatten = flatten, returnResponse = returnResponse)
	}

	#' Get a TempPlanPositionDistribution
	#'
	#' This function returns a dataframe or json object of a TempPlanPositionDistribution
	#' @param TempPlanPositionDistributionID The ID of the TempPlanPositionDistribution to return.
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given TempPlanPositionDistribution. Defaults to FALSE for all return fields which, for convenience, returns all fields for the TempPlanPositionDistribution.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object listed as text. Run \code{\link{getSchemaForObjects}}('TempPlanPositionDistribution') to get more field paths.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{listSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Staff Planning
	#' @return A dataframe or of TempPlanPositionDistribution
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	getTempPlanPositionDistribution <- function(TempPlanPositionDistributionID, TempPlanPositionEmployeeID = F, AssignmentTypeID = F, BuildingID = F, FTE = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, FTEGroupID = F, DepartmentID = F, fieldPaths = NULL, entityId = 1, schoolYearId = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment()) %>% purrr::keep(names(.) != "TempPlanPositionDistributionID")

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		getSkyObject(module = "StaffPlanning", objectName = "TempPlanPositionDistribution", objectId = TempPlanPositionDistributionID, searchFields = searchFields, entityId = entityId, flatten = flatten, returnResponse = returnResponse)
	}

	#' Delete a TempPlanPositionDistribution
	#'
	#' This function deletes a TempPlanPositionDistribution
	#' @param TempPlanPositionDistributionID The ID of the TempPlanPositionDistribution to delete
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{listSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Staff Planning
	#' @return The TempPlanPositionDistributionID of the deleted TempPlanPositionDistribution.
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	deleteTempPlanPositionDistribution <- function(TempPlanPositionDistributionID, ignoreWarnings = F, entityId = 1, schoolYearId = NULL, flatten = T, returnResponse = F){

		deleteSkyObject(module = "StaffPlanning", objectName = "TempPlanPositionDistribution", objectId = TempPlanPositionDistributionID, ignoreWarnings = ignoreWarnings, entityId = entityId, flatten = flatten, returnResponse = returnResponse)
	}

	#' Create a TempPlanPositionDistribution
	#'
	#' This function creates a TempPlanPositionDistribution
	#' @param fieldNames The field values to give the created TempPlanPositionDistribution. Each defaults to NULL.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{listSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Staff Planning
	#' @return A newly created TempPlanPositionDistribution
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	createTempPlanPositionDistribution <- function(TempPlanPositionEmployeeID = NULL, AssignmentTypeID = NULL, BuildingID = NULL, FTE = NULL, FTEGroupID = NULL, DepartmentID = NULL, entityId = 1, schoolYearId = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment())

		body <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper()) %>% purrr::compact()

		createSkyObject(module = "StaffPlanning", objectName = "TempPlanPositionDistribution", body = list(DataObject = body), searchFields = append("TempPlanPositionDistributionID", body %>% names()), entityId = entityId, schoolYearId = schoolYearId, flatten = flatten, returnResponse = returnResponse)
	}

	#' Modify a TempPlanPositionDistribution
	#'
	#' This function modifies a TempPlanPositionDistribution
	#' @param fieldNames The field values to give the modified TempPlanPositionDistribution. Each defaults to NULL.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{listSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Staff Planning
	#' @return The modified TempPlanPositionDistribution
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	modifyTempPlanPositionDistribution <- function(TempPlanPositionDistributionID, TempPlanPositionEmployeeID = NULL, AssignmentTypeID = NULL, BuildingID = NULL, FTE = NULL, FTEGroupID = NULL, DepartmentID = NULL, entityId = 1, schoolYearId = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment())

		body <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper()) %>% purrr::compact()

		modifySkyObject(module = "StaffPlanning", objectName = "TempPlanPositionDistribution", objectId = TempPlanPositionDistributionID, body = list(DataObject = body), searchFields = append("TempPlanPositionDistributionID", body %>% names()), entityId = entityId, schoolYearId = schoolYearId, flatten = flatten, returnResponse = returnResponse)
	}

	#' List TempPlanPositionEmployees
	#'
	#' This function returns a dataframe or json object of TempPlanPositionEmployees
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given TempPlanPositionEmployees. Defaults to FALSE for all return fields which, for convenience, returns all fields for the TempPlanPositionEmployees.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object listed as text. Run \code{\link{getSchemaForObjects}}('TempPlanPositionEmployee') to get more field paths.
	#' @param searchConditionsList A list of search conditions to filter results which are joined by the searchConditionsGroupType. Of the form {FieldName} {ConditionType} {SearchCondition}. For example, c('StudentID LessEqual 500', 'LastName Like Ander\%'). Run \code{\link{listSearchConditionTypes}} for a list of ConditionTypes. Defaults to NULL (unfiltered).
	#' @param searchConditionsGroupType The conjunction which joins multiple searchConditions in the searchConditionsList. Either 'Or' or 'And'. Defaults to 'And'.
	#' @param searchSortFieldNamesList The list of fields sort results by. Defaults to NULL (unsorted).
	#' @param searchSortFieldNamesDescendingList A list of T/F values corresponding to whether to sort each field in searchSortFieldNamesList in descending order. Defaults to F for each FieldName in searchSortFieldNamesList.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{listSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param page Results are paginated. The page of results to return. Default is 1.
	#' @param pageSize Results are paginated. The number of records per page to return. Default is 100,000 (essentially all records for most objects).
	#' @param flatten Whether to flatten results into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Staff Planning
	#' @return A list of TempPlanPositionEmployees
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	listTempPlanPositionEmployees <- function(searchConditionsList = NULL, TempPlanPositionEmployeeID = F, PositionNumberID = F, PositionNumberCode = F, PositionIdentifier = F, PositionTypeCodeDescription = F, BudgetedFTE = F, EmployeeID = F, EmployeeFullNameLFM = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, MatrixID = F, CalendarID = F, FullPaidDays = F, FormattedFullPaySecondsPerDay = F, Rate = F, StepNumber = F, SalaryCalculationMethodID = F, AmountTypeCode = F, AmountType = F, AnnualPay = F, FullPaySecondsPerDay = F, PositionGroupID = F, JobTypeID = F, PositionIDClonedFrom = F, OldBudgetedFTE = F, MatrixCode = F, OldMatrixCode = F, LaneID = F, OldLaneCode = F, LaneCode = F, RequiredCredits = F, OldRequiredCredits = F, CalendarCode = F, OldCalendarCode = F, OldFormattedFullPaySecondsPerDay = F, OldRate = F, OldStepNumber = F, SalaryCalculationMethodCode = F, OldSalaryCalculationMethodCode = F, OldAnnualPay = F, OldFullPaySecondsPerDay = F, Description = F, PlanPositionID = F, EmployeeNumber = F, PositionTypeID = F, AssignmentTypeCodes = F, AssignmentTypeDescriptions = F, BuildingCodes = F, BuildingDescriptions = F, PlacementID = F, PlacementCode = F, OldPlacementCode = F, HasFatalException = F, ErrorMessage = F, MidpointGroup = F, CappedAtMaximumRate = F, MidpointGroupID = F, MidpointGroupCodeDescription = F, OldMidpointGroup = F, TRSStateBaseStep = F, OldTRSStateBaseStep = F, TRSStateBaseStepID = F, OldTRSStateBaseStepAmount = F, TRSStateBaseStepAmount = F, ConfigFiscalYearTRSStateBaseStepID = F, ConfigFiscalYearTRSStateBaseStepLaneCodeStepNumber = F, EmployeePlacementDescription = F, OldEmployeePlacementDescription = F, PositionTypeCode = F, ErrorCount = F, fieldPaths = NULL, searchConditionsGroupType = "And", searchSortFieldNamesList = NULL, searchSortFieldNamesDescendingList = NULL, entityId = 1, schoolYearId = NULL, page = 1, pageSize = 100000, flatten = T, returnResponse = F){

		params <- as.list(environment())

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		listSkyObjects(module = "StaffPlanning", objectName = "TempPlanPositionEmployee", searchFields = searchFields %>% append(fieldPaths), page = page, pageSize = pageSize, SearchConditionsList = searchConditionsList, SearchConditionsGroupType = searchConditionsGroupType, SearchSortFieldNamesList = searchSortFieldNamesList, SearchSortFieldNamesDescendingList = searchSortFieldNamesDescendingList, entityId = entityId, schoolYearId = schoolYearId, flatten = flatten, returnResponse = returnResponse)
	}

	#' Get a TempPlanPositionEmployee
	#'
	#' This function returns a dataframe or json object of a TempPlanPositionEmployee
	#' @param TempPlanPositionEmployeeID The ID of the TempPlanPositionEmployee to return.
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given TempPlanPositionEmployee. Defaults to FALSE for all return fields which, for convenience, returns all fields for the TempPlanPositionEmployee.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object listed as text. Run \code{\link{getSchemaForObjects}}('TempPlanPositionEmployee') to get more field paths.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{listSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Staff Planning
	#' @return A dataframe or of TempPlanPositionEmployee
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	getTempPlanPositionEmployee <- function(TempPlanPositionEmployeeID, PositionNumberID = F, PositionNumberCode = F, PositionIdentifier = F, PositionTypeCodeDescription = F, BudgetedFTE = F, EmployeeID = F, EmployeeFullNameLFM = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, MatrixID = F, CalendarID = F, FullPaidDays = F, FormattedFullPaySecondsPerDay = F, Rate = F, StepNumber = F, SalaryCalculationMethodID = F, AmountTypeCode = F, AmountType = F, AnnualPay = F, FullPaySecondsPerDay = F, PositionGroupID = F, JobTypeID = F, PositionIDClonedFrom = F, OldBudgetedFTE = F, MatrixCode = F, OldMatrixCode = F, LaneID = F, OldLaneCode = F, LaneCode = F, RequiredCredits = F, OldRequiredCredits = F, CalendarCode = F, OldCalendarCode = F, OldFormattedFullPaySecondsPerDay = F, OldRate = F, OldStepNumber = F, SalaryCalculationMethodCode = F, OldSalaryCalculationMethodCode = F, OldAnnualPay = F, OldFullPaySecondsPerDay = F, Description = F, PlanPositionID = F, EmployeeNumber = F, PositionTypeID = F, AssignmentTypeCodes = F, AssignmentTypeDescriptions = F, BuildingCodes = F, BuildingDescriptions = F, PlacementID = F, PlacementCode = F, OldPlacementCode = F, HasFatalException = F, ErrorMessage = F, MidpointGroup = F, CappedAtMaximumRate = F, MidpointGroupID = F, MidpointGroupCodeDescription = F, OldMidpointGroup = F, TRSStateBaseStep = F, OldTRSStateBaseStep = F, TRSStateBaseStepID = F, OldTRSStateBaseStepAmount = F, TRSStateBaseStepAmount = F, ConfigFiscalYearTRSStateBaseStepID = F, ConfigFiscalYearTRSStateBaseStepLaneCodeStepNumber = F, EmployeePlacementDescription = F, OldEmployeePlacementDescription = F, PositionTypeCode = F, ErrorCount = F, fieldPaths = NULL, entityId = 1, schoolYearId = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment()) %>% purrr::keep(names(.) != "TempPlanPositionEmployeeID")

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		getSkyObject(module = "StaffPlanning", objectName = "TempPlanPositionEmployee", objectId = TempPlanPositionEmployeeID, searchFields = searchFields, entityId = entityId, flatten = flatten, returnResponse = returnResponse)
	}

	#' Delete a TempPlanPositionEmployee
	#'
	#' This function deletes a TempPlanPositionEmployee
	#' @param TempPlanPositionEmployeeID The ID of the TempPlanPositionEmployee to delete
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{listSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Staff Planning
	#' @return The TempPlanPositionEmployeeID of the deleted TempPlanPositionEmployee.
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	deleteTempPlanPositionEmployee <- function(TempPlanPositionEmployeeID, ignoreWarnings = F, entityId = 1, schoolYearId = NULL, flatten = T, returnResponse = F){

		deleteSkyObject(module = "StaffPlanning", objectName = "TempPlanPositionEmployee", objectId = TempPlanPositionEmployeeID, ignoreWarnings = ignoreWarnings, entityId = entityId, flatten = flatten, returnResponse = returnResponse)
	}

	#' Create a TempPlanPositionEmployee
	#'
	#' This function creates a TempPlanPositionEmployee
	#' @param fieldNames The field values to give the created TempPlanPositionEmployee. Each defaults to NULL.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{listSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Staff Planning
	#' @return A newly created TempPlanPositionEmployee
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	createTempPlanPositionEmployee <- function(PositionNumberID = NULL, PositionNumberCode = NULL, PositionIdentifier = NULL, PositionTypeCodeDescription = NULL, BudgetedFTE = NULL, EmployeeID = NULL, EmployeeFullNameLFM = NULL, MatrixID = NULL, CalendarID = NULL, FullPaidDays = NULL, FormattedFullPaySecondsPerDay = NULL, Rate = NULL, StepNumber = NULL, SalaryCalculationMethodID = NULL, AmountTypeCode = NULL, AmountType = NULL, AnnualPay = NULL, FullPaySecondsPerDay = NULL, PositionGroupID = NULL, JobTypeID = NULL, PositionIDClonedFrom = NULL, OldBudgetedFTE = NULL, MatrixCode = NULL, OldMatrixCode = NULL, LaneID = NULL, OldLaneCode = NULL, LaneCode = NULL, RequiredCredits = NULL, OldRequiredCredits = NULL, CalendarCode = NULL, OldCalendarCode = NULL, OldFormattedFullPaySecondsPerDay = NULL, OldRate = NULL, OldStepNumber = NULL, SalaryCalculationMethodCode = NULL, OldSalaryCalculationMethodCode = NULL, OldAnnualPay = NULL, OldFullPaySecondsPerDay = NULL, Description = NULL, PlanPositionID = NULL, EmployeeNumber = NULL, PositionTypeID = NULL, AssignmentTypeCodes = NULL, AssignmentTypeDescriptions = NULL, BuildingCodes = NULL, BuildingDescriptions = NULL, PlacementID = NULL, PlacementCode = NULL, OldPlacementCode = NULL, HasFatalException = NULL, ErrorMessage = NULL, MidpointGroup = NULL, CappedAtMaximumRate = NULL, MidpointGroupID = NULL, MidpointGroupCodeDescription = NULL, OldMidpointGroup = NULL, TRSStateBaseStep = NULL, OldTRSStateBaseStep = NULL, TRSStateBaseStepID = NULL, OldTRSStateBaseStepAmount = NULL, TRSStateBaseStepAmount = NULL, ConfigFiscalYearTRSStateBaseStepID = NULL, ConfigFiscalYearTRSStateBaseStepLaneCodeStepNumber = NULL, EmployeePlacementDescription = NULL, OldEmployeePlacementDescription = NULL, PositionTypeCode = NULL, ErrorCount = NULL, entityId = 1, schoolYearId = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment())

		body <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper()) %>% purrr::compact()

		createSkyObject(module = "StaffPlanning", objectName = "TempPlanPositionEmployee", body = list(DataObject = body), searchFields = append("TempPlanPositionEmployeeID", body %>% names()), entityId = entityId, schoolYearId = schoolYearId, flatten = flatten, returnResponse = returnResponse)
	}

	#' Modify a TempPlanPositionEmployee
	#'
	#' This function modifies a TempPlanPositionEmployee
	#' @param fieldNames The field values to give the modified TempPlanPositionEmployee. Each defaults to NULL.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{listSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Staff Planning
	#' @return The modified TempPlanPositionEmployee
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	modifyTempPlanPositionEmployee <- function(TempPlanPositionEmployeeID, PositionNumberID = NULL, PositionNumberCode = NULL, PositionIdentifier = NULL, PositionTypeCodeDescription = NULL, BudgetedFTE = NULL, EmployeeID = NULL, EmployeeFullNameLFM = NULL, MatrixID = NULL, CalendarID = NULL, FullPaidDays = NULL, FormattedFullPaySecondsPerDay = NULL, Rate = NULL, StepNumber = NULL, SalaryCalculationMethodID = NULL, AmountTypeCode = NULL, AmountType = NULL, AnnualPay = NULL, FullPaySecondsPerDay = NULL, PositionGroupID = NULL, JobTypeID = NULL, PositionIDClonedFrom = NULL, OldBudgetedFTE = NULL, MatrixCode = NULL, OldMatrixCode = NULL, LaneID = NULL, OldLaneCode = NULL, LaneCode = NULL, RequiredCredits = NULL, OldRequiredCredits = NULL, CalendarCode = NULL, OldCalendarCode = NULL, OldFormattedFullPaySecondsPerDay = NULL, OldRate = NULL, OldStepNumber = NULL, SalaryCalculationMethodCode = NULL, OldSalaryCalculationMethodCode = NULL, OldAnnualPay = NULL, OldFullPaySecondsPerDay = NULL, Description = NULL, PlanPositionID = NULL, EmployeeNumber = NULL, PositionTypeID = NULL, AssignmentTypeCodes = NULL, AssignmentTypeDescriptions = NULL, BuildingCodes = NULL, BuildingDescriptions = NULL, PlacementID = NULL, PlacementCode = NULL, OldPlacementCode = NULL, HasFatalException = NULL, ErrorMessage = NULL, MidpointGroup = NULL, CappedAtMaximumRate = NULL, MidpointGroupID = NULL, MidpointGroupCodeDescription = NULL, OldMidpointGroup = NULL, TRSStateBaseStep = NULL, OldTRSStateBaseStep = NULL, TRSStateBaseStepID = NULL, OldTRSStateBaseStepAmount = NULL, TRSStateBaseStepAmount = NULL, ConfigFiscalYearTRSStateBaseStepID = NULL, ConfigFiscalYearTRSStateBaseStepLaneCodeStepNumber = NULL, EmployeePlacementDescription = NULL, OldEmployeePlacementDescription = NULL, PositionTypeCode = NULL, ErrorCount = NULL, entityId = 1, schoolYearId = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment())

		body <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper()) %>% purrr::compact()

		modifySkyObject(module = "StaffPlanning", objectName = "TempPlanPositionEmployee", objectId = TempPlanPositionEmployeeID, body = list(DataObject = body), searchFields = append("TempPlanPositionEmployeeID", body %>% names()), entityId = entityId, schoolYearId = schoolYearId, flatten = flatten, returnResponse = returnResponse)
	}

	#' List AssignmentTypePlanPositionDistributionSummaries
	#'
	#' This function returns a dataframe or json object of AssignmentTypePlanPositionDistributionSummaries
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given AssignmentTypePlanPositionDistributionSummaries. Defaults to FALSE for all return fields which, for convenience, returns all fields for the AssignmentTypePlanPositionDistributionSummaries.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object listed as text. Run \code{\link{getSchemaForObjects}}('AssignmentTypePlanPositionDistributionSummary') to get more field paths.
	#' @param searchConditionsList A list of search conditions to filter results which are joined by the searchConditionsGroupType. Of the form {FieldName} {ConditionType} {SearchCondition}. For example, c('StudentID LessEqual 500', 'LastName Like Ander\%'). Run \code{\link{listSearchConditionTypes}} for a list of ConditionTypes. Defaults to NULL (unfiltered).
	#' @param searchConditionsGroupType The conjunction which joins multiple searchConditions in the searchConditionsList. Either 'Or' or 'And'. Defaults to 'And'.
	#' @param searchSortFieldNamesList The list of fields sort results by. Defaults to NULL (unsorted).
	#' @param searchSortFieldNamesDescendingList A list of T/F values corresponding to whether to sort each field in searchSortFieldNamesList in descending order. Defaults to F for each FieldName in searchSortFieldNamesList.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{listSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param page Results are paginated. The page of results to return. Default is 1.
	#' @param pageSize Results are paginated. The number of records per page to return. Default is 100,000 (essentially all records for most objects).
	#' @param flatten Whether to flatten results into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Staff Planning
	#' @return A list of AssignmentTypePlanPositionDistributionSummaries
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	listAssignmentTypePlanPositionDistributionSummaries <- function(searchConditionsList = NULL, PlanPositionDistributionIDFirst = F, PlanGroupID = F, PositionTypeID = F, AssignmentTypeID = F, FTE = F, fieldPaths = NULL, searchConditionsGroupType = "And", searchSortFieldNamesList = NULL, searchSortFieldNamesDescendingList = NULL, entityId = 1, schoolYearId = NULL, page = 1, pageSize = 100000, flatten = T, returnResponse = F){

		params <- as.list(environment())

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		listSkyObjects(module = "StaffPlanning", objectName = "AssignmentTypePlanPositionDistributionSummary", searchFields = searchFields %>% append(fieldPaths), page = page, pageSize = pageSize, SearchConditionsList = searchConditionsList, SearchConditionsGroupType = searchConditionsGroupType, SearchSortFieldNamesList = searchSortFieldNamesList, SearchSortFieldNamesDescendingList = searchSortFieldNamesDescendingList, entityId = entityId, schoolYearId = schoolYearId, flatten = flatten, returnResponse = returnResponse)
	}

	#' Get an AssignmentTypePlanPositionDistributionSummary
	#'
	#' This function returns a dataframe or json object of an AssignmentTypePlanPositionDistributionSummary
	#' @param AssignmentTypePlanPositionDistributionSummaryID The ID of the AssignmentTypePlanPositionDistributionSummary to return.
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given AssignmentTypePlanPositionDistributionSummary. Defaults to FALSE for all return fields which, for convenience, returns all fields for the AssignmentTypePlanPositionDistributionSummary.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object listed as text. Run \code{\link{getSchemaForObjects}}('AssignmentTypePlanPositionDistributionSummary') to get more field paths.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{listSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Staff Planning
	#' @return A dataframe or of AssignmentTypePlanPositionDistributionSummary
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	getAssignmentTypePlanPositionDistributionSummary <- function(AssignmentTypePlanPositionDistributionSummaryID, PlanPositionDistributionIDFirst = F, PlanGroupID = F, PositionTypeID = F, AssignmentTypeID = F, FTE = F, fieldPaths = NULL, entityId = 1, schoolYearId = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment()) %>% purrr::keep(names(.) != "AssignmentTypePlanPositionDistributionSummaryID")

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		getSkyObject(module = "StaffPlanning", objectName = "AssignmentTypePlanPositionDistributionSummary", objectId = AssignmentTypePlanPositionDistributionSummaryID, searchFields = searchFields, entityId = entityId, flatten = flatten, returnResponse = returnResponse)
	}

	#' Delete an AssignmentTypePlanPositionDistributionSummary
	#'
	#' This function deletes an AssignmentTypePlanPositionDistributionSummary
	#' @param AssignmentTypePlanPositionDistributionSummaryID The ID of the AssignmentTypePlanPositionDistributionSummary to delete
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{listSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Staff Planning
	#' @return The AssignmentTypePlanPositionDistributionSummaryID of the deleted AssignmentTypePlanPositionDistributionSummary.
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	deleteAssignmentTypePlanPositionDistributionSummary <- function(AssignmentTypePlanPositionDistributionSummaryID, ignoreWarnings = F, entityId = 1, schoolYearId = NULL, flatten = T, returnResponse = F){

		deleteSkyObject(module = "StaffPlanning", objectName = "AssignmentTypePlanPositionDistributionSummary", objectId = AssignmentTypePlanPositionDistributionSummaryID, ignoreWarnings = ignoreWarnings, entityId = entityId, flatten = flatten, returnResponse = returnResponse)
	}

	#' Create an AssignmentTypePlanPositionDistributionSummary
	#'
	#' This function creates an AssignmentTypePlanPositionDistributionSummary
	#' @param fieldNames The field values to give the created AssignmentTypePlanPositionDistributionSummary. Each defaults to NULL.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{listSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Staff Planning
	#' @return A newly created AssignmentTypePlanPositionDistributionSummary
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	createAssignmentTypePlanPositionDistributionSummary <- function(PlanGroupID = NULL, PositionTypeID = NULL, AssignmentTypeID = NULL, entityId = 1, schoolYearId = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment())

		body <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper()) %>% purrr::compact()

		createSkyObject(module = "StaffPlanning", objectName = "AssignmentTypePlanPositionDistributionSummary", body = list(DataObject = body), searchFields = append("AssignmentTypePlanPositionDistributionSummaryID", body %>% names()), entityId = entityId, schoolYearId = schoolYearId, flatten = flatten, returnResponse = returnResponse)
	}

	#' Modify an AssignmentTypePlanPositionDistributionSummary
	#'
	#' This function modifies an AssignmentTypePlanPositionDistributionSummary
	#' @param fieldNames The field values to give the modified AssignmentTypePlanPositionDistributionSummary. Each defaults to NULL.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{listSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Staff Planning
	#' @return The modified AssignmentTypePlanPositionDistributionSummary
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	modifyAssignmentTypePlanPositionDistributionSummary <- function(AssignmentTypePlanPositionDistributionSummaryID, PlanGroupID = NULL, PositionTypeID = NULL, AssignmentTypeID = NULL, entityId = 1, schoolYearId = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment())

		body <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper()) %>% purrr::compact()

		modifySkyObject(module = "StaffPlanning", objectName = "AssignmentTypePlanPositionDistributionSummary", objectId = AssignmentTypePlanPositionDistributionSummaryID, body = list(DataObject = body), searchFields = append("AssignmentTypePlanPositionDistributionSummaryID", body %>% names()), entityId = entityId, schoolYearId = schoolYearId, flatten = flatten, returnResponse = returnResponse)
	}

	#' List BuildingPlanPositionDistributionSummaries
	#'
	#' This function returns a dataframe or json object of BuildingPlanPositionDistributionSummaries
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given BuildingPlanPositionDistributionSummaries. Defaults to FALSE for all return fields which, for convenience, returns all fields for the BuildingPlanPositionDistributionSummaries.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object listed as text. Run \code{\link{getSchemaForObjects}}('BuildingPlanPositionDistributionSummary') to get more field paths.
	#' @param searchConditionsList A list of search conditions to filter results which are joined by the searchConditionsGroupType. Of the form {FieldName} {ConditionType} {SearchCondition}. For example, c('StudentID LessEqual 500', 'LastName Like Ander\%'). Run \code{\link{listSearchConditionTypes}} for a list of ConditionTypes. Defaults to NULL (unfiltered).
	#' @param searchConditionsGroupType The conjunction which joins multiple searchConditions in the searchConditionsList. Either 'Or' or 'And'. Defaults to 'And'.
	#' @param searchSortFieldNamesList The list of fields sort results by. Defaults to NULL (unsorted).
	#' @param searchSortFieldNamesDescendingList A list of T/F values corresponding to whether to sort each field in searchSortFieldNamesList in descending order. Defaults to F for each FieldName in searchSortFieldNamesList.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{listSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param page Results are paginated. The page of results to return. Default is 1.
	#' @param pageSize Results are paginated. The number of records per page to return. Default is 100,000 (essentially all records for most objects).
	#' @param flatten Whether to flatten results into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Staff Planning
	#' @return A list of BuildingPlanPositionDistributionSummaries
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	listBuildingPlanPositionDistributionSummaries <- function(searchConditionsList = NULL, PlanPositionDistributionIDFirst = F, PlanGroupID = F, PositionTypeID = F, BuildingID = F, FTE = F, fieldPaths = NULL, searchConditionsGroupType = "And", searchSortFieldNamesList = NULL, searchSortFieldNamesDescendingList = NULL, entityId = 1, schoolYearId = NULL, page = 1, pageSize = 100000, flatten = T, returnResponse = F){

		params <- as.list(environment())

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		listSkyObjects(module = "StaffPlanning", objectName = "BuildingPlanPositionDistributionSummary", searchFields = searchFields %>% append(fieldPaths), page = page, pageSize = pageSize, SearchConditionsList = searchConditionsList, SearchConditionsGroupType = searchConditionsGroupType, SearchSortFieldNamesList = searchSortFieldNamesList, SearchSortFieldNamesDescendingList = searchSortFieldNamesDescendingList, entityId = entityId, schoolYearId = schoolYearId, flatten = flatten, returnResponse = returnResponse)
	}

	#' Get a BuildingPlanPositionDistributionSummary
	#'
	#' This function returns a dataframe or json object of a BuildingPlanPositionDistributionSummary
	#' @param BuildingPlanPositionDistributionSummaryID The ID of the BuildingPlanPositionDistributionSummary to return.
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given BuildingPlanPositionDistributionSummary. Defaults to FALSE for all return fields which, for convenience, returns all fields for the BuildingPlanPositionDistributionSummary.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object listed as text. Run \code{\link{getSchemaForObjects}}('BuildingPlanPositionDistributionSummary') to get more field paths.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{listSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Staff Planning
	#' @return A dataframe or of BuildingPlanPositionDistributionSummary
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	getBuildingPlanPositionDistributionSummary <- function(BuildingPlanPositionDistributionSummaryID, PlanPositionDistributionIDFirst = F, PlanGroupID = F, PositionTypeID = F, BuildingID = F, FTE = F, fieldPaths = NULL, entityId = 1, schoolYearId = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment()) %>% purrr::keep(names(.) != "BuildingPlanPositionDistributionSummaryID")

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		getSkyObject(module = "StaffPlanning", objectName = "BuildingPlanPositionDistributionSummary", objectId = BuildingPlanPositionDistributionSummaryID, searchFields = searchFields, entityId = entityId, flatten = flatten, returnResponse = returnResponse)
	}

	#' Delete a BuildingPlanPositionDistributionSummary
	#'
	#' This function deletes a BuildingPlanPositionDistributionSummary
	#' @param BuildingPlanPositionDistributionSummaryID The ID of the BuildingPlanPositionDistributionSummary to delete
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{listSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Staff Planning
	#' @return The BuildingPlanPositionDistributionSummaryID of the deleted BuildingPlanPositionDistributionSummary.
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	deleteBuildingPlanPositionDistributionSummary <- function(BuildingPlanPositionDistributionSummaryID, ignoreWarnings = F, entityId = 1, schoolYearId = NULL, flatten = T, returnResponse = F){

		deleteSkyObject(module = "StaffPlanning", objectName = "BuildingPlanPositionDistributionSummary", objectId = BuildingPlanPositionDistributionSummaryID, ignoreWarnings = ignoreWarnings, entityId = entityId, flatten = flatten, returnResponse = returnResponse)
	}

	#' Create a BuildingPlanPositionDistributionSummary
	#'
	#' This function creates a BuildingPlanPositionDistributionSummary
	#' @param fieldNames The field values to give the created BuildingPlanPositionDistributionSummary. Each defaults to NULL.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{listSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Staff Planning
	#' @return A newly created BuildingPlanPositionDistributionSummary
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	createBuildingPlanPositionDistributionSummary <- function(PlanGroupID = NULL, PositionTypeID = NULL, BuildingID = NULL, entityId = 1, schoolYearId = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment())

		body <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper()) %>% purrr::compact()

		createSkyObject(module = "StaffPlanning", objectName = "BuildingPlanPositionDistributionSummary", body = list(DataObject = body), searchFields = append("BuildingPlanPositionDistributionSummaryID", body %>% names()), entityId = entityId, schoolYearId = schoolYearId, flatten = flatten, returnResponse = returnResponse)
	}

	#' Modify a BuildingPlanPositionDistributionSummary
	#'
	#' This function modifies a BuildingPlanPositionDistributionSummary
	#' @param fieldNames The field values to give the modified BuildingPlanPositionDistributionSummary. Each defaults to NULL.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{listSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Staff Planning
	#' @return The modified BuildingPlanPositionDistributionSummary
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	modifyBuildingPlanPositionDistributionSummary <- function(BuildingPlanPositionDistributionSummaryID, PlanGroupID = NULL, PositionTypeID = NULL, BuildingID = NULL, entityId = 1, schoolYearId = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment())

		body <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper()) %>% purrr::compact()

		modifySkyObject(module = "StaffPlanning", objectName = "BuildingPlanPositionDistributionSummary", objectId = BuildingPlanPositionDistributionSummaryID, body = list(DataObject = body), searchFields = append("BuildingPlanPositionDistributionSummaryID", body %>% names()), entityId = entityId, schoolYearId = schoolYearId, flatten = flatten, returnResponse = returnResponse)
	}

	#' List PlanPositionPayAccountCalculations
	#'
	#' This function returns a dataframe or json object of PlanPositionPayAccountCalculations
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given PlanPositionPayAccountCalculations. Defaults to FALSE for all return fields which, for convenience, returns all fields for the PlanPositionPayAccountCalculations.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object listed as text. Run \code{\link{getSchemaForObjects}}('PlanPositionPayAccountCalculation') to get more field paths.
	#' @param searchConditionsList A list of search conditions to filter results which are joined by the searchConditionsGroupType. Of the form {FieldName} {ConditionType} {SearchCondition}. For example, c('StudentID LessEqual 500', 'LastName Like Ander\%'). Run \code{\link{listSearchConditionTypes}} for a list of ConditionTypes. Defaults to NULL (unfiltered).
	#' @param searchConditionsGroupType The conjunction which joins multiple searchConditions in the searchConditionsList. Either 'Or' or 'And'. Defaults to 'And'.
	#' @param searchSortFieldNamesList The list of fields sort results by. Defaults to NULL (unsorted).
	#' @param searchSortFieldNamesDescendingList A list of T/F values corresponding to whether to sort each field in searchSortFieldNamesList in descending order. Defaults to F for each FieldName in searchSortFieldNamesList.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{listSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param page Results are paginated. The page of results to return. Default is 1.
	#' @param pageSize Results are paginated. The number of records per page to return. Default is 100,000 (essentially all records for most objects).
	#' @param flatten Whether to flatten results into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Staff Planning
	#' @return A list of PlanPositionPayAccountCalculations
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	listPlanPositionPayAccountCalculations <- function(searchConditionsList = NULL, PlanPositionPayAccountCalculationID = F, PlanPositionPayAccountDistributionID = F, AnnualAmount = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, fieldPaths = NULL, searchConditionsGroupType = "And", searchSortFieldNamesList = NULL, searchSortFieldNamesDescendingList = NULL, entityId = 1, schoolYearId = NULL, page = 1, pageSize = 100000, flatten = T, returnResponse = F){

		params <- as.list(environment())

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		listSkyObjects(module = "StaffPlanning", objectName = "PlanPositionPayAccountCalculation", searchFields = searchFields %>% append(fieldPaths), page = page, pageSize = pageSize, SearchConditionsList = searchConditionsList, SearchConditionsGroupType = searchConditionsGroupType, SearchSortFieldNamesList = searchSortFieldNamesList, SearchSortFieldNamesDescendingList = searchSortFieldNamesDescendingList, entityId = entityId, schoolYearId = schoolYearId, flatten = flatten, returnResponse = returnResponse)
	}

	#' Get a PlanPositionPayAccountCalculation
	#'
	#' This function returns a dataframe or json object of a PlanPositionPayAccountCalculation
	#' @param PlanPositionPayAccountCalculationID The ID of the PlanPositionPayAccountCalculation to return.
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given PlanPositionPayAccountCalculation. Defaults to FALSE for all return fields which, for convenience, returns all fields for the PlanPositionPayAccountCalculation.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object listed as text. Run \code{\link{getSchemaForObjects}}('PlanPositionPayAccountCalculation') to get more field paths.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{listSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Staff Planning
	#' @return A dataframe or of PlanPositionPayAccountCalculation
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	getPlanPositionPayAccountCalculation <- function(PlanPositionPayAccountCalculationID, PlanPositionPayAccountDistributionID = F, AnnualAmount = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, fieldPaths = NULL, entityId = 1, schoolYearId = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment()) %>% purrr::keep(names(.) != "PlanPositionPayAccountCalculationID")

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		getSkyObject(module = "StaffPlanning", objectName = "PlanPositionPayAccountCalculation", objectId = PlanPositionPayAccountCalculationID, searchFields = searchFields, entityId = entityId, flatten = flatten, returnResponse = returnResponse)
	}

	#' Delete a PlanPositionPayAccountCalculation
	#'
	#' This function deletes a PlanPositionPayAccountCalculation
	#' @param PlanPositionPayAccountCalculationID The ID of the PlanPositionPayAccountCalculation to delete
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{listSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Staff Planning
	#' @return The PlanPositionPayAccountCalculationID of the deleted PlanPositionPayAccountCalculation.
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	deletePlanPositionPayAccountCalculation <- function(PlanPositionPayAccountCalculationID, ignoreWarnings = F, entityId = 1, schoolYearId = NULL, flatten = T, returnResponse = F){

		deleteSkyObject(module = "StaffPlanning", objectName = "PlanPositionPayAccountCalculation", objectId = PlanPositionPayAccountCalculationID, ignoreWarnings = ignoreWarnings, entityId = entityId, flatten = flatten, returnResponse = returnResponse)
	}

	#' Create a PlanPositionPayAccountCalculation
	#'
	#' This function creates a PlanPositionPayAccountCalculation
	#' @param fieldNames The field values to give the created PlanPositionPayAccountCalculation. Each defaults to NULL.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{listSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Staff Planning
	#' @return A newly created PlanPositionPayAccountCalculation
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	createPlanPositionPayAccountCalculation <- function(PlanPositionPayAccountDistributionID = NULL, AnnualAmount = NULL, entityId = 1, schoolYearId = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment())

		body <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper()) %>% purrr::compact()

		createSkyObject(module = "StaffPlanning", objectName = "PlanPositionPayAccountCalculation", body = list(DataObject = body), searchFields = append("PlanPositionPayAccountCalculationID", body %>% names()), entityId = entityId, schoolYearId = schoolYearId, flatten = flatten, returnResponse = returnResponse)
	}

	#' Modify a PlanPositionPayAccountCalculation
	#'
	#' This function modifies a PlanPositionPayAccountCalculation
	#' @param fieldNames The field values to give the modified PlanPositionPayAccountCalculation. Each defaults to NULL.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{listSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Staff Planning
	#' @return The modified PlanPositionPayAccountCalculation
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	modifyPlanPositionPayAccountCalculation <- function(PlanPositionPayAccountCalculationID, PlanPositionPayAccountDistributionID = NULL, AnnualAmount = NULL, entityId = 1, schoolYearId = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment())

		body <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper()) %>% purrr::compact()

		modifySkyObject(module = "StaffPlanning", objectName = "PlanPositionPayAccountCalculation", objectId = PlanPositionPayAccountCalculationID, body = list(DataObject = body), searchFields = append("PlanPositionPayAccountCalculationID", body %>% names()), entityId = entityId, schoolYearId = schoolYearId, flatten = flatten, returnResponse = returnResponse)
	}

	#' List PlanPositionPayBenefits
	#'
	#' This function returns a dataframe or json object of PlanPositionPayBenefits
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given PlanPositionPayBenefits. Defaults to FALSE for all return fields which, for convenience, returns all fields for the PlanPositionPayBenefits.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object listed as text. Run \code{\link{getSchemaForObjects}}('PlanPositionPayBenefit') to get more field paths.
	#' @param searchConditionsList A list of search conditions to filter results which are joined by the searchConditionsGroupType. Of the form {FieldName} {ConditionType} {SearchCondition}. For example, c('StudentID LessEqual 500', 'LastName Like Ander\%'). Run \code{\link{listSearchConditionTypes}} for a list of ConditionTypes. Defaults to NULL (unfiltered).
	#' @param searchConditionsGroupType The conjunction which joins multiple searchConditions in the searchConditionsList. Either 'Or' or 'And'. Defaults to 'And'.
	#' @param searchSortFieldNamesList The list of fields sort results by. Defaults to NULL (unsorted).
	#' @param searchSortFieldNamesDescendingList A list of T/F values corresponding to whether to sort each field in searchSortFieldNamesList in descending order. Defaults to F for each FieldName in searchSortFieldNamesList.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{listSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param page Results are paginated. The page of results to return. Default is 1.
	#' @param pageSize Results are paginated. The number of records per page to return. Default is 100,000 (essentially all records for most objects).
	#' @param flatten Whether to flatten results into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Staff Planning
	#' @return A list of PlanPositionPayBenefits
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	listPlanPositionPayBenefits <- function(searchConditionsList = NULL, PlanPositionPayBenefitID = F, PlanPositionPayID = F, PlanPositionBenefitID = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, fieldPaths = NULL, searchConditionsGroupType = "And", searchSortFieldNamesList = NULL, searchSortFieldNamesDescendingList = NULL, entityId = 1, schoolYearId = NULL, page = 1, pageSize = 100000, flatten = T, returnResponse = F){

		params <- as.list(environment())

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		listSkyObjects(module = "StaffPlanning", objectName = "PlanPositionPayBenefit", searchFields = searchFields %>% append(fieldPaths), page = page, pageSize = pageSize, SearchConditionsList = searchConditionsList, SearchConditionsGroupType = searchConditionsGroupType, SearchSortFieldNamesList = searchSortFieldNamesList, SearchSortFieldNamesDescendingList = searchSortFieldNamesDescendingList, entityId = entityId, schoolYearId = schoolYearId, flatten = flatten, returnResponse = returnResponse)
	}

	#' Get a PlanPositionPayBenefit
	#'
	#' This function returns a dataframe or json object of a PlanPositionPayBenefit
	#' @param PlanPositionPayBenefitID The ID of the PlanPositionPayBenefit to return.
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given PlanPositionPayBenefit. Defaults to FALSE for all return fields which, for convenience, returns all fields for the PlanPositionPayBenefit.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object listed as text. Run \code{\link{getSchemaForObjects}}('PlanPositionPayBenefit') to get more field paths.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{listSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Staff Planning
	#' @return A dataframe or of PlanPositionPayBenefit
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	getPlanPositionPayBenefit <- function(PlanPositionPayBenefitID, PlanPositionPayID = F, PlanPositionBenefitID = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, fieldPaths = NULL, entityId = 1, schoolYearId = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment()) %>% purrr::keep(names(.) != "PlanPositionPayBenefitID")

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		getSkyObject(module = "StaffPlanning", objectName = "PlanPositionPayBenefit", objectId = PlanPositionPayBenefitID, searchFields = searchFields, entityId = entityId, flatten = flatten, returnResponse = returnResponse)
	}

	#' Delete a PlanPositionPayBenefit
	#'
	#' This function deletes a PlanPositionPayBenefit
	#' @param PlanPositionPayBenefitID The ID of the PlanPositionPayBenefit to delete
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{listSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Staff Planning
	#' @return The PlanPositionPayBenefitID of the deleted PlanPositionPayBenefit.
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	deletePlanPositionPayBenefit <- function(PlanPositionPayBenefitID, ignoreWarnings = F, entityId = 1, schoolYearId = NULL, flatten = T, returnResponse = F){

		deleteSkyObject(module = "StaffPlanning", objectName = "PlanPositionPayBenefit", objectId = PlanPositionPayBenefitID, ignoreWarnings = ignoreWarnings, entityId = entityId, flatten = flatten, returnResponse = returnResponse)
	}

	#' Create a PlanPositionPayBenefit
	#'
	#' This function creates a PlanPositionPayBenefit
	#' @param fieldNames The field values to give the created PlanPositionPayBenefit. Each defaults to NULL.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{listSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Staff Planning
	#' @return A newly created PlanPositionPayBenefit
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	createPlanPositionPayBenefit <- function(PlanPositionPayID = NULL, PlanPositionBenefitID = NULL, entityId = 1, schoolYearId = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment())

		body <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper()) %>% purrr::compact()

		createSkyObject(module = "StaffPlanning", objectName = "PlanPositionPayBenefit", body = list(DataObject = body), searchFields = append("PlanPositionPayBenefitID", body %>% names()), entityId = entityId, schoolYearId = schoolYearId, flatten = flatten, returnResponse = returnResponse)
	}

	#' Modify a PlanPositionPayBenefit
	#'
	#' This function modifies a PlanPositionPayBenefit
	#' @param fieldNames The field values to give the modified PlanPositionPayBenefit. Each defaults to NULL.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{listSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Staff Planning
	#' @return The modified PlanPositionPayBenefit
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	modifyPlanPositionPayBenefit <- function(PlanPositionPayBenefitID, PlanPositionPayID = NULL, PlanPositionBenefitID = NULL, entityId = 1, schoolYearId = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment())

		body <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper()) %>% purrr::compact()

		modifySkyObject(module = "StaffPlanning", objectName = "PlanPositionPayBenefit", objectId = PlanPositionPayBenefitID, body = list(DataObject = body), searchFields = append("PlanPositionPayBenefitID", body %>% names()), entityId = entityId, schoolYearId = schoolYearId, flatten = flatten, returnResponse = returnResponse)
	}

	#' List PlanPositionPayBenefitAccounts
	#'
	#' This function returns a dataframe or json object of PlanPositionPayBenefitAccounts
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given PlanPositionPayBenefitAccounts. Defaults to FALSE for all return fields which, for convenience, returns all fields for the PlanPositionPayBenefitAccounts.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object listed as text. Run \code{\link{getSchemaForObjects}}('PlanPositionPayBenefitAccount') to get more field paths.
	#' @param searchConditionsList A list of search conditions to filter results which are joined by the searchConditionsGroupType. Of the form {FieldName} {ConditionType} {SearchCondition}. For example, c('StudentID LessEqual 500', 'LastName Like Ander\%'). Run \code{\link{listSearchConditionTypes}} for a list of ConditionTypes. Defaults to NULL (unfiltered).
	#' @param searchConditionsGroupType The conjunction which joins multiple searchConditions in the searchConditionsList. Either 'Or' or 'And'. Defaults to 'And'.
	#' @param searchSortFieldNamesList The list of fields sort results by. Defaults to NULL (unsorted).
	#' @param searchSortFieldNamesDescendingList A list of T/F values corresponding to whether to sort each field in searchSortFieldNamesList in descending order. Defaults to F for each FieldName in searchSortFieldNamesList.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{listSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param page Results are paginated. The page of results to return. Default is 1.
	#' @param pageSize Results are paginated. The number of records per page to return. Default is 100,000 (essentially all records for most objects).
	#' @param flatten Whether to flatten results into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Staff Planning
	#' @return A list of PlanPositionPayBenefitAccounts
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	listPlanPositionPayBenefitAccounts <- function(searchConditionsList = NULL, PlanPositionPayBenefitAccountID = F, PlanPositionPayAccountDistributionID = F, PlanPositionPayBenefitID = F, AnnualAmount = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, AccountID = F, fieldPaths = NULL, searchConditionsGroupType = "And", searchSortFieldNamesList = NULL, searchSortFieldNamesDescendingList = NULL, entityId = 1, schoolYearId = NULL, page = 1, pageSize = 100000, flatten = T, returnResponse = F){

		params <- as.list(environment())

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		listSkyObjects(module = "StaffPlanning", objectName = "PlanPositionPayBenefitAccount", searchFields = searchFields %>% append(fieldPaths), page = page, pageSize = pageSize, SearchConditionsList = searchConditionsList, SearchConditionsGroupType = searchConditionsGroupType, SearchSortFieldNamesList = searchSortFieldNamesList, SearchSortFieldNamesDescendingList = searchSortFieldNamesDescendingList, entityId = entityId, schoolYearId = schoolYearId, flatten = flatten, returnResponse = returnResponse)
	}

	#' Get a PlanPositionPayBenefitAccount
	#'
	#' This function returns a dataframe or json object of a PlanPositionPayBenefitAccount
	#' @param PlanPositionPayBenefitAccountID The ID of the PlanPositionPayBenefitAccount to return.
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given PlanPositionPayBenefitAccount. Defaults to FALSE for all return fields which, for convenience, returns all fields for the PlanPositionPayBenefitAccount.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object listed as text. Run \code{\link{getSchemaForObjects}}('PlanPositionPayBenefitAccount') to get more field paths.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{listSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Staff Planning
	#' @return A dataframe or of PlanPositionPayBenefitAccount
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	getPlanPositionPayBenefitAccount <- function(PlanPositionPayBenefitAccountID, PlanPositionPayAccountDistributionID = F, PlanPositionPayBenefitID = F, AnnualAmount = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, AccountID = F, fieldPaths = NULL, entityId = 1, schoolYearId = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment()) %>% purrr::keep(names(.) != "PlanPositionPayBenefitAccountID")

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		getSkyObject(module = "StaffPlanning", objectName = "PlanPositionPayBenefitAccount", objectId = PlanPositionPayBenefitAccountID, searchFields = searchFields, entityId = entityId, flatten = flatten, returnResponse = returnResponse)
	}

	#' Delete a PlanPositionPayBenefitAccount
	#'
	#' This function deletes a PlanPositionPayBenefitAccount
	#' @param PlanPositionPayBenefitAccountID The ID of the PlanPositionPayBenefitAccount to delete
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{listSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Staff Planning
	#' @return The PlanPositionPayBenefitAccountID of the deleted PlanPositionPayBenefitAccount.
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	deletePlanPositionPayBenefitAccount <- function(PlanPositionPayBenefitAccountID, ignoreWarnings = F, entityId = 1, schoolYearId = NULL, flatten = T, returnResponse = F){

		deleteSkyObject(module = "StaffPlanning", objectName = "PlanPositionPayBenefitAccount", objectId = PlanPositionPayBenefitAccountID, ignoreWarnings = ignoreWarnings, entityId = entityId, flatten = flatten, returnResponse = returnResponse)
	}

	#' Create a PlanPositionPayBenefitAccount
	#'
	#' This function creates a PlanPositionPayBenefitAccount
	#' @param fieldNames The field values to give the created PlanPositionPayBenefitAccount. Each defaults to NULL.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{listSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Staff Planning
	#' @return A newly created PlanPositionPayBenefitAccount
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	createPlanPositionPayBenefitAccount <- function(PlanPositionPayAccountDistributionID = NULL, PlanPositionPayBenefitID = NULL, AnnualAmount = NULL, AccountID = NULL, entityId = 1, schoolYearId = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment())

		body <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper()) %>% purrr::compact()

		createSkyObject(module = "StaffPlanning", objectName = "PlanPositionPayBenefitAccount", body = list(DataObject = body), searchFields = append("PlanPositionPayBenefitAccountID", body %>% names()), entityId = entityId, schoolYearId = schoolYearId, flatten = flatten, returnResponse = returnResponse)
	}

	#' Modify a PlanPositionPayBenefitAccount
	#'
	#' This function modifies a PlanPositionPayBenefitAccount
	#' @param fieldNames The field values to give the modified PlanPositionPayBenefitAccount. Each defaults to NULL.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{listSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Staff Planning
	#' @return The modified PlanPositionPayBenefitAccount
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	modifyPlanPositionPayBenefitAccount <- function(PlanPositionPayBenefitAccountID, PlanPositionPayAccountDistributionID = NULL, PlanPositionPayBenefitID = NULL, AnnualAmount = NULL, AccountID = NULL, entityId = 1, schoolYearId = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment())

		body <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper()) %>% purrr::compact()

		modifySkyObject(module = "StaffPlanning", objectName = "PlanPositionPayBenefitAccount", objectId = PlanPositionPayBenefitAccountID, body = list(DataObject = body), searchFields = append("PlanPositionPayBenefitAccountID", body %>% names()), entityId = entityId, schoolYearId = schoolYearId, flatten = flatten, returnResponse = returnResponse)
	}

	#' List PlanPositionPays
	#'
	#' This function returns a dataframe or json object of PlanPositionPays
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given PlanPositionPays. Defaults to FALSE for all return fields which, for convenience, returns all fields for the PlanPositionPays.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object listed as text. Run \code{\link{getSchemaForObjects}}('PlanPositionPay') to get more field paths.
	#' @param searchConditionsList A list of search conditions to filter results which are joined by the searchConditionsGroupType. Of the form {FieldName} {ConditionType} {SearchCondition}. For example, c('StudentID LessEqual 500', 'LastName Like Ander\%'). Run \code{\link{listSearchConditionTypes}} for a list of ConditionTypes. Defaults to NULL (unfiltered).
	#' @param searchConditionsGroupType The conjunction which joins multiple searchConditions in the searchConditionsList. Either 'Or' or 'And'. Defaults to 'And'.
	#' @param searchSortFieldNamesList The list of fields sort results by. Defaults to NULL (unsorted).
	#' @param searchSortFieldNamesDescendingList A list of T/F values corresponding to whether to sort each field in searchSortFieldNamesList in descending order. Defaults to F for each FieldName in searchSortFieldNamesList.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{listSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param page Results are paginated. The page of results to return. Default is 1.
	#' @param pageSize Results are paginated. The number of records per page to return. Default is 100,000 (essentially all records for most objects).
	#' @param flatten Whether to flatten results into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Staff Planning
	#' @return A list of PlanPositionPays
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	listPlanPositionPays <- function(searchConditionsList = NULL, PlanPositionPayID = F, PlanPositionID = F, PayTypeID = F, Type = F, StipendAmount = F, AccountDistributionString = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, TotalAnnualAmountCalculated = F, PayScheduleID = F, AssignmentPayTypeIDOriginal = F, TotalAmount = F, fieldPaths = NULL, searchConditionsGroupType = "And", searchSortFieldNamesList = NULL, searchSortFieldNamesDescendingList = NULL, entityId = 1, schoolYearId = NULL, page = 1, pageSize = 100000, flatten = T, returnResponse = F){

		params <- as.list(environment())

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		listSkyObjects(module = "StaffPlanning", objectName = "PlanPositionPay", searchFields = searchFields %>% append(fieldPaths), page = page, pageSize = pageSize, SearchConditionsList = searchConditionsList, SearchConditionsGroupType = searchConditionsGroupType, SearchSortFieldNamesList = searchSortFieldNamesList, SearchSortFieldNamesDescendingList = searchSortFieldNamesDescendingList, entityId = entityId, schoolYearId = schoolYearId, flatten = flatten, returnResponse = returnResponse)
	}

	#' Get a PlanPositionPay
	#'
	#' This function returns a dataframe or json object of a PlanPositionPay
	#' @param PlanPositionPayID The ID of the PlanPositionPay to return.
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given PlanPositionPay. Defaults to FALSE for all return fields which, for convenience, returns all fields for the PlanPositionPay.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object listed as text. Run \code{\link{getSchemaForObjects}}('PlanPositionPay') to get more field paths.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{listSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Staff Planning
	#' @return A dataframe or of PlanPositionPay
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	getPlanPositionPay <- function(PlanPositionPayID, PlanPositionID = F, PayTypeID = F, Type = F, StipendAmount = F, AccountDistributionString = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, TotalAnnualAmountCalculated = F, PayScheduleID = F, AssignmentPayTypeIDOriginal = F, TotalAmount = F, fieldPaths = NULL, entityId = 1, schoolYearId = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment()) %>% purrr::keep(names(.) != "PlanPositionPayID")

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		getSkyObject(module = "StaffPlanning", objectName = "PlanPositionPay", objectId = PlanPositionPayID, searchFields = searchFields, entityId = entityId, flatten = flatten, returnResponse = returnResponse)
	}

	#' Delete a PlanPositionPay
	#'
	#' This function deletes a PlanPositionPay
	#' @param PlanPositionPayID The ID of the PlanPositionPay to delete
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{listSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Staff Planning
	#' @return The PlanPositionPayID of the deleted PlanPositionPay.
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	deletePlanPositionPay <- function(PlanPositionPayID, ignoreWarnings = F, entityId = 1, schoolYearId = NULL, flatten = T, returnResponse = F){

		deleteSkyObject(module = "StaffPlanning", objectName = "PlanPositionPay", objectId = PlanPositionPayID, ignoreWarnings = ignoreWarnings, entityId = entityId, flatten = flatten, returnResponse = returnResponse)
	}

	#' Create a PlanPositionPay
	#'
	#' This function creates a PlanPositionPay
	#' @param fieldNames The field values to give the created PlanPositionPay. Each defaults to NULL.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{listSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Staff Planning
	#' @return A newly created PlanPositionPay
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	createPlanPositionPay <- function(PlanPositionID = NULL, PayTypeID = NULL, Type = NULL, StipendAmount = NULL, AccountDistributionString = NULL, PayScheduleID = NULL, AssignmentPayTypeIDOriginal = NULL, entityId = 1, schoolYearId = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment())

		body <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper()) %>% purrr::compact()

		createSkyObject(module = "StaffPlanning", objectName = "PlanPositionPay", body = list(DataObject = body), searchFields = append("PlanPositionPayID", body %>% names()), entityId = entityId, schoolYearId = schoolYearId, flatten = flatten, returnResponse = returnResponse)
	}

	#' Modify a PlanPositionPay
	#'
	#' This function modifies a PlanPositionPay
	#' @param fieldNames The field values to give the modified PlanPositionPay. Each defaults to NULL.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{listSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Staff Planning
	#' @return The modified PlanPositionPay
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	modifyPlanPositionPay <- function(PlanPositionPayID, PlanPositionID = NULL, PayTypeID = NULL, Type = NULL, StipendAmount = NULL, AccountDistributionString = NULL, PayScheduleID = NULL, AssignmentPayTypeIDOriginal = NULL, entityId = 1, schoolYearId = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment())

		body <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper()) %>% purrr::compact()

		modifySkyObject(module = "StaffPlanning", objectName = "PlanPositionPay", objectId = PlanPositionPayID, body = list(DataObject = body), searchFields = append("PlanPositionPayID", body %>% names()), entityId = entityId, schoolYearId = schoolYearId, flatten = flatten, returnResponse = returnResponse)
	}

	#' List PlanPositionPayAccountDistributions
	#'
	#' This function returns a dataframe or json object of PlanPositionPayAccountDistributions
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given PlanPositionPayAccountDistributions. Defaults to FALSE for all return fields which, for convenience, returns all fields for the PlanPositionPayAccountDistributions.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object listed as text. Run \code{\link{getSchemaForObjects}}('PlanPositionPayAccountDistribution') to get more field paths.
	#' @param searchConditionsList A list of search conditions to filter results which are joined by the searchConditionsGroupType. Of the form {FieldName} {ConditionType} {SearchCondition}. For example, c('StudentID LessEqual 500', 'LastName Like Ander\%'). Run \code{\link{listSearchConditionTypes}} for a list of ConditionTypes. Defaults to NULL (unfiltered).
	#' @param searchConditionsGroupType The conjunction which joins multiple searchConditions in the searchConditionsList. Either 'Or' or 'And'. Defaults to 'And'.
	#' @param searchSortFieldNamesList The list of fields sort results by. Defaults to NULL (unsorted).
	#' @param searchSortFieldNamesDescendingList A list of T/F values corresponding to whether to sort each field in searchSortFieldNamesList in descending order. Defaults to F for each FieldName in searchSortFieldNamesList.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{listSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param page Results are paginated. The page of results to return. Default is 1.
	#' @param pageSize Results are paginated. The number of records per page to return. Default is 100,000 (essentially all records for most objects).
	#' @param flatten Whether to flatten results into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Staff Planning
	#' @return A list of PlanPositionPayAccountDistributions
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	listPlanPositionPayAccountDistributions <- function(searchConditionsList = NULL, PlanPositionPayAccountDistributionID = F, PlanPositionPayID = F, AccountID = F, DistributionPercent = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, fieldPaths = NULL, searchConditionsGroupType = "And", searchSortFieldNamesList = NULL, searchSortFieldNamesDescendingList = NULL, entityId = 1, schoolYearId = NULL, page = 1, pageSize = 100000, flatten = T, returnResponse = F){

		params <- as.list(environment())

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		listSkyObjects(module = "StaffPlanning", objectName = "PlanPositionPayAccountDistribution", searchFields = searchFields %>% append(fieldPaths), page = page, pageSize = pageSize, SearchConditionsList = searchConditionsList, SearchConditionsGroupType = searchConditionsGroupType, SearchSortFieldNamesList = searchSortFieldNamesList, SearchSortFieldNamesDescendingList = searchSortFieldNamesDescendingList, entityId = entityId, schoolYearId = schoolYearId, flatten = flatten, returnResponse = returnResponse)
	}

	#' Get a PlanPositionPayAccountDistribution
	#'
	#' This function returns a dataframe or json object of a PlanPositionPayAccountDistribution
	#' @param PlanPositionPayAccountDistributionID The ID of the PlanPositionPayAccountDistribution to return.
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given PlanPositionPayAccountDistribution. Defaults to FALSE for all return fields which, for convenience, returns all fields for the PlanPositionPayAccountDistribution.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object listed as text. Run \code{\link{getSchemaForObjects}}('PlanPositionPayAccountDistribution') to get more field paths.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{listSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Staff Planning
	#' @return A dataframe or of PlanPositionPayAccountDistribution
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	getPlanPositionPayAccountDistribution <- function(PlanPositionPayAccountDistributionID, PlanPositionPayID = F, AccountID = F, DistributionPercent = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, fieldPaths = NULL, entityId = 1, schoolYearId = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment()) %>% purrr::keep(names(.) != "PlanPositionPayAccountDistributionID")

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		getSkyObject(module = "StaffPlanning", objectName = "PlanPositionPayAccountDistribution", objectId = PlanPositionPayAccountDistributionID, searchFields = searchFields, entityId = entityId, flatten = flatten, returnResponse = returnResponse)
	}

	#' Delete a PlanPositionPayAccountDistribution
	#'
	#' This function deletes a PlanPositionPayAccountDistribution
	#' @param PlanPositionPayAccountDistributionID The ID of the PlanPositionPayAccountDistribution to delete
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{listSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Staff Planning
	#' @return The PlanPositionPayAccountDistributionID of the deleted PlanPositionPayAccountDistribution.
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	deletePlanPositionPayAccountDistribution <- function(PlanPositionPayAccountDistributionID, ignoreWarnings = F, entityId = 1, schoolYearId = NULL, flatten = T, returnResponse = F){

		deleteSkyObject(module = "StaffPlanning", objectName = "PlanPositionPayAccountDistribution", objectId = PlanPositionPayAccountDistributionID, ignoreWarnings = ignoreWarnings, entityId = entityId, flatten = flatten, returnResponse = returnResponse)
	}

	#' Create a PlanPositionPayAccountDistribution
	#'
	#' This function creates a PlanPositionPayAccountDistribution
	#' @param fieldNames The field values to give the created PlanPositionPayAccountDistribution. Each defaults to NULL.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{listSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Staff Planning
	#' @return A newly created PlanPositionPayAccountDistribution
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	createPlanPositionPayAccountDistribution <- function(PlanPositionPayID = NULL, AccountID = NULL, DistributionPercent = NULL, entityId = 1, schoolYearId = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment())

		body <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper()) %>% purrr::compact()

		createSkyObject(module = "StaffPlanning", objectName = "PlanPositionPayAccountDistribution", body = list(DataObject = body), searchFields = append("PlanPositionPayAccountDistributionID", body %>% names()), entityId = entityId, schoolYearId = schoolYearId, flatten = flatten, returnResponse = returnResponse)
	}

	#' Modify a PlanPositionPayAccountDistribution
	#'
	#' This function modifies a PlanPositionPayAccountDistribution
	#' @param fieldNames The field values to give the modified PlanPositionPayAccountDistribution. Each defaults to NULL.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{listSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Staff Planning
	#' @return The modified PlanPositionPayAccountDistribution
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	modifyPlanPositionPayAccountDistribution <- function(PlanPositionPayAccountDistributionID, PlanPositionPayID = NULL, AccountID = NULL, DistributionPercent = NULL, entityId = 1, schoolYearId = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment())

		body <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper()) %>% purrr::compact()

		modifySkyObject(module = "StaffPlanning", objectName = "PlanPositionPayAccountDistribution", objectId = PlanPositionPayAccountDistributionID, body = list(DataObject = body), searchFields = append("PlanPositionPayAccountDistributionID", body %>% names()), entityId = entityId, schoolYearId = schoolYearId, flatten = flatten, returnResponse = returnResponse)
	}

	#' List PlanPositionBenefits
	#'
	#' This function returns a dataframe or json object of PlanPositionBenefits
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given PlanPositionBenefits. Defaults to FALSE for all return fields which, for convenience, returns all fields for the PlanPositionBenefits.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object listed as text. Run \code{\link{getSchemaForObjects}}('PlanPositionBenefit') to get more field paths.
	#' @param searchConditionsList A list of search conditions to filter results which are joined by the searchConditionsGroupType. Of the form {FieldName} {ConditionType} {SearchCondition}. For example, c('StudentID LessEqual 500', 'LastName Like Ander\%'). Run \code{\link{listSearchConditionTypes}} for a list of ConditionTypes. Defaults to NULL (unfiltered).
	#' @param searchConditionsGroupType The conjunction which joins multiple searchConditions in the searchConditionsList. Either 'Or' or 'And'. Defaults to 'And'.
	#' @param searchSortFieldNamesList The list of fields sort results by. Defaults to NULL (unsorted).
	#' @param searchSortFieldNamesDescendingList A list of T/F values corresponding to whether to sort each field in searchSortFieldNamesList in descending order. Defaults to F for each FieldName in searchSortFieldNamesList.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{listSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param page Results are paginated. The page of results to return. Default is 1.
	#' @param pageSize Results are paginated. The number of records per page to return. Default is 100,000 (essentially all records for most objects).
	#' @param flatten Whether to flatten results into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Staff Planning
	#' @return A list of PlanPositionBenefits
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	listPlanPositionBenefits <- function(searchConditionsList = NULL, PlanPositionBenefitID = F, PlanPositionID = F, BenefitID = F, CalculationType = F, Value = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, TotalAnnualAmountCalculated = F, fieldPaths = NULL, searchConditionsGroupType = "And", searchSortFieldNamesList = NULL, searchSortFieldNamesDescendingList = NULL, entityId = 1, schoolYearId = NULL, page = 1, pageSize = 100000, flatten = T, returnResponse = F){

		params <- as.list(environment())

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		listSkyObjects(module = "StaffPlanning", objectName = "PlanPositionBenefit", searchFields = searchFields %>% append(fieldPaths), page = page, pageSize = pageSize, SearchConditionsList = searchConditionsList, SearchConditionsGroupType = searchConditionsGroupType, SearchSortFieldNamesList = searchSortFieldNamesList, SearchSortFieldNamesDescendingList = searchSortFieldNamesDescendingList, entityId = entityId, schoolYearId = schoolYearId, flatten = flatten, returnResponse = returnResponse)
	}

	#' Get a PlanPositionBenefit
	#'
	#' This function returns a dataframe or json object of a PlanPositionBenefit
	#' @param PlanPositionBenefitID The ID of the PlanPositionBenefit to return.
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given PlanPositionBenefit. Defaults to FALSE for all return fields which, for convenience, returns all fields for the PlanPositionBenefit.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object listed as text. Run \code{\link{getSchemaForObjects}}('PlanPositionBenefit') to get more field paths.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{listSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Staff Planning
	#' @return A dataframe or of PlanPositionBenefit
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	getPlanPositionBenefit <- function(PlanPositionBenefitID, PlanPositionID = F, BenefitID = F, CalculationType = F, Value = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, TotalAnnualAmountCalculated = F, fieldPaths = NULL, entityId = 1, schoolYearId = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment()) %>% purrr::keep(names(.) != "PlanPositionBenefitID")

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		getSkyObject(module = "StaffPlanning", objectName = "PlanPositionBenefit", objectId = PlanPositionBenefitID, searchFields = searchFields, entityId = entityId, flatten = flatten, returnResponse = returnResponse)
	}

	#' Delete a PlanPositionBenefit
	#'
	#' This function deletes a PlanPositionBenefit
	#' @param PlanPositionBenefitID The ID of the PlanPositionBenefit to delete
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{listSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Staff Planning
	#' @return The PlanPositionBenefitID of the deleted PlanPositionBenefit.
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	deletePlanPositionBenefit <- function(PlanPositionBenefitID, ignoreWarnings = F, entityId = 1, schoolYearId = NULL, flatten = T, returnResponse = F){

		deleteSkyObject(module = "StaffPlanning", objectName = "PlanPositionBenefit", objectId = PlanPositionBenefitID, ignoreWarnings = ignoreWarnings, entityId = entityId, flatten = flatten, returnResponse = returnResponse)
	}

	#' Create a PlanPositionBenefit
	#'
	#' This function creates a PlanPositionBenefit
	#' @param fieldNames The field values to give the created PlanPositionBenefit. Each defaults to NULL.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{listSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Staff Planning
	#' @return A newly created PlanPositionBenefit
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	createPlanPositionBenefit <- function(PlanPositionID = NULL, BenefitID = NULL, CalculationType = NULL, Value = NULL, entityId = 1, schoolYearId = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment())

		body <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper()) %>% purrr::compact()

		createSkyObject(module = "StaffPlanning", objectName = "PlanPositionBenefit", body = list(DataObject = body), searchFields = append("PlanPositionBenefitID", body %>% names()), entityId = entityId, schoolYearId = schoolYearId, flatten = flatten, returnResponse = returnResponse)
	}

	#' Modify a PlanPositionBenefit
	#'
	#' This function modifies a PlanPositionBenefit
	#' @param fieldNames The field values to give the modified PlanPositionBenefit. Each defaults to NULL.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{listSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Staff Planning
	#' @return The modified PlanPositionBenefit
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	modifyPlanPositionBenefit <- function(PlanPositionBenefitID, PlanPositionID = NULL, BenefitID = NULL, CalculationType = NULL, Value = NULL, entityId = 1, schoolYearId = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment())

		body <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper()) %>% purrr::compact()

		modifySkyObject(module = "StaffPlanning", objectName = "PlanPositionBenefit", objectId = PlanPositionBenefitID, body = list(DataObject = body), searchFields = append("PlanPositionBenefitID", body %>% names()), entityId = entityId, schoolYearId = schoolYearId, flatten = flatten, returnResponse = returnResponse)
	}

	#' List PlanPositionSupplements
	#'
	#' This function returns a dataframe or json object of PlanPositionSupplements
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given PlanPositionSupplements. Defaults to FALSE for all return fields which, for convenience, returns all fields for the PlanPositionSupplements.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object listed as text. Run \code{\link{getSchemaForObjects}}('PlanPositionSupplement') to get more field paths.
	#' @param searchConditionsList A list of search conditions to filter results which are joined by the searchConditionsGroupType. Of the form {FieldName} {ConditionType} {SearchCondition}. For example, c('StudentID LessEqual 500', 'LastName Like Ander\%'). Run \code{\link{listSearchConditionTypes}} for a list of ConditionTypes. Defaults to NULL (unfiltered).
	#' @param searchConditionsGroupType The conjunction which joins multiple searchConditions in the searchConditionsList. Either 'Or' or 'And'. Defaults to 'And'.
	#' @param searchSortFieldNamesList The list of fields sort results by. Defaults to NULL (unsorted).
	#' @param searchSortFieldNamesDescendingList A list of T/F values corresponding to whether to sort each field in searchSortFieldNamesList in descending order. Defaults to F for each FieldName in searchSortFieldNamesList.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{listSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param page Results are paginated. The page of results to return. Default is 1.
	#' @param pageSize Results are paginated. The number of records per page to return. Default is 100,000 (essentially all records for most objects).
	#' @param flatten Whether to flatten results into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Staff Planning
	#' @return A list of PlanPositionSupplements
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	listPlanPositionSupplements <- function(searchConditionsList = NULL, PlanPositionSupplementID = F, PlanPositionID = F, SupplementTypeID = F, Rate = F, AnnualPay = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, fieldPaths = NULL, searchConditionsGroupType = "And", searchSortFieldNamesList = NULL, searchSortFieldNamesDescendingList = NULL, entityId = 1, schoolYearId = NULL, page = 1, pageSize = 100000, flatten = T, returnResponse = F){

		params <- as.list(environment())

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		listSkyObjects(module = "StaffPlanning", objectName = "PlanPositionSupplement", searchFields = searchFields %>% append(fieldPaths), page = page, pageSize = pageSize, SearchConditionsList = searchConditionsList, SearchConditionsGroupType = searchConditionsGroupType, SearchSortFieldNamesList = searchSortFieldNamesList, SearchSortFieldNamesDescendingList = searchSortFieldNamesDescendingList, entityId = entityId, schoolYearId = schoolYearId, flatten = flatten, returnResponse = returnResponse)
	}

	#' Get a PlanPositionSupplement
	#'
	#' This function returns a dataframe or json object of a PlanPositionSupplement
	#' @param PlanPositionSupplementID The ID of the PlanPositionSupplement to return.
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given PlanPositionSupplement. Defaults to FALSE for all return fields which, for convenience, returns all fields for the PlanPositionSupplement.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object listed as text. Run \code{\link{getSchemaForObjects}}('PlanPositionSupplement') to get more field paths.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{listSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Staff Planning
	#' @return A dataframe or of PlanPositionSupplement
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	getPlanPositionSupplement <- function(PlanPositionSupplementID, PlanPositionID = F, SupplementTypeID = F, Rate = F, AnnualPay = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, fieldPaths = NULL, entityId = 1, schoolYearId = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment()) %>% purrr::keep(names(.) != "PlanPositionSupplementID")

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		getSkyObject(module = "StaffPlanning", objectName = "PlanPositionSupplement", objectId = PlanPositionSupplementID, searchFields = searchFields, entityId = entityId, flatten = flatten, returnResponse = returnResponse)
	}

	#' Delete a PlanPositionSupplement
	#'
	#' This function deletes a PlanPositionSupplement
	#' @param PlanPositionSupplementID The ID of the PlanPositionSupplement to delete
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{listSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Staff Planning
	#' @return The PlanPositionSupplementID of the deleted PlanPositionSupplement.
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	deletePlanPositionSupplement <- function(PlanPositionSupplementID, ignoreWarnings = F, entityId = 1, schoolYearId = NULL, flatten = T, returnResponse = F){

		deleteSkyObject(module = "StaffPlanning", objectName = "PlanPositionSupplement", objectId = PlanPositionSupplementID, ignoreWarnings = ignoreWarnings, entityId = entityId, flatten = flatten, returnResponse = returnResponse)
	}

	#' Create a PlanPositionSupplement
	#'
	#' This function creates a PlanPositionSupplement
	#' @param fieldNames The field values to give the created PlanPositionSupplement. Each defaults to NULL.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{listSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Staff Planning
	#' @return A newly created PlanPositionSupplement
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	createPlanPositionSupplement <- function(PlanPositionID = NULL, SupplementTypeID = NULL, Rate = NULL, AnnualPay = NULL, entityId = 1, schoolYearId = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment())

		body <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper()) %>% purrr::compact()

		createSkyObject(module = "StaffPlanning", objectName = "PlanPositionSupplement", body = list(DataObject = body), searchFields = append("PlanPositionSupplementID", body %>% names()), entityId = entityId, schoolYearId = schoolYearId, flatten = flatten, returnResponse = returnResponse)
	}

	#' Modify a PlanPositionSupplement
	#'
	#' This function modifies a PlanPositionSupplement
	#' @param fieldNames The field values to give the modified PlanPositionSupplement. Each defaults to NULL.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{listSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Staff Planning
	#' @return The modified PlanPositionSupplement
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	modifyPlanPositionSupplement <- function(PlanPositionSupplementID, PlanPositionID = NULL, SupplementTypeID = NULL, Rate = NULL, AnnualPay = NULL, entityId = 1, schoolYearId = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment())

		body <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper()) %>% purrr::compact()

		modifySkyObject(module = "StaffPlanning", objectName = "PlanPositionSupplement", objectId = PlanPositionSupplementID, body = list(DataObject = body), searchFields = append("PlanPositionSupplementID", body %>% names()), entityId = entityId, schoolYearId = schoolYearId, flatten = flatten, returnResponse = returnResponse)
	}

	#' List TempPlanPositionPayAccountCalculations
	#'
	#' This function returns a dataframe or json object of TempPlanPositionPayAccountCalculations
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given TempPlanPositionPayAccountCalculations. Defaults to FALSE for all return fields which, for convenience, returns all fields for the TempPlanPositionPayAccountCalculations.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object listed as text. Run \code{\link{getSchemaForObjects}}('TempPlanPositionPayAccountCalculation') to get more field paths.
	#' @param searchConditionsList A list of search conditions to filter results which are joined by the searchConditionsGroupType. Of the form {FieldName} {ConditionType} {SearchCondition}. For example, c('StudentID LessEqual 500', 'LastName Like Ander\%'). Run \code{\link{listSearchConditionTypes}} for a list of ConditionTypes. Defaults to NULL (unfiltered).
	#' @param searchConditionsGroupType The conjunction which joins multiple searchConditions in the searchConditionsList. Either 'Or' or 'And'. Defaults to 'And'.
	#' @param searchSortFieldNamesList The list of fields sort results by. Defaults to NULL (unsorted).
	#' @param searchSortFieldNamesDescendingList A list of T/F values corresponding to whether to sort each field in searchSortFieldNamesList in descending order. Defaults to F for each FieldName in searchSortFieldNamesList.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{listSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param page Results are paginated. The page of results to return. Default is 1.
	#' @param pageSize Results are paginated. The number of records per page to return. Default is 100,000 (essentially all records for most objects).
	#' @param flatten Whether to flatten results into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Staff Planning
	#' @return A list of TempPlanPositionPayAccountCalculations
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	listTempPlanPositionPayAccountCalculations <- function(searchConditionsList = NULL, TempPlanPositionPayAccountCalculationID = F, PlanPositionPayAccountDistributionID = F, AnnualAmount = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, fieldPaths = NULL, searchConditionsGroupType = "And", searchSortFieldNamesList = NULL, searchSortFieldNamesDescendingList = NULL, entityId = 1, schoolYearId = NULL, page = 1, pageSize = 100000, flatten = T, returnResponse = F){

		params <- as.list(environment())

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		listSkyObjects(module = "StaffPlanning", objectName = "TempPlanPositionPayAccountCalculation", searchFields = searchFields %>% append(fieldPaths), page = page, pageSize = pageSize, SearchConditionsList = searchConditionsList, SearchConditionsGroupType = searchConditionsGroupType, SearchSortFieldNamesList = searchSortFieldNamesList, SearchSortFieldNamesDescendingList = searchSortFieldNamesDescendingList, entityId = entityId, schoolYearId = schoolYearId, flatten = flatten, returnResponse = returnResponse)
	}

	#' Get a TempPlanPositionPayAccountCalculation
	#'
	#' This function returns a dataframe or json object of a TempPlanPositionPayAccountCalculation
	#' @param TempPlanPositionPayAccountCalculationID The ID of the TempPlanPositionPayAccountCalculation to return.
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given TempPlanPositionPayAccountCalculation. Defaults to FALSE for all return fields which, for convenience, returns all fields for the TempPlanPositionPayAccountCalculation.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object listed as text. Run \code{\link{getSchemaForObjects}}('TempPlanPositionPayAccountCalculation') to get more field paths.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{listSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Staff Planning
	#' @return A dataframe or of TempPlanPositionPayAccountCalculation
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	getTempPlanPositionPayAccountCalculation <- function(TempPlanPositionPayAccountCalculationID, PlanPositionPayAccountDistributionID = F, AnnualAmount = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, fieldPaths = NULL, entityId = 1, schoolYearId = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment()) %>% purrr::keep(names(.) != "TempPlanPositionPayAccountCalculationID")

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		getSkyObject(module = "StaffPlanning", objectName = "TempPlanPositionPayAccountCalculation", objectId = TempPlanPositionPayAccountCalculationID, searchFields = searchFields, entityId = entityId, flatten = flatten, returnResponse = returnResponse)
	}

	#' Delete a TempPlanPositionPayAccountCalculation
	#'
	#' This function deletes a TempPlanPositionPayAccountCalculation
	#' @param TempPlanPositionPayAccountCalculationID The ID of the TempPlanPositionPayAccountCalculation to delete
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{listSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Staff Planning
	#' @return The TempPlanPositionPayAccountCalculationID of the deleted TempPlanPositionPayAccountCalculation.
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	deleteTempPlanPositionPayAccountCalculation <- function(TempPlanPositionPayAccountCalculationID, ignoreWarnings = F, entityId = 1, schoolYearId = NULL, flatten = T, returnResponse = F){

		deleteSkyObject(module = "StaffPlanning", objectName = "TempPlanPositionPayAccountCalculation", objectId = TempPlanPositionPayAccountCalculationID, ignoreWarnings = ignoreWarnings, entityId = entityId, flatten = flatten, returnResponse = returnResponse)
	}

	#' Create a TempPlanPositionPayAccountCalculation
	#'
	#' This function creates a TempPlanPositionPayAccountCalculation
	#' @param fieldNames The field values to give the created TempPlanPositionPayAccountCalculation. Each defaults to NULL.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{listSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Staff Planning
	#' @return A newly created TempPlanPositionPayAccountCalculation
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	createTempPlanPositionPayAccountCalculation <- function(PlanPositionPayAccountDistributionID = NULL, AnnualAmount = NULL, entityId = 1, schoolYearId = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment())

		body <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper()) %>% purrr::compact()

		createSkyObject(module = "StaffPlanning", objectName = "TempPlanPositionPayAccountCalculation", body = list(DataObject = body), searchFields = append("TempPlanPositionPayAccountCalculationID", body %>% names()), entityId = entityId, schoolYearId = schoolYearId, flatten = flatten, returnResponse = returnResponse)
	}

	#' Modify a TempPlanPositionPayAccountCalculation
	#'
	#' This function modifies a TempPlanPositionPayAccountCalculation
	#' @param fieldNames The field values to give the modified TempPlanPositionPayAccountCalculation. Each defaults to NULL.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{listSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Staff Planning
	#' @return The modified TempPlanPositionPayAccountCalculation
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	modifyTempPlanPositionPayAccountCalculation <- function(TempPlanPositionPayAccountCalculationID, PlanPositionPayAccountDistributionID = NULL, AnnualAmount = NULL, entityId = 1, schoolYearId = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment())

		body <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper()) %>% purrr::compact()

		modifySkyObject(module = "StaffPlanning", objectName = "TempPlanPositionPayAccountCalculation", objectId = TempPlanPositionPayAccountCalculationID, body = list(DataObject = body), searchFields = append("TempPlanPositionPayAccountCalculationID", body %>% names()), entityId = entityId, schoolYearId = schoolYearId, flatten = flatten, returnResponse = returnResponse)
	}

	#' List TempPlanPositionPayBenefitAccounts
	#'
	#' This function returns a dataframe or json object of TempPlanPositionPayBenefitAccounts
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given TempPlanPositionPayBenefitAccounts. Defaults to FALSE for all return fields which, for convenience, returns all fields for the TempPlanPositionPayBenefitAccounts.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object listed as text. Run \code{\link{getSchemaForObjects}}('TempPlanPositionPayBenefitAccount') to get more field paths.
	#' @param searchConditionsList A list of search conditions to filter results which are joined by the searchConditionsGroupType. Of the form {FieldName} {ConditionType} {SearchCondition}. For example, c('StudentID LessEqual 500', 'LastName Like Ander\%'). Run \code{\link{listSearchConditionTypes}} for a list of ConditionTypes. Defaults to NULL (unfiltered).
	#' @param searchConditionsGroupType The conjunction which joins multiple searchConditions in the searchConditionsList. Either 'Or' or 'And'. Defaults to 'And'.
	#' @param searchSortFieldNamesList The list of fields sort results by. Defaults to NULL (unsorted).
	#' @param searchSortFieldNamesDescendingList A list of T/F values corresponding to whether to sort each field in searchSortFieldNamesList in descending order. Defaults to F for each FieldName in searchSortFieldNamesList.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{listSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param page Results are paginated. The page of results to return. Default is 1.
	#' @param pageSize Results are paginated. The number of records per page to return. Default is 100,000 (essentially all records for most objects).
	#' @param flatten Whether to flatten results into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Staff Planning
	#' @return A list of TempPlanPositionPayBenefitAccounts
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	listTempPlanPositionPayBenefitAccounts <- function(searchConditionsList = NULL, TempPlanPositionPayBenefitAccountID = F, PlanPositionPayAccountDistributionID = F, PlanPositionPayBenefitID = F, AccountID = F, DisplayAccount = F, AnnualAmount = F, IsAccountPreviouslyCreated = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, TempPlanPositionPayBenefitID = F, fieldPaths = NULL, searchConditionsGroupType = "And", searchSortFieldNamesList = NULL, searchSortFieldNamesDescendingList = NULL, entityId = 1, schoolYearId = NULL, page = 1, pageSize = 100000, flatten = T, returnResponse = F){

		params <- as.list(environment())

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		listSkyObjects(module = "StaffPlanning", objectName = "TempPlanPositionPayBenefitAccount", searchFields = searchFields %>% append(fieldPaths), page = page, pageSize = pageSize, SearchConditionsList = searchConditionsList, SearchConditionsGroupType = searchConditionsGroupType, SearchSortFieldNamesList = searchSortFieldNamesList, SearchSortFieldNamesDescendingList = searchSortFieldNamesDescendingList, entityId = entityId, schoolYearId = schoolYearId, flatten = flatten, returnResponse = returnResponse)
	}

	#' Get a TempPlanPositionPayBenefitAccount
	#'
	#' This function returns a dataframe or json object of a TempPlanPositionPayBenefitAccount
	#' @param TempPlanPositionPayBenefitAccountID The ID of the TempPlanPositionPayBenefitAccount to return.
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given TempPlanPositionPayBenefitAccount. Defaults to FALSE for all return fields which, for convenience, returns all fields for the TempPlanPositionPayBenefitAccount.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object listed as text. Run \code{\link{getSchemaForObjects}}('TempPlanPositionPayBenefitAccount') to get more field paths.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{listSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Staff Planning
	#' @return A dataframe or of TempPlanPositionPayBenefitAccount
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	getTempPlanPositionPayBenefitAccount <- function(TempPlanPositionPayBenefitAccountID, PlanPositionPayAccountDistributionID = F, PlanPositionPayBenefitID = F, AccountID = F, DisplayAccount = F, AnnualAmount = F, IsAccountPreviouslyCreated = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, TempPlanPositionPayBenefitID = F, fieldPaths = NULL, entityId = 1, schoolYearId = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment()) %>% purrr::keep(names(.) != "TempPlanPositionPayBenefitAccountID")

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		getSkyObject(module = "StaffPlanning", objectName = "TempPlanPositionPayBenefitAccount", objectId = TempPlanPositionPayBenefitAccountID, searchFields = searchFields, entityId = entityId, flatten = flatten, returnResponse = returnResponse)
	}

	#' Delete a TempPlanPositionPayBenefitAccount
	#'
	#' This function deletes a TempPlanPositionPayBenefitAccount
	#' @param TempPlanPositionPayBenefitAccountID The ID of the TempPlanPositionPayBenefitAccount to delete
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{listSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Staff Planning
	#' @return The TempPlanPositionPayBenefitAccountID of the deleted TempPlanPositionPayBenefitAccount.
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	deleteTempPlanPositionPayBenefitAccount <- function(TempPlanPositionPayBenefitAccountID, ignoreWarnings = F, entityId = 1, schoolYearId = NULL, flatten = T, returnResponse = F){

		deleteSkyObject(module = "StaffPlanning", objectName = "TempPlanPositionPayBenefitAccount", objectId = TempPlanPositionPayBenefitAccountID, ignoreWarnings = ignoreWarnings, entityId = entityId, flatten = flatten, returnResponse = returnResponse)
	}

	#' Create a TempPlanPositionPayBenefitAccount
	#'
	#' This function creates a TempPlanPositionPayBenefitAccount
	#' @param fieldNames The field values to give the created TempPlanPositionPayBenefitAccount. Each defaults to NULL.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{listSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Staff Planning
	#' @return A newly created TempPlanPositionPayBenefitAccount
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	createTempPlanPositionPayBenefitAccount <- function(PlanPositionPayAccountDistributionID = NULL, PlanPositionPayBenefitID = NULL, AccountID = NULL, DisplayAccount = NULL, AnnualAmount = NULL, IsAccountPreviouslyCreated = NULL, TempPlanPositionPayBenefitID = NULL, entityId = 1, schoolYearId = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment())

		body <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper()) %>% purrr::compact()

		createSkyObject(module = "StaffPlanning", objectName = "TempPlanPositionPayBenefitAccount", body = list(DataObject = body), searchFields = append("TempPlanPositionPayBenefitAccountID", body %>% names()), entityId = entityId, schoolYearId = schoolYearId, flatten = flatten, returnResponse = returnResponse)
	}

	#' Modify a TempPlanPositionPayBenefitAccount
	#'
	#' This function modifies a TempPlanPositionPayBenefitAccount
	#' @param fieldNames The field values to give the modified TempPlanPositionPayBenefitAccount. Each defaults to NULL.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{listSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Staff Planning
	#' @return The modified TempPlanPositionPayBenefitAccount
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	modifyTempPlanPositionPayBenefitAccount <- function(TempPlanPositionPayBenefitAccountID, PlanPositionPayAccountDistributionID = NULL, PlanPositionPayBenefitID = NULL, AccountID = NULL, DisplayAccount = NULL, AnnualAmount = NULL, IsAccountPreviouslyCreated = NULL, TempPlanPositionPayBenefitID = NULL, entityId = 1, schoolYearId = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment())

		body <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper()) %>% purrr::compact()

		modifySkyObject(module = "StaffPlanning", objectName = "TempPlanPositionPayBenefitAccount", objectId = TempPlanPositionPayBenefitAccountID, body = list(DataObject = body), searchFields = append("TempPlanPositionPayBenefitAccountID", body %>% names()), entityId = entityId, schoolYearId = schoolYearId, flatten = flatten, returnResponse = returnResponse)
	}

	#' List TempPlanPositionSupplements
	#'
	#' This function returns a dataframe or json object of TempPlanPositionSupplements
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given TempPlanPositionSupplements. Defaults to FALSE for all return fields which, for convenience, returns all fields for the TempPlanPositionSupplements.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object listed as text. Run \code{\link{getSchemaForObjects}}('TempPlanPositionSupplement') to get more field paths.
	#' @param searchConditionsList A list of search conditions to filter results which are joined by the searchConditionsGroupType. Of the form {FieldName} {ConditionType} {SearchCondition}. For example, c('StudentID LessEqual 500', 'LastName Like Ander\%'). Run \code{\link{listSearchConditionTypes}} for a list of ConditionTypes. Defaults to NULL (unfiltered).
	#' @param searchConditionsGroupType The conjunction which joins multiple searchConditions in the searchConditionsList. Either 'Or' or 'And'. Defaults to 'And'.
	#' @param searchSortFieldNamesList The list of fields sort results by. Defaults to NULL (unsorted).
	#' @param searchSortFieldNamesDescendingList A list of T/F values corresponding to whether to sort each field in searchSortFieldNamesList in descending order. Defaults to F for each FieldName in searchSortFieldNamesList.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{listSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param page Results are paginated. The page of results to return. Default is 1.
	#' @param pageSize Results are paginated. The number of records per page to return. Default is 100,000 (essentially all records for most objects).
	#' @param flatten Whether to flatten results into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Staff Planning
	#' @return A list of TempPlanPositionSupplements
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	listTempPlanPositionSupplements <- function(searchConditionsList = NULL, TempPlanPositionSupplementID = F, TempPlanPositionEmployeeID = F, SupplementTypeID = F, Rate = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, AnnualPay = F, PlanPositionID = F, PlanPositionSupplementID = F, SupplementTypeCodeDescription = F, PlanPositionOldAnnualPay = F, PlanPositionNewAnnualPay = F, PlanPositionPositionTypeCodeDescription = F, PlanPositionDistributionsBuildingCodes = F, PlanPositionDistributionsAssignmentTypeCodes = F, PlanPositionEmployeeName = F, PlanPositionEmployeeNumber = F, ErrorCount = F, OldSupplementTypeCodeDescription = F, SupplementTypeAmountTypeCode = F, OldSupplementTypeAmountTypeCode = F, OldRate = F, OldAnnualPay = F, fieldPaths = NULL, searchConditionsGroupType = "And", searchSortFieldNamesList = NULL, searchSortFieldNamesDescendingList = NULL, entityId = 1, schoolYearId = NULL, page = 1, pageSize = 100000, flatten = T, returnResponse = F){

		params <- as.list(environment())

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		listSkyObjects(module = "StaffPlanning", objectName = "TempPlanPositionSupplement", searchFields = searchFields %>% append(fieldPaths), page = page, pageSize = pageSize, SearchConditionsList = searchConditionsList, SearchConditionsGroupType = searchConditionsGroupType, SearchSortFieldNamesList = searchSortFieldNamesList, SearchSortFieldNamesDescendingList = searchSortFieldNamesDescendingList, entityId = entityId, schoolYearId = schoolYearId, flatten = flatten, returnResponse = returnResponse)
	}

	#' Get a TempPlanPositionSupplement
	#'
	#' This function returns a dataframe or json object of a TempPlanPositionSupplement
	#' @param TempPlanPositionSupplementID The ID of the TempPlanPositionSupplement to return.
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given TempPlanPositionSupplement. Defaults to FALSE for all return fields which, for convenience, returns all fields for the TempPlanPositionSupplement.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object listed as text. Run \code{\link{getSchemaForObjects}}('TempPlanPositionSupplement') to get more field paths.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{listSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Staff Planning
	#' @return A dataframe or of TempPlanPositionSupplement
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	getTempPlanPositionSupplement <- function(TempPlanPositionSupplementID, TempPlanPositionEmployeeID = F, SupplementTypeID = F, Rate = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, AnnualPay = F, PlanPositionID = F, PlanPositionSupplementID = F, SupplementTypeCodeDescription = F, PlanPositionOldAnnualPay = F, PlanPositionNewAnnualPay = F, PlanPositionPositionTypeCodeDescription = F, PlanPositionDistributionsBuildingCodes = F, PlanPositionDistributionsAssignmentTypeCodes = F, PlanPositionEmployeeName = F, PlanPositionEmployeeNumber = F, ErrorCount = F, OldSupplementTypeCodeDescription = F, SupplementTypeAmountTypeCode = F, OldSupplementTypeAmountTypeCode = F, OldRate = F, OldAnnualPay = F, fieldPaths = NULL, entityId = 1, schoolYearId = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment()) %>% purrr::keep(names(.) != "TempPlanPositionSupplementID")

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		getSkyObject(module = "StaffPlanning", objectName = "TempPlanPositionSupplement", objectId = TempPlanPositionSupplementID, searchFields = searchFields, entityId = entityId, flatten = flatten, returnResponse = returnResponse)
	}

	#' Delete a TempPlanPositionSupplement
	#'
	#' This function deletes a TempPlanPositionSupplement
	#' @param TempPlanPositionSupplementID The ID of the TempPlanPositionSupplement to delete
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{listSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Staff Planning
	#' @return The TempPlanPositionSupplementID of the deleted TempPlanPositionSupplement.
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	deleteTempPlanPositionSupplement <- function(TempPlanPositionSupplementID, ignoreWarnings = F, entityId = 1, schoolYearId = NULL, flatten = T, returnResponse = F){

		deleteSkyObject(module = "StaffPlanning", objectName = "TempPlanPositionSupplement", objectId = TempPlanPositionSupplementID, ignoreWarnings = ignoreWarnings, entityId = entityId, flatten = flatten, returnResponse = returnResponse)
	}

	#' Create a TempPlanPositionSupplement
	#'
	#' This function creates a TempPlanPositionSupplement
	#' @param fieldNames The field values to give the created TempPlanPositionSupplement. Each defaults to NULL.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{listSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Staff Planning
	#' @return A newly created TempPlanPositionSupplement
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	createTempPlanPositionSupplement <- function(TempPlanPositionEmployeeID = NULL, SupplementTypeID = NULL, Rate = NULL, AnnualPay = NULL, PlanPositionID = NULL, PlanPositionSupplementID = NULL, SupplementTypeCodeDescription = NULL, PlanPositionOldAnnualPay = NULL, PlanPositionNewAnnualPay = NULL, PlanPositionPositionTypeCodeDescription = NULL, PlanPositionDistributionsBuildingCodes = NULL, PlanPositionDistributionsAssignmentTypeCodes = NULL, PlanPositionEmployeeName = NULL, PlanPositionEmployeeNumber = NULL, ErrorCount = NULL, OldSupplementTypeCodeDescription = NULL, SupplementTypeAmountTypeCode = NULL, OldSupplementTypeAmountTypeCode = NULL, OldRate = NULL, OldAnnualPay = NULL, entityId = 1, schoolYearId = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment())

		body <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper()) %>% purrr::compact()

		createSkyObject(module = "StaffPlanning", objectName = "TempPlanPositionSupplement", body = list(DataObject = body), searchFields = append("TempPlanPositionSupplementID", body %>% names()), entityId = entityId, schoolYearId = schoolYearId, flatten = flatten, returnResponse = returnResponse)
	}

	#' Modify a TempPlanPositionSupplement
	#'
	#' This function modifies a TempPlanPositionSupplement
	#' @param fieldNames The field values to give the modified TempPlanPositionSupplement. Each defaults to NULL.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{listSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Staff Planning
	#' @return The modified TempPlanPositionSupplement
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	modifyTempPlanPositionSupplement <- function(TempPlanPositionSupplementID, TempPlanPositionEmployeeID = NULL, SupplementTypeID = NULL, Rate = NULL, AnnualPay = NULL, PlanPositionID = NULL, PlanPositionSupplementID = NULL, SupplementTypeCodeDescription = NULL, PlanPositionOldAnnualPay = NULL, PlanPositionNewAnnualPay = NULL, PlanPositionPositionTypeCodeDescription = NULL, PlanPositionDistributionsBuildingCodes = NULL, PlanPositionDistributionsAssignmentTypeCodes = NULL, PlanPositionEmployeeName = NULL, PlanPositionEmployeeNumber = NULL, ErrorCount = NULL, OldSupplementTypeCodeDescription = NULL, SupplementTypeAmountTypeCode = NULL, OldSupplementTypeAmountTypeCode = NULL, OldRate = NULL, OldAnnualPay = NULL, entityId = 1, schoolYearId = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment())

		body <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper()) %>% purrr::compact()

		modifySkyObject(module = "StaffPlanning", objectName = "TempPlanPositionSupplement", objectId = TempPlanPositionSupplementID, body = list(DataObject = body), searchFields = append("TempPlanPositionSupplementID", body %>% names()), entityId = entityId, schoolYearId = schoolYearId, flatten = flatten, returnResponse = returnResponse)
	}

	#' List TempPlanPositionBenefits
	#'
	#' This function returns a dataframe or json object of TempPlanPositionBenefits
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given TempPlanPositionBenefits. Defaults to FALSE for all return fields which, for convenience, returns all fields for the TempPlanPositionBenefits.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object listed as text. Run \code{\link{getSchemaForObjects}}('TempPlanPositionBenefit') to get more field paths.
	#' @param searchConditionsList A list of search conditions to filter results which are joined by the searchConditionsGroupType. Of the form {FieldName} {ConditionType} {SearchCondition}. For example, c('StudentID LessEqual 500', 'LastName Like Ander\%'). Run \code{\link{listSearchConditionTypes}} for a list of ConditionTypes. Defaults to NULL (unfiltered).
	#' @param searchConditionsGroupType The conjunction which joins multiple searchConditions in the searchConditionsList. Either 'Or' or 'And'. Defaults to 'And'.
	#' @param searchSortFieldNamesList The list of fields sort results by. Defaults to NULL (unsorted).
	#' @param searchSortFieldNamesDescendingList A list of T/F values corresponding to whether to sort each field in searchSortFieldNamesList in descending order. Defaults to F for each FieldName in searchSortFieldNamesList.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{listSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param page Results are paginated. The page of results to return. Default is 1.
	#' @param pageSize Results are paginated. The number of records per page to return. Default is 100,000 (essentially all records for most objects).
	#' @param flatten Whether to flatten results into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Staff Planning
	#' @return A list of TempPlanPositionBenefits
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	listTempPlanPositionBenefits <- function(searchConditionsList = NULL, TempPlanPositionBenefitID = F, TempPlanPositionEmployeeID = F, BenefitID = F, Value = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, PlanPositionBenefitID = F, BenefitCodeDescription = F, PositionNumber = F, PlanPositionEmployeeName = F, CalculationType = F, NewValue = F, ErrorCount = F, PlanPositionID = F, PositionTypeDescription = F, PlanPositionEmployeeNumber = F, StateBenefitTXID = F, BenefitTypeTX = F, fieldPaths = NULL, searchConditionsGroupType = "And", searchSortFieldNamesList = NULL, searchSortFieldNamesDescendingList = NULL, entityId = 1, schoolYearId = NULL, page = 1, pageSize = 100000, flatten = T, returnResponse = F){

		params <- as.list(environment())

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		listSkyObjects(module = "StaffPlanning", objectName = "TempPlanPositionBenefit", searchFields = searchFields %>% append(fieldPaths), page = page, pageSize = pageSize, SearchConditionsList = searchConditionsList, SearchConditionsGroupType = searchConditionsGroupType, SearchSortFieldNamesList = searchSortFieldNamesList, SearchSortFieldNamesDescendingList = searchSortFieldNamesDescendingList, entityId = entityId, schoolYearId = schoolYearId, flatten = flatten, returnResponse = returnResponse)
	}

	#' Get a TempPlanPositionBenefit
	#'
	#' This function returns a dataframe or json object of a TempPlanPositionBenefit
	#' @param TempPlanPositionBenefitID The ID of the TempPlanPositionBenefit to return.
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given TempPlanPositionBenefit. Defaults to FALSE for all return fields which, for convenience, returns all fields for the TempPlanPositionBenefit.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object listed as text. Run \code{\link{getSchemaForObjects}}('TempPlanPositionBenefit') to get more field paths.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{listSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Staff Planning
	#' @return A dataframe or of TempPlanPositionBenefit
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	getTempPlanPositionBenefit <- function(TempPlanPositionBenefitID, TempPlanPositionEmployeeID = F, BenefitID = F, Value = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, PlanPositionBenefitID = F, BenefitCodeDescription = F, PositionNumber = F, PlanPositionEmployeeName = F, CalculationType = F, NewValue = F, ErrorCount = F, PlanPositionID = F, PositionTypeDescription = F, PlanPositionEmployeeNumber = F, StateBenefitTXID = F, BenefitTypeTX = F, fieldPaths = NULL, entityId = 1, schoolYearId = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment()) %>% purrr::keep(names(.) != "TempPlanPositionBenefitID")

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		getSkyObject(module = "StaffPlanning", objectName = "TempPlanPositionBenefit", objectId = TempPlanPositionBenefitID, searchFields = searchFields, entityId = entityId, flatten = flatten, returnResponse = returnResponse)
	}

	#' Delete a TempPlanPositionBenefit
	#'
	#' This function deletes a TempPlanPositionBenefit
	#' @param TempPlanPositionBenefitID The ID of the TempPlanPositionBenefit to delete
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{listSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Staff Planning
	#' @return The TempPlanPositionBenefitID of the deleted TempPlanPositionBenefit.
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	deleteTempPlanPositionBenefit <- function(TempPlanPositionBenefitID, ignoreWarnings = F, entityId = 1, schoolYearId = NULL, flatten = T, returnResponse = F){

		deleteSkyObject(module = "StaffPlanning", objectName = "TempPlanPositionBenefit", objectId = TempPlanPositionBenefitID, ignoreWarnings = ignoreWarnings, entityId = entityId, flatten = flatten, returnResponse = returnResponse)
	}

	#' Create a TempPlanPositionBenefit
	#'
	#' This function creates a TempPlanPositionBenefit
	#' @param fieldNames The field values to give the created TempPlanPositionBenefit. Each defaults to NULL.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{listSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Staff Planning
	#' @return A newly created TempPlanPositionBenefit
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	createTempPlanPositionBenefit <- function(TempPlanPositionEmployeeID = NULL, BenefitID = NULL, Value = NULL, PlanPositionBenefitID = NULL, BenefitCodeDescription = NULL, PositionNumber = NULL, PlanPositionEmployeeName = NULL, CalculationType = NULL, NewValue = NULL, ErrorCount = NULL, PlanPositionID = NULL, PositionTypeDescription = NULL, PlanPositionEmployeeNumber = NULL, StateBenefitTXID = NULL, BenefitTypeTX = NULL, entityId = 1, schoolYearId = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment())

		body <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper()) %>% purrr::compact()

		createSkyObject(module = "StaffPlanning", objectName = "TempPlanPositionBenefit", body = list(DataObject = body), searchFields = append("TempPlanPositionBenefitID", body %>% names()), entityId = entityId, schoolYearId = schoolYearId, flatten = flatten, returnResponse = returnResponse)
	}

	#' Modify a TempPlanPositionBenefit
	#'
	#' This function modifies a TempPlanPositionBenefit
	#' @param fieldNames The field values to give the modified TempPlanPositionBenefit. Each defaults to NULL.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{listSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Staff Planning
	#' @return The modified TempPlanPositionBenefit
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	modifyTempPlanPositionBenefit <- function(TempPlanPositionBenefitID, TempPlanPositionEmployeeID = NULL, BenefitID = NULL, Value = NULL, PlanPositionBenefitID = NULL, BenefitCodeDescription = NULL, PositionNumber = NULL, PlanPositionEmployeeName = NULL, CalculationType = NULL, NewValue = NULL, ErrorCount = NULL, PlanPositionID = NULL, PositionTypeDescription = NULL, PlanPositionEmployeeNumber = NULL, StateBenefitTXID = NULL, BenefitTypeTX = NULL, entityId = 1, schoolYearId = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment())

		body <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper()) %>% purrr::compact()

		modifySkyObject(module = "StaffPlanning", objectName = "TempPlanPositionBenefit", objectId = TempPlanPositionBenefitID, body = list(DataObject = body), searchFields = append("TempPlanPositionBenefitID", body %>% names()), entityId = entityId, schoolYearId = schoolYearId, flatten = flatten, returnResponse = returnResponse)
	}

	#' List TempPlanPositionPays
	#'
	#' This function returns a dataframe or json object of TempPlanPositionPays
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given TempPlanPositionPays. Defaults to FALSE for all return fields which, for convenience, returns all fields for the TempPlanPositionPays.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object listed as text. Run \code{\link{getSchemaForObjects}}('TempPlanPositionPay') to get more field paths.
	#' @param searchConditionsList A list of search conditions to filter results which are joined by the searchConditionsGroupType. Of the form {FieldName} {ConditionType} {SearchCondition}. For example, c('StudentID LessEqual 500', 'LastName Like Ander\%'). Run \code{\link{listSearchConditionTypes}} for a list of ConditionTypes. Defaults to NULL (unfiltered).
	#' @param searchConditionsGroupType The conjunction which joins multiple searchConditions in the searchConditionsList. Either 'Or' or 'And'. Defaults to 'And'.
	#' @param searchSortFieldNamesList The list of fields sort results by. Defaults to NULL (unsorted).
	#' @param searchSortFieldNamesDescendingList A list of T/F values corresponding to whether to sort each field in searchSortFieldNamesList in descending order. Defaults to F for each FieldName in searchSortFieldNamesList.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{listSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param page Results are paginated. The page of results to return. Default is 1.
	#' @param pageSize Results are paginated. The number of records per page to return. Default is 100,000 (essentially all records for most objects).
	#' @param flatten Whether to flatten results into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Staff Planning
	#' @return A list of TempPlanPositionPays
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	listTempPlanPositionPays <- function(searchConditionsList = NULL, TempPlanPositionPayID = F, TempPlanPositionEmployeeID = F, PayTypeID = F, TypeCode = F, StipendAmount = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, AccountDistributionString = F, PayScheduleID = F, PositionTypeCodeDescription = F, AssignmentTypeCodes = F, AssignmentTypeDescriptions = F, BuildingCodes = F, BuildingDescriptions = F, EmployeeFullNameLFM = F, EmployeeNumber = F, PayTypeCodeDescription = F, OldDisplayAccount = F, NewDisplayAccount = F, PlanPositionPayID = F, ErrorCount = F, PayScheduleCodeDescription = F, MatchingExpenditureEligibilityCode = F, NewMatchingExpenditureEligibilityCode = F, AssignmentPayTypeIDOriginal = F, StatePayrollActivityCodeTXID = F, fieldPaths = NULL, searchConditionsGroupType = "And", searchSortFieldNamesList = NULL, searchSortFieldNamesDescendingList = NULL, entityId = 1, schoolYearId = NULL, page = 1, pageSize = 100000, flatten = T, returnResponse = F){

		params <- as.list(environment())

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		listSkyObjects(module = "StaffPlanning", objectName = "TempPlanPositionPay", searchFields = searchFields %>% append(fieldPaths), page = page, pageSize = pageSize, SearchConditionsList = searchConditionsList, SearchConditionsGroupType = searchConditionsGroupType, SearchSortFieldNamesList = searchSortFieldNamesList, SearchSortFieldNamesDescendingList = searchSortFieldNamesDescendingList, entityId = entityId, schoolYearId = schoolYearId, flatten = flatten, returnResponse = returnResponse)
	}

	#' Get a TempPlanPositionPay
	#'
	#' This function returns a dataframe or json object of a TempPlanPositionPay
	#' @param TempPlanPositionPayID The ID of the TempPlanPositionPay to return.
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given TempPlanPositionPay. Defaults to FALSE for all return fields which, for convenience, returns all fields for the TempPlanPositionPay.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object listed as text. Run \code{\link{getSchemaForObjects}}('TempPlanPositionPay') to get more field paths.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{listSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Staff Planning
	#' @return A dataframe or of TempPlanPositionPay
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	getTempPlanPositionPay <- function(TempPlanPositionPayID, TempPlanPositionEmployeeID = F, PayTypeID = F, TypeCode = F, StipendAmount = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, AccountDistributionString = F, PayScheduleID = F, PositionTypeCodeDescription = F, AssignmentTypeCodes = F, AssignmentTypeDescriptions = F, BuildingCodes = F, BuildingDescriptions = F, EmployeeFullNameLFM = F, EmployeeNumber = F, PayTypeCodeDescription = F, OldDisplayAccount = F, NewDisplayAccount = F, PlanPositionPayID = F, ErrorCount = F, PayScheduleCodeDescription = F, MatchingExpenditureEligibilityCode = F, NewMatchingExpenditureEligibilityCode = F, AssignmentPayTypeIDOriginal = F, StatePayrollActivityCodeTXID = F, fieldPaths = NULL, entityId = 1, schoolYearId = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment()) %>% purrr::keep(names(.) != "TempPlanPositionPayID")

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		getSkyObject(module = "StaffPlanning", objectName = "TempPlanPositionPay", objectId = TempPlanPositionPayID, searchFields = searchFields, entityId = entityId, flatten = flatten, returnResponse = returnResponse)
	}

	#' Delete a TempPlanPositionPay
	#'
	#' This function deletes a TempPlanPositionPay
	#' @param TempPlanPositionPayID The ID of the TempPlanPositionPay to delete
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{listSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Staff Planning
	#' @return The TempPlanPositionPayID of the deleted TempPlanPositionPay.
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	deleteTempPlanPositionPay <- function(TempPlanPositionPayID, ignoreWarnings = F, entityId = 1, schoolYearId = NULL, flatten = T, returnResponse = F){

		deleteSkyObject(module = "StaffPlanning", objectName = "TempPlanPositionPay", objectId = TempPlanPositionPayID, ignoreWarnings = ignoreWarnings, entityId = entityId, flatten = flatten, returnResponse = returnResponse)
	}

	#' Create a TempPlanPositionPay
	#'
	#' This function creates a TempPlanPositionPay
	#' @param fieldNames The field values to give the created TempPlanPositionPay. Each defaults to NULL.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{listSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Staff Planning
	#' @return A newly created TempPlanPositionPay
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	createTempPlanPositionPay <- function(TempPlanPositionEmployeeID = NULL, PayTypeID = NULL, TypeCode = NULL, StipendAmount = NULL, AccountDistributionString = NULL, PayScheduleID = NULL, PositionTypeCodeDescription = NULL, AssignmentTypeCodes = NULL, AssignmentTypeDescriptions = NULL, BuildingCodes = NULL, BuildingDescriptions = NULL, EmployeeFullNameLFM = NULL, EmployeeNumber = NULL, PayTypeCodeDescription = NULL, OldDisplayAccount = NULL, NewDisplayAccount = NULL, PlanPositionPayID = NULL, ErrorCount = NULL, PayScheduleCodeDescription = NULL, MatchingExpenditureEligibilityCode = NULL, NewMatchingExpenditureEligibilityCode = NULL, AssignmentPayTypeIDOriginal = NULL, StatePayrollActivityCodeTXID = NULL, entityId = 1, schoolYearId = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment())

		body <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper()) %>% purrr::compact()

		createSkyObject(module = "StaffPlanning", objectName = "TempPlanPositionPay", body = list(DataObject = body), searchFields = append("TempPlanPositionPayID", body %>% names()), entityId = entityId, schoolYearId = schoolYearId, flatten = flatten, returnResponse = returnResponse)
	}

	#' Modify a TempPlanPositionPay
	#'
	#' This function modifies a TempPlanPositionPay
	#' @param fieldNames The field values to give the modified TempPlanPositionPay. Each defaults to NULL.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{listSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Staff Planning
	#' @return The modified TempPlanPositionPay
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	modifyTempPlanPositionPay <- function(TempPlanPositionPayID, TempPlanPositionEmployeeID = NULL, PayTypeID = NULL, TypeCode = NULL, StipendAmount = NULL, AccountDistributionString = NULL, PayScheduleID = NULL, PositionTypeCodeDescription = NULL, AssignmentTypeCodes = NULL, AssignmentTypeDescriptions = NULL, BuildingCodes = NULL, BuildingDescriptions = NULL, EmployeeFullNameLFM = NULL, EmployeeNumber = NULL, PayTypeCodeDescription = NULL, OldDisplayAccount = NULL, NewDisplayAccount = NULL, PlanPositionPayID = NULL, ErrorCount = NULL, PayScheduleCodeDescription = NULL, MatchingExpenditureEligibilityCode = NULL, NewMatchingExpenditureEligibilityCode = NULL, AssignmentPayTypeIDOriginal = NULL, StatePayrollActivityCodeTXID = NULL, entityId = 1, schoolYearId = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment())

		body <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper()) %>% purrr::compact()

		modifySkyObject(module = "StaffPlanning", objectName = "TempPlanPositionPay", objectId = TempPlanPositionPayID, body = list(DataObject = body), searchFields = append("TempPlanPositionPayID", body %>% names()), entityId = entityId, schoolYearId = schoolYearId, flatten = flatten, returnResponse = returnResponse)
	}

	#' List TempPlanPositionPayAccountDistributions
	#'
	#' This function returns a dataframe or json object of TempPlanPositionPayAccountDistributions
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given TempPlanPositionPayAccountDistributions. Defaults to FALSE for all return fields which, for convenience, returns all fields for the TempPlanPositionPayAccountDistributions.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object listed as text. Run \code{\link{getSchemaForObjects}}('TempPlanPositionPayAccountDistribution') to get more field paths.
	#' @param searchConditionsList A list of search conditions to filter results which are joined by the searchConditionsGroupType. Of the form {FieldName} {ConditionType} {SearchCondition}. For example, c('StudentID LessEqual 500', 'LastName Like Ander\%'). Run \code{\link{listSearchConditionTypes}} for a list of ConditionTypes. Defaults to NULL (unfiltered).
	#' @param searchConditionsGroupType The conjunction which joins multiple searchConditions in the searchConditionsList. Either 'Or' or 'And'. Defaults to 'And'.
	#' @param searchSortFieldNamesList The list of fields sort results by. Defaults to NULL (unsorted).
	#' @param searchSortFieldNamesDescendingList A list of T/F values corresponding to whether to sort each field in searchSortFieldNamesList in descending order. Defaults to F for each FieldName in searchSortFieldNamesList.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{listSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param page Results are paginated. The page of results to return. Default is 1.
	#' @param pageSize Results are paginated. The number of records per page to return. Default is 100,000 (essentially all records for most objects).
	#' @param flatten Whether to flatten results into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Staff Planning
	#' @return A list of TempPlanPositionPayAccountDistributions
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	listTempPlanPositionPayAccountDistributions <- function(searchConditionsList = NULL, TempPlanPositionPayAccountDistributionID = F, TempPlanPositionPayID = F, AccountID = F, DistributionPercent = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, PlanPositionPayID = F, DisplayAccount = F, StateConcordDepartmentTNID = F, fieldPaths = NULL, searchConditionsGroupType = "And", searchSortFieldNamesList = NULL, searchSortFieldNamesDescendingList = NULL, entityId = 1, schoolYearId = NULL, page = 1, pageSize = 100000, flatten = T, returnResponse = F){

		params <- as.list(environment())

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		listSkyObjects(module = "StaffPlanning", objectName = "TempPlanPositionPayAccountDistribution", searchFields = searchFields %>% append(fieldPaths), page = page, pageSize = pageSize, SearchConditionsList = searchConditionsList, SearchConditionsGroupType = searchConditionsGroupType, SearchSortFieldNamesList = searchSortFieldNamesList, SearchSortFieldNamesDescendingList = searchSortFieldNamesDescendingList, entityId = entityId, schoolYearId = schoolYearId, flatten = flatten, returnResponse = returnResponse)
	}

	#' Get a TempPlanPositionPayAccountDistribution
	#'
	#' This function returns a dataframe or json object of a TempPlanPositionPayAccountDistribution
	#' @param TempPlanPositionPayAccountDistributionID The ID of the TempPlanPositionPayAccountDistribution to return.
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given TempPlanPositionPayAccountDistribution. Defaults to FALSE for all return fields which, for convenience, returns all fields for the TempPlanPositionPayAccountDistribution.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object listed as text. Run \code{\link{getSchemaForObjects}}('TempPlanPositionPayAccountDistribution') to get more field paths.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{listSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Staff Planning
	#' @return A dataframe or of TempPlanPositionPayAccountDistribution
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	getTempPlanPositionPayAccountDistribution <- function(TempPlanPositionPayAccountDistributionID, TempPlanPositionPayID = F, AccountID = F, DistributionPercent = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, PlanPositionPayID = F, DisplayAccount = F, StateConcordDepartmentTNID = F, fieldPaths = NULL, entityId = 1, schoolYearId = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment()) %>% purrr::keep(names(.) != "TempPlanPositionPayAccountDistributionID")

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		getSkyObject(module = "StaffPlanning", objectName = "TempPlanPositionPayAccountDistribution", objectId = TempPlanPositionPayAccountDistributionID, searchFields = searchFields, entityId = entityId, flatten = flatten, returnResponse = returnResponse)
	}

	#' Delete a TempPlanPositionPayAccountDistribution
	#'
	#' This function deletes a TempPlanPositionPayAccountDistribution
	#' @param TempPlanPositionPayAccountDistributionID The ID of the TempPlanPositionPayAccountDistribution to delete
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{listSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Staff Planning
	#' @return The TempPlanPositionPayAccountDistributionID of the deleted TempPlanPositionPayAccountDistribution.
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	deleteTempPlanPositionPayAccountDistribution <- function(TempPlanPositionPayAccountDistributionID, ignoreWarnings = F, entityId = 1, schoolYearId = NULL, flatten = T, returnResponse = F){

		deleteSkyObject(module = "StaffPlanning", objectName = "TempPlanPositionPayAccountDistribution", objectId = TempPlanPositionPayAccountDistributionID, ignoreWarnings = ignoreWarnings, entityId = entityId, flatten = flatten, returnResponse = returnResponse)
	}

	#' Create a TempPlanPositionPayAccountDistribution
	#'
	#' This function creates a TempPlanPositionPayAccountDistribution
	#' @param fieldNames The field values to give the created TempPlanPositionPayAccountDistribution. Each defaults to NULL.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{listSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Staff Planning
	#' @return A newly created TempPlanPositionPayAccountDistribution
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	createTempPlanPositionPayAccountDistribution <- function(TempPlanPositionPayID = NULL, AccountID = NULL, DistributionPercent = NULL, PlanPositionPayID = NULL, DisplayAccount = NULL, StateConcordDepartmentTNID = NULL, entityId = 1, schoolYearId = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment())

		body <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper()) %>% purrr::compact()

		createSkyObject(module = "StaffPlanning", objectName = "TempPlanPositionPayAccountDistribution", body = list(DataObject = body), searchFields = append("TempPlanPositionPayAccountDistributionID", body %>% names()), entityId = entityId, schoolYearId = schoolYearId, flatten = flatten, returnResponse = returnResponse)
	}

	#' Modify a TempPlanPositionPayAccountDistribution
	#'
	#' This function modifies a TempPlanPositionPayAccountDistribution
	#' @param fieldNames The field values to give the modified TempPlanPositionPayAccountDistribution. Each defaults to NULL.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{listSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Staff Planning
	#' @return The modified TempPlanPositionPayAccountDistribution
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	modifyTempPlanPositionPayAccountDistribution <- function(TempPlanPositionPayAccountDistributionID, TempPlanPositionPayID = NULL, AccountID = NULL, DistributionPercent = NULL, PlanPositionPayID = NULL, DisplayAccount = NULL, StateConcordDepartmentTNID = NULL, entityId = 1, schoolYearId = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment())

		body <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper()) %>% purrr::compact()

		modifySkyObject(module = "StaffPlanning", objectName = "TempPlanPositionPayAccountDistribution", objectId = TempPlanPositionPayAccountDistributionID, body = list(DataObject = body), searchFields = append("TempPlanPositionPayAccountDistributionID", body %>% names()), entityId = entityId, schoolYearId = schoolYearId, flatten = flatten, returnResponse = returnResponse)
	}

	#' List TempPlanPositionPayBenefits
	#'
	#' This function returns a dataframe or json object of TempPlanPositionPayBenefits
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given TempPlanPositionPayBenefits. Defaults to FALSE for all return fields which, for convenience, returns all fields for the TempPlanPositionPayBenefits.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object listed as text. Run \code{\link{getSchemaForObjects}}('TempPlanPositionPayBenefit') to get more field paths.
	#' @param searchConditionsList A list of search conditions to filter results which are joined by the searchConditionsGroupType. Of the form {FieldName} {ConditionType} {SearchCondition}. For example, c('StudentID LessEqual 500', 'LastName Like Ander\%'). Run \code{\link{listSearchConditionTypes}} for a list of ConditionTypes. Defaults to NULL (unfiltered).
	#' @param searchConditionsGroupType The conjunction which joins multiple searchConditions in the searchConditionsList. Either 'Or' or 'And'. Defaults to 'And'.
	#' @param searchSortFieldNamesList The list of fields sort results by. Defaults to NULL (unsorted).
	#' @param searchSortFieldNamesDescendingList A list of T/F values corresponding to whether to sort each field in searchSortFieldNamesList in descending order. Defaults to F for each FieldName in searchSortFieldNamesList.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{listSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param page Results are paginated. The page of results to return. Default is 1.
	#' @param pageSize Results are paginated. The number of records per page to return. Default is 100,000 (essentially all records for most objects).
	#' @param flatten Whether to flatten results into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Staff Planning
	#' @return A list of TempPlanPositionPayBenefits
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	listTempPlanPositionPayBenefits <- function(searchConditionsList = NULL, TempPlanPositionPayBenefitID = F, TempPlanPositionPayID = F, TempPlanPositionBenefitID = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, PlanPositionPayBenefitID = F, PlanPositionPayID = F, BenefitCodeDescription = F, PlanPositionEmployeeName = F, EmployeeNumber = F, PositionTypeDescription = F, PayTypeCodeDescription = F, PayScheduleCodeDescription = F, ErrorCount = F, fieldPaths = NULL, searchConditionsGroupType = "And", searchSortFieldNamesList = NULL, searchSortFieldNamesDescendingList = NULL, entityId = 1, schoolYearId = NULL, page = 1, pageSize = 100000, flatten = T, returnResponse = F){

		params <- as.list(environment())

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		listSkyObjects(module = "StaffPlanning", objectName = "TempPlanPositionPayBenefit", searchFields = searchFields %>% append(fieldPaths), page = page, pageSize = pageSize, SearchConditionsList = searchConditionsList, SearchConditionsGroupType = searchConditionsGroupType, SearchSortFieldNamesList = searchSortFieldNamesList, SearchSortFieldNamesDescendingList = searchSortFieldNamesDescendingList, entityId = entityId, schoolYearId = schoolYearId, flatten = flatten, returnResponse = returnResponse)
	}

	#' Get a TempPlanPositionPayBenefit
	#'
	#' This function returns a dataframe or json object of a TempPlanPositionPayBenefit
	#' @param TempPlanPositionPayBenefitID The ID of the TempPlanPositionPayBenefit to return.
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given TempPlanPositionPayBenefit. Defaults to FALSE for all return fields which, for convenience, returns all fields for the TempPlanPositionPayBenefit.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object listed as text. Run \code{\link{getSchemaForObjects}}('TempPlanPositionPayBenefit') to get more field paths.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{listSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Staff Planning
	#' @return A dataframe or of TempPlanPositionPayBenefit
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	getTempPlanPositionPayBenefit <- function(TempPlanPositionPayBenefitID, TempPlanPositionPayID = F, TempPlanPositionBenefitID = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, PlanPositionPayBenefitID = F, PlanPositionPayID = F, BenefitCodeDescription = F, PlanPositionEmployeeName = F, EmployeeNumber = F, PositionTypeDescription = F, PayTypeCodeDescription = F, PayScheduleCodeDescription = F, ErrorCount = F, fieldPaths = NULL, entityId = 1, schoolYearId = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment()) %>% purrr::keep(names(.) != "TempPlanPositionPayBenefitID")

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		getSkyObject(module = "StaffPlanning", objectName = "TempPlanPositionPayBenefit", objectId = TempPlanPositionPayBenefitID, searchFields = searchFields, entityId = entityId, flatten = flatten, returnResponse = returnResponse)
	}

	#' Delete a TempPlanPositionPayBenefit
	#'
	#' This function deletes a TempPlanPositionPayBenefit
	#' @param TempPlanPositionPayBenefitID The ID of the TempPlanPositionPayBenefit to delete
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{listSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Staff Planning
	#' @return The TempPlanPositionPayBenefitID of the deleted TempPlanPositionPayBenefit.
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	deleteTempPlanPositionPayBenefit <- function(TempPlanPositionPayBenefitID, ignoreWarnings = F, entityId = 1, schoolYearId = NULL, flatten = T, returnResponse = F){

		deleteSkyObject(module = "StaffPlanning", objectName = "TempPlanPositionPayBenefit", objectId = TempPlanPositionPayBenefitID, ignoreWarnings = ignoreWarnings, entityId = entityId, flatten = flatten, returnResponse = returnResponse)
	}

	#' Create a TempPlanPositionPayBenefit
	#'
	#' This function creates a TempPlanPositionPayBenefit
	#' @param fieldNames The field values to give the created TempPlanPositionPayBenefit. Each defaults to NULL.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{listSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Staff Planning
	#' @return A newly created TempPlanPositionPayBenefit
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	createTempPlanPositionPayBenefit <- function(TempPlanPositionPayID = NULL, TempPlanPositionBenefitID = NULL, PlanPositionPayBenefitID = NULL, PlanPositionPayID = NULL, BenefitCodeDescription = NULL, PlanPositionEmployeeName = NULL, EmployeeNumber = NULL, PositionTypeDescription = NULL, PayTypeCodeDescription = NULL, PayScheduleCodeDescription = NULL, ErrorCount = NULL, entityId = 1, schoolYearId = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment())

		body <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper()) %>% purrr::compact()

		createSkyObject(module = "StaffPlanning", objectName = "TempPlanPositionPayBenefit", body = list(DataObject = body), searchFields = append("TempPlanPositionPayBenefitID", body %>% names()), entityId = entityId, schoolYearId = schoolYearId, flatten = flatten, returnResponse = returnResponse)
	}

	#' Modify a TempPlanPositionPayBenefit
	#'
	#' This function modifies a TempPlanPositionPayBenefit
	#' @param fieldNames The field values to give the modified TempPlanPositionPayBenefit. Each defaults to NULL.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{listSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Staff Planning
	#' @return The modified TempPlanPositionPayBenefit
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	modifyTempPlanPositionPayBenefit <- function(TempPlanPositionPayBenefitID, TempPlanPositionPayID = NULL, TempPlanPositionBenefitID = NULL, PlanPositionPayBenefitID = NULL, PlanPositionPayID = NULL, BenefitCodeDescription = NULL, PlanPositionEmployeeName = NULL, EmployeeNumber = NULL, PositionTypeDescription = NULL, PayTypeCodeDescription = NULL, PayScheduleCodeDescription = NULL, ErrorCount = NULL, entityId = 1, schoolYearId = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment())

		body <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper()) %>% purrr::compact()

		modifySkyObject(module = "StaffPlanning", objectName = "TempPlanPositionPayBenefit", objectId = TempPlanPositionPayBenefitID, body = list(DataObject = body), searchFields = append("TempPlanPositionPayBenefitID", body %>% names()), entityId = entityId, schoolYearId = schoolYearId, flatten = flatten, returnResponse = returnResponse)
	}

	#' List TempPlanPositionExceptions
	#'
	#' This function returns a dataframe or json object of TempPlanPositionExceptions
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given TempPlanPositionExceptions. Defaults to FALSE for all return fields which, for convenience, returns all fields for the TempPlanPositionExceptions.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object listed as text. Run \code{\link{getSchemaForObjects}}('TempPlanPositionException') to get more field paths.
	#' @param searchConditionsList A list of search conditions to filter results which are joined by the searchConditionsGroupType. Of the form {FieldName} {ConditionType} {SearchCondition}. For example, c('StudentID LessEqual 500', 'LastName Like Ander\%'). Run \code{\link{listSearchConditionTypes}} for a list of ConditionTypes. Defaults to NULL (unfiltered).
	#' @param searchConditionsGroupType The conjunction which joins multiple searchConditions in the searchConditionsList. Either 'Or' or 'And'. Defaults to 'And'.
	#' @param searchSortFieldNamesList The list of fields sort results by. Defaults to NULL (unsorted).
	#' @param searchSortFieldNamesDescendingList A list of T/F values corresponding to whether to sort each field in searchSortFieldNamesList in descending order. Defaults to F for each FieldName in searchSortFieldNamesList.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{listSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param page Results are paginated. The page of results to return. Default is 1.
	#' @param pageSize Results are paginated. The number of records per page to return. Default is 100,000 (essentially all records for most objects).
	#' @param flatten Whether to flatten results into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Staff Planning
	#' @return A list of TempPlanPositionExceptions
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	listTempPlanPositionExceptions <- function(searchConditionsList = NULL, TempPlanPositionExceptionID = F, PositionNumberCode = F, Description = F, PositionIdentifier = F, Error = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, ErrorType = F, fieldPaths = NULL, searchConditionsGroupType = "And", searchSortFieldNamesList = NULL, searchSortFieldNamesDescendingList = NULL, entityId = 1, schoolYearId = NULL, page = 1, pageSize = 100000, flatten = T, returnResponse = F){

		params <- as.list(environment())

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		listSkyObjects(module = "StaffPlanning", objectName = "TempPlanPositionException", searchFields = searchFields %>% append(fieldPaths), page = page, pageSize = pageSize, SearchConditionsList = searchConditionsList, SearchConditionsGroupType = searchConditionsGroupType, SearchSortFieldNamesList = searchSortFieldNamesList, SearchSortFieldNamesDescendingList = searchSortFieldNamesDescendingList, entityId = entityId, schoolYearId = schoolYearId, flatten = flatten, returnResponse = returnResponse)
	}

	#' Get a TempPlanPositionException
	#'
	#' This function returns a dataframe or json object of a TempPlanPositionException
	#' @param TempPlanPositionExceptionID The ID of the TempPlanPositionException to return.
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given TempPlanPositionException. Defaults to FALSE for all return fields which, for convenience, returns all fields for the TempPlanPositionException.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object listed as text. Run \code{\link{getSchemaForObjects}}('TempPlanPositionException') to get more field paths.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{listSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Staff Planning
	#' @return A dataframe or of TempPlanPositionException
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	getTempPlanPositionException <- function(TempPlanPositionExceptionID, PositionNumberCode = F, Description = F, PositionIdentifier = F, Error = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, ErrorType = F, fieldPaths = NULL, entityId = 1, schoolYearId = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment()) %>% purrr::keep(names(.) != "TempPlanPositionExceptionID")

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		getSkyObject(module = "StaffPlanning", objectName = "TempPlanPositionException", objectId = TempPlanPositionExceptionID, searchFields = searchFields, entityId = entityId, flatten = flatten, returnResponse = returnResponse)
	}

	#' Delete a TempPlanPositionException
	#'
	#' This function deletes a TempPlanPositionException
	#' @param TempPlanPositionExceptionID The ID of the TempPlanPositionException to delete
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{listSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Staff Planning
	#' @return The TempPlanPositionExceptionID of the deleted TempPlanPositionException.
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	deleteTempPlanPositionException <- function(TempPlanPositionExceptionID, ignoreWarnings = F, entityId = 1, schoolYearId = NULL, flatten = T, returnResponse = F){

		deleteSkyObject(module = "StaffPlanning", objectName = "TempPlanPositionException", objectId = TempPlanPositionExceptionID, ignoreWarnings = ignoreWarnings, entityId = entityId, flatten = flatten, returnResponse = returnResponse)
	}

	#' Create a TempPlanPositionException
	#'
	#' This function creates a TempPlanPositionException
	#' @param fieldNames The field values to give the created TempPlanPositionException. Each defaults to NULL.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{listSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Staff Planning
	#' @return A newly created TempPlanPositionException
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	createTempPlanPositionException <- function(PositionNumberCode = NULL, Description = NULL, PositionIdentifier = NULL, Error = NULL, ErrorType = NULL, entityId = 1, schoolYearId = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment())

		body <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper()) %>% purrr::compact()

		createSkyObject(module = "StaffPlanning", objectName = "TempPlanPositionException", body = list(DataObject = body), searchFields = append("TempPlanPositionExceptionID", body %>% names()), entityId = entityId, schoolYearId = schoolYearId, flatten = flatten, returnResponse = returnResponse)
	}

	#' Modify a TempPlanPositionException
	#'
	#' This function modifies a TempPlanPositionException
	#' @param fieldNames The field values to give the modified TempPlanPositionException. Each defaults to NULL.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{listSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Staff Planning
	#' @return The modified TempPlanPositionException
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	modifyTempPlanPositionException <- function(TempPlanPositionExceptionID, PositionNumberCode = NULL, Description = NULL, PositionIdentifier = NULL, Error = NULL, ErrorType = NULL, entityId = 1, schoolYearId = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment())

		body <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper()) %>% purrr::compact()

		modifySkyObject(module = "StaffPlanning", objectName = "TempPlanPositionException", objectId = TempPlanPositionExceptionID, body = list(DataObject = body), searchFields = append("TempPlanPositionExceptionID", body %>% names()), entityId = entityId, schoolYearId = schoolYearId, flatten = flatten, returnResponse = returnResponse)
	}

	#' List StaffPlanningTempExceptions
	#'
	#' This function returns a dataframe or json object of StaffPlanningTempExceptions
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given StaffPlanningTempExceptions. Defaults to FALSE for all return fields which, for convenience, returns all fields for the StaffPlanningTempExceptions.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object listed as text. Run \code{\link{getSchemaForObjects}}('StaffPlanningTempException') to get more field paths.
	#' @param searchConditionsList A list of search conditions to filter results which are joined by the searchConditionsGroupType. Of the form {FieldName} {ConditionType} {SearchCondition}. For example, c('StudentID LessEqual 500', 'LastName Like Ander\%'). Run \code{\link{listSearchConditionTypes}} for a list of ConditionTypes. Defaults to NULL (unfiltered).
	#' @param searchConditionsGroupType The conjunction which joins multiple searchConditions in the searchConditionsList. Either 'Or' or 'And'. Defaults to 'And'.
	#' @param searchSortFieldNamesList The list of fields sort results by. Defaults to NULL (unsorted).
	#' @param searchSortFieldNamesDescendingList A list of T/F values corresponding to whether to sort each field in searchSortFieldNamesList in descending order. Defaults to F for each FieldName in searchSortFieldNamesList.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{listSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param page Results are paginated. The page of results to return. Default is 1.
	#' @param pageSize Results are paginated. The number of records per page to return. Default is 100,000 (essentially all records for most objects).
	#' @param flatten Whether to flatten results into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Staff Planning
	#' @return A list of StaffPlanningTempExceptions
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	listStaffPlanningTempExceptions <- function(searchConditionsList = NULL, TempExceptionID = F, ErroredObjectID = F, ErrorField = F, Error = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, fieldPaths = NULL, searchConditionsGroupType = "And", searchSortFieldNamesList = NULL, searchSortFieldNamesDescendingList = NULL, entityId = 1, schoolYearId = NULL, page = 1, pageSize = 100000, flatten = T, returnResponse = F){

		params <- as.list(environment())

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		listSkyObjects(module = "StaffPlanning", objectName = "TempException", searchFields = searchFields %>% append(fieldPaths), page = page, pageSize = pageSize, SearchConditionsList = searchConditionsList, SearchConditionsGroupType = searchConditionsGroupType, SearchSortFieldNamesList = searchSortFieldNamesList, SearchSortFieldNamesDescendingList = searchSortFieldNamesDescendingList, entityId = entityId, schoolYearId = schoolYearId, flatten = flatten, returnResponse = returnResponse)
	}

	#' Get a StaffPlanningTempException
	#'
	#' This function returns a dataframe or json object of a StaffPlanningTempException
	#' @param StaffPlanningTempExceptionID The ID of the StaffPlanningTempException to return.
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given StaffPlanningTempException. Defaults to FALSE for all return fields which, for convenience, returns all fields for the StaffPlanningTempException.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object listed as text. Run \code{\link{getSchemaForObjects}}('StaffPlanningTempException') to get more field paths.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{listSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Staff Planning
	#' @return A dataframe or of StaffPlanningTempException
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	getStaffPlanningTempException <- function(StaffPlanningTempExceptionID, TempExceptionID = F, ErroredObjectID = F, ErrorField = F, Error = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, fieldPaths = NULL, entityId = 1, schoolYearId = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment()) %>% purrr::keep(names(.) != "StaffPlanningTempExceptionID")

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		getSkyObject(module = "StaffPlanning", objectName = "TempException", objectId = StaffPlanningTempExceptionID, searchFields = searchFields, entityId = entityId, flatten = flatten, returnResponse = returnResponse)
	}

	#' Delete a StaffPlanningTempException
	#'
	#' This function deletes a StaffPlanningTempException
	#' @param StaffPlanningTempExceptionID The ID of the StaffPlanningTempException to delete
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{listSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Staff Planning
	#' @return The StaffPlanningTempExceptionID of the deleted StaffPlanningTempException.
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	deleteStaffPlanningTempException <- function(StaffPlanningTempExceptionID, ignoreWarnings = F, entityId = 1, schoolYearId = NULL, flatten = T, returnResponse = F){

		deleteSkyObject(module = "StaffPlanning", objectName = "TempException", objectId = StaffPlanningTempExceptionID, ignoreWarnings = ignoreWarnings, entityId = entityId, flatten = flatten, returnResponse = returnResponse)
	}

	#' Create a StaffPlanningTempException
	#'
	#' This function creates a StaffPlanningTempException
	#' @param fieldNames The field values to give the created StaffPlanningTempException. Each defaults to NULL.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{listSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Staff Planning
	#' @return A newly created StaffPlanningTempException
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	createStaffPlanningTempException <- function(ErroredObjectID = NULL, ErrorField = NULL, Error = NULL, entityId = 1, schoolYearId = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment())

		body <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper()) %>% purrr::compact()

		createSkyObject(module = "StaffPlanning", objectName = "TempException", body = list(DataObject = body), searchFields = append("TempExceptionID", body %>% names()), entityId = entityId, schoolYearId = schoolYearId, flatten = flatten, returnResponse = returnResponse)
	}

	#' Modify a StaffPlanningTempException
	#'
	#' This function modifies a StaffPlanningTempException
	#' @param fieldNames The field values to give the modified StaffPlanningTempException. Each defaults to NULL.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{listSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Staff Planning
	#' @return The modified StaffPlanningTempException
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	modifyStaffPlanningTempException <- function(TempExceptionID, ErroredObjectID = NULL, ErrorField = NULL, Error = NULL, entityId = 1, schoolYearId = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment())

		body <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper()) %>% purrr::compact()

		modifySkyObject(module = "StaffPlanning", objectName = "TempException", objectId = TempExceptionID, body = list(DataObject = body), searchFields = append("TempExceptionID", body %>% names()), entityId = entityId, schoolYearId = schoolYearId, flatten = flatten, returnResponse = returnResponse)
	}

	#' List PositionTypePlanPositionDistributionSummaries
	#'
	#' This function returns a dataframe or json object of PositionTypePlanPositionDistributionSummaries
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given PositionTypePlanPositionDistributionSummaries. Defaults to FALSE for all return fields which, for convenience, returns all fields for the PositionTypePlanPositionDistributionSummaries.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object listed as text. Run \code{\link{getSchemaForObjects}}('PositionTypePlanPositionDistributionSummary') to get more field paths.
	#' @param searchConditionsList A list of search conditions to filter results which are joined by the searchConditionsGroupType. Of the form {FieldName} {ConditionType} {SearchCondition}. For example, c('StudentID LessEqual 500', 'LastName Like Ander\%'). Run \code{\link{listSearchConditionTypes}} for a list of ConditionTypes. Defaults to NULL (unfiltered).
	#' @param searchConditionsGroupType The conjunction which joins multiple searchConditions in the searchConditionsList. Either 'Or' or 'And'. Defaults to 'And'.
	#' @param searchSortFieldNamesList The list of fields sort results by. Defaults to NULL (unsorted).
	#' @param searchSortFieldNamesDescendingList A list of T/F values corresponding to whether to sort each field in searchSortFieldNamesList in descending order. Defaults to F for each FieldName in searchSortFieldNamesList.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{listSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param page Results are paginated. The page of results to return. Default is 1.
	#' @param pageSize Results are paginated. The number of records per page to return. Default is 100,000 (essentially all records for most objects).
	#' @param flatten Whether to flatten results into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Staff Planning
	#' @return A list of PositionTypePlanPositionDistributionSummaries
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	listPositionTypePlanPositionDistributionSummaries <- function(searchConditionsList = NULL, PlanPositionDistributionIDFirst = F, PlanGroupID = F, PositionTypeID = F, FTE = F, fieldPaths = NULL, searchConditionsGroupType = "And", searchSortFieldNamesList = NULL, searchSortFieldNamesDescendingList = NULL, entityId = 1, schoolYearId = NULL, page = 1, pageSize = 100000, flatten = T, returnResponse = F){

		params <- as.list(environment())

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		listSkyObjects(module = "StaffPlanning", objectName = "PositionTypePlanPositionDistributionSummary", searchFields = searchFields %>% append(fieldPaths), page = page, pageSize = pageSize, SearchConditionsList = searchConditionsList, SearchConditionsGroupType = searchConditionsGroupType, SearchSortFieldNamesList = searchSortFieldNamesList, SearchSortFieldNamesDescendingList = searchSortFieldNamesDescendingList, entityId = entityId, schoolYearId = schoolYearId, flatten = flatten, returnResponse = returnResponse)
	}

	#' Get a PositionTypePlanPositionDistributionSummary
	#'
	#' This function returns a dataframe or json object of a PositionTypePlanPositionDistributionSummary
	#' @param PositionTypePlanPositionDistributionSummaryID The ID of the PositionTypePlanPositionDistributionSummary to return.
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given PositionTypePlanPositionDistributionSummary. Defaults to FALSE for all return fields which, for convenience, returns all fields for the PositionTypePlanPositionDistributionSummary.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object listed as text. Run \code{\link{getSchemaForObjects}}('PositionTypePlanPositionDistributionSummary') to get more field paths.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{listSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Staff Planning
	#' @return A dataframe or of PositionTypePlanPositionDistributionSummary
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	getPositionTypePlanPositionDistributionSummary <- function(PositionTypePlanPositionDistributionSummaryID, PlanPositionDistributionIDFirst = F, PlanGroupID = F, PositionTypeID = F, FTE = F, fieldPaths = NULL, entityId = 1, schoolYearId = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment()) %>% purrr::keep(names(.) != "PositionTypePlanPositionDistributionSummaryID")

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		getSkyObject(module = "StaffPlanning", objectName = "PositionTypePlanPositionDistributionSummary", objectId = PositionTypePlanPositionDistributionSummaryID, searchFields = searchFields, entityId = entityId, flatten = flatten, returnResponse = returnResponse)
	}

	#' Delete a PositionTypePlanPositionDistributionSummary
	#'
	#' This function deletes a PositionTypePlanPositionDistributionSummary
	#' @param PositionTypePlanPositionDistributionSummaryID The ID of the PositionTypePlanPositionDistributionSummary to delete
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{listSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Staff Planning
	#' @return The PositionTypePlanPositionDistributionSummaryID of the deleted PositionTypePlanPositionDistributionSummary.
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	deletePositionTypePlanPositionDistributionSummary <- function(PositionTypePlanPositionDistributionSummaryID, ignoreWarnings = F, entityId = 1, schoolYearId = NULL, flatten = T, returnResponse = F){

		deleteSkyObject(module = "StaffPlanning", objectName = "PositionTypePlanPositionDistributionSummary", objectId = PositionTypePlanPositionDistributionSummaryID, ignoreWarnings = ignoreWarnings, entityId = entityId, flatten = flatten, returnResponse = returnResponse)
	}

	#' Create a PositionTypePlanPositionDistributionSummary
	#'
	#' This function creates a PositionTypePlanPositionDistributionSummary
	#' @param fieldNames The field values to give the created PositionTypePlanPositionDistributionSummary. Each defaults to NULL.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{listSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Staff Planning
	#' @return A newly created PositionTypePlanPositionDistributionSummary
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	createPositionTypePlanPositionDistributionSummary <- function(PlanGroupID = NULL, PositionTypeID = NULL, entityId = 1, schoolYearId = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment())

		body <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper()) %>% purrr::compact()

		createSkyObject(module = "StaffPlanning", objectName = "PositionTypePlanPositionDistributionSummary", body = list(DataObject = body), searchFields = append("PositionTypePlanPositionDistributionSummaryID", body %>% names()), entityId = entityId, schoolYearId = schoolYearId, flatten = flatten, returnResponse = returnResponse)
	}

	#' Modify a PositionTypePlanPositionDistributionSummary
	#'
	#' This function modifies a PositionTypePlanPositionDistributionSummary
	#' @param fieldNames The field values to give the modified PositionTypePlanPositionDistributionSummary. Each defaults to NULL.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{listSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Staff Planning
	#' @return The modified PositionTypePlanPositionDistributionSummary
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	modifyPositionTypePlanPositionDistributionSummary <- function(PositionTypePlanPositionDistributionSummaryID, PlanGroupID = NULL, PositionTypeID = NULL, entityId = 1, schoolYearId = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment())

		body <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper()) %>% purrr::compact()

		modifySkyObject(module = "StaffPlanning", objectName = "PositionTypePlanPositionDistributionSummary", objectId = PositionTypePlanPositionDistributionSummaryID, body = list(DataObject = body), searchFields = append("PositionTypePlanPositionDistributionSummaryID", body %>% names()), entityId = entityId, schoolYearId = schoolYearId, flatten = flatten, returnResponse = returnResponse)
	}

	#' List TempPlanPositionDistributionAccountDistributions
	#'
	#' This function returns a dataframe or json object of TempPlanPositionDistributionAccountDistributions
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given TempPlanPositionDistributionAccountDistributions. Defaults to FALSE for all return fields which, for convenience, returns all fields for the TempPlanPositionDistributionAccountDistributions.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object listed as text. Run \code{\link{getSchemaForObjects}}('TempPlanPositionDistributionAccountDistribution') to get more field paths.
	#' @param searchConditionsList A list of search conditions to filter results which are joined by the searchConditionsGroupType. Of the form {FieldName} {ConditionType} {SearchCondition}. For example, c('StudentID LessEqual 500', 'LastName Like Ander\%'). Run \code{\link{listSearchConditionTypes}} for a list of ConditionTypes. Defaults to NULL (unfiltered).
	#' @param searchConditionsGroupType The conjunction which joins multiple searchConditions in the searchConditionsList. Either 'Or' or 'And'. Defaults to 'And'.
	#' @param searchSortFieldNamesList The list of fields sort results by. Defaults to NULL (unsorted).
	#' @param searchSortFieldNamesDescendingList A list of T/F values corresponding to whether to sort each field in searchSortFieldNamesList in descending order. Defaults to F for each FieldName in searchSortFieldNamesList.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{listSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param page Results are paginated. The page of results to return. Default is 1.
	#' @param pageSize Results are paginated. The number of records per page to return. Default is 100,000 (essentially all records for most objects).
	#' @param flatten Whether to flatten results into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Staff Planning
	#' @return A list of TempPlanPositionDistributionAccountDistributions
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	listTempPlanPositionDistributionAccountDistributions <- function(searchConditionsList = NULL, TempPlanPositionDistributionAccountDistributionID = F, TempPlanPositionDistributionID = F, AccountID = F, DistributionPercent = F, StateConcordDepartmentTNID = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, fieldPaths = NULL, searchConditionsGroupType = "And", searchSortFieldNamesList = NULL, searchSortFieldNamesDescendingList = NULL, entityId = 1, schoolYearId = NULL, page = 1, pageSize = 100000, flatten = T, returnResponse = F){

		params <- as.list(environment())

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		listSkyObjects(module = "StaffPlanning", objectName = "TempPlanPositionDistributionAccountDistribution", searchFields = searchFields %>% append(fieldPaths), page = page, pageSize = pageSize, SearchConditionsList = searchConditionsList, SearchConditionsGroupType = searchConditionsGroupType, SearchSortFieldNamesList = searchSortFieldNamesList, SearchSortFieldNamesDescendingList = searchSortFieldNamesDescendingList, entityId = entityId, schoolYearId = schoolYearId, flatten = flatten, returnResponse = returnResponse)
	}

	#' Get a TempPlanPositionDistributionAccountDistribution
	#'
	#' This function returns a dataframe or json object of a TempPlanPositionDistributionAccountDistribution
	#' @param TempPlanPositionDistributionAccountDistributionID The ID of the TempPlanPositionDistributionAccountDistribution to return.
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given TempPlanPositionDistributionAccountDistribution. Defaults to FALSE for all return fields which, for convenience, returns all fields for the TempPlanPositionDistributionAccountDistribution.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object listed as text. Run \code{\link{getSchemaForObjects}}('TempPlanPositionDistributionAccountDistribution') to get more field paths.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{listSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Staff Planning
	#' @return A dataframe or of TempPlanPositionDistributionAccountDistribution
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	getTempPlanPositionDistributionAccountDistribution <- function(TempPlanPositionDistributionAccountDistributionID, TempPlanPositionDistributionID = F, AccountID = F, DistributionPercent = F, StateConcordDepartmentTNID = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, fieldPaths = NULL, entityId = 1, schoolYearId = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment()) %>% purrr::keep(names(.) != "TempPlanPositionDistributionAccountDistributionID")

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		getSkyObject(module = "StaffPlanning", objectName = "TempPlanPositionDistributionAccountDistribution", objectId = TempPlanPositionDistributionAccountDistributionID, searchFields = searchFields, entityId = entityId, flatten = flatten, returnResponse = returnResponse)
	}

	#' Delete a TempPlanPositionDistributionAccountDistribution
	#'
	#' This function deletes a TempPlanPositionDistributionAccountDistribution
	#' @param TempPlanPositionDistributionAccountDistributionID The ID of the TempPlanPositionDistributionAccountDistribution to delete
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{listSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Staff Planning
	#' @return The TempPlanPositionDistributionAccountDistributionID of the deleted TempPlanPositionDistributionAccountDistribution.
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	deleteTempPlanPositionDistributionAccountDistribution <- function(TempPlanPositionDistributionAccountDistributionID, ignoreWarnings = F, entityId = 1, schoolYearId = NULL, flatten = T, returnResponse = F){

		deleteSkyObject(module = "StaffPlanning", objectName = "TempPlanPositionDistributionAccountDistribution", objectId = TempPlanPositionDistributionAccountDistributionID, ignoreWarnings = ignoreWarnings, entityId = entityId, flatten = flatten, returnResponse = returnResponse)
	}

	#' Create a TempPlanPositionDistributionAccountDistribution
	#'
	#' This function creates a TempPlanPositionDistributionAccountDistribution
	#' @param fieldNames The field values to give the created TempPlanPositionDistributionAccountDistribution. Each defaults to NULL.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{listSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Staff Planning
	#' @return A newly created TempPlanPositionDistributionAccountDistribution
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	createTempPlanPositionDistributionAccountDistribution <- function(TempPlanPositionDistributionID = NULL, AccountID = NULL, DistributionPercent = NULL, StateConcordDepartmentTNID = NULL, entityId = 1, schoolYearId = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment())

		body <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper()) %>% purrr::compact()

		createSkyObject(module = "StaffPlanning", objectName = "TempPlanPositionDistributionAccountDistribution", body = list(DataObject = body), searchFields = append("TempPlanPositionDistributionAccountDistributionID", body %>% names()), entityId = entityId, schoolYearId = schoolYearId, flatten = flatten, returnResponse = returnResponse)
	}

	#' Modify a TempPlanPositionDistributionAccountDistribution
	#'
	#' This function modifies a TempPlanPositionDistributionAccountDistribution
	#' @param fieldNames The field values to give the modified TempPlanPositionDistributionAccountDistribution. Each defaults to NULL.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{listSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Staff Planning
	#' @return The modified TempPlanPositionDistributionAccountDistribution
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	modifyTempPlanPositionDistributionAccountDistribution <- function(TempPlanPositionDistributionAccountDistributionID, TempPlanPositionDistributionID = NULL, AccountID = NULL, DistributionPercent = NULL, StateConcordDepartmentTNID = NULL, entityId = 1, schoolYearId = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment())

		body <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper()) %>% purrr::compact()

		modifySkyObject(module = "StaffPlanning", objectName = "TempPlanPositionDistributionAccountDistribution", objectId = TempPlanPositionDistributionAccountDistributionID, body = list(DataObject = body), searchFields = append("TempPlanPositionDistributionAccountDistributionID", body %>% names()), entityId = entityId, schoolYearId = schoolYearId, flatten = flatten, returnResponse = returnResponse)
	}

	#' List PlanPositionDistributionAccountDistributions
	#'
	#' This function returns a dataframe or json object of PlanPositionDistributionAccountDistributions
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given PlanPositionDistributionAccountDistributions. Defaults to FALSE for all return fields which, for convenience, returns all fields for the PlanPositionDistributionAccountDistributions.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object listed as text. Run \code{\link{getSchemaForObjects}}('PlanPositionDistributionAccountDistribution') to get more field paths.
	#' @param searchConditionsList A list of search conditions to filter results which are joined by the searchConditionsGroupType. Of the form {FieldName} {ConditionType} {SearchCondition}. For example, c('StudentID LessEqual 500', 'LastName Like Ander\%'). Run \code{\link{listSearchConditionTypes}} for a list of ConditionTypes. Defaults to NULL (unfiltered).
	#' @param searchConditionsGroupType The conjunction which joins multiple searchConditions in the searchConditionsList. Either 'Or' or 'And'. Defaults to 'And'.
	#' @param searchSortFieldNamesList The list of fields sort results by. Defaults to NULL (unsorted).
	#' @param searchSortFieldNamesDescendingList A list of T/F values corresponding to whether to sort each field in searchSortFieldNamesList in descending order. Defaults to F for each FieldName in searchSortFieldNamesList.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{listSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param page Results are paginated. The page of results to return. Default is 1.
	#' @param pageSize Results are paginated. The number of records per page to return. Default is 100,000 (essentially all records for most objects).
	#' @param flatten Whether to flatten results into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Staff Planning
	#' @return A list of PlanPositionDistributionAccountDistributions
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	listPlanPositionDistributionAccountDistributions <- function(searchConditionsList = NULL, PlanPositionDistributionAccountDistributionID = F, PlanPositionDistributionID = F, AccountID = F, DistributionPercent = F, ProratedDistributionPercent = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, fieldPaths = NULL, searchConditionsGroupType = "And", searchSortFieldNamesList = NULL, searchSortFieldNamesDescendingList = NULL, entityId = 1, schoolYearId = NULL, page = 1, pageSize = 100000, flatten = T, returnResponse = F){

		params <- as.list(environment())

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		listSkyObjects(module = "StaffPlanning", objectName = "PlanPositionDistributionAccountDistribution", searchFields = searchFields %>% append(fieldPaths), page = page, pageSize = pageSize, SearchConditionsList = searchConditionsList, SearchConditionsGroupType = searchConditionsGroupType, SearchSortFieldNamesList = searchSortFieldNamesList, SearchSortFieldNamesDescendingList = searchSortFieldNamesDescendingList, entityId = entityId, schoolYearId = schoolYearId, flatten = flatten, returnResponse = returnResponse)
	}

	#' Get a PlanPositionDistributionAccountDistribution
	#'
	#' This function returns a dataframe or json object of a PlanPositionDistributionAccountDistribution
	#' @param PlanPositionDistributionAccountDistributionID The ID of the PlanPositionDistributionAccountDistribution to return.
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given PlanPositionDistributionAccountDistribution. Defaults to FALSE for all return fields which, for convenience, returns all fields for the PlanPositionDistributionAccountDistribution.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object listed as text. Run \code{\link{getSchemaForObjects}}('PlanPositionDistributionAccountDistribution') to get more field paths.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{listSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Staff Planning
	#' @return A dataframe or of PlanPositionDistributionAccountDistribution
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	getPlanPositionDistributionAccountDistribution <- function(PlanPositionDistributionAccountDistributionID, PlanPositionDistributionID = F, AccountID = F, DistributionPercent = F, ProratedDistributionPercent = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, fieldPaths = NULL, entityId = 1, schoolYearId = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment()) %>% purrr::keep(names(.) != "PlanPositionDistributionAccountDistributionID")

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		getSkyObject(module = "StaffPlanning", objectName = "PlanPositionDistributionAccountDistribution", objectId = PlanPositionDistributionAccountDistributionID, searchFields = searchFields, entityId = entityId, flatten = flatten, returnResponse = returnResponse)
	}

	#' Delete a PlanPositionDistributionAccountDistribution
	#'
	#' This function deletes a PlanPositionDistributionAccountDistribution
	#' @param PlanPositionDistributionAccountDistributionID The ID of the PlanPositionDistributionAccountDistribution to delete
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{listSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Staff Planning
	#' @return The PlanPositionDistributionAccountDistributionID of the deleted PlanPositionDistributionAccountDistribution.
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	deletePlanPositionDistributionAccountDistribution <- function(PlanPositionDistributionAccountDistributionID, ignoreWarnings = F, entityId = 1, schoolYearId = NULL, flatten = T, returnResponse = F){

		deleteSkyObject(module = "StaffPlanning", objectName = "PlanPositionDistributionAccountDistribution", objectId = PlanPositionDistributionAccountDistributionID, ignoreWarnings = ignoreWarnings, entityId = entityId, flatten = flatten, returnResponse = returnResponse)
	}

	#' Create a PlanPositionDistributionAccountDistribution
	#'
	#' This function creates a PlanPositionDistributionAccountDistribution
	#' @param fieldNames The field values to give the created PlanPositionDistributionAccountDistribution. Each defaults to NULL.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{listSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Staff Planning
	#' @return A newly created PlanPositionDistributionAccountDistribution
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	createPlanPositionDistributionAccountDistribution <- function(PlanPositionDistributionID = NULL, AccountID = NULL, DistributionPercent = NULL, entityId = 1, schoolYearId = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment())

		body <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper()) %>% purrr::compact()

		createSkyObject(module = "StaffPlanning", objectName = "PlanPositionDistributionAccountDistribution", body = list(DataObject = body), searchFields = append("PlanPositionDistributionAccountDistributionID", body %>% names()), entityId = entityId, schoolYearId = schoolYearId, flatten = flatten, returnResponse = returnResponse)
	}

	#' Modify a PlanPositionDistributionAccountDistribution
	#'
	#' This function modifies a PlanPositionDistributionAccountDistribution
	#' @param fieldNames The field values to give the modified PlanPositionDistributionAccountDistribution. Each defaults to NULL.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{listSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Staff Planning
	#' @return The modified PlanPositionDistributionAccountDistribution
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	modifyPlanPositionDistributionAccountDistribution <- function(PlanPositionDistributionAccountDistributionID, PlanPositionDistributionID = NULL, AccountID = NULL, DistributionPercent = NULL, entityId = 1, schoolYearId = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment())

		body <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper()) %>% purrr::compact()

		modifySkyObject(module = "StaffPlanning", objectName = "PlanPositionDistributionAccountDistribution", objectId = PlanPositionDistributionAccountDistributionID, body = list(DataObject = body), searchFields = append("PlanPositionDistributionAccountDistributionID", body %>% names()), entityId = entityId, schoolYearId = schoolYearId, flatten = flatten, returnResponse = returnResponse)
	}
