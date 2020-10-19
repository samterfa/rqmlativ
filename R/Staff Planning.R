

	listPlanGroups <- function(searchConditionsList = NULL, PlanGroupID = F, Code = F, Description = F, CodeDescription = F, PlanID = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, IsUnassigned = F, BudgetedFTE = F, AssignedFTE = F, AvailableFTE = F, NumberOfVacantPositions = F, CurrentUserHasPlanGroupAccess = F, NumberOfOccupiedPositions = F, searchConditionsGroupType = "And", searchSortFieldNamesList = NULL, searchSortFieldNamesDescendingList = NULL, entityID = 1, schoolYearID = NULL, page = 1, pageSize = 100000, flatten = T, ...){

		params <- as.list(environment()) %>% append(list(...))

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- "all", searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		listSkyObjects(module = "StaffPlanning", objectName = "PlanGroup", schoolYearID = schoolYearID, searchFields = searchFields, page = page, pageSize = pageSize, SearchConditionsList = searchConditionsList, SearchConditionsGroupType = searchConditionsGroupType, SearchSortFieldNamesList = searchSortFieldNamesList, SearchSortFieldNamesDescendingList = searchSortFieldNamesDescendingList, entityId = entityID, flatten = flatten)
	}


	listPlanPositions <- function(searchConditionsList = NULL, PlanPositionID = F, PlanID = F, PositionNumberID = F, BudgetedFTE = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, PlanEmployeeID = F, Description = F, PositionIdentifier = F, AssignedFTE = F, AvailableFTE = F, MatrixID = F, StepNumber = F, CalendarID = F, FullPaidDays = F, FullPaySecondsPerDay = F, Rate = F, AnnualPay = F, CurrentUserHasPlanGroupAccess = F, PositionGroupID = F, JobTypeID = F, PositionIDClonedFrom = F, CalculationStatus = F, TotalBenefitAmount = F, TotalPayAmount = F, SalaryCalculationMethodID = F, TotalCompensationAmount = F, LaneID = F, RequiredCredits = F, PositionTypeID = F, FormattedFullPaySecondsPerDayDecimal = F, PlanPositionDistributionsBuildingCodes = F, PlanPositionDistributionsBuildingDescriptions = F, PlanPositionDistributionsAssignmentTypeCodes = F, PlanPositionDistributionsAssignmentTypeDescriptions = F, PlacementID = F, PlanPositionDistributionsDepartmentCodes = F, PlanPositionDistributionsDepartmentDescriptions = F, PlanPositionDistributionsFTEGroupCodes = F, PlanPositionDistributionsFTEGroupDescriptions = F, EmployeePlacementDescription = F, searchConditionsGroupType = "And", searchSortFieldNamesList = NULL, searchSortFieldNamesDescendingList = NULL, entityID = 1, schoolYearID = NULL, page = 1, pageSize = 100000, flatten = T, ...){

		params <- as.list(environment()) %>% append(list(...))

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- "all", searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		listSkyObjects(module = "StaffPlanning", objectName = "PlanPosition", schoolYearID = schoolYearID, searchFields = searchFields, page = page, pageSize = pageSize, SearchConditionsList = searchConditionsList, SearchConditionsGroupType = searchConditionsGroupType, SearchSortFieldNamesList = searchSortFieldNamesList, SearchSortFieldNamesDescendingList = searchSortFieldNamesDescendingList, entityId = entityID, flatten = flatten)
	}


	listStaffPlanningPlans <- function(searchConditionsList = NULL, PlanID = F, DistrictID = F, FiscalYearID = F, Description = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, NumberOfOccupiedPositions = F, NumberOfVacantPositions = F, CurrentUserHasPlanGroupAccess = F, CalculationStatusCode = F, TotalBenefitAmount = F, TotalPayAmount = F, TotalCompensationAmount = F, IsBudgeted = F, BudgetVersionAmount = F, BudgetVersionDifference = F, searchConditionsGroupType = "And", searchSortFieldNamesList = NULL, searchSortFieldNamesDescendingList = NULL, entityID = 1, schoolYearID = NULL, page = 1, pageSize = 100000, flatten = T, ...){

		params <- as.list(environment()) %>% append(list(...))

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- "all", searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		listSkyObjects(module = "StaffPlanning", objectName = "Plan", schoolYearID = schoolYearID, searchFields = searchFields, page = page, pageSize = pageSize, SearchConditionsList = searchConditionsList, SearchConditionsGroupType = searchConditionsGroupType, SearchSortFieldNamesList = searchSortFieldNamesList, SearchSortFieldNamesDescendingList = searchSortFieldNamesDescendingList, entityId = entityID, flatten = flatten)
	}


	listPlanPositionDistributions <- function(searchConditionsList = NULL, PlanPositionDistributionID = F, PlanGroupID = F, PlanPositionID = F, AssignmentTypeID = F, BuildingID = F, FTE = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, CurrentUserHasPlanGroupAccess = F, DepartmentID = F, FTEGroupID = F, searchConditionsGroupType = "And", searchSortFieldNamesList = NULL, searchSortFieldNamesDescendingList = NULL, entityID = 1, schoolYearID = NULL, page = 1, pageSize = 100000, flatten = T, ...){

		params <- as.list(environment()) %>% append(list(...))

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- "all", searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		listSkyObjects(module = "StaffPlanning", objectName = "PlanPositionDistribution", schoolYearID = schoolYearID, searchFields = searchFields, page = page, pageSize = pageSize, SearchConditionsList = searchConditionsList, SearchConditionsGroupType = searchConditionsGroupType, SearchSortFieldNamesList = searchSortFieldNamesList, SearchSortFieldNamesDescendingList = searchSortFieldNamesDescendingList, entityId = entityID, flatten = flatten)
	}


	listPlanGroupClearances <- function(searchConditionsList = NULL, PlanGroupClearanceID = F, PlanGroupID = F, SecurityGroupID = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, CurrentUserHasPlanGroupAccess = F, searchConditionsGroupType = "And", searchSortFieldNamesList = NULL, searchSortFieldNamesDescendingList = NULL, entityID = 1, schoolYearID = NULL, page = 1, pageSize = 100000, flatten = T, ...){

		params <- as.list(environment()) %>% append(list(...))

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- "all", searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		listSkyObjects(module = "StaffPlanning", objectName = "PlanGroupClearance", schoolYearID = schoolYearID, searchFields = searchFields, page = page, pageSize = pageSize, SearchConditionsList = searchConditionsList, SearchConditionsGroupType = searchConditionsGroupType, SearchSortFieldNamesList = searchSortFieldNamesList, SearchSortFieldNamesDescendingList = searchSortFieldNamesDescendingList, entityId = entityID, flatten = flatten)
	}


	listPlanEmployees <- function(searchConditionsList = NULL, PlanEmployeeID = F, EmployeeID = F, PlanID = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, AssignedFTE = F, CurrentUserHasPlanGroupAccess = F, TotalBenefitAmount = F, TotalPayAmount = F, TotalCompensationAmount = F, searchConditionsGroupType = "And", searchSortFieldNamesList = NULL, searchSortFieldNamesDescendingList = NULL, entityID = 1, schoolYearID = NULL, page = 1, pageSize = 100000, flatten = T, ...){

		params <- as.list(environment()) %>% append(list(...))

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- "all", searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		listSkyObjects(module = "StaffPlanning", objectName = "PlanEmployee", schoolYearID = schoolYearID, searchFields = searchFields, page = page, pageSize = pageSize, SearchConditionsList = searchConditionsList, SearchConditionsGroupType = searchConditionsGroupType, SearchSortFieldNamesList = searchSortFieldNamesList, SearchSortFieldNamesDescendingList = searchSortFieldNamesDescendingList, entityId = entityID, flatten = flatten)
	}


	listPlanGroupBuildings <- function(searchConditionsList = NULL, PlanGroupBuildingID = F, PlanGroupID = F, BuildingID = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, CurrentUserHasPlanGroupAccess = F, searchConditionsGroupType = "And", searchSortFieldNamesList = NULL, searchSortFieldNamesDescendingList = NULL, entityID = 1, schoolYearID = NULL, page = 1, pageSize = 100000, flatten = T, ...){

		params <- as.list(environment()) %>% append(list(...))

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- "all", searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		listSkyObjects(module = "StaffPlanning", objectName = "PlanGroupBuilding", schoolYearID = schoolYearID, searchFields = searchFields, page = page, pageSize = pageSize, SearchConditionsList = searchConditionsList, SearchConditionsGroupType = searchConditionsGroupType, SearchSortFieldNamesList = searchSortFieldNamesList, SearchSortFieldNamesDescendingList = searchSortFieldNamesDescendingList, entityId = entityID, flatten = flatten)
	}


	listPlanGroupAssignmentTypes <- function(searchConditionsList = NULL, PlanGroupAssignmentTypeID = F, PlanGroupID = F, AssignmentTypeID = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, CurrentUserHasPlanGroupAccess = F, searchConditionsGroupType = "And", searchSortFieldNamesList = NULL, searchSortFieldNamesDescendingList = NULL, entityID = 1, schoolYearID = NULL, page = 1, pageSize = 100000, flatten = T, ...){

		params <- as.list(environment()) %>% append(list(...))

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- "all", searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		listSkyObjects(module = "StaffPlanning", objectName = "PlanGroupAssignmentType", schoolYearID = schoolYearID, searchFields = searchFields, page = page, pageSize = pageSize, SearchConditionsList = searchConditionsList, SearchConditionsGroupType = searchConditionsGroupType, SearchSortFieldNamesList = searchSortFieldNamesList, SearchSortFieldNamesDescendingList = searchSortFieldNamesDescendingList, entityId = entityID, flatten = flatten)
	}


	listPlanGroupPositionTypes <- function(searchConditionsList = NULL, PlanGroupPositionTypeID = F, PlanGroupID = F, PositionTypeID = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, CurrentUserHasPlanGroupAccess = F, searchConditionsGroupType = "And", searchSortFieldNamesList = NULL, searchSortFieldNamesDescendingList = NULL, entityID = 1, schoolYearID = NULL, page = 1, pageSize = 100000, flatten = T, ...){

		params <- as.list(environment()) %>% append(list(...))

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- "all", searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		listSkyObjects(module = "StaffPlanning", objectName = "PlanGroupPositionType", schoolYearID = schoolYearID, searchFields = searchFields, page = page, pageSize = pageSize, SearchConditionsList = searchConditionsList, SearchConditionsGroupType = searchConditionsGroupType, SearchSortFieldNamesList = searchSortFieldNamesList, SearchSortFieldNamesDescendingList = searchSortFieldNamesDescendingList, entityId = entityID, flatten = flatten)
	}


	listTempPlanPositionDistributions <- function(searchConditionsList = NULL, TempPlanPositionDistributionID = F, TempPlanPositionEmployeeID = F, AssignmentTypeID = F, BuildingID = F, FTE = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, FTEGroupID = F, DepartmentID = F, searchConditionsGroupType = "And", searchSortFieldNamesList = NULL, searchSortFieldNamesDescendingList = NULL, entityID = 1, schoolYearID = NULL, page = 1, pageSize = 100000, flatten = T, ...){

		params <- as.list(environment()) %>% append(list(...))

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- "all", searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		listSkyObjects(module = "StaffPlanning", objectName = "TempPlanPositionDistribution", schoolYearID = schoolYearID, searchFields = searchFields, page = page, pageSize = pageSize, SearchConditionsList = searchConditionsList, SearchConditionsGroupType = searchConditionsGroupType, SearchSortFieldNamesList = searchSortFieldNamesList, SearchSortFieldNamesDescendingList = searchSortFieldNamesDescendingList, entityId = entityID, flatten = flatten)
	}


	listTempPlanPositionEmployees <- function(searchConditionsList = NULL, TempPlanPositionEmployeeID = F, PositionNumberID = F, PositionNumberCode = F, PositionIdentifier = F, PositionTypeCodeDescription = F, BudgetedFTE = F, EmployeeID = F, EmployeeFullNameLFM = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, MatrixID = F, CalendarID = F, FullPaidDays = F, FormattedFullPaySecondsPerDay = F, Rate = F, StepNumber = F, SalaryCalculationMethodID = F, AmountTypeCode = F, AmountType = F, AnnualPay = F, FullPaySecondsPerDay = F, PositionGroupID = F, JobTypeID = F, PositionIDClonedFrom = F, OldBudgetedFTE = F, MatrixCode = F, OldMatrixCode = F, LaneID = F, OldLaneCode = F, LaneCode = F, RequiredCredits = F, OldRequiredCredits = F, CalendarCode = F, OldCalendarCode = F, OldFormattedFullPaySecondsPerDay = F, OldRate = F, OldStepNumber = F, SalaryCalculationMethodCode = F, OldSalaryCalculationMethodCode = F, OldAnnualPay = F, OldFullPaySecondsPerDay = F, Description = F, PlanPositionID = F, EmployeeNumber = F, PositionTypeID = F, AssignmentTypeCodes = F, AssignmentTypeDescriptions = F, BuildingCodes = F, BuildingDescriptions = F, PlacementID = F, PlacementCode = F, OldPlacementCode = F, HasFatalException = F, ErrorMessage = F, MidpointGroup = F, CappedAtMaximumRate = F, MidpointGroupID = F, MidpointGroupCodeDescription = F, OldMidpointGroup = F, TRSStateBaseStep = F, OldTRSStateBaseStep = F, TRSStateBaseStepID = F, OldTRSStateBaseStepAmount = F, TRSStateBaseStepAmount = F, ConfigFiscalYearTRSStateBaseStepID = F, ConfigFiscalYearTRSStateBaseStepLaneCodeStepNumber = F, EmployeePlacementDescription = F, OldEmployeePlacementDescription = F, PositionTypeCode = F, ErrorCount = F, searchConditionsGroupType = "And", searchSortFieldNamesList = NULL, searchSortFieldNamesDescendingList = NULL, entityID = 1, schoolYearID = NULL, page = 1, pageSize = 100000, flatten = T, ...){

		params <- as.list(environment()) %>% append(list(...))

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- "all", searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		listSkyObjects(module = "StaffPlanning", objectName = "TempPlanPositionEmployee", schoolYearID = schoolYearID, searchFields = searchFields, page = page, pageSize = pageSize, SearchConditionsList = searchConditionsList, SearchConditionsGroupType = searchConditionsGroupType, SearchSortFieldNamesList = searchSortFieldNamesList, SearchSortFieldNamesDescendingList = searchSortFieldNamesDescendingList, entityId = entityID, flatten = flatten)
	}


	listAssignmentTypePlanPositionDistributionSummaries <- function(searchConditionsList = NULL, PlanPositionDistributionIDFirst = F, PlanGroupID = F, PositionTypeID = F, AssignmentTypeID = F, FTE = F, searchConditionsGroupType = "And", searchSortFieldNamesList = NULL, searchSortFieldNamesDescendingList = NULL, entityID = 1, schoolYearID = NULL, page = 1, pageSize = 100000, flatten = T, ...){

		params <- as.list(environment()) %>% append(list(...))

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- "all", searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		listSkyObjects(module = "StaffPlanning", objectName = "AssignmentTypePlanPositionDistributionSummary", schoolYearID = schoolYearID, searchFields = searchFields, page = page, pageSize = pageSize, SearchConditionsList = searchConditionsList, SearchConditionsGroupType = searchConditionsGroupType, SearchSortFieldNamesList = searchSortFieldNamesList, SearchSortFieldNamesDescendingList = searchSortFieldNamesDescendingList, entityId = entityID, flatten = flatten)
	}


	listBuildingPlanPositionDistributionSummaries <- function(searchConditionsList = NULL, PlanPositionDistributionIDFirst = F, PlanGroupID = F, PositionTypeID = F, BuildingID = F, FTE = F, searchConditionsGroupType = "And", searchSortFieldNamesList = NULL, searchSortFieldNamesDescendingList = NULL, entityID = 1, schoolYearID = NULL, page = 1, pageSize = 100000, flatten = T, ...){

		params <- as.list(environment()) %>% append(list(...))

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- "all", searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		listSkyObjects(module = "StaffPlanning", objectName = "BuildingPlanPositionDistributionSummary", schoolYearID = schoolYearID, searchFields = searchFields, page = page, pageSize = pageSize, SearchConditionsList = searchConditionsList, SearchConditionsGroupType = searchConditionsGroupType, SearchSortFieldNamesList = searchSortFieldNamesList, SearchSortFieldNamesDescendingList = searchSortFieldNamesDescendingList, entityId = entityID, flatten = flatten)
	}


	listPlanPositionPayAccountCalculations <- function(searchConditionsList = NULL, PlanPositionPayAccountCalculationID = F, PlanPositionPayAccountDistributionID = F, AnnualAmount = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, searchConditionsGroupType = "And", searchSortFieldNamesList = NULL, searchSortFieldNamesDescendingList = NULL, entityID = 1, schoolYearID = NULL, page = 1, pageSize = 100000, flatten = T, ...){

		params <- as.list(environment()) %>% append(list(...))

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- "all", searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		listSkyObjects(module = "StaffPlanning", objectName = "PlanPositionPayAccountCalculation", schoolYearID = schoolYearID, searchFields = searchFields, page = page, pageSize = pageSize, SearchConditionsList = searchConditionsList, SearchConditionsGroupType = searchConditionsGroupType, SearchSortFieldNamesList = searchSortFieldNamesList, SearchSortFieldNamesDescendingList = searchSortFieldNamesDescendingList, entityId = entityID, flatten = flatten)
	}


	listPlanPositionPayBenefits <- function(searchConditionsList = NULL, PlanPositionPayBenefitID = F, PlanPositionPayID = F, PlanPositionBenefitID = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, searchConditionsGroupType = "And", searchSortFieldNamesList = NULL, searchSortFieldNamesDescendingList = NULL, entityID = 1, schoolYearID = NULL, page = 1, pageSize = 100000, flatten = T, ...){

		params <- as.list(environment()) %>% append(list(...))

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- "all", searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		listSkyObjects(module = "StaffPlanning", objectName = "PlanPositionPayBenefit", schoolYearID = schoolYearID, searchFields = searchFields, page = page, pageSize = pageSize, SearchConditionsList = searchConditionsList, SearchConditionsGroupType = searchConditionsGroupType, SearchSortFieldNamesList = searchSortFieldNamesList, SearchSortFieldNamesDescendingList = searchSortFieldNamesDescendingList, entityId = entityID, flatten = flatten)
	}


	listPlanPositionPayBenefitAccounts <- function(searchConditionsList = NULL, PlanPositionPayBenefitAccountID = F, PlanPositionPayAccountDistributionID = F, PlanPositionPayBenefitID = F, AnnualAmount = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, AccountID = F, searchConditionsGroupType = "And", searchSortFieldNamesList = NULL, searchSortFieldNamesDescendingList = NULL, entityID = 1, schoolYearID = NULL, page = 1, pageSize = 100000, flatten = T, ...){

		params <- as.list(environment()) %>% append(list(...))

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- "all", searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		listSkyObjects(module = "StaffPlanning", objectName = "PlanPositionPayBenefitAccount", schoolYearID = schoolYearID, searchFields = searchFields, page = page, pageSize = pageSize, SearchConditionsList = searchConditionsList, SearchConditionsGroupType = searchConditionsGroupType, SearchSortFieldNamesList = searchSortFieldNamesList, SearchSortFieldNamesDescendingList = searchSortFieldNamesDescendingList, entityId = entityID, flatten = flatten)
	}


	listPlanPositionPays <- function(searchConditionsList = NULL, PlanPositionPayID = F, PlanPositionID = F, PayTypeID = F, Type = F, StipendAmount = F, AccountDistributionString = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, TotalAnnualAmountCalculated = F, PayScheduleID = F, AssignmentPayTypeIDOriginal = F, TotalAmount = F, searchConditionsGroupType = "And", searchSortFieldNamesList = NULL, searchSortFieldNamesDescendingList = NULL, entityID = 1, schoolYearID = NULL, page = 1, pageSize = 100000, flatten = T, ...){

		params <- as.list(environment()) %>% append(list(...))

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- "all", searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		listSkyObjects(module = "StaffPlanning", objectName = "PlanPositionPay", schoolYearID = schoolYearID, searchFields = searchFields, page = page, pageSize = pageSize, SearchConditionsList = searchConditionsList, SearchConditionsGroupType = searchConditionsGroupType, SearchSortFieldNamesList = searchSortFieldNamesList, SearchSortFieldNamesDescendingList = searchSortFieldNamesDescendingList, entityId = entityID, flatten = flatten)
	}


	listPlanPositionPayAccountDistributions <- function(searchConditionsList = NULL, PlanPositionPayAccountDistributionID = F, PlanPositionPayID = F, AccountID = F, DistributionPercent = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, searchConditionsGroupType = "And", searchSortFieldNamesList = NULL, searchSortFieldNamesDescendingList = NULL, entityID = 1, schoolYearID = NULL, page = 1, pageSize = 100000, flatten = T, ...){

		params <- as.list(environment()) %>% append(list(...))

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- "all", searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		listSkyObjects(module = "StaffPlanning", objectName = "PlanPositionPayAccountDistribution", schoolYearID = schoolYearID, searchFields = searchFields, page = page, pageSize = pageSize, SearchConditionsList = searchConditionsList, SearchConditionsGroupType = searchConditionsGroupType, SearchSortFieldNamesList = searchSortFieldNamesList, SearchSortFieldNamesDescendingList = searchSortFieldNamesDescendingList, entityId = entityID, flatten = flatten)
	}


	listPlanPositionBenefits <- function(searchConditionsList = NULL, PlanPositionBenefitID = F, PlanPositionID = F, BenefitID = F, CalculationType = F, Value = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, TotalAnnualAmountCalculated = F, searchConditionsGroupType = "And", searchSortFieldNamesList = NULL, searchSortFieldNamesDescendingList = NULL, entityID = 1, schoolYearID = NULL, page = 1, pageSize = 100000, flatten = T, ...){

		params <- as.list(environment()) %>% append(list(...))

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- "all", searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		listSkyObjects(module = "StaffPlanning", objectName = "PlanPositionBenefit", schoolYearID = schoolYearID, searchFields = searchFields, page = page, pageSize = pageSize, SearchConditionsList = searchConditionsList, SearchConditionsGroupType = searchConditionsGroupType, SearchSortFieldNamesList = searchSortFieldNamesList, SearchSortFieldNamesDescendingList = searchSortFieldNamesDescendingList, entityId = entityID, flatten = flatten)
	}


	listPlanPositionSupplements <- function(searchConditionsList = NULL, PlanPositionSupplementID = F, PlanPositionID = F, SupplementTypeID = F, Rate = F, AnnualPay = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, searchConditionsGroupType = "And", searchSortFieldNamesList = NULL, searchSortFieldNamesDescendingList = NULL, entityID = 1, schoolYearID = NULL, page = 1, pageSize = 100000, flatten = T, ...){

		params <- as.list(environment()) %>% append(list(...))

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- "all", searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		listSkyObjects(module = "StaffPlanning", objectName = "PlanPositionSupplement", schoolYearID = schoolYearID, searchFields = searchFields, page = page, pageSize = pageSize, SearchConditionsList = searchConditionsList, SearchConditionsGroupType = searchConditionsGroupType, SearchSortFieldNamesList = searchSortFieldNamesList, SearchSortFieldNamesDescendingList = searchSortFieldNamesDescendingList, entityId = entityID, flatten = flatten)
	}


	listTempPlanPositionPayAccountCalculations <- function(searchConditionsList = NULL, TempPlanPositionPayAccountCalculationID = F, PlanPositionPayAccountDistributionID = F, AnnualAmount = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, searchConditionsGroupType = "And", searchSortFieldNamesList = NULL, searchSortFieldNamesDescendingList = NULL, entityID = 1, schoolYearID = NULL, page = 1, pageSize = 100000, flatten = T, ...){

		params <- as.list(environment()) %>% append(list(...))

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- "all", searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		listSkyObjects(module = "StaffPlanning", objectName = "TempPlanPositionPayAccountCalculation", schoolYearID = schoolYearID, searchFields = searchFields, page = page, pageSize = pageSize, SearchConditionsList = searchConditionsList, SearchConditionsGroupType = searchConditionsGroupType, SearchSortFieldNamesList = searchSortFieldNamesList, SearchSortFieldNamesDescendingList = searchSortFieldNamesDescendingList, entityId = entityID, flatten = flatten)
	}


	listTempPlanPositionPayBenefitAccounts <- function(searchConditionsList = NULL, TempPlanPositionPayBenefitAccountID = F, PlanPositionPayAccountDistributionID = F, PlanPositionPayBenefitID = F, AccountID = F, DisplayAccount = F, AnnualAmount = F, IsAccountPreviouslyCreated = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, TempPlanPositionPayBenefitID = F, searchConditionsGroupType = "And", searchSortFieldNamesList = NULL, searchSortFieldNamesDescendingList = NULL, entityID = 1, schoolYearID = NULL, page = 1, pageSize = 100000, flatten = T, ...){

		params <- as.list(environment()) %>% append(list(...))

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- "all", searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		listSkyObjects(module = "StaffPlanning", objectName = "TempPlanPositionPayBenefitAccount", schoolYearID = schoolYearID, searchFields = searchFields, page = page, pageSize = pageSize, SearchConditionsList = searchConditionsList, SearchConditionsGroupType = searchConditionsGroupType, SearchSortFieldNamesList = searchSortFieldNamesList, SearchSortFieldNamesDescendingList = searchSortFieldNamesDescendingList, entityId = entityID, flatten = flatten)
	}


	listTempPlanPositionSupplements <- function(searchConditionsList = NULL, TempPlanPositionSupplementID = F, TempPlanPositionEmployeeID = F, SupplementTypeID = F, Rate = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, AnnualPay = F, PlanPositionID = F, PlanPositionSupplementID = F, SupplementTypeCodeDescription = F, PlanPositionOldAnnualPay = F, PlanPositionNewAnnualPay = F, PlanPositionPositionTypeCodeDescription = F, PlanPositionDistributionsBuildingCodes = F, PlanPositionDistributionsAssignmentTypeCodes = F, PlanPositionEmployeeName = F, PlanPositionEmployeeNumber = F, ErrorCount = F, OldSupplementTypeCodeDescription = F, SupplementTypeAmountTypeCode = F, OldSupplementTypeAmountTypeCode = F, OldRate = F, OldAnnualPay = F, searchConditionsGroupType = "And", searchSortFieldNamesList = NULL, searchSortFieldNamesDescendingList = NULL, entityID = 1, schoolYearID = NULL, page = 1, pageSize = 100000, flatten = T, ...){

		params <- as.list(environment()) %>% append(list(...))

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- "all", searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		listSkyObjects(module = "StaffPlanning", objectName = "TempPlanPositionSupplement", schoolYearID = schoolYearID, searchFields = searchFields, page = page, pageSize = pageSize, SearchConditionsList = searchConditionsList, SearchConditionsGroupType = searchConditionsGroupType, SearchSortFieldNamesList = searchSortFieldNamesList, SearchSortFieldNamesDescendingList = searchSortFieldNamesDescendingList, entityId = entityID, flatten = flatten)
	}


	listTempPlanPositionBenefits <- function(searchConditionsList = NULL, TempPlanPositionBenefitID = F, TempPlanPositionEmployeeID = F, BenefitID = F, Value = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, PlanPositionBenefitID = F, BenefitCodeDescription = F, PositionNumber = F, PlanPositionEmployeeName = F, CalculationType = F, NewValue = F, ErrorCount = F, PlanPositionID = F, PositionTypeDescription = F, PlanPositionEmployeeNumber = F, StateBenefitTXID = F, BenefitTypeTX = F, searchConditionsGroupType = "And", searchSortFieldNamesList = NULL, searchSortFieldNamesDescendingList = NULL, entityID = 1, schoolYearID = NULL, page = 1, pageSize = 100000, flatten = T, ...){

		params <- as.list(environment()) %>% append(list(...))

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- "all", searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		listSkyObjects(module = "StaffPlanning", objectName = "TempPlanPositionBenefit", schoolYearID = schoolYearID, searchFields = searchFields, page = page, pageSize = pageSize, SearchConditionsList = searchConditionsList, SearchConditionsGroupType = searchConditionsGroupType, SearchSortFieldNamesList = searchSortFieldNamesList, SearchSortFieldNamesDescendingList = searchSortFieldNamesDescendingList, entityId = entityID, flatten = flatten)
	}


	listTempPlanPositionPays <- function(searchConditionsList = NULL, TempPlanPositionPayID = F, TempPlanPositionEmployeeID = F, PayTypeID = F, TypeCode = F, StipendAmount = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, AccountDistributionString = F, PayScheduleID = F, PositionTypeCodeDescription = F, AssignmentTypeCodes = F, AssignmentTypeDescriptions = F, BuildingCodes = F, BuildingDescriptions = F, EmployeeFullNameLFM = F, EmployeeNumber = F, PayTypeCodeDescription = F, OldDisplayAccount = F, NewDisplayAccount = F, PlanPositionPayID = F, ErrorCount = F, PayScheduleCodeDescription = F, MatchingExpenditureEligibilityCode = F, NewMatchingExpenditureEligibilityCode = F, AssignmentPayTypeIDOriginal = F, StatePayrollActivityCodeTXID = F, searchConditionsGroupType = "And", searchSortFieldNamesList = NULL, searchSortFieldNamesDescendingList = NULL, entityID = 1, schoolYearID = NULL, page = 1, pageSize = 100000, flatten = T, ...){

		params <- as.list(environment()) %>% append(list(...))

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- "all", searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		listSkyObjects(module = "StaffPlanning", objectName = "TempPlanPositionPay", schoolYearID = schoolYearID, searchFields = searchFields, page = page, pageSize = pageSize, SearchConditionsList = searchConditionsList, SearchConditionsGroupType = searchConditionsGroupType, SearchSortFieldNamesList = searchSortFieldNamesList, SearchSortFieldNamesDescendingList = searchSortFieldNamesDescendingList, entityId = entityID, flatten = flatten)
	}


	listTempPlanPositionPayAccountDistributions <- function(searchConditionsList = NULL, TempPlanPositionPayAccountDistributionID = F, TempPlanPositionPayID = F, AccountID = F, DistributionPercent = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, PlanPositionPayID = F, DisplayAccount = F, StateConcordDepartmentTNID = F, searchConditionsGroupType = "And", searchSortFieldNamesList = NULL, searchSortFieldNamesDescendingList = NULL, entityID = 1, schoolYearID = NULL, page = 1, pageSize = 100000, flatten = T, ...){

		params <- as.list(environment()) %>% append(list(...))

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- "all", searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		listSkyObjects(module = "StaffPlanning", objectName = "TempPlanPositionPayAccountDistribution", schoolYearID = schoolYearID, searchFields = searchFields, page = page, pageSize = pageSize, SearchConditionsList = searchConditionsList, SearchConditionsGroupType = searchConditionsGroupType, SearchSortFieldNamesList = searchSortFieldNamesList, SearchSortFieldNamesDescendingList = searchSortFieldNamesDescendingList, entityId = entityID, flatten = flatten)
	}


	listTempPlanPositionPayBenefits <- function(searchConditionsList = NULL, TempPlanPositionPayBenefitID = F, TempPlanPositionPayID = F, TempPlanPositionBenefitID = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, PlanPositionPayBenefitID = F, PlanPositionPayID = F, BenefitCodeDescription = F, PlanPositionEmployeeName = F, EmployeeNumber = F, PositionTypeDescription = F, PayTypeCodeDescription = F, PayScheduleCodeDescription = F, ErrorCount = F, searchConditionsGroupType = "And", searchSortFieldNamesList = NULL, searchSortFieldNamesDescendingList = NULL, entityID = 1, schoolYearID = NULL, page = 1, pageSize = 100000, flatten = T, ...){

		params <- as.list(environment()) %>% append(list(...))

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- "all", searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		listSkyObjects(module = "StaffPlanning", objectName = "TempPlanPositionPayBenefit", schoolYearID = schoolYearID, searchFields = searchFields, page = page, pageSize = pageSize, SearchConditionsList = searchConditionsList, SearchConditionsGroupType = searchConditionsGroupType, SearchSortFieldNamesList = searchSortFieldNamesList, SearchSortFieldNamesDescendingList = searchSortFieldNamesDescendingList, entityId = entityID, flatten = flatten)
	}


	listTempPlanPositionExceptions <- function(searchConditionsList = NULL, TempPlanPositionExceptionID = F, PositionNumberCode = F, Description = F, PositionIdentifier = F, Error = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, ErrorType = F, searchConditionsGroupType = "And", searchSortFieldNamesList = NULL, searchSortFieldNamesDescendingList = NULL, entityID = 1, schoolYearID = NULL, page = 1, pageSize = 100000, flatten = T, ...){

		params <- as.list(environment()) %>% append(list(...))

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- "all", searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		listSkyObjects(module = "StaffPlanning", objectName = "TempPlanPositionException", schoolYearID = schoolYearID, searchFields = searchFields, page = page, pageSize = pageSize, SearchConditionsList = searchConditionsList, SearchConditionsGroupType = searchConditionsGroupType, SearchSortFieldNamesList = searchSortFieldNamesList, SearchSortFieldNamesDescendingList = searchSortFieldNamesDescendingList, entityId = entityID, flatten = flatten)
	}


	listStaffPlanningTempExceptions <- function(searchConditionsList = NULL, TempExceptionID = F, ErroredObjectID = F, ErrorField = F, Error = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, searchConditionsGroupType = "And", searchSortFieldNamesList = NULL, searchSortFieldNamesDescendingList = NULL, entityID = 1, schoolYearID = NULL, page = 1, pageSize = 100000, flatten = T, ...){

		params <- as.list(environment()) %>% append(list(...))

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- "all", searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		listSkyObjects(module = "StaffPlanning", objectName = "TempException", schoolYearID = schoolYearID, searchFields = searchFields, page = page, pageSize = pageSize, SearchConditionsList = searchConditionsList, SearchConditionsGroupType = searchConditionsGroupType, SearchSortFieldNamesList = searchSortFieldNamesList, SearchSortFieldNamesDescendingList = searchSortFieldNamesDescendingList, entityId = entityID, flatten = flatten)
	}


	listPositionTypePlanPositionDistributionSummaries <- function(searchConditionsList = NULL, PlanPositionDistributionIDFirst = F, PlanGroupID = F, PositionTypeID = F, FTE = F, searchConditionsGroupType = "And", searchSortFieldNamesList = NULL, searchSortFieldNamesDescendingList = NULL, entityID = 1, schoolYearID = NULL, page = 1, pageSize = 100000, flatten = T, ...){

		params <- as.list(environment()) %>% append(list(...))

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- "all", searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		listSkyObjects(module = "StaffPlanning", objectName = "PositionTypePlanPositionDistributionSummary", schoolYearID = schoolYearID, searchFields = searchFields, page = page, pageSize = pageSize, SearchConditionsList = searchConditionsList, SearchConditionsGroupType = searchConditionsGroupType, SearchSortFieldNamesList = searchSortFieldNamesList, SearchSortFieldNamesDescendingList = searchSortFieldNamesDescendingList, entityId = entityID, flatten = flatten)
	}


	listTempPlanPositionDistributionAccountDistributions <- function(searchConditionsList = NULL, TempPlanPositionDistributionAccountDistributionID = F, TempPlanPositionDistributionID = F, AccountID = F, DistributionPercent = F, StateConcordDepartmentTNID = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, searchConditionsGroupType = "And", searchSortFieldNamesList = NULL, searchSortFieldNamesDescendingList = NULL, entityID = 1, schoolYearID = NULL, page = 1, pageSize = 100000, flatten = T, ...){

		params <- as.list(environment()) %>% append(list(...))

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- "all", searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		listSkyObjects(module = "StaffPlanning", objectName = "TempPlanPositionDistributionAccountDistribution", schoolYearID = schoolYearID, searchFields = searchFields, page = page, pageSize = pageSize, SearchConditionsList = searchConditionsList, SearchConditionsGroupType = searchConditionsGroupType, SearchSortFieldNamesList = searchSortFieldNamesList, SearchSortFieldNamesDescendingList = searchSortFieldNamesDescendingList, entityId = entityID, flatten = flatten)
	}


	listPlanPositionDistributionAccountDistributions <- function(searchConditionsList = NULL, PlanPositionDistributionAccountDistributionID = F, PlanPositionDistributionID = F, AccountID = F, DistributionPercent = F, ProratedDistributionPercent = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, searchConditionsGroupType = "And", searchSortFieldNamesList = NULL, searchSortFieldNamesDescendingList = NULL, entityID = 1, schoolYearID = NULL, page = 1, pageSize = 100000, flatten = T, ...){

		params <- as.list(environment()) %>% append(list(...))

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- "all", searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		listSkyObjects(module = "StaffPlanning", objectName = "PlanPositionDistributionAccountDistribution", schoolYearID = schoolYearID, searchFields = searchFields, page = page, pageSize = pageSize, SearchConditionsList = searchConditionsList, SearchConditionsGroupType = searchConditionsGroupType, SearchSortFieldNamesList = searchSortFieldNamesList, SearchSortFieldNamesDescendingList = searchSortFieldNamesDescendingList, entityId = entityID, flatten = flatten)
	}
