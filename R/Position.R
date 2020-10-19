

	listAssignmentTypes <- function(searchConditionsList = NULL, AssignmentTypeMNID = F, StateSTARAssignmentCodeMNID = F, AssignmentTypeID = F, Code = F, Description = F, DistrictID = F, FiscalYearID = F, CodeDescription = F, AssignmentTypeIDClonedFrom = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, FederalCRDCStaffTypeID = F, FederalEEOCJobCategoryID = F, searchConditionsGroupType = "And", searchSortFieldNamesList = NULL, searchSortFieldNamesDescendingList = NULL, entityID = 1, schoolYearID = NULL, page = 1, pageSize = 100000, flatten = T, ...){

		params <- as.list(environment()) %>% append(list(...))

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- "all", searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		listSkyObjects(module = "Position", objectName = "AssignmentType", schoolYearID = schoolYearID, searchFields = searchFields, page = page, pageSize = pageSize, SearchConditionsList = searchConditionsList, SearchConditionsGroupType = searchConditionsGroupType, SearchSortFieldNamesList = searchSortFieldNamesList, SearchSortFieldNamesDescendingList = searchSortFieldNamesDescendingList, entityId = entityID, flatten = flatten)
	}


	listStateSTARAssignmentCodeMNS <- function(searchConditionsList = NULL, StateSTARAssignmentCodeMNID = F, IsLicensedAssignment = F, Code = F, Description = F, CodeDescription = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, SkywardID = F, SkywardHash = F, ValidYearLow = F, ValidYearHigh = F, IsInvalid = F, searchConditionsGroupType = "And", searchSortFieldNamesList = NULL, searchSortFieldNamesDescendingList = NULL, entityID = 1, schoolYearID = NULL, page = 1, pageSize = 100000, flatten = T, ...){

		params <- as.list(environment()) %>% append(list(...))

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- "all", searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		listSkyObjects(module = "Position", objectName = "StateSTARAssignmentCodeMN", schoolYearID = schoolYearID, searchFields = searchFields, page = page, pageSize = pageSize, SearchConditionsList = searchConditionsList, SearchConditionsGroupType = searchConditionsGroupType, SearchSortFieldNamesList = searchSortFieldNamesList, SearchSortFieldNamesDescendingList = searchSortFieldNamesDescendingList, entityId = entityID, flatten = flatten)
	}


	listStateSTARHighestEducationLevelMNS <- function(searchConditionsList = NULL, StateSTARHighestEducationLevelMNID = F, Code = F, Description = F, CodeDescription = F, BaseCode = F, MinimumAdditionalCreditsThreshold = F, MaximumAdditionalCreditsThreshold = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, SkywardID = F, SkywardHash = F, ValidYearLow = F, ValidYearHigh = F, IsInvalid = F, searchConditionsGroupType = "And", searchSortFieldNamesList = NULL, searchSortFieldNamesDescendingList = NULL, entityID = 1, schoolYearID = NULL, page = 1, pageSize = 100000, flatten = T, ...){

		params <- as.list(environment()) %>% append(list(...))

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- "all", searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		listSkyObjects(module = "Position", objectName = "StateSTARHighestEducationLevelMN", schoolYearID = schoolYearID, searchFields = searchFields, page = page, pageSize = pageSize, SearchConditionsList = searchConditionsList, SearchConditionsGroupType = searchConditionsGroupType, SearchSortFieldNamesList = searchSortFieldNamesList, SearchSortFieldNamesDescendingList = searchSortFieldNamesDescendingList, entityId = entityID, flatten = flatten)
	}


	listStateReportingDistributions <- function(searchConditionsList = NULL, StateReportingDistributionMNID = F, StateSTARGradeLevelMNID = F, StateSTARModeOfTeachingMNID = F, STARPeriodsPerWeek = F, STARLengthOfPeriod = F, STAROutOfDistrictAssignment = F, StateReportingDistributionID = F, AssignmentID = F, PositionTypeID = F, AssignmentTypeID = F, BuildingID = F, Percentage = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, StateFTE = F, StateFTEOverride = F, PositionIdentifier = F, ReportedStateFTE = F, searchConditionsGroupType = "And", searchSortFieldNamesList = NULL, searchSortFieldNamesDescendingList = NULL, entityID = 1, schoolYearID = NULL, page = 1, pageSize = 100000, flatten = T, ...){

		params <- as.list(environment()) %>% append(list(...))

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- "all", searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		listSkyObjects(module = "Position", objectName = "StateReportingDistribution", schoolYearID = schoolYearID, searchFields = searchFields, page = page, pageSize = pageSize, SearchConditionsList = searchConditionsList, SearchConditionsGroupType = searchConditionsGroupType, SearchSortFieldNamesList = searchSortFieldNamesList, SearchSortFieldNamesDescendingList = searchSortFieldNamesDescendingList, entityId = entityID, flatten = flatten)
	}


	listStateSTARModeOfTeachingMNS <- function(searchConditionsList = NULL, StateSTARModeOfTeachingMNID = F, Code = F, Description = F, CodeDescription = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, SkywardID = F, SkywardHash = F, ValidYearLow = F, ValidYearHigh = F, IsInvalid = F, searchConditionsGroupType = "And", searchSortFieldNamesList = NULL, searchSortFieldNamesDescendingList = NULL, entityID = 1, schoolYearID = NULL, page = 1, pageSize = 100000, flatten = T, ...){

		params <- as.list(environment()) %>% append(list(...))

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- "all", searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		listSkyObjects(module = "Position", objectName = "StateSTARModeOfTeachingMN", schoolYearID = schoolYearID, searchFields = searchFields, page = page, pageSize = pageSize, SearchConditionsList = searchConditionsList, SearchConditionsGroupType = searchConditionsGroupType, SearchSortFieldNamesList = searchSortFieldNamesList, SearchSortFieldNamesDescendingList = searchSortFieldNamesDescendingList, entityId = entityID, flatten = flatten)
	}


	listStateSTARGradeLevelMNS <- function(searchConditionsList = NULL, StateSTARGradeLevelMNID = F, Code = F, Description = F, CodeDescription = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, IsIndividualGradeLevel = F, SkywardID = F, SkywardHash = F, ValidYearLow = F, ValidYearHigh = F, IsInvalid = F, searchConditionsGroupType = "And", searchSortFieldNamesList = NULL, searchSortFieldNamesDescendingList = NULL, entityID = 1, schoolYearID = NULL, page = 1, pageSize = 100000, flatten = T, ...){

		params <- as.list(environment()) %>% append(list(...))

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- "all", searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		listSkyObjects(module = "Position", objectName = "StateSTARGradeLevelMN", schoolYearID = schoolYearID, searchFields = searchFields, page = page, pageSize = pageSize, SearchConditionsList = searchConditionsList, SearchConditionsGroupType = searchConditionsGroupType, SearchSortFieldNamesList = searchSortFieldNamesList, SearchSortFieldNamesDescendingList = searchSortFieldNamesDescendingList, entityId = entityID, flatten = flatten)
	}


	listTempAssignmentDetails <- function(searchConditionsList = NULL, TempAssignmentDetailID = F, Employee = F, OldAnnualizedPay = F, NewAnnualizedPay = F, OldTotalPay = F, NewTotalPay = F, OldHourlyPay = F, NewHourlyPay = F, OldDailyPay = F, NewDailyPay = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, StartDate = F, EndDate = F, OriginalAssignmentDetailID = F, OldEnteredRate = F, NewEnteredRate = F, EmployeeNumber = F, AssignmentID = F, EnteredFTE = F, SecondsPerDay = F, MatrixID = F, OldMatrixCode = F, NewMatrixCode = F, PositionTypeCode = F, PositionTypeDescription = F, PositionDistributionsAssignmentTypeCodes = F, PositionDistributionsAssignmentTypeDescriptions = F, PositionDistributionsBuildingCodes = F, PositionDistributionsBuildingDescriptions = F, OldSupplementTotalPay = F, NewSupplementTotalPay = F, ErrorCount = F, IsPrimary = F, OldEnteredFTE = F, OldSecondsPerDay = F, OldActivePaidDayCount = F, NewActivePaidDayCount = F, OldWorkdayCount = F, NewWorkdayCount = F, PositionID = F, CreatedFromPositionChange = F, OldCalendarSeconds = F, OldFormattedCalendarSeconds = F, NewCalendarSeconds = F, NewFormattedCalendarSeconds = F, OldPaidFullDays = F, NewPaidFullDays = F, searchConditionsGroupType = "And", searchSortFieldNamesList = NULL, searchSortFieldNamesDescendingList = NULL, entityID = 1, schoolYearID = NULL, page = 1, pageSize = 100000, flatten = T, ...){

		params <- as.list(environment()) %>% append(list(...))

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- "all", searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		listSkyObjects(module = "Position", objectName = "TempAssignmentDetail", schoolYearID = schoolYearID, searchFields = searchFields, page = page, pageSize = pageSize, SearchConditionsList = searchConditionsList, SearchConditionsGroupType = searchConditionsGroupType, SearchSortFieldNamesList = searchSortFieldNamesList, SearchSortFieldNamesDescendingList = searchSortFieldNamesDescendingList, entityId = entityID, flatten = flatten)
	}


	listTempEmployeePlacementDetails <- function(searchConditionsList = NULL, TempEmployeePlacementDetailID = F, Employee = F, OldStep = F, NewStep = F, Credits = F, OldLane = F, OldLaneID = F, EmployeePlacementID = F, Placement = F, EffectiveDate = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, EmployeeNumber = F, TempType = F, EmployeePlacementDetailID = F, NewLane = F, NewLaneID = F, Message = F, EmployeeID = F, NewCredits = F, PlacementID = F, EmployeePlacementDescription = F, searchConditionsGroupType = "And", searchSortFieldNamesList = NULL, searchSortFieldNamesDescendingList = NULL, entityID = 1, schoolYearID = NULL, page = 1, pageSize = 100000, flatten = T, ...){

		params <- as.list(environment()) %>% append(list(...))

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- "all", searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		listSkyObjects(module = "Position", objectName = "TempEmployeePlacementDetail", schoolYearID = schoolYearID, searchFields = searchFields, page = page, pageSize = pageSize, SearchConditionsList = searchConditionsList, SearchConditionsGroupType = searchConditionsGroupType, SearchSortFieldNamesList = searchSortFieldNamesList, SearchSortFieldNamesDescendingList = searchSortFieldNamesDescendingList, entityId = entityID, flatten = flatten)
	}


	listTempOrganizationChartRelationships <- function(searchConditionsList = NULL, TempOrganizationChartRelationshipID = F, OrganizationChartID = F, PositionIDSupervisor = F, PositionID = F, PositionPositionTypeDescription = F, PositionPositionNumberCode = F, PositionClosingAssignmentEmployee = F, PositionCurrentAssignmentEmployee = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, PositionClosingAssignmentEmployeeNumber = F, PositionCurrentAssignmentEmployeeNumber = F, PositionPositionTypeCode = F, PositionDistributionsAssignmentTypeCodes = F, PositionDistributionsAssignmentTypeDescriptions = F, PositionDistributionsBuildingCodes = F, PositionDistributionsBuildingDescriptions = F, searchConditionsGroupType = "And", searchSortFieldNamesList = NULL, searchSortFieldNamesDescendingList = NULL, entityID = 1, schoolYearID = NULL, page = 1, pageSize = 100000, flatten = T, ...){

		params <- as.list(environment()) %>% append(list(...))

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- "all", searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		listSkyObjects(module = "Position", objectName = "TempOrganizationChartRelationship", schoolYearID = schoolYearID, searchFields = searchFields, page = page, pageSize = pageSize, SearchConditionsList = searchConditionsList, SearchConditionsGroupType = searchConditionsGroupType, SearchSortFieldNamesList = searchSortFieldNamesList, SearchSortFieldNamesDescendingList = searchSortFieldNamesDescendingList, entityId = entityID, flatten = flatten)
	}


	listAssignmentSchedules <- function(searchConditionsList = NULL, AssignmentScheduleID = F, AssignmentID = F, StartDate = F, EndDate = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, searchConditionsGroupType = "And", searchSortFieldNamesList = NULL, searchSortFieldNamesDescendingList = NULL, entityID = 1, schoolYearID = NULL, page = 1, pageSize = 100000, flatten = T, ...){

		params <- as.list(environment()) %>% append(list(...))

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- "all", searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		listSkyObjects(module = "Position", objectName = "AssignmentSchedule", schoolYearID = schoolYearID, searchFields = searchFields, page = page, pageSize = pageSize, SearchConditionsList = searchConditionsList, SearchConditionsGroupType = searchConditionsGroupType, SearchSortFieldNamesList = searchSortFieldNamesList, SearchSortFieldNamesDescendingList = searchSortFieldNamesDescendingList, entityId = entityID, flatten = flatten)
	}


	listAssignmentScheduleDetails <- function(searchConditionsList = NULL, AssignmentScheduleDetailID = F, AssignmentScheduleID = F, StartTime = F, EndTime = F, Status = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, searchConditionsGroupType = "And", searchSortFieldNamesList = NULL, searchSortFieldNamesDescendingList = NULL, entityID = 1, schoolYearID = NULL, page = 1, pageSize = 100000, flatten = T, ...){

		params <- as.list(environment()) %>% append(list(...))

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- "all", searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		listSkyObjects(module = "Position", objectName = "AssignmentScheduleDetail", schoolYearID = schoolYearID, searchFields = searchFields, page = page, pageSize = pageSize, SearchConditionsList = searchConditionsList, SearchConditionsGroupType = searchConditionsGroupType, SearchSortFieldNamesList = searchSortFieldNamesList, SearchSortFieldNamesDescendingList = searchSortFieldNamesDescendingList, entityId = entityID, flatten = flatten)
	}


	listStatePERAPositionClassMNS <- function(searchConditionsList = NULL, StatePERAPositionClassMNID = F, Code = F, Description = F, CodeDescription = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, SkywardID = F, SkywardHash = F, ValidYearLow = F, ValidYearHigh = F, IsInvalid = F, searchConditionsGroupType = "And", searchSortFieldNamesList = NULL, searchSortFieldNamesDescendingList = NULL, entityID = 1, schoolYearID = NULL, page = 1, pageSize = 100000, flatten = T, ...){

		params <- as.list(environment()) %>% append(list(...))

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- "all", searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		listSkyObjects(module = "Position", objectName = "StatePERAPositionClassMN", schoolYearID = schoolYearID, searchFields = searchFields, page = page, pageSize = pageSize, SearchConditionsList = searchConditionsList, SearchConditionsGroupType = searchConditionsGroupType, SearchSortFieldNamesList = searchSortFieldNamesList, SearchSortFieldNamesDescendingList = searchSortFieldNamesDescendingList, entityId = entityID, flatten = flatten)
	}


	listStatePERAPositionCodeMNS <- function(searchConditionsList = NULL, StatePERAPositionCodeMNID = F, Code = F, Description = F, CodeDescription = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, SkywardID = F, SkywardHash = F, ValidYearLow = F, ValidYearHigh = F, IsInvalid = F, searchConditionsGroupType = "And", searchSortFieldNamesList = NULL, searchSortFieldNamesDescendingList = NULL, entityID = 1, schoolYearID = NULL, page = 1, pageSize = 100000, flatten = T, ...){

		params <- as.list(environment()) %>% append(list(...))

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- "all", searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		listSkyObjects(module = "Position", objectName = "StatePERAPositionCodeMN", schoolYearID = schoolYearID, searchFields = searchFields, page = page, pageSize = pageSize, SearchConditionsList = searchConditionsList, SearchConditionsGroupType = searchConditionsGroupType, SearchSortFieldNamesList = searchSortFieldNamesList, SearchSortFieldNamesDescendingList = searchSortFieldNamesDescendingList, entityId = entityID, flatten = flatten)
	}


	listStateTRAEligibilityMNS <- function(searchConditionsList = NULL, StateTRAEligibilityMNID = F, Code = F, Description = F, CodeDescription = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, SkywardID = F, SkywardHash = F, ValidYearLow = F, ValidYearHigh = F, IsInvalid = F, searchConditionsGroupType = "And", searchSortFieldNamesList = NULL, searchSortFieldNamesDescendingList = NULL, entityID = 1, schoolYearID = NULL, page = 1, pageSize = 100000, flatten = T, ...){

		params <- as.list(environment()) %>% append(list(...))

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- "all", searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		listSkyObjects(module = "Position", objectName = "StateTRAEligibilityMN", schoolYearID = schoolYearID, searchFields = searchFields, page = page, pageSize = pageSize, SearchConditionsList = searchConditionsList, SearchConditionsGroupType = searchConditionsGroupType, SearchSortFieldNamesList = searchSortFieldNamesList, SearchSortFieldNamesDescendingList = searchSortFieldNamesDescendingList, entityId = entityID, flatten = flatten)
	}


	listPositionTypes <- function(searchConditionsList = NULL, PositionTypeMNID = F, StateTRACurrentPositionMNID = F, StateRetirementAssociationTypeMNID = F, StateTRAEligibilityMNID = F, StatePERAExclusionCodeMNIDDefault = F, StatePERAPositionCodeMNIDDefault = F, StatePERAPositionClassMNIDDefault = F, RetirementAssociation = F, PositionTypeID = F, Code = F, Description = F, DistrictID = F, FiscalYearID = F, SalaryCalculationMethodID = F, EntitlementID = F, PayScaleID = F, TaxableLifeInsuranceFactor = F, CodeDescription = F, BudgetedFTE = F, ClosingAssignmentFTE = F, VacantClosingAssignmentFTE = F, PositionTypeIDClonedFrom = F, AssignmentTimeTrackingGroupIDDefault = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, STARPeriodsPerWeek = F, STARLengthOfPeriod = F, StateReportingDistributionSummaryCurrentFTE = F, StateReportingDistributionSummaryClosingAssignmentFTE = F, StateReportingDistributionSummaryVacantClosingAssignmentFTE = F, PlanPositionDistributionsForPlanGroupFTE = F, BenefitGroupID = F, MatrixID = F, CalendarID = F, FullPaySecondsPerDay = F, AllowTimeOff = F, FTEGroupID = F, searchConditionsGroupType = "And", searchSortFieldNamesList = NULL, searchSortFieldNamesDescendingList = NULL, entityID = 1, schoolYearID = NULL, page = 1, pageSize = 100000, flatten = T, ...){

		params <- as.list(environment()) %>% append(list(...))

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- "all", searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		listSkyObjects(module = "Position", objectName = "PositionType", schoolYearID = schoolYearID, searchFields = searchFields, page = page, pageSize = pageSize, SearchConditionsList = searchConditionsList, SearchConditionsGroupType = searchConditionsGroupType, SearchSortFieldNamesList = searchSortFieldNamesList, SearchSortFieldNamesDescendingList = searchSortFieldNamesDescendingList, entityId = entityID, flatten = flatten)
	}


	listStateTRACurrentPositionMNS <- function(searchConditionsList = NULL, StateTRACurrentPositionMNID = F, Code = F, Description = F, CodeDescription = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, SkywardID = F, SkywardHash = F, ValidYearLow = F, ValidYearHigh = F, IsInvalid = F, searchConditionsGroupType = "And", searchSortFieldNamesList = NULL, searchSortFieldNamesDescendingList = NULL, entityID = 1, schoolYearID = NULL, page = 1, pageSize = 100000, flatten = T, ...){

		params <- as.list(environment()) %>% append(list(...))

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- "all", searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		listSkyObjects(module = "Position", objectName = "StateTRACurrentPositionMN", schoolYearID = schoolYearID, searchFields = searchFields, page = page, pageSize = pageSize, SearchConditionsList = searchConditionsList, SearchConditionsGroupType = searchConditionsGroupType, SearchSortFieldNamesList = searchSortFieldNamesList, SearchSortFieldNamesDescendingList = searchSortFieldNamesDescendingList, entityId = entityID, flatten = flatten)
	}


	listPositionTempAssignmentErrors <- function(searchConditionsList = NULL, TempAssignmentErrorID = F, PositionNumberCode = F, Employee = F, ErrorReason = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, TotalPay = F, EmployeeNumber = F, PositionTypeCode = F, PositionTypeDescription = F, PositionDistributionsAssignmentTypeCodes = F, PositionDistributionsAssignmentTypeDescriptions = F, PositionDistributionsBuildingCodes = F, PositionDistributionsBuildingDescriptions = F, TempAssignmentID = F, ErrorField = F, searchConditionsGroupType = "And", searchSortFieldNamesList = NULL, searchSortFieldNamesDescendingList = NULL, entityID = 1, schoolYearID = NULL, page = 1, pageSize = 100000, flatten = T, ...){

		params <- as.list(environment()) %>% append(list(...))

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- "all", searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		listSkyObjects(module = "Position", objectName = "TempAssignmentError", schoolYearID = schoolYearID, searchFields = searchFields, page = page, pageSize = pageSize, SearchConditionsList = searchConditionsList, SearchConditionsGroupType = searchConditionsGroupType, SearchSortFieldNamesList = searchSortFieldNamesList, SearchSortFieldNamesDescendingList = searchSortFieldNamesDescendingList, entityId = entityID, flatten = flatten)
	}


	listTempAssignments <- function(searchConditionsList = NULL, TempAssignmentID = F, PositionNumberCode = F, EmployeeID = F, Employee = F, PositionID = F, SalaryCalculationMethodID = F, NextYearIntentID = F, NextYearIntentCodeDescription = F, MatrixID = F, MatrixIDBase = F, AssignmentDetailStartDate = F, AssignmentDetailEndDate = F, AssignmentDetailIsPrimary = F, AssignmentDetailTotalPay = F, AssignmentTimeTrackingGroupID = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, AssignmentID = F, AssignmentStartDate = F, AssignmentEndDate = F, TotalPay = F, NewTotalPay = F, DailyPay = F, NewDailyPay = F, HourlyPay = F, NewHourlyPay = F, AnnualizedPay = F, NewAnnualizedPay = F, EmployeeNumber = F, OldEntitlementCodeDescription = F, NewEntitlementCodeDescription = F, OldNextYearIntentCodeDescription = F, NewNextYearIntentCodeDescription = F, OldAssignmentTimeTrackingGroupCodeDescription = F, NewAssignmentTimeTrackingGroupCodeDescription = F, OldRetirementJobCategoryCodeDescription = F, RetirementJobCategoryCodeDescription = F, OldRetirementWorkStatusCodeDescription = F, RetirementWorkStatusCodeDescription = F, OldChapter40TermDate = F, Chapter40TermDate = F, PositionTypeCode = F, PositionTypeDescription = F, PositionDistributionsAssignmentTypeCodes = F, PositionDistributionsAssignmentTypeDescriptions = F, PositionDistributionsBuildingCodes = F, PositionDistributionsBuildingDescriptions = F, AssignmentDetailFormattedSecondsPerDay = F, EntitlementCode = F, AssignmentDetailIsOverloaded = F, PositionBudgetedFTE = F, ErrorCount = F, TempPositionID = F, EmployeePlacementID = F, EntitlementID = F, IsTotalPayChanging = F, ActivePaidDayCount = F, NewActivePaidDayCount = F, WorkdayCount = F, NewWorkdayCount = F, searchConditionsGroupType = "And", searchSortFieldNamesList = NULL, searchSortFieldNamesDescendingList = NULL, entityID = 1, schoolYearID = NULL, page = 1, pageSize = 100000, flatten = T, ...){

		params <- as.list(environment()) %>% append(list(...))

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- "all", searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		listSkyObjects(module = "Position", objectName = "TempAssignment", schoolYearID = schoolYearID, searchFields = searchFields, page = page, pageSize = pageSize, SearchConditionsList = searchConditionsList, SearchConditionsGroupType = searchConditionsGroupType, SearchSortFieldNamesList = searchSortFieldNamesList, SearchSortFieldNamesDescendingList = searchSortFieldNamesDescendingList, entityId = entityID, flatten = flatten)
	}


	listTempPositions <- function(searchConditionsList = NULL, TempPositionID = F, PositionIDClonedFrom = F, PositionNumberCode = F, PositionNumberID = F, PositionTypeCodeDescription = F, PositionTypeID = F, CalendarID = F, CalendarCodeDescription = F, StartDate = F, EndDate = F, FullPaySecondsPerDay = F, BudgetedFTE = F, PositionGroupID = F, JobTypeID = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, ConfigFiscalYearTRSStateBaseLaneID = F, PlanPositionIDClonedFrom = F, PositionDistributionsAssignmentTypeCodes = F, PositionDistributionsAssignmentTypeDescriptions = F, PositionDistributionsBuildingCodes = F, PositionDistributionsBuildingDescriptions = F, PositionDistributionsFTEGroupCodes = F, PositionDistributionsFTEGroupDescriptions = F, ActivePaidDayCount = F, TotalPaidSeconds = F, FullPaidDays = F, PlanPositionAnnualPay = F, Employee = F, EmployeeNumber = F, AnnualPay = F, IsPrimary = F, ErrorCount = F, PositionID = F, EmployeeID = F, DailyPay = F, HourlyPay = F, CanBeDeleted = F, SalaryCalculationMethodID = F, EntitlementID = F, EmployeePlacementID = F, AssignmentTimeTrackingGroupID = F, NextYearIntentID = F, AssignmentStartDate = F, AssignmentEndDate = F, AssignmentSecondsPerDay = F, AssignmentEnteredFTE = F, AssignmentEnteredRate = F, MatrixID = F, EmployeePlacementDetailID = F, TempEmployeePlacementDetailID = F, StepID = F, StepValue = F, Exception = F, LineNumber = F, WillCreatePosition = F, WillCreatePositionNumber = F, EmployeeDistrictID = F, ConfigFiscalYearTRSStateBaseStepID = F, PositionTypeCode = F, OldActivePaidDayCount = F, FormattedTotalPaidSeconds = F, OldTotalPaidSeconds = F, OldFormattedTotalPaidSeconds = F, OldFullPaidDays = F, searchConditionsGroupType = "And", searchSortFieldNamesList = NULL, searchSortFieldNamesDescendingList = NULL, entityID = 1, schoolYearID = NULL, page = 1, pageSize = 100000, flatten = T, ...){

		params <- as.list(environment()) %>% append(list(...))

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- "all", searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		listSkyObjects(module = "Position", objectName = "TempPosition", schoolYearID = schoolYearID, searchFields = searchFields, page = page, pageSize = pageSize, SearchConditionsList = searchConditionsList, SearchConditionsGroupType = searchConditionsGroupType, SearchSortFieldNamesList = searchSortFieldNamesList, SearchSortFieldNamesDescendingList = searchSortFieldNamesDescendingList, entityId = entityID, flatten = flatten)
	}


	listTempPositionErrors <- function(searchConditionsList = NULL, TempPositionErrorID = F, PositionNumberCode = F, ErrorDescription = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, PositionTypeCodeDescription = F, EmployeeFullNameLFM = F, TempPositionID = F, EmployeeNumber = F, PositionDistributionsAssignmentTypeCodes = F, PositionDistributionsAssignmentTypeDescriptions = F, PositionDistributionsBuildingCodes = F, PositionDistributionsBuildingDescriptions = F, PositionDistributionsFTEGroupCodes = F, PositionDistributionsFTEGroupDescriptions = F, FatalException = F, searchConditionsGroupType = "And", searchSortFieldNamesList = NULL, searchSortFieldNamesDescendingList = NULL, entityID = 1, schoolYearID = NULL, page = 1, pageSize = 100000, flatten = T, ...){

		params <- as.list(environment()) %>% append(list(...))

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- "all", searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		listSkyObjects(module = "Position", objectName = "TempPositionError", schoolYearID = schoolYearID, searchFields = searchFields, page = page, pageSize = pageSize, SearchConditionsList = searchConditionsList, SearchConditionsGroupType = searchConditionsGroupType, SearchSortFieldNamesList = searchSortFieldNamesList, SearchSortFieldNamesDescendingList = searchSortFieldNamesDescendingList, entityId = entityID, flatten = flatten)
	}


	listNextYearIntents <- function(searchConditionsList = NULL, NextYearIntentID = F, Code = F, Description = F, DistrictID = F, FiscalYearID = F, CodeDescription = F, NextYearIntentIDClonedFrom = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, searchConditionsGroupType = "And", searchSortFieldNamesList = NULL, searchSortFieldNamesDescendingList = NULL, entityID = 1, schoolYearID = NULL, page = 1, pageSize = 100000, flatten = T, ...){

		params <- as.list(environment()) %>% append(list(...))

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- "all", searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		listSkyObjects(module = "Position", objectName = "NextYearIntent", schoolYearID = schoolYearID, searchFields = searchFields, page = page, pageSize = pageSize, SearchConditionsList = searchConditionsList, SearchConditionsGroupType = searchConditionsGroupType, SearchSortFieldNamesList = searchSortFieldNamesList, SearchSortFieldNamesDescendingList = searchSortFieldNamesDescendingList, entityId = entityID, flatten = flatten)
	}


	listOrganizationCharts <- function(searchConditionsList = NULL, OrganizationChartID = F, DistrictID = F, FiscalYearID = F, Code = F, Description = F, OrganizationChartIDClonedFrom = F, CodeDescription = F, IsUsedInTimeOff = F, IsUsedInTimeTracking = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, IsUsedInAccountsPayable = F, IsCloneable = F, searchConditionsGroupType = "And", searchSortFieldNamesList = NULL, searchSortFieldNamesDescendingList = NULL, entityID = 1, schoolYearID = NULL, page = 1, pageSize = 100000, flatten = T, ...){

		params <- as.list(environment()) %>% append(list(...))

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- "all", searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		listSkyObjects(module = "Position", objectName = "OrganizationChart", schoolYearID = schoolYearID, searchFields = searchFields, page = page, pageSize = pageSize, SearchConditionsList = searchConditionsList, SearchConditionsGroupType = searchConditionsGroupType, SearchSortFieldNamesList = searchSortFieldNamesList, SearchSortFieldNamesDescendingList = searchSortFieldNamesDescendingList, entityId = entityID, flatten = flatten)
	}


	listOrganizationChartRelationships <- function(searchConditionsList = NULL, OrganizationChartRelationshipID = F, OrganizationChartID = F, PositionID = F, PositionIDSupervisor = F, FinalApproval = F, SelfApproval = F, RelationshipIdentifier = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, UsePositionDistributionFilter = F, FilterData = F, StandardFilterCollectionData = F, StandardFilterCollectionSearchCondition = F, searchConditionsGroupType = "And", searchSortFieldNamesList = NULL, searchSortFieldNamesDescendingList = NULL, entityID = 1, schoolYearID = NULL, page = 1, pageSize = 100000, flatten = T, ...){

		params <- as.list(environment()) %>% append(list(...))

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- "all", searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		listSkyObjects(module = "Position", objectName = "OrganizationChartRelationship", schoolYearID = schoolYearID, searchFields = searchFields, page = page, pageSize = pageSize, SearchConditionsList = searchConditionsList, SearchConditionsGroupType = searchConditionsGroupType, SearchSortFieldNamesList = searchSortFieldNamesList, SearchSortFieldNamesDescendingList = searchSortFieldNamesDescendingList, entityId = entityID, flatten = flatten)
	}


	listPositionAssignments <- function(searchConditionsList = NULL, AssignmentID = F, PositionID = F, EmployeeID = F, SalaryCalculationMethodID = F, EmployeePlacementID = F, EntitlementID = F, NextYearIntentID = F, StartDate = F, EndDate = F, ActivePaidDayCount = F, CalendarSeconds = F, TotalPay = F, FormattedCalendarSeconds = F, ContractPaidToDate = F, ContractBalance = F, CurrentScheduledPaidHours = F, AssignmentIdentifier = F, AssignmentCodeIdentifier = F, AttachmentCount = F, TotalRetroPay = F, TotalDockPay = F, AssignmentTimeTrackingGroupID = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, PercentEmployed = F, TotalStipendAmount = F, PositionDistributionsBuildingCodes = F, PositionDistributionsBuildingDescriptions = F, StateReportingDistributionsPositionTypeCodes = F, StateReportingDistributionsPositionTypeDescriptions = F, PositionDistributionsAssignmentTypeCodes = F, PositionDistributionsAssignmentTypeDescriptions = F, FormattedCalendarSecondsDecimal = F, CurrentScheduledPaidHoursDecimal = F, PositionTypeEmployeeIdentifier = F, PositionDistributionAssignmentRangeIdentifier = F, IsEEOCPrimaryAssignment = F, WorkStartDate = F, WorkEndDate = F, DockedTotalPay = F, AssignmentThirdPartyImportID = F, BaseAssignmentPay = F, SupplementTotalPay = F, searchConditionsGroupType = "And", searchSortFieldNamesList = NULL, searchSortFieldNamesDescendingList = NULL, entityID = 1, schoolYearID = NULL, page = 1, pageSize = 100000, flatten = T, ...){

		params <- as.list(environment()) %>% append(list(...))

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- "all", searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		listSkyObjects(module = "Position", objectName = "Assignment", schoolYearID = schoolYearID, searchFields = searchFields, page = page, pageSize = pageSize, SearchConditionsList = searchConditionsList, SearchConditionsGroupType = searchConditionsGroupType, SearchSortFieldNamesList = searchSortFieldNamesList, SearchSortFieldNamesDescendingList = searchSortFieldNamesDescendingList, entityId = entityID, flatten = flatten)
	}


	listAssignmentDetails <- function(searchConditionsList = NULL, AssignmentDetailID = F, AssignmentID = F, StartDate = F, EndDate = F, ActivePaidDayCount = F, ActivePaidDayCountOverride = F, SecondsPerDay = F, EnteredFTE = F, CalendarSeconds = F, TotalPay = F, AnnualizedPay = F, HourlyPay = F, DailyPay = F, IsPrimary = F, MatrixID = F, MatrixIDBase = F, StepID = F, EmployeePlacementDetailID = F, EnteredRate = F, StepIDBase = F, EmployeePlacementIDBase = F, PaidFullDays = F, BaseMatrixStep = F, FormattedCalendarSeconds = F, MatrixStep = F, AssignmentDetailIdentifier = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, Comment = F, SupplementDailyPayTotal = F, SupplementHourlyPayTotal = F, SupplementAnnualizedPayTotal = F, SupplementPayTotal = F, FormattedSecondsPerDayDecimal = F, FormattedCalendarSecondsDecimal = F, IsOverloaded = F, searchConditionsGroupType = "And", searchSortFieldNamesList = NULL, searchSortFieldNamesDescendingList = NULL, entityID = 1, schoolYearID = NULL, page = 1, pageSize = 100000, flatten = T, ...){

		params <- as.list(environment()) %>% append(list(...))

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- "all", searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		listSkyObjects(module = "Position", objectName = "AssignmentDetail", schoolYearID = schoolYearID, searchFields = searchFields, page = page, pageSize = pageSize, SearchConditionsList = searchConditionsList, SearchConditionsGroupType = searchConditionsGroupType, SearchSortFieldNamesList = searchSortFieldNamesList, SearchSortFieldNamesDescendingList = searchSortFieldNamesDescendingList, entityId = entityID, flatten = flatten)
	}


	listPositionDepartments <- function(searchConditionsList = NULL, DepartmentID = F, DistrictID = F, Code = F, Description = F, FiscalYearID = F, CodeDescription = F, DepartmentIDClonedFrom = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, searchConditionsGroupType = "And", searchSortFieldNamesList = NULL, searchSortFieldNamesDescendingList = NULL, entityID = 1, schoolYearID = NULL, page = 1, pageSize = 100000, flatten = T, ...){

		params <- as.list(environment()) %>% append(list(...))

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- "all", searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		listSkyObjects(module = "Position", objectName = "Department", schoolYearID = schoolYearID, searchFields = searchFields, page = page, pageSize = pageSize, SearchConditionsList = searchConditionsList, SearchConditionsGroupType = searchConditionsGroupType, SearchSortFieldNamesList = searchSortFieldNamesList, SearchSortFieldNamesDescendingList = searchSortFieldNamesDescendingList, entityId = entityID, flatten = flatten)
	}


	listEmployeePlacements <- function(searchConditionsList = NULL, EmployeePlacementID = F, EmployeeID = F, PlacementID = F, Description = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, searchConditionsGroupType = "And", searchSortFieldNamesList = NULL, searchSortFieldNamesDescendingList = NULL, entityID = 1, schoolYearID = NULL, page = 1, pageSize = 100000, flatten = T, ...){

		params <- as.list(environment()) %>% append(list(...))

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- "all", searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		listSkyObjects(module = "Position", objectName = "EmployeePlacement", schoolYearID = schoolYearID, searchFields = searchFields, page = page, pageSize = pageSize, SearchConditionsList = searchConditionsList, SearchConditionsGroupType = searchConditionsGroupType, SearchSortFieldNamesList = searchSortFieldNamesList, SearchSortFieldNamesDescendingList = searchSortFieldNamesDescendingList, entityId = entityID, flatten = flatten)
	}


	listEmployeePlacementDetails <- function(searchConditionsList = NULL, EmployeePlacementDetailID = F, EmployeePlacementID = F, EffectiveDate = F, LaneID = F, StepNumber = F, Credits = F, Placement = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, searchConditionsGroupType = "And", searchSortFieldNamesList = NULL, searchSortFieldNamesDescendingList = NULL, entityID = 1, schoolYearID = NULL, page = 1, pageSize = 100000, flatten = T, ...){

		params <- as.list(environment()) %>% append(list(...))

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- "all", searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		listSkyObjects(module = "Position", objectName = "EmployeePlacementDetail", schoolYearID = schoolYearID, searchFields = searchFields, page = page, pageSize = pageSize, SearchConditionsList = searchConditionsList, SearchConditionsGroupType = searchConditionsGroupType, SearchSortFieldNamesList = searchSortFieldNamesList, SearchSortFieldNamesDescendingList = searchSortFieldNamesDescendingList, entityId = entityID, flatten = flatten)
	}


	listJobTypes <- function(searchConditionsList = NULL, JobTypeID = F, DistrictID = F, Code = F, Description = F, FiscalYearID = F, CodeDescription = F, JobTypeIDClonedFrom = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, searchConditionsGroupType = "And", searchSortFieldNamesList = NULL, searchSortFieldNamesDescendingList = NULL, entityID = 1, schoolYearID = NULL, page = 1, pageSize = 100000, flatten = T, ...){

		params <- as.list(environment()) %>% append(list(...))

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- "all", searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		listSkyObjects(module = "Position", objectName = "JobType", schoolYearID = schoolYearID, searchFields = searchFields, page = page, pageSize = pageSize, SearchConditionsList = searchConditionsList, SearchConditionsGroupType = searchConditionsGroupType, SearchSortFieldNamesList = searchSortFieldNamesList, SearchSortFieldNamesDescendingList = searchSortFieldNamesDescendingList, entityId = entityID, flatten = flatten)
	}


	listMatrices <- function(searchConditionsList = NULL, MatrixID = F, DistrictID = F, Code = F, Description = F, FiscalYearID = F, DistrictBase = F, Type = F, CodeDescription = F, MatrixIDClonedFrom = F, AttachmentCount = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, searchConditionsGroupType = "And", searchSortFieldNamesList = NULL, searchSortFieldNamesDescendingList = NULL, entityID = 1, schoolYearID = NULL, page = 1, pageSize = 100000, flatten = T, ...){

		params <- as.list(environment()) %>% append(list(...))

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- "all", searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		listSkyObjects(module = "Position", objectName = "Matrix", schoolYearID = schoolYearID, searchFields = searchFields, page = page, pageSize = pageSize, SearchConditionsList = searchConditionsList, SearchConditionsGroupType = searchConditionsGroupType, SearchSortFieldNamesList = searchSortFieldNamesList, SearchSortFieldNamesDescendingList = searchSortFieldNamesDescendingList, entityId = entityID, flatten = flatten)
	}


	listMatrixLanes <- function(searchConditionsList = NULL, MatrixLaneID = F, MatrixID = F, LaneID = F, RequiredCredits = F, DisplayOrder = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, searchConditionsGroupType = "And", searchSortFieldNamesList = NULL, searchSortFieldNamesDescendingList = NULL, entityID = 1, schoolYearID = NULL, page = 1, pageSize = 100000, flatten = T, ...){

		params <- as.list(environment()) %>% append(list(...))

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- "all", searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		listSkyObjects(module = "Position", objectName = "MatrixLane", schoolYearID = schoolYearID, searchFields = searchFields, page = page, pageSize = pageSize, SearchConditionsList = searchConditionsList, SearchConditionsGroupType = searchConditionsGroupType, SearchSortFieldNamesList = searchSortFieldNamesList, SearchSortFieldNamesDescendingList = searchSortFieldNamesDescendingList, entityId = entityID, flatten = flatten)
	}


	listPlacements <- function(searchConditionsList = NULL, PlacementID = F, DistrictID = F, Code = F, Description = F, CodeDescription = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, searchConditionsGroupType = "And", searchSortFieldNamesList = NULL, searchSortFieldNamesDescendingList = NULL, entityID = 1, schoolYearID = NULL, page = 1, pageSize = 100000, flatten = T, ...){

		params <- as.list(environment()) %>% append(list(...))

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- "all", searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		listSkyObjects(module = "Position", objectName = "Placement", schoolYearID = schoolYearID, searchFields = searchFields, page = page, pageSize = pageSize, SearchConditionsList = searchConditionsList, SearchConditionsGroupType = searchConditionsGroupType, SearchSortFieldNamesList = searchSortFieldNamesList, SearchSortFieldNamesDescendingList = searchSortFieldNamesDescendingList, entityId = entityID, flatten = flatten)
	}


	listPositions <- function(searchConditionsList = NULL, PositionID = F, PositionNumberID = F, DistrictID = F, FiscalYearID = F, PositionTypeID = F, CalendarID = F, StartDate = F, EndDate = F, ActivePaidDayCount = F, FullPaidDays = F, FullPaySecondsPerDay = F, FormattedTotalPaidSeconds = F, TotalPaidSeconds = F, BudgetedFTE = F, PositionGroupID = F, JobTypeID = F, PositionIDClonedFrom = F, ClosingAssignmentFTE = F, VacantClosingAssignmentFTE = F, CurrentAssignmentFTE = F, VacantCurrentAssignmentFTE = F, PositionClosingAssignmentIdentifier = F, PositionCodeIdentifier = F, FormattedPositionCodeEELLabel = F, AttachmentCount = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, PlanPositionIDClonedFrom = F, FormattedFullPaySecondsPerDayDecimal = F, FormattedTotalPaidSecondsDecimal = F, PositionDistributionsBuildingCodes = F, PositionDistributionsBuildingDescriptions = F, PositionDistributionsAssignmentTypeCodes = F, PositionDistributionsAssignmentTypeDescriptions = F, PositionDistributionsFTEGroupCodes = F, PositionDistributionsFTEGroupDescriptions = F, PositionDistributionsDepartmentCodes = F, PositionDistributionsDepartmentDescriptions = F, AvailableClosingFTE = F, EmployeeConversionKey = F, HasTimeOffTransactionsAwaitingApproval = F, HasTimeOffTransactionHistory = F, HasTimesheetSubmissionsAwaitingApproval = F, HasTimesheetSubmissionHistory = F, HasExpenseReimbursementsAwaitingApproval = F, HasExpenseReimbursementHistory = F, HasSubstituteTransactionHistory = F, AssignmentThirdPartyImportID = F, BudgetedHoursPerDay = F, ApprovingEmployeeNames = F, searchConditionsGroupType = "And", searchSortFieldNamesList = NULL, searchSortFieldNamesDescendingList = NULL, entityID = 1, schoolYearID = NULL, page = 1, pageSize = 100000, flatten = T, ...){

		params <- as.list(environment()) %>% append(list(...))

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- "all", searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		listSkyObjects(module = "Position", objectName = "Position", schoolYearID = schoolYearID, searchFields = searchFields, page = page, pageSize = pageSize, SearchConditionsList = searchConditionsList, SearchConditionsGroupType = searchConditionsGroupType, SearchSortFieldNamesList = searchSortFieldNamesList, SearchSortFieldNamesDescendingList = searchSortFieldNamesDescendingList, entityId = entityID, flatten = flatten)
	}


	listPositionGroups <- function(searchConditionsList = NULL, PositionGroupID = F, Code = F, Description = F, DistrictID = F, FiscalYearID = F, CodeDescription = F, PositionGroupIDClonedFrom = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, searchConditionsGroupType = "And", searchSortFieldNamesList = NULL, searchSortFieldNamesDescendingList = NULL, entityID = 1, schoolYearID = NULL, page = 1, pageSize = 100000, flatten = T, ...){

		params <- as.list(environment()) %>% append(list(...))

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- "all", searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		listSkyObjects(module = "Position", objectName = "PositionGroup", schoolYearID = schoolYearID, searchFields = searchFields, page = page, pageSize = pageSize, SearchConditionsList = searchConditionsList, SearchConditionsGroupType = searchConditionsGroupType, SearchSortFieldNamesList = searchSortFieldNamesList, SearchSortFieldNamesDescendingList = searchSortFieldNamesDescendingList, entityId = entityID, flatten = flatten)
	}


	listFTEGroups <- function(searchConditionsList = NULL, FTEGroupID = F, Code = F, Description = F, OptimalFTE = F, MaximumFTE = F, FiscalYearID = F, DistrictID = F, TotalPositionFTE = F, CodeDescription = F, ClosingAssignmentFTE = F, FTEGroupIDClonedFrom = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, searchConditionsGroupType = "And", searchSortFieldNamesList = NULL, searchSortFieldNamesDescendingList = NULL, entityID = 1, schoolYearID = NULL, page = 1, pageSize = 100000, flatten = T, ...){

		params <- as.list(environment()) %>% append(list(...))

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- "all", searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		listSkyObjects(module = "Position", objectName = "FTEGroup", schoolYearID = schoolYearID, searchFields = searchFields, page = page, pageSize = pageSize, SearchConditionsList = searchConditionsList, SearchConditionsGroupType = searchConditionsGroupType, SearchSortFieldNamesList = searchSortFieldNamesList, SearchSortFieldNamesDescendingList = searchSortFieldNamesDescendingList, entityId = entityID, flatten = flatten)
	}


	listPositionNumbers <- function(searchConditionsList = NULL, PositionNumberID = F, DistrictID = F, Code = F, FullPositionNumber = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, IsActive = F, HasPositionInFiscalYear = F, searchConditionsGroupType = "And", searchSortFieldNamesList = NULL, searchSortFieldNamesDescendingList = NULL, entityID = 1, schoolYearID = NULL, page = 1, pageSize = 100000, flatten = T, ...){

		params <- as.list(environment()) %>% append(list(...))

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- "all", searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		listSkyObjects(module = "Position", objectName = "PositionNumber", schoolYearID = schoolYearID, searchFields = searchFields, page = page, pageSize = pageSize, SearchConditionsList = searchConditionsList, SearchConditionsGroupType = searchConditionsGroupType, SearchSortFieldNamesList = searchSortFieldNamesList, SearchSortFieldNamesDescendingList = searchSortFieldNamesDescendingList, entityId = entityID, flatten = flatten)
	}


	listSalaryCalculationMethods <- function(searchConditionsList = NULL, SalaryCalculationMethodID = F, FiscalYearID = F, DistrictID = F, Code = F, Description = F, TimeEntryField = F, SkywardID = F, TotalPayExpression = F, DailyPayExpression = F, HourlyPayExpression = F, AnnualizedPayExpression = F, CodeDescription = F, RenderRestore = F, SalaryCalculationMethodIDClonedFrom = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, AmountType = F, HasMatrix = F, HasSupplement = F, HasEnteredRate = F, searchConditionsGroupType = "And", searchSortFieldNamesList = NULL, searchSortFieldNamesDescendingList = NULL, entityID = 1, schoolYearID = NULL, page = 1, pageSize = 100000, flatten = T, ...){

		params <- as.list(environment()) %>% append(list(...))

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- "all", searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		listSkyObjects(module = "Position", objectName = "SalaryCalculationMethod", schoolYearID = schoolYearID, searchFields = searchFields, page = page, pageSize = pageSize, SearchConditionsList = searchConditionsList, SearchConditionsGroupType = searchConditionsGroupType, SearchSortFieldNamesList = searchSortFieldNamesList, SearchSortFieldNamesDescendingList = searchSortFieldNamesDescendingList, entityId = entityID, flatten = flatten)
	}


	listSalaryCalculationMethodSystems <- function(searchConditionsList = NULL, SalaryCalculationMethodSystemID = F, SkywardCode = F, SkywardDescription = F, SkywardID = F, TimeEntryField = F, TotalPayCalculation = F, DailyPayCalculation = F, HourlyPayCalculation = F, AnnualizedPayCalculation = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, AmountType = F, HasMatrix = F, HasSupplement = F, HasEnteredRate = F, SkywardHash = F, searchConditionsGroupType = "And", searchSortFieldNamesList = NULL, searchSortFieldNamesDescendingList = NULL, entityID = 1, schoolYearID = NULL, page = 1, pageSize = 100000, flatten = T, ...){

		params <- as.list(environment()) %>% append(list(...))

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- "all", searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		listSkyObjects(module = "Position", objectName = "SalaryCalculationMethodSystem", schoolYearID = schoolYearID, searchFields = searchFields, page = page, pageSize = pageSize, SearchConditionsList = searchConditionsList, SearchConditionsGroupType = searchConditionsGroupType, SearchSortFieldNamesList = searchSortFieldNamesList, SearchSortFieldNamesDescendingList = searchSortFieldNamesDescendingList, entityId = entityID, flatten = flatten)
	}


	listPositionSteps <- function(searchConditionsList = NULL, StepID = F, MatrixLaneID = F, StepNumber = F, Value = F, Increment = F, StepNumberOverride = F, LaneIDOverride = F, MatrixIDOverride = F, PlacementIDDefault = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, Description = F, searchConditionsGroupType = "And", searchSortFieldNamesList = NULL, searchSortFieldNamesDescendingList = NULL, entityID = 1, schoolYearID = NULL, page = 1, pageSize = 100000, flatten = T, ...){

		params <- as.list(environment()) %>% append(list(...))

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- "all", searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		listSkyObjects(module = "Position", objectName = "Step", schoolYearID = schoolYearID, searchFields = searchFields, page = page, pageSize = pageSize, SearchConditionsList = searchConditionsList, SearchConditionsGroupType = searchConditionsGroupType, SearchSortFieldNamesList = searchSortFieldNamesList, SearchSortFieldNamesDescendingList = searchSortFieldNamesDescendingList, entityId = entityID, flatten = flatten)
	}


	listLanes <- function(searchConditionsList = NULL, LaneID = F, DistrictID = F, Code = F, Description = F, CodeDescription = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, searchConditionsGroupType = "And", searchSortFieldNamesList = NULL, searchSortFieldNamesDescendingList = NULL, entityID = 1, schoolYearID = NULL, page = 1, pageSize = 100000, flatten = T, ...){

		params <- as.list(environment()) %>% append(list(...))

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- "all", searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		listSkyObjects(module = "Position", objectName = "Lane", schoolYearID = schoolYearID, searchFields = searchFields, page = page, pageSize = pageSize, SearchConditionsList = searchConditionsList, SearchConditionsGroupType = searchConditionsGroupType, SearchSortFieldNamesList = searchSortFieldNamesList, SearchSortFieldNamesDescendingList = searchSortFieldNamesDescendingList, entityId = entityID, flatten = flatten)
	}


	listPositionSecurityGroups <- function(searchConditionsList = NULL, PositionSecurityGroupID = F, PositionID = F, GroupIDSecurity = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, searchConditionsGroupType = "And", searchSortFieldNamesList = NULL, searchSortFieldNamesDescendingList = NULL, entityID = 1, schoolYearID = NULL, page = 1, pageSize = 100000, flatten = T, ...){

		params <- as.list(environment()) %>% append(list(...))

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- "all", searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		listSkyObjects(module = "Position", objectName = "PositionSecurityGroup", schoolYearID = schoolYearID, searchFields = searchFields, page = page, pageSize = pageSize, SearchConditionsList = searchConditionsList, SearchConditionsGroupType = searchConditionsGroupType, SearchSortFieldNamesList = searchSortFieldNamesList, SearchSortFieldNamesDescendingList = searchSortFieldNamesDescendingList, entityId = entityID, flatten = flatten)
	}


	listTempOrganizationChartRelationshipErrors <- function(searchConditionsList = NULL, TempOrganizationChartRelationshipErrorID = F, CurrentEmployee = F, ErrorReason = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, EmployeeNumber = F, PositionNumber = F, TempOrganizationChartID = F, PositionTypeCode = F, PositionTypeDescription = F, PositionDistributionsAssignmentTypeCodes = F, PositionDistributionsAssignmentTypeDescriptions = F, PositionDistributionsBuildingCodes = F, PositionDistributionsBuildingDescriptions = F, searchConditionsGroupType = "And", searchSortFieldNamesList = NULL, searchSortFieldNamesDescendingList = NULL, entityID = 1, schoolYearID = NULL, page = 1, pageSize = 100000, flatten = T, ...){

		params <- as.list(environment()) %>% append(list(...))

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- "all", searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		listSkyObjects(module = "Position", objectName = "TempOrganizationChartRelationshipError", schoolYearID = schoolYearID, searchFields = searchFields, page = page, pageSize = pageSize, SearchConditionsList = searchConditionsList, SearchConditionsGroupType = searchConditionsGroupType, SearchSortFieldNamesList = searchSortFieldNamesList, SearchSortFieldNamesDescendingList = searchSortFieldNamesDescendingList, entityId = entityID, flatten = flatten)
	}


	listTempPositionAssignmentPayTypes <- function(searchConditionsList = NULL, TempPositionAssignmentPayTypeID = F, Employee = F, PayTypeCodeDescription = F, AssignmentPayTypeID = F, PositionTypeCodeDescription = F, AssignmentStartDate = F, AssignmentEndDate = F, PositionNumberCode = F, PositionID = F, AssignmentID = F, PositionTypeID = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, EmployeeNumber = F, PositionDistributionsAssignmentTypeCodes = F, PositionDistributionsAssignmentTypeDescriptions = F, PositionDistributionsBuildingCodes = F, PositionDistributionsBuildingDescriptions = F, searchConditionsGroupType = "And", searchSortFieldNamesList = NULL, searchSortFieldNamesDescendingList = NULL, entityID = 1, schoolYearID = NULL, page = 1, pageSize = 100000, flatten = T, ...){

		params <- as.list(environment()) %>% append(list(...))

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- "all", searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		listSkyObjects(module = "Position", objectName = "TempPositionAssignmentPayType", schoolYearID = schoolYearID, searchFields = searchFields, page = page, pageSize = pageSize, SearchConditionsList = searchConditionsList, SearchConditionsGroupType = searchConditionsGroupType, SearchSortFieldNamesList = searchSortFieldNamesList, SearchSortFieldNamesDescendingList = searchSortFieldNamesDescendingList, entityId = entityID, flatten = flatten)
	}


	listTempPositionAssignmentPayTypeErrors <- function(searchConditionsList = NULL, TempPositionAssignmentPayTypeErrorID = F, Employee = F, PositionNumberCode = F, PositionTypeCodeDescription = F, PayTypeCodeDescription = F, Message = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, EmployeeNumber = F, PositionDistributionsAssignmentTypeCodes = F, PositionDistributionsAssignmentTypeDescriptions = F, PositionDistributionsBuildingCodes = F, PositionDistributionsBuildingDescriptions = F, searchConditionsGroupType = "And", searchSortFieldNamesList = NULL, searchSortFieldNamesDescendingList = NULL, entityID = 1, schoolYearID = NULL, page = 1, pageSize = 100000, flatten = T, ...){

		params <- as.list(environment()) %>% append(list(...))

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- "all", searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		listSkyObjects(module = "Position", objectName = "TempPositionAssignmentPayTypeError", schoolYearID = schoolYearID, searchFields = searchFields, page = page, pageSize = pageSize, SearchConditionsList = searchConditionsList, SearchConditionsGroupType = searchConditionsGroupType, SearchSortFieldNamesList = searchSortFieldNamesList, SearchSortFieldNamesDescendingList = searchSortFieldNamesDescendingList, entityId = entityID, flatten = flatten)
	}


	listAssignmentTypeStateReportingDistributionSummaries <- function(searchConditionsList = NULL, StateReportingDistributionIDFirst = F, PositionTypeID = F, AssignmentTypeID = F, CurrentFTE = F, ClosingAssignmentFTE = F, VacantClosingAssignmentFTE = F, searchConditionsGroupType = "And", searchSortFieldNamesList = NULL, searchSortFieldNamesDescendingList = NULL, entityID = 1, schoolYearID = NULL, page = 1, pageSize = 100000, flatten = T, ...){

		params <- as.list(environment()) %>% append(list(...))

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- "all", searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		listSkyObjects(module = "Position", objectName = "AssignmentTypeStateReportingDistributionSummary", schoolYearID = schoolYearID, searchFields = searchFields, page = page, pageSize = pageSize, SearchConditionsList = searchConditionsList, SearchConditionsGroupType = searchConditionsGroupType, SearchSortFieldNamesList = searchSortFieldNamesList, SearchSortFieldNamesDescendingList = searchSortFieldNamesDescendingList, entityId = entityID, flatten = flatten)
	}


	listBuildingStateReportingDistributionSummaries <- function(searchConditionsList = NULL, StateReportingDistributionIDFirst = F, PositionTypeID = F, BuildingID = F, CurrentFTE = F, ClosingAssignmentFTE = F, VacantClosingAssignmentFTE = F, searchConditionsGroupType = "And", searchSortFieldNamesList = NULL, searchSortFieldNamesDescendingList = NULL, entityID = 1, schoolYearID = NULL, page = 1, pageSize = 100000, flatten = T, ...){

		params <- as.list(environment()) %>% append(list(...))

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- "all", searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		listSkyObjects(module = "Position", objectName = "BuildingStateReportingDistributionSummary", schoolYearID = schoolYearID, searchFields = searchFields, page = page, pageSize = pageSize, SearchConditionsList = searchConditionsList, SearchConditionsGroupType = searchConditionsGroupType, SearchSortFieldNamesList = searchSortFieldNamesList, SearchSortFieldNamesDescendingList = searchSortFieldNamesDescendingList, entityId = entityID, flatten = flatten)
	}


	listTempStateReportingDistributions <- function(searchConditionsList = NULL, TempStateReportingDistributionID = F, AssignmentID = F, PositionTypeID = F, AssignmentTypeID = F, BuildingID = F, DistrictNumber = F, StateDistrictTypeCodeMNID = F, StateDistrictTypeCodeMNCode = F, FileFolderNumber = F, STARSchoolNumber = F, SocialSecurityNumber = F, StateSTARAssignmentCodeMNID = F, StateSTARAssignmentCodeMNCode = F, StateSTARGradeLevelMNID = F, StateSTARGradeLevelMNCode = F, StateSTARModeOfTeachingMNID = F, StateSTARModeOfTeachingMNCode = F, STARPeriodsPerWeek = F, STARLengthOfPeriod = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, EmployeeID = F, EmployeeFullNameFML = F, EmployeeFullNameLFM = F, AssignmentCodeIdentifier = F, PositionTypeCodeDescription = F, AssignmentTypeCodeDescription = F, BuildingCodeDescription = F, Percentage = F, StateReportingDistributionID = F, SaveChanges = F, StateEISBilingualLanguageILID = F, StateEISGradeLevelAssignmentILID = F, StateEISPositionTimeFrameILID = F, StateEISNonCertifiedCategoryILIDOverride = F, STAROutOfDistrictAssignment = F, WISEStaffWorkingLEANumber = F, WISEStaffBilingualProgram = F, WISEStaffLongTermSub = F, WISEStaffAlternativeEducationProgram = F, WISEStaffSubcontractedIndividual = F, WISEStaffGradeLevelValue = F, StateEISEd360RoleILID = F, StateFTE = F, StateFTEOverride = F, EmployeeNumber = F, EISWorkload = F, StateRetirementJobCategoryWIID = F, IsPR1500Teacher = F, IsPR1500ESL = F, IsPR1500SpecialEd = F, PR1500TeacherGradeRange = F, StatePR1500AssignmentTypeTXID = F, PIMSAssignmentStartDate = F, StateREPAssignmentMIIDOverride = F, REPNumberOfClassesTaught = F, REPGradeLevels = F, REPEducationalSettings = F, EnableMOSISFiscalAgentCountyDistrictOverride = F, MOSISFiscalAgentCountyDistrictOverride = F, StateMOSISPositionMOIDOverride = F, StateMOSISCTEProgramTypeMOIDOverride = F, StatePSRSPEERSPositionMOIDOverride = F, searchConditionsGroupType = "And", searchSortFieldNamesList = NULL, searchSortFieldNamesDescendingList = NULL, entityID = 1, schoolYearID = NULL, page = 1, pageSize = 100000, flatten = T, ...){

		params <- as.list(environment()) %>% append(list(...))

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- "all", searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		listSkyObjects(module = "Position", objectName = "TempStateReportingDistribution", schoolYearID = schoolYearID, searchFields = searchFields, page = page, pageSize = pageSize, SearchConditionsList = searchConditionsList, SearchConditionsGroupType = searchConditionsGroupType, SearchSortFieldNamesList = searchSortFieldNamesList, SearchSortFieldNamesDescendingList = searchSortFieldNamesDescendingList, entityId = entityID, flatten = flatten)
	}


	listPositionTempExceptions <- function(searchConditionsList = NULL, TempExceptionID = F, Message = F, AssignmentIdentifier = F, AssignmentDetailIdentifier = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, EmployeeName = F, EmployeeNumber = F, PositionTypeCode = F, PositionTypeDescription = F, PositionDistributionsAssignmentTypeCodes = F, PositionDistributionsAssignmentTypeDescriptions = F, PositionDistributionsBuildingCodes = F, PositionDistributionsBuildingDescriptions = F, searchConditionsGroupType = "And", searchSortFieldNamesList = NULL, searchSortFieldNamesDescendingList = NULL, entityID = 1, schoolYearID = NULL, page = 1, pageSize = 100000, flatten = T, ...){

		params <- as.list(environment()) %>% append(list(...))

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- "all", searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		listSkyObjects(module = "Position", objectName = "TempException", schoolYearID = schoolYearID, searchFields = searchFields, page = page, pageSize = pageSize, SearchConditionsList = searchConditionsList, SearchConditionsGroupType = searchConditionsGroupType, SearchSortFieldNamesList = searchSortFieldNamesList, SearchSortFieldNamesDescendingList = searchSortFieldNamesDescendingList, entityId = entityID, flatten = flatten)
	}


	listTempStateReportingDistributionExceptions <- function(searchConditionsList = NULL, TempStateReportingDistributionExceptionID = F, EmployeeID = F, EmployeeFullNameFML = F, EmployeeFullNameLFM = F, AssignmentID = F, AssignmentCodeIdentifier = F, PositionTypeID = F, PositionTypeCodeDescription = F, AssignmentTypeID = F, AssignmentTypeCodeDescription = F, BuildingID = F, BuildingCodeDescription = F, Message = F, RetrievalSource = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, IsFatal = F, EmployeeNumber = F, searchConditionsGroupType = "And", searchSortFieldNamesList = NULL, searchSortFieldNamesDescendingList = NULL, entityID = 1, schoolYearID = NULL, page = 1, pageSize = 100000, flatten = T, ...){

		params <- as.list(environment()) %>% append(list(...))

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- "all", searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		listSkyObjects(module = "Position", objectName = "TempStateReportingDistributionException", schoolYearID = schoolYearID, searchFields = searchFields, page = page, pageSize = pageSize, SearchConditionsList = searchConditionsList, SearchConditionsGroupType = searchConditionsGroupType, SearchSortFieldNamesList = searchSortFieldNamesList, SearchSortFieldNamesDescendingList = searchSortFieldNamesDescendingList, entityId = entityID, flatten = flatten)
	}


	listPositionConfigDistricts <- function(searchConditionsList = NULL, ConfigDistrictID = F, DistrictID = F, AssignmentOverlapThreshold = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, searchConditionsGroupType = "And", searchSortFieldNamesList = NULL, searchSortFieldNamesDescendingList = NULL, entityID = 1, schoolYearID = NULL, page = 1, pageSize = 100000, flatten = T, ...){

		params <- as.list(environment()) %>% append(list(...))

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- "all", searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		listSkyObjects(module = "Position", objectName = "ConfigDistrict", schoolYearID = schoolYearID, searchFields = searchFields, page = page, pageSize = pageSize, SearchConditionsList = searchConditionsList, SearchConditionsGroupType = searchConditionsGroupType, SearchSortFieldNamesList = searchSortFieldNamesList, SearchSortFieldNamesDescendingList = searchSortFieldNamesDescendingList, entityId = entityID, flatten = flatten)
	}


	listSupplementCalculationMethods <- function(searchConditionsList = NULL, SupplementCalculationMethodID = F, FiscalYearID = F, DistrictID = F, Code = F, Description = F, SkywardID = F, SkywardIDClonedFrom = F, SupplementCalculationMethodIDClonedFrom = F, AnnualPayExpression = F, HourlyPayExpression = F, DailyPayExpression = F, IsSystemLoaded = F, CodeDescription = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, TotalPayExpression = F, RenderRestore = F, SkywardHash = F, searchConditionsGroupType = "And", searchSortFieldNamesList = NULL, searchSortFieldNamesDescendingList = NULL, entityID = 1, schoolYearID = NULL, page = 1, pageSize = 100000, flatten = T, ...){

		params <- as.list(environment()) %>% append(list(...))

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- "all", searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		listSkyObjects(module = "Position", objectName = "SupplementCalculationMethod", schoolYearID = schoolYearID, searchFields = searchFields, page = page, pageSize = pageSize, SearchConditionsList = searchConditionsList, SearchConditionsGroupType = searchConditionsGroupType, SearchSortFieldNamesList = searchSortFieldNamesList, SearchSortFieldNamesDescendingList = searchSortFieldNamesDescendingList, entityId = entityID, flatten = flatten)
	}


	listSupplementTypes <- function(searchConditionsList = NULL, SupplementTypeID = F, Code = F, Description = F, DistrictID = F, FiscalYearID = F, AmountType = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, SupplementCalculationMethodID = F, CodeDescription = F, TotalPlanPositionSupplements = F, SupplementTypeIDClonedFrom = F, searchConditionsGroupType = "And", searchSortFieldNamesList = NULL, searchSortFieldNamesDescendingList = NULL, entityID = 1, schoolYearID = NULL, page = 1, pageSize = 100000, flatten = T, ...){

		params <- as.list(environment()) %>% append(list(...))

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- "all", searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		listSkyObjects(module = "Position", objectName = "SupplementType", schoolYearID = schoolYearID, searchFields = searchFields, page = page, pageSize = pageSize, SearchConditionsList = searchConditionsList, SearchConditionsGroupType = searchConditionsGroupType, SearchSortFieldNamesList = searchSortFieldNamesList, SearchSortFieldNamesDescendingList = searchSortFieldNamesDescendingList, entityId = entityID, flatten = flatten)
	}


	listSupplements <- function(searchConditionsList = NULL, SupplementID = F, SupplementTypeID = F, AssignmentDetailID = F, EnteredRate = F, DailyPay = F, HourlyPay = F, TotalPay = F, AnnualizedPay = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, SupplementIDClonedFrom = F, PlanPositionSupplementID = F, searchConditionsGroupType = "And", searchSortFieldNamesList = NULL, searchSortFieldNamesDescendingList = NULL, entityID = 1, schoolYearID = NULL, page = 1, pageSize = 100000, flatten = T, ...){

		params <- as.list(environment()) %>% append(list(...))

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- "all", searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		listSkyObjects(module = "Position", objectName = "Supplement", schoolYearID = schoolYearID, searchFields = searchFields, page = page, pageSize = pageSize, SearchConditionsList = searchConditionsList, SearchConditionsGroupType = searchConditionsGroupType, SearchSortFieldNamesList = searchSortFieldNamesList, SearchSortFieldNamesDescendingList = searchSortFieldNamesDescendingList, entityId = entityID, flatten = flatten)
	}


	listTempMassUpdatePositions <- function(searchConditionsList = NULL, TempMassUpdatePositionID = F, PositionID = F, EmployeeFullNameLFM = F, PositionNumberCode = F, PositionNumberID = F, OldPositionTypeCodeDescription = F, NewPositionTypeCodeDescription = F, PositionTypeID = F, PositionStartDate = F, PositionEndDate = F, OldFullPaySecondsPerDay = F, NewFullPaySecondsPerDay = F, OldPositionGroupCodeDescription = F, NewPositionGroupCodeDescription = F, PositionGroupID = F, OldJobTypeCodeDescription = F, NewJobTypeCodeDescription = F, JobTypeID = F, TotalPaidSeconds = F, ActivePaidDayCount = F, FullPaidDays = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, OldStateTRSBaseLaneTXCodeDescription = F, NewStateTRSBaseLaneTXCodeDescription = F, StateTRSBaseLaneTXID = F, EmployeeNumber = F, PositionDistributionsAssignmentTypeCodes = F, PositionDistributionsAssignmentTypeDescriptions = F, PositionDistributionsBuildingCodes = F, PositionDistributionsBuildingDescriptions = F, PositionDistributionsFTEGroupCodes = F, PositionDistributionsFTEGroupDescriptions = F, ErrorCount = F, OldCalendarCodeDescription = F, NewCalendarCodeDescription = F, CalendarID = F, CanBeUpdated = F, ExceptionMessage = F, searchConditionsGroupType = "And", searchSortFieldNamesList = NULL, searchSortFieldNamesDescendingList = NULL, entityID = 1, schoolYearID = NULL, page = 1, pageSize = 100000, flatten = T, ...){

		params <- as.list(environment()) %>% append(list(...))

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- "all", searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		listSkyObjects(module = "Position", objectName = "TempMassUpdatePosition", schoolYearID = schoolYearID, searchFields = searchFields, page = page, pageSize = pageSize, SearchConditionsList = searchConditionsList, SearchConditionsGroupType = searchConditionsGroupType, SearchSortFieldNamesList = searchSortFieldNamesList, SearchSortFieldNamesDescendingList = searchSortFieldNamesDescendingList, entityId = entityID, flatten = flatten)
	}


	listTempMassUpdatePositionAssignmentDetails <- function(searchConditionsList = NULL, TempMassUpdatePositionAssignmentDetailID = F, AssignmentID = F, EmployeeFullNameLFM = F, OriginalAssignmentDetailID = F, AssignmentDetailStartDate = F, AssignmentDetailEndDate = F, OldTotalPay = F, NewTotalPay = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, EmployeeNumber = F, searchConditionsGroupType = "And", searchSortFieldNamesList = NULL, searchSortFieldNamesDescendingList = NULL, entityID = 1, schoolYearID = NULL, page = 1, pageSize = 100000, flatten = T, ...){

		params <- as.list(environment()) %>% append(list(...))

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- "all", searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		listSkyObjects(module = "Position", objectName = "TempMassUpdatePositionAssignmentDetail", schoolYearID = schoolYearID, searchFields = searchFields, page = page, pageSize = pageSize, SearchConditionsList = searchConditionsList, SearchConditionsGroupType = searchConditionsGroupType, SearchSortFieldNamesList = searchSortFieldNamesList, SearchSortFieldNamesDescendingList = searchSortFieldNamesDescendingList, entityId = entityID, flatten = flatten)
	}


	listTempSupplements <- function(searchConditionsList = NULL, TempSupplementID = F, AssignmentDetailID = F, EmployeeFullNameLFM = F, SalaryCalculationMethodCodeDescription = F, OldAssignmentDetailHourlyPay = F, OldAssignmentDetailDailyPay = F, OldAssignmentDetailAnnualizedPay = F, OldAssignmentDetailTotalPay = F, NewAssignmentDetailHourlyPay = F, NewAssignmentDetailDailyPay = F, NewAssignmentDetailAnnualizedPay = F, NewAssignmentDetailTotalPay = F, SupplementHourlyPay = F, SupplementDailyPay = F, SupplementAnnualizedPay = F, SupplementPayTotal = F, StartDate = F, EndDate = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, SupplementID = F, OldSupplementHourlyPay = F, OldSupplementDailyPay = F, OldSupplementAnnualizedPay = F, OldSupplementPayTotal = F, OldSupplementEnteredRate = F, NewSupplementEnteredRate = F, EnteredRate = F, TotalPay = F, HourlyPay = F, DailyPay = F, AnnualizedPay = F, SupplementType = F, EmployeeNumber = F, PositionNumberCode = F, PositionTypeCode = F, PositionTypeDescription = F, PositionDistributionsAssignmentTypeCodes = F, PositionDistributionsAssignmentTypeDescriptions = F, PositionDistributionsBuildingCodes = F, PositionDistributionsBuildingDescriptions = F, ErrorCount = F, HasErrors = F, TempPositionID = F, SupplementTypeID = F, PlanPositionSupplementID = F, IsMinimumSalarySupplement = F, searchConditionsGroupType = "And", searchSortFieldNamesList = NULL, searchSortFieldNamesDescendingList = NULL, entityID = 1, schoolYearID = NULL, page = 1, pageSize = 100000, flatten = T, ...){

		params <- as.list(environment()) %>% append(list(...))

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- "all", searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		listSkyObjects(module = "Position", objectName = "TempSupplement", schoolYearID = schoolYearID, searchFields = searchFields, page = page, pageSize = pageSize, SearchConditionsList = searchConditionsList, SearchConditionsGroupType = searchConditionsGroupType, SearchSortFieldNamesList = searchSortFieldNamesList, SearchSortFieldNamesDescendingList = searchSortFieldNamesDescendingList, entityId = entityID, flatten = flatten)
	}


	listTempSupplementErrors <- function(searchConditionsList = NULL, TempSupplementErrorID = F, EmployeeFullNameLFM = F, SalaryCalculationMethodCodeDescription = F, OldAssignmentDetailHourlyPay = F, OldAssignmentDetailDailyPay = F, OldAssignmentDetailAnnualizedPay = F, OldAssignmentDetailTotalPay = F, NewAssignmentDetailHourlyPay = F, NewAssignmentDetailDailyPay = F, NewAssignmentDetailAnnualizedPay = F, NewAssignmentDetailTotalPay = F, SupplementHourlyPay = F, SupplementDailyPay = F, SupplementAnnualizedPay = F, SupplementPayTotal = F, StartDate = F, EndDate = F, ErrorDescription = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, OldSupplementHourlyPay = F, OldSupplementDailyPay = F, OldSupplementAnnualizedPay = F, OldSupplementPayTotal = F, OldSupplementEnteredRate = F, NewSupplementEnteredRate = F, EmployeeNumber = F, PositionNumberCode = F, PositionTypeCode = F, PositionTypeDescription = F, PositionDistributionsAssignmentTypeCodes = F, PositionDistributionsAssignmentTypeDescriptions = F, PositionDistributionsBuildingCodes = F, PositionDistributionsBuildingDescriptions = F, TempSupplementID = F, ErrorNumber = F, IsMinimumSalarySupplement = F, searchConditionsGroupType = "And", searchSortFieldNamesList = NULL, searchSortFieldNamesDescendingList = NULL, entityID = 1, schoolYearID = NULL, page = 1, pageSize = 100000, flatten = T, ...){

		params <- as.list(environment()) %>% append(list(...))

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- "all", searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		listSkyObjects(module = "Position", objectName = "TempSupplementError", schoolYearID = schoolYearID, searchFields = searchFields, page = page, pageSize = pageSize, SearchConditionsList = searchConditionsList, SearchConditionsGroupType = searchConditionsGroupType, SearchSortFieldNamesList = searchSortFieldNamesList, SearchSortFieldNamesDescendingList = searchSortFieldNamesDescendingList, entityId = entityID, flatten = flatten)
	}


	listOrganizationChartRelationshipBridges <- function(searchConditionsList = NULL, OrganizationChartRelationshipBridgeID = F, OrganizationChartID = F, PositionIDSupervisor = F, PositionIDEmployee = F, LevelsBelowSupervisor = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, searchConditionsGroupType = "And", searchSortFieldNamesList = NULL, searchSortFieldNamesDescendingList = NULL, entityID = 1, schoolYearID = NULL, page = 1, pageSize = 100000, flatten = T, ...){

		params <- as.list(environment()) %>% append(list(...))

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- "all", searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		listSkyObjects(module = "Position", objectName = "OrganizationChartRelationshipBridge", schoolYearID = schoolYearID, searchFields = searchFields, page = page, pageSize = pageSize, SearchConditionsList = searchConditionsList, SearchConditionsGroupType = searchConditionsGroupType, SearchSortFieldNamesList = searchSortFieldNamesList, SearchSortFieldNamesDescendingList = searchSortFieldNamesDescendingList, entityId = entityID, flatten = flatten)
	}


	listTempOrganizationCharts <- function(searchConditionsList = NULL, TempOrganizationChartID = F, Code = F, Description = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, searchConditionsGroupType = "And", searchSortFieldNamesList = NULL, searchSortFieldNamesDescendingList = NULL, entityID = 1, schoolYearID = NULL, page = 1, pageSize = 100000, flatten = T, ...){

		params <- as.list(environment()) %>% append(list(...))

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- "all", searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		listSkyObjects(module = "Position", objectName = "TempOrganizationChart", schoolYearID = schoolYearID, searchFields = searchFields, page = page, pageSize = pageSize, SearchConditionsList = searchConditionsList, SearchConditionsGroupType = searchConditionsGroupType, SearchSortFieldNamesList = searchSortFieldNamesList, SearchSortFieldNamesDescendingList = searchSortFieldNamesDescendingList, entityId = entityID, flatten = flatten)
	}


	listTempPositionDistributions <- function(searchConditionsList = NULL, TempPositionDistributionID = F, PositionIDClonedFrom = F, PlanPositionIDClonedFrom = F, AssignmentTypeID = F, BuildingID = F, FTEGroupID = F, DepartmentID = F, BudgetedFTE = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, TempPositionID = F, PositionDistributionID = F, PositionDistributionSetID = F, AssignmentTypeCodeDescription = F, NewAssignmentTypeID = F, NewAssignmentTypeCodeDescription = F, BuildingCodeDescription = F, NewBuildingID = F, NewBuildingCodeDescription = F, FTEGroupCodeDescription = F, DepartmentCodeDescription = F, NewDepartmentID = F, NewDepartmentCodeDescription = F, Merged = F, ToDelete = F, Employee = F, PositionType = F, ErrorCount = F, OldAccountDistribution = F, NewAccountDistribution = F, FullPaySecondsPerDay = F, BudgetedHoursPerDay = F, searchConditionsGroupType = "And", searchSortFieldNamesList = NULL, searchSortFieldNamesDescendingList = NULL, entityID = 1, schoolYearID = NULL, page = 1, pageSize = 100000, flatten = T, ...){

		params <- as.list(environment()) %>% append(list(...))

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- "all", searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		listSkyObjects(module = "Position", objectName = "TempPositionDistribution", schoolYearID = schoolYearID, searchFields = searchFields, page = page, pageSize = pageSize, SearchConditionsList = searchConditionsList, SearchConditionsGroupType = searchConditionsGroupType, SearchSortFieldNamesList = searchSortFieldNamesList, SearchSortFieldNamesDescendingList = searchSortFieldNamesDescendingList, entityId = entityID, flatten = flatten)
	}


	listAssignmentTypePositionDistributionSummaries <- function(searchConditionsList = NULL, PositionDistributionIDFirst = F, PositionTypeID = F, AssignmentTypeID = F, BudgetedFTE = F, ClosingAssignmentFTE = F, VacantClosingAssignmentFTE = F, searchConditionsGroupType = "And", searchSortFieldNamesList = NULL, searchSortFieldNamesDescendingList = NULL, entityID = 1, schoolYearID = NULL, page = 1, pageSize = 100000, flatten = T, ...){

		params <- as.list(environment()) %>% append(list(...))

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- "all", searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		listSkyObjects(module = "Position", objectName = "AssignmentTypePositionDistributionSummary", schoolYearID = schoolYearID, searchFields = searchFields, page = page, pageSize = pageSize, SearchConditionsList = searchConditionsList, SearchConditionsGroupType = searchConditionsGroupType, SearchSortFieldNamesList = searchSortFieldNamesList, SearchSortFieldNamesDescendingList = searchSortFieldNamesDescendingList, entityId = entityID, flatten = flatten)
	}


	listBuildingPositionDistributionSummaries <- function(searchConditionsList = NULL, PositionDistributionIDFirst = F, PositionTypeID = F, BuildingID = F, BudgetedFTE = F, ClosingAssignmentFTE = F, VacantClosingAssignmentFTE = F, searchConditionsGroupType = "And", searchSortFieldNamesList = NULL, searchSortFieldNamesDescendingList = NULL, entityID = 1, schoolYearID = NULL, page = 1, pageSize = 100000, flatten = T, ...){

		params <- as.list(environment()) %>% append(list(...))

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- "all", searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		listSkyObjects(module = "Position", objectName = "BuildingPositionDistributionSummary", schoolYearID = schoolYearID, searchFields = searchFields, page = page, pageSize = pageSize, SearchConditionsList = searchConditionsList, SearchConditionsGroupType = searchConditionsGroupType, SearchSortFieldNamesList = searchSortFieldNamesList, SearchSortFieldNamesDescendingList = searchSortFieldNamesDescendingList, entityId = entityID, flatten = flatten)
	}


	listPositionDistributions <- function(searchConditionsList = NULL, PositionDistributionID = F, AssignmentTypeID = F, PositionID = F, BuildingID = F, FTEGroupID = F, DepartmentID = F, BudgetedFTE = F, ClosingAssignmentFTE = F, VacantClosingAssignmentFTE = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, PositionDistributionSetID = F, BudgetedHoursPerDay = F, CalculatedBudgetedHoursPerDay = F, searchConditionsGroupType = "And", searchSortFieldNamesList = NULL, searchSortFieldNamesDescendingList = NULL, entityID = 1, schoolYearID = NULL, page = 1, pageSize = 100000, flatten = T, ...){

		params <- as.list(environment()) %>% append(list(...))

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- "all", searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		listSkyObjects(module = "Position", objectName = "PositionDistribution", schoolYearID = schoolYearID, searchFields = searchFields, page = page, pageSize = pageSize, SearchConditionsList = searchConditionsList, SearchConditionsGroupType = searchConditionsGroupType, SearchSortFieldNamesList = searchSortFieldNamesList, SearchSortFieldNamesDescendingList = searchSortFieldNamesDescendingList, entityId = entityID, flatten = flatten)
	}


	listEmployeePositionTypes <- function(searchConditionsList = NULL, EmployeePositionTypeID = F, EmployeeID = F, PositionTypeID = F, CurrentTotalFTE = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, TotalPay = F, HasPositionsToMerge = F, searchConditionsGroupType = "And", searchSortFieldNamesList = NULL, searchSortFieldNamesDescendingList = NULL, entityID = 1, schoolYearID = NULL, page = 1, pageSize = 100000, flatten = T, ...){

		params <- as.list(environment()) %>% append(list(...))

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- "all", searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		listSkyObjects(module = "Position", objectName = "EmployeePositionType", schoolYearID = schoolYearID, searchFields = searchFields, page = page, pageSize = pageSize, SearchConditionsList = searchConditionsList, SearchConditionsGroupType = searchConditionsGroupType, SearchSortFieldNamesList = searchSortFieldNamesList, SearchSortFieldNamesDescendingList = searchSortFieldNamesDescendingList, entityId = entityID, flatten = flatten)
	}


	listAssignmentDelimitedFileFormats <- function(searchConditionsList = NULL, AssignmentDelimitedFileFormatID = F, AssignmentThirdPartyFormatID = F, NumberOfHeaderRows = F, DelimiterCharacter = F, EmployeeIdentifierColumnNumber = F, PositionNumberColumnNumber = F, PositionStartDateColumnNumber = F, PositionEndDateColumnNumber = F, PositionCalendarColumnNumber = F, FullHoursPerDayColumnNumber = F, PositionGroupColumnNumber = F, JobTypeColumnNumber = F, PositionTypeColumnNumber = F, AssignmentTypeColumnNumber = F, BuildingColumnNumber = F, FTEGroupColumnNumber = F, DepartmentColumnNumber = F, PositionFTEColumnNumber = F, PositionDistributionAccountDistributionColumnNumber = F, AssignmentStartDateColumnNumber = F, AssignmentEndDateColumnNumber = F, AssignmentHoursPerDayColumnNumber = F, AssignmentFTEColumnNumber = F, SalaryCalculationMethodColumnNumber = F, MatrixColumnNumber = F, StepColumnNumber = F, LaneColumnNumber = F, CreditsColumnNumber = F, PlacementColumnNumber = F, RateColumnNumber = F, EntitlementColumnNumber = F, AssignmentTimeTrackingGroupColumnNumber = F, NextYearIntentColumnNumber = F, SupplementType1ColumnNumber = F, SupplementAmount1ColumnNumber = F, SupplementType2ColumnNumber = F, SupplementAmount2ColumnNumber = F, SupplementType3ColumnNumber = F, SupplementAmount3ColumnNumber = F, DelimiterType = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, searchConditionsGroupType = "And", searchSortFieldNamesList = NULL, searchSortFieldNamesDescendingList = NULL, entityID = 1, schoolYearID = NULL, page = 1, pageSize = 100000, flatten = T, ...){

		params <- as.list(environment()) %>% append(list(...))

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- "all", searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		listSkyObjects(module = "Position", objectName = "AssignmentDelimitedFileFormat", schoolYearID = schoolYearID, searchFields = searchFields, page = page, pageSize = pageSize, SearchConditionsList = searchConditionsList, SearchConditionsGroupType = searchConditionsGroupType, SearchSortFieldNamesList = searchSortFieldNamesList, SearchSortFieldNamesDescendingList = searchSortFieldNamesDescendingList, entityId = entityID, flatten = flatten)
	}


	listAssignmentThirdPartyFormats <- function(searchConditionsList = NULL, AssignmentThirdPartyFormatID = F, DistrictID = F, Code = F, Description = F, EmployeeIdentification = F, DateFormat = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, searchConditionsGroupType = "And", searchSortFieldNamesList = NULL, searchSortFieldNamesDescendingList = NULL, entityID = 1, schoolYearID = NULL, page = 1, pageSize = 100000, flatten = T, ...){

		params <- as.list(environment()) %>% append(list(...))

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- "all", searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		listSkyObjects(module = "Position", objectName = "AssignmentThirdPartyFormat", schoolYearID = schoolYearID, searchFields = searchFields, page = page, pageSize = pageSize, SearchConditionsList = searchConditionsList, SearchConditionsGroupType = searchConditionsGroupType, SearchSortFieldNamesList = searchSortFieldNamesList, SearchSortFieldNamesDescendingList = searchSortFieldNamesDescendingList, entityId = entityID, flatten = flatten)
	}


	listAssignmentThirdPartyImports <- function(searchConditionsList = NULL, AssignmentThirdPartyImportID = F, AssignmentThirdPartyFormatID = F, ImportTime = F, MediaID = F, MediaIDFailedResult = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, searchConditionsGroupType = "And", searchSortFieldNamesList = NULL, searchSortFieldNamesDescendingList = NULL, entityID = 1, schoolYearID = NULL, page = 1, pageSize = 100000, flatten = T, ...){

		params <- as.list(environment()) %>% append(list(...))

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- "all", searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		listSkyObjects(module = "Position", objectName = "AssignmentThirdPartyImport", schoolYearID = schoolYearID, searchFields = searchFields, page = page, pageSize = pageSize, SearchConditionsList = searchConditionsList, SearchConditionsGroupType = searchConditionsGroupType, SearchSortFieldNamesList = searchSortFieldNamesList, SearchSortFieldNamesDescendingList = searchSortFieldNamesDescendingList, entityId = entityID, flatten = flatten)
	}


	listPositionDistributionSets <- function(searchConditionsList = NULL, PositionDistributionSetID = F, PositionID = F, StartDate = F, EndDate = F, BudgetedFTE = F, IsCurrent = F, IsClosing = F, PositionDistributionsBuildingCodes = F, PositionDistributionsBuildingDescriptions = F, PositionDistributionsDepartmentCodes = F, PositionDistributionsDepartmentDescriptions = F, PositionDistributionsAssignmentTypeCodes = F, PositionDistributionsAssignmentTypeDescriptions = F, PositionDistributionsFTEGroupCodes = F, PositionDistributionsFTEGroupDescriptions = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, AccountDistribution = F, BudgetedHoursPerDay = F, searchConditionsGroupType = "And", searchSortFieldNamesList = NULL, searchSortFieldNamesDescendingList = NULL, entityID = 1, schoolYearID = NULL, page = 1, pageSize = 100000, flatten = T, ...){

		params <- as.list(environment()) %>% append(list(...))

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- "all", searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		listSkyObjects(module = "Position", objectName = "PositionDistributionSet", schoolYearID = schoolYearID, searchFields = searchFields, page = page, pageSize = pageSize, SearchConditionsList = searchConditionsList, SearchConditionsGroupType = searchConditionsGroupType, SearchSortFieldNamesList = searchSortFieldNamesList, SearchSortFieldNamesDescendingList = searchSortFieldNamesDescendingList, entityId = entityID, flatten = flatten)
	}


	listPositionDistributionAccountDistributions <- function(searchConditionsList = NULL, PositionDistributionAccountDistributionID = F, PositionDistributionID = F, AccountID = F, DistributionPercent = F, ProratedDistributionPercent = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, searchConditionsGroupType = "And", searchSortFieldNamesList = NULL, searchSortFieldNamesDescendingList = NULL, entityID = 1, schoolYearID = NULL, page = 1, pageSize = 100000, flatten = T, ...){

		params <- as.list(environment()) %>% append(list(...))

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- "all", searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		listSkyObjects(module = "Position", objectName = "PositionDistributionAccountDistribution", schoolYearID = schoolYearID, searchFields = searchFields, page = page, pageSize = pageSize, SearchConditionsList = searchConditionsList, SearchConditionsGroupType = searchConditionsGroupType, SearchSortFieldNamesList = searchSortFieldNamesList, SearchSortFieldNamesDescendingList = searchSortFieldNamesDescendingList, entityId = entityID, flatten = flatten)
	}


	listTempPositionDistributionAccountDistributions <- function(searchConditionsList = NULL, TempPositionDistributionAccountDistributionID = F, PositionDistributionID = F, TempPositionDistributionID = F, AccountID = F, DistributionPercent = F, StateConcordDepartmentTNID = F, OldAccount = F, NewAccount = F, TempAssignmentPayTypeID = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, searchConditionsGroupType = "And", searchSortFieldNamesList = NULL, searchSortFieldNamesDescendingList = NULL, entityID = 1, schoolYearID = NULL, page = 1, pageSize = 100000, flatten = T, ...){

		params <- as.list(environment()) %>% append(list(...))

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- "all", searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		listSkyObjects(module = "Position", objectName = "TempPositionDistributionAccountDistribution", schoolYearID = schoolYearID, searchFields = searchFields, page = page, pageSize = pageSize, SearchConditionsList = searchConditionsList, SearchConditionsGroupType = searchConditionsGroupType, SearchSortFieldNamesList = searchSortFieldNamesList, SearchSortFieldNamesDescendingList = searchSortFieldNamesDescendingList, entityId = entityID, flatten = flatten)
	}


	listTempAssignmentCalendarDays <- function(searchConditionsList = NULL, TempAssignmentCalendarDayID = F, Date = F, OldIsWorkday = F, IsWorkday = F, OldIsPaid = F, IsPaid = F, OldSeconds = F, Seconds = F, ErrorCount = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, searchConditionsGroupType = "And", searchSortFieldNamesList = NULL, searchSortFieldNamesDescendingList = NULL, entityID = 1, schoolYearID = NULL, page = 1, pageSize = 100000, flatten = T, ...){

		params <- as.list(environment()) %>% append(list(...))

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- "all", searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		listSkyObjects(module = "Position", objectName = "TempAssignmentCalendarDay", schoolYearID = schoolYearID, searchFields = searchFields, page = page, pageSize = pageSize, SearchConditionsList = searchConditionsList, SearchConditionsGroupType = searchConditionsGroupType, SearchSortFieldNamesList = searchSortFieldNamesList, SearchSortFieldNamesDescendingList = searchSortFieldNamesDescendingList, entityId = entityID, flatten = flatten)
	}


	listAssignmentCalendarDays <- function(searchConditionsList = NULL, CalendarID = F, CalendarDayID = F, AssignmentID = F, EmployeeID = F, AssignmentDetailID = F, AssignmentCalendarDayOverrideID = F, Date = F, IsPaid = F, IsWorkday = F, Comment = F, AssignmentDetailSecondsPerDay = F, PaidSeconds = F, OverrideExists = F, IsPaidHoliday = F, searchConditionsGroupType = "And", searchSortFieldNamesList = NULL, searchSortFieldNamesDescendingList = NULL, entityID = 1, schoolYearID = NULL, page = 1, pageSize = 100000, flatten = T, ...){

		params <- as.list(environment()) %>% append(list(...))

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- "all", searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		listSkyObjects(module = "Position", objectName = "AssignmentCalendarDay", schoolYearID = schoolYearID, searchFields = searchFields, page = page, pageSize = pageSize, SearchConditionsList = searchConditionsList, SearchConditionsGroupType = searchConditionsGroupType, SearchSortFieldNamesList = searchSortFieldNamesList, SearchSortFieldNamesDescendingList = searchSortFieldNamesDescendingList, entityId = entityID, flatten = flatten)
	}


	listAssignmentCalendarDayOverrides <- function(searchConditionsList = NULL, AssignmentCalendarDayOverrideID = F, AssignmentID = F, CalendarID = F, EmployeeID = F, IsPaid = F, IsWorkday = F, Date = F, OverrideSeconds = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, searchConditionsGroupType = "And", searchSortFieldNamesList = NULL, searchSortFieldNamesDescendingList = NULL, entityID = 1, schoolYearID = NULL, page = 1, pageSize = 100000, flatten = T, ...){

		params <- as.list(environment()) %>% append(list(...))

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- "all", searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		listSkyObjects(module = "Position", objectName = "AssignmentCalendarDayOverride", schoolYearID = schoolYearID, searchFields = searchFields, page = page, pageSize = pageSize, SearchConditionsList = searchConditionsList, SearchConditionsGroupType = searchConditionsGroupType, SearchSortFieldNamesList = searchSortFieldNamesList, SearchSortFieldNamesDescendingList = searchSortFieldNamesDescendingList, entityId = entityID, flatten = flatten)
	}


	listOrganizationChartApprovers <- function(searchConditionsList = NULL, OrganizationChartID = F, OrganizationChartRelationshipIDSupervisor = F, OrganizationChartRelationshipIDEmployee = F, PositionDistributionID = F, LevelsBelowSupervisor = F, IsFinalApproval = F, PositionIDSupervisor = F, PositionIDEmployee = F, IsApprovalPathForApprovalObject = F, ApprovalStatusForApprovalObject = F, searchConditionsGroupType = "And", searchSortFieldNamesList = NULL, searchSortFieldNamesDescendingList = NULL, entityID = 1, schoolYearID = NULL, page = 1, pageSize = 100000, flatten = T, ...){

		params <- as.list(environment()) %>% append(list(...))

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- "all", searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		listSkyObjects(module = "Position", objectName = "OrganizationChartApprover", schoolYearID = schoolYearID, searchFields = searchFields, page = page, pageSize = pageSize, SearchConditionsList = searchConditionsList, SearchConditionsGroupType = searchConditionsGroupType, SearchSortFieldNamesList = searchSortFieldNamesList, SearchSortFieldNamesDescendingList = searchSortFieldNamesDescendingList, entityId = entityID, flatten = flatten)
	}


	listOrganizationChartRelationshipPositionDistributions <- function(searchConditionsList = NULL, OrganizationChartRelationshipPositionDistributionID = F, OrganizationChartRelationshipID = F, PositionDistributionID = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, searchConditionsGroupType = "And", searchSortFieldNamesList = NULL, searchSortFieldNamesDescendingList = NULL, entityID = 1, schoolYearID = NULL, page = 1, pageSize = 100000, flatten = T, ...){

		params <- as.list(environment()) %>% append(list(...))

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- "all", searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		listSkyObjects(module = "Position", objectName = "OrganizationChartRelationshipPositionDistribution", schoolYearID = schoolYearID, searchFields = searchFields, page = page, pageSize = pageSize, SearchConditionsList = searchConditionsList, SearchConditionsGroupType = searchConditionsGroupType, SearchSortFieldNamesList = searchSortFieldNamesList, SearchSortFieldNamesDescendingList = searchSortFieldNamesDescendingList, entityId = entityID, flatten = flatten)
	}


	listTempOrganizationChartRelationshipPositionDistributions <- function(searchConditionsList = NULL, TempOrganizationChartRelationshipPositionDistributionID = F, TempType = F, OrganizationChartRelationshipPositionDistributionID = F, OrganizationChartRelationshipID = F, OrganizationChartCode = F, OrganizationChartDescription = F, SupervisorEmployeeNumber = F, SupervisorFullName = F, SupervisorPositionTypeCode = F, SupervisorPositionTypeDescription = F, SupervisorAssignmentTypeCodes = F, SupervisorBuildingCodes = F, PositionDistributionID = F, EmployeePositionNumber = F, EmployeeAssignmentTypeCode = F, EmployeeAssignmentTypeDescription = F, EmployeeBuildingCode = F, EmployeeBuildingDescription = F, EmployeeFTEGroupCode = F, EmployeeFTEGroupDescription = F, EmployeeNumber = F, EmployeeFullName = F, ErrorCount = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, searchConditionsGroupType = "And", searchSortFieldNamesList = NULL, searchSortFieldNamesDescendingList = NULL, entityID = 1, schoolYearID = NULL, page = 1, pageSize = 100000, flatten = T, ...){

		params <- as.list(environment()) %>% append(list(...))

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- "all", searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		listSkyObjects(module = "Position", objectName = "TempOrganizationChartRelationshipPositionDistribution", schoolYearID = schoolYearID, searchFields = searchFields, page = page, pageSize = pageSize, SearchConditionsList = searchConditionsList, SearchConditionsGroupType = searchConditionsGroupType, SearchSortFieldNamesList = searchSortFieldNamesList, SearchSortFieldNamesDescendingList = searchSortFieldNamesDescendingList, entityId = entityID, flatten = flatten)
	}
