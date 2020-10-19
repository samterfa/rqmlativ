

	listAssignmentTimesheets <- function(searchConditionsList = NULL, AssignmentTimesheetWeekIDFirst = F, AssignmentID = F, TimesheetWeekID = F, searchConditionsGroupType = "And", searchSortFieldNamesList = NULL, searchSortFieldNamesDescendingList = NULL, entityID = 1, schoolYearID = NULL, page = 1, pageSize = 100000, flatten = T, ...){

		params <- as.list(environment()) %>% append(list(...))

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- "all", searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		listSkyObjects(module = "TimeTracking", objectName = "AssignmentTimesheet", schoolYearID = schoolYearID, searchFields = searchFields, page = page, pageSize = pageSize, SearchConditionsList = searchConditionsList, SearchConditionsGroupType = searchConditionsGroupType, SearchSortFieldNamesList = searchSortFieldNamesList, SearchSortFieldNamesDescendingList = searchSortFieldNamesDescendingList, entityId = entityID, flatten = flatten)
	}


	listTimeTrackingConfigFiscalYears <- function(searchConditionsList = NULL, ConfigFiscalYearID = F, FiscalYearID = F, DistrictID = F, OrganizationChartID = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, searchConditionsGroupType = "And", searchSortFieldNamesList = NULL, searchSortFieldNamesDescendingList = NULL, entityID = 1, schoolYearID = NULL, page = 1, pageSize = 100000, flatten = T, ...){

		params <- as.list(environment()) %>% append(list(...))

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- "all", searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		listSkyObjects(module = "TimeTracking", objectName = "ConfigFiscalYear", schoolYearID = schoolYearID, searchFields = searchFields, page = page, pageSize = pageSize, SearchConditionsList = searchConditionsList, SearchConditionsGroupType = searchConditionsGroupType, SearchSortFieldNamesList = searchSortFieldNamesList, SearchSortFieldNamesDescendingList = searchSortFieldNamesDescendingList, entityId = entityID, flatten = flatten)
	}


	listTimeTrackingTempAccountDistributions <- function(searchConditionsList = NULL, TempAccountDistributionID = F, TempTimesheetID = F, AccountID = F, DistributionPercent = F, StateConcordDepartmentTNID = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, searchConditionsGroupType = "And", searchSortFieldNamesList = NULL, searchSortFieldNamesDescendingList = NULL, entityID = 1, schoolYearID = NULL, page = 1, pageSize = 100000, flatten = T, ...){

		params <- as.list(environment()) %>% append(list(...))

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- "all", searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		listSkyObjects(module = "TimeTracking", objectName = "TempAccountDistribution", schoolYearID = schoolYearID, searchFields = searchFields, page = page, pageSize = pageSize, SearchConditionsList = searchConditionsList, SearchConditionsGroupType = searchConditionsGroupType, SearchSortFieldNamesList = searchSortFieldNamesList, SearchSortFieldNamesDescendingList = searchSortFieldNamesDescendingList, entityId = entityID, flatten = flatten)
	}


	listTimeTrackingTempTimesheets <- function(searchConditionsList = NULL, TempTimesheetID = F, Factor = F, Rate = F, HoursWorked = F, WorkStartDate = F, WorkEndDate = F, AssignmentPayTypeID = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, TempAssignmentPayTypeTimesheetWeekID = F, GroupOnPayCheck = F, CommentOnPayCheck = F, searchConditionsGroupType = "And", searchSortFieldNamesList = NULL, searchSortFieldNamesDescendingList = NULL, entityID = 1, schoolYearID = NULL, page = 1, pageSize = 100000, flatten = T, ...){

		params <- as.list(environment()) %>% append(list(...))

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- "all", searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		listSkyObjects(module = "TimeTracking", objectName = "TempTimesheet", schoolYearID = schoolYearID, searchFields = searchFields, page = page, pageSize = pageSize, SearchConditionsList = searchConditionsList, SearchConditionsGroupType = searchConditionsGroupType, SearchSortFieldNamesList = searchSortFieldNamesList, SearchSortFieldNamesDescendingList = searchSortFieldNamesDescendingList, entityId = entityID, flatten = flatten)
	}


	listTimesheetApprovalTasks <- function(searchConditionsList = NULL, TimesheetApprovalTaskID = F, DistrictID = F, IsConditional = F, Level = F, Description = F, FilterData = F, StandardFilterCollectionData = F, UseOrganizationChart = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, searchConditionsGroupType = "And", searchSortFieldNamesList = NULL, searchSortFieldNamesDescendingList = NULL, entityID = 1, schoolYearID = NULL, page = 1, pageSize = 100000, flatten = T, ...){

		params <- as.list(environment()) %>% append(list(...))

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- "all", searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		listSkyObjects(module = "TimeTracking", objectName = "TimesheetApprovalTask", schoolYearID = schoolYearID, searchFields = searchFields, page = page, pageSize = pageSize, SearchConditionsList = searchConditionsList, SearchConditionsGroupType = searchConditionsGroupType, SearchSortFieldNamesList = searchSortFieldNamesList, SearchSortFieldNamesDescendingList = searchSortFieldNamesDescendingList, entityId = entityID, flatten = flatten)
	}


	listTimesheetApprovalTaskSecurityGroups <- function(searchConditionsList = NULL, TimesheetApprovalTaskSecurityGroupID = F, TimesheetApprovalTaskID = F, GroupIDSecurity = F, IsAlternate = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, searchConditionsGroupType = "And", searchSortFieldNamesList = NULL, searchSortFieldNamesDescendingList = NULL, entityID = 1, schoolYearID = NULL, page = 1, pageSize = 100000, flatten = T, ...){

		params <- as.list(environment()) %>% append(list(...))

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- "all", searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		listSkyObjects(module = "TimeTracking", objectName = "TimesheetApprovalTaskSecurityGroup", schoolYearID = schoolYearID, searchFields = searchFields, page = page, pageSize = pageSize, SearchConditionsList = searchConditionsList, SearchConditionsGroupType = searchConditionsGroupType, SearchSortFieldNamesList = searchSortFieldNamesList, SearchSortFieldNamesDescendingList = searchSortFieldNamesDescendingList, entityId = entityID, flatten = flatten)
	}


	listTimeTrackingTempExceptions <- function(searchConditionsList = NULL, TempExceptionID = F, Message = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, searchConditionsGroupType = "And", searchSortFieldNamesList = NULL, searchSortFieldNamesDescendingList = NULL, entityID = 1, schoolYearID = NULL, page = 1, pageSize = 100000, flatten = T, ...){

		params <- as.list(environment()) %>% append(list(...))

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- "all", searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		listSkyObjects(module = "TimeTracking", objectName = "TempException", schoolYearID = schoolYearID, searchFields = searchFields, page = page, pageSize = pageSize, SearchConditionsList = searchConditionsList, SearchConditionsGroupType = searchConditionsGroupType, SearchSortFieldNamesList = searchSortFieldNamesList, SearchSortFieldNamesDescendingList = searchSortFieldNamesDescendingList, entityId = entityID, flatten = flatten)
	}


	listTimeTrackingConfigDistricts <- function(searchConditionsList = NULL, ConfigDistrictID = F, DistrictID = F, SendApprovedMessage = F, TimesheetSubmissionApprovedMessageSubject = F, TimesheetSubmissionApprovedMessageContent = F, SendDeniedMessage = F, TimesheetSubmissionDeniedMessageSubject = F, TimesheetSubmissionDeniedMessageContent = F, SendWaitingMessage = F, TimesheetSubmissionWaitingMessageSubject = F, TimesheetSubmissionWaitingMessageContent = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, searchConditionsGroupType = "And", searchSortFieldNamesList = NULL, searchSortFieldNamesDescendingList = NULL, entityID = 1, schoolYearID = NULL, page = 1, pageSize = 100000, flatten = T, ...){

		params <- as.list(environment()) %>% append(list(...))

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- "all", searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		listSkyObjects(module = "TimeTracking", objectName = "ConfigDistrict", schoolYearID = schoolYearID, searchFields = searchFields, page = page, pageSize = pageSize, SearchConditionsList = searchConditionsList, SearchConditionsGroupType = searchConditionsGroupType, SearchSortFieldNamesList = searchSortFieldNamesList, SearchSortFieldNamesDescendingList = searchSortFieldNamesDescendingList, entityId = entityID, flatten = flatten)
	}


	listAssignmentTimesheetWeeks <- function(searchConditionsList = NULL, AssignmentTimesheetWeekID = F, AssignmentID = F, TimesheetWeekID = F, PayScheduleDetailID = F, StartDate = F, EndDate = F, TotalIn = F, TotalWorkOutOfOffice = F, TotalWork = F, TotalBreak = F, TotalLunch = F, TotalPaid = F, TotalPaidEligibleForOvertime = F, TotalOvertime = F, TotalUnpaidBreak = F, TotalUnroundedPaid = F, TotalHoliday = F, AttachmentCount = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, TotalTimeOff = F, TotalInSeconds = F, TotalWorkOutOfOfficeSeconds = F, TotalWorkSeconds = F, TotalBreakSeconds = F, TotalLunchSeconds = F, TotalPaidSeconds = F, TotalPaidEligibleForOvertimeSeconds = F, TotalOvertimeSeconds = F, TotalUnpaidBreakSeconds = F, TotalUnroundedPaidSeconds = F, TotalHolidaySeconds = F, TotalTimeOffSeconds = F, TimesheetSubmissionID = F, HasNonTimeTransactionSource = F, TotalSalaryPaid = F, TotalSalaryPaidSeconds = F, TotalSalaryOverage = F, TotalSalaryOverageSeconds = F, TotalSalaryUnderage = F, TotalSalaryUnderageSeconds = F, searchConditionsGroupType = "And", searchSortFieldNamesList = NULL, searchSortFieldNamesDescendingList = NULL, entityID = 1, schoolYearID = NULL, page = 1, pageSize = 100000, flatten = T, ...){

		params <- as.list(environment()) %>% append(list(...))

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- "all", searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		listSkyObjects(module = "TimeTracking", objectName = "AssignmentTimesheetWeek", schoolYearID = schoolYearID, searchFields = searchFields, page = page, pageSize = pageSize, SearchConditionsList = searchConditionsList, SearchConditionsGroupType = searchConditionsGroupType, SearchSortFieldNamesList = searchSortFieldNamesList, SearchSortFieldNamesDescendingList = searchSortFieldNamesDescendingList, entityId = entityID, flatten = flatten)
	}


	listAssignmentTimeTrackingGroupDetailPayTypes <- function(searchConditionsList = NULL, AssignmentTimeTrackingGroupDetailPayTypeID = F, PayTypeID = F, AssignmentTimeTrackingGroupDetailID = F, AllowTimeTracking = F, IncludeInOvertimeCalculation = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, AllowTimeTrackingOutsideAssignment = F, searchConditionsGroupType = "And", searchSortFieldNamesList = NULL, searchSortFieldNamesDescendingList = NULL, entityID = 1, schoolYearID = NULL, page = 1, pageSize = 100000, flatten = T, ...){

		params <- as.list(environment()) %>% append(list(...))

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- "all", searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		listSkyObjects(module = "TimeTracking", objectName = "AssignmentTimeTrackingGroupDetailPayType", schoolYearID = schoolYearID, searchFields = searchFields, page = page, pageSize = pageSize, SearchConditionsList = searchConditionsList, SearchConditionsGroupType = searchConditionsGroupType, SearchSortFieldNamesList = searchSortFieldNamesList, SearchSortFieldNamesDescendingList = searchSortFieldNamesDescendingList, entityId = entityID, flatten = flatten)
	}


	listTimesheetWeekDays <- function(searchConditionsList = NULL, TimesheetWeekDayID = F, TimesheetWeekID = F, Date = F, TotalIn = F, TotalBreak = F, TotalLunch = F, TotalWorkOutOfOffice = F, TotalWork = F, TimeIn = F, TimeOut = F, AttachmentCount = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, TotalInSeconds = F, TotalBreakSeconds = F, TotalLunchSeconds = F, TotalWorkOutOfOfficeSeconds = F, TotalWorkSeconds = F, TotalPaid = F, TotalUnpaidBreak = F, TotalUnroundedPaid = F, TotalPaidEligibleForOvertime = F, TotalOvertime = F, TotalHoliday = F, TotalTimeOff = F, ManuallyEdited = F, TotalDailyOvertime = F, TotalWeeklyOvertime = F, TotalSalaryPaid = F, TotalSalaryOverage = F, TotalSalaryUnderage = F, searchConditionsGroupType = "And", searchSortFieldNamesList = NULL, searchSortFieldNamesDescendingList = NULL, entityID = 1, schoolYearID = NULL, page = 1, pageSize = 100000, flatten = T, ...){

		params <- as.list(environment()) %>% append(list(...))

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- "all", searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		listSkyObjects(module = "TimeTracking", objectName = "TimesheetWeekDay", schoolYearID = schoolYearID, searchFields = searchFields, page = page, pageSize = pageSize, SearchConditionsList = searchConditionsList, SearchConditionsGroupType = searchConditionsGroupType, SearchSortFieldNamesList = searchSortFieldNamesList, SearchSortFieldNamesDescendingList = searchSortFieldNamesDescendingList, entityId = entityID, flatten = flatten)
	}


	listTimeTransactions <- function(searchConditionsList = NULL, TimeTransactionID = F, StartDateTime = F, StartDateTimeEntered = F, EndDateTime = F, EndDateTimeEntered = F, Status = F, IPAddressStart = F, IPAddressEnd = F, Comment = F, TimesheetWeekDayID = F, AssignmentPayTypeID = F, StartDate = F, StartTime = F, StartTimeFormatted = F, EndDate = F, EndTimeFormatted = F, EndTime = F, Duration = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, DurationInSeconds = F, AssignmentTimesheetWeekID = F, ManuallyEdited = F, TimeTransactionAccountDistributionString = F, BuildingID = F, searchConditionsGroupType = "And", searchSortFieldNamesList = NULL, searchSortFieldNamesDescendingList = NULL, entityID = 1, schoolYearID = NULL, page = 1, pageSize = 100000, flatten = T, ...){

		params <- as.list(environment()) %>% append(list(...))

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- "all", searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		listSkyObjects(module = "TimeTracking", objectName = "TimeTransaction", schoolYearID = schoolYearID, searchFields = searchFields, page = page, pageSize = pageSize, SearchConditionsList = searchConditionsList, SearchConditionsGroupType = searchConditionsGroupType, SearchSortFieldNamesList = searchSortFieldNamesList, SearchSortFieldNamesDescendingList = searchSortFieldNamesDescendingList, entityId = entityID, flatten = flatten)
	}


	listEmployeeTimeTrackingGroups <- function(searchConditionsList = NULL, EmployeeTimeTrackingGroupID = F, DistrictID = F, Code = F, Description = F, CodeDescription = F, WorkWeekStartDay = F, AttachmentCount = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, searchConditionsGroupType = "And", searchSortFieldNamesList = NULL, searchSortFieldNamesDescendingList = NULL, entityID = 1, schoolYearID = NULL, page = 1, pageSize = 100000, flatten = T, ...){

		params <- as.list(environment()) %>% append(list(...))

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- "all", searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		listSkyObjects(module = "TimeTracking", objectName = "EmployeeTimeTrackingGroup", schoolYearID = schoolYearID, searchFields = searchFields, page = page, pageSize = pageSize, SearchConditionsList = searchConditionsList, SearchConditionsGroupType = searchConditionsGroupType, SearchSortFieldNamesList = searchSortFieldNamesList, SearchSortFieldNamesDescendingList = searchSortFieldNamesDescendingList, entityId = entityID, flatten = flatten)
	}


	listEmployeeTimeTrackingGroupDetails <- function(searchConditionsList = NULL, EmployeeTimeTrackingGroupDetailID = F, EmployeeTimeTrackingGroupID = F, StartDate = F, EndDate = F, WeeklyOvertimeThreshold = F, DailyOvertimeThreshold = F, OvertimeMethod = F, PaidTimeRoundingType = F, NearestTimeIncrement = F, RoundingValue = F, OvertimeDistributionType = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, OvertimePaymentType = F, AllowEmployeeAccessEdits = F, searchConditionsGroupType = "And", searchSortFieldNamesList = NULL, searchSortFieldNamesDescendingList = NULL, entityID = 1, schoolYearID = NULL, page = 1, pageSize = 100000, flatten = T, ...){

		params <- as.list(environment()) %>% append(list(...))

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- "all", searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		listSkyObjects(module = "TimeTracking", objectName = "EmployeeTimeTrackingGroupDetail", schoolYearID = schoolYearID, searchFields = searchFields, page = page, pageSize = pageSize, SearchConditionsList = searchConditionsList, SearchConditionsGroupType = searchConditionsGroupType, SearchSortFieldNamesList = searchSortFieldNamesList, SearchSortFieldNamesDescendingList = searchSortFieldNamesDescendingList, entityId = entityID, flatten = flatten)
	}


	listTimesheetWeeks <- function(searchConditionsList = NULL, TimesheetWeekID = F, DistrictID = F, EmployeeID = F, StartDate = F, EndDate = F, TotalIn = F, TotalWorkOutOfOffice = F, TotalWork = F, TotalBreak = F, TotalLunch = F, AttachmentCount = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, TotalInSeconds = F, TotalWorkOutOfOfficeSeconds = F, TotalWorkSeconds = F, TotalBreakSeconds = F, TotalLunchSeconds = F, RenderMoveToHistoryWithoutPayroll = F, RenderRestoreToUnsubmitted = F, TotalPaid = F, TotalUnpaidBreak = F, TotalUnroundedPaid = F, TotalPaidEligibleForOvertime = F, TotalOvertime = F, TotalHoliday = F, TotalTimeOff = F, HasUnsubmitted = F, HasWaiting = F, HasApproved = F, ManuallyEdited = F, TotalOvertimeWorked = F, TotalWorkHours = F, TotalSalaryPaid = F, TotalSalaryOverage = F, TotalSalaryUnderage = F, RenderSalary = F, searchConditionsGroupType = "And", searchSortFieldNamesList = NULL, searchSortFieldNamesDescendingList = NULL, entityID = 1, schoolYearID = NULL, page = 1, pageSize = 100000, flatten = T, ...){

		params <- as.list(environment()) %>% append(list(...))

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- "all", searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		listSkyObjects(module = "TimeTracking", objectName = "TimesheetWeek", schoolYearID = schoolYearID, searchFields = searchFields, page = page, pageSize = pageSize, SearchConditionsList = searchConditionsList, SearchConditionsGroupType = searchConditionsGroupType, SearchSortFieldNamesList = searchSortFieldNamesList, SearchSortFieldNamesDescendingList = searchSortFieldNamesDescendingList, entityId = entityID, flatten = flatten)
	}


	listAssignmentTimeTrackingGroups <- function(searchConditionsList = NULL, AssignmentTimeTrackingGroupID = F, DistrictID = F, Code = F, FiscalYearID = F, Description = F, CodeDescription = F, AssignmentTimeTrackingGroupIDClonedFrom = F, AttachmentCount = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, searchConditionsGroupType = "And", searchSortFieldNamesList = NULL, searchSortFieldNamesDescendingList = NULL, entityID = 1, schoolYearID = NULL, page = 1, pageSize = 100000, flatten = T, ...){

		params <- as.list(environment()) %>% append(list(...))

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- "all", searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		listSkyObjects(module = "TimeTracking", objectName = "AssignmentTimeTrackingGroup", schoolYearID = schoolYearID, searchFields = searchFields, page = page, pageSize = pageSize, SearchConditionsList = searchConditionsList, SearchConditionsGroupType = searchConditionsGroupType, SearchSortFieldNamesList = searchSortFieldNamesList, SearchSortFieldNamesDescendingList = searchSortFieldNamesDescendingList, entityId = entityID, flatten = flatten)
	}


	listAssignmentTimeTrackingGroupDetails <- function(searchConditionsList = NULL, AssignmentTimeTrackingGroupDetailID = F, AssignmentTimeTrackingGroupID = F, PayScheduleID = F, StartDate = F, EndDate = F, AllowBreak = F, AllowLunch = F, AllowWorkOutOfOffice = F, OvertimeFactor = F, PayTypeIDOvertime = F, UnpaidBreakThresholdPercent = F, PayTypeIDHoliday = F, HolidayOvertime = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, AllowOverrideTimeTransactionAccountDistributions = F, SelectBuildingOnTimeTransactionType = F, PaySalaryOverages = F, PayTypeIDOverage = F, OverageThreshold = F, DockSalaryUnderages = F, PayTypeIDUnderage = F, UnderageThreshold = F, UseOverageAssignmentPayTypeRate = F, UseUnderageAssignmentPayTypeRate = F, UseOvertimeAssignmentPayTypeRate = F, searchConditionsGroupType = "And", searchSortFieldNamesList = NULL, searchSortFieldNamesDescendingList = NULL, entityID = 1, schoolYearID = NULL, page = 1, pageSize = 100000, flatten = T, ...){

		params <- as.list(environment()) %>% append(list(...))

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- "all", searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		listSkyObjects(module = "TimeTracking", objectName = "AssignmentTimeTrackingGroupDetail", schoolYearID = schoolYearID, searchFields = searchFields, page = page, pageSize = pageSize, SearchConditionsList = searchConditionsList, SearchConditionsGroupType = searchConditionsGroupType, SearchSortFieldNamesList = searchSortFieldNamesList, SearchSortFieldNamesDescendingList = searchSortFieldNamesDescendingList, entityId = entityID, flatten = flatten)
	}


	listAssignmentTimeTrackingGroupDetailTimeOffTypes <- function(searchConditionsList = NULL, AssignmentTimeTrackingGroupDetailTimeOffTypeID = F, AssignmentTimeTrackingGroupDetailID = F, TimeOffTypeID = F, PayTypeID = F, IncludeInOvertimeCalculation = F, IsPaidTime = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, searchConditionsGroupType = "And", searchSortFieldNamesList = NULL, searchSortFieldNamesDescendingList = NULL, entityID = 1, schoolYearID = NULL, page = 1, pageSize = 100000, flatten = T, ...){

		params <- as.list(environment()) %>% append(list(...))

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- "all", searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		listSkyObjects(module = "TimeTracking", objectName = "AssignmentTimeTrackingGroupDetailTimeOffType", schoolYearID = schoolYearID, searchFields = searchFields, page = page, pageSize = pageSize, SearchConditionsList = searchConditionsList, SearchConditionsGroupType = searchConditionsGroupType, SearchSortFieldNamesList = searchSortFieldNamesList, SearchSortFieldNamesDescendingList = searchSortFieldNamesDescendingList, entityId = entityID, flatten = flatten)
	}


	listTempTimeTransactions <- function(searchConditionsList = NULL, TempTimeTransactionID = F, StartDateTime = F, EndDateTime = F, Status = F, TimesheetWeekDayID = F, AssignmentPayTypeID = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, searchConditionsGroupType = "And", searchSortFieldNamesList = NULL, searchSortFieldNamesDescendingList = NULL, entityID = 1, schoolYearID = NULL, page = 1, pageSize = 100000, flatten = T, ...){

		params <- as.list(environment()) %>% append(list(...))

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- "all", searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		listSkyObjects(module = "TimeTracking", objectName = "TempTimeTransaction", schoolYearID = schoolYearID, searchFields = searchFields, page = page, pageSize = pageSize, SearchConditionsList = searchConditionsList, SearchConditionsGroupType = searchConditionsGroupType, SearchSortFieldNamesList = searchSortFieldNamesList, SearchSortFieldNamesDescendingList = searchSortFieldNamesDescendingList, entityId = entityID, flatten = flatten)
	}


	listTempMassCalculateExceptions <- function(searchConditionsList = NULL, TempMassCalculateExceptionID = F, EmployeeNameLFM = F, StartDate = F, EndDate = F, Message = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, EmployeeNumber = F, searchConditionsGroupType = "And", searchSortFieldNamesList = NULL, searchSortFieldNamesDescendingList = NULL, entityID = 1, schoolYearID = NULL, page = 1, pageSize = 100000, flatten = T, ...){

		params <- as.list(environment()) %>% append(list(...))

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- "all", searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		listSkyObjects(module = "TimeTracking", objectName = "TempMassCalculateException", schoolYearID = schoolYearID, searchFields = searchFields, page = page, pageSize = pageSize, SearchConditionsList = searchConditionsList, SearchConditionsGroupType = searchConditionsGroupType, SearchSortFieldNamesList = searchSortFieldNamesList, SearchSortFieldNamesDescendingList = searchSortFieldNamesDescendingList, entityId = entityID, flatten = flatten)
	}


	listTempTimesheetSubmissions <- function(searchConditionsList = NULL, TempTimesheetSubmissionID = F, TimesheetSubmissionID = F, EmployeeName = F, StartDate = F, EndDate = F, TotalInSeconds = F, TotalWorkOutOfOfficeSeconds = F, TotalWorkSeconds = F, TotalBreakSeconds = F, TotalLunchSeconds = F, TotalPaidSeconds = F, TotalPaidEligibleForOvertimeSeconds = F, TotalOvertimeSeconds = F, TotalUnpaidBreakSeconds = F, TotalUnroundedPaidSeconds = F, TotalHolidaySeconds = F, TotalTimeOffSeconds = F, LatestApprover = F, LatestApprovalLevelDescription = F, HasErrors = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, TotalSalaryPaidSeconds = F, TotalSalaryOverageSeconds = F, TotalSalaryUnderageSeconds = F, HasSupervisors = F, searchConditionsGroupType = "And", searchSortFieldNamesList = NULL, searchSortFieldNamesDescendingList = NULL, entityID = 1, schoolYearID = NULL, page = 1, pageSize = 100000, flatten = T, ...){

		params <- as.list(environment()) %>% append(list(...))

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- "all", searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		listSkyObjects(module = "TimeTracking", objectName = "TempTimesheetSubmission", schoolYearID = schoolYearID, searchFields = searchFields, page = page, pageSize = pageSize, SearchConditionsList = searchConditionsList, SearchConditionsGroupType = searchConditionsGroupType, SearchSortFieldNamesList = searchSortFieldNamesList, SearchSortFieldNamesDescendingList = searchSortFieldNamesDescendingList, entityId = entityID, flatten = flatten)
	}


	listTempTimesheetSubmissionErrors <- function(searchConditionsList = NULL, TempTimesheetSubmissionErrorID = F, TempTimesheetSubmissionID = F, Error = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, searchConditionsGroupType = "And", searchSortFieldNamesList = NULL, searchSortFieldNamesDescendingList = NULL, entityID = 1, schoolYearID = NULL, page = 1, pageSize = 100000, flatten = T, ...){

		params <- as.list(environment()) %>% append(list(...))

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- "all", searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		listSkyObjects(module = "TimeTracking", objectName = "TempTimesheetSubmissionError", schoolYearID = schoolYearID, searchFields = searchFields, page = page, pageSize = pageSize, SearchConditionsList = searchConditionsList, SearchConditionsGroupType = searchConditionsGroupType, SearchSortFieldNamesList = searchSortFieldNamesList, SearchSortFieldNamesDescendingList = searchSortFieldNamesDescendingList, entityId = entityID, flatten = flatten)
	}


	listTimesheetSubmissions <- function(searchConditionsList = NULL, TimesheetSubmissionID = F, Status = F, StartDate = F, EndDate = F, TimesheetWeekID = F, AllowMoveToHistoryWithoutPayroll = F, AllowRestoreToUnsubmitted = F, TotalInSeconds = F, TotalWorkOutOfOfficeSeconds = F, TotalWorkSeconds = F, TotalBreakSeconds = F, TotalLunchSeconds = F, TotalPaidSeconds = F, TotalPaidEligibleForOvertimeSeconds = F, TotalOvertimeSeconds = F, TotalUnpaidBreakSeconds = F, TotalUnroundedPaidSeconds = F, TotalHolidaySeconds = F, TotalTimeOffSeconds = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, TotalSalaryPaidSeconds = F, TotalSalaryOverageSeconds = F, TotalSalaryUnderageSeconds = F, RenderSalaryTotals = F, searchConditionsGroupType = "And", searchSortFieldNamesList = NULL, searchSortFieldNamesDescendingList = NULL, entityID = 1, schoolYearID = NULL, page = 1, pageSize = 100000, flatten = T, ...){

		params <- as.list(environment()) %>% append(list(...))

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- "all", searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		listSkyObjects(module = "TimeTracking", objectName = "TimesheetSubmission", schoolYearID = schoolYearID, searchFields = searchFields, page = page, pageSize = pageSize, SearchConditionsList = searchConditionsList, SearchConditionsGroupType = searchConditionsGroupType, SearchSortFieldNamesList = searchSortFieldNamesList, SearchSortFieldNamesDescendingList = searchSortFieldNamesDescendingList, entityId = entityID, flatten = flatten)
	}


	listTimesheetSubmissionApprovals <- function(searchConditionsList = NULL, TimesheetSubmissionApprovalID = F, TimesheetSubmissionID = F, Status = F, Comment = F, UserIDApprover = F, OrganizationChartRelationshipID = F, Level = F, LevelDescription = F, ApprovalActionTime = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, searchConditionsGroupType = "And", searchSortFieldNamesList = NULL, searchSortFieldNamesDescendingList = NULL, entityID = 1, schoolYearID = NULL, page = 1, pageSize = 100000, flatten = T, ...){

		params <- as.list(environment()) %>% append(list(...))

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- "all", searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		listSkyObjects(module = "TimeTracking", objectName = "TimesheetSubmissionApproval", schoolYearID = schoolYearID, searchFields = searchFields, page = page, pageSize = pageSize, SearchConditionsList = searchConditionsList, SearchConditionsGroupType = searchConditionsGroupType, SearchSortFieldNamesList = searchSortFieldNamesList, SearchSortFieldNamesDescendingList = searchSortFieldNamesDescendingList, entityId = entityID, flatten = flatten)
	}


	listAssignmentPayTypeTimesheetWeeks <- function(searchConditionsList = NULL, AssignmentPayTypeTimesheetWeekID = F, AssignmentPayTypeID = F, TimesheetWeekID = F, AssignmentTimesheetWeekID = F, StartDate = F, EndDate = F, TotalPaid = F, TotalUnpaidBreak = F, TotalUnroundedPaid = F, TotalPaidEligibleForOvertime = F, TotalOvertime = F, TotalHoliday = F, TotalTimeOff = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, TotalDailyOvertime = F, TotalWeeklyOvertime = F, TotalSalaryPaid = F, TotalSalaryOverage = F, TotalSalaryUnderage = F, searchConditionsGroupType = "And", searchSortFieldNamesList = NULL, searchSortFieldNamesDescendingList = NULL, entityID = 1, schoolYearID = NULL, page = 1, pageSize = 100000, flatten = T, ...){

		params <- as.list(environment()) %>% append(list(...))

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- "all", searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		listSkyObjects(module = "TimeTracking", objectName = "AssignmentPayTypeTimesheetWeek", schoolYearID = schoolYearID, searchFields = searchFields, page = page, pageSize = pageSize, SearchConditionsList = searchConditionsList, SearchConditionsGroupType = searchConditionsGroupType, SearchSortFieldNamesList = searchSortFieldNamesList, SearchSortFieldNamesDescendingList = searchSortFieldNamesDescendingList, entityId = entityID, flatten = flatten)
	}


	listAssignmentPayTypeTimesheetWeekDays <- function(searchConditionsList = NULL, AssignmentPayTypeTimesheetWeekDayID = F, AssignmentPayTypeTimesheetWeekID = F, TimesheetWeekDayID = F, Date = F, Rate = F, Paid = F, UnpaidBreak = F, UnroundedPaid = F, PaidEligibleForOvertime = F, DailyOvertime = F, WeeklyOvertime = F, Holiday = F, TimeOff = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, OvertimePaid = F, SalaryPaid = F, SalaryOverage = F, SalaryUnderage = F, OvertimeRateOverride = F, OverageRateOverride = F, UnderageRateOverride = F, searchConditionsGroupType = "And", searchSortFieldNamesList = NULL, searchSortFieldNamesDescendingList = NULL, entityID = 1, schoolYearID = NULL, page = 1, pageSize = 100000, flatten = T, ...){

		params <- as.list(environment()) %>% append(list(...))

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- "all", searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		listSkyObjects(module = "TimeTracking", objectName = "AssignmentPayTypeTimesheetWeekDay", schoolYearID = schoolYearID, searchFields = searchFields, page = page, pageSize = pageSize, SearchConditionsList = searchConditionsList, SearchConditionsGroupType = searchConditionsGroupType, SearchSortFieldNamesList = searchSortFieldNamesList, SearchSortFieldNamesDescendingList = searchSortFieldNamesDescendingList, entityId = entityID, flatten = flatten)
	}


	listTempAssignmentPayTypeTimesheetWeeks <- function(searchConditionsList = NULL, TempAssignmentPayTypeTimesheetWeekID = F, AssignmentPayTypeID = F, TimesheetWeekID = F, AssignmentTimesheetWeekID = F, APTIdentifier = F, StartDate = F, EndDate = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, searchConditionsGroupType = "And", searchSortFieldNamesList = NULL, searchSortFieldNamesDescendingList = NULL, entityID = 1, schoolYearID = NULL, page = 1, pageSize = 100000, flatten = T, ...){

		params <- as.list(environment()) %>% append(list(...))

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- "all", searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		listSkyObjects(module = "TimeTracking", objectName = "TempAssignmentPayTypeTimesheetWeek", schoolYearID = schoolYearID, searchFields = searchFields, page = page, pageSize = pageSize, SearchConditionsList = searchConditionsList, SearchConditionsGroupType = searchConditionsGroupType, SearchSortFieldNamesList = searchSortFieldNamesList, SearchSortFieldNamesDescendingList = searchSortFieldNamesDescendingList, entityId = entityID, flatten = flatten)
	}


	listTempAssignmentPayTypeTimesheetWeekDays <- function(searchConditionsList = NULL, TempAssignmentPayTypeTimesheetWeekDayID = F, TempAssignmentPayTypeTimesheetWeekID = F, APTIdentifier = F, TimesheetWeekDayID = F, Date = F, Rate = F, Paid = F, UnpaidBreak = F, UnroundedPaid = F, PaidEligibleForOvertime = F, DailyOvertime = F, WeeklyOvertime = F, Holiday = F, TimeOff = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, OvertimePaid = F, SalaryPaid = F, SalaryOverage = F, SalaryUnderage = F, UseOverageAssignmentPayTypeRate = F, UseUnderageAssignmentPayTypeRate = F, UseOvertimeAssignmentPayTypeRate = F, OverageRateOverride = F, UnderageRateOverride = F, OvertimeRateOverride = F, searchConditionsGroupType = "And", searchSortFieldNamesList = NULL, searchSortFieldNamesDescendingList = NULL, entityID = 1, schoolYearID = NULL, page = 1, pageSize = 100000, flatten = T, ...){

		params <- as.list(environment()) %>% append(list(...))

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- "all", searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		listSkyObjects(module = "TimeTracking", objectName = "TempAssignmentPayTypeTimesheetWeekDay", schoolYearID = schoolYearID, searchFields = searchFields, page = page, pageSize = pageSize, SearchConditionsList = searchConditionsList, SearchConditionsGroupType = searchConditionsGroupType, SearchSortFieldNamesList = searchSortFieldNamesList, SearchSortFieldNamesDescendingList = searchSortFieldNamesDescendingList, entityId = entityID, flatten = flatten)
	}


	listTempTimesheetWeeks <- function(searchConditionsList = NULL, TempTimesheetWeekID = F, TimesheetWeekID = F, EmployeeID = F, EmployeeFullNameLFM = F, EmployeeNumber = F, StartDate = F, EndDate = F, OldPaid = F, FormattedOldPaid = F, HasUnsubmitted = F, HasWaiting = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, PositionTypeCodeDescription = F, AssignmentTypeCodes = F, BuildingCodes = F, HasApproved = F, searchConditionsGroupType = "And", searchSortFieldNamesList = NULL, searchSortFieldNamesDescendingList = NULL, entityID = 1, schoolYearID = NULL, page = 1, pageSize = 100000, flatten = T, ...){

		params <- as.list(environment()) %>% append(list(...))

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- "all", searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		listSkyObjects(module = "TimeTracking", objectName = "TempTimesheetWeek", schoolYearID = schoolYearID, searchFields = searchFields, page = page, pageSize = pageSize, SearchConditionsList = searchConditionsList, SearchConditionsGroupType = searchConditionsGroupType, SearchSortFieldNamesList = searchSortFieldNamesList, SearchSortFieldNamesDescendingList = searchSortFieldNamesDescendingList, entityId = entityID, flatten = flatten)
	}


	listEmployeeTimeTrackingGroupDetailIPRanges <- function(searchConditionsList = NULL, EmployeeTimeTrackingGroupDetailIPRangeID = F, EmployeeTimeTrackingGroupDetailID = F, IPRangeID = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, searchConditionsGroupType = "And", searchSortFieldNamesList = NULL, searchSortFieldNamesDescendingList = NULL, entityID = 1, schoolYearID = NULL, page = 1, pageSize = 100000, flatten = T, ...){

		params <- as.list(environment()) %>% append(list(...))

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- "all", searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		listSkyObjects(module = "TimeTracking", objectName = "EmployeeTimeTrackingGroupDetailIPRange", schoolYearID = schoolYearID, searchFields = searchFields, page = page, pageSize = pageSize, SearchConditionsList = searchConditionsList, SearchConditionsGroupType = searchConditionsGroupType, SearchSortFieldNamesList = searchSortFieldNamesList, SearchSortFieldNamesDescendingList = searchSortFieldNamesDescendingList, entityId = entityID, flatten = flatten)
	}


	listTimeTransactionAccountDistributionOverrides <- function(searchConditionsList = NULL, TimeTransactionAccountDistributionOverrideID = F, TimeTransactionID = F, AccountID = F, Percent = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, searchConditionsGroupType = "And", searchSortFieldNamesList = NULL, searchSortFieldNamesDescendingList = NULL, entityID = 1, schoolYearID = NULL, page = 1, pageSize = 100000, flatten = T, ...){

		params <- as.list(environment()) %>% append(list(...))

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- "all", searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		listSkyObjects(module = "TimeTracking", objectName = "TimeTransactionAccountDistributionOverride", schoolYearID = schoolYearID, searchFields = searchFields, page = page, pageSize = pageSize, SearchConditionsList = searchConditionsList, SearchConditionsGroupType = searchConditionsGroupType, SearchSortFieldNamesList = searchSortFieldNamesList, SearchSortFieldNamesDescendingList = searchSortFieldNamesDescendingList, entityId = entityID, flatten = flatten)
	}
