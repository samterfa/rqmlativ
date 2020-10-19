

	listBudgetPublicationDetailV1IncludedAccounts <- function(searchConditionsList = NULL, BudgetPublicationDetailV1IncludedAccountID = F, BudgetPublicationDetailV1ID = F, AccountID = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, Amount = F, searchConditionsGroupType = "And", searchSortFieldNamesList = NULL, searchSortFieldNamesDescendingList = NULL, entityID = 1, schoolYearID = NULL, page = 1, pageSize = 100000, flatten = T, ...){

		params <- as.list(environment()) %>% append(list(...))

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- "all", searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		listSkyObjects(module = "StateReportingMN", objectName = "BudgetPublicationDetailV1IncludedAccount", schoolYearID = schoolYearID, searchFields = searchFields, page = page, pageSize = pageSize, SearchConditionsList = searchConditionsList, SearchConditionsGroupType = searchConditionsGroupType, SearchSortFieldNamesList = searchSortFieldNamesList, SearchSortFieldNamesDescendingList = searchSortFieldNamesDescendingList, entityId = entityID, flatten = flatten)
	}


	listStateReportingMNConfigFiscalYears <- function(searchConditionsList = NULL, ConfigFiscalYearID = F, FiscalYearID = F, GeneralFundRestrictedFilter = F, GeneralFundOtherFilter = F, FoodServiceFundFilter = F, CommunityServiceFundFilter = F, BuildingConstructionFundFilter = F, DebtServiceFundFilter = F, TrustFundFilter = F, InternalServiceFundFilter = F, OPEBRevocableTrustFundFilter = F, OPEBIrrevocableTrustFundFilter = F, OPEBDebtServiceFundFilter = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, GeneralFundRestrictedStandardFilterData = F, GeneralFundOtherStandardFilterData = F, FoodServiceFundStandardFilterData = F, CommunityServiceFundStandardFilterData = F, BuildingConstructionFundStandardFilterData = F, DebtServiceFundStandardFilterData = F, TrustFundStandardFilterData = F, InternalServiceFundStandardFilterData = F, OPEBRevocableTrustFundStandardFilterData = F, OPEBIrrevocableTrustFundStandardFilterData = F, OPEBDebtServiceFundStandardFilterData = F, searchConditionsGroupType = "And", searchSortFieldNamesList = NULL, searchSortFieldNamesDescendingList = NULL, entityID = 1, schoolYearID = NULL, page = 1, pageSize = 100000, flatten = T, ...){

		params <- as.list(environment()) %>% append(list(...))

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- "all", searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		listSkyObjects(module = "StateReportingMN", objectName = "ConfigFiscalYear", schoolYearID = schoolYearID, searchFields = searchFields, page = page, pageSize = pageSize, SearchConditionsList = searchConditionsList, SearchConditionsGroupType = searchConditionsGroupType, SearchSortFieldNamesList = searchSortFieldNamesList, SearchSortFieldNamesDescendingList = searchSortFieldNamesDescendingList, entityId = entityID, flatten = flatten)
	}


	listBudgetPublicationExtractRuns <- function(searchConditionsList = NULL, BudgetPublicationExtractRunID = F, FiscalYearID = F, DistrictName = F, DistrictNumber = F, LongTermDebtAtStartOfPriorYear = F, LongTermDebtNewIssues = F, LongTermDebtRedeemedIssues = F, ShortTermCertificateOfIndebtedness = F, ShortTermOtherIndebtedness = F, GeneralFundDeficitInExcessOfTwoAndAHalfPercentOfExpendituresAtEndOfPriorYear = F, ADMTotalOperatingExpenditure = F, ADMTotalServedAndTuitionedOutAndAdjustedExtendedInPriorYear = F, GeneralFundRestrictedFilter = F, GeneralFundOtherFilter = F, FoodServiceFundFilter = F, CommunityServiceFundFilter = F, BuildingConstructionFundFilter = F, DebtServiceFundFilter = F, TrustFundFilter = F, InternalServiceFundFilter = F, OPEBRevocableTrustFundFilter = F, OPEBIrrevocableTrustFundFilter = F, OPEBDebtServiceFundFilter = F, Version = F, FatalExceptionCount = F, NonFatalExceptionCount = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, ExtractedAccountsCount = F, LatestExport = F, LatestSentOrAcceptedExportTime = F, searchConditionsGroupType = "And", searchSortFieldNamesList = NULL, searchSortFieldNamesDescendingList = NULL, entityID = 1, schoolYearID = NULL, page = 1, pageSize = 100000, flatten = T, ...){

		params <- as.list(environment()) %>% append(list(...))

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- "all", searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		listSkyObjects(module = "StateReportingMN", objectName = "BudgetPublicationExtractRun", schoolYearID = schoolYearID, searchFields = searchFields, page = page, pageSize = pageSize, SearchConditionsList = searchConditionsList, SearchConditionsGroupType = searchConditionsGroupType, SearchSortFieldNamesList = searchSortFieldNamesList, SearchSortFieldNamesDescendingList = searchSortFieldNamesDescendingList, entityId = entityID, flatten = flatten)
	}


	listBudgetPublicationDetailV1s <- function(searchConditionsList = NULL, BudgetPublicationDetailV1ID = F, BudgetPublicationExtractRunID = F, CellRow = F, CellColumn = F, Amount = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, ExtractedAmount = F, searchConditionsGroupType = "And", searchSortFieldNamesList = NULL, searchSortFieldNamesDescendingList = NULL, entityID = 1, schoolYearID = NULL, page = 1, pageSize = 100000, flatten = T, ...){

		params <- as.list(environment()) %>% append(list(...))

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- "all", searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		listSkyObjects(module = "StateReportingMN", objectName = "BudgetPublicationDetailV1", schoolYearID = schoolYearID, searchFields = searchFields, page = page, pageSize = pageSize, SearchConditionsList = searchConditionsList, SearchConditionsGroupType = searchConditionsGroupType, SearchSortFieldNamesList = searchSortFieldNamesList, SearchSortFieldNamesDescendingList = searchSortFieldNamesDescendingList, entityId = entityID, flatten = flatten)
	}


	listBudgetPublicationExceptions <- function(searchConditionsList = NULL, BudgetPublicationExceptionID = F, BudgetPublicationExtractRunID = F, AccountID = F, Amount = F, IsFatal = F, Type = F, Message = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, PreserveException = F, searchConditionsGroupType = "And", searchSortFieldNamesList = NULL, searchSortFieldNamesDescendingList = NULL, entityID = 1, schoolYearID = NULL, page = 1, pageSize = 100000, flatten = T, ...){

		params <- as.list(environment()) %>% append(list(...))

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- "all", searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		listSkyObjects(module = "StateReportingMN", objectName = "BudgetPublicationException", schoolYearID = schoolYearID, searchFields = searchFields, page = page, pageSize = pageSize, SearchConditionsList = searchConditionsList, SearchConditionsGroupType = searchConditionsGroupType, SearchSortFieldNamesList = searchSortFieldNamesList, SearchSortFieldNamesDescendingList = searchSortFieldNamesDescendingList, entityId = entityID, flatten = flatten)
	}


	listUFARSExceptions <- function(searchConditionsList = NULL, UFARSExceptionID = F, UFARSExtractRunID = F, IsFatal = F, Type = F, Message = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, AccountFiscalYearID = F, UFARSGeneralLedgerV1ID = F, UFARSGeneralLedgerV1IncludedAccountID = F, UFARSRevenueExpenseV1ID = F, ExtractedCrosswalkAccount = F, PreserveException = F, searchConditionsGroupType = "And", searchSortFieldNamesList = NULL, searchSortFieldNamesDescendingList = NULL, entityID = 1, schoolYearID = NULL, page = 1, pageSize = 100000, flatten = T, ...){

		params <- as.list(environment()) %>% append(list(...))

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- "all", searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		listSkyObjects(module = "StateReportingMN", objectName = "UFARSException", schoolYearID = schoolYearID, searchFields = searchFields, page = page, pageSize = pageSize, SearchConditionsList = searchConditionsList, SearchConditionsGroupType = searchConditionsGroupType, SearchSortFieldNamesList = searchSortFieldNamesList, SearchSortFieldNamesDescendingList = searchSortFieldNamesDescendingList, entityId = entityID, flatten = flatten)
	}


	listUFARSExports <- function(searchConditionsList = NULL, UFARSExportID = F, UFARSExtractRunID = F, MediaID = F, Status = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, searchConditionsGroupType = "And", searchSortFieldNamesList = NULL, searchSortFieldNamesDescendingList = NULL, entityID = 1, schoolYearID = NULL, page = 1, pageSize = 100000, flatten = T, ...){

		params <- as.list(environment()) %>% append(list(...))

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- "all", searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		listSkyObjects(module = "StateReportingMN", objectName = "UFARSExport", schoolYearID = schoolYearID, searchFields = searchFields, page = page, pageSize = pageSize, SearchConditionsList = searchConditionsList, SearchConditionsGroupType = searchConditionsGroupType, SearchSortFieldNamesList = searchSortFieldNamesList, SearchSortFieldNamesDescendingList = searchSortFieldNamesDescendingList, entityId = entityID, flatten = flatten)
	}


	listUFARSExtractRuns <- function(searchConditionsList = NULL, UFARSExtractRunID = F, FiscalYearID = F, IsAudited = F, RegionNumber = F, DistrictNumber = F, DistrictType = F, AccountingPeriod = F, TotalGNLRecords = F, TotalExpenseRecords = F, TotalRevenueRecords = F, Version = F, FatalExceptionCount = F, NonFatalExceptionCount = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, LatestExport = F, LatestSentOrAcceptedExportTime = F, GeneralLedgerFiscalYearEndingBalance = F, searchConditionsGroupType = "And", searchSortFieldNamesList = NULL, searchSortFieldNamesDescendingList = NULL, entityID = 1, schoolYearID = NULL, page = 1, pageSize = 100000, flatten = T, ...){

		params <- as.list(environment()) %>% append(list(...))

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- "all", searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		listSkyObjects(module = "StateReportingMN", objectName = "UFARSExtractRun", schoolYearID = schoolYearID, searchFields = searchFields, page = page, pageSize = pageSize, SearchConditionsList = searchConditionsList, SearchConditionsGroupType = searchConditionsGroupType, SearchSortFieldNamesList = searchSortFieldNamesList, SearchSortFieldNamesDescendingList = searchSortFieldNamesDescendingList, entityId = entityID, flatten = flatten)
	}


	listUFARSGeneralLedgerV1s <- function(searchConditionsList = NULL, UFARSGeneralLedgerV1ID = F, UFARSExtractRunID = F, DimensionValueFund = F, DimensionValueGNL = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, DimensionIDFund = F, DimensionIDGNL = F, FiscalYearBeginningBalance = F, FiscalYearDebits = F, FiscalYearCredits = F, FiscalYearEndingBalance = F, ExcludeFromExport = F, CrosswalkMNValueFund = F, CrosswalkMNValueGNL = F, StateCrosswalkIDFund = F, StateCrosswalkIDGNL = F, ExtractedCrosswalkAccount = F, searchConditionsGroupType = "And", searchSortFieldNamesList = NULL, searchSortFieldNamesDescendingList = NULL, entityID = 1, schoolYearID = NULL, page = 1, pageSize = 100000, flatten = T, ...){

		params <- as.list(environment()) %>% append(list(...))

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- "all", searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		listSkyObjects(module = "StateReportingMN", objectName = "UFARSGeneralLedgerV1", schoolYearID = schoolYearID, searchFields = searchFields, page = page, pageSize = pageSize, SearchConditionsList = searchConditionsList, SearchConditionsGroupType = searchConditionsGroupType, SearchSortFieldNamesList = searchSortFieldNamesList, SearchSortFieldNamesDescendingList = searchSortFieldNamesDescendingList, entityId = entityID, flatten = flatten)
	}


	listUFARSRevenueExpenseV1s <- function(searchConditionsList = NULL, UFARSRevenueExpenseV1ID = F, UFARSExtractRunID = F, Type = F, DimensionValueGNL = F, DimensionValueFund = F, DimensionValueOrgSite = F, DimensionValueProgram = F, DimensionValueFinance = F, DimensionValueSourceObject = F, DimensionValueCourse = F, AdoptedBudget = F, RevisedBudget = F, NextYearBudget = F, YearToDateAmount = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, AccountFiscalYearID = F, DimensionIDFund = F, DimensionIDOrgSite = F, DimensionIDProgram = F, DimensionIDFinance = F, DimensionIDSourceObject = F, DimensionIDCourse = F, ExcludeFromExport = F, CrosswalkMNValueFund = F, CrosswalkMNValueOrgSite = F, CrosswalkMNValueProgram = F, CrosswalkMNValueFinance = F, CrosswalkMNValueSourceObject = F, CrosswalkMNValueCourse = F, StateCrosswalkIDFund = F, StateCrosswalkIDOrgSite = F, StateCrosswalkIDProgram = F, StateCrosswalkIDFinance = F, StateCrosswalkIDSourceObject = F, StateCrosswalkIDCourse = F, ExtractedCrosswalkAccount = F, searchConditionsGroupType = "And", searchSortFieldNamesList = NULL, searchSortFieldNamesDescendingList = NULL, entityID = 1, schoolYearID = NULL, page = 1, pageSize = 100000, flatten = T, ...){

		params <- as.list(environment()) %>% append(list(...))

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- "all", searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		listSkyObjects(module = "StateReportingMN", objectName = "UFARSRevenueExpenseV1", schoolYearID = schoolYearID, searchFields = searchFields, page = page, pageSize = pageSize, SearchConditionsList = searchConditionsList, SearchConditionsGroupType = searchConditionsGroupType, SearchSortFieldNamesList = searchSortFieldNamesList, SearchSortFieldNamesDescendingList = searchSortFieldNamesDescendingList, entityId = entityID, flatten = flatten)
	}


	listMCCCStaffV1Exceptions <- function(searchConditionsList = NULL, MCCCStaffV1ExceptionID = F, MCCCStaffV1ID = F, EntityID = F, SchoolYearID = F, CourseID = F, StaffID = F, MCCCSubmissionID = F, MCCCSubmissionRunHistoryID = F, MessageType = F, RuleNumber = F, Message = F, ProcessType = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, LinkedToStaff = F, searchConditionsGroupType = "And", searchSortFieldNamesList = NULL, searchSortFieldNamesDescendingList = NULL, entityID = 1, schoolYearID = NULL, page = 1, pageSize = 100000, flatten = T, ...){

		params <- as.list(environment()) %>% append(list(...))

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- "all", searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		listSkyObjects(module = "StateReportingMN", objectName = "MCCCStaffV1Exception", schoolYearID = schoolYearID, searchFields = searchFields, page = page, pageSize = pageSize, SearchConditionsList = searchConditionsList, SearchConditionsGroupType = searchConditionsGroupType, SearchSortFieldNamesList = searchSortFieldNamesList, SearchSortFieldNamesDescendingList = searchSortFieldNamesDescendingList, entityId = entityID, flatten = flatten)
	}


	listMCCCStudentAssessmentV1Exceptions <- function(searchConditionsList = NULL, MCCCStudentAssessmentV1ExceptionID = F, MCCCStudentAssessmentV1ID = F, EntityID = F, SchoolYearID = F, CourseID = F, StaffID = F, StudentID = F, MCCCSubmissionID = F, MCCCSubmissionRunHistoryID = F, MessageType = F, RuleNumber = F, Message = F, ProcessType = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, searchConditionsGroupType = "And", searchSortFieldNamesList = NULL, searchSortFieldNamesDescendingList = NULL, entityID = 1, schoolYearID = NULL, page = 1, pageSize = 100000, flatten = T, ...){

		params <- as.list(environment()) %>% append(list(...))

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- "all", searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		listSkyObjects(module = "StateReportingMN", objectName = "MCCCStudentAssessmentV1Exception", schoolYearID = schoolYearID, searchFields = searchFields, page = page, pageSize = pageSize, SearchConditionsList = searchConditionsList, SearchConditionsGroupType = searchConditionsGroupType, SearchSortFieldNamesList = searchSortFieldNamesList, SearchSortFieldNamesDescendingList = searchSortFieldNamesDescendingList, entityId = entityID, flatten = flatten)
	}


	listMCCCStaffV1s <- function(searchConditionsList = NULL, MCCCStaffV1ID = F, LocalCourseCode = F, SectionCode = F, SchoolNumber = F, MCCCAcademicYearImportID = F, AcademicYearNumber = F, MCCCCalendarImportID = F, CalendarNumber = F, MCCCTermImportID = F, TermType = F, TermNumber = F, FileFolderNumber = F, IsFixedPeriod = F, MarkingIndicator = F, StateInstructionalMethodCodeMNID = F, InstructionMethod = F, StateLanguageCodeMNID = F, InstructionLanguage = F, InstructionMinutes = F, IsTeacherOfRecord = F, SchoolID = F, EntityID = F, SchoolYearID = F, CourseID = F, SectionID = F, MeetID = F, StaffMeetID = F, StaffID = F, MCCCSubmissionID = F, MCCCSubmissionRunHistoryID = F, PeriodData = F, PeriodCodes = F, HasErrors = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, KeyHash = F, UpdateHash = F, searchConditionsGroupType = "And", searchSortFieldNamesList = NULL, searchSortFieldNamesDescendingList = NULL, entityID = 1, schoolYearID = NULL, page = 1, pageSize = 100000, flatten = T, ...){

		params <- as.list(environment()) %>% append(list(...))

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- "all", searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		listSkyObjects(module = "StateReportingMN", objectName = "MCCCStaffV1", schoolYearID = schoolYearID, searchFields = searchFields, page = page, pageSize = pageSize, SearchConditionsList = searchConditionsList, SearchConditionsGroupType = searchConditionsGroupType, SearchSortFieldNamesList = searchSortFieldNamesList, SearchSortFieldNamesDescendingList = searchSortFieldNamesDescendingList, entityId = entityID, flatten = flatten)
	}


	listMCCCStudentAssessmentV1s <- function(searchConditionsList = NULL, MCCCStudentAssessmentV1ID = F, CourseID = F, LocalCourseCode = F, CurriculumYearID = F, StateStandardCodeMNID = F, StateStandardCode = F, MCCCTermImportID = F, ScoringTermNumber = F, RubricAssessmentMNID = F, ScoreValue = F, RubricLevel = F, StaffID = F, StudentID = F, StudentStateID = F, MCCCSubmissionRunHistoryID = F, EntityID = F, SchoolYearID = F, SchoolID = F, MCCCSubmissionID = F, FileFolderNumber = F, IsDirectPay = F, HasErrors = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, ScoringTermType = F, SchoolNumber = F, IsEarlyEducationAllCode = F, KeyHash = F, UpdateHash = F, StartDate = F, EndDate = F, StateAssessmentToolMNID = F, AssessmentToolCode = F, StateImplementationStatusMNID = F, ImplementationStatusCode = F, searchConditionsGroupType = "And", searchSortFieldNamesList = NULL, searchSortFieldNamesDescendingList = NULL, entityID = 1, schoolYearID = NULL, page = 1, pageSize = 100000, flatten = T, ...){

		params <- as.list(environment()) %>% append(list(...))

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- "all", searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		listSkyObjects(module = "StateReportingMN", objectName = "MCCCStudentAssessmentV1", schoolYearID = schoolYearID, searchFields = searchFields, page = page, pageSize = pageSize, SearchConditionsList = searchConditionsList, SearchConditionsGroupType = searchConditionsGroupType, SearchSortFieldNamesList = searchSortFieldNamesList, SearchSortFieldNamesDescendingList = searchSortFieldNamesDescendingList, entityId = entityID, flatten = flatten)
	}


	listPERAExclusionEmployeeV1s <- function(searchConditionsList = NULL, PERAExclusionEmployeeV1ID = F, PERAExclusionExtractRunID = F, EmployeeID = F, StatePERAExclusionCodeMNID = F, StatePERAExclusionCodeMNValue = F, StatePERAExclusionPayCycleMNID = F, StatePERAExclusionPayCycleMNValue = F, SocialSecurityNumber = F, LastName = F, FirstName = F, MiddleInitial = F, OriginalHireDate = F, LastHireDate = F, AnnualSalary = F, LastPayPeriodSalary = F, JobTitle = F, StatusAtYearEnd = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, searchConditionsGroupType = "And", searchSortFieldNamesList = NULL, searchSortFieldNamesDescendingList = NULL, entityID = 1, schoolYearID = NULL, page = 1, pageSize = 100000, flatten = T, ...){

		params <- as.list(environment()) %>% append(list(...))

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- "all", searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		listSkyObjects(module = "StateReportingMN", objectName = "PERAExclusionEmployeeV1", schoolYearID = schoolYearID, searchFields = searchFields, page = page, pageSize = pageSize, SearchConditionsList = searchConditionsList, SearchConditionsGroupType = searchConditionsGroupType, SearchSortFieldNamesList = searchSortFieldNamesList, SearchSortFieldNamesDescendingList = searchSortFieldNamesDescendingList, entityId = entityID, flatten = flatten)
	}


	listPERAExclusionExceptions <- function(searchConditionsList = NULL, PERAExclusionExceptionID = F, PERAExclusionExtractRunID = F, PERAExclusionEmployeeV1ID = F, Source = F, IsFatal = F, Type = F, Message = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, PreserveException = F, searchConditionsGroupType = "And", searchSortFieldNamesList = NULL, searchSortFieldNamesDescendingList = NULL, entityID = 1, schoolYearID = NULL, page = 1, pageSize = 100000, flatten = T, ...){

		params <- as.list(environment()) %>% append(list(...))

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- "all", searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		listSkyObjects(module = "StateReportingMN", objectName = "PERAExclusionException", schoolYearID = schoolYearID, searchFields = searchFields, page = page, pageSize = pageSize, SearchConditionsList = searchConditionsList, SearchConditionsGroupType = searchConditionsGroupType, SearchSortFieldNamesList = searchSortFieldNamesList, SearchSortFieldNamesDescendingList = searchSortFieldNamesDescendingList, entityId = entityID, flatten = flatten)
	}


	listPERAExclusionExports <- function(searchConditionsList = NULL, PERAExclusionExportID = F, PERAExclusionExtractRunID = F, MediaID = F, Status = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, searchConditionsGroupType = "And", searchSortFieldNamesList = NULL, searchSortFieldNamesDescendingList = NULL, entityID = 1, schoolYearID = NULL, page = 1, pageSize = 100000, flatten = T, ...){

		params <- as.list(environment()) %>% append(list(...))

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- "all", searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		listSkyObjects(module = "StateReportingMN", objectName = "PERAExclusionExport", schoolYearID = schoolYearID, searchFields = searchFields, page = page, pageSize = pageSize, SearchConditionsList = searchConditionsList, SearchConditionsGroupType = searchConditionsGroupType, SearchSortFieldNamesList = searchSortFieldNamesList, SearchSortFieldNamesDescendingList = searchSortFieldNamesDescendingList, entityId = entityID, flatten = flatten)
	}


	listPERAExclusionExtractRuns <- function(searchConditionsList = NULL, PERAExclusionExtractRunID = F, EmployerID = F, EmployerNumber = F, ExclusionYear = F, PERAExclusionType = F, Version = F, FatalExceptionCount = F, NonFatalExceptionCount = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, ExtractedEmployeeCount = F, LatestExport = F, LatestSentOrAcceptedExportTime = F, searchConditionsGroupType = "And", searchSortFieldNamesList = NULL, searchSortFieldNamesDescendingList = NULL, entityID = 1, schoolYearID = NULL, page = 1, pageSize = 100000, flatten = T, ...){

		params <- as.list(environment()) %>% append(list(...))

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- "all", searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		listSkyObjects(module = "StateReportingMN", objectName = "PERAExclusionExtractRun", schoolYearID = schoolYearID, searchFields = searchFields, page = page, pageSize = pageSize, SearchConditionsList = searchConditionsList, SearchConditionsGroupType = searchConditionsGroupType, SearchSortFieldNamesList = searchSortFieldNamesList, SearchSortFieldNamesDescendingList = searchSortFieldNamesDescendingList, entityId = entityID, flatten = flatten)
	}


	listSTARExports <- function(searchConditionsList = NULL, STARExportID = F, STARExtractRunID = F, Type = F, MediaID = F, Status = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, searchConditionsGroupType = "And", searchSortFieldNamesList = NULL, searchSortFieldNamesDescendingList = NULL, entityID = 1, schoolYearID = NULL, page = 1, pageSize = 100000, flatten = T, ...){

		params <- as.list(environment()) %>% append(list(...))

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- "all", searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		listSkyObjects(module = "StateReportingMN", objectName = "STARExport", schoolYearID = schoolYearID, searchFields = searchFields, page = page, pageSize = pageSize, SearchConditionsList = searchConditionsList, SearchConditionsGroupType = searchConditionsGroupType, SearchSortFieldNamesList = searchSortFieldNamesList, SearchSortFieldNamesDescendingList = searchSortFieldNamesDescendingList, entityId = entityID, flatten = flatten)
	}


	listStateUFARSRestrictedGridMNS <- function(searchConditionsList = NULL, StateUFARSRestrictedGridMNID = F, SkywardID = F, GridType = F, AccountType = F, Finance = F, Fund = F, OrganizationLow = F, OrganizationHigh = F, ProgramLow = F, ProgramHigh = F, ObjectLow = F, ObjectHigh = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, SkywardHash = F, FiscalYearLow = F, FiscalYearHigh = F, IsInvalid = F, searchConditionsGroupType = "And", searchSortFieldNamesList = NULL, searchSortFieldNamesDescendingList = NULL, entityID = 1, schoolYearID = NULL, page = 1, pageSize = 100000, flatten = T, ...){

		params <- as.list(environment()) %>% append(list(...))

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- "all", searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		listSkyObjects(module = "StateReportingMN", objectName = "StateUFARSRestrictedGridMN", schoolYearID = schoolYearID, searchFields = searchFields, page = page, pageSize = pageSize, SearchConditionsList = searchConditionsList, SearchConditionsGroupType = searchConditionsGroupType, SearchSortFieldNamesList = searchSortFieldNamesList, SearchSortFieldNamesDescendingList = searchSortFieldNamesDescendingList, entityId = entityID, flatten = flatten)
	}


	listStateUFARSUnrestrictedGridMNS <- function(searchConditionsList = NULL, StateUFARSUnrestrictedGridMNID = F, SkywardID = F, GridType = F, AccountType = F, Fund = F, ProgramLow = F, ProgramHigh = F, OrganizationLow = F, OrganizationHigh = F, ObjectLow = F, ObjectHigh = F, FinanceLow = F, FinanceHigh = F, CourseLow = F, CourseHigh = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, SkywardHash = F, FiscalYearLow = F, FiscalYearHigh = F, IsInvalid = F, searchConditionsGroupType = "And", searchSortFieldNamesList = NULL, searchSortFieldNamesDescendingList = NULL, entityID = 1, schoolYearID = NULL, page = 1, pageSize = 100000, flatten = T, ...){

		params <- as.list(environment()) %>% append(list(...))

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- "all", searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		listSkyObjects(module = "StateReportingMN", objectName = "StateUFARSUnrestrictedGridMN", schoolYearID = schoolYearID, searchFields = searchFields, page = page, pageSize = pageSize, SearchConditionsList = searchConditionsList, SearchConditionsGroupType = searchConditionsGroupType, SearchSortFieldNamesList = searchSortFieldNamesList, SearchSortFieldNamesDescendingList = searchSortFieldNamesDescendingList, entityId = entityID, flatten = flatten)
	}


	listMCCCStudentCourseV1Exceptions <- function(searchConditionsList = NULL, MCCCStudentCourseV1ExceptionID = F, MCCCStudentCourseV1ID = F, EntityID = F, SchoolYearID = F, CourseID = F, StudentID = F, StaffID = F, MCCCSubmissionID = F, MCCCSubmissionRunHistoryID = F, MessageType = F, RuleNumber = F, Message = F, ProcessType = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, searchConditionsGroupType = "And", searchSortFieldNamesList = NULL, searchSortFieldNamesDescendingList = NULL, entityID = 1, schoolYearID = NULL, page = 1, pageSize = 100000, flatten = T, ...){

		params <- as.list(environment()) %>% append(list(...))

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- "all", searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		listSkyObjects(module = "StateReportingMN", objectName = "MCCCStudentCourseV1Exception", schoolYearID = schoolYearID, searchFields = searchFields, page = page, pageSize = pageSize, SearchConditionsList = searchConditionsList, SearchConditionsGroupType = searchConditionsGroupType, SearchSortFieldNamesList = searchSortFieldNamesList, SearchSortFieldNamesDescendingList = searchSortFieldNamesDescendingList, entityId = entityID, flatten = flatten)
	}


	listMCCCStudentCourseV1s <- function(searchConditionsList = NULL, MCCCStudentCourseV1ID = F, LocalCourseCode = F, CourseID = F, SectionID = F, SchoolID = F, SectionCode = F, CurriculumYearID = F, StateCourseCodeMNID = F, StateCourseCode = F, StudentID = F, LastName = F, FirstName = F, MiddleName = F, Suffix = F, Gender = F, BirthDate = F, StudentStateID = F, MeetID = F, SchoolNumber = F, RecordType = F, AcademicYearNumber = F, MCCCAcademicYearImportID = F, CalendarNumber = F, MCCCCalendarImportID = F, TermType = F, MCCCTermImportID = F, TermNumber = F, StateSubjectAreaCodeMNID = F, SubjectArea = F, MCCCTermImportIDMarkingTerm = F, MarkingTermType = F, MarkingTermNumber = F, StaffID = F, FileFolderNumber = F, StudentGradeBucketID = F, GradeMarkIDEarned = F, GradeMark = F, LocalCreditsEarned = F, CollegeMarkEarned = F, CollegeCredits = F, CourseLevelMNID = F, CollegeCode = F, CollegeCourseCode = F, CollegeCourseTitle = F, MCCCSubmissionID = F, MCCCSubmissionRunHistoryID = F, EntityID = F, SchoolYearID = F, HasErrors = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, InstitutionID = F, StateGradeMarkMNID = F, KeyHash = F, UpdateHash = F, EarlyEducationInstructionalMinutes = F, searchConditionsGroupType = "And", searchSortFieldNamesList = NULL, searchSortFieldNamesDescendingList = NULL, entityID = 1, schoolYearID = NULL, page = 1, pageSize = 100000, flatten = T, ...){

		params <- as.list(environment()) %>% append(list(...))

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- "all", searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		listSkyObjects(module = "StateReportingMN", objectName = "MCCCStudentCourseV1", schoolYearID = schoolYearID, searchFields = searchFields, page = page, pageSize = pageSize, SearchConditionsList = searchConditionsList, SearchConditionsGroupType = searchConditionsGroupType, SearchSortFieldNamesList = searchSortFieldNamesList, SearchSortFieldNamesDescendingList = searchSortFieldNamesDescendingList, entityId = entityID, flatten = flatten)
	}


	listSTARExceptions <- function(searchConditionsList = NULL, STARExceptionID = F, STARExtractRunID = F, STAREmployeeV1ID = F, STARLicensedStaffEmploymentV1ID = F, STARLicensedStaffAssignmentV1ID = F, STARNonLicensedStaffV1ID = F, Source = F, IsFatal = F, Type = F, Message = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, PreserveException = F, searchConditionsGroupType = "And", searchSortFieldNamesList = NULL, searchSortFieldNamesDescendingList = NULL, entityID = 1, schoolYearID = NULL, page = 1, pageSize = 100000, flatten = T, ...){

		params <- as.list(environment()) %>% append(list(...))

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- "all", searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		listSkyObjects(module = "StateReportingMN", objectName = "STARException", schoolYearID = schoolYearID, searchFields = searchFields, page = page, pageSize = pageSize, SearchConditionsList = searchConditionsList, SearchConditionsGroupType = searchConditionsGroupType, SearchSortFieldNamesList = searchSortFieldNamesList, SearchSortFieldNamesDescendingList = searchSortFieldNamesDescendingList, entityId = entityID, flatten = flatten)
	}


	listMCCCCourseOfferingV1Exceptions <- function(searchConditionsList = NULL, MCCCCourseOfferingV1ExceptionID = F, MCCCCourseOfferingV1ID = F, SchoolID = F, CurriculumYearID = F, EntityID = F, SchoolYearID = F, CourseID = F, IsCollegeRecordError = F, MCCCSubmissionID = F, MCCCSubmissionRunHistoryID = F, MessageType = F, RuleNumber = F, Message = F, ProcessType = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, searchConditionsGroupType = "And", searchSortFieldNamesList = NULL, searchSortFieldNamesDescendingList = NULL, entityID = 1, schoolYearID = NULL, page = 1, pageSize = 100000, flatten = T, ...){

		params <- as.list(environment()) %>% append(list(...))

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- "all", searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		listSkyObjects(module = "StateReportingMN", objectName = "MCCCCourseOfferingV1Exception", schoolYearID = schoolYearID, searchFields = searchFields, page = page, pageSize = pageSize, SearchConditionsList = searchConditionsList, SearchConditionsGroupType = searchConditionsGroupType, SearchSortFieldNamesList = searchSortFieldNamesList, SearchSortFieldNamesDescendingList = searchSortFieldNamesDescendingList, entityId = entityID, flatten = flatten)
	}


	listMCCCStandardPlacementV1Exceptions <- function(searchConditionsList = NULL, MCCCStandardPlacementV1ExceptionID = F, MCCCStandardPlacementV1ID = F, SchoolID = F, MCCCSubmissionID = F, MCCCSubmissionRunHistoryID = F, MessageType = F, RuleNumber = F, Message = F, ProcessType = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, CourseID = F, CurriculumYearID = F, EntityID = F, SchoolYearID = F, searchConditionsGroupType = "And", searchSortFieldNamesList = NULL, searchSortFieldNamesDescendingList = NULL, entityID = 1, schoolYearID = NULL, page = 1, pageSize = 100000, flatten = T, ...){

		params <- as.list(environment()) %>% append(list(...))

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- "all", searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		listSkyObjects(module = "StateReportingMN", objectName = "MCCCStandardPlacementV1Exception", schoolYearID = schoolYearID, searchFields = searchFields, page = page, pageSize = pageSize, SearchConditionsList = searchConditionsList, SearchConditionsGroupType = searchConditionsGroupType, SearchSortFieldNamesList = searchSortFieldNamesList, SearchSortFieldNamesDescendingList = searchSortFieldNamesDescendingList, entityId = entityID, flatten = flatten)
	}


	listMCCCCourseOfferingCollegeCourseV1s <- function(searchConditionsList = NULL, MCCCCourseOfferingCollegeCourseV1ID = F, MCCCCourseOfferingV1ID = F, CourseLevelMNID = F, CollegeCourseCode = F, StateCollegeCourseLevelMNID = F, CollegeCourseLevel = F, CollegeCode = F, CollegeCourseCredit = F, HasErrors = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, InstitutionID = F, KeyHash = F, UpdateHash = F, searchConditionsGroupType = "And", searchSortFieldNamesList = NULL, searchSortFieldNamesDescendingList = NULL, entityID = 1, schoolYearID = NULL, page = 1, pageSize = 100000, flatten = T, ...){

		params <- as.list(environment()) %>% append(list(...))

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- "all", searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		listSkyObjects(module = "StateReportingMN", objectName = "MCCCCourseOfferingCollegeCourseV1", schoolYearID = schoolYearID, searchFields = searchFields, page = page, pageSize = pageSize, SearchConditionsList = searchConditionsList, SearchConditionsGroupType = searchConditionsGroupType, SearchSortFieldNamesList = searchSortFieldNamesList, SearchSortFieldNamesDescendingList = searchSortFieldNamesDescendingList, entityId = entityID, flatten = flatten)
	}


	listMCCCCourseOfferingV1s <- function(searchConditionsList = NULL, MCCCCourseOfferingV1ID = F, LocalCourseCode = F, LocalCourseTitle = F, SchoolNumber = F, StateCourseCodeMNID = F, StateCourseCode = F, LocalDescription = F, SequenceNumber = F, SequenceLimit = F, StateStandardAddressedCodeMNID = F, StandardAddressedCode = F, GraduationRequirementIndicator = F, EndOfCourseIndicator = F, MCCCSubmissionID = F, MCCCSubmissionRunHistoryID = F, EntityID = F, SchoolYearID = F, SchoolID = F, CurriculumYearID = F, CourseID = F, IsDirectPayCourse = F, CourseLevelData = F, CourseLevelCodes = F, HasErrors = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, CourseClassificationType = F, GradeRange = F, KeyHash = F, UpdateHash = F, StateEarlyEducationLocationMNID = F, EarlyEducationLocationCode = F, StateProgramIndicator = F, FederalProgramIndicator = F, ABEIndicator = F, searchConditionsGroupType = "And", searchSortFieldNamesList = NULL, searchSortFieldNamesDescendingList = NULL, entityID = 1, schoolYearID = NULL, page = 1, pageSize = 100000, flatten = T, ...){

		params <- as.list(environment()) %>% append(list(...))

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- "all", searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		listSkyObjects(module = "StateReportingMN", objectName = "MCCCCourseOfferingV1", schoolYearID = schoolYearID, searchFields = searchFields, page = page, pageSize = pageSize, SearchConditionsList = searchConditionsList, SearchConditionsGroupType = searchConditionsGroupType, SearchSortFieldNamesList = searchSortFieldNamesList, SearchSortFieldNamesDescendingList = searchSortFieldNamesDescendingList, entityId = entityID, flatten = flatten)
	}


	listMCCCStandardPlacementV1s <- function(searchConditionsList = NULL, MCCCStandardPlacementV1ID = F, StandardPlacementMNID = F, StateStandardCodeMNID = F, StandardCode = F, HasErrors = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, IsEarlyEducationAllCode = F, KeyHash = F, UpdateHash = F, CurriculumYearID = F, EntityID = F, SchoolYearID = F, MCCCSubmissionID = F, MCCCSubmissionRunHistoryID = F, SchoolID = F, SchoolNumber = F, CourseID = F, LocalCourseCode = F, searchConditionsGroupType = "And", searchSortFieldNamesList = NULL, searchSortFieldNamesDescendingList = NULL, entityID = 1, schoolYearID = NULL, page = 1, pageSize = 100000, flatten = T, ...){

		params <- as.list(environment()) %>% append(list(...))

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- "all", searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		listSkyObjects(module = "StateReportingMN", objectName = "MCCCStandardPlacementV1", schoolYearID = schoolYearID, searchFields = searchFields, page = page, pageSize = pageSize, SearchConditionsList = searchConditionsList, SearchConditionsGroupType = searchConditionsGroupType, SearchSortFieldNamesList = searchSortFieldNamesList, SearchSortFieldNamesDescendingList = searchSortFieldNamesDescendingList, entityId = entityID, flatten = flatten)
	}


	listMCCCAcademicYearImports <- function(searchConditionsList = NULL, MCCCAcademicYearImportID = F, SchoolYearID = F, DistrictID = F, MCCCSubmissionID = F, MCCCSubmissionRunHistoryID = F, Code = F, Description = F, CodeDescription = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, searchConditionsGroupType = "And", searchSortFieldNamesList = NULL, searchSortFieldNamesDescendingList = NULL, entityID = 1, schoolYearID = NULL, page = 1, pageSize = 100000, flatten = T, ...){

		params <- as.list(environment()) %>% append(list(...))

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- "all", searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		listSkyObjects(module = "StateReportingMN", objectName = "MCCCAcademicYearImport", schoolYearID = schoolYearID, searchFields = searchFields, page = page, pageSize = pageSize, SearchConditionsList = searchConditionsList, SearchConditionsGroupType = searchConditionsGroupType, SearchSortFieldNamesList = searchSortFieldNamesList, SearchSortFieldNamesDescendingList = searchSortFieldNamesDescendingList, entityId = entityID, flatten = flatten)
	}


	listMCCCCalendarImports <- function(searchConditionsList = NULL, MCCCCalendarImportID = F, SchoolYearID = F, DistrictID = F, MCCCAcademicYearImportID = F, MCCCSubmissionID = F, MCCCSubmissionRunHistoryID = F, Code = F, Description = F, CodeDescription = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, searchConditionsGroupType = "And", searchSortFieldNamesList = NULL, searchSortFieldNamesDescendingList = NULL, entityID = 1, schoolYearID = NULL, page = 1, pageSize = 100000, flatten = T, ...){

		params <- as.list(environment()) %>% append(list(...))

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- "all", searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		listSkyObjects(module = "StateReportingMN", objectName = "MCCCCalendarImport", schoolYearID = schoolYearID, searchFields = searchFields, page = page, pageSize = pageSize, SearchConditionsList = searchConditionsList, SearchConditionsGroupType = searchConditionsGroupType, SearchSortFieldNamesList = searchSortFieldNamesList, SearchSortFieldNamesDescendingList = searchSortFieldNamesDescendingList, entityId = entityID, flatten = flatten)
	}


	listMCCCTermImports <- function(searchConditionsList = NULL, MCCCTermImportID = F, SchoolYearID = F, DistrictID = F, MCCCSubmissionID = F, MCCCSubmissionRunHistoryID = F, TermType = F, Code = F, Description = F, CodeDescription = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, searchConditionsGroupType = "And", searchSortFieldNamesList = NULL, searchSortFieldNamesDescendingList = NULL, entityID = 1, schoolYearID = NULL, page = 1, pageSize = 100000, flatten = T, ...){

		params <- as.list(environment()) %>% append(list(...))

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- "all", searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		listSkyObjects(module = "StateReportingMN", objectName = "MCCCTermImport", schoolYearID = schoolYearID, searchFields = searchFields, page = page, pageSize = pageSize, SearchConditionsList = searchConditionsList, SearchConditionsGroupType = searchConditionsGroupType, SearchSortFieldNamesList = searchSortFieldNamesList, SearchSortFieldNamesDescendingList = searchSortFieldNamesDescendingList, entityId = entityID, flatten = flatten)
	}


	listTempMCCCCalendarImports <- function(searchConditionsList = NULL, TempMCCCCalendarImportID = F, Code = F, Description = F, ImportTable = F, AcademicYearCode = F, TermType = F, FailedSave = F, Message = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, searchConditionsGroupType = "And", searchSortFieldNamesList = NULL, searchSortFieldNamesDescendingList = NULL, entityID = 1, schoolYearID = NULL, page = 1, pageSize = 100000, flatten = T, ...){

		params <- as.list(environment()) %>% append(list(...))

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- "all", searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		listSkyObjects(module = "StateReportingMN", objectName = "TempMCCCCalendarImport", schoolYearID = schoolYearID, searchFields = searchFields, page = page, pageSize = pageSize, SearchConditionsList = searchConditionsList, SearchConditionsGroupType = searchConditionsGroupType, SearchSortFieldNamesList = searchSortFieldNamesList, SearchSortFieldNamesDescendingList = searchSortFieldNamesDescendingList, entityId = entityID, flatten = flatten)
	}


	listNewHireExceptions <- function(searchConditionsList = NULL, NewHireExceptionID = F, NewHireExtractRunID = F, EmployerID = F, EmployeeID = F, Source = F, IsFatal = F, Type = F, Message = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, PreserveException = F, searchConditionsGroupType = "And", searchSortFieldNamesList = NULL, searchSortFieldNamesDescendingList = NULL, entityID = 1, schoolYearID = NULL, page = 1, pageSize = 100000, flatten = T, ...){

		params <- as.list(environment()) %>% append(list(...))

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- "all", searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		listSkyObjects(module = "StateReportingMN", objectName = "NewHireException", schoolYearID = schoolYearID, searchFields = searchFields, page = page, pageSize = pageSize, SearchConditionsList = searchConditionsList, SearchConditionsGroupType = searchConditionsGroupType, SearchSortFieldNamesList = searchSortFieldNamesList, SearchSortFieldNamesDescendingList = searchSortFieldNamesDescendingList, entityId = entityID, flatten = flatten)
	}


	listNewHireExports <- function(searchConditionsList = NULL, NewHireExportID = F, NewHireExtractRunID = F, MediaID = F, Status = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, searchConditionsGroupType = "And", searchSortFieldNamesList = NULL, searchSortFieldNamesDescendingList = NULL, entityID = 1, schoolYearID = NULL, page = 1, pageSize = 100000, flatten = T, ...){

		params <- as.list(environment()) %>% append(list(...))

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- "all", searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		listSkyObjects(module = "StateReportingMN", objectName = "NewHireExport", schoolYearID = schoolYearID, searchFields = searchFields, page = page, pageSize = pageSize, SearchConditionsList = searchConditionsList, SearchConditionsGroupType = searchConditionsGroupType, SearchSortFieldNamesList = searchSortFieldNamesList, SearchSortFieldNamesDescendingList = searchSortFieldNamesDescendingList, entityId = entityID, flatten = flatten)
	}


	listSTARLicensedStaffAssignmentV1s <- function(searchConditionsList = NULL, STARLicensedStaffAssignmentV1ID = F, STARLicensedStaffEmploymentV1ID = F, StateSTARAssignmentCodeMNID = F, StateSTARAssignmentCodeMNValue = F, StateSTARGradeLevelMNID = F, StateSTARGradeLevelMNValue = F, StateSTARModeOfTeachingMNID = F, StateSTARModeOfTeachingMNValue = F, SchoolNumber = F, PeriodsPerWeek = F, LengthOfPeriod = F, TotalNumberOfPupils = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, searchConditionsGroupType = "And", searchSortFieldNamesList = NULL, searchSortFieldNamesDescendingList = NULL, entityID = 1, schoolYearID = NULL, page = 1, pageSize = 100000, flatten = T, ...){

		params <- as.list(environment()) %>% append(list(...))

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- "all", searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		listSkyObjects(module = "StateReportingMN", objectName = "STARLicensedStaffAssignmentV1", schoolYearID = schoolYearID, searchFields = searchFields, page = page, pageSize = pageSize, SearchConditionsList = searchConditionsList, SearchConditionsGroupType = searchConditionsGroupType, SearchSortFieldNamesList = searchSortFieldNamesList, SearchSortFieldNamesDescendingList = searchSortFieldNamesDescendingList, entityId = entityID, flatten = flatten)
	}


	listSTARLicensedStaffEmploymentV1s <- function(searchConditionsList = NULL, STARLicensedStaffEmploymentV1ID = F, STAREmployeeV1ID = F, StateSTARNewLicensedStaffMNID = F, StateSTARNewLicensedStaffMNValue = F, StateSTARInactiveTransferTerminationMNID = F, StateSTARInactiveTransferTerminationMNValue = F, StateSTARHighestEducationLevelMNID = F, StateSTARHighestEducationLevelMNValue = F, ContractSalary = F, ReportedContractSalary = F, ContractDays = F, YearsOfExperienceSuperintendent = F, YearsOfExperiencePrincipal = F, YearsOfExperienceClassroomTeacher = F, YearsOfExperienceOther = F, OutOfDistrictAssignment = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, STAREmploymentType = F, searchConditionsGroupType = "And", searchSortFieldNamesList = NULL, searchSortFieldNamesDescendingList = NULL, entityID = 1, schoolYearID = NULL, page = 1, pageSize = 100000, flatten = T, ...){

		params <- as.list(environment()) %>% append(list(...))

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- "all", searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		listSkyObjects(module = "StateReportingMN", objectName = "STARLicensedStaffEmploymentV1", schoolYearID = schoolYearID, searchFields = searchFields, page = page, pageSize = pageSize, SearchConditionsList = searchConditionsList, SearchConditionsGroupType = searchConditionsGroupType, SearchSortFieldNamesList = searchSortFieldNamesList, SearchSortFieldNamesDescendingList = searchSortFieldNamesDescendingList, entityId = entityID, flatten = flatten)
	}


	listSTAREmployeeV1s <- function(searchConditionsList = NULL, STAREmployeeV1ID = F, STARExtractRunID = F, EmployeeID = F, FileFolderNumber = F, SocialSecurityNumber = F, LastName = F, FirstName = F, MiddleName = F, ReportedName = F, Gender = F, BirthDate = F, StateEthnicityRaceCodeMNID = F, StateEthnicityRaceCodeMNValue = F, IsHispanic = F, IsAmericanIndianOrAlaskanNative = F, IsAsian = F, IsNativeHawaiianOrPacificIslander = F, IsBlack = F, IsWhite = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, HasLAFile = F, HasLBFile = F, HasNAFile = F, STARUniqueIdentifier = F, NameSuffixID = F, NameSuffixValue = F, searchConditionsGroupType = "And", searchSortFieldNamesList = NULL, searchSortFieldNamesDescendingList = NULL, entityID = 1, schoolYearID = NULL, page = 1, pageSize = 100000, flatten = T, ...){

		params <- as.list(environment()) %>% append(list(...))

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- "all", searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		listSkyObjects(module = "StateReportingMN", objectName = "STAREmployeeV1", schoolYearID = schoolYearID, searchFields = searchFields, page = page, pageSize = pageSize, SearchConditionsList = searchConditionsList, SearchConditionsGroupType = searchConditionsGroupType, SearchSortFieldNamesList = searchSortFieldNamesList, SearchSortFieldNamesDescendingList = searchSortFieldNamesDescendingList, entityId = entityID, flatten = flatten)
	}


	listSTARExtractRuns <- function(searchConditionsList = NULL, STARExtractRunID = F, EmployerID = F, FiscalYearID = F, SnapshotDate = F, DistrictNumber = F, StateDistrictTypeCodeMNID = F, StateDistrictTypeCodeMNValue = F, Version = F, FatalExceptionCount = F, NonFatalExceptionCount = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, ExtractedLAFileCount = F, ExtractedLBFileCount = F, ExtractedNAFileCount = F, LatestLicensedStaffEmploymentExport = F, LatestLicensedStaffAssignmentExport = F, LatestNonLicensedStaffExport = F, LatestExport = F, LatestSentOrAcceptedExportTime = F, searchConditionsGroupType = "And", searchSortFieldNamesList = NULL, searchSortFieldNamesDescendingList = NULL, entityID = 1, schoolYearID = NULL, page = 1, pageSize = 100000, flatten = T, ...){

		params <- as.list(environment()) %>% append(list(...))

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- "all", searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		listSkyObjects(module = "StateReportingMN", objectName = "STARExtractRun", schoolYearID = schoolYearID, searchFields = searchFields, page = page, pageSize = pageSize, SearchConditionsList = searchConditionsList, SearchConditionsGroupType = searchConditionsGroupType, SearchSortFieldNamesList = searchSortFieldNamesList, SearchSortFieldNamesDescendingList = searchSortFieldNamesDescendingList, entityId = entityID, flatten = flatten)
	}


	listSTARNonLicensedStaffV1s <- function(searchConditionsList = NULL, STARNonLicensedStaffV1ID = F, STAREmployeeV1ID = F, SchoolNumber = F, StateSTARNewNonLicensedStaffMNID = F, StateSTARNewNonLicensedStaffMNValue = F, StateSTARAssignmentCodeMNID = F, StateSTARAssignmentCodeMNValue = F, HoursWorkedPerWeek = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, STAREmploymentType = F, searchConditionsGroupType = "And", searchSortFieldNamesList = NULL, searchSortFieldNamesDescendingList = NULL, entityID = 1, schoolYearID = NULL, page = 1, pageSize = 100000, flatten = T, ...){

		params <- as.list(environment()) %>% append(list(...))

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- "all", searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		listSkyObjects(module = "StateReportingMN", objectName = "STARNonLicensedStaffV1", schoolYearID = schoolYearID, searchFields = searchFields, page = page, pageSize = pageSize, SearchConditionsList = searchConditionsList, SearchConditionsGroupType = searchConditionsGroupType, SearchSortFieldNamesList = searchSortFieldNamesList, SearchSortFieldNamesDescendingList = searchSortFieldNamesDescendingList, entityId = entityID, flatten = flatten)
	}


	listStateStandardAddressedCodeMNS <- function(searchConditionsList = NULL, StateStandardAddressedCodeMNID = F, Code = F, Description = F, CodeDescription = F, ValidYearLow = F, ValidYearHigh = F, IsInvalid = F, SkywardID = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, SkywardHash = F, searchConditionsGroupType = "And", searchSortFieldNamesList = NULL, searchSortFieldNamesDescendingList = NULL, entityID = 1, schoolYearID = NULL, page = 1, pageSize = 100000, flatten = T, ...){

		params <- as.list(environment()) %>% append(list(...))

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- "all", searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		listSkyObjects(module = "StateReportingMN", objectName = "StateStandardAddressedCodeMN", schoolYearID = schoolYearID, searchFields = searchFields, page = page, pageSize = pageSize, SearchConditionsList = searchConditionsList, SearchConditionsGroupType = searchConditionsGroupType, SearchSortFieldNamesList = searchSortFieldNamesList, SearchSortFieldNamesDescendingList = searchSortFieldNamesDescendingList, entityId = entityID, flatten = flatten)
	}


	listMCCCSubmissions <- function(searchConditionsList = NULL, MCCCSubmissionID = F, SkywardID = F, Description = F, OpenDate = F, CloseDate = F, SchoolYearID = F, HasAnExport = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, SkywardHash = F, searchConditionsGroupType = "And", searchSortFieldNamesList = NULL, searchSortFieldNamesDescendingList = NULL, entityID = 1, schoolYearID = NULL, page = 1, pageSize = 100000, flatten = T, ...){

		params <- as.list(environment()) %>% append(list(...))

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- "all", searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		listSkyObjects(module = "StateReportingMN", objectName = "MCCCSubmission", schoolYearID = schoolYearID, searchFields = searchFields, page = page, pageSize = pageSize, SearchConditionsList = searchConditionsList, SearchConditionsGroupType = searchConditionsGroupType, SearchSortFieldNamesList = searchSortFieldNamesList, SearchSortFieldNamesDescendingList = searchSortFieldNamesDescendingList, entityId = entityID, flatten = flatten)
	}


	listMCCCSubmissionRunHistories <- function(searchConditionsList = NULL, MCCCSubmissionRunHistoryID = F, MCCCSubmissionID = F, StartDateTime = F, EndDateTime = F, Type = F, MediaID = F, IsExport = F, IsLocked = F, Status = F, RunData = F, UserIDCanceledBy = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, ScopeAccessAllowed = F, HasValidMedia = F, CanAccessMedia = F, RunIdentification = F, RunParameters = F, searchConditionsGroupType = "And", searchSortFieldNamesList = NULL, searchSortFieldNamesDescendingList = NULL, entityID = 1, schoolYearID = NULL, page = 1, pageSize = 100000, flatten = T, ...){

		params <- as.list(environment()) %>% append(list(...))

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- "all", searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		listSkyObjects(module = "StateReportingMN", objectName = "MCCCSubmissionRunHistory", schoolYearID = schoolYearID, searchFields = searchFields, page = page, pageSize = pageSize, SearchConditionsList = searchConditionsList, SearchConditionsGroupType = searchConditionsGroupType, SearchSortFieldNamesList = searchSortFieldNamesList, SearchSortFieldNamesDescendingList = searchSortFieldNamesDescendingList, entityId = entityID, flatten = flatten)
	}


	listStateSubjectAreaCodeMNS <- function(searchConditionsList = NULL, StateSubjectAreaCodeMNID = F, Rank = F, Code = F, Description = F, CodeDescription = F, ValidYearLow = F, ValidYearHigh = F, IsInvalid = F, SkywardID = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, SkywardHash = F, searchConditionsGroupType = "And", searchSortFieldNamesList = NULL, searchSortFieldNamesDescendingList = NULL, entityID = 1, schoolYearID = NULL, page = 1, pageSize = 100000, flatten = T, ...){

		params <- as.list(environment()) %>% append(list(...))

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- "all", searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		listSkyObjects(module = "StateReportingMN", objectName = "StateSubjectAreaCodeMN", schoolYearID = schoolYearID, searchFields = searchFields, page = page, pageSize = pageSize, SearchConditionsList = searchConditionsList, SearchConditionsGroupType = searchConditionsGroupType, SearchSortFieldNamesList = searchSortFieldNamesList, SearchSortFieldNamesDescendingList = searchSortFieldNamesDescendingList, entityId = entityID, flatten = flatten)
	}


	listStateCollegeCourseLevelMNS <- function(searchConditionsList = NULL, StateCollegeCourseLevelMNID = F, Code = F, Description = F, CodeDescription = F, ValidYearLow = F, ValidYearHigh = F, IsInvalid = F, SkywardID = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, IsCollegeCourseLevel = F, SkywardHash = F, searchConditionsGroupType = "And", searchSortFieldNamesList = NULL, searchSortFieldNamesDescendingList = NULL, entityID = 1, schoolYearID = NULL, page = 1, pageSize = 100000, flatten = T, ...){

		params <- as.list(environment()) %>% append(list(...))

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- "all", searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		listSkyObjects(module = "StateReportingMN", objectName = "StateCollegeCourseLevelMN", schoolYearID = schoolYearID, searchFields = searchFields, page = page, pageSize = pageSize, SearchConditionsList = searchConditionsList, SearchConditionsGroupType = searchConditionsGroupType, SearchSortFieldNamesList = searchSortFieldNamesList, SearchSortFieldNamesDescendingList = searchSortFieldNamesDescendingList, entityId = entityID, flatten = flatten)
	}


	listStateInstructionalMethodCodeMNS <- function(searchConditionsList = NULL, StateInstructionalMethodCodeMNID = F, Code = F, Description = F, CodeDescription = F, GradeRange = F, ValidYearLow = F, ValidYearHigh = F, IsInvalid = F, SkywardID = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, SkywardHash = F, searchConditionsGroupType = "And", searchSortFieldNamesList = NULL, searchSortFieldNamesDescendingList = NULL, entityID = 1, schoolYearID = NULL, page = 1, pageSize = 100000, flatten = T, ...){

		params <- as.list(environment()) %>% append(list(...))

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- "all", searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		listSkyObjects(module = "StateReportingMN", objectName = "StateInstructionalMethodCodeMN", schoolYearID = schoolYearID, searchFields = searchFields, page = page, pageSize = pageSize, SearchConditionsList = searchConditionsList, SearchConditionsGroupType = searchConditionsGroupType, SearchSortFieldNamesList = searchSortFieldNamesList, SearchSortFieldNamesDescendingList = searchSortFieldNamesDescendingList, entityId = entityID, flatten = flatten)
	}


	listStateStandardCodeMNS <- function(searchConditionsList = NULL, StateStandardCodeMNID = F, Code = F, Description = F, StandardType = F, CodeDescription = F, ValidYearLow = F, ValidYearHigh = F, IsInvalid = F, SkywardID = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, IsEarlyEducationAllCode = F, SkywardHash = F, searchConditionsGroupType = "And", searchSortFieldNamesList = NULL, searchSortFieldNamesDescendingList = NULL, entityID = 1, schoolYearID = NULL, page = 1, pageSize = 100000, flatten = T, ...){

		params <- as.list(environment()) %>% append(list(...))

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- "all", searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		listSkyObjects(module = "StateReportingMN", objectName = "StateStandardCodeMN", schoolYearID = schoolYearID, searchFields = searchFields, page = page, pageSize = pageSize, SearchConditionsList = searchConditionsList, SearchConditionsGroupType = searchConditionsGroupType, SearchSortFieldNamesList = searchSortFieldNamesList, SearchSortFieldNamesDescendingList = searchSortFieldNamesDescendingList, entityId = entityID, flatten = flatten)
	}


	listStateCourseCodeMNS <- function(searchConditionsList = NULL, StateCourseCodeMNID = F, Code = F, Description = F, CodeDescription = F, CodeNumericValue = F, CourseClassificationType = F, GradeRange = F, IsUndefinedSubject = F, ValidYearLow = F, ValidYearHigh = F, IsInvalid = F, SkywardID = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, SkywardHash = F, searchConditionsGroupType = "And", searchSortFieldNamesList = NULL, searchSortFieldNamesDescendingList = NULL, entityID = 1, schoolYearID = NULL, page = 1, pageSize = 100000, flatten = T, ...){

		params <- as.list(environment()) %>% append(list(...))

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- "all", searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		listSkyObjects(module = "StateReportingMN", objectName = "StateCourseCodeMN", schoolYearID = schoolYearID, searchFields = searchFields, page = page, pageSize = pageSize, SearchConditionsList = searchConditionsList, SearchConditionsGroupType = searchConditionsGroupType, SearchSortFieldNamesList = searchSortFieldNamesList, SearchSortFieldNamesDescendingList = searchSortFieldNamesDescendingList, entityId = entityID, flatten = flatten)
	}


	listStateReportingMNNewHireEmployeeV1s <- function(searchConditionsList = NULL, NewHireEmployeeV1ID = F, NewHireEmployerV1ID = F, EmployeeID = F, FirstNameLegal = F, MiddleNameLegal = F, LastNameLegal = F, SocialSecurityNumber = F, AddressID = F, AddressLine1 = F, AddressLine2 = F, AddressLine3 = F, City = F, StateCode = F, ZipCode = F, ZipCodePlusFour = F, CountryCode = F, BirthDate = F, HireDate = F, StateIDHire = F, HireStateCode = F, IsIndependentContractor = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, IsRehire = F, searchConditionsGroupType = "And", searchSortFieldNamesList = NULL, searchSortFieldNamesDescendingList = NULL, entityID = 1, schoolYearID = NULL, page = 1, pageSize = 100000, flatten = T, ...){

		params <- as.list(environment()) %>% append(list(...))

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- "all", searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		listSkyObjects(module = "StateReportingMN", objectName = "NewHireEmployeeV1", schoolYearID = schoolYearID, searchFields = searchFields, page = page, pageSize = pageSize, SearchConditionsList = searchConditionsList, SearchConditionsGroupType = searchConditionsGroupType, SearchSortFieldNamesList = searchSortFieldNamesList, SearchSortFieldNamesDescendingList = searchSortFieldNamesDescendingList, entityId = entityID, flatten = flatten)
	}


	listNewHireExtractRuns <- function(searchConditionsList = NULL, NewHireExtractRunID = F, PostingDate = F, Version = F, FatalExceptionCount = F, NonFatalExceptionCount = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, ExtractedEmployeeCount = F, LatestExport = F, LatestSentOrAcceptedExportTime = F, EmploymentHireDateLow = F, EmploymentHireDateHigh = F, NewHireDateType = F, searchConditionsGroupType = "And", searchSortFieldNamesList = NULL, searchSortFieldNamesDescendingList = NULL, entityID = 1, schoolYearID = NULL, page = 1, pageSize = 100000, flatten = T, ...){

		params <- as.list(environment()) %>% append(list(...))

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- "all", searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		listSkyObjects(module = "StateReportingMN", objectName = "NewHireExtractRun", schoolYearID = schoolYearID, searchFields = searchFields, page = page, pageSize = pageSize, SearchConditionsList = searchConditionsList, SearchConditionsGroupType = searchConditionsGroupType, SearchSortFieldNamesList = searchSortFieldNamesList, SearchSortFieldNamesDescendingList = searchSortFieldNamesDescendingList, entityId = entityID, flatten = flatten)
	}


	listStateReportingMNNewHireEmployerV1s <- function(searchConditionsList = NULL, NewHireEmployerV1ID = F, NewHireExtractRunID = F, EmployerID = F, FederalEIN = F, EmployerName = F, AddressID = F, AddressLine1 = F, AddressLine2 = F, AddressLine3 = F, City = F, StateCode = F, ZipCode = F, ZipCodePlusFour = F, CountryCode = F, ContactPerson = F, ContactPhoneNumber = F, ContactPhoneExtension = F, AddressIDSecondary = F, SecondaryAddressLine1 = F, SecondaryAddressLine2 = F, SecondaryAddressLine3 = F, SecondaryCity = F, SecondaryStateCode = F, SecondaryZipCode = F, SecondaryZipCodePlusFour = F, SecondaryCountryCode = F, SecondaryContactPerson = F, SecondaryContactPhoneNumber = F, SecondaryContactPhoneExtension = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, searchConditionsGroupType = "And", searchSortFieldNamesList = NULL, searchSortFieldNamesDescendingList = NULL, entityID = 1, schoolYearID = NULL, page = 1, pageSize = 100000, flatten = T, ...){

		params <- as.list(environment()) %>% append(list(...))

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- "all", searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		listSkyObjects(module = "StateReportingMN", objectName = "NewHireEmployerV1", schoolYearID = schoolYearID, searchFields = searchFields, page = page, pageSize = pageSize, SearchConditionsList = searchConditionsList, SearchConditionsGroupType = searchConditionsGroupType, SearchSortFieldNamesList = searchSortFieldNamesList, SearchSortFieldNamesDescendingList = searchSortFieldNamesDescendingList, entityId = entityID, flatten = flatten)
	}


	listNewHireEmployerV1IncludedPayrollRuns <- function(searchConditionsList = NULL, NewHireEmployerV1IncludedPayrollRunID = F, NewHireExtractRunID = F, EmployerID = F, PayrollRunID = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, searchConditionsGroupType = "And", searchSortFieldNamesList = NULL, searchSortFieldNamesDescendingList = NULL, entityID = 1, schoolYearID = NULL, page = 1, pageSize = 100000, flatten = T, ...){

		params <- as.list(environment()) %>% append(list(...))

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- "all", searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		listSkyObjects(module = "StateReportingMN", objectName = "NewHireEmployerV1IncludedPayrollRun", schoolYearID = schoolYearID, searchFields = searchFields, page = page, pageSize = pageSize, SearchConditionsList = searchConditionsList, SearchConditionsGroupType = searchConditionsGroupType, SearchSortFieldNamesList = searchSortFieldNamesList, SearchSortFieldNamesDescendingList = searchSortFieldNamesDescendingList, entityId = entityID, flatten = flatten)
	}


	listWageDetailEmployerV1s <- function(searchConditionsList = NULL, WageDetailEmployerV1ID = F, WageDetailExtractRunID = F, EmployerID = F, UnemploymentInsuranceAccountNumber = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, searchConditionsGroupType = "And", searchSortFieldNamesList = NULL, searchSortFieldNamesDescendingList = NULL, entityID = 1, schoolYearID = NULL, page = 1, pageSize = 100000, flatten = T, ...){

		params <- as.list(environment()) %>% append(list(...))

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- "all", searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		listSkyObjects(module = "StateReportingMN", objectName = "WageDetailEmployerV1", schoolYearID = schoolYearID, searchFields = searchFields, page = page, pageSize = pageSize, SearchConditionsList = searchConditionsList, SearchConditionsGroupType = searchConditionsGroupType, SearchSortFieldNamesList = searchSortFieldNamesList, SearchSortFieldNamesDescendingList = searchSortFieldNamesDescendingList, entityId = entityID, flatten = flatten)
	}


	listWageDetailExceptions <- function(searchConditionsList = NULL, WageDetailExceptionID = F, WageDetailExtractRunID = F, EmployerID = F, EmployeeID = F, UnemploymentInsuranceUnitLocationNumber = F, Source = F, IsFatal = F, Type = F, Message = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, PreserveException = F, searchConditionsGroupType = "And", searchSortFieldNamesList = NULL, searchSortFieldNamesDescendingList = NULL, entityID = 1, schoolYearID = NULL, page = 1, pageSize = 100000, flatten = T, ...){

		params <- as.list(environment()) %>% append(list(...))

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- "all", searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		listSkyObjects(module = "StateReportingMN", objectName = "WageDetailException", schoolYearID = schoolYearID, searchFields = searchFields, page = page, pageSize = pageSize, SearchConditionsList = searchConditionsList, SearchConditionsGroupType = searchConditionsGroupType, SearchSortFieldNamesList = searchSortFieldNamesList, SearchSortFieldNamesDescendingList = searchSortFieldNamesDescendingList, entityId = entityID, flatten = flatten)
	}


	listWageDetailExports <- function(searchConditionsList = NULL, WageDetailExportID = F, WageDetailExtractRunID = F, MediaID = F, Status = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, searchConditionsGroupType = "And", searchSortFieldNamesList = NULL, searchSortFieldNamesDescendingList = NULL, entityID = 1, schoolYearID = NULL, page = 1, pageSize = 100000, flatten = T, ...){

		params <- as.list(environment()) %>% append(list(...))

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- "all", searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		listSkyObjects(module = "StateReportingMN", objectName = "WageDetailExport", schoolYearID = schoolYearID, searchFields = searchFields, page = page, pageSize = pageSize, SearchConditionsList = searchConditionsList, SearchConditionsGroupType = searchConditionsGroupType, SearchSortFieldNamesList = searchSortFieldNamesList, SearchSortFieldNamesDescendingList = searchSortFieldNamesDescendingList, entityId = entityID, flatten = flatten)
	}


	listWageDetailExtractRuns <- function(searchConditionsList = NULL, WageDetailExtractRunID = F, CalendarYear = F, Quarter = F, Version = F, FatalExceptionCount = F, NonFatalExceptionCount = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, ExtractedEmployeeCount = F, LatestExport = F, LatestSentOrAcceptedExportTime = F, searchConditionsGroupType = "And", searchSortFieldNamesList = NULL, searchSortFieldNamesDescendingList = NULL, entityID = 1, schoolYearID = NULL, page = 1, pageSize = 100000, flatten = T, ...){

		params <- as.list(environment()) %>% append(list(...))

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- "all", searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		listSkyObjects(module = "StateReportingMN", objectName = "WageDetailExtractRun", schoolYearID = schoolYearID, searchFields = searchFields, page = page, pageSize = pageSize, SearchConditionsList = searchConditionsList, SearchConditionsGroupType = searchConditionsGroupType, SearchSortFieldNamesList = searchSortFieldNamesList, SearchSortFieldNamesDescendingList = searchSortFieldNamesDescendingList, entityId = entityID, flatten = flatten)
	}


	listWageDetailEmployerIncludedPayrollRuns <- function(searchConditionsList = NULL, WageDetailEmployerIncludedPayrollRunID = F, WageDetailExtractRunID = F, PayrollRunID = F, EmployerID = F, Month = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, searchConditionsGroupType = "And", searchSortFieldNamesList = NULL, searchSortFieldNamesDescendingList = NULL, entityID = 1, schoolYearID = NULL, page = 1, pageSize = 100000, flatten = T, ...){

		params <- as.list(environment()) %>% append(list(...))

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- "all", searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		listSkyObjects(module = "StateReportingMN", objectName = "WageDetailEmployerIncludedPayrollRun", schoolYearID = schoolYearID, searchFields = searchFields, page = page, pageSize = pageSize, SearchConditionsList = searchConditionsList, SearchConditionsGroupType = searchConditionsGroupType, SearchSortFieldNamesList = searchSortFieldNamesList, SearchSortFieldNamesDescendingList = searchSortFieldNamesDescendingList, entityId = entityID, flatten = flatten)
	}


	listWageDetailEmployeeV1s <- function(searchConditionsList = NULL, WageDetailEmployeeV1ID = F, WageDetailEmployerV1ID = F, EmployeeID = F, UnemploymentInsuranceUnitLocation = F, FirstName = F, MiddleInitial = F, LastName = F, SocialSecurityNumber = F, Month1Wages = F, Month2Wages = F, Month3Wages = F, PaidFor12thDayOfMonth1 = F, PaidFor12thDayOfMonth2 = F, PaidFor12thDayOfMonth3 = F, HoursWorked = F, IsOfficer = F, TotalWagesInQuarter = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, PreviousQuarterYTDWages = F, CurrentQuarterYTDWages = F, TaxStateID = F, searchConditionsGroupType = "And", searchSortFieldNamesList = NULL, searchSortFieldNamesDescendingList = NULL, entityID = 1, schoolYearID = NULL, page = 1, pageSize = 100000, flatten = T, ...){

		params <- as.list(environment()) %>% append(list(...))

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- "all", searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		listSkyObjects(module = "StateReportingMN", objectName = "WageDetailEmployeeV1", schoolYearID = schoolYearID, searchFields = searchFields, page = page, pageSize = pageSize, SearchConditionsList = searchConditionsList, SearchConditionsGroupType = searchConditionsGroupType, SearchSortFieldNamesList = searchSortFieldNamesList, SearchSortFieldNamesDescendingList = searchSortFieldNamesDescendingList, entityId = entityID, flatten = flatten)
	}


	listMARSSSchoolFileFallV1s <- function(searchConditionsList = NULL, MARSSSchoolFileFallV1ID = F, SchoolYearID = F, MARSSSubmissionID = F, MARSSSubmissionRunHistoryID = F, EntityID = F, EntitySchoolID = F, SchoolID = F, StateDistrictMNID = F, DistrictCode = F, StateDistrictTypeCodeMNID = F, DistrictType = F, SchoolNumber = F, GradeReferenceID = F, GradeLevelID = F, GradeLevelCode = F, StateTitleISchoolIndicatorCodeMNID = F, TitleIIndicator = F, StateKindergartenScheduleIndicatorCodeMNID = F, KindergartenIndicator = F, InstructionalDays = F, MinutesInSchoolDay = F, HasErrors = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, KeyHash = F, UpdateHash = F, IsCharter = F, searchConditionsGroupType = "And", searchSortFieldNamesList = NULL, searchSortFieldNamesDescendingList = NULL, entityID = 1, schoolYearID = NULL, page = 1, pageSize = 100000, flatten = T, ...){

		params <- as.list(environment()) %>% append(list(...))

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- "all", searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		listSkyObjects(module = "StateReportingMN", objectName = "MARSSSchoolFileFallV1", schoolYearID = schoolYearID, searchFields = searchFields, page = page, pageSize = pageSize, SearchConditionsList = searchConditionsList, SearchConditionsGroupType = searchConditionsGroupType, SearchSortFieldNamesList = searchSortFieldNamesList, SearchSortFieldNamesDescendingList = searchSortFieldNamesDescendingList, entityId = entityID, flatten = flatten)
	}


	listMARSSSchoolFileFallV1Exceptions <- function(searchConditionsList = NULL, MARSSSchoolFileFallV1ExceptionID = F, SchoolYearID = F, EntityID = F, SchoolID = F, MARSSSchoolFileFallV1ID = F, MARSSSubmissionID = F, MARSSSubmissionRunHistoryID = F, MessageType = F, RuleNumber = F, Message = F, ProcessType = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, searchConditionsGroupType = "And", searchSortFieldNamesList = NULL, searchSortFieldNamesDescendingList = NULL, entityID = 1, schoolYearID = NULL, page = 1, pageSize = 100000, flatten = T, ...){

		params <- as.list(environment()) %>% append(list(...))

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- "all", searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		listSkyObjects(module = "StateReportingMN", objectName = "MARSSSchoolFileFallV1Exception", schoolYearID = schoolYearID, searchFields = searchFields, page = page, pageSize = pageSize, SearchConditionsList = searchConditionsList, SearchConditionsGroupType = searchConditionsGroupType, SearchSortFieldNamesList = searchSortFieldNamesList, SearchSortFieldNamesDescendingList = searchSortFieldNamesDescendingList, entityId = entityID, flatten = flatten)
	}


	listMARSSStudentFileFallV1s <- function(searchConditionsList = NULL, MARSSStudentFileFallV1ID = F, SchoolYearID = F, SchoolYearCode = F, EntryWithdrawalID = F, StateStudentNumber = F, SocialSecurityNumber = F, MARSSSubmissionID = F, MARSSSubmissionRunHistoryID = F, EntityID = F, StudentID = F, SchoolID = F, StateDistrictMNID = F, DistrictCode = F, StateDistrictTypeCodeMNID = F, DistrictType = F, SchoolNumber = F, GradeReferenceID = F, GradeLevelID = F, StudentGradeLevelCode = F, StateDistrictMNIDStudent = F, StudentResidentDistrictCode = F, StateDistrictTypeCodeMNIDStudent = F, StudentResidentDistrictType = F, StateAidCategoryCodeMNID = F, StateAidCategoryCode = F, StatusBeginDate = F, StateLastAttendanceLocationCodeMNID = F, LastAttendanceLocation = F, StateStatusEndCodeMNID = F, StatusEndCode = F, StatusEndDate = F, PercentEnrolled = F, AttendanceDays = F, MembershipDays = F, PostSecondaryOption = F, PostSecondaryEducationOptionHours = F, HomeboundService = F, EnrollmentMNID = F, StateEvaluationStatusCodeMNIDSpecialEd = F, SpecialEdEvalStatus = F, StateSpecEdInstructionalSettingCodeMNID = F, SpecialEdInstructionalSetting = F, LimitedEnglishProficiency = F, LEPBeginDate = F, GiftedAndTalented = F, Gender = F, BirthDate = F, LanguageSchoolYearMNID = F, StateLanguageCodeMNID = F, HomePrimaryLanguage = F, DisabilityMNIDPrimary = F, StateDisabilityCodeMNIDPrimary = F, PrimaryDisability = F, StateTransportationCategoryCodeMNID = F, TransportationCategory = F, EconomicIndicatorMNID = F, StateEconomicIndicatorCodeMNID = F, EconomicIndicator = F, TitleIStudentIndicator = F, Homeless = F, StateDistrictMNIDTransportation = F, TransportationResidentDistrictCode = F, StateDistrictTypeCodeMNIDTransportation = F, TransportationResidentDistrictType = F, StateEthnicityRaceCodeMNID = F, EthnicityRaceCode = F, SpecialPupilsCareAndTreatment = F, IndependentStudy = F, SpecialEducationServiceHours = F, OptOutMNID = F, MinnesotaHealthCareOptOut = F, HispanicLatino = F, AmericanIndianAlaskaNative = F, Asian = F, BlackAfricanAmerican = F, NativeHawaiianPacificIslander = F, White = F, LocalUseData = F, StudentFirstName = F, StudentMiddleName = F, StudentLastName = F, Suffix = F, HasErrors = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, KeyHash = F, UpdateHash = F, StudentTransportationID = F, searchConditionsGroupType = "And", searchSortFieldNamesList = NULL, searchSortFieldNamesDescendingList = NULL, entityID = 1, schoolYearID = NULL, page = 1, pageSize = 100000, flatten = T, ...){

		params <- as.list(environment()) %>% append(list(...))

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- "all", searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		listSkyObjects(module = "StateReportingMN", objectName = "MARSSStudentFileFallV1", schoolYearID = schoolYearID, searchFields = searchFields, page = page, pageSize = pageSize, SearchConditionsList = searchConditionsList, SearchConditionsGroupType = searchConditionsGroupType, SearchSortFieldNamesList = searchSortFieldNamesList, SearchSortFieldNamesDescendingList = searchSortFieldNamesDescendingList, entityId = entityID, flatten = flatten)
	}


	listMARSSStudentFileFallV1Exceptions <- function(searchConditionsList = NULL, MARSSStudentFileFallV1ExceptionID = F, SchoolYearID = F, EntityID = F, StudentID = F, MARSSStudentFileFallV1ID = F, MARSSSubmissionID = F, MARSSSubmissionRunHistoryID = F, MessageType = F, RuleNumber = F, Message = F, ProcessType = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, searchConditionsGroupType = "And", searchSortFieldNamesList = NULL, searchSortFieldNamesDescendingList = NULL, entityID = 1, schoolYearID = NULL, page = 1, pageSize = 100000, flatten = T, ...){

		params <- as.list(environment()) %>% append(list(...))

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- "all", searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		listSkyObjects(module = "StateReportingMN", objectName = "MARSSStudentFileFallV1Exception", schoolYearID = schoolYearID, searchFields = searchFields, page = page, pageSize = pageSize, SearchConditionsList = searchConditionsList, SearchConditionsGroupType = searchConditionsGroupType, SearchSortFieldNamesList = searchSortFieldNamesList, SearchSortFieldNamesDescendingList = searchSortFieldNamesDescendingList, entityId = entityID, flatten = flatten)
	}


	listMARSSSubmissionRunHistories <- function(searchConditionsList = NULL, MARSSSubmissionRunHistoryID = F, MARSSSubmissionID = F, StartDateTime = F, EndDateTime = F, Type = F, MediaID = F, IsExport = F, IsLocked = F, Status = F, RunData = F, UserIDCanceledBy = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, ScopeAccessAllowed = F, HasValidMedia = F, CanAccessMedia = F, RunParameters = F, searchConditionsGroupType = "And", searchSortFieldNamesList = NULL, searchSortFieldNamesDescendingList = NULL, entityID = 1, schoolYearID = NULL, page = 1, pageSize = 100000, flatten = T, ...){

		params <- as.list(environment()) %>% append(list(...))

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- "all", searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		listSkyObjects(module = "StateReportingMN", objectName = "MARSSSubmissionRunHistory", schoolYearID = schoolYearID, searchFields = searchFields, page = page, pageSize = pageSize, SearchConditionsList = searchConditionsList, SearchConditionsGroupType = searchConditionsGroupType, SearchSortFieldNamesList = searchSortFieldNamesList, SearchSortFieldNamesDescendingList = searchSortFieldNamesDescendingList, entityId = entityID, flatten = flatten)
	}


	listMARSSSubmissions <- function(searchConditionsList = NULL, MARSSSubmissionID = F, SkywardID = F, Description = F, OpenDate = F, CloseDate = F, SchoolYearID = F, Collection = F, HasAnExport = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, SkywardHash = F, searchConditionsGroupType = "And", searchSortFieldNamesList = NULL, searchSortFieldNamesDescendingList = NULL, entityID = 1, schoolYearID = NULL, page = 1, pageSize = 100000, flatten = T, ...){

		params <- as.list(environment()) %>% append(list(...))

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- "all", searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		listSkyObjects(module = "StateReportingMN", objectName = "MARSSSubmission", schoolYearID = schoolYearID, searchFields = searchFields, page = page, pageSize = pageSize, SearchConditionsList = searchConditionsList, SearchConditionsGroupType = searchConditionsGroupType, SearchSortFieldNamesList = searchSortFieldNamesList, SearchSortFieldNamesDescendingList = searchSortFieldNamesDescendingList, entityId = entityID, flatten = flatten)
	}


	listStateDisabilityCodeMNS <- function(searchConditionsList = NULL, StateDisabilityCodeMNID = F, Code = F, Description = F, CodeDescription = F, ValidYearLow = F, ValidYearHigh = F, IsInvalid = F, SkywardID = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, SkywardHash = F, searchConditionsGroupType = "And", searchSortFieldNamesList = NULL, searchSortFieldNamesDescendingList = NULL, entityID = 1, schoolYearID = NULL, page = 1, pageSize = 100000, flatten = T, ...){

		params <- as.list(environment()) %>% append(list(...))

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- "all", searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		listSkyObjects(module = "StateReportingMN", objectName = "StateDisabilityCodeMN", schoolYearID = schoolYearID, searchFields = searchFields, page = page, pageSize = pageSize, SearchConditionsList = searchConditionsList, SearchConditionsGroupType = searchConditionsGroupType, SearchSortFieldNamesList = searchSortFieldNamesList, SearchSortFieldNamesDescendingList = searchSortFieldNamesDescendingList, entityId = entityID, flatten = flatten)
	}


	listStateEthnicityRaceCodeMNS <- function(searchConditionsList = NULL, StateEthnicityRaceCodeMNID = F, Code = F, Description = F, CodeDescription = F, IsInvalidFinHR = F, ValidYearLow = F, ValidYearHigh = F, IsInvalid = F, SkywardID = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, SkywardHash = F, searchConditionsGroupType = "And", searchSortFieldNamesList = NULL, searchSortFieldNamesDescendingList = NULL, entityID = 1, schoolYearID = NULL, page = 1, pageSize = 100000, flatten = T, ...){

		params <- as.list(environment()) %>% append(list(...))

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- "all", searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		listSkyObjects(module = "StateReportingMN", objectName = "StateEthnicityRaceCodeMN", schoolYearID = schoolYearID, searchFields = searchFields, page = page, pageSize = pageSize, SearchConditionsList = searchConditionsList, SearchConditionsGroupType = searchConditionsGroupType, SearchSortFieldNamesList = searchSortFieldNamesList, SearchSortFieldNamesDescendingList = searchSortFieldNamesDescendingList, entityId = entityID, flatten = flatten)
	}


	listStateEvaluationStatusCodeMNS <- function(searchConditionsList = NULL, StateEvaluationStatusCodeMNID = F, Code = F, Description = F, CodeDescription = F, ValidYearLow = F, ValidYearHigh = F, IsInvalid = F, SkywardID = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, SkywardHash = F, searchConditionsGroupType = "And", searchSortFieldNamesList = NULL, searchSortFieldNamesDescendingList = NULL, entityID = 1, schoolYearID = NULL, page = 1, pageSize = 100000, flatten = T, ...){

		params <- as.list(environment()) %>% append(list(...))

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- "all", searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		listSkyObjects(module = "StateReportingMN", objectName = "StateEvaluationStatusCodeMN", schoolYearID = schoolYearID, searchFields = searchFields, page = page, pageSize = pageSize, SearchConditionsList = searchConditionsList, SearchConditionsGroupType = searchConditionsGroupType, SearchSortFieldNamesList = searchSortFieldNamesList, SearchSortFieldNamesDescendingList = searchSortFieldNamesDescendingList, entityId = entityID, flatten = flatten)
	}


	listStateEconomicIndicatorCodeMNS <- function(searchConditionsList = NULL, StateEconomicIndicatorCodeMNID = F, Rank = F, Code = F, Description = F, CodeDescription = F, ValidYearLow = F, ValidYearHigh = F, IsInvalid = F, SkywardID = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, SkywardHash = F, searchConditionsGroupType = "And", searchSortFieldNamesList = NULL, searchSortFieldNamesDescendingList = NULL, entityID = 1, schoolYearID = NULL, page = 1, pageSize = 100000, flatten = T, ...){

		params <- as.list(environment()) %>% append(list(...))

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- "all", searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		listSkyObjects(module = "StateReportingMN", objectName = "StateEconomicIndicatorCodeMN", schoolYearID = schoolYearID, searchFields = searchFields, page = page, pageSize = pageSize, SearchConditionsList = searchConditionsList, SearchConditionsGroupType = searchConditionsGroupType, SearchSortFieldNamesList = searchSortFieldNamesList, SearchSortFieldNamesDescendingList = searchSortFieldNamesDescendingList, entityId = entityID, flatten = flatten)
	}


	listStateAidCategoryCodeMNS <- function(searchConditionsList = NULL, StateAidCategoryCodeMNID = F, Code = F, Description = F, StateAid = F, CodeDescription = F, ValidYearLow = F, ValidYearHigh = F, IsInvalid = F, SkywardID = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, SkywardHash = F, searchConditionsGroupType = "And", searchSortFieldNamesList = NULL, searchSortFieldNamesDescendingList = NULL, entityID = 1, schoolYearID = NULL, page = 1, pageSize = 100000, flatten = T, ...){

		params <- as.list(environment()) %>% append(list(...))

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- "all", searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		listSkyObjects(module = "StateReportingMN", objectName = "StateAidCategoryCodeMN", schoolYearID = schoolYearID, searchFields = searchFields, page = page, pageSize = pageSize, SearchConditionsList = searchConditionsList, SearchConditionsGroupType = searchConditionsGroupType, SearchSortFieldNamesList = searchSortFieldNamesList, SearchSortFieldNamesDescendingList = searchSortFieldNamesDescendingList, entityId = entityID, flatten = flatten)
	}


	listStateDistrictTypeCodeMNS <- function(searchConditionsList = NULL, StateDistrictTypeCodeMNID = F, Code = F, Description = F, IsResidentDistrict = F, CodeDescription = F, IsInvalidFINHR = F, ValidYearLow = F, ValidYearHigh = F, IsInvalid = F, SkywardID = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, SkywardHash = F, searchConditionsGroupType = "And", searchSortFieldNamesList = NULL, searchSortFieldNamesDescendingList = NULL, entityID = 1, schoolYearID = NULL, page = 1, pageSize = 100000, flatten = T, ...){

		params <- as.list(environment()) %>% append(list(...))

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- "all", searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		listSkyObjects(module = "StateReportingMN", objectName = "StateDistrictTypeCodeMN", schoolYearID = schoolYearID, searchFields = searchFields, page = page, pageSize = pageSize, SearchConditionsList = searchConditionsList, SearchConditionsGroupType = searchConditionsGroupType, SearchSortFieldNamesList = searchSortFieldNamesList, SearchSortFieldNamesDescendingList = searchSortFieldNamesDescendingList, entityId = entityID, flatten = flatten)
	}


	listStateKindergartenScheduleIndicatorCodeMNS <- function(searchConditionsList = NULL, StateKindergartenScheduleIndicatorCodeMNID = F, Code = F, Description = F, CodeDescription = F, ValidYearLow = F, ValidYearHigh = F, IsInvalid = F, SkywardID = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, SkywardHash = F, searchConditionsGroupType = "And", searchSortFieldNamesList = NULL, searchSortFieldNamesDescendingList = NULL, entityID = 1, schoolYearID = NULL, page = 1, pageSize = 100000, flatten = T, ...){

		params <- as.list(environment()) %>% append(list(...))

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- "all", searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		listSkyObjects(module = "StateReportingMN", objectName = "StateKindergartenScheduleIndicatorCodeMN", schoolYearID = schoolYearID, searchFields = searchFields, page = page, pageSize = pageSize, SearchConditionsList = searchConditionsList, SearchConditionsGroupType = searchConditionsGroupType, SearchSortFieldNamesList = searchSortFieldNamesList, SearchSortFieldNamesDescendingList = searchSortFieldNamesDescendingList, entityId = entityID, flatten = flatten)
	}


	listStateLanguageCodeMNS <- function(searchConditionsList = NULL, StateLanguageCodeMNID = F, Code = F, Description = F, Country = F, CodeDescription = F, ValidYearLow = F, ValidYearHigh = F, IsInvalid = F, SkywardID = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, SkywardHash = F, searchConditionsGroupType = "And", searchSortFieldNamesList = NULL, searchSortFieldNamesDescendingList = NULL, entityID = 1, schoolYearID = NULL, page = 1, pageSize = 100000, flatten = T, ...){

		params <- as.list(environment()) %>% append(list(...))

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- "all", searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		listSkyObjects(module = "StateReportingMN", objectName = "StateLanguageCodeMN", schoolYearID = schoolYearID, searchFields = searchFields, page = page, pageSize = pageSize, SearchConditionsList = searchConditionsList, SearchConditionsGroupType = searchConditionsGroupType, SearchSortFieldNamesList = searchSortFieldNamesList, SearchSortFieldNamesDescendingList = searchSortFieldNamesDescendingList, entityId = entityID, flatten = flatten)
	}


	listStateSpecEdInstructionalSettingCodeMNS <- function(searchConditionsList = NULL, StateSpecEdInstructionalSettingCodeMNID = F, Code = F, Description = F, CodeDescription = F, ValidYearLow = F, ValidYearHigh = F, IsInvalid = F, SkywardID = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, SkywardHash = F, searchConditionsGroupType = "And", searchSortFieldNamesList = NULL, searchSortFieldNamesDescendingList = NULL, entityID = 1, schoolYearID = NULL, page = 1, pageSize = 100000, flatten = T, ...){

		params <- as.list(environment()) %>% append(list(...))

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- "all", searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		listSkyObjects(module = "StateReportingMN", objectName = "StateSpecEdInstructionalSettingCodeMN", schoolYearID = schoolYearID, searchFields = searchFields, page = page, pageSize = pageSize, SearchConditionsList = searchConditionsList, SearchConditionsGroupType = searchConditionsGroupType, SearchSortFieldNamesList = searchSortFieldNamesList, SearchSortFieldNamesDescendingList = searchSortFieldNamesDescendingList, entityId = entityID, flatten = flatten)
	}


	listStateStatusEndCodeMNS <- function(searchConditionsList = NULL, StateStatusEndCodeMNID = F, Code = F, Description = F, CodeDescription = F, ValidYearLow = F, ValidYearHigh = F, IsInvalid = F, SkywardID = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, SkywardHash = F, searchConditionsGroupType = "And", searchSortFieldNamesList = NULL, searchSortFieldNamesDescendingList = NULL, entityID = 1, schoolYearID = NULL, page = 1, pageSize = 100000, flatten = T, ...){

		params <- as.list(environment()) %>% append(list(...))

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- "all", searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		listSkyObjects(module = "StateReportingMN", objectName = "StateStatusEndCodeMN", schoolYearID = schoolYearID, searchFields = searchFields, page = page, pageSize = pageSize, SearchConditionsList = searchConditionsList, SearchConditionsGroupType = searchConditionsGroupType, SearchSortFieldNamesList = searchSortFieldNamesList, SearchSortFieldNamesDescendingList = searchSortFieldNamesDescendingList, entityId = entityID, flatten = flatten)
	}


	listStateTitleISchoolIndicatorCodeMNS <- function(searchConditionsList = NULL, StateTitleISchoolIndicatorCodeMNID = F, Code = F, Description = F, CodeDescription = F, ValidYearLow = F, ValidYearHigh = F, IsInvalid = F, SkywardID = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, SkywardHash = F, searchConditionsGroupType = "And", searchSortFieldNamesList = NULL, searchSortFieldNamesDescendingList = NULL, entityID = 1, schoolYearID = NULL, page = 1, pageSize = 100000, flatten = T, ...){

		params <- as.list(environment()) %>% append(list(...))

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- "all", searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		listSkyObjects(module = "StateReportingMN", objectName = "StateTitleISchoolIndicatorCodeMN", schoolYearID = schoolYearID, searchFields = searchFields, page = page, pageSize = pageSize, SearchConditionsList = searchConditionsList, SearchConditionsGroupType = searchConditionsGroupType, SearchSortFieldNamesList = searchSortFieldNamesList, SearchSortFieldNamesDescendingList = searchSortFieldNamesDescendingList, entityId = entityID, flatten = flatten)
	}


	listStateLastAttendanceLocationCodeMNS <- function(searchConditionsList = NULL, StateLastAttendanceLocationCodeMNID = F, Code = F, Description = F, CodeDescription = F, ValidYearLow = F, ValidYearHigh = F, IsInvalid = F, SkywardID = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, SkywardHash = F, searchConditionsGroupType = "And", searchSortFieldNamesList = NULL, searchSortFieldNamesDescendingList = NULL, entityID = 1, schoolYearID = NULL, page = 1, pageSize = 100000, flatten = T, ...){

		params <- as.list(environment()) %>% append(list(...))

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- "all", searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		listSkyObjects(module = "StateReportingMN", objectName = "StateLastAttendanceLocationCodeMN", schoolYearID = schoolYearID, searchFields = searchFields, page = page, pageSize = pageSize, SearchConditionsList = searchConditionsList, SearchConditionsGroupType = searchConditionsGroupType, SearchSortFieldNamesList = searchSortFieldNamesList, SearchSortFieldNamesDescendingList = searchSortFieldNamesDescendingList, entityId = entityID, flatten = flatten)
	}


	listStateTransportationCategoryCodeMNS <- function(searchConditionsList = NULL, StateTransportationCategoryCodeMNID = F, Code = F, Description = F, IsTransported = F, CodeDescription = F, ValidYearLow = F, ValidYearHigh = F, IsInvalid = F, SkywardID = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, SkywardHash = F, searchConditionsGroupType = "And", searchSortFieldNamesList = NULL, searchSortFieldNamesDescendingList = NULL, entityID = 1, schoolYearID = NULL, page = 1, pageSize = 100000, flatten = T, ...){

		params <- as.list(environment()) %>% append(list(...))

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- "all", searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		listSkyObjects(module = "StateReportingMN", objectName = "StateTransportationCategoryCodeMN", schoolYearID = schoolYearID, searchFields = searchFields, page = page, pageSize = pageSize, SearchConditionsList = searchConditionsList, SearchConditionsGroupType = searchConditionsGroupType, SearchSortFieldNamesList = searchSortFieldNamesList, SearchSortFieldNamesDescendingList = searchSortFieldNamesDescendingList, entityId = entityID, flatten = flatten)
	}


	listPERAExports <- function(searchConditionsList = NULL, PERAExportID = F, PERAExtractRunID = F, MediaID = F, EmployerNumber = F, PaidDate = F, Status = F, ExportType = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, searchConditionsGroupType = "And", searchSortFieldNamesList = NULL, searchSortFieldNamesDescendingList = NULL, entityID = 1, schoolYearID = NULL, page = 1, pageSize = 100000, flatten = T, ...){

		params <- as.list(environment()) %>% append(list(...))

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- "all", searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		listSkyObjects(module = "StateReportingMN", objectName = "PERAExport", schoolYearID = schoolYearID, searchFields = searchFields, page = page, pageSize = pageSize, SearchConditionsList = searchConditionsList, SearchConditionsGroupType = searchConditionsGroupType, SearchSortFieldNamesList = searchSortFieldNamesList, SearchSortFieldNamesDescendingList = searchSortFieldNamesDescendingList, entityId = entityID, flatten = flatten)
	}


	listPERAExtractEmployeeContributionV1s <- function(searchConditionsList = NULL, PERAExtractEmployeeContributionV1ID = F, PERAExtractRunID = F, EmployeeID = F, DistrictID = F, SocialSecurityNumber = F, FirstName = F, MiddleInitial = F, LastName = F, TitleAfterName = F, PayrollStartDate = F, PayrollEndDate = F, PaidDate = F, MemberAmount = F, EligibleEarnings = F, EmployerAmount = F, SchoolFiscalYear = F, CompensatedHours = F, OvertimePay = F, StatePERAPayTypeMNID = F, StatePERAPayTypeMNValue = F, StateRetirementAssociationTypeMNIDPlan = F, StateRetirementAssociationTypePlanValue = F, AdjustmentIndicator = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, searchConditionsGroupType = "And", searchSortFieldNamesList = NULL, searchSortFieldNamesDescendingList = NULL, entityID = 1, schoolYearID = NULL, page = 1, pageSize = 100000, flatten = T, ...){

		params <- as.list(environment()) %>% append(list(...))

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- "all", searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		listSkyObjects(module = "StateReportingMN", objectName = "PERAExtractEmployeeContributionV1", schoolYearID = schoolYearID, searchFields = searchFields, page = page, pageSize = pageSize, SearchConditionsList = searchConditionsList, SearchConditionsGroupType = searchConditionsGroupType, SearchSortFieldNamesList = searchSortFieldNamesList, SearchSortFieldNamesDescendingList = searchSortFieldNamesDescendingList, entityId = entityID, flatten = flatten)
	}


	listPERADemographicDetailV1s <- function(searchConditionsList = NULL, PERADemographicDetailV1ID = F, PERAExtractRunID = F, EmployeeID = F, DistrictID = F, EmployerNumber = F, StateRetirementAssociationTypeMNIDPlan = F, StateRetirementAssociationTypeMNPlanValue = F, SocialSecurityNumber = F, LastName = F, FirstName = F, MiddleInitial = F, TitleFollowingName = F, MostRecentHireDate = F, EligibilityDate = F, StatePERAExclusionCodeMNID = F, StatePERAExclusionCodeMNValue = F, StatePERAMemberEmploymentStatusMNID = F, StatePERAMemberEmploymentStatusMNValue = F, MemberEmploymentStatusEffectiveDate = F, StatePERAPositionCodeMNID = F, StatePERAPositionCodeMNValue = F, StatePERAPositionClassMNID = F, StatePERAPositionClassMNValue = F, JobTitle = F, BirthLastName = F, Sex = F, BirthDate = F, AddressID = F, AddressAttentionTo = F, AddressLine1 = F, AddressLine2 = F, City = F, State = F, ZipCode = F, ZipCodePlusFour = F, ExcludeFromExport = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, searchConditionsGroupType = "And", searchSortFieldNamesList = NULL, searchSortFieldNamesDescendingList = NULL, entityID = 1, schoolYearID = NULL, page = 1, pageSize = 100000, flatten = T, ...){

		params <- as.list(environment()) %>% append(list(...))

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- "all", searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		listSkyObjects(module = "StateReportingMN", objectName = "PERADemographicDetailV1", schoolYearID = schoolYearID, searchFields = searchFields, page = page, pageSize = pageSize, SearchConditionsList = searchConditionsList, SearchConditionsGroupType = searchConditionsGroupType, SearchSortFieldNamesList = searchSortFieldNamesList, SearchSortFieldNamesDescendingList = searchSortFieldNamesDescendingList, entityId = entityID, flatten = flatten)
	}


	listPERAExtractRunExceptions <- function(searchConditionsList = NULL, PERAExtractRunExceptionID = F, PERAExtractRunID = F, EmployeeID = F, DistrictID = F, PERAExtractEmployeeContributionV1ID = F, PERADemographicDetailV1ID = F, Message = F, IsFatal = F, Type = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, PreserveException = F, searchConditionsGroupType = "And", searchSortFieldNamesList = NULL, searchSortFieldNamesDescendingList = NULL, entityID = 1, schoolYearID = NULL, page = 1, pageSize = 100000, flatten = T, ...){

		params <- as.list(environment()) %>% append(list(...))

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- "all", searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		listSkyObjects(module = "StateReportingMN", objectName = "PERAExtractRunException", schoolYearID = schoolYearID, searchFields = searchFields, page = page, pageSize = pageSize, SearchConditionsList = searchConditionsList, SearchConditionsGroupType = searchConditionsGroupType, SearchSortFieldNamesList = searchSortFieldNamesList, SearchSortFieldNamesDescendingList = searchSortFieldNamesDescendingList, entityId = entityID, flatten = flatten)
	}


	listPERAExtractRunIncludedPayrollRuns <- function(searchConditionsList = NULL, PERAExtractRunIncludedPayrollRunID = F, PERAExtractRunID = F, PayrollRunID = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, searchConditionsGroupType = "And", searchSortFieldNamesList = NULL, searchSortFieldNamesDescendingList = NULL, entityID = 1, schoolYearID = NULL, page = 1, pageSize = 100000, flatten = T, ...){

		params <- as.list(environment()) %>% append(list(...))

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- "all", searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		listSkyObjects(module = "StateReportingMN", objectName = "PERAExtractRunIncludedPayrollRun", schoolYearID = schoolYearID, searchFields = searchFields, page = page, pageSize = pageSize, SearchConditionsList = searchConditionsList, SearchConditionsGroupType = searchConditionsGroupType, SearchSortFieldNamesList = searchSortFieldNamesList, SearchSortFieldNamesDescendingList = searchSortFieldNamesDescendingList, entityId = entityID, flatten = flatten)
	}


	listPERAExtractRuns <- function(searchConditionsList = NULL, PERAExtractRunID = F, EntityID = F, Description = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, ExtractedContributionFileCount = F, ExtractedDemographicFileCount = F, LatestDemographicExport = F, LatestContributionExport = F, Version = F, FatalExceptionCount = F, NonFatalExceptionCount = F, LatestExport = F, LatestSentOrAcceptedExportTime = F, searchConditionsGroupType = "And", searchSortFieldNamesList = NULL, searchSortFieldNamesDescendingList = NULL, entityID = 1, schoolYearID = NULL, page = 1, pageSize = 100000, flatten = T, ...){

		params <- as.list(environment()) %>% append(list(...))

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- "all", searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		listSkyObjects(module = "StateReportingMN", objectName = "PERAExtractRun", schoolYearID = schoolYearID, searchFields = searchFields, page = page, pageSize = pageSize, SearchConditionsList = searchConditionsList, SearchConditionsGroupType = searchConditionsGroupType, SearchSortFieldNamesList = searchSortFieldNamesList, SearchSortFieldNamesDescendingList = searchSortFieldNamesDescendingList, entityId = entityID, flatten = flatten)
	}


	listStateTransactionTypeMNS <- function(searchConditionsList = NULL, StateTransactionTypeMNID = F, Code = F, Description = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, SkywardID = F, SkywardHash = F, ValidYearLow = F, ValidYearHigh = F, IsInvalid = F, searchConditionsGroupType = "And", searchSortFieldNamesList = NULL, searchSortFieldNamesDescendingList = NULL, entityID = 1, schoolYearID = NULL, page = 1, pageSize = 100000, flatten = T, ...){

		params <- as.list(environment()) %>% append(list(...))

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- "all", searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		listSkyObjects(module = "StateReportingMN", objectName = "StateTransactionTypeMN", schoolYearID = schoolYearID, searchFields = searchFields, page = page, pageSize = pageSize, SearchConditionsList = searchConditionsList, SearchConditionsGroupType = searchConditionsGroupType, SearchSortFieldNamesList = searchSortFieldNamesList, SearchSortFieldNamesDescendingList = searchSortFieldNamesDescendingList, entityId = entityID, flatten = flatten)
	}


	listBudgetPublicationExports <- function(searchConditionsList = NULL, BudgetPublicationExportID = F, BudgetPublicationExtractRunID = F, MediaID = F, Status = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, searchConditionsGroupType = "And", searchSortFieldNamesList = NULL, searchSortFieldNamesDescendingList = NULL, entityID = 1, schoolYearID = NULL, page = 1, pageSize = 100000, flatten = T, ...){

		params <- as.list(environment()) %>% append(list(...))

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- "all", searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		listSkyObjects(module = "StateReportingMN", objectName = "BudgetPublicationExport", schoolYearID = schoolYearID, searchFields = searchFields, page = page, pageSize = pageSize, SearchConditionsList = searchConditionsList, SearchConditionsGroupType = searchConditionsGroupType, SearchSortFieldNamesList = searchSortFieldNamesList, SearchSortFieldNamesDescendingList = searchSortFieldNamesDescendingList, entityId = entityID, flatten = flatten)
	}


	listMARSSSchoolFileEOYV1Exceptions <- function(searchConditionsList = NULL, MARSSSchoolFileEOYV1ExceptionID = F, SchoolYearID = F, EntityID = F, SchoolID = F, MARSSSchoolFileEOYV1ID = F, MARSSSubmissionID = F, MARSSSubmissionRunHistoryID = F, MessageType = F, RuleNumber = F, Message = F, ProcessType = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, searchConditionsGroupType = "And", searchSortFieldNamesList = NULL, searchSortFieldNamesDescendingList = NULL, entityID = 1, schoolYearID = NULL, page = 1, pageSize = 100000, flatten = T, ...){

		params <- as.list(environment()) %>% append(list(...))

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- "all", searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		listSkyObjects(module = "StateReportingMN", objectName = "MARSSSchoolFileEOYV1Exception", schoolYearID = schoolYearID, searchFields = searchFields, page = page, pageSize = pageSize, SearchConditionsList = searchConditionsList, SearchConditionsGroupType = searchConditionsGroupType, SearchSortFieldNamesList = searchSortFieldNamesList, SearchSortFieldNamesDescendingList = searchSortFieldNamesDescendingList, entityId = entityID, flatten = flatten)
	}


	listMARSSStudentFileEOYV1Exceptions <- function(searchConditionsList = NULL, MARSSStudentFileEOYV1ExceptionID = F, SchoolYearID = F, EntityID = F, StudentID = F, MARSSStudentFileEOYV1ID = F, MARSSSubmissionID = F, MARSSSubmissionRunHistoryID = F, MessageType = F, RuleNumber = F, Message = F, ProcessType = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, searchConditionsGroupType = "And", searchSortFieldNamesList = NULL, searchSortFieldNamesDescendingList = NULL, entityID = 1, schoolYearID = NULL, page = 1, pageSize = 100000, flatten = T, ...){

		params <- as.list(environment()) %>% append(list(...))

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- "all", searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		listSkyObjects(module = "StateReportingMN", objectName = "MARSSStudentFileEOYV1Exception", schoolYearID = schoolYearID, searchFields = searchFields, page = page, pageSize = pageSize, SearchConditionsList = searchConditionsList, SearchConditionsGroupType = searchConditionsGroupType, SearchSortFieldNamesList = searchSortFieldNamesList, SearchSortFieldNamesDescendingList = searchSortFieldNamesDescendingList, entityId = entityID, flatten = flatten)
	}


	listMARSSSchoolFileEOYV1s <- function(searchConditionsList = NULL, MARSSSchoolFileEOYV1ID = F, SchoolYearID = F, MARSSSubmissionID = F, MARSSSubmissionRunHistoryID = F, EntityID = F, EntitySchoolID = F, SchoolID = F, StateDistrictMNID = F, DistrictCode = F, StateDistrictTypeCodeMNID = F, DistrictType = F, SchoolNumber = F, GradeReferenceID = F, GradeLevelID = F, GradeLevelCode = F, StateTitleISchoolIndicatorCodeMNID = F, TitleIIndicator = F, StateKindergartenScheduleIndicatorCodeMNID = F, KindergartenIndicator = F, InstructionalDays = F, MinutesInSchoolDay = F, HasErrors = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, KeyHash = F, UpdateHash = F, IsCharter = F, searchConditionsGroupType = "And", searchSortFieldNamesList = NULL, searchSortFieldNamesDescendingList = NULL, entityID = 1, schoolYearID = NULL, page = 1, pageSize = 100000, flatten = T, ...){

		params <- as.list(environment()) %>% append(list(...))

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- "all", searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		listSkyObjects(module = "StateReportingMN", objectName = "MARSSSchoolFileEOYV1", schoolYearID = schoolYearID, searchFields = searchFields, page = page, pageSize = pageSize, SearchConditionsList = searchConditionsList, SearchConditionsGroupType = searchConditionsGroupType, SearchSortFieldNamesList = searchSortFieldNamesList, SearchSortFieldNamesDescendingList = searchSortFieldNamesDescendingList, entityId = entityID, flatten = flatten)
	}


	listMARSSStudentFileEOYV1s <- function(searchConditionsList = NULL, MARSSStudentFileEOYV1ID = F, SchoolYearID = F, SchoolYearCode = F, EntryWithdrawalID = F, StateStudentNumber = F, SocialSecurityNumber = F, MARSSSubmissionID = F, MARSSSubmissionRunHistoryID = F, EntityID = F, StudentID = F, SchoolID = F, StateDistrictMNID = F, DistrictCode = F, StateDistrictTypeCodeMNID = F, DistrictType = F, SchoolNumber = F, GradeReferenceID = F, GradeLevelID = F, StudentGradeLevelCode = F, StateDistrictMNIDStudent = F, StudentResidentDistrictCode = F, StateDistrictTypeCodeMNIDStudent = F, StudentResidentDistrictType = F, StateAidCategoryCodeMNID = F, StateAidCategoryCode = F, StatusBeginDate = F, StateLastAttendanceLocationCodeMNID = F, LastAttendanceLocation = F, StateStatusEndCodeMNID = F, StatusEndCode = F, StatusEndDate = F, PercentEnrolled = F, AttendanceDays = F, MembershipDays = F, PostSecondaryOption = F, PostSecondaryEducationOptionHours = F, HomeboundServiceCode = F, EnrollmentMNID = F, StateEvaluationStatusCodeMNIDSpecialEd = F, SpecialEdEvalStatus = F, StateSpecEdInstructionalSettingCodeMNID = F, SpecialEdInstructionalSetting = F, LimitedEnglishProficiencyCode = F, IsReceivingServices = F, LEPBeginDate = F, GiftedAndTalentedCode = F, Gender = F, BirthDate = F, LanguageSchoolYearMNID = F, StateLanguageCodeMNID = F, HomePrimaryLanguage = F, DisabilityMNIDPrimary = F, StateDisabilityCodeMNIDPrimary = F, PrimaryDisability = F, StateTransportationCategoryCodeMNID = F, TransportationCategory = F, EconomicIndicatorMNID = F, StateEconomicIndicatorCodeMNID = F, EconomicIndicator = F, TitleIStudentIndicatorCode = F, HomelessCode = F, StateDistrictMNIDTransportation = F, TransportationResidentDistrictCode = F, StateDistrictTypeCodeMNIDTransportation = F, TransportationResidentDistrictType = F, StateEthnicityRaceCodeMNID = F, EthnicityRaceCode = F, SpecialPupilsCareAndTreatmentCode = F, IndependentStudyCode = F, SpecialEducationServiceHours = F, OptOutMNID = F, MinnesotaHealthCareOptOutCode = F, HispanicLatino = F, AmericanIndianAlaskaNative = F, Asian = F, BlackAfricanAmerican = F, NativeHawaiianPacificIslander = F, White = F, IsPSEOConcurrentEnrollment = F, LocalUseData = F, StudentFirstName = F, StudentMiddleName = F, StudentLastName = F, Suffix = F, HasErrors = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, KeyHash = F, UpdateHash = F, StudentTransportationID = F, searchConditionsGroupType = "And", searchSortFieldNamesList = NULL, searchSortFieldNamesDescendingList = NULL, entityID = 1, schoolYearID = NULL, page = 1, pageSize = 100000, flatten = T, ...){

		params <- as.list(environment()) %>% append(list(...))

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- "all", searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		listSkyObjects(module = "StateReportingMN", objectName = "MARSSStudentFileEOYV1", schoolYearID = schoolYearID, searchFields = searchFields, page = page, pageSize = pageSize, SearchConditionsList = searchConditionsList, SearchConditionsGroupType = searchConditionsGroupType, SearchSortFieldNamesList = searchSortFieldNamesList, SearchSortFieldNamesDescendingList = searchSortFieldNamesDescendingList, entityId = entityID, flatten = flatten)
	}


	listSTARStudentFileV1s <- function(searchConditionsList = NULL, STARStudentFileV1ID = F, EntityID = F, SchoolYearID = F, CourseID = F, SectionID = F, StaffID = F, SchoolID = F, STARStudentSubmissionID = F, STARStudentSubmissionRunHistoryID = F, CurriculumYearID = F, StateDistrictMNID = F, DistrictNumber = F, StateDistrictTypeCodeMNID = F, DistrictType = F, FileFolderNumber = F, SchoolNumber = F, StateSTARAssignmentCodeMNID = F, AssignmentCode = F, StateSTARGradeLevelMNID = F, GradeLevel = F, StateSTARModeOfTeachingMNID = F, TeachingMode = F, PeriodsPerWeek = F, LengthOfPeriod = F, NumberOfPupils = F, HasErrors = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, KeyHash = F, UpdateHash = F, searchConditionsGroupType = "And", searchSortFieldNamesList = NULL, searchSortFieldNamesDescendingList = NULL, entityID = 1, schoolYearID = NULL, page = 1, pageSize = 100000, flatten = T, ...){

		params <- as.list(environment()) %>% append(list(...))

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- "all", searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		listSkyObjects(module = "StateReportingMN", objectName = "STARStudentFileV1", schoolYearID = schoolYearID, searchFields = searchFields, page = page, pageSize = pageSize, SearchConditionsList = searchConditionsList, SearchConditionsGroupType = searchConditionsGroupType, SearchSortFieldNamesList = searchSortFieldNamesList, SearchSortFieldNamesDescendingList = searchSortFieldNamesDescendingList, entityId = entityID, flatten = flatten)
	}


	listSTARStudentFileV1Exceptions <- function(searchConditionsList = NULL, STARStudentFileV1ExceptionID = F, EntityID = F, SchoolYearID = F, CourseID = F, SectionID = F, StaffID = F, SchoolID = F, STARStudentSubmissionID = F, STARStudentSubmissionRunHistoryID = F, STARStudentFileV1ID = F, MessageType = F, RuleNumber = F, Message = F, LinkedToStaff = F, ProcessType = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, searchConditionsGroupType = "And", searchSortFieldNamesList = NULL, searchSortFieldNamesDescendingList = NULL, entityID = 1, schoolYearID = NULL, page = 1, pageSize = 100000, flatten = T, ...){

		params <- as.list(environment()) %>% append(list(...))

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- "all", searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		listSkyObjects(module = "StateReportingMN", objectName = "STARStudentFileV1Exception", schoolYearID = schoolYearID, searchFields = searchFields, page = page, pageSize = pageSize, SearchConditionsList = searchConditionsList, SearchConditionsGroupType = searchConditionsGroupType, SearchSortFieldNamesList = searchSortFieldNamesList, SearchSortFieldNamesDescendingList = searchSortFieldNamesDescendingList, entityId = entityID, flatten = flatten)
	}


	listSTARStudentSubmissions <- function(searchConditionsList = NULL, STARStudentSubmissionID = F, SkywardID = F, Description = F, OpenDate = F, CloseDate = F, SchoolYearID = F, SnapshotDate = F, SkywardHash = F, HasAnExport = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, searchConditionsGroupType = "And", searchSortFieldNamesList = NULL, searchSortFieldNamesDescendingList = NULL, entityID = 1, schoolYearID = NULL, page = 1, pageSize = 100000, flatten = T, ...){

		params <- as.list(environment()) %>% append(list(...))

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- "all", searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		listSkyObjects(module = "StateReportingMN", objectName = "STARStudentSubmission", schoolYearID = schoolYearID, searchFields = searchFields, page = page, pageSize = pageSize, SearchConditionsList = searchConditionsList, SearchConditionsGroupType = searchConditionsGroupType, SearchSortFieldNamesList = searchSortFieldNamesList, SearchSortFieldNamesDescendingList = searchSortFieldNamesDescendingList, entityId = entityID, flatten = flatten)
	}


	listSTARStudentSubmissionRunHistories <- function(searchConditionsList = NULL, STARStudentSubmissionRunHistoryID = F, STARStudentSubmissionID = F, StartDateTime = F, EndDateTime = F, Type = F, MediaID = F, IsExport = F, IsLocked = F, Status = F, RunData = F, UserIDCanceledBy = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, ScopeAccessAllowed = F, HasValidMedia = F, CanAccessMedia = F, RunParameters = F, searchConditionsGroupType = "And", searchSortFieldNamesList = NULL, searchSortFieldNamesDescendingList = NULL, entityID = 1, schoolYearID = NULL, page = 1, pageSize = 100000, flatten = T, ...){

		params <- as.list(environment()) %>% append(list(...))

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- "all", searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		listSkyObjects(module = "StateReportingMN", objectName = "STARStudentSubmissionRunHistory", schoolYearID = schoolYearID, searchFields = searchFields, page = page, pageSize = pageSize, SearchConditionsList = searchConditionsList, SearchConditionsGroupType = searchConditionsGroupType, SearchSortFieldNamesList = searchSortFieldNamesList, SearchSortFieldNamesDescendingList = searchSortFieldNamesDescendingList, entityId = entityID, flatten = flatten)
	}


	listCarlPerkinsSubmissions <- function(searchConditionsList = NULL, CarlPerkinsSubmissionID = F, SkywardID = F, Description = F, OpenDate = F, CloseDate = F, SchoolYearID = F, SkywardHash = F, HasAnExport = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, searchConditionsGroupType = "And", searchSortFieldNamesList = NULL, searchSortFieldNamesDescendingList = NULL, entityID = 1, schoolYearID = NULL, page = 1, pageSize = 100000, flatten = T, ...){

		params <- as.list(environment()) %>% append(list(...))

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- "all", searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		listSkyObjects(module = "StateReportingMN", objectName = "CarlPerkinsSubmission", schoolYearID = schoolYearID, searchFields = searchFields, page = page, pageSize = pageSize, SearchConditionsList = searchConditionsList, SearchConditionsGroupType = searchConditionsGroupType, SearchSortFieldNamesList = searchSortFieldNamesList, SearchSortFieldNamesDescendingList = searchSortFieldNamesDescendingList, entityId = entityID, flatten = flatten)
	}


	listCarlPerkinsSubmissionRunHistories <- function(searchConditionsList = NULL, CarlPerkinsSubmissionRunHistoryID = F, CarlPerkinsSubmissionID = F, StartDateTime = F, EndDateTime = F, Type = F, MediaID = F, IsExport = F, IsLocked = F, Status = F, RunData = F, UserIDCanceledBy = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, ScopeAccessAllowed = F, HasValidMedia = F, CanAccessMedia = F, RunParameters = F, searchConditionsGroupType = "And", searchSortFieldNamesList = NULL, searchSortFieldNamesDescendingList = NULL, entityID = 1, schoolYearID = NULL, page = 1, pageSize = 100000, flatten = T, ...){

		params <- as.list(environment()) %>% append(list(...))

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- "all", searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		listSkyObjects(module = "StateReportingMN", objectName = "CarlPerkinsSubmissionRunHistory", schoolYearID = schoolYearID, searchFields = searchFields, page = page, pageSize = pageSize, SearchConditionsList = searchConditionsList, SearchConditionsGroupType = searchConditionsGroupType, SearchSortFieldNamesList = searchSortFieldNamesList, SearchSortFieldNamesDescendingList = searchSortFieldNamesDescendingList, entityId = entityID, flatten = flatten)
	}


	listStateCarlPerkinsProgramCodeMNS <- function(searchConditionsList = NULL, StateCarlPerkinsProgramCodeMNID = F, Code = F, Description = F, CourseCode = F, CourseTitle = F, CodeDescription = F, CourseCodeTitle = F, ValidYearLow = F, ValidYearHigh = F, IsInvalid = F, SkywardID = F, SkywardHash = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, searchConditionsGroupType = "And", searchSortFieldNamesList = NULL, searchSortFieldNamesDescendingList = NULL, entityID = 1, schoolYearID = NULL, page = 1, pageSize = 100000, flatten = T, ...){

		params <- as.list(environment()) %>% append(list(...))

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- "all", searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		listSkyObjects(module = "StateReportingMN", objectName = "StateCarlPerkinsProgramCodeMN", schoolYearID = schoolYearID, searchFields = searchFields, page = page, pageSize = pageSize, SearchConditionsList = searchConditionsList, SearchConditionsGroupType = searchConditionsGroupType, SearchSortFieldNamesList = searchSortFieldNamesList, SearchSortFieldNamesDescendingList = searchSortFieldNamesDescendingList, entityId = entityID, flatten = flatten)
	}


	listCarlPerkinsSubmissionFileV1Exceptions <- function(searchConditionsList = NULL, CarlPerkinsSubmissionFileV1ExceptionID = F, SchoolID = F, EntityID = F, CourseID = F, SchoolYearID = F, StudentID = F, StudentGradeBucketID = F, CarlPerkinsSubmissionFileV1ID = F, CarlPerkinsSubmissionID = F, CarlPerkinsSubmissionRunHistoryID = F, MessageType = F, RuleNumber = F, Message = F, ProcessType = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, searchConditionsGroupType = "And", searchSortFieldNamesList = NULL, searchSortFieldNamesDescendingList = NULL, entityID = 1, schoolYearID = NULL, page = 1, pageSize = 100000, flatten = T, ...){

		params <- as.list(environment()) %>% append(list(...))

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- "all", searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		listSkyObjects(module = "StateReportingMN", objectName = "CarlPerkinsSubmissionFileV1Exception", schoolYearID = schoolYearID, searchFields = searchFields, page = page, pageSize = pageSize, SearchConditionsList = searchConditionsList, SearchConditionsGroupType = searchConditionsGroupType, SearchSortFieldNamesList = searchSortFieldNamesList, SearchSortFieldNamesDescendingList = searchSortFieldNamesDescendingList, entityId = entityID, flatten = flatten)
	}


	listCarlPerkinsSubmissionFileV1s <- function(searchConditionsList = NULL, CarlPerkinsSubmissionFileV1ID = F, SchoolID = F, SchoolNumber = F, EntityID = F, SchoolYearID = F, StudentID = F, LastName = F, FirstName = F, MiddleName = F, Suffix = F, StudentStateID = F, Gender = F, BirthDate = F, StateDistrictMNID = F, DistrictCode = F, StateDistrictTypeCodeMNID = F, DistrictType = F, CourseID = F, StateCarlPerkinsProgramCodeMNID = F, CarlPerkinsProgramCode = F, CarlPerkinsCourseCode = F, CourseLength = F, StudentGradeBucketID = F, IsPassingGrade = F, CarlPerkinsMNID = F, IsTeenParent = F, IsDisplacedHomemaker = F, IsAdministeredForTSA = F, IsTSAProficient = F, CarlPerkinsSubmissionID = F, CarlPerkinsSubmissionRunHistoryID = F, HasErrors = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, KeyHash = F, UpdateHash = F, searchConditionsGroupType = "And", searchSortFieldNamesList = NULL, searchSortFieldNamesDescendingList = NULL, entityID = 1, schoolYearID = NULL, page = 1, pageSize = 100000, flatten = T, ...){

		params <- as.list(environment()) %>% append(list(...))

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- "all", searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		listSkyObjects(module = "StateReportingMN", objectName = "CarlPerkinsSubmissionFileV1", schoolYearID = schoolYearID, searchFields = searchFields, page = page, pageSize = pageSize, SearchConditionsList = searchConditionsList, SearchConditionsGroupType = searchConditionsGroupType, SearchSortFieldNamesList = searchSortFieldNamesList, SearchSortFieldNamesDescendingList = searchSortFieldNamesDescendingList, entityId = entityID, flatten = flatten)
	}


	listStateActionTypeMNS <- function(searchConditionsList = NULL, StateActionTypeMNID = F, Code = F, Description = F, CodeDescription = F, ValidYearLow = F, ValidYearHigh = F, IsInvalid = F, SkywardID = F, SkywardHash = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, searchConditionsGroupType = "And", searchSortFieldNamesList = NULL, searchSortFieldNamesDescendingList = NULL, entityID = 1, schoolYearID = NULL, page = 1, pageSize = 100000, flatten = T, ...){

		params <- as.list(environment()) %>% append(list(...))

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- "all", searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		listSkyObjects(module = "StateReportingMN", objectName = "StateActionTypeMN", schoolYearID = schoolYearID, searchFields = searchFields, page = page, pageSize = pageSize, SearchConditionsList = searchConditionsList, SearchConditionsGroupType = searchConditionsGroupType, SearchSortFieldNamesList = searchSortFieldNamesList, SearchSortFieldNamesDescendingList = searchSortFieldNamesDescendingList, entityId = entityID, flatten = flatten)
	}


	listStateDIRSAESTypeMNS <- function(searchConditionsList = NULL, StateDIRSAESTypeMNID = F, Code = F, Description = F, CodeDescription = F, ValidYearLow = F, ValidYearHigh = F, IsInvalid = F, SkywardID = F, SkywardHash = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, searchConditionsGroupType = "And", searchSortFieldNamesList = NULL, searchSortFieldNamesDescendingList = NULL, entityID = 1, schoolYearID = NULL, page = 1, pageSize = 100000, flatten = T, ...){

		params <- as.list(environment()) %>% append(list(...))

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- "all", searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		listSkyObjects(module = "StateReportingMN", objectName = "StateDIRSAESTypeMN", schoolYearID = schoolYearID, searchFields = searchFields, page = page, pageSize = pageSize, SearchConditionsList = searchConditionsList, SearchConditionsGroupType = searchConditionsGroupType, SearchSortFieldNamesList = searchSortFieldNamesList, SearchSortFieldNamesDescendingList = searchSortFieldNamesDescendingList, entityId = entityID, flatten = flatten)
	}


	listStateDIRSTimeMNS <- function(searchConditionsList = NULL, StateDIRSTimeMNID = F, Code = F, Description = F, CodeDescription = F, ValidYearLow = F, ValidYearHigh = F, IsInvalid = F, SkywardID = F, SkywardHash = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, searchConditionsGroupType = "And", searchSortFieldNamesList = NULL, searchSortFieldNamesDescendingList = NULL, entityID = 1, schoolYearID = NULL, page = 1, pageSize = 100000, flatten = T, ...){

		params <- as.list(environment()) %>% append(list(...))

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- "all", searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		listSkyObjects(module = "StateReportingMN", objectName = "StateDIRSTimeMN", schoolYearID = schoolYearID, searchFields = searchFields, page = page, pageSize = pageSize, SearchConditionsList = searchConditionsList, SearchConditionsGroupType = searchConditionsGroupType, SearchSortFieldNamesList = searchSortFieldNamesList, SearchSortFieldNamesDescendingList = searchSortFieldNamesDescendingList, entityId = entityID, flatten = flatten)
	}


	listStateDrugTypeMNS <- function(searchConditionsList = NULL, StateDrugTypeMNID = F, Code = F, Description = F, CodeDescription = F, ValidYearLow = F, ValidYearHigh = F, IsInvalid = F, SkywardID = F, SkywardHash = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, searchConditionsGroupType = "And", searchSortFieldNamesList = NULL, searchSortFieldNamesDescendingList = NULL, entityID = 1, schoolYearID = NULL, page = 1, pageSize = 100000, flatten = T, ...){

		params <- as.list(environment()) %>% append(list(...))

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- "all", searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		listSkyObjects(module = "StateReportingMN", objectName = "StateDrugTypeMN", schoolYearID = schoolYearID, searchFields = searchFields, page = page, pageSize = pageSize, SearchConditionsList = searchConditionsList, SearchConditionsGroupType = searchConditionsGroupType, SearchSortFieldNamesList = searchSortFieldNamesList, SearchSortFieldNamesDescendingList = searchSortFieldNamesDescendingList, entityId = entityID, flatten = flatten)
	}


	listStateIncidentLocationMNS <- function(searchConditionsList = NULL, StateIncidentLocationMNID = F, Code = F, Description = F, CodeDescription = F, ValidYearLow = F, ValidYearHigh = F, IsInvalid = F, SkywardID = F, SkywardHash = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, searchConditionsGroupType = "And", searchSortFieldNamesList = NULL, searchSortFieldNamesDescendingList = NULL, entityID = 1, schoolYearID = NULL, page = 1, pageSize = 100000, flatten = T, ...){

		params <- as.list(environment()) %>% append(list(...))

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- "all", searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		listSkyObjects(module = "StateReportingMN", objectName = "StateIncidentLocationMN", schoolYearID = schoolYearID, searchFields = searchFields, page = page, pageSize = pageSize, SearchConditionsList = searchConditionsList, SearchConditionsGroupType = searchConditionsGroupType, SearchSortFieldNamesList = searchSortFieldNamesList, SearchSortFieldNamesDescendingList = searchSortFieldNamesDescendingList, entityId = entityID, flatten = flatten)
	}


	listStateOffenderActivityMNS <- function(searchConditionsList = NULL, StateOffenderActivityMNID = F, Code = F, Description = F, SeverityRank = F, ActivityTypeCode = F, ActivityTypeDescription = F, CodeDescription = F, ActivityTypeCodeDescription = F, ValidYearLow = F, ValidYearHigh = F, IsInvalid = F, SkywardID = F, SkywardHash = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, searchConditionsGroupType = "And", searchSortFieldNamesList = NULL, searchSortFieldNamesDescendingList = NULL, entityID = 1, schoolYearID = NULL, page = 1, pageSize = 100000, flatten = T, ...){

		params <- as.list(environment()) %>% append(list(...))

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- "all", searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		listSkyObjects(module = "StateReportingMN", objectName = "StateOffenderActivityMN", schoolYearID = schoolYearID, searchFields = searchFields, page = page, pageSize = pageSize, SearchConditionsList = searchConditionsList, SearchConditionsGroupType = searchConditionsGroupType, SearchSortFieldNamesList = searchSortFieldNamesList, SearchSortFieldNamesDescendingList = searchSortFieldNamesDescendingList, entityId = entityID, flatten = flatten)
	}


	listStatePelletGunTypeMNS <- function(searchConditionsList = NULL, StatePelletGunTypeMNID = F, Code = F, Description = F, CodeDescription = F, ValidYearLow = F, ValidYearHigh = F, IsInvalid = F, SkywardID = F, SkywardHash = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, searchConditionsGroupType = "And", searchSortFieldNamesList = NULL, searchSortFieldNamesDescendingList = NULL, entityID = 1, schoolYearID = NULL, page = 1, pageSize = 100000, flatten = T, ...){

		params <- as.list(environment()) %>% append(list(...))

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- "all", searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		listSkyObjects(module = "StateReportingMN", objectName = "StatePelletGunTypeMN", schoolYearID = schoolYearID, searchFields = searchFields, page = page, pageSize = pageSize, SearchConditionsList = searchConditionsList, SearchConditionsGroupType = searchConditionsGroupType, SearchSortFieldNamesList = searchSortFieldNamesList, SearchSortFieldNamesDescendingList = searchSortFieldNamesDescendingList, entityId = entityID, flatten = flatten)
	}


	listStateVictimCostMNS <- function(searchConditionsList = NULL, StateVictimCostMNID = F, Code = F, Description = F, CodeDescription = F, ValidYearLow = F, ValidYearHigh = F, IsInvalid = F, SkywardID = F, SkywardHash = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, searchConditionsGroupType = "And", searchSortFieldNamesList = NULL, searchSortFieldNamesDescendingList = NULL, entityID = 1, schoolYearID = NULL, page = 1, pageSize = 100000, flatten = T, ...){

		params <- as.list(environment()) %>% append(list(...))

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- "all", searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		listSkyObjects(module = "StateReportingMN", objectName = "StateVictimCostMN", schoolYearID = schoolYearID, searchFields = searchFields, page = page, pageSize = pageSize, SearchConditionsList = searchConditionsList, SearchConditionsGroupType = searchConditionsGroupType, SearchSortFieldNamesList = searchSortFieldNamesList, SearchSortFieldNamesDescendingList = searchSortFieldNamesDescendingList, entityId = entityID, flatten = flatten)
	}


	listStateVictimTypeMNS <- function(searchConditionsList = NULL, StateVictimTypeMNID = F, Code = F, Description = F, CodeDescription = F, ValidYearLow = F, ValidYearHigh = F, IsInvalid = F, SkywardID = F, SkywardHash = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, searchConditionsGroupType = "And", searchSortFieldNamesList = NULL, searchSortFieldNamesDescendingList = NULL, entityID = 1, schoolYearID = NULL, page = 1, pageSize = 100000, flatten = T, ...){

		params <- as.list(environment()) %>% append(list(...))

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- "all", searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		listSkyObjects(module = "StateReportingMN", objectName = "StateVictimTypeMN", schoolYearID = schoolYearID, searchFields = searchFields, page = page, pageSize = pageSize, SearchConditionsList = searchConditionsList, SearchConditionsGroupType = searchConditionsGroupType, SearchSortFieldNamesList = searchSortFieldNamesList, SearchSortFieldNamesDescendingList = searchSortFieldNamesDescendingList, entityId = entityID, flatten = flatten)
	}


	listStateWeaponTypeMNS <- function(searchConditionsList = NULL, StateWeaponTypeMNID = F, Code = F, Description = F, CodeDescription = F, ValidYearLow = F, ValidYearHigh = F, IsInvalid = F, SkywardID = F, SkywardHash = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, IsPelletGun = F, IsGun = F, searchConditionsGroupType = "And", searchSortFieldNamesList = NULL, searchSortFieldNamesDescendingList = NULL, entityID = 1, schoolYearID = NULL, page = 1, pageSize = 100000, flatten = T, ...){

		params <- as.list(environment()) %>% append(list(...))

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- "all", searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		listSkyObjects(module = "StateReportingMN", objectName = "StateWeaponTypeMN", schoolYearID = schoolYearID, searchFields = searchFields, page = page, pageSize = pageSize, SearchConditionsList = searchConditionsList, SearchConditionsGroupType = searchConditionsGroupType, SearchSortFieldNamesList = searchSortFieldNamesList, SearchSortFieldNamesDescendingList = searchSortFieldNamesDescendingList, entityId = entityID, flatten = flatten)
	}


	listDIRSSubmissionRunHistories <- function(searchConditionsList = NULL, DIRSSubmissionRunHistoryID = F, DIRSSubmissionID = F, StartDateTime = F, EndDateTime = F, Type = F, MediaID = F, IsExport = F, IsLocked = F, Status = F, RunData = F, UserIDCanceledBy = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, ScopeAccessAllowed = F, HasValidMedia = F, CanAccessMedia = F, RunParameters = F, searchConditionsGroupType = "And", searchSortFieldNamesList = NULL, searchSortFieldNamesDescendingList = NULL, entityID = 1, schoolYearID = NULL, page = 1, pageSize = 100000, flatten = T, ...){

		params <- as.list(environment()) %>% append(list(...))

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- "all", searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		listSkyObjects(module = "StateReportingMN", objectName = "DIRSSubmissionRunHistory", schoolYearID = schoolYearID, searchFields = searchFields, page = page, pageSize = pageSize, SearchConditionsList = searchConditionsList, SearchConditionsGroupType = searchConditionsGroupType, SearchSortFieldNamesList = searchSortFieldNamesList, SearchSortFieldNamesDescendingList = searchSortFieldNamesDescendingList, entityId = entityID, flatten = flatten)
	}


	listDIRSSubmissions <- function(searchConditionsList = NULL, DIRSSubmissionID = F, SkywardID = F, Description = F, OpenDate = F, CloseDate = F, SchoolYearID = F, SkywardHash = F, HasAnExport = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, searchConditionsGroupType = "And", searchSortFieldNamesList = NULL, searchSortFieldNamesDescendingList = NULL, entityID = 1, schoolYearID = NULL, page = 1, pageSize = 100000, flatten = T, ...){

		params <- as.list(environment()) %>% append(list(...))

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- "all", searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		listSkyObjects(module = "StateReportingMN", objectName = "DIRSSubmission", schoolYearID = schoolYearID, searchFields = searchFields, page = page, pageSize = pageSize, SearchConditionsList = searchConditionsList, SearchConditionsGroupType = searchConditionsGroupType, SearchSortFieldNamesList = searchSortFieldNamesList, SearchSortFieldNamesDescendingList = searchSortFieldNamesDescendingList, entityId = entityID, flatten = flatten)
	}


	listDIRSSchoolV1Exceptions <- function(searchConditionsList = NULL, DIRSSchoolV1ExceptionID = F, DIRSSchoolV1ID = F, EntityID = F, SchoolYearID = F, SchoolID = F, DIRSSubmissionID = F, DIRSSubmissionRunHistoryID = F, MessageType = F, RuleNumber = F, Message = F, ProcessType = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, searchConditionsGroupType = "And", searchSortFieldNamesList = NULL, searchSortFieldNamesDescendingList = NULL, entityID = 1, schoolYearID = NULL, page = 1, pageSize = 100000, flatten = T, ...){

		params <- as.list(environment()) %>% append(list(...))

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- "all", searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		listSkyObjects(module = "StateReportingMN", objectName = "DIRSSchoolV1Exception", schoolYearID = schoolYearID, searchFields = searchFields, page = page, pageSize = pageSize, SearchConditionsList = searchConditionsList, SearchConditionsGroupType = searchConditionsGroupType, SearchSortFieldNamesList = searchSortFieldNamesList, SearchSortFieldNamesDescendingList = searchSortFieldNamesDescendingList, entityId = entityID, flatten = flatten)
	}


	listDIRSSchoolV1s <- function(searchConditionsList = NULL, DIRSSchoolV1ID = F, HasAntiBullyingPolicy = F, HasAntiViolencePolicy = F, HasCrisisPlan = F, HasDrugEducation = F, HasSafetyPlan = F, HasZeroTolerancePolicy = F, HabitualTruants = F, NameIDSafetySpecialist = F, NameEmailID = F, SafetySpecialistEmail = F, SchoolNumber = F, SchoolID = F, EntityID = F, SchoolYearID = F, DIRSSubmissionID = F, DIRSSubmissionRunHistoryID = F, HasErrors = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, EntitySchoolID = F, KeyHash = F, UpdateHash = F, searchConditionsGroupType = "And", searchSortFieldNamesList = NULL, searchSortFieldNamesDescendingList = NULL, entityID = 1, schoolYearID = NULL, page = 1, pageSize = 100000, flatten = T, ...){

		params <- as.list(environment()) %>% append(list(...))

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- "all", searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		listSkyObjects(module = "StateReportingMN", objectName = "DIRSSchoolV1", schoolYearID = schoolYearID, searchFields = searchFields, page = page, pageSize = pageSize, SearchConditionsList = searchConditionsList, SearchConditionsGroupType = searchConditionsGroupType, SearchSortFieldNamesList = searchSortFieldNamesList, SearchSortFieldNamesDescendingList = searchSortFieldNamesDescendingList, entityId = entityID, flatten = flatten)
	}


	listDIRSDrugV1Exceptions <- function(searchConditionsList = NULL, DIRSDrugV1ExceptionID = F, EntityID = F, SchoolYearID = F, IncidentOffenseNameID = F, IncidentOffenseNameDrugID = F, StudentID = F, DIRSDrugV1ID = F, DIRSSubmissionID = F, DIRSSubmissionRunHistoryID = F, MessageType = F, RuleNumber = F, Message = F, ProcessType = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, searchConditionsGroupType = "And", searchSortFieldNamesList = NULL, searchSortFieldNamesDescendingList = NULL, entityID = 1, schoolYearID = NULL, page = 1, pageSize = 100000, flatten = T, ...){

		params <- as.list(environment()) %>% append(list(...))

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- "all", searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		listSkyObjects(module = "StateReportingMN", objectName = "DIRSDrugV1Exception", schoolYearID = schoolYearID, searchFields = searchFields, page = page, pageSize = pageSize, SearchConditionsList = searchConditionsList, SearchConditionsGroupType = searchConditionsGroupType, SearchSortFieldNamesList = searchSortFieldNamesList, SearchSortFieldNamesDescendingList = searchSortFieldNamesDescendingList, entityId = entityID, flatten = flatten)
	}


	listDIRSIncidentV1Exceptions <- function(searchConditionsList = NULL, DIRSIncidentV1ExceptionID = F, EntityID = F, SchoolYearID = F, DIRSIncidentV1ID = F, IncidentID = F, DIRSSubmissionID = F, DIRSSubmissionRunHistoryID = F, MessageType = F, RuleNumber = F, Message = F, ProcessType = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, searchConditionsGroupType = "And", searchSortFieldNamesList = NULL, searchSortFieldNamesDescendingList = NULL, entityID = 1, schoolYearID = NULL, page = 1, pageSize = 100000, flatten = T, ...){

		params <- as.list(environment()) %>% append(list(...))

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- "all", searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		listSkyObjects(module = "StateReportingMN", objectName = "DIRSIncidentV1Exception", schoolYearID = schoolYearID, searchFields = searchFields, page = page, pageSize = pageSize, SearchConditionsList = searchConditionsList, SearchConditionsGroupType = searchConditionsGroupType, SearchSortFieldNamesList = searchSortFieldNamesList, SearchSortFieldNamesDescendingList = searchSortFieldNamesDescendingList, entityId = entityID, flatten = flatten)
	}


	listDIRSVictimV1Exceptions <- function(searchConditionsList = NULL, DIRSVictimV1ExceptionID = F, DIRSVictimV1ID = F, EntityID = F, SchoolYearID = F, IncidentOffenseNameID = F, StudentID = F, DIRSSubmissionID = F, DIRSSubmissionRunHistoryID = F, MessageType = F, RuleNumber = F, Message = F, ProcessType = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, StaffID = F, VictimName = F, LinkedToStaff = F, LinkedToStudent = F, searchConditionsGroupType = "And", searchSortFieldNamesList = NULL, searchSortFieldNamesDescendingList = NULL, entityID = 1, schoolYearID = NULL, page = 1, pageSize = 100000, flatten = T, ...){

		params <- as.list(environment()) %>% append(list(...))

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- "all", searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		listSkyObjects(module = "StateReportingMN", objectName = "DIRSVictimV1Exception", schoolYearID = schoolYearID, searchFields = searchFields, page = page, pageSize = pageSize, SearchConditionsList = searchConditionsList, SearchConditionsGroupType = searchConditionsGroupType, SearchSortFieldNamesList = searchSortFieldNamesList, SearchSortFieldNamesDescendingList = searchSortFieldNamesDescendingList, entityId = entityID, flatten = flatten)
	}


	listDIRSDisciplinaryActionV1Exceptions <- function(searchConditionsList = NULL, DIRSDisciplinaryActionV1ExceptionID = F, DIRSDisciplinaryActionV1ID = F, EntityID = F, SchoolYearID = F, IncidentOffenseNameActionID = F, StudentID = F, DIRSSubmissionID = F, DIRSSubmissionRunHistoryID = F, MessageType = F, RuleNumber = F, Message = F, ProcessType = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, searchConditionsGroupType = "And", searchSortFieldNamesList = NULL, searchSortFieldNamesDescendingList = NULL, entityID = 1, schoolYearID = NULL, page = 1, pageSize = 100000, flatten = T, ...){

		params <- as.list(environment()) %>% append(list(...))

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- "all", searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		listSkyObjects(module = "StateReportingMN", objectName = "DIRSDisciplinaryActionV1Exception", schoolYearID = schoolYearID, searchFields = searchFields, page = page, pageSize = pageSize, SearchConditionsList = searchConditionsList, SearchConditionsGroupType = searchConditionsGroupType, SearchSortFieldNamesList = searchSortFieldNamesList, SearchSortFieldNamesDescendingList = searchSortFieldNamesDescendingList, entityId = entityID, flatten = flatten)
	}


	listDIRSWeaponV1Exceptions <- function(searchConditionsList = NULL, DIRSWeaponV1ExceptionID = F, EntityID = F, SchoolYearID = F, IncidentOffenseNameID = F, IncidentOffenseNameWeaponID = F, StudentID = F, DIRSWeaponV1ID = F, DIRSSubmissionID = F, DIRSSubmissionRunHistoryID = F, MessageType = F, RuleNumber = F, Message = F, ProcessType = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, searchConditionsGroupType = "And", searchSortFieldNamesList = NULL, searchSortFieldNamesDescendingList = NULL, entityID = 1, schoolYearID = NULL, page = 1, pageSize = 100000, flatten = T, ...){

		params <- as.list(environment()) %>% append(list(...))

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- "all", searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		listSkyObjects(module = "StateReportingMN", objectName = "DIRSWeaponV1Exception", schoolYearID = schoolYearID, searchFields = searchFields, page = page, pageSize = pageSize, SearchConditionsList = searchConditionsList, SearchConditionsGroupType = searchConditionsGroupType, SearchSortFieldNamesList = searchSortFieldNamesList, SearchSortFieldNamesDescendingList = searchSortFieldNamesDescendingList, entityId = entityID, flatten = flatten)
	}


	listDIRSOffenderV1Exceptions <- function(searchConditionsList = NULL, DIRSOffenderV1ExceptionID = F, DIRSOffenderV1ID = F, EntityID = F, SchoolYearID = F, IncidentOffenseNameID = F, StudentID = F, DIRSSubmissionID = F, DIRSSubmissionRunHistoryID = F, MessageType = F, RuleNumber = F, Message = F, ProcessType = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, searchConditionsGroupType = "And", searchSortFieldNamesList = NULL, searchSortFieldNamesDescendingList = NULL, entityID = 1, schoolYearID = NULL, page = 1, pageSize = 100000, flatten = T, ...){

		params <- as.list(environment()) %>% append(list(...))

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- "all", searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		listSkyObjects(module = "StateReportingMN", objectName = "DIRSOffenderV1Exception", schoolYearID = schoolYearID, searchFields = searchFields, page = page, pageSize = pageSize, SearchConditionsList = searchConditionsList, SearchConditionsGroupType = searchConditionsGroupType, SearchSortFieldNamesList = searchSortFieldNamesList, SearchSortFieldNamesDescendingList = searchSortFieldNamesDescendingList, entityId = entityID, flatten = flatten)
	}


	listDIRSDrugV1s <- function(searchConditionsList = NULL, DIRSDrugV1ID = F, EntityID = F, SchoolYearID = F, DIRSSubmissionID = F, DIRSSubmissionRunHistoryID = F, IncidentOffenseNameID = F, IncidentOffenseNameDrugID = F, StudentID = F, MARSSNumber = F, IncidentID = F, ExternalIncidentID = F, StateDrugTypeMNID = F, DrugType = F, DrugTypeOtherDescription = F, HasErrors = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, StateOffenderActivityMNID = F, OffenderActivityCode = F, OffenderActivityTypeCode = F, KeyHash = F, UpdateHash = F, searchConditionsGroupType = "And", searchSortFieldNamesList = NULL, searchSortFieldNamesDescendingList = NULL, entityID = 1, schoolYearID = NULL, page = 1, pageSize = 100000, flatten = T, ...){

		params <- as.list(environment()) %>% append(list(...))

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- "all", searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		listSkyObjects(module = "StateReportingMN", objectName = "DIRSDrugV1", schoolYearID = schoolYearID, searchFields = searchFields, page = page, pageSize = pageSize, SearchConditionsList = searchConditionsList, SearchConditionsGroupType = searchConditionsGroupType, SearchSortFieldNamesList = searchSortFieldNamesList, SearchSortFieldNamesDescendingList = searchSortFieldNamesDescendingList, entityId = entityID, flatten = flatten)
	}


	listDIRSIncidentV1s <- function(searchConditionsList = NULL, DIRSIncidentV1ID = F, CostToProperty = F, ExternalIncidentID = F, IncidentDate = F, LocationID = F, StateIncidentLocationMNID = F, IncidentLocation = F, StateDIRSTimeMNID = F, TimeOfIncident = F, NumberOfUnknownOffenders = F, NumberOfUnknownVictims = F, DIRSIncidentID = F, SchoolID = F, SchoolNumber = F, EntityID = F, SchoolYearID = F, DIRSSubmissionID = F, DIRSSubmissionRunHistoryID = F, IncidentID = F, HasErrors = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, StateVictimCostMNID = F, CostToPropertyCode = F, KeyHash = F, UpdateHash = F, searchConditionsGroupType = "And", searchSortFieldNamesList = NULL, searchSortFieldNamesDescendingList = NULL, entityID = 1, schoolYearID = NULL, page = 1, pageSize = 100000, flatten = T, ...){

		params <- as.list(environment()) %>% append(list(...))

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- "all", searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		listSkyObjects(module = "StateReportingMN", objectName = "DIRSIncidentV1", schoolYearID = schoolYearID, searchFields = searchFields, page = page, pageSize = pageSize, SearchConditionsList = searchConditionsList, SearchConditionsGroupType = searchConditionsGroupType, SearchSortFieldNamesList = searchSortFieldNamesList, SearchSortFieldNamesDescendingList = searchSortFieldNamesDescendingList, entityId = entityID, flatten = flatten)
	}


	listDIRSDisciplinaryActionV1s <- function(searchConditionsList = NULL, DIRSDisciplinaryActionV1ID = F, IncidentID = F, ExternalIncidentID = F, MARSSNumber = F, StateDIRSAESTypeMNID = F, AESType = F, ReturnBeforeEndOfSchoolYear = F, StateActionTypeMNID = F, DisciplinaryActionType = F, StartDate = F, EndDate = F, DIRSActionExplanation = F, IncidentOffenseNameActionID = F, NumberDays = F, NumberDaysExpulsionReduced = F, WasDAThroughYear = F, WasExpulsionModified = F, EntityID = F, SchoolYearID = F, DIRSSubmissionID = F, DIRSSubmissionRunHistoryID = F, StudentID = F, HasErrors = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, StateOffenderActivityMNID = F, OffenderActivityCode = F, OffenderActivityTypeCode = F, KeyHash = F, UpdateHash = F, NoServiceProvidedExplanation = F, searchConditionsGroupType = "And", searchSortFieldNamesList = NULL, searchSortFieldNamesDescendingList = NULL, entityID = 1, schoolYearID = NULL, page = 1, pageSize = 100000, flatten = T, ...){

		params <- as.list(environment()) %>% append(list(...))

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- "all", searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		listSkyObjects(module = "StateReportingMN", objectName = "DIRSDisciplinaryActionV1", schoolYearID = schoolYearID, searchFields = searchFields, page = page, pageSize = pageSize, SearchConditionsList = searchConditionsList, SearchConditionsGroupType = searchConditionsGroupType, SearchSortFieldNamesList = searchSortFieldNamesList, SearchSortFieldNamesDescendingList = searchSortFieldNamesDescendingList, entityId = entityID, flatten = flatten)
	}


	listDIRSWeaponV1s <- function(searchConditionsList = NULL, DIRSWeaponV1ID = F, EntityID = F, SchoolYearID = F, DIRSSubmissionID = F, DIRSSubmissionRunHistoryID = F, IncidentOffenseNameID = F, IncidentOffenseNameWeaponID = F, StudentID = F, MARSSNumber = F, IncidentID = F, ExternalIncidentID = F, StateOffenderActivityMNID = F, OffenderActivityCode = F, OffenderActivityTypeCode = F, IsGunCased = F, IsGunInTrunk = F, IsGunLoaded = F, WasUsedAsADangerousWeaponFederal = F, WasUsedAsADangerousWeaponState = F, StateWeaponTypeMNID = F, WeaponType = F, HasErrors = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, KeyHash = F, UpdateHash = F, searchConditionsGroupType = "And", searchSortFieldNamesList = NULL, searchSortFieldNamesDescendingList = NULL, entityID = 1, schoolYearID = NULL, page = 1, pageSize = 100000, flatten = T, ...){

		params <- as.list(environment()) %>% append(list(...))

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- "all", searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		listSkyObjects(module = "StateReportingMN", objectName = "DIRSWeaponV1", schoolYearID = schoolYearID, searchFields = searchFields, page = page, pageSize = pageSize, SearchConditionsList = searchConditionsList, SearchConditionsGroupType = searchConditionsGroupType, SearchSortFieldNamesList = searchSortFieldNamesList, SearchSortFieldNamesDescendingList = searchSortFieldNamesDescendingList, entityId = entityID, flatten = flatten)
	}


	listDIRSOffenderV1s <- function(searchConditionsList = NULL, DIRSOffenderV1ID = F, IncidentID = F, ExternalIncidentID = F, MARSSNumber = F, StateOffenderActivityMNID = F, OffenderActivityCode = F, OffenderActivityTypeCode = F, IncidentOffenseNameID = F, WasRefferredToLawEnforcement = F, WasUnderCurrentSuspension = F, WasUsedAsDangerousWeaponState = F, WasUsedAsDangerousWeaponFederal = F, StudentID = F, EntityID = F, SchoolYearID = F, DIRSSubmissionID = F, DIRSSubmissionRunHistoryID = F, HasErrors = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, KeyHash = F, UpdateHash = F, WasArrestedByLawEnforcement = F, searchConditionsGroupType = "And", searchSortFieldNamesList = NULL, searchSortFieldNamesDescendingList = NULL, entityID = 1, schoolYearID = NULL, page = 1, pageSize = 100000, flatten = T, ...){

		params <- as.list(environment()) %>% append(list(...))

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- "all", searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		listSkyObjects(module = "StateReportingMN", objectName = "DIRSOffenderV1", schoolYearID = schoolYearID, searchFields = searchFields, page = page, pageSize = pageSize, SearchConditionsList = searchConditionsList, SearchConditionsGroupType = searchConditionsGroupType, SearchSortFieldNamesList = searchSortFieldNamesList, SearchSortFieldNamesDescendingList = searchSortFieldNamesDescendingList, entityId = entityID, flatten = flatten)
	}


	listDIRSVictimV1s <- function(searchConditionsList = NULL, DIRSVictimV1ID = F, IncidentID = F, ExternalIncidentID = F, MARSSNumber = F, IncidentOffenseNameID = F, StudentID = F, StateVictimCostMNID = F, CostToVictimCode = F, DidInjuryOccur = F, StateVictimTypeMNID = F, VictimTypeCode = F, WasSeriousBodilyInjury = F, EntityID = F, SchoolYearID = F, DIRSSubmissionID = F, DIRSSubmissionRunHistoryID = F, HasErrors = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, StaffID = F, NameType = F, VictimName = F, LinkedToStaff = F, LinkedToStudent = F, IsPhysicalAssault = F, IsPhysicalAssaultState = F, KeyHash = F, UpdateHash = F, MultipleVictimCount = F, EstimatedVictimsEnrolled = F, EstimatedVictimsNotEnrolled = F, searchConditionsGroupType = "And", searchSortFieldNamesList = NULL, searchSortFieldNamesDescendingList = NULL, entityID = 1, schoolYearID = NULL, page = 1, pageSize = 100000, flatten = T, ...){

		params <- as.list(environment()) %>% append(list(...))

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- "all", searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		listSkyObjects(module = "StateReportingMN", objectName = "DIRSVictimV1", schoolYearID = schoolYearID, searchFields = searchFields, page = page, pageSize = pageSize, SearchConditionsList = searchConditionsList, SearchConditionsGroupType = searchConditionsGroupType, SearchSortFieldNamesList = searchSortFieldNamesList, SearchSortFieldNamesDescendingList = searchSortFieldNamesDescendingList, entityId = entityID, flatten = flatten)
	}


	listUFARSGeneralLedgerV1IncludedAccounts <- function(searchConditionsList = NULL, UFARSGeneralLedgerV1IncludedAccountID = F, UFARSGeneralLedgerV1ID = F, AccountFiscalYearID = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, FiscalYearBeginningBalance = F, FiscalYearDebits = F, FiscalYearCredits = F, FiscalYearEndingBalance = F, ExcludeFromExport = F, ExtractedCrosswalkAccount = F, searchConditionsGroupType = "And", searchSortFieldNamesList = NULL, searchSortFieldNamesDescendingList = NULL, entityID = 1, schoolYearID = NULL, page = 1, pageSize = 100000, flatten = T, ...){

		params <- as.list(environment()) %>% append(list(...))

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- "all", searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		listSkyObjects(module = "StateReportingMN", objectName = "UFARSGeneralLedgerV1IncludedAccount", schoolYearID = schoolYearID, searchFields = searchFields, page = page, pageSize = pageSize, SearchConditionsList = searchConditionsList, SearchConditionsGroupType = searchConditionsGroupType, SearchSortFieldNamesList = searchSortFieldNamesList, SearchSortFieldNamesDescendingList = searchSortFieldNamesDescendingList, entityId = entityID, flatten = flatten)
	}


	listStateAssessmentToolMNS <- function(searchConditionsList = NULL, StateAssessmentToolMNID = F, Code = F, Description = F, CodeDescription = F, ValidYearLow = F, ValidYearHigh = F, IsInvalid = F, SkywardID = F, SkywardHash = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, searchConditionsGroupType = "And", searchSortFieldNamesList = NULL, searchSortFieldNamesDescendingList = NULL, entityID = 1, schoolYearID = NULL, page = 1, pageSize = 100000, flatten = T, ...){

		params <- as.list(environment()) %>% append(list(...))

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- "all", searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		listSkyObjects(module = "StateReportingMN", objectName = "StateAssessmentToolMN", schoolYearID = schoolYearID, searchFields = searchFields, page = page, pageSize = pageSize, SearchConditionsList = searchConditionsList, SearchConditionsGroupType = searchConditionsGroupType, SearchSortFieldNamesList = searchSortFieldNamesList, SearchSortFieldNamesDescendingList = searchSortFieldNamesDescendingList, entityId = entityID, flatten = flatten)
	}


	listStateCurriculumProgramMNS <- function(searchConditionsList = NULL, StateCurriculumProgramMNID = F, Code = F, Description = F, CodeDescription = F, ValidYearLow = F, ValidYearHigh = F, IsInvalid = F, SkywardID = F, SkywardHash = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, searchConditionsGroupType = "And", searchSortFieldNamesList = NULL, searchSortFieldNamesDescendingList = NULL, entityID = 1, schoolYearID = NULL, page = 1, pageSize = 100000, flatten = T, ...){

		params <- as.list(environment()) %>% append(list(...))

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- "all", searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		listSkyObjects(module = "StateReportingMN", objectName = "StateCurriculumProgramMN", schoolYearID = schoolYearID, searchFields = searchFields, page = page, pageSize = pageSize, SearchConditionsList = searchConditionsList, SearchConditionsGroupType = searchConditionsGroupType, SearchSortFieldNamesList = searchSortFieldNamesList, SearchSortFieldNamesDescendingList = searchSortFieldNamesDescendingList, entityId = entityID, flatten = flatten)
	}


	listStateECFEClassroomParticipationMNS <- function(searchConditionsList = NULL, StateECFEClassroomParticipationMNID = F, Code = F, Description = F, CodeDescription = F, ValidYearLow = F, ValidYearHigh = F, IsInvalid = F, SkywardID = F, SkywardHash = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, searchConditionsGroupType = "And", searchSortFieldNamesList = NULL, searchSortFieldNamesDescendingList = NULL, entityID = 1, schoolYearID = NULL, page = 1, pageSize = 100000, flatten = T, ...){

		params <- as.list(environment()) %>% append(list(...))

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- "all", searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		listSkyObjects(module = "StateReportingMN", objectName = "StateECFEClassroomParticipationMN", schoolYearID = schoolYearID, searchFields = searchFields, page = page, pageSize = pageSize, SearchConditionsList = searchConditionsList, SearchConditionsGroupType = searchConditionsGroupType, SearchSortFieldNamesList = searchSortFieldNamesList, SearchSortFieldNamesDescendingList = searchSortFieldNamesDescendingList, entityId = entityID, flatten = flatten)
	}


	listStateECFEEducationBackgroundMNS <- function(searchConditionsList = NULL, StateECFEEducationBackgroundMNID = F, Code = F, Description = F, CodeDescription = F, ValidYearLow = F, ValidYearHigh = F, IsInvalid = F, SkywardID = F, SkywardHash = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, searchConditionsGroupType = "And", searchSortFieldNamesList = NULL, searchSortFieldNamesDescendingList = NULL, entityID = 1, schoolYearID = NULL, page = 1, pageSize = 100000, flatten = T, ...){

		params <- as.list(environment()) %>% append(list(...))

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- "all", searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		listSkyObjects(module = "StateReportingMN", objectName = "StateECFEEducationBackgroundMN", schoolYearID = schoolYearID, searchFields = searchFields, page = page, pageSize = pageSize, SearchConditionsList = searchConditionsList, SearchConditionsGroupType = searchConditionsGroupType, SearchSortFieldNamesList = searchSortFieldNamesList, SearchSortFieldNamesDescendingList = searchSortFieldNamesDescendingList, entityId = entityID, flatten = flatten)
	}


	listStateECFEEmploymentStatusMNS <- function(searchConditionsList = NULL, StateECFEEmploymentStatusMNID = F, Code = F, Description = F, CodeDescription = F, ValidYearLow = F, ValidYearHigh = F, IsInvalid = F, SkywardID = F, SkywardHash = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, searchConditionsGroupType = "And", searchSortFieldNamesList = NULL, searchSortFieldNamesDescendingList = NULL, entityID = 1, schoolYearID = NULL, page = 1, pageSize = 100000, flatten = T, ...){

		params <- as.list(environment()) %>% append(list(...))

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- "all", searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		listSkyObjects(module = "StateReportingMN", objectName = "StateECFEEmploymentStatusMN", schoolYearID = schoolYearID, searchFields = searchFields, page = page, pageSize = pageSize, SearchConditionsList = searchConditionsList, SearchConditionsGroupType = searchConditionsGroupType, SearchSortFieldNamesList = searchSortFieldNamesList, SearchSortFieldNamesDescendingList = searchSortFieldNamesDescendingList, entityId = entityID, flatten = flatten)
	}


	listStateECFEFeeStatusMNS <- function(searchConditionsList = NULL, StateECFEFeeStatusMNID = F, Code = F, Description = F, CodeDescription = F, ValidYearLow = F, ValidYearHigh = F, IsInvalid = F, SkywardID = F, SkywardHash = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, searchConditionsGroupType = "And", searchSortFieldNamesList = NULL, searchSortFieldNamesDescendingList = NULL, entityID = 1, schoolYearID = NULL, page = 1, pageSize = 100000, flatten = T, ...){

		params <- as.list(environment()) %>% append(list(...))

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- "all", searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		listSkyObjects(module = "StateReportingMN", objectName = "StateECFEFeeStatusMN", schoolYearID = schoolYearID, searchFields = searchFields, page = page, pageSize = pageSize, SearchConditionsList = searchConditionsList, SearchConditionsGroupType = searchConditionsGroupType, SearchSortFieldNamesList = searchSortFieldNamesList, SearchSortFieldNamesDescendingList = searchSortFieldNamesDescendingList, entityId = entityID, flatten = flatten)
	}


	listStateECFEFundingSourceMNS <- function(searchConditionsList = NULL, StateECFEFundingSourceMNID = F, Code = F, Description = F, CodeDescription = F, ValidYearLow = F, ValidYearHigh = F, IsInvalid = F, SkywardID = F, SkywardHash = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, searchConditionsGroupType = "And", searchSortFieldNamesList = NULL, searchSortFieldNamesDescendingList = NULL, entityID = 1, schoolYearID = NULL, page = 1, pageSize = 100000, flatten = T, ...){

		params <- as.list(environment()) %>% append(list(...))

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- "all", searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		listSkyObjects(module = "StateReportingMN", objectName = "StateECFEFundingSourceMN", schoolYearID = schoolYearID, searchFields = searchFields, page = page, pageSize = pageSize, SearchConditionsList = searchConditionsList, SearchConditionsGroupType = searchConditionsGroupType, SearchSortFieldNamesList = searchSortFieldNamesList, SearchSortFieldNamesDescendingList = searchSortFieldNamesDescendingList, entityId = entityID, flatten = flatten)
	}


	listStateECFEIEPStatusMNS <- function(searchConditionsList = NULL, StateECFEIEPStatusMNID = F, Code = F, Description = F, CodeDescription = F, ValidYearLow = F, ValidYearHigh = F, IsInvalid = F, SkywardID = F, SkywardHash = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, searchConditionsGroupType = "And", searchSortFieldNamesList = NULL, searchSortFieldNamesDescendingList = NULL, entityID = 1, schoolYearID = NULL, page = 1, pageSize = 100000, flatten = T, ...){

		params <- as.list(environment()) %>% append(list(...))

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- "all", searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		listSkyObjects(module = "StateReportingMN", objectName = "StateECFEIEPStatusMN", schoolYearID = schoolYearID, searchFields = searchFields, page = page, pageSize = pageSize, SearchConditionsList = searchConditionsList, SearchConditionsGroupType = searchConditionsGroupType, SearchSortFieldNamesList = searchSortFieldNamesList, SearchSortFieldNamesDescendingList = searchSortFieldNamesDescendingList, entityId = entityID, flatten = flatten)
	}


	listStateECFEProgramModelMNS <- function(searchConditionsList = NULL, StateECFEProgramModelMNID = F, Code = F, Description = F, CodeDescription = F, ValidYearLow = F, ValidYearHigh = F, IsInvalid = F, SkywardID = F, SkywardHash = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, searchConditionsGroupType = "And", searchSortFieldNamesList = NULL, searchSortFieldNamesDescendingList = NULL, entityID = 1, schoolYearID = NULL, page = 1, pageSize = 100000, flatten = T, ...){

		params <- as.list(environment()) %>% append(list(...))

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- "all", searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		listSkyObjects(module = "StateReportingMN", objectName = "StateECFEProgramModelMN", schoolYearID = schoolYearID, searchFields = searchFields, page = page, pageSize = pageSize, SearchConditionsList = searchConditionsList, SearchConditionsGroupType = searchConditionsGroupType, SearchSortFieldNamesList = searchSortFieldNamesList, SearchSortFieldNamesDescendingList = searchSortFieldNamesDescendingList, entityId = entityID, flatten = flatten)
	}


	listStateECFEProgramNameMNS <- function(searchConditionsList = NULL, StateECFEProgramNameMNID = F, Code = F, Description = F, CodeDescription = F, ValidYearLow = F, ValidYearHigh = F, IsInvalid = F, SkywardID = F, SkywardHash = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, searchConditionsGroupType = "And", searchSortFieldNamesList = NULL, searchSortFieldNamesDescendingList = NULL, entityID = 1, schoolYearID = NULL, page = 1, pageSize = 100000, flatten = T, ...){

		params <- as.list(environment()) %>% append(list(...))

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- "all", searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		listSkyObjects(module = "StateReportingMN", objectName = "StateECFEProgramNameMN", schoolYearID = schoolYearID, searchFields = searchFields, page = page, pageSize = pageSize, SearchConditionsList = searchConditionsList, SearchConditionsGroupType = searchConditionsGroupType, SearchSortFieldNamesList = searchSortFieldNamesList, SearchSortFieldNamesDescendingList = searchSortFieldNamesDescendingList, entityId = entityID, flatten = flatten)
	}


	listStateECFERegisteringPersonMNS <- function(searchConditionsList = NULL, StateECFERegisteringPersonMNID = F, Code = F, Description = F, CodeDescription = F, ValidYearLow = F, ValidYearHigh = F, IsInvalid = F, SkywardID = F, SkywardHash = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, searchConditionsGroupType = "And", searchSortFieldNamesList = NULL, searchSortFieldNamesDescendingList = NULL, entityID = 1, schoolYearID = NULL, page = 1, pageSize = 100000, flatten = T, ...){

		params <- as.list(environment()) %>% append(list(...))

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- "all", searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		listSkyObjects(module = "StateReportingMN", objectName = "StateECFERegisteringPersonMN", schoolYearID = schoolYearID, searchFields = searchFields, page = page, pageSize = pageSize, SearchConditionsList = searchConditionsList, SearchConditionsGroupType = searchConditionsGroupType, SearchSortFieldNamesList = searchSortFieldNamesList, SearchSortFieldNamesDescendingList = searchSortFieldNamesDescendingList, entityId = entityID, flatten = flatten)
	}


	listStateHeadStartLanguageMNS <- function(searchConditionsList = NULL, StateHeadStartLanguageMNID = F, Code = F, Description = F, CodeDescription = F, ValidYearLow = F, ValidYearHigh = F, IsInvalid = F, SkywardID = F, SkywardHash = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, searchConditionsGroupType = "And", searchSortFieldNamesList = NULL, searchSortFieldNamesDescendingList = NULL, entityID = 1, schoolYearID = NULL, page = 1, pageSize = 100000, flatten = T, ...){

		params <- as.list(environment()) %>% append(list(...))

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- "all", searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		listSkyObjects(module = "StateReportingMN", objectName = "StateHeadStartLanguageMN", schoolYearID = schoolYearID, searchFields = searchFields, page = page, pageSize = pageSize, SearchConditionsList = searchConditionsList, SearchConditionsGroupType = searchConditionsGroupType, SearchSortFieldNamesList = searchSortFieldNamesList, SearchSortFieldNamesDescendingList = searchSortFieldNamesDescendingList, entityId = entityID, flatten = flatten)
	}


	listStateEarlyEducationInstructionalApproachMNS <- function(searchConditionsList = NULL, StateEarlyEducationInstructionalApproachMNID = F, Code = F, Description = F, CodeDescription = F, ValidYearLow = F, ValidYearHigh = F, IsInvalid = F, SkywardID = F, SkywardHash = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, searchConditionsGroupType = "And", searchSortFieldNamesList = NULL, searchSortFieldNamesDescendingList = NULL, entityID = 1, schoolYearID = NULL, page = 1, pageSize = 100000, flatten = T, ...){

		params <- as.list(environment()) %>% append(list(...))

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- "all", searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		listSkyObjects(module = "StateReportingMN", objectName = "StateEarlyEducationInstructionalApproachMN", schoolYearID = schoolYearID, searchFields = searchFields, page = page, pageSize = pageSize, SearchConditionsList = searchConditionsList, SearchConditionsGroupType = searchConditionsGroupType, SearchSortFieldNamesList = searchSortFieldNamesList, SearchSortFieldNamesDescendingList = searchSortFieldNamesDescendingList, entityId = entityID, flatten = flatten)
	}


	listStateEarlyEducationLocationMNS <- function(searchConditionsList = NULL, StateEarlyEducationLocationMNID = F, Code = F, Description = F, CodeDescription = F, ValidYearLow = F, ValidYearHigh = F, IsInvalid = F, SkywardID = F, SkywardHash = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, searchConditionsGroupType = "And", searchSortFieldNamesList = NULL, searchSortFieldNamesDescendingList = NULL, entityID = 1, schoolYearID = NULL, page = 1, pageSize = 100000, flatten = T, ...){

		params <- as.list(environment()) %>% append(list(...))

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- "all", searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		listSkyObjects(module = "StateReportingMN", objectName = "StateEarlyEducationLocationMN", schoolYearID = schoolYearID, searchFields = searchFields, page = page, pageSize = pageSize, SearchConditionsList = searchConditionsList, SearchConditionsGroupType = searchConditionsGroupType, SearchSortFieldNamesList = searchSortFieldNamesList, SearchSortFieldNamesDescendingList = searchSortFieldNamesDescendingList, entityId = entityID, flatten = flatten)
	}


	listStateEarlyEducationProgramMNS <- function(searchConditionsList = NULL, StateEarlyEducationProgramMNID = F, Code = F, Description = F, CodeDescription = F, ValidYearLow = F, ValidYearHigh = F, IsInvalid = F, SkywardID = F, SkywardHash = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, searchConditionsGroupType = "And", searchSortFieldNamesList = NULL, searchSortFieldNamesDescendingList = NULL, entityID = 1, schoolYearID = NULL, page = 1, pageSize = 100000, flatten = T, ...){

		params <- as.list(environment()) %>% append(list(...))

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- "all", searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		listSkyObjects(module = "StateReportingMN", objectName = "StateEarlyEducationProgramMN", schoolYearID = schoolYearID, searchFields = searchFields, page = page, pageSize = pageSize, SearchConditionsList = searchConditionsList, SearchConditionsGroupType = searchConditionsGroupType, SearchSortFieldNamesList = searchSortFieldNamesList, SearchSortFieldNamesDescendingList = searchSortFieldNamesDescendingList, entityId = entityID, flatten = flatten)
	}


	listStateImplementationStatusMNS <- function(searchConditionsList = NULL, StateImplementationStatusMNID = F, Code = F, Description = F, CodeDescription = F, ValidYearLow = F, ValidYearHigh = F, IsInvalid = F, SkywardID = F, SkywardHash = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, searchConditionsGroupType = "And", searchSortFieldNamesList = NULL, searchSortFieldNamesDescendingList = NULL, entityID = 1, schoolYearID = NULL, page = 1, pageSize = 100000, flatten = T, ...){

		params <- as.list(environment()) %>% append(list(...))

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- "all", searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		listSkyObjects(module = "StateReportingMN", objectName = "StateImplementationStatusMN", schoolYearID = schoolYearID, searchFields = searchFields, page = page, pageSize = pageSize, SearchConditionsList = searchConditionsList, SearchConditionsGroupType = searchConditionsGroupType, SearchSortFieldNamesList = searchSortFieldNamesList, SearchSortFieldNamesDescendingList = searchSortFieldNamesDescendingList, entityId = entityID, flatten = flatten)
	}


	listStateSiteBasedInitiativeMNS <- function(searchConditionsList = NULL, StateSiteBasedInitiativeMNID = F, Code = F, Description = F, CodeDescription = F, ValidYearLow = F, ValidYearHigh = F, IsInvalid = F, SkywardID = F, SkywardHash = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, searchConditionsGroupType = "And", searchSortFieldNamesList = NULL, searchSortFieldNamesDescendingList = NULL, entityID = 1, schoolYearID = NULL, page = 1, pageSize = 100000, flatten = T, ...){

		params <- as.list(environment()) %>% append(list(...))

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- "all", searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		listSkyObjects(module = "StateReportingMN", objectName = "StateSiteBasedInitiativeMN", schoolYearID = schoolYearID, searchFields = searchFields, page = page, pageSize = pageSize, SearchConditionsList = searchConditionsList, SearchConditionsGroupType = searchConditionsGroupType, SearchSortFieldNamesList = searchSortFieldNamesList, SearchSortFieldNamesDescendingList = searchSortFieldNamesDescendingList, entityId = entityID, flatten = flatten)
	}


	listTempEducatedEmployees <- function(searchConditionsList = NULL, TempEducatedEmployeeID = F, EmployeeID = F, StateSTARHighestEducationLevelMNID = F, EmployeeFullNameFML = F, CurrentLane = F, CurrentCredits = F, STARHighestEducationLevelOverride = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, EmployeeNumber = F, searchConditionsGroupType = "And", searchSortFieldNamesList = NULL, searchSortFieldNamesDescendingList = NULL, entityID = 1, schoolYearID = NULL, page = 1, pageSize = 100000, flatten = T, ...){

		params <- as.list(environment()) %>% append(list(...))

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- "all", searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		listSkyObjects(module = "StateReportingMN", objectName = "TempEducatedEmployee", schoolYearID = schoolYearID, searchFields = searchFields, page = page, pageSize = pageSize, SearchConditionsList = searchConditionsList, SearchConditionsGroupType = searchConditionsGroupType, SearchSortFieldNamesList = searchSortFieldNamesList, SearchSortFieldNamesDescendingList = searchSortFieldNamesDescendingList, entityId = entityID, flatten = flatten)
	}


	listTempEducatedEmployeeExceptions <- function(searchConditionsList = NULL, TempEducatedEmployeeExceptionID = F, EmployeeFullNameFML = F, Error = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, EmployeeNumber = F, searchConditionsGroupType = "And", searchSortFieldNamesList = NULL, searchSortFieldNamesDescendingList = NULL, entityID = 1, schoolYearID = NULL, page = 1, pageSize = 100000, flatten = T, ...){

		params <- as.list(environment()) %>% append(list(...))

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- "all", searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		listSkyObjects(module = "StateReportingMN", objectName = "TempEducatedEmployeeException", schoolYearID = schoolYearID, searchFields = searchFields, page = page, pageSize = pageSize, SearchConditionsList = searchConditionsList, SearchConditionsGroupType = searchConditionsGroupType, SearchSortFieldNamesList = searchSortFieldNamesList, SearchSortFieldNamesDescendingList = searchSortFieldNamesDescendingList, entityId = entityID, flatten = flatten)
	}


	listStateGraduationRequirementMNS <- function(searchConditionsList = NULL, StateGraduationRequirementMNID = F, Code = F, Description = F, CodeDescription = F, ValidYearLow = F, ValidYearHigh = F, IsInvalid = F, SkywardID = F, SkywardHash = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, searchConditionsGroupType = "And", searchSortFieldNamesList = NULL, searchSortFieldNamesDescendingList = NULL, entityID = 1, schoolYearID = NULL, page = 1, pageSize = 100000, flatten = T, ...){

		params <- as.list(environment()) %>% append(list(...))

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- "all", searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		listSkyObjects(module = "StateReportingMN", objectName = "StateGraduationRequirementMN", schoolYearID = schoolYearID, searchFields = searchFields, page = page, pageSize = pageSize, SearchConditionsList = searchConditionsList, SearchConditionsGroupType = searchConditionsGroupType, SearchSortFieldNamesList = searchSortFieldNamesList, SearchSortFieldNamesDescendingList = searchSortFieldNamesDescendingList, entityId = entityID, flatten = flatten)
	}


	listStateGraduationRequirementMethodMNS <- function(searchConditionsList = NULL, StateGraduationRequirementMethodMNID = F, Code = F, Description = F, CodeDescription = F, ValidYearLow = F, ValidYearHigh = F, IsInvalid = F, SkywardID = F, SkywardHash = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, searchConditionsGroupType = "And", searchSortFieldNamesList = NULL, searchSortFieldNamesDescendingList = NULL, entityID = 1, schoolYearID = NULL, page = 1, pageSize = 100000, flatten = T, ...){

		params <- as.list(environment()) %>% append(list(...))

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- "all", searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		listSkyObjects(module = "StateReportingMN", objectName = "StateGraduationRequirementMethodMN", schoolYearID = schoolYearID, searchFields = searchFields, page = page, pageSize = pageSize, SearchConditionsList = searchConditionsList, SearchConditionsGroupType = searchConditionsGroupType, SearchSortFieldNamesList = searchSortFieldNamesList, SearchSortFieldNamesDescendingList = searchSortFieldNamesDescendingList, entityId = entityID, flatten = flatten)
	}


	listStateGradeMarkMNS <- function(searchConditionsList = NULL, StateGradeMarkMNID = F, Code = F, Description = F, CodeDescription = F, ValidYearLow = F, ValidYearHigh = F, IsInvalid = F, SkywardID = F, SkywardHash = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, searchConditionsGroupType = "And", searchSortFieldNamesList = NULL, searchSortFieldNamesDescendingList = NULL, entityID = 1, schoolYearID = NULL, page = 1, pageSize = 100000, flatten = T, ...){

		params <- as.list(environment()) %>% append(list(...))

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- "all", searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		listSkyObjects(module = "StateReportingMN", objectName = "StateGradeMarkMN", schoolYearID = schoolYearID, searchFields = searchFields, page = page, pageSize = pageSize, SearchConditionsList = searchConditionsList, SearchConditionsGroupType = searchConditionsGroupType, SearchSortFieldNamesList = searchSortFieldNamesList, SearchSortFieldNamesDescendingList = searchSortFieldNamesDescendingList, entityId = entityID, flatten = flatten)
	}


	listTRASubmissionDemographicDetailV1Exceptions <- function(searchConditionsList = NULL, TRASubmissionDemographicDetailV1ExceptionID = F, DistrictID = F, EmployeeID = F, TRASubmissionDemographicDetailV1ID = F, TRASubmissionID = F, TRASubmissionRunHistoryID = F, RuleNumber = F, MessageType = F, Message = F, ProcessType = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, searchConditionsGroupType = "And", searchSortFieldNamesList = NULL, searchSortFieldNamesDescendingList = NULL, entityID = 1, schoolYearID = NULL, page = 1, pageSize = 100000, flatten = T, ...){

		params <- as.list(environment()) %>% append(list(...))

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- "all", searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		listSkyObjects(module = "StateReportingMN", objectName = "TRASubmissionDemographicDetailV1Exception", schoolYearID = schoolYearID, searchFields = searchFields, page = page, pageSize = pageSize, SearchConditionsList = searchConditionsList, SearchConditionsGroupType = searchConditionsGroupType, SearchSortFieldNamesList = searchSortFieldNamesList, SearchSortFieldNamesDescendingList = searchSortFieldNamesDescendingList, entityId = entityID, flatten = flatten)
	}


	listTRASubmissionEmployerV1Exceptions <- function(searchConditionsList = NULL, TRASubmissionEmployerV1ExceptionID = F, EmployerID = F, TRASubmissionEmployerV1ID = F, DistrictID = F, TRASubmissionID = F, TRASubmissionRunHistoryID = F, RuleNumber = F, MessageType = F, Message = F, ProcessType = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, searchConditionsGroupType = "And", searchSortFieldNamesList = NULL, searchSortFieldNamesDescendingList = NULL, entityID = 1, schoolYearID = NULL, page = 1, pageSize = 100000, flatten = T, ...){

		params <- as.list(environment()) %>% append(list(...))

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- "all", searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		listSkyObjects(module = "StateReportingMN", objectName = "TRASubmissionEmployerV1Exception", schoolYearID = schoolYearID, searchFields = searchFields, page = page, pageSize = pageSize, SearchConditionsList = searchConditionsList, SearchConditionsGroupType = searchConditionsGroupType, SearchSortFieldNamesList = searchSortFieldNamesList, SearchSortFieldNamesDescendingList = searchSortFieldNamesDescendingList, entityId = entityID, flatten = flatten)
	}


	listTRASubmissionPayrollDetailV1Exceptions <- function(searchConditionsList = NULL, TRASubmissionPayrollDetailV1ExceptionID = F, DistrictID = F, EmployeeID = F, TRASubmissionPayrollDetailV1ID = F, TRASubmissionID = F, TRASubmissionRunHistoryID = F, RuleNumber = F, MessageType = F, Message = F, ProcessType = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, searchConditionsGroupType = "And", searchSortFieldNamesList = NULL, searchSortFieldNamesDescendingList = NULL, entityID = 1, schoolYearID = NULL, page = 1, pageSize = 100000, flatten = T, ...){

		params <- as.list(environment()) %>% append(list(...))

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- "all", searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		listSkyObjects(module = "StateReportingMN", objectName = "TRASubmissionPayrollDetailV1Exception", schoolYearID = schoolYearID, searchFields = searchFields, page = page, pageSize = pageSize, SearchConditionsList = searchConditionsList, SearchConditionsGroupType = searchConditionsGroupType, SearchSortFieldNamesList = searchSortFieldNamesList, SearchSortFieldNamesDescendingList = searchSortFieldNamesDescendingList, entityId = entityID, flatten = flatten)
	}


	listTRASubmissionDemographicDetailV1s <- function(searchConditionsList = NULL, TRASubmissionDemographicDetailV1ID = F, EmployeeID = F, EmployeeName = F, AddressID = F, StreetLine1 = F, StreetLine2 = F, City = F, State = F, Zip = F, SocialSecurityNumber = F, EmployeeNumber = F, TRANumber = F, DateOfBirth = F, EffectiveDate = F, StateTRAExemptStatusMNID = F, StateTRAExemptStatusMNValue = F, StateTRAEligibilityMNID = F, StateTRAEligibilityMNValue = F, StateTRAStatusMNID = F, StateTRAStatusMNValue = F, ExcludeFromExport = F, GenderCode = F, TRASubmissionID = F, DistrictID = F, TRASubmissionRunHistoryID = F, HasErrors = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, KeyHash = F, UpdateHash = F, searchConditionsGroupType = "And", searchSortFieldNamesList = NULL, searchSortFieldNamesDescendingList = NULL, entityID = 1, schoolYearID = NULL, page = 1, pageSize = 100000, flatten = T, ...){

		params <- as.list(environment()) %>% append(list(...))

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- "all", searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		listSkyObjects(module = "StateReportingMN", objectName = "TRASubmissionDemographicDetailV1", schoolYearID = schoolYearID, searchFields = searchFields, page = page, pageSize = pageSize, SearchConditionsList = searchConditionsList, SearchConditionsGroupType = searchConditionsGroupType, SearchSortFieldNamesList = searchSortFieldNamesList, SearchSortFieldNamesDescendingList = searchSortFieldNamesDescendingList, entityId = entityID, flatten = flatten)
	}


	listTRASubmissionEmployerV1s <- function(searchConditionsList = NULL, TRASubmissionEmployerV1ID = F, EmployerID = F, TRACountyGroupNumber = F, TRADistrictUnitNumber = F, TRASubmissionID = F, DistrictID = F, TRASubmissionRunHistoryID = F, HasErrors = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, KeyHash = F, UpdateHash = F, searchConditionsGroupType = "And", searchSortFieldNamesList = NULL, searchSortFieldNamesDescendingList = NULL, entityID = 1, schoolYearID = NULL, page = 1, pageSize = 100000, flatten = T, ...){

		params <- as.list(environment()) %>% append(list(...))

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- "all", searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		listSkyObjects(module = "StateReportingMN", objectName = "TRASubmissionEmployerV1", schoolYearID = schoolYearID, searchFields = searchFields, page = page, pageSize = pageSize, SearchConditionsList = searchConditionsList, SearchConditionsGroupType = searchConditionsGroupType, SearchSortFieldNamesList = searchSortFieldNamesList, SearchSortFieldNamesDescendingList = searchSortFieldNamesDescendingList, entityId = entityID, flatten = flatten)
	}


	listTRASubmissionPayrollDetailV1s <- function(searchConditionsList = NULL, TRASubmissionPayrollDetailV1ID = F, EmployeeID = F, SocialSecurityNumber = F, EmployeeNumber = F, TRANumber = F, StateTRACurrentPositionMNID = F, StateTRACurrentPositionMNValue = F, Salary = F, EmployeeDeductionAmount = F, EmployerDeductionAmount = F, StateTransactionTypeMNID = F, StateTransactionTypeMNValue = F, StateTRAPaymentTypeMNID = F, StateTRAPaymentTypeMNValue = F, TRAFiscalYear = F, PayPeriodBeginDate = F, PayPeriodEndDate = F, PaymentDate = F, StateTRAPayrollFrequencyMNID = F, StateTRAPayrollFrequencyMNValue = F, StateRetirementAssociationTypeCode = F, TRASubmissionID = F, DistrictID = F, TRASubmissionRunHistoryID = F, HasErrors = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, KeyHash = F, UpdateHash = F, StateTRAExemptStatusMNID = F, StateTRAExemptStatusMNCode = F, searchConditionsGroupType = "And", searchSortFieldNamesList = NULL, searchSortFieldNamesDescendingList = NULL, entityID = 1, schoolYearID = NULL, page = 1, pageSize = 100000, flatten = T, ...){

		params <- as.list(environment()) %>% append(list(...))

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- "all", searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		listSkyObjects(module = "StateReportingMN", objectName = "TRASubmissionPayrollDetailV1", schoolYearID = schoolYearID, searchFields = searchFields, page = page, pageSize = pageSize, SearchConditionsList = searchConditionsList, SearchConditionsGroupType = searchConditionsGroupType, SearchSortFieldNamesList = searchSortFieldNamesList, SearchSortFieldNamesDescendingList = searchSortFieldNamesDescendingList, entityId = entityID, flatten = flatten)
	}


	listTRASubmissions <- function(searchConditionsList = NULL, TRASubmissionID = F, DistrictID = F, FiscalYearID = F, Description = F, IsFinal = F, HasRunHistories = F, ReportType = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, searchConditionsGroupType = "And", searchSortFieldNamesList = NULL, searchSortFieldNamesDescendingList = NULL, entityID = 1, schoolYearID = NULL, page = 1, pageSize = 100000, flatten = T, ...){

		params <- as.list(environment()) %>% append(list(...))

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- "all", searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		listSkyObjects(module = "StateReportingMN", objectName = "TRASubmission", schoolYearID = schoolYearID, searchFields = searchFields, page = page, pageSize = pageSize, SearchConditionsList = searchConditionsList, SearchConditionsGroupType = searchConditionsGroupType, SearchSortFieldNamesList = searchSortFieldNamesList, SearchSortFieldNamesDescendingList = searchSortFieldNamesDescendingList, entityId = entityID, flatten = flatten)
	}


	listTRASubmissionPayrollRuns <- function(searchConditionsList = NULL, TRASubmissionPayrollRunID = F, TRASubmissionID = F, PayrollRunID = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, searchConditionsGroupType = "And", searchSortFieldNamesList = NULL, searchSortFieldNamesDescendingList = NULL, entityID = 1, schoolYearID = NULL, page = 1, pageSize = 100000, flatten = T, ...){

		params <- as.list(environment()) %>% append(list(...))

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- "all", searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		listSkyObjects(module = "StateReportingMN", objectName = "TRASubmissionPayrollRun", schoolYearID = schoolYearID, searchFields = searchFields, page = page, pageSize = pageSize, SearchConditionsList = searchConditionsList, SearchConditionsGroupType = searchConditionsGroupType, SearchSortFieldNamesList = searchSortFieldNamesList, SearchSortFieldNamesDescendingList = searchSortFieldNamesDescendingList, entityId = entityID, flatten = flatten)
	}


	listTRASubmissionRunHistories <- function(searchConditionsList = NULL, TRASubmissionRunHistoryID = F, TRASubmissionID = F, DistrictID = F, StartDateTime = F, EndDateTime = F, Type = F, MediaID = F, ScopeAccessAllowed = F, HasValidMedia = F, CanAccessMedia = F, IsExport = F, IsLocked = F, Status = F, RunData = F, UserIDCanceledBy = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, RunParameters = F, searchConditionsGroupType = "And", searchSortFieldNamesList = NULL, searchSortFieldNamesDescendingList = NULL, entityID = 1, schoolYearID = NULL, page = 1, pageSize = 100000, flatten = T, ...){

		params <- as.list(environment()) %>% append(list(...))

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- "all", searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		listSkyObjects(module = "StateReportingMN", objectName = "TRASubmissionRunHistory", schoolYearID = schoolYearID, searchFields = searchFields, page = page, pageSize = pageSize, SearchConditionsList = searchConditionsList, SearchConditionsGroupType = searchConditionsGroupType, SearchSortFieldNamesList = searchSortFieldNamesList, SearchSortFieldNamesDescendingList = searchSortFieldNamesDescendingList, entityId = entityID, flatten = flatten)
	}


	listStateCountyMNS <- function(searchConditionsList = NULL, StateCountyMNID = F, Code = F, Description = F, CodeDescription = F, SkywardID = F, SkywardHash = F, ValidYearLow = F, ValidYearHigh = F, IsInvalid = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, searchConditionsGroupType = "And", searchSortFieldNamesList = NULL, searchSortFieldNamesDescendingList = NULL, entityID = 1, schoolYearID = NULL, page = 1, pageSize = 100000, flatten = T, ...){

		params <- as.list(environment()) %>% append(list(...))

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- "all", searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		listSkyObjects(module = "StateReportingMN", objectName = "StateCountyMN", schoolYearID = schoolYearID, searchFields = searchFields, page = page, pageSize = pageSize, SearchConditionsList = searchConditionsList, SearchConditionsGroupType = searchConditionsGroupType, SearchSortFieldNamesList = searchSortFieldNamesList, SearchSortFieldNamesDescendingList = searchSortFieldNamesDescendingList, entityId = entityID, flatten = flatten)
	}


	listGRRStudentExceptions <- function(searchConditionsList = NULL, GRRStudentExceptionID = F, GRRSubmissionID = F, GRRStudentV1ID = F, GRRSubmissionRunHistoryID = F, SchoolYearID = F, EntityID = F, SchoolID = F, StudentID = F, MessageType = F, RuleNumber = F, Message = F, ProcessType = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, searchConditionsGroupType = "And", searchSortFieldNamesList = NULL, searchSortFieldNamesDescendingList = NULL, entityID = 1, schoolYearID = NULL, page = 1, pageSize = 100000, flatten = T, ...){

		params <- as.list(environment()) %>% append(list(...))

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- "all", searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		listSkyObjects(module = "StateReportingMN", objectName = "GRRStudentException", schoolYearID = schoolYearID, searchFields = searchFields, page = page, pageSize = pageSize, SearchConditionsList = searchConditionsList, SearchConditionsGroupType = searchConditionsGroupType, SearchSortFieldNamesList = searchSortFieldNamesList, SearchSortFieldNamesDescendingList = searchSortFieldNamesDescendingList, entityId = entityID, flatten = flatten)
	}


	listGRRSubmissions <- function(searchConditionsList = NULL, GRRSubmissionID = F, SkywardID = F, Description = F, TestTakenByDate = F, SubmissionDate = F, SchoolYearID = F, SkywardHash = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, HasAnExport = F, searchConditionsGroupType = "And", searchSortFieldNamesList = NULL, searchSortFieldNamesDescendingList = NULL, entityID = 1, schoolYearID = NULL, page = 1, pageSize = 100000, flatten = T, ...){

		params <- as.list(environment()) %>% append(list(...))

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- "all", searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		listSkyObjects(module = "StateReportingMN", objectName = "GRRSubmission", schoolYearID = schoolYearID, searchFields = searchFields, page = page, pageSize = pageSize, SearchConditionsList = searchConditionsList, SearchConditionsGroupType = searchConditionsGroupType, SearchSortFieldNamesList = searchSortFieldNamesList, SearchSortFieldNamesDescendingList = searchSortFieldNamesDescendingList, entityId = entityID, flatten = flatten)
	}


	listGRRSubmissionRunHistories <- function(searchConditionsList = NULL, GRRSubmissionRunHistoryID = F, GRRSubmissionID = F, DistrictID = F, SchoolYearID = F, StartDateTime = F, EndDateTime = F, Type = F, MediaID = F, ScopeAccessAllowed = F, HasValidMedia = F, CanAccessMedia = F, IsExport = F, IsLocked = F, Status = F, RunData = F, UserIDCanceledBy = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, IsFinalized = F, RunParameters = F, searchConditionsGroupType = "And", searchSortFieldNamesList = NULL, searchSortFieldNamesDescendingList = NULL, entityID = 1, schoolYearID = NULL, page = 1, pageSize = 100000, flatten = T, ...){

		params <- as.list(environment()) %>% append(list(...))

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- "all", searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		listSkyObjects(module = "StateReportingMN", objectName = "GRRSubmissionRunHistory", schoolYearID = schoolYearID, searchFields = searchFields, page = page, pageSize = pageSize, SearchConditionsList = searchConditionsList, SearchConditionsGroupType = searchConditionsGroupType, SearchSortFieldNamesList = searchSortFieldNamesList, SearchSortFieldNamesDescendingList = searchSortFieldNamesDescendingList, entityId = entityID, flatten = flatten)
	}


	listGRRStudentV1s <- function(searchConditionsList = NULL, GRRStudentV1ID = F, GRRSubmissionID = F, GRRSubmissionRunHistoryID = F, GraduationRequirementMNID = F, StateUnitNumber = F, SchoolYearID = F, EntityID = F, StateDistrictTypeCodeMNID = F, StateUnitType = F, SchoolID = F, StateSiteNumber = F, LastName = F, FirstName = F, MiddleName = F, Suffix = F, StudentID = F, StateStudentID = F, BirthDate = F, Gender = F, LocalUseID = F, StateGraduationRequirementMNID = F, SubjectCode = F, StateGraduationRequirementMethodMNID = F, MethodCode = F, MetGraduationRequirement = F, TestedDate = F, Comment = F, RequestReimbursement = F, KeyHash = F, UpdateHash = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, StateDistrictMNID = F, Version = F, HasErrors = F, searchConditionsGroupType = "And", searchSortFieldNamesList = NULL, searchSortFieldNamesDescendingList = NULL, entityID = 1, schoolYearID = NULL, page = 1, pageSize = 100000, flatten = T, ...){

		params <- as.list(environment()) %>% append(list(...))

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- "all", searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		listSkyObjects(module = "StateReportingMN", objectName = "GRRStudentV1", schoolYearID = schoolYearID, searchFields = searchFields, page = page, pageSize = pageSize, SearchConditionsList = searchConditionsList, SearchConditionsGroupType = searchConditionsGroupType, SearchSortFieldNamesList = searchSortFieldNamesList, SearchSortFieldNamesDescendingList = searchSortFieldNamesDescendingList, entityId = entityID, flatten = flatten)
	}


	listImmunizationStatusRunHistoryMNS <- function(searchConditionsList = NULL, ImmunizationStatusRunHistoryMNID = F, DistrictID = F, SchoolYearID = F, ComplianceDate = F, StartDateTime = F, EndDateTime = F, Type = F, MediaID = F, ScopeAccessAllowed = F, HasValidMedia = F, CanAccessMedia = F, IsExport = F, IsLocked = F, Status = F, RunData = F, UserIDCanceledBy = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, RunParameters = F, searchConditionsGroupType = "And", searchSortFieldNamesList = NULL, searchSortFieldNamesDescendingList = NULL, entityID = 1, schoolYearID = NULL, page = 1, pageSize = 100000, flatten = T, ...){

		params <- as.list(environment()) %>% append(list(...))

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- "all", searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		listSkyObjects(module = "StateReportingMN", objectName = "ImmunizationStatusRunHistoryMN", schoolYearID = schoolYearID, searchFields = searchFields, page = page, pageSize = pageSize, SearchConditionsList = searchConditionsList, SearchConditionsGroupType = searchConditionsGroupType, SearchSortFieldNamesList = searchSortFieldNamesList, SearchSortFieldNamesDescendingList = searchSortFieldNamesDescendingList, entityId = entityID, flatten = flatten)
	}


	listImmunizationStatusStudentMNS <- function(searchConditionsList = NULL, ImmunizationStatusStudentMNID = F, ImmunizationStatusRunHistoryMNID = F, DistrictID = F, EntityID = F, StudentID = F, StateSTARGradeLevelMNID = F, IsExemptCOAllVaccines = F, IsExemptMEAllVaccines = F, HasAllRequiredDosesDTP = F, IsMissingDosesDTP = F, IsExemptCODTP = F, IsExemptMEDTP = F, HasAllRequiredDosesPolio = F, IsMissingDosesPolio = F, IsExemptCOPolio = F, IsExemptMEPolio = F, HasAllRequiredDosesMMR = F, IsMissingDosesMMR = F, IsExemptCOMMR = F, IsExemptMEMMR = F, HasAllRequiredDosesHepatitisB = F, IsMissingDosesHepatitisB = F, IsExemptCOHepatitisB = F, IsExemptMEHepatitisB = F, HasAllRequiredDosesVaricella = F, IsMissingDosesVaricella = F, HasHistoryOfVaricella = F, IsExemptCOVaricella = F, IsExemptMEVaricella = F, HasAllRequiredDosesTdap = F, IsMissingDosesTdap = F, IsExemptCOTdap = F, IsExemptMETdap = F, HasAllRequiredDosesMeningococcal = F, IsMissingDosesMeningococcal = F, IsExemptCOMeningococcal = F, IsExemptMEMeningococcal = F, KeyHash = F, UpdateHash = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, searchConditionsGroupType = "And", searchSortFieldNamesList = NULL, searchSortFieldNamesDescendingList = NULL, entityID = 1, schoolYearID = NULL, page = 1, pageSize = 100000, flatten = T, ...){

		params <- as.list(environment()) %>% append(list(...))

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- "all", searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		listSkyObjects(module = "StateReportingMN", objectName = "ImmunizationStatusStudentMN", schoolYearID = schoolYearID, searchFields = searchFields, page = page, pageSize = pageSize, SearchConditionsList = searchConditionsList, SearchConditionsGroupType = searchConditionsGroupType, SearchSortFieldNamesList = searchSortFieldNamesList, SearchSortFieldNamesDescendingList = searchSortFieldNamesDescendingList, entityId = entityID, flatten = flatten)
	}


	listImmunizationStatusSummaryMNS <- function(searchConditionsList = NULL, DistrictID = F, EntityID = F, StateSTARGradeLevelMNID = F, EnrollmentCount = F, ExemptAllVaccinesCO = F, ExemptAllVaccinesME = F, DTPAllRequiredDoses = F, DTPMissingDoses = F, DTPExemptCO = F, DTPExemptME = F, PolioAllRequiredDoses = F, PolioMissingDoses = F, PolioExemptCO = F, PolioExemptME = F, MMRAllRequiredDoses = F, MMRMissingDoses = F, MMRExemptCO = F, MMRExemptME = F, HepatitisBAllRequiredDoses = F, HepatitisBMissingDoses = F, HepatitisBExemptCO = F, HepatitisBExemptME = F, VaricellaAllRequiredDoses = F, VaricellaMissingDoses = F, VaricellaHistoryOfDisease = F, VaricellaExemptCO = F, VaricellaExemptME = F, TdapAllRequiredDoses = F, TdapMissingDoses = F, TdapExemptCO = F, TdapExemptME = F, MeningococcalAllRequiredDoses = F, MeningococcalMissingDoses = F, MeningococcalExemptCO = F, MeningococcalExemptME = F, searchConditionsGroupType = "And", searchSortFieldNamesList = NULL, searchSortFieldNamesDescendingList = NULL, entityID = 1, schoolYearID = NULL, page = 1, pageSize = 100000, flatten = T, ...){

		params <- as.list(environment()) %>% append(list(...))

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- "all", searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		listSkyObjects(module = "StateReportingMN", objectName = "ImmunizationStatusSummaryMN", schoolYearID = schoolYearID, searchFields = searchFields, page = page, pageSize = pageSize, SearchConditionsList = searchConditionsList, SearchConditionsGroupType = searchConditionsGroupType, SearchSortFieldNamesList = searchSortFieldNamesList, SearchSortFieldNamesDescendingList = searchSortFieldNamesDescendingList, entityId = entityID, flatten = flatten)
	}


	listMCCCAssessmentToolV1Exceptions <- function(searchConditionsList = NULL, MCCCAssessmentToolV1ExceptionID = F, MCCCAssessmentToolV1ID = F, SchoolID = F, CourseID = F, CurriculumYearID = F, EntityID = F, SchoolYearID = F, MCCCSubmissionID = F, MCCCSubmissionRunHistoryID = F, MessageType = F, RuleNumber = F, Message = F, ProcessType = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, searchConditionsGroupType = "And", searchSortFieldNamesList = NULL, searchSortFieldNamesDescendingList = NULL, entityID = 1, schoolYearID = NULL, page = 1, pageSize = 100000, flatten = T, ...){

		params <- as.list(environment()) %>% append(list(...))

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- "all", searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		listSkyObjects(module = "StateReportingMN", objectName = "MCCCAssessmentToolV1Exception", schoolYearID = schoolYearID, searchFields = searchFields, page = page, pageSize = pageSize, SearchConditionsList = searchConditionsList, SearchConditionsGroupType = searchConditionsGroupType, SearchSortFieldNamesList = searchSortFieldNamesList, SearchSortFieldNamesDescendingList = searchSortFieldNamesDescendingList, entityId = entityID, flatten = flatten)
	}


	listMCCCAssessmentToolV1s <- function(searchConditionsList = NULL, MCCCAssessmentToolV1ID = F, AssessmentToolMNID = F, StateAssessmentToolMNID = F, AssessmentToolCode = F, StateImplementationStatusMNID = F, ImplementationStatusCode = F, CurriculumYearID = F, EntityID = F, SchoolYearID = F, MCCCSubmissionID = F, MCCCSubmissionRunHistoryID = F, SchoolID = F, SchoolNumber = F, CourseID = F, LocalCourseCode = F, HasErrors = F, KeyHash = F, UpdateHash = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, searchConditionsGroupType = "And", searchSortFieldNamesList = NULL, searchSortFieldNamesDescendingList = NULL, entityID = 1, schoolYearID = NULL, page = 1, pageSize = 100000, flatten = T, ...){

		params <- as.list(environment()) %>% append(list(...))

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- "all", searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		listSkyObjects(module = "StateReportingMN", objectName = "MCCCAssessmentToolV1", schoolYearID = schoolYearID, searchFields = searchFields, page = page, pageSize = pageSize, SearchConditionsList = searchConditionsList, SearchConditionsGroupType = searchConditionsGroupType, SearchSortFieldNamesList = searchSortFieldNamesList, SearchSortFieldNamesDescendingList = searchSortFieldNamesDescendingList, entityId = entityID, flatten = flatten)
	}


	listMCCCEarlyEducationInstructionalApproachV1Exceptions <- function(searchConditionsList = NULL, MCCCEarlyEducationInstructionalApproachV1ExceptionID = F, MCCCEarlyEducationInstructionalApproachV1ID = F, SchoolID = F, CourseID = F, CurriculumYearID = F, EntityID = F, SchoolYearID = F, MCCCSubmissionID = F, MCCCSubmissionRunHistoryID = F, MessageType = F, RuleNumber = F, Message = F, ProcessType = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, searchConditionsGroupType = "And", searchSortFieldNamesList = NULL, searchSortFieldNamesDescendingList = NULL, entityID = 1, schoolYearID = NULL, page = 1, pageSize = 100000, flatten = T, ...){

		params <- as.list(environment()) %>% append(list(...))

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- "all", searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		listSkyObjects(module = "StateReportingMN", objectName = "MCCCEarlyEducationInstructionalApproachV1Exception", schoolYearID = schoolYearID, searchFields = searchFields, page = page, pageSize = pageSize, SearchConditionsList = searchConditionsList, SearchConditionsGroupType = searchConditionsGroupType, SearchSortFieldNamesList = searchSortFieldNamesList, SearchSortFieldNamesDescendingList = searchSortFieldNamesDescendingList, entityId = entityID, flatten = flatten)
	}


	listMCCCEarlyEducationInstructionalApproachV1s <- function(searchConditionsList = NULL, MCCCEarlyEducationInstructionalApproachV1ID = F, EarlyEducationInstructionalApproachMNID = F, StateEarlyEducationInstructionalApproachMNID = F, EarlyEducationInstructionalApproachCode = F, StateImplementationStatusMNID = F, ImplementationStatusCode = F, CurriculumYearID = F, EntityID = F, SchoolYearID = F, MCCCSubmissionID = F, MCCCSubmissionRunHistoryID = F, SchoolID = F, SchoolNumber = F, CourseID = F, LocalCourseCode = F, HasErrors = F, KeyHash = F, UpdateHash = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, searchConditionsGroupType = "And", searchSortFieldNamesList = NULL, searchSortFieldNamesDescendingList = NULL, entityID = 1, schoolYearID = NULL, page = 1, pageSize = 100000, flatten = T, ...){

		params <- as.list(environment()) %>% append(list(...))

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- "all", searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		listSkyObjects(module = "StateReportingMN", objectName = "MCCCEarlyEducationInstructionalApproachV1", schoolYearID = schoolYearID, searchFields = searchFields, page = page, pageSize = pageSize, SearchConditionsList = searchConditionsList, SearchConditionsGroupType = searchConditionsGroupType, SearchSortFieldNamesList = searchSortFieldNamesList, SearchSortFieldNamesDescendingList = searchSortFieldNamesDescendingList, entityId = entityID, flatten = flatten)
	}


	listStateSchoolMNS <- function(searchConditionsList = NULL, StateSchoolMNID = F, Code = F, Description = F, SchoolClassificationCode = F, CodeDescription = F, SchoolNumber = F, DistrictCode = F, DistrictType = F, Number = F, SkywardID = F, SkywardHash = F, ValidYearLow = F, ValidYearHigh = F, IsInvalid = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, searchConditionsGroupType = "And", searchSortFieldNamesList = NULL, searchSortFieldNamesDescendingList = NULL, entityID = 1, schoolYearID = NULL, page = 1, pageSize = 100000, flatten = T, ...){

		params <- as.list(environment()) %>% append(list(...))

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- "all", searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		listSkyObjects(module = "StateReportingMN", objectName = "StateSchoolMN", schoolYearID = schoolYearID, searchFields = searchFields, page = page, pageSize = pageSize, SearchConditionsList = searchConditionsList, SearchConditionsGroupType = searchConditionsGroupType, SearchSortFieldNamesList = searchSortFieldNamesList, SearchSortFieldNamesDescendingList = searchSortFieldNamesDescendingList, entityId = entityID, flatten = flatten)
	}


	listStateSchoolClassificationMNS <- function(searchConditionsList = NULL, StateSchoolClassificationMNID = F, Code = F, Description = F, Classification = F, CodeDescription = F, SkywardID = F, SkywardHash = F, ValidYearLow = F, ValidYearHigh = F, IsInvalid = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, searchConditionsGroupType = "And", searchSortFieldNamesList = NULL, searchSortFieldNamesDescendingList = NULL, entityID = 1, schoolYearID = NULL, page = 1, pageSize = 100000, flatten = T, ...){

		params <- as.list(environment()) %>% append(list(...))

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- "all", searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		listSkyObjects(module = "StateReportingMN", objectName = "StateSchoolClassificationMN", schoolYearID = schoolYearID, searchFields = searchFields, page = page, pageSize = pageSize, SearchConditionsList = searchConditionsList, SearchConditionsGroupType = searchConditionsGroupType, SearchSortFieldNamesList = searchSortFieldNamesList, SearchSortFieldNamesDescendingList = searchSortFieldNamesDescendingList, entityId = entityID, flatten = flatten)
	}


	listECFEProgramRegistrationStateECFEFundingSourceMNS <- function(searchConditionsList = NULL, ECFEProgramRegistrationStateECFEFundingSourceMNID = F, ECFEProgramRegistrationMNID = F, StateECFEFundingSourceMNID = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, searchConditionsGroupType = "And", searchSortFieldNamesList = NULL, searchSortFieldNamesDescendingList = NULL, entityID = 1, schoolYearID = NULL, page = 1, pageSize = 100000, flatten = T, ...){

		params <- as.list(environment()) %>% append(list(...))

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- "all", searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		listSkyObjects(module = "StateReportingMN", objectName = "ECFEProgramRegistrationStateECFEFundingSourceMN", schoolYearID = schoolYearID, searchFields = searchFields, page = page, pageSize = pageSize, SearchConditionsList = searchConditionsList, SearchConditionsGroupType = searchConditionsGroupType, SearchSortFieldNamesList = searchSortFieldNamesList, SearchSortFieldNamesDescendingList = searchSortFieldNamesDescendingList, entityId = entityID, flatten = flatten)
	}


	listECFEProgramRegistrationStateECFEProgramNameMNS <- function(searchConditionsList = NULL, ECFEProgramRegistrationStateECFEProgramNameMNID = F, ECFEProgramRegistrationMNID = F, StateECFEProgramNameMNID = F, ProgramReferred = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, searchConditionsGroupType = "And", searchSortFieldNamesList = NULL, searchSortFieldNamesDescendingList = NULL, entityID = 1, schoolYearID = NULL, page = 1, pageSize = 100000, flatten = T, ...){

		params <- as.list(environment()) %>% append(list(...))

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- "all", searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		listSkyObjects(module = "StateReportingMN", objectName = "ECFEProgramRegistrationStateECFEProgramNameMN", schoolYearID = schoolYearID, searchFields = searchFields, page = page, pageSize = pageSize, SearchConditionsList = searchConditionsList, SearchConditionsGroupType = searchConditionsGroupType, SearchSortFieldNamesList = searchSortFieldNamesList, SearchSortFieldNamesDescendingList = searchSortFieldNamesDescendingList, entityId = entityID, flatten = flatten)
	}


	listMCCCSiteBasedInitiativeExceptions <- function(searchConditionsList = NULL, MCCCSiteBasedInitiativeExceptionID = F, MCCCSiteBasedInitiativeV1ID = F, SchoolID = F, CourseID = F, CurriculumYearID = F, EntityID = F, SchoolYearID = F, MCCCSubmissionID = F, MCCCSubmissionRunHistoryID = F, MessageType = F, RuleNumber = F, Message = F, ProcessType = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, searchConditionsGroupType = "And", searchSortFieldNamesList = NULL, searchSortFieldNamesDescendingList = NULL, entityID = 1, schoolYearID = NULL, page = 1, pageSize = 100000, flatten = T, ...){

		params <- as.list(environment()) %>% append(list(...))

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- "all", searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		listSkyObjects(module = "StateReportingMN", objectName = "MCCCSiteBasedInitiativeException", schoolYearID = schoolYearID, searchFields = searchFields, page = page, pageSize = pageSize, SearchConditionsList = searchConditionsList, SearchConditionsGroupType = searchConditionsGroupType, SearchSortFieldNamesList = searchSortFieldNamesList, SearchSortFieldNamesDescendingList = searchSortFieldNamesDescendingList, entityId = entityID, flatten = flatten)
	}


	listMCCCEarlyEducationProgramV1Exceptions <- function(searchConditionsList = NULL, MCCCEarlyEducationProgramV1ExceptionID = F, MCCCEarlyEducationProgramV1ID = F, SchoolID = F, CourseID = F, CurriculumYearID = F, EntityID = F, SchoolYearID = F, MCCCSubmissionID = F, MCCCSubmissionRunHistoryID = F, MessageType = F, RuleNumber = F, Message = F, ProcessType = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, searchConditionsGroupType = "And", searchSortFieldNamesList = NULL, searchSortFieldNamesDescendingList = NULL, entityID = 1, schoolYearID = NULL, page = 1, pageSize = 100000, flatten = T, ...){

		params <- as.list(environment()) %>% append(list(...))

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- "all", searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		listSkyObjects(module = "StateReportingMN", objectName = "MCCCEarlyEducationProgramV1Exception", schoolYearID = schoolYearID, searchFields = searchFields, page = page, pageSize = pageSize, SearchConditionsList = searchConditionsList, SearchConditionsGroupType = searchConditionsGroupType, SearchSortFieldNamesList = searchSortFieldNamesList, SearchSortFieldNamesDescendingList = searchSortFieldNamesDescendingList, entityId = entityID, flatten = flatten)
	}


	listMCCCSiteBasedInitiativeV1s <- function(searchConditionsList = NULL, MCCCSiteBasedInitiativeV1ID = F, SiteBasedInitiativeMNID = F, StateSiteBasedInitiativeMNID = F, SiteBasedInitiativeCode = F, StateImplementationStatusMNID = F, ImplementationStatusCode = F, CurriculumYearID = F, EntityID = F, SchoolYearID = F, MCCCSubmissionID = F, MCCCSubmissionRunHistoryID = F, SchoolID = F, SchoolNumber = F, CourseID = F, LocalCourseCode = F, HasErrors = F, KeyHash = F, UpdateHash = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, searchConditionsGroupType = "And", searchSortFieldNamesList = NULL, searchSortFieldNamesDescendingList = NULL, entityID = 1, schoolYearID = NULL, page = 1, pageSize = 100000, flatten = T, ...){

		params <- as.list(environment()) %>% append(list(...))

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- "all", searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		listSkyObjects(module = "StateReportingMN", objectName = "MCCCSiteBasedInitiativeV1", schoolYearID = schoolYearID, searchFields = searchFields, page = page, pageSize = pageSize, SearchConditionsList = searchConditionsList, SearchConditionsGroupType = searchConditionsGroupType, SearchSortFieldNamesList = searchSortFieldNamesList, SearchSortFieldNamesDescendingList = searchSortFieldNamesDescendingList, entityId = entityID, flatten = flatten)
	}


	listMCCCEarlyEducationProgramV1s <- function(searchConditionsList = NULL, MCCCEarlyEducationProgramV1ID = F, EarlyEducationProgramMNID = F, StateEarlyEducationProgramMNID = F, EarlyEducationProgramCode = F, CurriculumYearID = F, EntityID = F, SchoolYearID = F, MCCCSubmissionID = F, MCCCSubmissionRunHistoryID = F, SchoolID = F, SchoolNumber = F, CourseID = F, LocalCourseCode = F, HasErrors = F, KeyHash = F, UpdateHash = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, searchConditionsGroupType = "And", searchSortFieldNamesList = NULL, searchSortFieldNamesDescendingList = NULL, entityID = 1, schoolYearID = NULL, page = 1, pageSize = 100000, flatten = T, ...){

		params <- as.list(environment()) %>% append(list(...))

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- "all", searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		listSkyObjects(module = "StateReportingMN", objectName = "MCCCEarlyEducationProgramV1", schoolYearID = schoolYearID, searchFields = searchFields, page = page, pageSize = pageSize, SearchConditionsList = searchConditionsList, SearchConditionsGroupType = searchConditionsGroupType, SearchSortFieldNamesList = searchSortFieldNamesList, SearchSortFieldNamesDescendingList = searchSortFieldNamesDescendingList, entityId = entityID, flatten = flatten)
	}


	listMCCCCurriculumProgramV1Exceptions <- function(searchConditionsList = NULL, MCCCCurriculumProgramV1ExceptionID = F, MCCCCurriculumProgramV1ID = F, SchoolID = F, CourseID = F, CurriculumYearID = F, EntityID = F, SchoolYearID = F, MCCCSubmissionID = F, MCCCSubmissionRunHistoryID = F, MessageType = F, RuleNumber = F, Message = F, ProcessType = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, searchConditionsGroupType = "And", searchSortFieldNamesList = NULL, searchSortFieldNamesDescendingList = NULL, entityID = 1, schoolYearID = NULL, page = 1, pageSize = 100000, flatten = T, ...){

		params <- as.list(environment()) %>% append(list(...))

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- "all", searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		listSkyObjects(module = "StateReportingMN", objectName = "MCCCCurriculumProgramV1Exception", schoolYearID = schoolYearID, searchFields = searchFields, page = page, pageSize = pageSize, SearchConditionsList = searchConditionsList, SearchConditionsGroupType = searchConditionsGroupType, SearchSortFieldNamesList = searchSortFieldNamesList, SearchSortFieldNamesDescendingList = searchSortFieldNamesDescendingList, entityId = entityID, flatten = flatten)
	}


	listMCCCCurriculumProgramV1s <- function(searchConditionsList = NULL, MCCCCurriculumProgramV1ID = F, CurriculumProgramMNID = F, StateCurriculumProgramMNID = F, CurriculumProgramCode = F, StateImplementationStatusMNID = F, ImplementationStatusCode = F, CurriculumYearID = F, EntityID = F, SchoolYearID = F, MCCCSubmissionID = F, MCCCSubmissionRunHistoryID = F, SchoolID = F, SchoolNumber = F, CourseID = F, LocalCourseCode = F, HasErrors = F, KeyHash = F, UpdateHash = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, searchConditionsGroupType = "And", searchSortFieldNamesList = NULL, searchSortFieldNamesDescendingList = NULL, entityID = 1, schoolYearID = NULL, page = 1, pageSize = 100000, flatten = T, ...){

		params <- as.list(environment()) %>% append(list(...))

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- "all", searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		listSkyObjects(module = "StateReportingMN", objectName = "MCCCCurriculumProgramV1", schoolYearID = schoolYearID, searchFields = searchFields, page = page, pageSize = pageSize, SearchConditionsList = searchConditionsList, SearchConditionsGroupType = searchConditionsGroupType, SearchSortFieldNamesList = searchSortFieldNamesList, SearchSortFieldNamesDescendingList = searchSortFieldNamesDescendingList, entityId = entityID, flatten = flatten)
	}


	listStateEthnicityRaceSubcategoryMNS <- function(searchConditionsList = NULL, StateEthnicityRaceSubcategoryMNID = F, Code = F, Description = F, CodeDescription = F, EthnicityRaceType = F, SkywardID = F, SkywardHash = F, ValidYearLow = F, ValidYearHigh = F, IsInvalid = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, searchConditionsGroupType = "And", searchSortFieldNamesList = NULL, searchSortFieldNamesDescendingList = NULL, entityID = 1, schoolYearID = NULL, page = 1, pageSize = 100000, flatten = T, ...){

		params <- as.list(environment()) %>% append(list(...))

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- "all", searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		listSkyObjects(module = "StateReportingMN", objectName = "StateEthnicityRaceSubcategoryMN", schoolYearID = schoolYearID, searchFields = searchFields, page = page, pageSize = pageSize, SearchConditionsList = searchConditionsList, SearchConditionsGroupType = searchConditionsGroupType, SearchSortFieldNamesList = searchSortFieldNamesList, SearchSortFieldNamesDescendingList = searchSortFieldNamesDescendingList, entityId = entityID, flatten = flatten)
	}


	listMCCCAdditionalAssessmentStaffExceptions <- function(searchConditionsList = NULL, MCCCAdditionalAssessmentStaffExceptionID = F, MCCCAdditionalAssessmentStaffV1ID = F, SchoolID = F, CourseID = F, EntityID = F, SchoolYearID = F, StaffID = F, SectionID = F, MCCCSubmissionID = F, MCCCSubmissionRunHistoryID = F, MessageType = F, RuleNumber = F, Message = F, ProcessType = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, searchConditionsGroupType = "And", searchSortFieldNamesList = NULL, searchSortFieldNamesDescendingList = NULL, entityID = 1, schoolYearID = NULL, page = 1, pageSize = 100000, flatten = T, ...){

		params <- as.list(environment()) %>% append(list(...))

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- "all", searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		listSkyObjects(module = "StateReportingMN", objectName = "MCCCAdditionalAssessmentStaffException", schoolYearID = schoolYearID, searchFields = searchFields, page = page, pageSize = pageSize, SearchConditionsList = searchConditionsList, SearchConditionsGroupType = searchConditionsGroupType, SearchSortFieldNamesList = searchSortFieldNamesList, SearchSortFieldNamesDescendingList = searchSortFieldNamesDescendingList, entityId = entityID, flatten = flatten)
	}


	listMCCCAdditionalAssessmentStaffV1s <- function(searchConditionsList = NULL, MCCCAdditionalAssessmentStaffV1ID = F, LocalCourseCode = F, CourseID = F, SectionID = F, SectionCode = F, StaffID = F, MCCCSubmissionID = F, MCCCSubmissionRunHistoryID = F, FileFolderNumber = F, EntityID = F, SchoolYearID = F, SchoolID = F, SchoolNumber = F, HasErrors = F, KeyHash = F, UpdateHash = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, searchConditionsGroupType = "And", searchSortFieldNamesList = NULL, searchSortFieldNamesDescendingList = NULL, entityID = 1, schoolYearID = NULL, page = 1, pageSize = 100000, flatten = T, ...){

		params <- as.list(environment()) %>% append(list(...))

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- "all", searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		listSkyObjects(module = "StateReportingMN", objectName = "MCCCAdditionalAssessmentStaffV1", schoolYearID = schoolYearID, searchFields = searchFields, page = page, pageSize = pageSize, SearchConditionsList = searchConditionsList, SearchConditionsGroupType = searchConditionsGroupType, SearchSortFieldNamesList = searchSortFieldNamesList, SearchSortFieldNamesDescendingList = searchSortFieldNamesDescendingList, entityId = entityID, flatten = flatten)
	}


	listSTAREmploymentPeriods <- function(searchConditionsList = NULL, STAREmploymentPeriodID = F, DistrictID = F, EmployeeID = F, StartDate = F, EndDate = F, StateSTARNewLicensedStaffMNID = F, StateSTARNewNonLicensedStaffMNID = F, StateSTARInactiveTransferTerminationMNID = F, StateSTARInactiveTransferTerminationMNIDTermination = F, FileFolderNumber = F, StateSTARHighestEducationLevelMNIDOverride = F, STAREmploymentType = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, IsActive = F, searchConditionsGroupType = "And", searchSortFieldNamesList = NULL, searchSortFieldNamesDescendingList = NULL, entityID = 1, schoolYearID = NULL, page = 1, pageSize = 100000, flatten = T, ...){

		params <- as.list(environment()) %>% append(list(...))

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- "all", searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		listSkyObjects(module = "StateReportingMN", objectName = "STAREmploymentPeriod", schoolYearID = schoolYearID, searchFields = searchFields, page = page, pageSize = pageSize, SearchConditionsList = searchConditionsList, SearchConditionsGroupType = searchConditionsGroupType, SearchSortFieldNamesList = searchSortFieldNamesList, SearchSortFieldNamesDescendingList = searchSortFieldNamesDescendingList, entityId = entityID, flatten = flatten)
	}


	listECFERegistrationV1RegisteringPersonV1s <- function(searchConditionsList = NULL, ECFERegistrationV1RegisteringPersonV1ID = F, ECFERegistrationV1ID = F, ECFERegisteringPersonV1ID = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, searchConditionsGroupType = "And", searchSortFieldNamesList = NULL, searchSortFieldNamesDescendingList = NULL, entityID = 1, schoolYearID = NULL, page = 1, pageSize = 100000, flatten = T, ...){

		params <- as.list(environment()) %>% append(list(...))

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- "all", searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		listSkyObjects(module = "StateReportingMN", objectName = "ECFERegistrationV1RegisteringPersonV1", schoolYearID = schoolYearID, searchFields = searchFields, page = page, pageSize = pageSize, SearchConditionsList = searchConditionsList, SearchConditionsGroupType = searchConditionsGroupType, SearchSortFieldNamesList = searchSortFieldNamesList, SearchSortFieldNamesDescendingList = searchSortFieldNamesDescendingList, entityId = entityID, flatten = flatten)
	}


	listECFERegisteringPersonExceptions <- function(searchConditionsList = NULL, ECFERegisteringPersonExceptionID = F, ECFERegisteringPersonV1ID = F, NameID = F, LastName = F, FirstName = F, ECFESubmissionID = F, ECFESubmissionRunHistoryID = F, SchoolYearID = F, MessageType = F, RuleNumber = F, Message = F, ProcessType = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, EntityID = F, searchConditionsGroupType = "And", searchSortFieldNamesList = NULL, searchSortFieldNamesDescendingList = NULL, entityID = 1, schoolYearID = NULL, page = 1, pageSize = 100000, flatten = T, ...){

		params <- as.list(environment()) %>% append(list(...))

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- "all", searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		listSkyObjects(module = "StateReportingMN", objectName = "ECFERegisteringPersonException", schoolYearID = schoolYearID, searchFields = searchFields, page = page, pageSize = pageSize, SearchConditionsList = searchConditionsList, SearchConditionsGroupType = searchConditionsGroupType, SearchSortFieldNamesList = searchSortFieldNamesList, SearchSortFieldNamesDescendingList = searchSortFieldNamesDescendingList, entityId = entityID, flatten = flatten)
	}


	listECFERegistrationExceptions <- function(searchConditionsList = NULL, ECFERegistrationExceptionID = F, ECFERegistrationV1ID = F, StudentID = F, ECFEProgramRegistrationMNID = F, EntityID = F, ECFESubmissionID = F, ECFESubmissionRunHistoryID = F, SchoolYearID = F, MessageType = F, RuleNumber = F, Message = F, ProcessType = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, searchConditionsGroupType = "And", searchSortFieldNamesList = NULL, searchSortFieldNamesDescendingList = NULL, entityID = 1, schoolYearID = NULL, page = 1, pageSize = 100000, flatten = T, ...){

		params <- as.list(environment()) %>% append(list(...))

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- "all", searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		listSkyObjects(module = "StateReportingMN", objectName = "ECFERegistrationException", schoolYearID = schoolYearID, searchFields = searchFields, page = page, pageSize = pageSize, SearchConditionsList = searchConditionsList, SearchConditionsGroupType = searchConditionsGroupType, SearchSortFieldNamesList = searchSortFieldNamesList, SearchSortFieldNamesDescendingList = searchSortFieldNamesDescendingList, entityId = entityID, flatten = flatten)
	}


	listECFEStudentExceptions <- function(searchConditionsList = NULL, ECFEStudentExceptionID = F, ECFEStudentV1ID = F, NameID = F, StudentID = F, EntityID = F, ECFESubmissionID = F, ECFESubmissionRunHistoryID = F, SchoolYearID = F, MessageType = F, RuleNumber = F, Message = F, ProcessType = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, searchConditionsGroupType = "And", searchSortFieldNamesList = NULL, searchSortFieldNamesDescendingList = NULL, entityID = 1, schoolYearID = NULL, page = 1, pageSize = 100000, flatten = T, ...){

		params <- as.list(environment()) %>% append(list(...))

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- "all", searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		listSkyObjects(module = "StateReportingMN", objectName = "ECFEStudentException", schoolYearID = schoolYearID, searchFields = searchFields, page = page, pageSize = pageSize, SearchConditionsList = searchConditionsList, SearchConditionsGroupType = searchConditionsGroupType, SearchSortFieldNamesList = searchSortFieldNamesList, SearchSortFieldNamesDescendingList = searchSortFieldNamesDescendingList, entityId = entityID, flatten = flatten)
	}


	listECFESubmissions <- function(searchConditionsList = NULL, ECFESubmissionID = F, SchoolYearID = F, Description = F, OpenDate = F, CloseDate = F, SkywardID = F, SkywardHash = F, HasAnExtract = F, HasAnExport = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, searchConditionsGroupType = "And", searchSortFieldNamesList = NULL, searchSortFieldNamesDescendingList = NULL, entityID = 1, schoolYearID = NULL, page = 1, pageSize = 100000, flatten = T, ...){

		params <- as.list(environment()) %>% append(list(...))

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- "all", searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		listSkyObjects(module = "StateReportingMN", objectName = "ECFESubmission", schoolYearID = schoolYearID, searchFields = searchFields, page = page, pageSize = pageSize, SearchConditionsList = searchConditionsList, SearchConditionsGroupType = searchConditionsGroupType, SearchSortFieldNamesList = searchSortFieldNamesList, SearchSortFieldNamesDescendingList = searchSortFieldNamesDescendingList, entityId = entityID, flatten = flatten)
	}


	listECFESubmissionRunHistories <- function(searchConditionsList = NULL, ECFESubmissionRunHistoryID = F, DistrictID = F, ECFESubmissionID = F, StartDateTime = F, EndDateTime = F, Type = F, MediaID = F, ScopeAccessAllowed = F, HasValidMedia = F, CanAccessMedia = F, IsExport = F, IsLocked = F, Status = F, RunData = F, UserIDCanceledBy = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, RunParameters = F, searchConditionsGroupType = "And", searchSortFieldNamesList = NULL, searchSortFieldNamesDescendingList = NULL, entityID = 1, schoolYearID = NULL, page = 1, pageSize = 100000, flatten = T, ...){

		params <- as.list(environment()) %>% append(list(...))

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- "all", searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		listSkyObjects(module = "StateReportingMN", objectName = "ECFESubmissionRunHistory", schoolYearID = schoolYearID, searchFields = searchFields, page = page, pageSize = pageSize, SearchConditionsList = searchConditionsList, SearchConditionsGroupType = searchConditionsGroupType, SearchSortFieldNamesList = searchSortFieldNamesList, SearchSortFieldNamesDescendingList = searchSortFieldNamesDescendingList, entityId = entityID, flatten = flatten)
	}


	listECFERegisteringPersonV1s <- function(searchConditionsList = NULL, ECFERegisteringPersonV1ID = F, NameID = F, LastName = F, FirstName = F, FullName = F, DateOfBirth = F, StateECFERegisteringPersonMNID = F, RegisteringPersonType = F, StateECFEEducationBackgroundMNID = F, EducationBackground = F, StateECFEEmploymentStatusMNID = F, EmploymentStatus = F, HouseholdIncome = F, NoOfPeopleInHousehold = F, ReceivingInterpreterAsst = F, StateECFEClassroomParticipationMNID = F, ClassRoomVolunteerType = F, SchoolYearID = F, ECFESubmissionID = F, ECFESubmissionRunHistoryID = F, HasErrors = F, KeyHash = F, UpdateHash = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, EntityID = F, ECFEProgramRegistrationMNID = F, searchConditionsGroupType = "And", searchSortFieldNamesList = NULL, searchSortFieldNamesDescendingList = NULL, entityID = 1, schoolYearID = NULL, page = 1, pageSize = 100000, flatten = T, ...){

		params <- as.list(environment()) %>% append(list(...))

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- "all", searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		listSkyObjects(module = "StateReportingMN", objectName = "ECFERegisteringPersonV1", schoolYearID = schoolYearID, searchFields = searchFields, page = page, pageSize = pageSize, SearchConditionsList = searchConditionsList, SearchConditionsGroupType = searchConditionsGroupType, SearchSortFieldNamesList = searchSortFieldNamesList, SearchSortFieldNamesDescendingList = searchSortFieldNamesDescendingList, entityId = entityID, flatten = flatten)
	}


	listECFERegistrationV1s <- function(searchConditionsList = NULL, ECFERegistrationV1ID = F, StudentID = F, EntityID = F, ECFEProgramRegistrationMNID = F, SiteNumber = F, StateECFEProgramNameMNID = F, ProgramName = F, StateECFEProgramModelMNID = F, ProgramModel = F, ProgramReferralsToData = F, ProgramReferralsTo = F, ProgramReferralsFromData = F, ProgramReferralsFrom = F, ProgramRegistrationDate = F, CountOfClasses = F, HoursAttended = F, DaysAttended = F, DaysAbsent = F, StateECFEFeeStatusMNID = F, FeeStatus = F, FundingSourceData = F, FundingSources = F, StateECFEIEPStatusMNID = F, IEPStatus = F, SchoolYearID = F, ECFESubmissionID = F, ECFESubmissionRunHistoryID = F, HasErrors = F, KeyHash = F, UpdateHash = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, searchConditionsGroupType = "And", searchSortFieldNamesList = NULL, searchSortFieldNamesDescendingList = NULL, entityID = 1, schoolYearID = NULL, page = 1, pageSize = 100000, flatten = T, ...){

		params <- as.list(environment()) %>% append(list(...))

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- "all", searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		listSkyObjects(module = "StateReportingMN", objectName = "ECFERegistrationV1", schoolYearID = schoolYearID, searchFields = searchFields, page = page, pageSize = pageSize, SearchConditionsList = searchConditionsList, SearchConditionsGroupType = searchConditionsGroupType, SearchSortFieldNamesList = searchSortFieldNamesList, SearchSortFieldNamesDescendingList = searchSortFieldNamesDescendingList, entityId = entityID, flatten = flatten)
	}


	listECFEStudentV1s <- function(searchConditionsList = NULL, ECFEStudentV1ID = F, NameID = F, StudentID = F, EntityID = F, StateStudentID = F, LastName = F, FirstName = F, MiddleName = F, Suffix = F, Gender = F, StateDistrictMNID = F, UnitNumber = F, StateDistrictTypeCodeMNID = F, UnitType = F, DateOfBirth = F, HispanicLatino = F, RaceIsAmericanIndianOrAlaskanNative = F, RaceIsAsian = F, RaceIsBlackOrAfricanAmerican = F, RaceIsNativeHawaiianOrOtherPacificIslander = F, RaceIsWhite = F, RaceIsOther = F, RaceSpecifyOther = F, RaceIsUnspecified = F, Migrant = F, StateLanguageCodeMNIDPrimary = F, PrimaryLanguage = F, SpecifyOtherLanguage = F, StateLanguageCodeMNIDSecondary = F, SecondaryLanguage = F, McKinneyVentoHomeless = F, ImmunizationsUpToDate = F, ParentInKindHours = F, TotalHomeVisitsCompleted = F, StateCountyMNID = F, CountyOfResidence = F, SiblingData = F, IsSibling = F, IsMultiRacial = F, SchoolYearID = F, ECFESubmissionID = F, ECFESubmissionRunHistoryID = F, HasErrors = F, KeyHash = F, UpdateHash = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, searchConditionsGroupType = "And", searchSortFieldNamesList = NULL, searchSortFieldNamesDescendingList = NULL, entityID = 1, schoolYearID = NULL, page = 1, pageSize = 100000, flatten = T, ...){

		params <- as.list(environment()) %>% append(list(...))

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- "all", searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		listSkyObjects(module = "StateReportingMN", objectName = "ECFEStudentV1", schoolYearID = schoolYearID, searchFields = searchFields, page = page, pageSize = pageSize, SearchConditionsList = searchConditionsList, SearchConditionsGroupType = searchConditionsGroupType, SearchSortFieldNamesList = searchSortFieldNamesList, SearchSortFieldNamesDescendingList = searchSortFieldNamesDescendingList, entityId = entityID, flatten = flatten)
	}


	listPERAIncludedPayTransactions <- function(searchConditionsList = NULL, PERAIncludedPayTransactionID = F, PERAExtractRunID = F, EmployeeID = F, CheckDate = F, PayrollStartDate = F, MemberAmount = F, EligibleEarnings = F, EmployerAmount = F, CompensatedHours = F, OvertimePay = F, AdjustmentIndicator = F, PERAPaymentTypeCodeValue = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, searchConditionsGroupType = "And", searchSortFieldNamesList = NULL, searchSortFieldNamesDescendingList = NULL, entityID = 1, schoolYearID = NULL, page = 1, pageSize = 100000, flatten = T, ...){

		params <- as.list(environment()) %>% append(list(...))

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- "all", searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		listSkyObjects(module = "StateReportingMN", objectName = "PERAIncludedPayTransaction", schoolYearID = schoolYearID, searchFields = searchFields, page = page, pageSize = pageSize, SearchConditionsList = searchConditionsList, SearchConditionsGroupType = searchConditionsGroupType, SearchSortFieldNamesList = searchSortFieldNamesList, SearchSortFieldNamesDescendingList = searchSortFieldNamesDescendingList, entityId = entityID, flatten = flatten)
	}
