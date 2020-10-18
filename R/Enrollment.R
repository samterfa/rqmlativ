

	listEnrollmentConfigDistrictYears <- function(searchConditionsList = NULL, ConfigDistrictYearID = F, DistrictID = F, SchoolYearID = F, PreviouslyEnrolledSameEntityNoShowActionType = F, EnrolledDifferentEntityNoShowActionType = F, NoDistrictEnrollmentNoShowActionType = F, EnableNoShow = F, PriorNoShowRecord = F, ConfigDistrictYearIDClonedFrom = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, PreviouslyEnrolledSameEntityNoShowEntryDate = F, PreviouslyEnrolledSameEntityNoShowWithdrawalDate = F, EnrolledDifferentEntityNoShowEntryDate = F, EnrolledDifferentEntityNoShowWithdrawalDate = F, NoDistrictEnrollmentNoShowEntryDate = F, NoDistrictEnrollmentNoShowWithdrawalDate = F, AutoAddSchoolPathOverride = F, PermitIDAutoAdd = F, DefaultRetainInterventionPlanRecords = F, searchConditionsGroupType = "And", searchSortFieldNamesList = NULL, searchSortFieldNamesDescendingList = NULL, entityID = 1, schoolYearID = NULL, page = 1, pageSize = 100000, flatten = T, ...){

		params <- as.list(environment()) %>% append(list(...))

		searchFields <- params %>% keep(names(params) %>% str_sub(1,1) == names(params) %>% str_sub(1,1) %>% str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- "all", searchFields <- searchFields %>% keep(~.x) %>% names())

		listSkyObjects(module = "Enrollment", objectName = "ConfigDistrictYear", schoolYearID = schoolYearID, searchFields = searchFields, page = page, pageSize = pageSize, SearchConditionsList = searchConditionsList, SearchConditionsGroupType = searchConditionsGroupType, SearchSortFieldNamesList = searchSortFieldNamesList, SearchSortFieldNamesDescendingList = searchSortFieldNamesDescendingList, entityId = entityID, flatten = flatten)
	}


	listEnrollmentConfigDistrictYearWithdrawalCodes <- function(searchConditionsList = NULL, ConfigDistrictYearWithdrawalCodeID = F, ConfigDistrictYearID = F, WithdrawalCodeID = F, Type = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, ConfigDistrictYearWithdrawalCodeIDClonedFrom = F, searchConditionsGroupType = "And", searchSortFieldNamesList = NULL, searchSortFieldNamesDescendingList = NULL, entityID = 1, schoolYearID = NULL, page = 1, pageSize = 100000, flatten = T, ...){

		params <- as.list(environment()) %>% append(list(...))

		searchFields <- params %>% keep(names(params) %>% str_sub(1,1) == names(params) %>% str_sub(1,1) %>% str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- "all", searchFields <- searchFields %>% keep(~.x) %>% names())

		listSkyObjects(module = "Enrollment", objectName = "ConfigDistrictYearWithdrawalCode", schoolYearID = schoolYearID, searchFields = searchFields, page = page, pageSize = pageSize, SearchConditionsList = searchConditionsList, SearchConditionsGroupType = searchConditionsGroupType, SearchSortFieldNamesList = searchSortFieldNamesList, SearchSortFieldNamesDescendingList = searchSortFieldNamesDescendingList, entityId = entityID, flatten = flatten)
	}


	listTempStudentEntityYears <- function(searchConditionsList = NULL, TempStudentEntityYearID = F, StudentEntityYearID = F, StudentFullName = F, StudentNumber = F, GenderCode = F, GradeLevelCodeDescription = F, IsActive = F, HomeroomID = F, HomeroomDetails = F, CurrentHomeroomDetails = F, StaffIDAdvisor = F, AdvisorDetails = F, CurrentAdvisorDetails = F, Message = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, searchConditionsGroupType = "And", searchSortFieldNamesList = NULL, searchSortFieldNamesDescendingList = NULL, entityID = 1, schoolYearID = NULL, page = 1, pageSize = 100000, flatten = T, ...){

		params <- as.list(environment()) %>% append(list(...))

		searchFields <- params %>% keep(names(params) %>% str_sub(1,1) == names(params) %>% str_sub(1,1) %>% str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- "all", searchFields <- searchFields %>% keep(~.x) %>% names())

		listSkyObjects(module = "Enrollment", objectName = "TempStudentEntityYear", schoolYearID = schoolYearID, searchFields = searchFields, page = page, pageSize = pageSize, SearchConditionsList = searchConditionsList, SearchConditionsGroupType = searchConditionsGroupType, SearchSortFieldNamesList = searchSortFieldNamesList, SearchSortFieldNamesDescendingList = searchSortFieldNamesDescendingList, entityId = entityID, flatten = flatten)
	}


	listEntryWithdrawals <- function(searchConditionsList = NULL, EntryWithdrawalMNID = F, StateDistrictMNID = F, StateAidCategoryCodeMNID = F, StateLastAttendanceLocationCodeMNID = F, IsPostSecondaryOption = F, IsIndependentStudy = F, AttendanceDays = F, MembershipDays = F, SpecialEdServiceHours = F, TotalMembershipDays = F, EntryWithdrawalID = F, StudentID = F, EntityID = F, SchoolYearID = F, StartDate = F, EntryCodeID = F, EntryComment = F, SchoolID = F, StudentTypeID = F, GradeReferenceID = F, EntryWithdrawalIDStatusChangePrevious = F, PromotionStatus = F, RenderUndoStatusChangeOption = F, RenderDeleteOption = F, RenderPrintWithdrawalFormOption = F, RenderNoShowOption = F, PercentEnrolled = F, WithdrawalCodeID = F, WithdrawalComment = F, EndDate = F, IsDefaultEntity = F, CalendarID = F, IsNoShow = F, IsCurrentOrFutureEnrollment = F, StatusChangeEntry = F, StatusChangeWithdrawal = F, EnrolledAtLeastOneDay = F, IsStartDateOnOrAfterFirstDayOfSchool = F, IsCrossEntityCourseEnrollment = F, IsHistoricalEnrollment = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, PSEOHours = F, IsPSEOConcurrentEnrollment = F, HasMessageCenterAllowedWithdrawalCodeOverride = F, WithdrawalDate = F, IsCombinedEnrollmentFullTime = F, RenderStatusChangeOption = F, RenderWithdrawalOption = F, searchConditionsGroupType = "And", searchSortFieldNamesList = NULL, searchSortFieldNamesDescendingList = NULL, entityID = 1, schoolYearID = NULL, page = 1, pageSize = 100000, flatten = T, ...){

		params <- as.list(environment()) %>% append(list(...))

		searchFields <- params %>% keep(names(params) %>% str_sub(1,1) == names(params) %>% str_sub(1,1) %>% str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- "all", searchFields <- searchFields %>% keep(~.x) %>% names())

		listSkyObjects(module = "Enrollment", objectName = "EntryWithdrawal", schoolYearID = schoolYearID, searchFields = searchFields, page = page, pageSize = pageSize, SearchConditionsList = searchConditionsList, SearchConditionsGroupType = searchConditionsGroupType, SearchSortFieldNamesList = searchSortFieldNamesList, SearchSortFieldNamesDescendingList = searchSortFieldNamesDescendingList, entityId = entityID, flatten = flatten)
	}


	listSchools <- function(searchConditionsList = NULL, SchoolMNID = F, SchoolNumber = F, StateKindergartenScheduleIndicatorCodeMNID = F, StateTitleISchoolIndicatorCodeMNID = F, IsTitleIII = F, SchoolID = F, DistrictID = F, Code = F, Name = F, GradeLevelIDLow = F, GradeLevelIDHigh = F, PhoneNumber = F, PhoneNumberIsInternational = F, FaxNumber = F, FaxNumberIsInternational = F, StaffIDPrincipal = F, BuildingID = F, Type = F, FormattedPhoneNumber = F, FormattedFaxNumber = F, SchoolYearID = F, SchoolIDClonedFrom = F, CampusAccountabilityRatingID = F, EdFiSchoolCategoryID = F, FederalAlternativeSchoolDetailID = F, FederalJusticeFacilityTypeID = F, ExcludeFromCRDC = F, IsNonLEA = F, HasUngraded = F, HasPreschoolNonIDEAAge3 = F, HasPreschoolNonIDEAAge4 = F, HasPreschoolNonIDEAAge5 = F, HasUngradedMainlyElementary = F, HasUngradedMainlyMiddleSchool = F, HasUngradedMainlyHighSchool = F, IsSpecialEducation = F, IsMagnet = F, IsCharter = F, IsAlternative = F, IsEntireSchoolMagnet = F, HasGifted = F, HasIBDiplomaProgramme = F, HasAPCourses = F, HasAPSelfSelection = F, HasDualEnrollment = F, HasSingleSexClasses = F, HasCreditRecovery = F, HasSingleSexAthletics = F, HasCorporalPunishment = F, DaysInRegularSchoolYear = F, EducationalProgramHoursPerWeek = F, DaysPriorForAlgebraICounts = F, IsCRDCCollectedForSchoolYear = F, CodeName = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, IsALCSchool = F, SchoolIDClonedTo = F, HasSafetyPlan = F, HasZeroTolerance = F, HasAntiViolence = F, HasAntiBullying = F, HasAlcoholDrugEducation = F, HasCrisisPlan = F, NameIDSafetySpecialist = F, HasShootingOccurred = F, HasHomicideOccurred = F, FederalNCESSchoolID = F, EdFiSchoolID = F, HasFiberOptic = F, HasWiFi = F, AllowsSchoolDevices = F, AllowsStudentDevices = F, NumberWiFiDevices = F, IsCEP = F, StateSchoolMNID = F, CEEBCode = F, IsTitleISchoolwide = F, StateAssignedID = F, searchConditionsGroupType = "And", searchSortFieldNamesList = NULL, searchSortFieldNamesDescendingList = NULL, entityID = 1, schoolYearID = NULL, page = 1, pageSize = 100000, flatten = T, ...){

		params <- as.list(environment()) %>% append(list(...))

		searchFields <- params %>% keep(names(params) %>% str_sub(1,1) == names(params) %>% str_sub(1,1) %>% str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- "all", searchFields <- searchFields %>% keep(~.x) %>% names())

		listSkyObjects(module = "Enrollment", objectName = "School", schoolYearID = schoolYearID, searchFields = searchFields, page = page, pageSize = pageSize, SearchConditionsList = searchConditionsList, SearchConditionsGroupType = searchConditionsGroupType, SearchSortFieldNamesList = searchSortFieldNamesList, SearchSortFieldNamesDescendingList = searchSortFieldNamesDescendingList, entityId = entityID, flatten = flatten)
	}


	listWithdrawalCodes <- function(searchConditionsList = NULL, WithdrawalCodeMNID = F, StateStatusEndCodeMNID = F, WithdrawalCodeID = F, DistrictID = F, Code = F, Description = F, SchoolYearID = F, Type = F, DistrictGroupKey = F, WithdrawalCodeIDClonedFrom = F, CodeDescription = F, IsCrossEntityCourseEnrollment = F, EdFiExitWithdrawID = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, EdFiExitWithdrawTypeDescriptorID = F, searchConditionsGroupType = "And", searchSortFieldNamesList = NULL, searchSortFieldNamesDescendingList = NULL, entityID = 1, schoolYearID = NULL, page = 1, pageSize = 100000, flatten = T, ...){

		params <- as.list(environment()) %>% append(list(...))

		searchFields <- params %>% keep(names(params) %>% str_sub(1,1) == names(params) %>% str_sub(1,1) %>% str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- "all", searchFields <- searchFields %>% keep(~.x) %>% names())

		listSkyObjects(module = "Enrollment", objectName = "WithdrawalCode", schoolYearID = schoolYearID, searchFields = searchFields, page = page, pageSize = pageSize, SearchConditionsList = searchConditionsList, SearchConditionsGroupType = searchConditionsGroupType, SearchSortFieldNamesList = searchSortFieldNamesList, SearchSortFieldNamesDescendingList = searchSortFieldNamesDescendingList, entityId = entityID, flatten = flatten)
	}


	listTempStudentEnrollmentRecords <- function(searchConditionsList = NULL, TempStudentEnrollmentRecordID = F, StudentID = F, StudentFullName = F, StudentNumber = F, GradeReferenceID = F, GradYear = F, GradeLevelCode = F, IsCurrentActive = F, EntityID = F, EntityCode = F, SchoolYearID = F, NumericYear = F, StartDate = F, EndDate = F, EntryCodeID = F, EntryCode = F, SchoolID = F, SchoolCode = F, CalendarID = F, CalendarCode = F, PercentEnrolled = F, IsDefaultEntityForEntryWithdrawal = F, IsDefaultEntityForStudentEntityYear = F, StudentTypeID = F, StudentTypeCode = F, EntryComment = F, StaffIDAdvisor = F, AdvisorFullName = F, StaffIDDisciplineOfficer = F, DisciplineOfficerFullName = F, HomeroomID = F, HomeroomCode = F, ExcludeFromHonorRoll = F, ExcludeFromRank = F, FailureReason = F, OutgoingStudent = F, WithdrawalCode = F, WithdrawalCodeID = F, WithdrawalComment = F, EntryWithdrawalID = F, IsTuitionPaidOutOfDistrict = F, TestingSchoolCode = F, TestingSchoolCodeDisplayName = F, GSAADAClaimableOverrideCode = F, GSAADAClaimableOverrideCodeDisplayName = F, ServingRCDTSOverrideID = F, ServingRCDTSOverrideCode = F, ServingRCDTSOverrideCodeDisplayName = F, TestingSchoolRCDTSOverrideID = F, TestingSchoolRCDTSOverrideCode = F, TestingSchoolRCDTSOverrideCodeDisplayName = F, IsPermanentExit = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, IncludeAsProspectiveRank = F, StateDistrictMNID = F, CreateFeeManagementCustomer = F, CreateFeeManagementCustomerEntityYear = F, FeeManagementCustomerID = F, StateDistrictMNCodeName = F, StateAidCategoryMNID = F, StateLastAttendanceLocationCodeMNID = F, EdFiDistrictIDResidence = F, EdFiDistrictResidenceCodeDescription = F, ExcludeFromThirdFridaySeptemberCount = F, CompletedSchoolYearOverride = F, SourceEntryWithdrawalID = F, EnrollIntoEntityCode = F, TotalStudentCourseRequestCount = F, StudentCourseRequestNotMoveableCount = F, ScheduledSectionCount = F, EnrollmentMoveable = F, StudentCourseRequestToDeleteCount = F, IsPrivateSchoolChoiceStudent = F, EdFiDistrictIDTransfer = F, EdFiSchoolIDTransfer = F, PromotionStatus = F, Error = F, ErrorCount = F, ProcessEntryWithdrawal = F, HomeRCDTSOverrideID = F, HomeRCDTSOverrideCode = F, HomeRCDTSOverrideCodeDisplayName = F, searchConditionsGroupType = "And", searchSortFieldNamesList = NULL, searchSortFieldNamesDescendingList = NULL, entityID = 1, schoolYearID = NULL, page = 1, pageSize = 100000, flatten = T, ...){

		params <- as.list(environment()) %>% append(list(...))

		searchFields <- params %>% keep(names(params) %>% str_sub(1,1) == names(params) %>% str_sub(1,1) %>% str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- "all", searchFields <- searchFields %>% keep(~.x) %>% names())

		listSkyObjects(module = "Enrollment", objectName = "TempStudentEnrollmentRecord", schoolYearID = schoolYearID, searchFields = searchFields, page = page, pageSize = pageSize, SearchConditionsList = searchConditionsList, SearchConditionsGroupType = searchConditionsGroupType, SearchSortFieldNamesList = searchSortFieldNamesList, SearchSortFieldNamesDescendingList = searchSortFieldNamesDescendingList, entityId = entityID, flatten = flatten)
	}


	listTempAffectedWithdrawalRecords <- function(searchConditionsList = NULL, TempAffectedWithdrawalRecordID = F, AffectedPrimaryKey = F, StudentID = F, EntityID = F, SchoolYearID = F, NameIDRequestedBy = F, CourseID = F, Description = F, SectionID = F, Section = F, StartDate = F, EndDate = F, NewEndDate = F, HasGrades = F, HasFutureGrades = F, HasAttendance = F, HasFutureAttendance = F, HasPartiallyPaidFees = F, ActionMessage = F, MostFutureGradeStartDate = F, RecordType = F, Action = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, IsFutureEntryWithdrawal = F, HasTransactionPreventingStudentSectionDelete = F, ParentPrimaryKey = F, IsNoShowDeleteNoOtherEnrollmentInTheDistrict = F, searchConditionsGroupType = "And", searchSortFieldNamesList = NULL, searchSortFieldNamesDescendingList = NULL, entityID = 1, schoolYearID = NULL, page = 1, pageSize = 100000, flatten = T, ...){

		params <- as.list(environment()) %>% append(list(...))

		searchFields <- params %>% keep(names(params) %>% str_sub(1,1) == names(params) %>% str_sub(1,1) %>% str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- "all", searchFields <- searchFields %>% keep(~.x) %>% names())

		listSkyObjects(module = "Enrollment", objectName = "TempAffectedWithdrawalRecord", schoolYearID = schoolYearID, searchFields = searchFields, page = page, pageSize = pageSize, SearchConditionsList = searchConditionsList, SearchConditionsGroupType = searchConditionsGroupType, SearchSortFieldNamesList = searchSortFieldNamesList, SearchSortFieldNamesDescendingList = searchSortFieldNamesDescendingList, entityId = entityID, flatten = flatten)
	}


	listEnrollmentConfigDistricts <- function(searchConditionsList = NULL, ConfigDistrictID = F, DistrictID = F, EntryDaysBeforeCalendarStart = F, WithdrawalDaysAfterCalendarEnd = F, NumberDaysBackdateEntry = F, NumberDaysBackdateWithdrawal = F, AllowDualEnrollment = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, searchConditionsGroupType = "And", searchSortFieldNamesList = NULL, searchSortFieldNamesDescendingList = NULL, entityID = 1, schoolYearID = NULL, page = 1, pageSize = 100000, flatten = T, ...){

		params <- as.list(environment()) %>% append(list(...))

		searchFields <- params %>% keep(names(params) %>% str_sub(1,1) == names(params) %>% str_sub(1,1) %>% str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- "all", searchFields <- searchFields %>% keep(~.x) %>% names())

		listSkyObjects(module = "Enrollment", objectName = "ConfigDistrict", schoolYearID = schoolYearID, searchFields = searchFields, page = page, pageSize = pageSize, SearchConditionsList = searchConditionsList, SearchConditionsGroupType = searchConditionsGroupType, SearchSortFieldNamesList = searchSortFieldNamesList, SearchSortFieldNamesDescendingList = searchSortFieldNamesDescendingList, entityId = entityID, flatten = flatten)
	}


	listEntitySchools <- function(searchConditionsList = NULL, EntitySchoolID = F, EntityID = F, SchoolID = F, IsDefaultSchoolForEntity = F, IsDefaultEntityForSchool = F, EntitySchoolIDClonedFrom = F, IsOnlySchoolInEntity = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, EntitySchoolIDClonedTo = F, searchConditionsGroupType = "And", searchSortFieldNamesList = NULL, searchSortFieldNamesDescendingList = NULL, entityID = 1, schoolYearID = NULL, page = 1, pageSize = 100000, flatten = T, ...){

		params <- as.list(environment()) %>% append(list(...))

		searchFields <- params %>% keep(names(params) %>% str_sub(1,1) == names(params) %>% str_sub(1,1) %>% str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- "all", searchFields <- searchFields %>% keep(~.x) %>% names())

		listSkyObjects(module = "Enrollment", objectName = "EntitySchool", schoolYearID = schoolYearID, searchFields = searchFields, page = page, pageSize = pageSize, SearchConditionsList = searchConditionsList, SearchConditionsGroupType = searchConditionsGroupType, SearchSortFieldNamesList = searchSortFieldNamesList, SearchSortFieldNamesDescendingList = searchSortFieldNamesDescendingList, entityId = entityID, flatten = flatten)
	}


	listEntryCodes <- function(searchConditionsList = NULL, EntryCodeID = F, DistrictID = F, Code = F, Description = F, SchoolYearID = F, Type = F, DistrictGroupKey = F, EntryCodeIDClonedFrom = F, IsCrossEntityCourseEnrollment = F, CodeDescription = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, EdFiEntryTypeDescriptorID = F, searchConditionsGroupType = "And", searchSortFieldNamesList = NULL, searchSortFieldNamesDescendingList = NULL, entityID = 1, schoolYearID = NULL, page = 1, pageSize = 100000, flatten = T, ...){

		params <- as.list(environment()) %>% append(list(...))

		searchFields <- params %>% keep(names(params) %>% str_sub(1,1) == names(params) %>% str_sub(1,1) %>% str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- "all", searchFields <- searchFields %>% keep(~.x) %>% names())

		listSkyObjects(module = "Enrollment", objectName = "EntryCode", schoolYearID = schoolYearID, searchFields = searchFields, page = page, pageSize = pageSize, SearchConditionsList = searchConditionsList, SearchConditionsGroupType = searchConditionsGroupType, SearchSortFieldNamesList = searchSortFieldNamesList, SearchSortFieldNamesDescendingList = searchSortFieldNamesDescendingList, entityId = entityID, flatten = flatten)
	}


	listGradeLevels <- function(searchConditionsList = NULL, GradeLevelID = F, DistrictID = F, Code = F, Description = F, NumericValue = F, DistrictGroupKey = F, FederalGradeLevel = F, CodeDescription = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, StateGradeLevel = F, CommonEducationDataStandardsGradeLevelID = F, IlluminateOverride = F, EdFiGradeLevelDescriptorID = F, searchConditionsGroupType = "And", searchSortFieldNamesList = NULL, searchSortFieldNamesDescendingList = NULL, entityID = 1, schoolYearID = NULL, page = 1, pageSize = 100000, flatten = T, ...){

		params <- as.list(environment()) %>% append(list(...))

		searchFields <- params %>% keep(names(params) %>% str_sub(1,1) == names(params) %>% str_sub(1,1) %>% str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- "all", searchFields <- searchFields %>% keep(~.x) %>% names())

		listSkyObjects(module = "Enrollment", objectName = "GradeLevel", schoolYearID = schoolYearID, searchFields = searchFields, page = page, pageSize = pageSize, SearchConditionsList = searchConditionsList, SearchConditionsGroupType = searchConditionsGroupType, SearchSortFieldNamesList = searchSortFieldNamesList, SearchSortFieldNamesDescendingList = searchSortFieldNamesDescendingList, entityId = entityID, flatten = flatten)
	}


	listGradeReferences <- function(searchConditionsList = NULL, GradeReferenceID = F, GradeLevelID = F, GradYear = F, SchoolYearID = F, DistrictGroupKey = F, MinutesPresentHalfDay = F, MinutesPresentFullDay = F, GradeReferenceIDClonedFrom = F, GradeReferenceIDClonedTo = F, StateGradeLevel = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, GradeReferenceMNID = F, StateSTARGradeLevelMNID = F, GradeLevelCode = F, searchConditionsGroupType = "And", searchSortFieldNamesList = NULL, searchSortFieldNamesDescendingList = NULL, entityID = 1, schoolYearID = NULL, page = 1, pageSize = 100000, flatten = T, ...){

		params <- as.list(environment()) %>% append(list(...))

		searchFields <- params %>% keep(names(params) %>% str_sub(1,1) == names(params) %>% str_sub(1,1) %>% str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- "all", searchFields <- searchFields %>% keep(~.x) %>% names())

		listSkyObjects(module = "Enrollment", objectName = "GradeReference", schoolYearID = schoolYearID, searchFields = searchFields, page = page, pageSize = pageSize, SearchConditionsList = searchConditionsList, SearchConditionsGroupType = searchConditionsGroupType, SearchSortFieldNamesList = searchSortFieldNamesList, SearchSortFieldNamesDescendingList = searchSortFieldNamesDescendingList, entityId = entityID, flatten = flatten)
	}


	listHomerooms <- function(searchConditionsList = NULL, HomeroomID = F, EntityID = F, Code = F, StaffID = F, RoomID = F, SchoolYearID = F, HomeroomIDClonedFrom = F, HomeroomIDClonedTo = F, HomeroomDetails = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, searchConditionsGroupType = "And", searchSortFieldNamesList = NULL, searchSortFieldNamesDescendingList = NULL, entityID = 1, schoolYearID = NULL, page = 1, pageSize = 100000, flatten = T, ...){

		params <- as.list(environment()) %>% append(list(...))

		searchFields <- params %>% keep(names(params) %>% str_sub(1,1) == names(params) %>% str_sub(1,1) %>% str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- "all", searchFields <- searchFields %>% keep(~.x) %>% names())

		listSkyObjects(module = "Enrollment", objectName = "Homeroom", schoolYearID = schoolYearID, searchFields = searchFields, page = page, pageSize = pageSize, SearchConditionsList = searchConditionsList, SearchConditionsGroupType = searchConditionsGroupType, SearchSortFieldNamesList = searchSortFieldNamesList, SearchSortFieldNamesDescendingList = searchSortFieldNamesDescendingList, entityId = entityID, flatten = flatten)
	}


	listStudentEntityYears <- function(searchConditionsList = NULL, StudentEntityYearID = F, StudentID = F, EntityID = F, SchoolYearID = F, StaffIDAdvisor = F, HomeroomID = F, IsActive = F, IsDefaultEntity = F, SchedulingTeamID = F, StaffIDDisciplineOfficer = F, ExcludeFromHonorRoll = F, ExcludeFromRank = F, EntryWithdrawalIDLatest = F, SignedAcceptableUsePolicy = F, ChromebookDocumentsReturned = F, HandbookSigned = F, UILFeeReceived = F, OptOutOfMedia = F, IsTransportationRequested = F, IsCrossEntityCourseEnrollment = F, FeeChargeAmount = F, FeePaidAndWaivedAmount = F, FeePaidAmount = F, FeeWaivedAmount = F, FeeUnappliedAmount = F, FeeAmountDue = F, HasNoAttendanceToday = F, DaysAbsentYTD = F, DaysExcusedYTD = F, DaysOtherYTD = F, DaysUnexcusedYTD = F, TardyCountYTD = F, TardyKioskTotals = F, DaysEnrolledYTD = F, HasOpenDisplayPeriodsInRegularSchoolDay = F, HasOverscheduledPeriod = F, WithdrawalDate = F, TotalEarnedCreditsPossibleAnticipatedNonTransferStudentSectionsNonAlternateRequestCredits = F, TotalMissedAssignmentCount = F, HasMissingAssignments = F, HasValidStudentPlan = F, HasFlaggedMissingAssignments = F, FlaggedMissingAssignmentsCount = F, HasConflictedStudentCourseRequest = F, SchedulingCategories = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, HasActiveEndorsementDeclarationTimePeriod = F, IncludeAsProspectiveRank = F, FirstName = F, MiddleName = F, LastName = F, NameID = F, HasActiveCareerPlanDeclarationTimePeriod = F, SectionLengthEnrolled = F, Semester2Enrolled = F, SectionLengthAbsent = F, Semester2Absent = F, Grade = F, StudentNumber = F, HomeroomCodeFollettDestiny = F, HomeroomPeriodFollettDestiny = F, HomeroomStaffNameFollettDestiny = F, NumberOfStudentCourseRequests = F, NumberOfStudentSections = F, ExistsConflictedStudentCourseRequests = F, UnscheduledStudentCourseRequestCount = F, ExistsUnscheduleableStudentSections = F, UnscheduleableStudentSectionCount = F, CurrentPercentEnrolled = F, HasNonCrossEntityCourseSchedulingEntryWithdrawal = F, SchoolIDPathExpectedSchool = F, searchConditionsGroupType = "And", searchSortFieldNamesList = NULL, searchSortFieldNamesDescendingList = NULL, entityID = 1, schoolYearID = NULL, page = 1, pageSize = 100000, flatten = T, ...){

		params <- as.list(environment()) %>% append(list(...))

		searchFields <- params %>% keep(names(params) %>% str_sub(1,1) == names(params) %>% str_sub(1,1) %>% str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- "all", searchFields <- searchFields %>% keep(~.x) %>% names())

		listSkyObjects(module = "Enrollment", objectName = "StudentEntityYear", schoolYearID = schoolYearID, searchFields = searchFields, page = page, pageSize = pageSize, SearchConditionsList = searchConditionsList, SearchConditionsGroupType = searchConditionsGroupType, SearchSortFieldNamesList = searchSortFieldNamesList, SearchSortFieldNamesDescendingList = searchSortFieldNamesDescendingList, entityId = entityID, flatten = flatten)
	}


	listStudentTypes <- function(searchConditionsList = NULL, StudentTypeID = F, DistrictID = F, Code = F, Description = F, DistrictGroupKey = F, CodeDescription = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, searchConditionsGroupType = "And", searchSortFieldNamesList = NULL, searchSortFieldNamesDescendingList = NULL, entityID = 1, schoolYearID = NULL, page = 1, pageSize = 100000, flatten = T, ...){

		params <- as.list(environment()) %>% append(list(...))

		searchFields <- params %>% keep(names(params) %>% str_sub(1,1) == names(params) %>% str_sub(1,1) %>% str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- "all", searchFields <- searchFields %>% keep(~.x) %>% names())

		listSkyObjects(module = "Enrollment", objectName = "StudentType", schoolYearID = schoolYearID, searchFields = searchFields, page = page, pageSize = pageSize, SearchConditionsList = searchConditionsList, SearchConditionsGroupType = searchConditionsGroupType, SearchSortFieldNamesList = searchSortFieldNamesList, SearchSortFieldNamesDescendingList = searchSortFieldNamesDescendingList, entityId = entityID, flatten = flatten)
	}


	listPermits <- function(searchConditionsList = NULL, PermitID = F, DistrictID = F, Code = F, Description = F, CodeDescription = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, AllowSchoolPathAssignment = F, SchoolYearID = F, PermitIDClonedFrom = F, PermitIDClonedTo = F, searchConditionsGroupType = "And", searchSortFieldNamesList = NULL, searchSortFieldNamesDescendingList = NULL, entityID = 1, schoolYearID = NULL, page = 1, pageSize = 100000, flatten = T, ...){

		params <- as.list(environment()) %>% append(list(...))

		searchFields <- params %>% keep(names(params) %>% str_sub(1,1) == names(params) %>% str_sub(1,1) %>% str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- "all", searchFields <- searchFields %>% keep(~.x) %>% names())

		listSkyObjects(module = "Enrollment", objectName = "Permit", schoolYearID = schoolYearID, searchFields = searchFields, page = page, pageSize = pageSize, SearchConditionsList = searchConditionsList, SearchConditionsGroupType = searchConditionsGroupType, SearchSortFieldNamesList = searchSortFieldNamesList, SearchSortFieldNamesDescendingList = searchSortFieldNamesDescendingList, entityId = entityID, flatten = flatten)
	}


	listEntitySchoolBuildings <- function(searchConditionsList = NULL, EntitySchoolBuildingID = F, BuildingID = F, SchoolYearID = F, EntitySchoolID = F, IsPrimary = F, EntitySchoolBuildingIDClonedFrom = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, searchConditionsGroupType = "And", searchSortFieldNamesList = NULL, searchSortFieldNamesDescendingList = NULL, entityID = 1, schoolYearID = NULL, page = 1, pageSize = 100000, flatten = T, ...){

		params <- as.list(environment()) %>% append(list(...))

		searchFields <- params %>% keep(names(params) %>% str_sub(1,1) == names(params) %>% str_sub(1,1) %>% str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- "all", searchFields <- searchFields %>% keep(~.x) %>% names())

		listSkyObjects(module = "Enrollment", objectName = "EntitySchoolBuilding", schoolYearID = schoolYearID, searchFields = searchFields, page = page, pageSize = pageSize, SearchConditionsList = searchConditionsList, SearchConditionsGroupType = searchConditionsGroupType, SearchSortFieldNamesList = searchSortFieldNamesList, SearchSortFieldNamesDescendingList = searchSortFieldNamesDescendingList, entityId = entityID, flatten = flatten)
	}


	listSchoolPathSchoolOverrides <- function(searchConditionsList = NULL, SchoolPathSchoolOverrideID = F, StudentID = F, SchoolID = F, Order = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, searchConditionsGroupType = "And", searchSortFieldNamesList = NULL, searchSortFieldNamesDescendingList = NULL, entityID = 1, schoolYearID = NULL, page = 1, pageSize = 100000, flatten = T, ...){

		params <- as.list(environment()) %>% append(list(...))

		searchFields <- params %>% keep(names(params) %>% str_sub(1,1) == names(params) %>% str_sub(1,1) %>% str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- "all", searchFields <- searchFields %>% keep(~.x) %>% names())

		listSkyObjects(module = "Enrollment", objectName = "SchoolPathSchoolOverride", schoolYearID = schoolYearID, searchFields = searchFields, page = page, pageSize = pageSize, SearchConditionsList = searchConditionsList, SearchConditionsGroupType = searchConditionsGroupType, SearchSortFieldNamesList = searchSortFieldNamesList, SearchSortFieldNamesDescendingList = searchSortFieldNamesDescendingList, entityId = entityID, flatten = flatten)
	}


	listTempNoShowEntryWithdrawals <- function(searchConditionsList = NULL, TempNoShowEntryWithdrawalID = F, EntryWithdrawalID = F, StudentID = F, Student = F, GradeLevel = F, Entity = F, SchoolYear = F, StartDate = F, EndDate = F, EntryCode = F, WithdrawalCode = F, WithdrawalCodeID = F, NoShowTypeOfNoShow = F, NoShowAction = F, NoShowEntryWithdrawalType = F, DisplayAction = F, FailureReason = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, StudentNumber = F, SchoolYearID = F, AttemptToUpdateWithdrawalCode = F, searchConditionsGroupType = "And", searchSortFieldNamesList = NULL, searchSortFieldNamesDescendingList = NULL, entityID = 1, schoolYearID = NULL, page = 1, pageSize = 100000, flatten = T, ...){

		params <- as.list(environment()) %>% append(list(...))

		searchFields <- params %>% keep(names(params) %>% str_sub(1,1) == names(params) %>% str_sub(1,1) %>% str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- "all", searchFields <- searchFields %>% keep(~.x) %>% names())

		listSkyObjects(module = "Enrollment", objectName = "TempNoShowEntryWithdrawal", schoolYearID = schoolYearID, searchFields = searchFields, page = page, pageSize = pageSize, SearchConditionsList = searchConditionsList, SearchConditionsGroupType = searchConditionsGroupType, SearchSortFieldNamesList = searchSortFieldNamesList, SearchSortFieldNamesDescendingList = searchSortFieldNamesDescendingList, entityId = entityID, flatten = flatten)
	}


	listTempNameAddressMoveSchoolPathSchoolOverrides <- function(searchConditionsList = NULL, TempNameAddressMoveSchoolPathSchoolOverrideID = F, StudentFullNameLFM = F, SchoolNameToOverride = F, SchoolNameOverriddingTo = F, StudentID = F, SchoolID = F, SchoolPathSchoolOverrideID = F, Order = F, IsRemoveOverride = F, IsUpdateOverride = F, IsRemovePermit = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, IsOverrideExists = F, IsCreateOverride = F, SchoolYearDescription = F, PermitID = F, PermitSchoolYearID = F, IsPermitOptional = F, searchConditionsGroupType = "And", searchSortFieldNamesList = NULL, searchSortFieldNamesDescendingList = NULL, entityID = 1, schoolYearID = NULL, page = 1, pageSize = 100000, flatten = T, ...){

		params <- as.list(environment()) %>% append(list(...))

		searchFields <- params %>% keep(names(params) %>% str_sub(1,1) == names(params) %>% str_sub(1,1) %>% str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- "all", searchFields <- searchFields %>% keep(~.x) %>% names())

		listSkyObjects(module = "Enrollment", objectName = "TempNameAddressMoveSchoolPathSchoolOverride", schoolYearID = schoolYearID, searchFields = searchFields, page = page, pageSize = pageSize, SearchConditionsList = searchConditionsList, SearchConditionsGroupType = searchConditionsGroupType, SearchSortFieldNamesList = searchSortFieldNamesList, SearchSortFieldNamesDescendingList = searchSortFieldNamesDescendingList, entityId = entityID, flatten = flatten)
	}


	listTempSchoolPathSchoolOverrides <- function(searchConditionsList = NULL, TempSchoolPathSchoolOverrideID = F, StudentID = F, StudentName = F, SchoolID = F, SchoolIDClonedTo = F, SchoolCodeName = F, Order = F, HasExceptions = F, ExceptionNote = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, DistrictID = F, PermitID = F, PermitCodeDescription = F, SchoolYearID = F, searchConditionsGroupType = "And", searchSortFieldNamesList = NULL, searchSortFieldNamesDescendingList = NULL, entityID = 1, schoolYearID = NULL, page = 1, pageSize = 100000, flatten = T, ...){

		params <- as.list(environment()) %>% append(list(...))

		searchFields <- params %>% keep(names(params) %>% str_sub(1,1) == names(params) %>% str_sub(1,1) %>% str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- "all", searchFields <- searchFields %>% keep(~.x) %>% names())

		listSkyObjects(module = "Enrollment", objectName = "TempSchoolPathSchoolOverride", schoolYearID = schoolYearID, searchFields = searchFields, page = page, pageSize = pageSize, SearchConditionsList = searchConditionsList, SearchConditionsGroupType = searchConditionsGroupType, SearchSortFieldNamesList = searchSortFieldNamesList, SearchSortFieldNamesDescendingList = searchSortFieldNamesDescendingList, entityId = entityID, flatten = flatten)
	}


	listTempHomeroomErrors <- function(searchConditionsList = NULL, TempHomeroomErrorID = F, TempHomeroomRecordID = F, Code = F, FailureReason = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, searchConditionsGroupType = "And", searchSortFieldNamesList = NULL, searchSortFieldNamesDescendingList = NULL, entityID = 1, schoolYearID = NULL, page = 1, pageSize = 100000, flatten = T, ...){

		params <- as.list(environment()) %>% append(list(...))

		searchFields <- params %>% keep(names(params) %>% str_sub(1,1) == names(params) %>% str_sub(1,1) %>% str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- "all", searchFields <- searchFields %>% keep(~.x) %>% names())

		listSkyObjects(module = "Enrollment", objectName = "TempHomeroomError", schoolYearID = schoolYearID, searchFields = searchFields, page = page, pageSize = pageSize, SearchConditionsList = searchConditionsList, SearchConditionsGroupType = searchConditionsGroupType, SearchSortFieldNamesList = searchSortFieldNamesList, SearchSortFieldNamesDescendingList = searchSortFieldNamesDescendingList, entityId = entityID, flatten = flatten)
	}


	listTempHomeroomRecords <- function(searchConditionsList = NULL, TempHomeroomRecordID = F, HomeroomID = F, Code = F, StaffID = F, Staff = F, RoomID = F, Room = F, BuildingID = F, Building = F, SchoolYearID = F, SchoolYear = F, IsOverwrite = F, HasSaveError = F, ColumnIndex = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, searchConditionsGroupType = "And", searchSortFieldNamesList = NULL, searchSortFieldNamesDescendingList = NULL, entityID = 1, schoolYearID = NULL, page = 1, pageSize = 100000, flatten = T, ...){

		params <- as.list(environment()) %>% append(list(...))

		searchFields <- params %>% keep(names(params) %>% str_sub(1,1) == names(params) %>% str_sub(1,1) %>% str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- "all", searchFields <- searchFields %>% keep(~.x) %>% names())

		listSkyObjects(module = "Enrollment", objectName = "TempHomeroomRecord", schoolYearID = schoolYearID, searchFields = searchFields, page = page, pageSize = pageSize, SearchConditionsList = searchConditionsList, SearchConditionsGroupType = searchConditionsGroupType, SearchSortFieldNamesList = searchSortFieldNamesList, SearchSortFieldNamesDescendingList = searchSortFieldNamesDescendingList, entityId = entityID, flatten = flatten)
	}


	listStudentAccountsMAS <- function(searchConditionsList = NULL, StudentAccountsMAID = F, PaymentPlanMAID = F, FinancialAid = F, iPadLease = F, StudentID = F, ReligionID = F, PlaceofWorship = F, FacultyStaffChild = F, EthnicityMAID = F, NYDepositPaid = F, AMTransportationID = F, PMTransportationID = F, SchoolDistrictID = F, searchConditionsGroupType = "And", searchSortFieldNamesList = NULL, searchSortFieldNamesDescendingList = NULL, entityID = 1, schoolYearID = NULL, page = 1, pageSize = 100000, flatten = T, ...){

		params <- as.list(environment()) %>% append(list(...))

		searchFields <- params %>% keep(names(params) %>% str_sub(1,1) == names(params) %>% str_sub(1,1) %>% str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- "all", searchFields <- searchFields %>% keep(~.x) %>% names())

		listSkyObjects(module = "Enrollment", objectName = "StudentAccountsMA", schoolYearID = schoolYearID, searchFields = searchFields, page = page, pageSize = pageSize, SearchConditionsList = searchConditionsList, SearchConditionsGroupType = searchConditionsGroupType, SearchSortFieldNamesList = searchSortFieldNamesList, SearchSortFieldNamesDescendingList = searchSortFieldNamesDescendingList, entityId = entityID, flatten = flatten)
	}


	listPaymentPlanMAS <- function(searchConditionsList = NULL, PaymentPlanMAID = F, Code = F, Description = F, searchConditionsGroupType = "And", searchSortFieldNamesList = NULL, searchSortFieldNamesDescendingList = NULL, entityID = 1, schoolYearID = NULL, page = 1, pageSize = 100000, flatten = T, ...){

		params <- as.list(environment()) %>% append(list(...))

		searchFields <- params %>% keep(names(params) %>% str_sub(1,1) == names(params) %>% str_sub(1,1) %>% str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- "all", searchFields <- searchFields %>% keep(~.x) %>% names())

		listSkyObjects(module = "Enrollment", objectName = "PaymentPlanMA", schoolYearID = schoolYearID, searchFields = searchFields, page = page, pageSize = pageSize, SearchConditionsList = searchConditionsList, SearchConditionsGroupType = searchConditionsGroupType, SearchSortFieldNamesList = searchSortFieldNamesList, SearchSortFieldNamesDescendingList = searchSortFieldNamesDescendingList, entityId = entityID, flatten = flatten)
	}


	listNumberedStudentEntityYearForDistrictAndSchoolYears <- function(searchConditionsList = NULL, DistrictID = F, EntityID = F, IsDefaultEntity = F, SchoolYearID = F, StudentDistrictRowNumber = F, StudentID = F, StudentEntityYearID = F, searchConditionsGroupType = "And", searchSortFieldNamesList = NULL, searchSortFieldNamesDescendingList = NULL, entityID = 1, schoolYearID = NULL, page = 1, pageSize = 100000, flatten = T, ...){

		params <- as.list(environment()) %>% append(list(...))

		searchFields <- params %>% keep(names(params) %>% str_sub(1,1) == names(params) %>% str_sub(1,1) %>% str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- "all", searchFields <- searchFields %>% keep(~.x) %>% names())

		listSkyObjects(module = "Enrollment", objectName = "NumberedStudentEntityYearForDistrictAndSchoolYear", schoolYearID = schoolYearID, searchFields = searchFields, page = page, pageSize = pageSize, SearchConditionsList = searchConditionsList, SearchConditionsGroupType = searchConditionsGroupType, SearchSortFieldNamesList = searchSortFieldNamesList, SearchSortFieldNamesDescendingList = searchSortFieldNamesDescendingList, entityId = entityID, flatten = flatten)
	}


	listCommonEducationDataStandardsGradeLevels <- function(searchConditionsList = NULL, CommonEducationDataStandardsGradeLevelID = F, Code = F, Description = F, OrderNumber = F, SkywardID = F, SkywardHash = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, searchConditionsGroupType = "And", searchSortFieldNamesList = NULL, searchSortFieldNamesDescendingList = NULL, entityID = 1, schoolYearID = NULL, page = 1, pageSize = 100000, flatten = T, ...){

		params <- as.list(environment()) %>% append(list(...))

		searchFields <- params %>% keep(names(params) %>% str_sub(1,1) == names(params) %>% str_sub(1,1) %>% str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- "all", searchFields <- searchFields %>% keep(~.x) %>% names())

		listSkyObjects(module = "Enrollment", objectName = "CommonEducationDataStandardsGradeLevel", schoolYearID = schoolYearID, searchFields = searchFields, page = page, pageSize = pageSize, SearchConditionsList = searchConditionsList, SearchConditionsGroupType = searchConditionsGroupType, SearchSortFieldNamesList = searchSortFieldNamesList, SearchSortFieldNamesDescendingList = searchSortFieldNamesDescendingList, entityId = entityID, flatten = flatten)
	}


	listTempStudentEnrollmentErrors <- function(searchConditionsList = NULL, TempStudentEnrollmentErrorID = F, TempStudentEnrollmentRecordID = F, Error = F, ErrorDetail = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, ErrorCount = F, searchConditionsGroupType = "And", searchSortFieldNamesList = NULL, searchSortFieldNamesDescendingList = NULL, entityID = 1, schoolYearID = NULL, page = 1, pageSize = 100000, flatten = T, ...){

		params <- as.list(environment()) %>% append(list(...))

		searchFields <- params %>% keep(names(params) %>% str_sub(1,1) == names(params) %>% str_sub(1,1) %>% str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- "all", searchFields <- searchFields %>% keep(~.x) %>% names())

		listSkyObjects(module = "Enrollment", objectName = "TempStudentEnrollmentError", schoolYearID = schoolYearID, searchFields = searchFields, page = page, pageSize = pageSize, SearchConditionsList = searchConditionsList, SearchConditionsGroupType = searchConditionsGroupType, SearchSortFieldNamesList = searchSortFieldNamesList, SearchSortFieldNamesDescendingList = searchSortFieldNamesDescendingList, entityId = entityID, flatten = flatten)
	}


	listStudentPermits <- function(searchConditionsList = NULL, StudentPermitID = F, DistrictID = F, SchoolYearID = F, StudentID = F, PermitID = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, searchConditionsGroupType = "And", searchSortFieldNamesList = NULL, searchSortFieldNamesDescendingList = NULL, entityID = 1, schoolYearID = NULL, page = 1, pageSize = 100000, flatten = T, ...){

		params <- as.list(environment()) %>% append(list(...))

		searchFields <- params %>% keep(names(params) %>% str_sub(1,1) == names(params) %>% str_sub(1,1) %>% str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- "all", searchFields <- searchFields %>% keep(~.x) %>% names())

		listSkyObjects(module = "Enrollment", objectName = "StudentPermit", schoolYearID = schoolYearID, searchFields = searchFields, page = page, pageSize = pageSize, SearchConditionsList = searchConditionsList, SearchConditionsGroupType = searchConditionsGroupType, SearchSortFieldNamesList = searchSortFieldNamesList, SearchSortFieldNamesDescendingList = searchSortFieldNamesDescendingList, entityId = entityID, flatten = flatten)
	}


	listAMTransportations <- function(searchConditionsList = NULL, AMTransportationID = F, Code = F, Description = F, searchConditionsGroupType = "And", searchSortFieldNamesList = NULL, searchSortFieldNamesDescendingList = NULL, entityID = 1, schoolYearID = NULL, page = 1, pageSize = 100000, flatten = T, ...){

		params <- as.list(environment()) %>% append(list(...))

		searchFields <- params %>% keep(names(params) %>% str_sub(1,1) == names(params) %>% str_sub(1,1) %>% str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- "all", searchFields <- searchFields %>% keep(~.x) %>% names())

		listSkyObjects(module = "Enrollment", objectName = "AMTransportation", schoolYearID = schoolYearID, searchFields = searchFields, page = page, pageSize = pageSize, SearchConditionsList = searchConditionsList, SearchConditionsGroupType = searchConditionsGroupType, SearchSortFieldNamesList = searchSortFieldNamesList, SearchSortFieldNamesDescendingList = searchSortFieldNamesDescendingList, entityId = entityID, flatten = flatten)
	}


	listPMTransportations <- function(searchConditionsList = NULL, PMTransportationID = F, Code = F, Description = F, searchConditionsGroupType = "And", searchSortFieldNamesList = NULL, searchSortFieldNamesDescendingList = NULL, entityID = 1, schoolYearID = NULL, page = 1, pageSize = 100000, flatten = T, ...){

		params <- as.list(environment()) %>% append(list(...))

		searchFields <- params %>% keep(names(params) %>% str_sub(1,1) == names(params) %>% str_sub(1,1) %>% str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- "all", searchFields <- searchFields %>% keep(~.x) %>% names())

		listSkyObjects(module = "Enrollment", objectName = "PMTransportation", schoolYearID = schoolYearID, searchFields = searchFields, page = page, pageSize = pageSize, SearchConditionsList = searchConditionsList, SearchConditionsGroupType = searchConditionsGroupType, SearchSortFieldNamesList = searchSortFieldNamesList, SearchSortFieldNamesDescendingList = searchSortFieldNamesDescendingList, entityId = entityID, flatten = flatten)
	}


	listSchoolDistricts <- function(searchConditionsList = NULL, SchoolDistrictID = F, Code = F, Description = F, searchConditionsGroupType = "And", searchSortFieldNamesList = NULL, searchSortFieldNamesDescendingList = NULL, entityID = 1, schoolYearID = NULL, page = 1, pageSize = 100000, flatten = T, ...){

		params <- as.list(environment()) %>% append(list(...))

		searchFields <- params %>% keep(names(params) %>% str_sub(1,1) == names(params) %>% str_sub(1,1) %>% str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- "all", searchFields <- searchFields %>% keep(~.x) %>% names())

		listSkyObjects(module = "Enrollment", objectName = "SchoolDistrict", schoolYearID = schoolYearID, searchFields = searchFields, page = page, pageSize = pageSize, SearchConditionsList = searchConditionsList, SearchConditionsGroupType = searchConditionsGroupType, SearchSortFieldNamesList = searchSortFieldNamesList, SearchSortFieldNamesDescendingList = searchSortFieldNamesDescendingList, entityId = entityID, flatten = flatten)
	}


	listSchoolPathStudents <- function(searchConditionsList = NULL, SchoolPathStudentID = F, DistrictID = F, SchoolYearID = F, SchoolPathID = F, StudentID = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, searchConditionsGroupType = "And", searchSortFieldNamesList = NULL, searchSortFieldNamesDescendingList = NULL, entityID = 1, schoolYearID = NULL, page = 1, pageSize = 100000, flatten = T, ...){

		params <- as.list(environment()) %>% append(list(...))

		searchFields <- params %>% keep(names(params) %>% str_sub(1,1) == names(params) %>% str_sub(1,1) %>% str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- "all", searchFields <- searchFields %>% keep(~.x) %>% names())

		listSkyObjects(module = "Enrollment", objectName = "SchoolPathStudent", schoolYearID = schoolYearID, searchFields = searchFields, page = page, pageSize = pageSize, SearchConditionsList = searchConditionsList, SearchConditionsGroupType = searchConditionsGroupType, SearchSortFieldNamesList = searchSortFieldNamesList, SearchSortFieldNamesDescendingList = searchSortFieldNamesDescendingList, entityId = entityID, flatten = flatten)
	}


	listSchoolPaths <- function(searchConditionsList = NULL, SchoolPathID = F, Name = F, DistrictID = F, SchoolYearID = F, SchoolPathTypeID = F, SchoolPathIDClonedFrom = F, SchoolPathIDClonedTo = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, searchConditionsGroupType = "And", searchSortFieldNamesList = NULL, searchSortFieldNamesDescendingList = NULL, entityID = 1, schoolYearID = NULL, page = 1, pageSize = 100000, flatten = T, ...){

		params <- as.list(environment()) %>% append(list(...))

		searchFields <- params %>% keep(names(params) %>% str_sub(1,1) == names(params) %>% str_sub(1,1) %>% str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- "all", searchFields <- searchFields %>% keep(~.x) %>% names())

		listSkyObjects(module = "Enrollment", objectName = "SchoolPath", schoolYearID = schoolYearID, searchFields = searchFields, page = page, pageSize = pageSize, SearchConditionsList = searchConditionsList, SearchConditionsGroupType = searchConditionsGroupType, SearchSortFieldNamesList = searchSortFieldNamesList, SearchSortFieldNamesDescendingList = searchSortFieldNamesDescendingList, entityId = entityID, flatten = flatten)
	}


	listSchoolPathSchools <- function(searchConditionsList = NULL, SchoolPathSchoolID = F, DistrictID = F, SchoolYearID = F, SchoolPathID = F, SchoolID = F, Order = F, SchoolPathSchoolIDClonedFrom = F, SchoolPathSchoolIDClonedTo = F, CodeDescription = F, StudentHasPermit = F, IsOverriddenForStudent = F, OverriddenSchoolName = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, searchConditionsGroupType = "And", searchSortFieldNamesList = NULL, searchSortFieldNamesDescendingList = NULL, entityID = 1, schoolYearID = NULL, page = 1, pageSize = 100000, flatten = T, ...){

		params <- as.list(environment()) %>% append(list(...))

		searchFields <- params %>% keep(names(params) %>% str_sub(1,1) == names(params) %>% str_sub(1,1) %>% str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- "all", searchFields <- searchFields %>% keep(~.x) %>% names())

		listSkyObjects(module = "Enrollment", objectName = "SchoolPathSchool", schoolYearID = schoolYearID, searchFields = searchFields, page = page, pageSize = pageSize, SearchConditionsList = searchConditionsList, SearchConditionsGroupType = searchConditionsGroupType, SearchSortFieldNamesList = searchSortFieldNamesList, SearchSortFieldNamesDescendingList = searchSortFieldNamesDescendingList, entityId = entityID, flatten = flatten)
	}


	listSchoolPathTypes <- function(searchConditionsList = NULL, SchoolPathTypeID = F, Code = F, Description = F, SkywardID = F, SkywardHash = F, CodeDescription = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, searchConditionsGroupType = "And", searchSortFieldNamesList = NULL, searchSortFieldNamesDescendingList = NULL, entityID = 1, schoolYearID = NULL, page = 1, pageSize = 100000, flatten = T, ...){

		params <- as.list(environment()) %>% append(list(...))

		searchFields <- params %>% keep(names(params) %>% str_sub(1,1) == names(params) %>% str_sub(1,1) %>% str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- "all", searchFields <- searchFields %>% keep(~.x) %>% names())

		listSkyObjects(module = "Enrollment", objectName = "SchoolPathType", schoolYearID = schoolYearID, searchFields = searchFields, page = page, pageSize = pageSize, SearchConditionsList = searchConditionsList, SearchConditionsGroupType = searchConditionsGroupType, SearchSortFieldNamesList = searchSortFieldNamesList, SearchSortFieldNamesDescendingList = searchSortFieldNamesDescendingList, entityId = entityID, flatten = flatten)
	}
