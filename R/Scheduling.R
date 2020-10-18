

	listBlockPeriods <- function(searchConditionsList = NULL, BlockPeriodID = F, Code = F, BlockPeriodIDClonedFrom = F, BlockPeriodIDClonedTo = F, HasDisplayPeriods = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, EntityID = F, SchoolYearID = F, Description = F, CodeDescription = F, searchConditionsGroupType = "And", searchSortFieldNamesList = NULL, searchSortFieldNamesDescendingList = NULL, entityID = 1, schoolYearID = NULL, page = 1, pageSize = 100000, flatten = T, ...){

		params <- as.list(environment()) %>% append(list(...))

		searchFields <- params %>% keep(names(params) %>% str_sub(1,1) == names(params) %>% str_sub(1,1) %>% str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- "all", searchFields <- searchFields %>% keep(~.x) %>% names())

		listSkyObjects(module = "Scheduling", objectName = "BlockPeriod", schoolYearID = schoolYearID, searchFields = searchFields, page = page, pageSize = pageSize, SearchConditionsList = searchConditionsList, SearchConditionsGroupType = searchConditionsGroupType, SearchSortFieldNamesList = searchSortFieldNamesList, SearchSortFieldNamesDescendingList = searchSortFieldNamesDescendingList, entityId = entityID, flatten = flatten)
	}


	listBlockPeriodDisplayPeriods <- function(searchConditionsList = NULL, BlockPeriodDisplayPeriodID = F, BlockPeriodID = F, DisplayPeriodID = F, BlockPeriodDisplayPeriodIDClonedFrom = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, searchConditionsGroupType = "And", searchSortFieldNamesList = NULL, searchSortFieldNamesDescendingList = NULL, entityID = 1, schoolYearID = NULL, page = 1, pageSize = 100000, flatten = T, ...){

		params <- as.list(environment()) %>% append(list(...))

		searchFields <- params %>% keep(names(params) %>% str_sub(1,1) == names(params) %>% str_sub(1,1) %>% str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- "all", searchFields <- searchFields %>% keep(~.x) %>% names())

		listSkyObjects(module = "Scheduling", objectName = "BlockPeriodDisplayPeriod", schoolYearID = schoolYearID, searchFields = searchFields, page = page, pageSize = pageSize, SearchConditionsList = searchConditionsList, SearchConditionsGroupType = searchConditionsGroupType, SearchSortFieldNamesList = searchSortFieldNamesList, SearchSortFieldNamesDescendingList = searchSortFieldNamesDescendingList, entityId = entityID, flatten = flatten)
	}


	listMaximumTeachingHourOverrides <- function(searchConditionsList = NULL, MaximumTeachingHourOverrideID = F, StaffEntityYearID = F, DayRotationID = F, MaximumConsecutiveTeachingHours = F, MaximumTotalTeachingHours = F, MaximumTeachingHourOverrideIDClonedFrom = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, searchConditionsGroupType = "And", searchSortFieldNamesList = NULL, searchSortFieldNamesDescendingList = NULL, entityID = 1, schoolYearID = NULL, page = 1, pageSize = 100000, flatten = T, ...){

		params <- as.list(environment()) %>% append(list(...))

		searchFields <- params %>% keep(names(params) %>% str_sub(1,1) == names(params) %>% str_sub(1,1) %>% str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- "all", searchFields <- searchFields %>% keep(~.x) %>% names())

		listSkyObjects(module = "Scheduling", objectName = "MaximumTeachingHourOverride", schoolYearID = schoolYearID, searchFields = searchFields, page = page, pageSize = pageSize, SearchConditionsList = searchConditionsList, SearchConditionsGroupType = searchConditionsGroupType, SearchSortFieldNamesList = searchSortFieldNamesList, SearchSortFieldNamesDescendingList = searchSortFieldNamesDescendingList, entityId = entityID, flatten = flatten)
	}


	listSectionSchedulerDisplayPeriodExcludedForCourses <- function(searchConditionsList = NULL, SectionSchedulerDisplayPeriodExcludedForCourseID = F, CourseID = F, DisplayPeriodID = F, SectionSchedulerDisplayPeriodExcludedForCourseIDClonedFrom = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, searchConditionsGroupType = "And", searchSortFieldNamesList = NULL, searchSortFieldNamesDescendingList = NULL, entityID = 1, schoolYearID = NULL, page = 1, pageSize = 100000, flatten = T, ...){

		params <- as.list(environment()) %>% append(list(...))

		searchFields <- params %>% keep(names(params) %>% str_sub(1,1) == names(params) %>% str_sub(1,1) %>% str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- "all", searchFields <- searchFields %>% keep(~.x) %>% names())

		listSkyObjects(module = "Scheduling", objectName = "SectionSchedulerDisplayPeriodExcludedForCourse", schoolYearID = schoolYearID, searchFields = searchFields, page = page, pageSize = pageSize, SearchConditionsList = searchConditionsList, SearchConditionsGroupType = searchConditionsGroupType, SearchSortFieldNamesList = searchSortFieldNamesList, SearchSortFieldNamesDescendingList = searchSortFieldNamesDescendingList, entityId = entityID, flatten = flatten)
	}


	listCourseEntityOfferedToSectionMeetDisplayPeriods <- function(searchConditionsList = NULL, CourseEntityOfferedToSectionMeetDisplayPeriodID = F, CourseEntityOfferedToSectionID = F, MeetID = F, DisplayPeriodID = F, HideMeetDisplayPeriod = F, IsPrimary = F, CourseEntityOfferedToSectionMeetDisplayPeriodIDClonedFrom = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, searchConditionsGroupType = "And", searchSortFieldNamesList = NULL, searchSortFieldNamesDescendingList = NULL, entityID = 1, schoolYearID = NULL, page = 1, pageSize = 100000, flatten = T, ...){

		params <- as.list(environment()) %>% append(list(...))

		searchFields <- params %>% keep(names(params) %>% str_sub(1,1) == names(params) %>% str_sub(1,1) %>% str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- "all", searchFields <- searchFields %>% keep(~.x) %>% names())

		listSkyObjects(module = "Scheduling", objectName = "CourseEntityOfferedToSectionMeetDisplayPeriod", schoolYearID = schoolYearID, searchFields = searchFields, page = page, pageSize = pageSize, SearchConditionsList = searchConditionsList, SearchConditionsGroupType = searchConditionsGroupType, SearchSortFieldNamesList = searchSortFieldNamesList, SearchSortFieldNamesDescendingList = searchSortFieldNamesDescendingList, entityId = entityID, flatten = flatten)
	}


	listCourseEntityOfferedTos <- function(searchConditionsList = NULL, CourseEntityOfferedToID = F, CourseID = F, CourseCode = F, EntityIDOfferedTo = F, SchoolYearID = F, UseIsRequiredOverride = F, IsRequiredOverride = F, UseSchedulingPriorityOverride = F, SchedulingPriorityOverride = F, UseSchedulingTypeOverride = F, SchedulingTypeOverride = F, CourseEntityOfferedToIDClonedFrom = F, CourseEntityOfferedToIDClonedTo = F, IsAutoOffering = F, ViewingFromOfferedEntity = F, ViewingFromOfferingEntity = F, IsOfferedCourse = F, IsHomeCourse = F, IsRequired = F, SchedulingTypeCode = F, SchedulingPriorityCode = F, ActiveSections = F, ActiveSectionsOpen = F, TotalSeatsAvailable = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, Required = F, NumberOfAlternateCourseRequestsEntity = F, NumberOfCourseRequestsEntity = F, NumberOfCourseRequestsFemalesEntity = F, NumberOfCourseRequestsMalesEntity = F, HasStudentCourseRequestsEntity = F, IsCurrentSchoolYearEntity = F, NumberOfSeatsRemainingEntity = F, NumberTransferStudentSectionsEntity = F, TotalSectionCountEntity = F, TotalStudentCourseRequestSectionLengthSubsetCountEntity = F, TotalStudentSectionCountEntity = F, IsActive = F, EntityGroupKey = F, searchConditionsGroupType = "And", searchSortFieldNamesList = NULL, searchSortFieldNamesDescendingList = NULL, entityID = 1, schoolYearID = NULL, page = 1, pageSize = 100000, flatten = T, ...){

		params <- as.list(environment()) %>% append(list(...))

		searchFields <- params %>% keep(names(params) %>% str_sub(1,1) == names(params) %>% str_sub(1,1) %>% str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- "all", searchFields <- searchFields %>% keep(~.x) %>% names())

		listSkyObjects(module = "Scheduling", objectName = "CourseEntityOfferedTo", schoolYearID = schoolYearID, searchFields = searchFields, page = page, pageSize = pageSize, SearchConditionsList = searchConditionsList, SearchConditionsGroupType = searchConditionsGroupType, SearchSortFieldNamesList = searchSortFieldNamesList, SearchSortFieldNamesDescendingList = searchSortFieldNamesDescendingList, entityId = entityID, flatten = flatten)
	}


	listCourseEntityOfferedToSectionStaffMeets <- function(searchConditionsList = NULL, CourseEntityOfferedToSectionStaffMeetID = F, CourseEntityOfferedToSectionID = F, MeetID = F, StaffID = F, CourseEntityOfferedToSectionStaffMeetIDClonedFrom = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, searchConditionsGroupType = "And", searchSortFieldNamesList = NULL, searchSortFieldNamesDescendingList = NULL, entityID = 1, schoolYearID = NULL, page = 1, pageSize = 100000, flatten = T, ...){

		params <- as.list(environment()) %>% append(list(...))

		searchFields <- params %>% keep(names(params) %>% str_sub(1,1) == names(params) %>% str_sub(1,1) %>% str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- "all", searchFields <- searchFields %>% keep(~.x) %>% names())

		listSkyObjects(module = "Scheduling", objectName = "CourseEntityOfferedToSectionStaffMeet", schoolYearID = schoolYearID, searchFields = searchFields, page = page, pageSize = pageSize, SearchConditionsList = searchConditionsList, SearchConditionsGroupType = searchConditionsGroupType, SearchSortFieldNamesList = searchSortFieldNamesList, SearchSortFieldNamesDescendingList = searchSortFieldNamesDescendingList, entityId = entityID, flatten = flatten)
	}


	listCourseEntityOfferedToSections <- function(searchConditionsList = NULL, CourseEntityOfferedToSectionID = F, CourseEntityOfferedToID = F, SectionID = F, MaximumStudentCount = F, IsActive = F, HasMeetDisplayPeriodOverrides = F, CourseEntityOfferedToSectionIDClonedFrom = F, CourseEntityOfferedToSectionIDClonedTo = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, TotalEnrollmentCountEntity = F, StudentEnrollmentEntity = F, SeatsAvailableEntity = F, CourseID = F, EntityIDOfferedTo = F, SchoolYearID = F, ViewingFromOfferedEntity = F, ViewingFromOfferingEntity = F, RoomSeatsAvailable = F, searchConditionsGroupType = "And", searchSortFieldNamesList = NULL, searchSortFieldNamesDescendingList = NULL, entityID = 1, schoolYearID = NULL, page = 1, pageSize = 100000, flatten = T, ...){

		params <- as.list(environment()) %>% append(list(...))

		searchFields <- params %>% keep(names(params) %>% str_sub(1,1) == names(params) %>% str_sub(1,1) %>% str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- "all", searchFields <- searchFields %>% keep(~.x) %>% names())

		listSkyObjects(module = "Scheduling", objectName = "CourseEntityOfferedToSection", schoolYearID = schoolYearID, searchFields = searchFields, page = page, pageSize = pageSize, SearchConditionsList = searchConditionsList, SearchConditionsGroupType = searchConditionsGroupType, SearchSortFieldNamesList = searchSortFieldNamesList, SearchSortFieldNamesDescendingList = searchSortFieldNamesDescendingList, entityId = entityID, flatten = flatten)
	}


	listCourseEntityOfferedToSectionMeets <- function(searchConditionsList = NULL, CourseEntityOfferedToSectionMeetID = F, CourseEntityOfferedToSectionID = F, MeetID = F, RoomID = F, CourseEntityOfferedToSectionMeetIDClonedFrom = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, SectionID = F, EntityIDOfferedTo = F, SchoolYearID = F, searchConditionsGroupType = "And", searchSortFieldNamesList = NULL, searchSortFieldNamesDescendingList = NULL, entityID = 1, schoolYearID = NULL, page = 1, pageSize = 100000, flatten = T, ...){

		params <- as.list(environment()) %>% append(list(...))

		searchFields <- params %>% keep(names(params) %>% str_sub(1,1) == names(params) %>% str_sub(1,1) %>% str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- "all", searchFields <- searchFields %>% keep(~.x) %>% names())

		listSkyObjects(module = "Scheduling", objectName = "CourseEntityOfferedToSectionMeet", schoolYearID = schoolYearID, searchFields = searchFields, page = page, pageSize = pageSize, SearchConditionsList = searchConditionsList, SearchConditionsGroupType = searchConditionsGroupType, SearchSortFieldNamesList = searchSortFieldNamesList, SearchSortFieldNamesDescendingList = searchSortFieldNamesDescendingList, entityId = entityID, flatten = flatten)
	}


	listCourseLevelMNS <- function(searchConditionsList = NULL, CourseLevelMNID = F, Code = F, StateCollegeCourseLevelMNID = F, Credits = F, Title = F, CurriculumYearID = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, InstitutionID = F, searchConditionsGroupType = "And", searchSortFieldNamesList = NULL, searchSortFieldNamesDescendingList = NULL, entityID = 1, schoolYearID = NULL, page = 1, pageSize = 100000, flatten = T, ...){

		params <- as.list(environment()) %>% append(list(...))

		searchFields <- params %>% keep(names(params) %>% str_sub(1,1) == names(params) %>% str_sub(1,1) %>% str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- "all", searchFields <- searchFields %>% keep(~.x) %>% names())

		listSkyObjects(module = "Scheduling", objectName = "CourseLevelMN", schoolYearID = schoolYearID, searchFields = searchFields, page = page, pageSize = pageSize, SearchConditionsList = searchConditionsList, SearchConditionsGroupType = searchConditionsGroupType, SearchSortFieldNamesList = searchSortFieldNamesList, SearchSortFieldNamesDescendingList = searchSortFieldNamesDescendingList, entityId = entityID, flatten = flatten)
	}


	listMCCCTermGradeBucketMNS <- function(searchConditionsList = NULL, MCCCTermGradeBucketMNID = F, SectionLengthID = F, MCCCTermImportID = F, GradeBucketID = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, searchConditionsGroupType = "And", searchSortFieldNamesList = NULL, searchSortFieldNamesDescendingList = NULL, entityID = 1, schoolYearID = NULL, page = 1, pageSize = 100000, flatten = T, ...){

		params <- as.list(environment()) %>% append(list(...))

		searchFields <- params %>% keep(names(params) %>% str_sub(1,1) == names(params) %>% str_sub(1,1) %>% str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- "all", searchFields <- searchFields %>% keep(~.x) %>% names())

		listSkyObjects(module = "Scheduling", objectName = "MCCCTermGradeBucketMN", schoolYearID = schoolYearID, searchFields = searchFields, page = page, pageSize = pageSize, SearchConditionsList = searchConditionsList, SearchConditionsGroupType = searchConditionsGroupType, SearchSortFieldNamesList = searchSortFieldNamesList, SearchSortFieldNamesDescendingList = searchSortFieldNamesDescendingList, entityId = entityID, flatten = flatten)
	}


	listSchedulingCourses <- function(searchConditionsList = NULL, CourseMNID = F, SequenceNumber = F, SequenceLimit = F, IsDirectPay = F, CourseID = F, CourseCode = F, Category = F, Description = F, CurriculumID = F, IsActive = F, SchoolYearID = F, HideFromArenaScheduling = F, EarnedCredits = F, CourseLengthID = F, IsRequired = F, EntityID = F, EntityGroupKey = F, DepartmentID = F, CourseTypeID = F, CourseSubjectID = F, ActivityID = F, IsRepeatable = F, IsCoreAcademic = F, HideFromRequestEntry = F, IsHonors = F, IsWritingEmphasis = F, PreventDrop = F, Website = F, SectionSchedulerManualProcessingOrder = F, PreventMultipleSectionsUsingSingleDisplayPeriod = F, MaximumPercentageOfSectionsInSingleDisplayPeriod = F, LockCourseFromSectionScheduler = F, OverrideStudentSectionLinkByCourse = F, SchedulingPriority = F, SchedulingType = F, SchedulingTeamMode = F, CourseIDClonedFrom = F, CourseCloned = F, AcademicMinutes = F, KeepAttendance = F, GradeCourse = F, EstimatedNumberOfSections = F, EdFiCourseLevelCharacteristicID = F, EdFiSubjectTypeID = F, CodeDescription = F, HasSubjects = F, HasAttachedStandards = F, CourseIDClonedTo = F, IsAHistoricRecord = F, GradingPeriodSetID = F, RequestLimitPerStudent = F, GradeLevelSummary = F, Required = F, NumberOfAlternateCourseRequests = F, NumberOfCourseRequests = F, NumberOfCourseRequestsFemales = F, NumberOfCourseRequestsMales = F, HasNonAlternateStudentCourseRequests = F, NumberOfCourseRequestsInCommonWithCourse = F, HasCourseRequestsInCommonWithCourse = F, ActiveSections = F, ActiveSectionsOpen = F, IsCurrentSchoolYear = F, NumberOfSeatsAvailable = F, NumberOfSeatsRemaining = F, NumberOfTransferStudentSections = F, SpecificStudentRequests = F, SumTotalActiveSectionsOptimalStudentCount = F, TotalSectionCount = F, TotalStudentCourseRequestSectionLengthSubsetCount = F, TotalStudentSectionCount = F, ViewingFromOfferingEntity = F, HasOfferedCourseEntity = F, IsOffered = F, OfferingEntity = F, TotalEntitiesOfferedTo = F, UseRequiredOverride = F, IsRequiredOverride = F, UseSchedulingTypeOverride = F, SchedulingTypeCodeOverride = F, SchedulingTypeOverrideCode = F, UseSchedulingPriorityOverride = F, SchedulingPriorityCodeOverride = F, SchedulingPriorityOverrideCode = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, StateSTARAssignmentCodeMNID = F, LengthOfPeriod = F, StateSTARGradeLevelMNID = F, PeriodsPerWeek = F, ExcludeFromSTAR = F, StateCarlPerkinsProgramCodeMNID = F, IsActiveForEntity = F, CanBeOfferedToAnotherEntity = F, GradeLevelIDSummary = F, AllowTeachersToAddAssignments = F, CourseGradeLevelCodes = F, CourseGroupDescriptions = F, ExcludeFromStudentSectionLink = F, HasStudentCourseRequests = F, MaxRequestedCount = F, CourseFeesList = F, searchConditionsGroupType = "And", searchSortFieldNamesList = NULL, searchSortFieldNamesDescendingList = NULL, entityID = 1, schoolYearID = NULL, page = 1, pageSize = 100000, flatten = T, ...){

		params <- as.list(environment()) %>% append(list(...))

		searchFields <- params %>% keep(names(params) %>% str_sub(1,1) == names(params) %>% str_sub(1,1) %>% str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- "all", searchFields <- searchFields %>% keep(~.x) %>% names())

		listSkyObjects(module = "Scheduling", objectName = "Course", schoolYearID = schoolYearID, searchFields = searchFields, page = page, pageSize = pageSize, SearchConditionsList = searchConditionsList, SearchConditionsGroupType = searchConditionsGroupType, SearchSortFieldNamesList = searchSortFieldNamesList, SearchSortFieldNamesDescendingList = searchSortFieldNamesDescendingList, entityId = entityID, flatten = flatten)
	}


	listSectionLengthSubsets <- function(searchConditionsList = NULL, SectionLengthSubsetMNID = F, MCCCDropDate = F, MCCCTermImportID = F, SectionLengthSubsetID = F, SectionLengthID = F, Code = F, Description = F, IsFullSectionLength = F, StartDate = F, EndDate = F, EntityGroupKey = F, SectionLengthSubsetIDClonedFrom = F, CodeDescription = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, EdFiTermTypeID = F, GradeBucketIDCarlPerkins = F, SectionLengthSubsetIDClonedTo = F, EdFiTermDescriptorID = F, searchConditionsGroupType = "And", searchSortFieldNamesList = NULL, searchSortFieldNamesDescendingList = NULL, entityID = 1, schoolYearID = NULL, page = 1, pageSize = 100000, flatten = T, ...){

		params <- as.list(environment()) %>% append(list(...))

		searchFields <- params %>% keep(names(params) %>% str_sub(1,1) == names(params) %>% str_sub(1,1) %>% str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- "all", searchFields <- searchFields %>% keep(~.x) %>% names())

		listSkyObjects(module = "Scheduling", objectName = "SectionLengthSubset", schoolYearID = schoolYearID, searchFields = searchFields, page = page, pageSize = pageSize, SearchConditionsList = searchConditionsList, SearchConditionsGroupType = searchConditionsGroupType, SearchSortFieldNamesList = searchSortFieldNamesList, SearchSortFieldNamesDescendingList = searchSortFieldNamesDescendingList, entityId = entityID, flatten = flatten)
	}


	listSectionLengths <- function(searchConditionsList = NULL, SectionLengthMNID = F, MCCCDropDate = F, SectionLengthID = F, CourseLengthID = F, Code = F, Description = F, EntityGroupKey = F, StartDate = F, EndDate = F, SectionLengthIDClonedFrom = F, EdFiTermTypeID = F, CodeDescription = F, SectionRange = F, IsSectionCurrent = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, SectionLengthIDClonedTo = F, ThirdPartyTermTypeOverride = F, ThirdPartyTermNumber = F, searchConditionsGroupType = "And", searchSortFieldNamesList = NULL, searchSortFieldNamesDescendingList = NULL, entityID = 1, schoolYearID = NULL, page = 1, pageSize = 100000, flatten = T, ...){

		params <- as.list(environment()) %>% append(list(...))

		searchFields <- params %>% keep(names(params) %>% str_sub(1,1) == names(params) %>% str_sub(1,1) %>% str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- "all", searchFields <- searchFields %>% keep(~.x) %>% names())

		listSkyObjects(module = "Scheduling", objectName = "SectionLength", schoolYearID = schoolYearID, searchFields = searchFields, page = page, pageSize = pageSize, SearchConditionsList = searchConditionsList, SearchConditionsGroupType = searchConditionsGroupType, SearchSortFieldNamesList = searchSortFieldNamesList, SearchSortFieldNamesDescendingList = searchSortFieldNamesDescendingList, entityId = entityID, flatten = flatten)
	}


	listSections <- function(searchConditionsList = NULL, SectionMNID = F, LanguageID = F, StateInstructionalMethodCodeMNID = F, CalendarIDMCCCOverride = F, SectionID = F, CourseID = F, EdFiEducationalEnvironmentID = F, AllowCECE = F, Code = F, IsActive = F, SectionLengthID = F, SpecialEdPercentageLimit = F, IsBilingual = F, Website = F, MinimumStudentCount = F, MaximumStudentCount = F, OptimalStudentCount = F, ReservedSeatCount = F, RecalculateGradebook = F, RecalculateGradebookAdmin = F, AllowStudentsWithoutCategoryToBeAssigned = F, SectionIDClonedFrom = F, HomeroomID = F, FitCode = F, IsSelfPaced = F, IsAHistoricRecord = F, TotalMeetCount = F, CanAddStudentSection = F, SchedulingCategories = F, SchedulingTeams = F, CourseCodeSectionCode = F, CourseDescriptionCodeSectionCode = F, EntityCodeTeacherNumber = F, SectionIDClonedTo = F, OffenseCount = F, OffenseCountYTD = F, HasCalculationGroupCourse = F, HasCategoriesInDistrict = F, HasGradeMarksInEntity = F, HasGradingPeriodGradeBuckets = F, HasGradingScales = F, HasValidGradebookSetup = F, HasValidStandardsSetup = F, InPastYear = F, IsSelfPacedAndActive = F, AssignmentCount = F, MissingAssignmentCount = F, ScoredAssignmentCount = F, UnscoredAssignmentCount = F, DisplayForTeachers = F, GradebookCanHaveSettingsCopiedFromPreviousYear = F, EffectiveTeacherFirstLastName = F, EffectiveTeacherLastFirstName = F, ViewingFromOfferingEntity = F, CanBeOffered = F, IsOffered = F, IsActiveOverride = F, HasAssignments = F, StudentCountForTerm = F, NonGradedAssignmentCountForTerm = F, NonGradedAssignmentCountNoStudentAssignmentsForTerm = F, DueDateOfLastAssignmentScored = F, ScoredAssignmentRange100to90CurrentTerm = F, ScoredAssignmentRange89to80CurrentTerm = F, ScoredAssignmentRange79to70CurrentTerm = F, ScoredAssignmentRange69to60CurrentTerm = F, ScoredAssignmentRange59to50CurrentTerm = F, ScoredAssignmentRange49to1CurrentTerm = F, ScoredAssignmentRange0CurrentTerm = F, StudentAssignmentDataString = F, AssignmentDataString = F, ExcusedAbsencesForTerm = F, ExcusedAbsencesYTD = F, UnexcusedAbsencesForTerm = F, UnexcusedAbsencesYTD = F, TardiesForTerm = F, TardiesYTD = F, OtherAbsencesForTerm = F, OtherAbsencesYTD = F, CurrentGradingPeriod = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, MeetIDCurrentStoredPrimary = F, StaffMeetIDCurrentStoredPrimary = F, DisplayPeriodIDCurrentStoredPrimary = F, MeetSummaryIDCurrentStoredPrimary = F, StaffIDCurrentStoredPrimary = F, PreviousVersionOfFitCode = F, TotalSeatsOfferedToOtherEntities = F, StateSTARModeOfTeachingMNID = F, IsAdministeredForTSA = F, IsTSAProficient = F, SelfPacedEndTime = F, AssignmentsHaveBeenCreated = F, HasPreviousYearSettings = F, EntityID = F, SchoolYearID = F, HasAssignmentsWithSubjects = F, HasAssignmentsWithAcademicStandards = F, HasSubjectsCheckSectionLengthGradingPeriodSet = F, HasStandardsCheckSectionLengthGradingPeriodSet = F, SingleSex = F, IsCreditRecovery = F, IsOfferedToAnotherEntity = F, LockSectionFromSectionScheduler = F, LockSectionLengthFromSectionScheduler = F, SectionLengthScheduledBySectionScheduler = F, CurrentlyRecalculating = F, HasValidSubjectsSetup = F, TransferCourseEnrollmentCountFemales = F, TransferCourseEnrollmentCountMales = F, NonTransferCourseEnrollmentCountFemales = F, NonTransferCourseEnrollmentCountMales = F, TransferCourseEnrollmentCountFemalesEntity = F, TransferCourseEnrollmentCountMalesEntity = F, NonTransferCourseEnrollmentCountFemalesEntity = F, NonTransferCourseEnrollmentCountMalesEntity = F, TransferCourseEnrollmentCountFemalesToday = F, TransferCourseEnrollmentCountMalesToday = F, NonTransferCourseEnrollmentCountFemalesToday = F, NonTransferCourseEnrollmentCountMalesToday = F, TransferCourseEnrollmentCountFemalesEntityToday = F, TransferCourseEnrollmentCountMalesEntityToday = F, NonTransferCourseEnrollmentCountFemalesEntityToday = F, NonTransferCourseEnrollmentCountMalesEntityToday = F, TransferCourseEnrollmentCountFemalesSpecifiedDate = F, TransferCourseEnrollmentCountMalesSpecifiedDate = F, NonTransferCourseEnrollmentCountFemalesSpecifiedDate = F, NonTransferCourseEnrollmentCountMalesSpecifiedDate = F, TransferCourseEnrollmentCountFemalesEntitySpecifiedDate = F, TransferCourseEnrollmentCountMalesEntitySpecifiedDate = F, NonTransferCourseEnrollmentCountFemalesEntitySpecifiedDate = F, NonTransferCourseEnrollmentCountMalesEntitySpecifiedDate = F, TransferCourseEnrollmentCountFemalesFirstDay = F, TransferCourseEnrollmentCountMalesFirstDay = F, NonTransferCourseEnrollmentCountFemalesFirstDay = F, NonTransferCourseEnrollmentCountMalesFirstDay = F, TransferCourseEnrollmentCountFemalesEntityFirstDay = F, TransferCourseEnrollmentCountMalesEntityFirstDay = F, NonTransferCourseEnrollmentCountFemalesEntityFirstDay = F, NonTransferCourseEnrollmentCountMalesEntityFirstDay = F, ProgressStatusTodayCode = F, ProgressStatusSpecifiedDateCode = F, MaximumStudentCountOfferedToSpecifiedEntity = F, TotalEnrollmentCountForFilter = F, TotalEnrollmentCountEntityForFilter = F, CurrentEnrollmentForFilter = F, CurrentEnrollmentEntityForFilter = F, NumberOfTransferStudentSectionsForFilter = F, StudentEnrollmentForFilter = F, StudentEnrollmentEntityForFilter = F, StudentEnrollmentAsOfSpecifiedDateForFilter = F, StudentEnrollmentFemalesForFilter = F, StudentEnrollmentFemalesEntityForFilter = F, StudentEnrollmentFemalesAsOfSpecifiedDateForFilter = F, StudentEnrollmentMalesForFilter = F, StudentEnrollmentMalesEntityForFilter = F, StudentEnrollmentMalesAsOfSpecifiedDateForFilter = F, SeatsAvailableForFilter = F, SeatsAvailableEntityForFilter = F, MaximumStudentCountOfferedForFilter = F, IsInProgressForFilter = F, HasNotStartedForFilter = F, HasCompletedForFilter = F, MaximumStudentCountEntityForFilter = F, ExcludeFromStudentSectionLink = F, HasStudentSections = F, NonTransferCourseStudentEnrollmentCount = F, NonTransferCourseStudentEnrollmentCountEntity = F, SpecifiedPeriodDaySummary = F, ThisPeriodDaySummary = F, SectionEnrollmentTotalsForSectionLengthSubsetSummary = F, SectionEnrollmentTotalsForSectionLengthSubsetSummaryForEntity = F, HasAssignmentsWithStudentGroups = F, HasIncompleteClosedGradeChangeRequests = F, CourseCodeSectionCodeCourseDescription = F, BellScheduleGroupID = F, CalculatedBellScheduleGroupID = F, HasNonDeletedAssignments = F, RoomSeatsAvailable = F, UsingCurriculumSubjectsInGradebook = F, searchConditionsGroupType = "And", searchSortFieldNamesList = NULL, searchSortFieldNamesDescendingList = NULL, entityID = 1, schoolYearID = NULL, page = 1, pageSize = 100000, flatten = T, ...){

		params <- as.list(environment()) %>% append(list(...))

		searchFields <- params %>% keep(names(params) %>% str_sub(1,1) == names(params) %>% str_sub(1,1) %>% str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- "all", searchFields <- searchFields %>% keep(~.x) %>% names())

		listSkyObjects(module = "Scheduling", objectName = "Section", schoolYearID = schoolYearID, searchFields = searchFields, page = page, pageSize = pageSize, SearchConditionsList = searchConditionsList, SearchConditionsGroupType = searchConditionsGroupType, SearchSortFieldNamesList = searchSortFieldNamesList, SearchSortFieldNamesDescendingList = searchSortFieldNamesDescendingList, entityId = entityID, flatten = flatten)
	}


	listTempFailedSectionLengthSubsets <- function(searchConditionsList = NULL, TempFailedSectionLengthSubsetID = F, TempSectionLengthSubsetID = F, Note = F, SectionLengthSubsetID = F, CourseLengthCode = F, SectionLengthID = F, SectionLengthCode = F, Code = F, Description = F, StartDate = F, EndDate = F, OriginalStartDate = F, OriginalEndDate = F, IsFullSectionLength = F, ObjectIsDirty = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, CourseLengthID = F, CourseLengthCodeDescription = F, SectionLengthCodeDescription = F, CodeDescription = F, SectionLengthStartDate = F, SectionLengthEndDate = F, IsUpdated = F, ProcessAction = F, searchConditionsGroupType = "And", searchSortFieldNamesList = NULL, searchSortFieldNamesDescendingList = NULL, entityID = 1, schoolYearID = NULL, page = 1, pageSize = 100000, flatten = T, ...){

		params <- as.list(environment()) %>% append(list(...))

		searchFields <- params %>% keep(names(params) %>% str_sub(1,1) == names(params) %>% str_sub(1,1) %>% str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- "all", searchFields <- searchFields %>% keep(~.x) %>% names())

		listSkyObjects(module = "Scheduling", objectName = "TempFailedSectionLengthSubset", schoolYearID = schoolYearID, searchFields = searchFields, page = page, pageSize = pageSize, SearchConditionsList = searchConditionsList, SearchConditionsGroupType = searchConditionsGroupType, SearchSortFieldNamesList = searchSortFieldNamesList, SearchSortFieldNamesDescendingList = searchSortFieldNamesDescendingList, entityId = entityID, flatten = flatten)
	}


	listSchedulingGroupCourses <- function(searchConditionsList = NULL, SchedulingGroupCourseID = F, SchedulingGroupID = F, CourseID = F, SchedulingGroupCourseIDClonedFrom = F, SchedulingGroupCourseIDClonedTo = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, searchConditionsGroupType = "And", searchSortFieldNamesList = NULL, searchSortFieldNamesDescendingList = NULL, entityID = 1, schoolYearID = NULL, page = 1, pageSize = 100000, flatten = T, ...){

		params <- as.list(environment()) %>% append(list(...))

		searchFields <- params %>% keep(names(params) %>% str_sub(1,1) == names(params) %>% str_sub(1,1) %>% str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- "all", searchFields <- searchFields %>% keep(~.x) %>% names())

		listSkyObjects(module = "Scheduling", objectName = "SchedulingGroupCourse", schoolYearID = schoolYearID, searchFields = searchFields, page = page, pageSize = pageSize, SearchConditionsList = searchConditionsList, SearchConditionsGroupType = searchConditionsGroupType, SearchSortFieldNamesList = searchSortFieldNamesList, SearchSortFieldNamesDescendingList = searchSortFieldNamesDescendingList, entityId = entityID, flatten = flatten)
	}


	listProcessRestrictions <- function(searchConditionsList = NULL, ProcessRestrictionID = F, EntityID = F, SchoolYearID = F, UserIDLockCourseMasterPerformer = F, LockCourseMaster = F, LockMassStudentSchedulerUtility = F, LockMassUnscheduleStudentSectionsUtility = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, LockCourseMasterSetByMassStudentSchedulerUtility = F, LockMassStudentSchedulerUtilitySetByMassStudentSchedulerUtility = F, UserIDLockMassStudentSchedulerUtilityPerformer = F, LockMassUnscheduleStudentSectionsUtilitySetByMassStudentSchedulerUtility = F, UserIDLockMassUnscheduleStudentSectionsUtilityPerformer = F, UserIDLockSchedulingBoardPerformer = F, LockSchedulingBoard = F, LockSchedulingBoardSetByMassStudentSchedulerUtility = F, searchConditionsGroupType = "And", searchSortFieldNamesList = NULL, searchSortFieldNamesDescendingList = NULL, entityID = 1, schoolYearID = NULL, page = 1, pageSize = 100000, flatten = T, ...){

		params <- as.list(environment()) %>% append(list(...))

		searchFields <- params %>% keep(names(params) %>% str_sub(1,1) == names(params) %>% str_sub(1,1) %>% str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- "all", searchFields <- searchFields %>% keep(~.x) %>% names())

		listSkyObjects(module = "Scheduling", objectName = "ProcessRestriction", schoolYearID = schoolYearID, searchFields = searchFields, page = page, pageSize = pageSize, SearchConditionsList = searchConditionsList, SearchConditionsGroupType = searchConditionsGroupType, SearchSortFieldNamesList = searchSortFieldNamesList, SearchSortFieldNamesDescendingList = searchSortFieldNamesDescendingList, entityId = entityID, flatten = flatten)
	}


	listSchedulingTeamGradeReferences <- function(searchConditionsList = NULL, SchedulingTeamGradeReferenceID = F, SchedulingTeamID = F, GradeReferenceID = F, SchedulingTeamGradeReferenceIDClonedFrom = F, MaximumStudentCount = F, StudentEntityYearsCount = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, searchConditionsGroupType = "And", searchSortFieldNamesList = NULL, searchSortFieldNamesDescendingList = NULL, entityID = 1, schoolYearID = NULL, page = 1, pageSize = 100000, flatten = T, ...){

		params <- as.list(environment()) %>% append(list(...))

		searchFields <- params %>% keep(names(params) %>% str_sub(1,1) == names(params) %>% str_sub(1,1) %>% str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- "all", searchFields <- searchFields %>% keep(~.x) %>% names())

		listSkyObjects(module = "Scheduling", objectName = "SchedulingTeamGradeReference", schoolYearID = schoolYearID, searchFields = searchFields, page = page, pageSize = pageSize, SearchConditionsList = searchConditionsList, SearchConditionsGroupType = searchConditionsGroupType, SearchSortFieldNamesList = searchSortFieldNamesList, SearchSortFieldNamesDescendingList = searchSortFieldNamesDescendingList, entityId = entityID, flatten = flatten)
	}


	listTempFailedCourses <- function(searchConditionsList = NULL, TempFailedCourseID = F, TempCourseID = F, CourseID = F, CodeDescription = F, CourseCode = F, Description = F, IsActive = F, EarnedCredits = F, GradeLevelSummary = F, CourseSubjectCode = F, CourseTypeCode = F, ActiveSections = F, NumberOfAlternateCourseRequests = F, NumberOfCourseRequests = F, NumberOfSeatsAvailable = F, EstimatedNumberOfSections = F, MinimumSectionsRequired = F, AveragePerSectionMinimumSectionsRequired = F, OriginalEstimatedNumberOfSections = F, EstimatedStudentsPerSection = F, ObjectIsDirty = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, NumberOfTransferStudentSections = F, CourseLengthID = F, CourseLengthCode = F, GradingPeriodSetID = F, GradingPeriodSetCode = F, NewCourseLengthCode = F, NewCourseLengthID = F, NewGradingPeriodSetCode = F, NewGradingPeriodSetID = F, DefaultSectionLengthID = F, RowIsSelected = F, RowIsReadOnly = F, Note = F, CurriculumCode = F, EntityCode = F, ObjectName = F, RecordsUpdated = F, searchConditionsGroupType = "And", searchSortFieldNamesList = NULL, searchSortFieldNamesDescendingList = NULL, entityID = 1, schoolYearID = NULL, page = 1, pageSize = 100000, flatten = T, ...){

		params <- as.list(environment()) %>% append(list(...))

		searchFields <- params %>% keep(names(params) %>% str_sub(1,1) == names(params) %>% str_sub(1,1) %>% str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- "all", searchFields <- searchFields %>% keep(~.x) %>% names())

		listSkyObjects(module = "Scheduling", objectName = "TempFailedCourse", schoolYearID = schoolYearID, searchFields = searchFields, page = page, pageSize = pageSize, SearchConditionsList = searchConditionsList, SearchConditionsGroupType = searchConditionsGroupType, SearchSortFieldNamesList = searchSortFieldNamesList, SearchSortFieldNamesDescendingList = searchSortFieldNamesDescendingList, entityId = entityID, flatten = flatten)
	}


	listSchedulingGroups <- function(searchConditionsList = NULL, SchedulingGroupID = F, EntityID = F, SchoolYearID = F, Code = F, Description = F, GradeReferenceID = F, HomeroomID = F, SchedulingGroupIDClonedFrom = F, Type = F, CodeDescription = F, SchedulingGroupIDClonedTo = F, TotalSchedulingGroupCoursesCount = F, TotalSchedulingGroupSectionsCount = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, searchConditionsGroupType = "And", searchSortFieldNamesList = NULL, searchSortFieldNamesDescendingList = NULL, entityID = 1, schoolYearID = NULL, page = 1, pageSize = 100000, flatten = T, ...){

		params <- as.list(environment()) %>% append(list(...))

		searchFields <- params %>% keep(names(params) %>% str_sub(1,1) == names(params) %>% str_sub(1,1) %>% str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- "all", searchFields <- searchFields %>% keep(~.x) %>% names())

		listSkyObjects(module = "Scheduling", objectName = "SchedulingGroup", schoolYearID = schoolYearID, searchFields = searchFields, page = page, pageSize = pageSize, SearchConditionsList = searchConditionsList, SearchConditionsGroupType = searchConditionsGroupType, SearchSortFieldNamesList = searchSortFieldNamesList, SearchSortFieldNamesDescendingList = searchSortFieldNamesDescendingList, entityId = entityID, flatten = flatten)
	}


	listSchedulingGroupSections <- function(searchConditionsList = NULL, SchedulingGroupSectionID = F, SchedulingGroupID = F, SectionID = F, SchedulingGroupSectionIDClonedFrom = F, SchedulingGroupSectionIDClonedTo = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, searchConditionsGroupType = "And", searchSortFieldNamesList = NULL, searchSortFieldNamesDescendingList = NULL, entityID = 1, schoolYearID = NULL, page = 1, pageSize = 100000, flatten = T, ...){

		params <- as.list(environment()) %>% append(list(...))

		searchFields <- params %>% keep(names(params) %>% str_sub(1,1) == names(params) %>% str_sub(1,1) %>% str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- "all", searchFields <- searchFields %>% keep(~.x) %>% names())

		listSkyObjects(module = "Scheduling", objectName = "SchedulingGroupSection", schoolYearID = schoolYearID, searchFields = searchFields, page = page, pageSize = pageSize, SearchConditionsList = searchConditionsList, SearchConditionsGroupType = searchConditionsGroupType, SearchSortFieldNamesList = searchSortFieldNamesList, SearchSortFieldNamesDescendingList = searchSortFieldNamesDescendingList, entityId = entityID, flatten = flatten)
	}


	listTempCourses <- function(searchConditionsList = NULL, TempCourseID = F, CourseID = F, CodeDescription = F, CourseCode = F, Description = F, IsActive = F, EarnedCredits = F, GradeLevelSummary = F, CourseSubjectCode = F, CourseTypeCode = F, ActiveSections = F, NumberOfAlternateCourseRequests = F, NumberOfCourseRequests = F, NumberOfSeatsAvailable = F, EstimatedNumberOfSections = F, MinimumSectionsRequired = F, AveragePerSectionMinimumSectionsRequired = F, OriginalEstimatedNumberOfSections = F, EstimatedStudentsPerSection = F, ObjectIsDirty = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, NumberOfTransferStudentSections = F, CourseLengthID = F, CourseLengthCode = F, GradingPeriodSetID = F, GradingPeriodSetCode = F, NewCourseLengthCode = F, NewCourseLengthID = F, NewGradingPeriodSetCode = F, NewGradingPeriodSetID = F, DefaultSectionLengthID = F, RowIsSelected = F, RowIsReadOnly = F, Note = F, CurriculumCode = F, EntityCode = F, ObjectName = F, RecordsUpdated = F, searchConditionsGroupType = "And", searchSortFieldNamesList = NULL, searchSortFieldNamesDescendingList = NULL, entityID = 1, schoolYearID = NULL, page = 1, pageSize = 100000, flatten = T, ...){

		params <- as.list(environment()) %>% append(list(...))

		searchFields <- params %>% keep(names(params) %>% str_sub(1,1) == names(params) %>% str_sub(1,1) %>% str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- "all", searchFields <- searchFields %>% keep(~.x) %>% names())

		listSkyObjects(module = "Scheduling", objectName = "TempCourse", schoolYearID = schoolYearID, searchFields = searchFields, page = page, pageSize = pageSize, SearchConditionsList = searchConditionsList, SearchConditionsGroupType = searchConditionsGroupType, SearchSortFieldNamesList = searchSortFieldNamesList, SearchSortFieldNamesDescendingList = searchSortFieldNamesDescendingList, entityId = entityID, flatten = flatten)
	}


	listTempFailedSections <- function(searchConditionsList = NULL, TempFailedSectionID = F, TempSectionID = F, Note = F, CourseID = F, Course = F, SectionCode = F, SectionID = F, SectionLengthID = F, SectionLengthCode = F, SectionLengthStartDate = F, SectionLengthEndDate = F, CurrentEnrollment = F, MaximumStudentCount = F, PrimaryDisplayPeriod = F, PrimaryDays = F, StaffFullNameFML = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, ErrorCount = F, HasErrors = F, TargetCourse = F, EntityIDCourse = F, SchoolYearIDCourse = F, CourseDescription = F, CourseIsActive = F, CourseTypeCode = F, IsActive = F, NumberOfTransferStudentSections = F, NewCourseLengthCode = F, NewCourseLengthID = F, NewSectionLengthCode = F, NewSectionLengthID = F, RowIsSelected = F, RowIsReadOnly = F, PeriodDaySummary = F, CourseEntityOfferedToID = F, EntityIDOfferedTo = F, GradeLevelSummary = F, IsSourceSection = F, searchConditionsGroupType = "And", searchSortFieldNamesList = NULL, searchSortFieldNamesDescendingList = NULL, entityID = 1, schoolYearID = NULL, page = 1, pageSize = 100000, flatten = T, ...){

		params <- as.list(environment()) %>% append(list(...))

		searchFields <- params %>% keep(names(params) %>% str_sub(1,1) == names(params) %>% str_sub(1,1) %>% str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- "all", searchFields <- searchFields %>% keep(~.x) %>% names())

		listSkyObjects(module = "Scheduling", objectName = "TempFailedSection", schoolYearID = schoolYearID, searchFields = searchFields, page = page, pageSize = pageSize, SearchConditionsList = searchConditionsList, SearchConditionsGroupType = searchConditionsGroupType, SearchSortFieldNamesList = searchSortFieldNamesList, SearchSortFieldNamesDescendingList = searchSortFieldNamesDescendingList, entityId = entityID, flatten = flatten)
	}


	listTempSchedulingTeamGradeReferences <- function(searchConditionsList = NULL, TempSchedulingTeamGradeReferenceID = F, SchedulingTeamGradeReferenceID = F, SchedulingTeamID = F, Code = F, Description = F, MaximumStudentCount = F, CurrentStudentCount = F, TotalToBeAssignedCount = F, TotalToBeAssignedPercent = F, TotalStudents = F, SortOrder = F, OverrideTotalToBeAssignedCount = F, OverrideTotalToBeAssignedPercent = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, searchConditionsGroupType = "And", searchSortFieldNamesList = NULL, searchSortFieldNamesDescendingList = NULL, entityID = 1, schoolYearID = NULL, page = 1, pageSize = 100000, flatten = T, ...){

		params <- as.list(environment()) %>% append(list(...))

		searchFields <- params %>% keep(names(params) %>% str_sub(1,1) == names(params) %>% str_sub(1,1) %>% str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- "all", searchFields <- searchFields %>% keep(~.x) %>% names())

		listSkyObjects(module = "Scheduling", objectName = "TempSchedulingTeamGradeReference", schoolYearID = schoolYearID, searchFields = searchFields, page = page, pageSize = pageSize, SearchConditionsList = searchConditionsList, SearchConditionsGroupType = searchConditionsGroupType, SearchSortFieldNamesList = searchSortFieldNamesList, SearchSortFieldNamesDescendingList = searchSortFieldNamesDescendingList, entityId = entityID, flatten = flatten)
	}


	listTempStudentSchedulingCategories <- function(searchConditionsList = NULL, TempStudentSchedulingCategoryID = F, StudentNameLFM = F, SourceStudentEntityYearID = F, SourceCategoriesDisplayValue = F, SourceCategoryIDsCSV = F, TargetStudentEntityYearID = F, TargetCategoriesDisplayValue = F, TargetCategoryIDsCSV = F, ProposedCategoriesDisplayValue = F, ProposedTargetCategoryIDsToDeleteCSV = F, ProposedTargetCategoryIDsToInsertCSV = F, Message = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, searchConditionsGroupType = "And", searchSortFieldNamesList = NULL, searchSortFieldNamesDescendingList = NULL, entityID = 1, schoolYearID = NULL, page = 1, pageSize = 100000, flatten = T, ...){

		params <- as.list(environment()) %>% append(list(...))

		searchFields <- params %>% keep(names(params) %>% str_sub(1,1) == names(params) %>% str_sub(1,1) %>% str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- "all", searchFields <- searchFields %>% keep(~.x) %>% names())

		listSkyObjects(module = "Scheduling", objectName = "TempStudentSchedulingCategory", schoolYearID = schoolYearID, searchFields = searchFields, page = page, pageSize = pageSize, SearchConditionsList = searchConditionsList, SearchConditionsGroupType = searchConditionsGroupType, SearchSortFieldNamesList = searchSortFieldNamesList, SearchSortFieldNamesDescendingList = searchSortFieldNamesDescendingList, entityId = entityID, flatten = flatten)
	}


	listTempStudentSchedulingTeams <- function(searchConditionsList = NULL, TempStudentSchedulingTeamID = F, StudentNameLFM = F, SourceStudentEntityYearID = F, SourceSchoolYearDescription = F, SourceSchedulingTeamID = F, SourceSchedulingTeamCode = F, SourceSchedulingTeamDescription = F, TargetStudentEntityYearID = F, TargetSchoolYearDescription = F, TargetSchedulingTeamID = F, TargetSchedulingTeamCode = F, TargetSchedulingTeamDescription = F, Message = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, searchConditionsGroupType = "And", searchSortFieldNamesList = NULL, searchSortFieldNamesDescendingList = NULL, entityID = 1, schoolYearID = NULL, page = 1, pageSize = 100000, flatten = T, ...){

		params <- as.list(environment()) %>% append(list(...))

		searchFields <- params %>% keep(names(params) %>% str_sub(1,1) == names(params) %>% str_sub(1,1) %>% str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- "all", searchFields <- searchFields %>% keep(~.x) %>% names())

		listSkyObjects(module = "Scheduling", objectName = "TempStudentSchedulingTeam", schoolYearID = schoolYearID, searchFields = searchFields, page = page, pageSize = pageSize, SearchConditionsList = searchConditionsList, SearchConditionsGroupType = searchConditionsGroupType, SearchSortFieldNamesList = searchSortFieldNamesList, SearchSortFieldNamesDescendingList = searchSortFieldNamesDescendingList, entityId = entityID, flatten = flatten)
	}


	listStudentAutoSchedulerStudentCourseRequestSectionDetails <- function(searchConditionsList = NULL, StudentAutoSchedulerStudentCourseRequestSectionDetailID = F, StudentAutoSchedulerStudentCourseRequestSectionID = F, SectionConflictReason = F, SectionConflictReasonText = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, searchConditionsGroupType = "And", searchSortFieldNamesList = NULL, searchSortFieldNamesDescendingList = NULL, entityID = 1, schoolYearID = NULL, page = 1, pageSize = 100000, flatten = T, ...){

		params <- as.list(environment()) %>% append(list(...))

		searchFields <- params %>% keep(names(params) %>% str_sub(1,1) == names(params) %>% str_sub(1,1) %>% str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- "all", searchFields <- searchFields %>% keep(~.x) %>% names())

		listSkyObjects(module = "Scheduling", objectName = "StudentAutoSchedulerStudentCourseRequestSectionDetail", schoolYearID = schoolYearID, searchFields = searchFields, page = page, pageSize = pageSize, SearchConditionsList = searchConditionsList, SearchConditionsGroupType = searchConditionsGroupType, SearchSortFieldNamesList = searchSortFieldNamesList, SearchSortFieldNamesDescendingList = searchSortFieldNamesDescendingList, entityId = entityID, flatten = flatten)
	}


	listStudentAutoSchedulerStudentCourseRequestSections <- function(searchConditionsList = NULL, StudentAutoSchedulerStudentCourseRequestSectionID = F, StudentAutoSchedulerStudentCourseRequestID = F, StudentAutoSchedulerSectionID = F, SequenceNumber = F, AssignedToThisSection = F, EligibleToEnrollInSection = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, SeatsRemaining = F, PeriodDaySummary = F, searchConditionsGroupType = "And", searchSortFieldNamesList = NULL, searchSortFieldNamesDescendingList = NULL, entityID = 1, schoolYearID = NULL, page = 1, pageSize = 100000, flatten = T, ...){

		params <- as.list(environment()) %>% append(list(...))

		searchFields <- params %>% keep(names(params) %>% str_sub(1,1) == names(params) %>% str_sub(1,1) %>% str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- "all", searchFields <- searchFields %>% keep(~.x) %>% names())

		listSkyObjects(module = "Scheduling", objectName = "StudentAutoSchedulerStudentCourseRequestSection", schoolYearID = schoolYearID, searchFields = searchFields, page = page, pageSize = pageSize, SearchConditionsList = searchConditionsList, SearchConditionsGroupType = searchConditionsGroupType, SearchSortFieldNamesList = searchSortFieldNamesList, SearchSortFieldNamesDescendingList = searchSortFieldNamesDescendingList, entityId = entityID, flatten = flatten)
	}


	listTempFailedStudentSectionTransactions <- function(searchConditionsList = NULL, TempFailedStudentSectionTransactionID = F, TempStudentSectionTransactionID = F, Note = F, StudentNameLFM = F, StudentNumber = F, Course = F, SectionCode = F, StudentSectionTransactionID = F, StudentCourseRequestID = F, TempStudentCourseRequestToReactivateID = F, TempStudentCourseRequestID = F, SectionLengthSubsetID = F, StartDate = F, EndDate = F, EarlyExitReasonID = F, NameIDRequestedBy = F, EntityIDCountsAs = F, HideNewStudentButton = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, searchConditionsGroupType = "And", searchSortFieldNamesList = NULL, searchSortFieldNamesDescendingList = NULL, entityID = 1, schoolYearID = NULL, page = 1, pageSize = 100000, flatten = T, ...){

		params <- as.list(environment()) %>% append(list(...))

		searchFields <- params %>% keep(names(params) %>% str_sub(1,1) == names(params) %>% str_sub(1,1) %>% str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- "all", searchFields <- searchFields %>% keep(~.x) %>% names())

		listSkyObjects(module = "Scheduling", objectName = "TempFailedStudentSectionTransaction", schoolYearID = schoolYearID, searchFields = searchFields, page = page, pageSize = pageSize, SearchConditionsList = searchConditionsList, SearchConditionsGroupType = searchConditionsGroupType, SearchSortFieldNamesList = searchSortFieldNamesList, SearchSortFieldNamesDescendingList = searchSortFieldNamesDescendingList, entityId = entityID, flatten = flatten)
	}


	listTempSections <- function(searchConditionsList = NULL, TempSectionID = F, CourseID = F, Course = F, SectionCode = F, SectionID = F, SectionLengthID = F, SectionLengthCode = F, SectionLengthStartDate = F, SectionLengthEndDate = F, CurrentEnrollment = F, MaximumStudentCount = F, PrimaryDisplayPeriod = F, PrimaryDays = F, StaffFullNameFML = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, TargetCourse = F, EntityIDCourse = F, SchoolYearIDCourse = F, CourseDescription = F, CourseIsActive = F, CourseTypeCode = F, IsActive = F, NumberOfTransferStudentSections = F, NewCourseLengthCode = F, NewCourseLengthID = F, NewSectionLengthCode = F, NewSectionLengthID = F, RowIsSelected = F, RowIsReadOnly = F, Note = F, PeriodDaySummary = F, CourseEntityOfferedToID = F, EntityIDOfferedTo = F, GradeLevelSummary = F, IsSourceSection = F, searchConditionsGroupType = "And", searchSortFieldNamesList = NULL, searchSortFieldNamesDescendingList = NULL, entityID = 1, schoolYearID = NULL, page = 1, pageSize = 100000, flatten = T, ...){

		params <- as.list(environment()) %>% append(list(...))

		searchFields <- params %>% keep(names(params) %>% str_sub(1,1) == names(params) %>% str_sub(1,1) %>% str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- "all", searchFields <- searchFields %>% keep(~.x) %>% names())

		listSkyObjects(module = "Scheduling", objectName = "TempSection", schoolYearID = schoolYearID, searchFields = searchFields, page = page, pageSize = pageSize, SearchConditionsList = searchConditionsList, SearchConditionsGroupType = searchConditionsGroupType, SearchSortFieldNamesList = searchSortFieldNamesList, SearchSortFieldNamesDescendingList = searchSortFieldNamesDescendingList, entityId = entityID, flatten = flatten)
	}


	listTempStudentCourseRequestToReactivateNonStates <- function(searchConditionsList = NULL, TempStudentCourseRequestToReactivateNonStateID = F, StudentID = F, CourseID = F, EntityIDRequestedFrom = F, CourseEntityOfferedToID = F, CourseCode = F, CourseDescription = F, EarnedCredits = F, CourseGradeLevelSummary = F, SectionCode = F, CurrentEnrollment = F, MaximumStudentCount = F, Period = F, Days = F, StaffFullNameFML = F, StudentCourseRequestID = F, IsAlternate = F, AlternateRank = F, StudentCourseRequestIDAlternateFor = F, RequestStatus = F, RequestSource = F, SchedulingMethod = F, SectionID = F, SectionLengthID = F, ExcludeFromReportCardsAndTranscripts = F, IsTransferCourse = F, UseEarnedCreditOverride = F, EarnedCreditOverride = F, GradeReferenceID = F, TransferCourseName = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, DateFrom = F, DateTo = F, CourseCodeDescription = F, AuditRecordIsSchedulable = F, UseEarnedCreditTotalOverride = F, TotalEarnedCreditOverride = F, TotalFailedCreditOverride = F, ExcludeFromStudentSectionLink = F, EntityIDCourse = F, SchoolYearIDCourse = F, StartDate = F, EndDate = F, SectionLengthSubsetID = F, EarlyExitReasonID = F, EarlyExitReasonCodeDescription = F, NameIDRequestedBy = F, NameRequestedByLFM = F, EntityIDCountsAs = F, PreventReactivateCheckboxFromBeingRendered = F, AuditRecordIsRequestable = F, StudentSectionID = F, StudentSectionTransactionID = F, SectionLengthSubsetCode = F, SectionLengthSubsetDescription = F, searchConditionsGroupType = "And", searchSortFieldNamesList = NULL, searchSortFieldNamesDescendingList = NULL, entityID = 1, schoolYearID = NULL, page = 1, pageSize = 100000, flatten = T, ...){

		params <- as.list(environment()) %>% append(list(...))

		searchFields <- params %>% keep(names(params) %>% str_sub(1,1) == names(params) %>% str_sub(1,1) %>% str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- "all", searchFields <- searchFields %>% keep(~.x) %>% names())

		listSkyObjects(module = "Scheduling", objectName = "TempStudentCourseRequestToReactivateNonState", schoolYearID = schoolYearID, searchFields = searchFields, page = page, pageSize = pageSize, SearchConditionsList = searchConditionsList, SearchConditionsGroupType = searchConditionsGroupType, SearchSortFieldNamesList = searchSortFieldNamesList, SearchSortFieldNamesDescendingList = searchSortFieldNamesDescendingList, entityId = entityID, flatten = flatten)
	}


	listTempStudentCourseRequestSectionLengthSubsets <- function(searchConditionsList = NULL, TempStudentCourseRequestSectionLengthSubsetID = F, StudentCourseRequestSectionLengthSubsetID = F, StudentCourseRequestID = F, SectionLengthSubsetID = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, searchConditionsGroupType = "And", searchSortFieldNamesList = NULL, searchSortFieldNamesDescendingList = NULL, entityID = 1, schoolYearID = NULL, page = 1, pageSize = 100000, flatten = T, ...){

		params <- as.list(environment()) %>% append(list(...))

		searchFields <- params %>% keep(names(params) %>% str_sub(1,1) == names(params) %>% str_sub(1,1) %>% str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- "all", searchFields <- searchFields %>% keep(~.x) %>% names())

		listSkyObjects(module = "Scheduling", objectName = "TempStudentCourseRequestSectionLengthSubset", schoolYearID = schoolYearID, searchFields = searchFields, page = page, pageSize = pageSize, SearchConditionsList = searchConditionsList, SearchConditionsGroupType = searchConditionsGroupType, SearchSortFieldNamesList = searchSortFieldNamesList, SearchSortFieldNamesDescendingList = searchSortFieldNamesDescendingList, entityId = entityID, flatten = flatten)
	}


	listTempStudentSectionTransactions <- function(searchConditionsList = NULL, TempStudentSectionTransactionID = F, StudentSectionTransactionID = F, StudentCourseRequestID = F, TempStudentCourseRequestToReactivateID = F, TempStudentCourseRequestID = F, SectionLengthSubsetID = F, StartDate = F, EndDate = F, EarlyExitReasonID = F, NameIDRequestedBy = F, EntityIDCountsAs = F, HideNewStudentButton = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, searchConditionsGroupType = "And", searchSortFieldNamesList = NULL, searchSortFieldNamesDescendingList = NULL, entityID = 1, schoolYearID = NULL, page = 1, pageSize = 100000, flatten = T, ...){

		params <- as.list(environment()) %>% append(list(...))

		searchFields <- params %>% keep(names(params) %>% str_sub(1,1) == names(params) %>% str_sub(1,1) %>% str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- "all", searchFields <- searchFields %>% keep(~.x) %>% names())

		listSkyObjects(module = "Scheduling", objectName = "TempStudentSectionTransaction", schoolYearID = schoolYearID, searchFields = searchFields, page = page, pageSize = pageSize, SearchConditionsList = searchConditionsList, SearchConditionsGroupType = searchConditionsGroupType, SearchSortFieldNamesList = searchSortFieldNamesList, SearchSortFieldNamesDescendingList = searchSortFieldNamesDescendingList, entityId = entityID, flatten = flatten)
	}


	listOpenPeriodAnalysisStudents <- function(searchConditionsList = NULL, OpenPeriodAnalysisStudentID = F, OpenPeriodAnalysisID = F, StudentID = F, FirstName = F, MiddleName = F, LastName = F, GradeLevelCode = F, StudentNumber = F, IsActive = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, searchConditionsGroupType = "And", searchSortFieldNamesList = NULL, searchSortFieldNamesDescendingList = NULL, entityID = 1, schoolYearID = NULL, page = 1, pageSize = 100000, flatten = T, ...){

		params <- as.list(environment()) %>% append(list(...))

		searchFields <- params %>% keep(names(params) %>% str_sub(1,1) == names(params) %>% str_sub(1,1) %>% str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- "all", searchFields <- searchFields %>% keep(~.x) %>% names())

		listSkyObjects(module = "Scheduling", objectName = "OpenPeriodAnalysisStudent", schoolYearID = schoolYearID, searchFields = searchFields, page = page, pageSize = pageSize, SearchConditionsList = searchConditionsList, SearchConditionsGroupType = searchConditionsGroupType, SearchSortFieldNamesList = searchSortFieldNamesList, SearchSortFieldNamesDescendingList = searchSortFieldNamesDescendingList, entityId = entityID, flatten = flatten)
	}


	listOpenPeriodAnalyses <- function(searchConditionsList = NULL, OpenPeriodAnalysisID = F, SchoolYearID = F, EntityID = F, UserIDPerformer = F, Status = F, StartTime = F, EndTime = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, searchConditionsGroupType = "And", searchSortFieldNamesList = NULL, searchSortFieldNamesDescendingList = NULL, entityID = 1, schoolYearID = NULL, page = 1, pageSize = 100000, flatten = T, ...){

		params <- as.list(environment()) %>% append(list(...))

		searchFields <- params %>% keep(names(params) %>% str_sub(1,1) == names(params) %>% str_sub(1,1) %>% str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- "all", searchFields <- searchFields %>% keep(~.x) %>% names())

		listSkyObjects(module = "Scheduling", objectName = "OpenPeriodAnalysis", schoolYearID = schoolYearID, searchFields = searchFields, page = page, pageSize = pageSize, SearchConditionsList = searchConditionsList, SearchConditionsGroupType = searchConditionsGroupType, SearchSortFieldNamesList = searchSortFieldNamesList, SearchSortFieldNamesDescendingList = searchSortFieldNamesDescendingList, entityId = entityID, flatten = flatten)
	}


	listBaseRunAnalyses <- function(searchConditionsList = NULL, BaseRunAnalysisID = F, SchoolYearID = F, EntityID = F, RunType = F, UserIDPerformer = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, searchConditionsGroupType = "And", searchSortFieldNamesList = NULL, searchSortFieldNamesDescendingList = NULL, entityID = 1, schoolYearID = NULL, page = 1, pageSize = 100000, flatten = T, ...){

		params <- as.list(environment()) %>% append(list(...))

		searchFields <- params %>% keep(names(params) %>% str_sub(1,1) == names(params) %>% str_sub(1,1) %>% str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- "all", searchFields <- searchFields %>% keep(~.x) %>% names())

		listSkyObjects(module = "Scheduling", objectName = "BaseRunAnalysis", schoolYearID = schoolYearID, searchFields = searchFields, page = page, pageSize = pageSize, SearchConditionsList = searchConditionsList, SearchConditionsGroupType = searchConditionsGroupType, SearchSortFieldNamesList = searchSortFieldNamesList, SearchSortFieldNamesDescendingList = searchSortFieldNamesDescendingList, entityId = entityID, flatten = flatten)
	}


	listStudyHallSchedulerRunAnalyses <- function(searchConditionsList = NULL, StudyHallSchedulerRunAnalysisID = F, BaseRunAnalysisID = F, StartTimeAnalysis = F, EndTimeAnalysis = F, StartTimeFinalize = F, EndTimeFinalize = F, RunInformation = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, searchConditionsGroupType = "And", searchSortFieldNamesList = NULL, searchSortFieldNamesDescendingList = NULL, entityID = 1, schoolYearID = NULL, page = 1, pageSize = 100000, flatten = T, ...){

		params <- as.list(environment()) %>% append(list(...))

		searchFields <- params %>% keep(names(params) %>% str_sub(1,1) == names(params) %>% str_sub(1,1) %>% str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- "all", searchFields <- searchFields %>% keep(~.x) %>% names())

		listSkyObjects(module = "Scheduling", objectName = "StudyHallSchedulerRunAnalysis", schoolYearID = schoolYearID, searchFields = searchFields, page = page, pageSize = pageSize, SearchConditionsList = searchConditionsList, SearchConditionsGroupType = searchConditionsGroupType, SearchSortFieldNamesList = searchSortFieldNamesList, SearchSortFieldNamesDescendingList = searchSortFieldNamesDescendingList, entityId = entityID, flatten = flatten)
	}


	listStudentAutoSchedulerRunAnalysisExceptions <- function(searchConditionsList = NULL, StudentAutoSchedulerRunAnalysisExceptionID = F, StudentAutoSchedulerRunAnalysisID = F, StudentAutoSchedulerStudentCourseRequestID = F, SeverityType = F, Message = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, CourseCode = F, CourseDescription = F, SectionCode = F, StudentFullName = F, StudentNumber = F, searchConditionsGroupType = "And", searchSortFieldNamesList = NULL, searchSortFieldNamesDescendingList = NULL, entityID = 1, schoolYearID = NULL, page = 1, pageSize = 100000, flatten = T, ...){

		params <- as.list(environment()) %>% append(list(...))

		searchFields <- params %>% keep(names(params) %>% str_sub(1,1) == names(params) %>% str_sub(1,1) %>% str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- "all", searchFields <- searchFields %>% keep(~.x) %>% names())

		listSkyObjects(module = "Scheduling", objectName = "StudentAutoSchedulerRunAnalysisException", schoolYearID = schoolYearID, searchFields = searchFields, page = page, pageSize = pageSize, SearchConditionsList = searchConditionsList, SearchConditionsGroupType = searchConditionsGroupType, SearchSortFieldNamesList = searchSortFieldNamesList, SearchSortFieldNamesDescendingList = searchSortFieldNamesDescendingList, entityId = entityID, flatten = flatten)
	}


	listStudentAutoSchedulerProposedStudentSectionTransactions <- function(searchConditionsList = NULL, StudentAutoSchedulerProposedStudentSectionTransactionID = F, BaseRunAnalysisID = F, CachedStudentSectionID = F, StudentSectionTransactionIDOriginal = F, StudentSectionTransactionID = F, StudentSectionIDOriginal = F, StudentSectionID = F, SectionIDOriginal = F, SectionID = F, SectionLengthSubsetIDOriginal = F, SectionLengthSubsetID = F, EntityIDCountsAs = F, StartDateOriginal = F, StartDate = F, EndDateOriginal = F, EndDate = F, DataCommittedToRealObjects = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, StudentIDOriginal = F, StudentID = F, searchConditionsGroupType = "And", searchSortFieldNamesList = NULL, searchSortFieldNamesDescendingList = NULL, entityID = 1, schoolYearID = NULL, page = 1, pageSize = 100000, flatten = T, ...){

		params <- as.list(environment()) %>% append(list(...))

		searchFields <- params %>% keep(names(params) %>% str_sub(1,1) == names(params) %>% str_sub(1,1) %>% str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- "all", searchFields <- searchFields %>% keep(~.x) %>% names())

		listSkyObjects(module = "Scheduling", objectName = "StudentAutoSchedulerProposedStudentSectionTransaction", schoolYearID = schoolYearID, searchFields = searchFields, page = page, pageSize = pageSize, SearchConditionsList = searchConditionsList, SearchConditionsGroupType = searchConditionsGroupType, SearchSortFieldNamesList = searchSortFieldNamesList, SearchSortFieldNamesDescendingList = searchSortFieldNamesDescendingList, entityId = entityID, flatten = flatten)
	}


	listStudentAutoSchedulerProposedStudentSections <- function(searchConditionsList = NULL, StudentAutoSchedulerProposedStudentSectionID = F, BaseRunAnalysisID = F, CachedStudentSectionID = F, StudentSectionIDOriginal = F, StudentSectionID = F, StudentIDOriginal = F, StudentID = F, SectionIDOriginal = F, SectionID = F, GradeReferenceIDOriginal = F, GradeReferenceID = F, SequenceWithinEntireProcess = F, SequenceWithinGrade = F, SequenceWithinStudent = F, DataCommittedToRealObjects = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, searchConditionsGroupType = "And", searchSortFieldNamesList = NULL, searchSortFieldNamesDescendingList = NULL, entityID = 1, schoolYearID = NULL, page = 1, pageSize = 100000, flatten = T, ...){

		params <- as.list(environment()) %>% append(list(...))

		searchFields <- params %>% keep(names(params) %>% str_sub(1,1) == names(params) %>% str_sub(1,1) %>% str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- "all", searchFields <- searchFields %>% keep(~.x) %>% names())

		listSkyObjects(module = "Scheduling", objectName = "StudentAutoSchedulerProposedStudentSection", schoolYearID = schoolYearID, searchFields = searchFields, page = page, pageSize = pageSize, SearchConditionsList = searchConditionsList, SearchConditionsGroupType = searchConditionsGroupType, SearchSortFieldNamesList = searchSortFieldNamesList, SearchSortFieldNamesDescendingList = searchSortFieldNamesDescendingList, entityId = entityID, flatten = flatten)
	}


	listStudentAutoSchedulerProposedStudentCourseRequests <- function(searchConditionsList = NULL, StudentAutoSchedulerProposedStudentCourseRequestID = F, BaseRunAnalysisID = F, CachedStudentSectionID = F, StudentCourseRequestIDOriginal = F, StudentCourseRequestID = F, StudentIDOriginal = F, StudentID = F, CourseIDOriginal = F, CourseID = F, EntityIDRequestedFrom = F, SectionIDOriginal = F, SectionID = F, StudentSectionIDOriginal = F, StudentSectionID = F, RequestStatusOriginal = F, CourseConflictReason = F, RequestStatus = F, SchedulingMethodOriginal = F, SchedulingMethod = F, SequenceWithinEntireProcess = F, SequenceWithinGrade = F, SequenceWithinStudent = F, IsAlternate = F, CourseConflictReasonText = F, SectionLengthSubsetSummary = F, Ignore = F, UnscheduleStatus = F, DataCommittedToRealObjects = F, IsNewlyScheduled = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, SectionLengthSubsetsRequested = F, searchConditionsGroupType = "And", searchSortFieldNamesList = NULL, searchSortFieldNamesDescendingList = NULL, entityID = 1, schoolYearID = NULL, page = 1, pageSize = 100000, flatten = T, ...){

		params <- as.list(environment()) %>% append(list(...))

		searchFields <- params %>% keep(names(params) %>% str_sub(1,1) == names(params) %>% str_sub(1,1) %>% str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- "all", searchFields <- searchFields %>% keep(~.x) %>% names())

		listSkyObjects(module = "Scheduling", objectName = "StudentAutoSchedulerProposedStudentCourseRequest", schoolYearID = schoolYearID, searchFields = searchFields, page = page, pageSize = pageSize, SearchConditionsList = searchConditionsList, SearchConditionsGroupType = searchConditionsGroupType, SearchSortFieldNamesList = searchSortFieldNamesList, SearchSortFieldNamesDescendingList = searchSortFieldNamesDescendingList, entityId = entityID, flatten = flatten)
	}


	listStudentAutoSchedulerSections <- function(searchConditionsList = NULL, StudentAutoSchedulerSectionID = F, StudentAutoSchedulerCourseID = F, SectionID = F, TotalScheduled = F, SectionCode = F, SectionLengthCode = F, DisplayPeriodCode = F, DayRotationCode = F, IsActive = F, MinimumStudentCount = F, MaximumStudentCount = F, OptimalStudentCount = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, TotalNumberScheduledThisRun = F, AllowStudentsWithoutCategoryToBeAssigned = F, PrimaryMeetBuildingCode = F, PrimaryMeetRoomNumber = F, PrimaryMeetStaffNameLFM = F, SchedulingCategories = F, SchedulingTeams = F, PeriodDaySummary = F, searchConditionsGroupType = "And", searchSortFieldNamesList = NULL, searchSortFieldNamesDescendingList = NULL, entityID = 1, schoolYearID = NULL, page = 1, pageSize = 100000, flatten = T, ...){

		params <- as.list(environment()) %>% append(list(...))

		searchFields <- params %>% keep(names(params) %>% str_sub(1,1) == names(params) %>% str_sub(1,1) %>% str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- "all", searchFields <- searchFields %>% keep(~.x) %>% names())

		listSkyObjects(module = "Scheduling", objectName = "StudentAutoSchedulerSection", schoolYearID = schoolYearID, searchFields = searchFields, page = page, pageSize = pageSize, SearchConditionsList = searchConditionsList, SearchConditionsGroupType = searchConditionsGroupType, SearchSortFieldNamesList = searchSortFieldNamesList, SearchSortFieldNamesDescendingList = searchSortFieldNamesDescendingList, entityId = entityID, flatten = flatten)
	}


	listStudentAutoSchedulerStudentCourseRequests <- function(searchConditionsList = NULL, StudentAutoSchedulerStudentCourseRequestID = F, StudentAutoSchedulerStudentID = F, StudentAutoSchedulerCourseID = F, StudentAutoSchedulerSectionID = F, StudentCourseRequestID = F, EntityIDRequestedFrom = F, EntityRequestedFromEntityCode = F, CourseConflictReason = F, CourseConflictReasonText = F, SequenceNumber = F, SectionLengthSubsetSummary = F, SchedulingMethodCode = F, IsAlternate = F, HasRelatedStudentAutoSchedulerStudentCourseRequestSections = F, UpdatedDuringThisSchedulingRun = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, SectionLengthSubsetsRequested = F, InitialSequenceNumber = F, searchConditionsGroupType = "And", searchSortFieldNamesList = NULL, searchSortFieldNamesDescendingList = NULL, entityID = 1, schoolYearID = NULL, page = 1, pageSize = 100000, flatten = T, ...){

		params <- as.list(environment()) %>% append(list(...))

		searchFields <- params %>% keep(names(params) %>% str_sub(1,1) == names(params) %>% str_sub(1,1) %>% str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- "all", searchFields <- searchFields %>% keep(~.x) %>% names())

		listSkyObjects(module = "Scheduling", objectName = "StudentAutoSchedulerStudentCourseRequest", schoolYearID = schoolYearID, searchFields = searchFields, page = page, pageSize = pageSize, SearchConditionsList = searchConditionsList, SearchConditionsGroupType = searchConditionsGroupType, SearchSortFieldNamesList = searchSortFieldNamesList, SearchSortFieldNamesDescendingList = searchSortFieldNamesDescendingList, entityId = entityID, flatten = flatten)
	}


	listStudentAutoSchedulerStudents <- function(searchConditionsList = NULL, StudentAutoSchedulerStudentID = F, StudentAutoSchedulerRunAnalysisID = F, StudentID = F, SequenceNumber = F, HasConflict = F, FullName = F, Grade = F, BirthDate = F, StudentNumber = F, GenderCode = F, StudentTypeCode = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, StartTimeAnalysis = F, EndTimeAnalysis = F, RawPermutations = F, SchedulesConsidered = F, NumberOfConflictedStudentCourseRequests = F, NumberOfScheduledStudentCourseRequests = F, TotalNumberOfStudentCourseRequests = F, GradeReferenceID = F, SchedulingCategories = F, SchedulingTeamCode = F, TotalNumberOfAlternateStudentCourseRequests = F, ProcessedDuringThisSchedulingRun = F, RandomSchedulingInteger = F, CalendarCode = F, searchConditionsGroupType = "And", searchSortFieldNamesList = NULL, searchSortFieldNamesDescendingList = NULL, entityID = 1, schoolYearID = NULL, page = 1, pageSize = 100000, flatten = T, ...){

		params <- as.list(environment()) %>% append(list(...))

		searchFields <- params %>% keep(names(params) %>% str_sub(1,1) == names(params) %>% str_sub(1,1) %>% str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- "all", searchFields <- searchFields %>% keep(~.x) %>% names())

		listSkyObjects(module = "Scheduling", objectName = "StudentAutoSchedulerStudent", schoolYearID = schoolYearID, searchFields = searchFields, page = page, pageSize = pageSize, SearchConditionsList = searchConditionsList, SearchConditionsGroupType = searchConditionsGroupType, SearchSortFieldNamesList = searchSortFieldNamesList, SearchSortFieldNamesDescendingList = searchSortFieldNamesDescendingList, entityId = entityID, flatten = flatten)
	}


	listStudentAutoSchedulerRunAnalysisTotals <- function(searchConditionsList = NULL, StudentAutoSchedulerRunAnalysisTotalID = F, StudentAutoSchedulerRunAnalysisID = F, GradeReferenceID = F, TotalStudents = F, StudentsWithConflicts = F, ConflictPercent = F, TotalStudentCourseRequests = F, TotalConflictedStudentCourseRequests = F, TotalScheduledStudentCourseRequests = F, StudentCourseRequestsConflictedThisRun = F, StudentCourseRequestsProcessedThisRun = F, StudentCourseRequestsScheduledThisRun = F, ConflictedStudentCourseRequestPercent = F, ScheduledStudentCourseRequestPercent = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, StartTimeAnalysis = F, EndTimeAnalysis = F, StudentsWithOneConflict = F, StudentsWithTwoConflicts = F, StudentsWithThreeOrMoreConflicts = F, TotalAlternateStudentCourseRequests = F, searchConditionsGroupType = "And", searchSortFieldNamesList = NULL, searchSortFieldNamesDescendingList = NULL, entityID = 1, schoolYearID = NULL, page = 1, pageSize = 100000, flatten = T, ...){

		params <- as.list(environment()) %>% append(list(...))

		searchFields <- params %>% keep(names(params) %>% str_sub(1,1) == names(params) %>% str_sub(1,1) %>% str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- "all", searchFields <- searchFields %>% keep(~.x) %>% names())

		listSkyObjects(module = "Scheduling", objectName = "StudentAutoSchedulerRunAnalysisTotal", schoolYearID = schoolYearID, searchFields = searchFields, page = page, pageSize = pageSize, SearchConditionsList = searchConditionsList, SearchConditionsGroupType = searchConditionsGroupType, SearchSortFieldNamesList = searchSortFieldNamesList, SearchSortFieldNamesDescendingList = searchSortFieldNamesDescendingList, entityId = entityID, flatten = flatten)
	}


	listStudentAutoSchedulerRunAnalyses <- function(searchConditionsList = NULL, StudentAutoSchedulerRunAnalysisID = F, BaseRunAnalysisID = F, StartTimeAnalysis = F, EndTimeAnalysis = F, StartTimeFinalize = F, EndTimeFinalize = F, Description = F, CloseSectionsWhenFilled = F, CreatedCourseAndSectionAnalysisDetails = F, CreatedStudentAnalysisDetails = F, ProcessSpecialEdCourses = F, StudentInformation = F, RunInformation = F, StudentCourseRequestOrder = F, StudentDifficultyOrder = F, StudentAutoSchedulerMode = F, AnalysisDuration = F, CourseConflictPercent = F, FinalizeDuration = F, OverallDuration = F, ProposedSchedulesAccepted = F, StudentConflictPercent = F, ConflictedStudentCourseRequestPercent = F, ScheduledStudentCourseRequestPercent = F, TotalScheduledStudentCourseRequests = F, TotalConflictedStudentCourseRequests = F, TotalStudentCourseRequests = F, StudentCourseRequestsConflictedThisRun = F, StudentCourseRequestsProcessedThisRun = F, StudentCourseRequestsScheduledThisRun = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, TotalAlternateStudentCourseRequests = F, TotalStudents = F, TotalStudentsWithConflicts = F, TotalStudentsWithOneConflict = F, TotalStudentsWithTwoConflicts = F, TotalStudentsWithThreeOrMoreConflicts = F, CountOfStudentAutoSchedulerCourseRecords = F, CountOfStudentAutoSchedulerRunAnalysisExceptionRecords = F, CountOfStudentAutoSchedulerStudentRecords = F, AnalysisVersion = F, IncludedAbilityToAcceptProposedSchedules = F, PersistentSchedulingRunDataIsNoLongerAcceptable = F, SuccessfulStudentSectionsCount = F, FailedStudentSectionsCount = F, FailedStudentCourseRequestsCount = F, FailedStudentSectionTransactionsCount = F, FailedScheduledStudentSectionsCount = F, TotalStudentSectionsToFinalize = F, TotalStudentCourseRequestsToFinalize = F, TotalStudentSectionTransactionsToFinalize = F, SendMessageOnComplete = F, ExistsAutoScheduledCourses = F, ExistsAutoScheduledStudents = F, RunDescription = F, StudentEntityYearRangeXMLFilter = F, searchConditionsGroupType = "And", searchSortFieldNamesList = NULL, searchSortFieldNamesDescendingList = NULL, entityID = 1, schoolYearID = NULL, page = 1, pageSize = 100000, flatten = T, ...){

		params <- as.list(environment()) %>% append(list(...))

		searchFields <- params %>% keep(names(params) %>% str_sub(1,1) == names(params) %>% str_sub(1,1) %>% str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- "all", searchFields <- searchFields %>% keep(~.x) %>% names())

		listSkyObjects(module = "Scheduling", objectName = "StudentAutoSchedulerRunAnalysis", schoolYearID = schoolYearID, searchFields = searchFields, page = page, pageSize = pageSize, SearchConditionsList = searchConditionsList, SearchConditionsGroupType = searchConditionsGroupType, SearchSortFieldNamesList = searchSortFieldNamesList, SearchSortFieldNamesDescendingList = searchSortFieldNamesDescendingList, entityId = entityID, flatten = flatten)
	}


	listStudentAutoSchedulerCourses <- function(searchConditionsList = NULL, StudentAutoSchedulerCourseID = F, StudentAutoSchedulerRunAnalysisID = F, CourseID = F, TotalRequests = F, TotalScheduled = F, TotalConflicts = F, CourseCode = F, CourseDescription = F, CourseEntityID = F, CourseEntityCode = F, CourseLengthCode = F, CourseSubjectCode = F, SchedulingTypeCode = F, SchedulingPriorityCode = F, IsRequired = F, IsActive = F, SchedulingTeamModeCode = F, DepartmentCode = F, ActiveSections = F, ConflictedRequestPercent = F, ScheduledRequestPercent = F, TotalSections = F, TotalSeatsAvailable = F, TotalSeatsScheduled = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, TotalNumberScheduledThisRun = F, TotalAlternateRequests = F, CategoryCode = F, searchConditionsGroupType = "And", searchSortFieldNamesList = NULL, searchSortFieldNamesDescendingList = NULL, entityID = 1, schoolYearID = NULL, page = 1, pageSize = 100000, flatten = T, ...){

		params <- as.list(environment()) %>% append(list(...))

		searchFields <- params %>% keep(names(params) %>% str_sub(1,1) == names(params) %>% str_sub(1,1) %>% str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- "all", searchFields <- searchFields %>% keep(~.x) %>% names())

		listSkyObjects(module = "Scheduling", objectName = "StudentAutoSchedulerCourse", schoolYearID = schoolYearID, searchFields = searchFields, page = page, pageSize = pageSize, SearchConditionsList = searchConditionsList, SearchConditionsGroupType = searchConditionsGroupType, SearchSortFieldNamesList = searchSortFieldNamesList, SearchSortFieldNamesDescendingList = searchSortFieldNamesDescendingList, entityId = entityID, flatten = flatten)
	}


	listTempFailedStudentSections <- function(searchConditionsList = NULL, TempFailedStudentSectionID = F, TempStudentSectionID = F, StudentSectionID = F, StudentCourseRequestID = F, StudentNameLFM = F, StudentNumber = F, CourseCodeDescription = F, SectionCode = F, SectionID = F, StudentID = F, StartDate = F, EndDate = F, GradeReferenceID = F, StudentGradeLevelCode = F, StudentGradYear = F, StudentGenderCode = F, CourseGradeLevelSummary = F, EarlyExitReasonCodeDescription = F, Note = F, StudentSectionTransactionIDToUpdate = F, RenderCheckbox = F, WorkflowAction = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, CourseID = F, EntityIDCourse = F, SchoolYearIDCourse = F, CourseEntityOfferedToID = F, EntityIDCountsAs = F, TempStudentID = F, CourseCode = F, CourseDescription = F, SectionLengthID = F, SectionLengthCode = F, SectionLengthStartDate = F, SectionLengthEndDate = F, SectionLengthSubsetID = F, RowIsSelected = F, RowIsReadOnly = F, AutomaticRequestSetting = F, AutomaticScheduleSetting = F, SectionCorequisiteGroupName = F, ScheduleAllSectionsInGroupOrNone = F, searchConditionsGroupType = "And", searchSortFieldNamesList = NULL, searchSortFieldNamesDescendingList = NULL, entityID = 1, schoolYearID = NULL, page = 1, pageSize = 100000, flatten = T, ...){

		params <- as.list(environment()) %>% append(list(...))

		searchFields <- params %>% keep(names(params) %>% str_sub(1,1) == names(params) %>% str_sub(1,1) %>% str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- "all", searchFields <- searchFields %>% keep(~.x) %>% names())

		listSkyObjects(module = "Scheduling", objectName = "TempFailedStudentSection", schoolYearID = schoolYearID, searchFields = searchFields, page = page, pageSize = pageSize, SearchConditionsList = searchConditionsList, SearchConditionsGroupType = searchConditionsGroupType, SearchSortFieldNamesList = searchSortFieldNamesList, SearchSortFieldNamesDescendingList = searchSortFieldNamesDescendingList, entityId = entityID, flatten = flatten)
	}


	listStudentCourseRequestSectionLengthSubsets <- function(searchConditionsList = NULL, StudentCourseRequestSectionLengthSubsetID = F, StudentCourseRequestID = F, SectionLengthSubsetID = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, searchConditionsGroupType = "And", searchSortFieldNamesList = NULL, searchSortFieldNamesDescendingList = NULL, entityID = 1, schoolYearID = NULL, page = 1, pageSize = 100000, flatten = T, ...){

		params <- as.list(environment()) %>% append(list(...))

		searchFields <- params %>% keep(names(params) %>% str_sub(1,1) == names(params) %>% str_sub(1,1) %>% str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- "all", searchFields <- searchFields %>% keep(~.x) %>% names())

		listSkyObjects(module = "Scheduling", objectName = "StudentCourseRequestSectionLengthSubset", schoolYearID = schoolYearID, searchFields = searchFields, page = page, pageSize = pageSize, SearchConditionsList = searchConditionsList, SearchConditionsGroupType = searchConditionsGroupType, SearchSortFieldNamesList = searchSortFieldNamesList, SearchSortFieldNamesDescendingList = searchSortFieldNamesDescendingList, entityId = entityID, flatten = flatten)
	}


	listTempFailedStudentCourseRequests <- function(searchConditionsList = NULL, TempFailedStudentCourseRequestID = F, TempStudentCourseRequestID = F, Note = F, StudentID = F, FullStudentNameLFM = F, StudentNumber = F, CourseID = F, EntityIDRequestedFrom = F, CourseEntityOfferedToID = F, CourseCode = F, CourseDescription = F, EarnedCredits = F, CourseSubjectCodeDescription = F, CourseDepartmentCodeDescription = F, CourseNumericSchoolYear = F, CourseSchoolYearDescription = F, CourseGradeLevelSummary = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, StudentCourseRequestID = F, StudentCourseRequestSectionLengthSubsetID = F, SectionLengthSubsetID = F, SectionlengthSubsetCode = F, CourseLengthCode = F, TempStudentEnrollmentRecordID = F, Selected = F, Failed = F, ErrorMessage = F, WorkflowAction = F, StudentSectionID = F, TempStudentID = F, SectionCode = F, searchConditionsGroupType = "And", searchSortFieldNamesList = NULL, searchSortFieldNamesDescendingList = NULL, entityID = 1, schoolYearID = NULL, page = 1, pageSize = 100000, flatten = T, ...){

		params <- as.list(environment()) %>% append(list(...))

		searchFields <- params %>% keep(names(params) %>% str_sub(1,1) == names(params) %>% str_sub(1,1) %>% str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- "all", searchFields <- searchFields %>% keep(~.x) %>% names())

		listSkyObjects(module = "Scheduling", objectName = "TempFailedStudentCourseRequest", schoolYearID = schoolYearID, searchFields = searchFields, page = page, pageSize = pageSize, SearchConditionsList = searchConditionsList, SearchConditionsGroupType = searchConditionsGroupType, SearchSortFieldNamesList = searchSortFieldNamesList, SearchSortFieldNamesDescendingList = searchSortFieldNamesDescendingList, entityId = entityID, flatten = flatten)
	}


	listTempStudentCourseRequests <- function(searchConditionsList = NULL, TempStudentCourseRequestID = F, StudentID = F, FullStudentNameLFM = F, StudentNumber = F, CourseID = F, EntityIDRequestedFrom = F, CourseEntityOfferedToID = F, CourseCode = F, CourseDescription = F, EarnedCredits = F, CourseSubjectCodeDescription = F, CourseDepartmentCodeDescription = F, CourseNumericSchoolYear = F, CourseSchoolYearDescription = F, CourseGradeLevelSummary = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, StudentCourseRequestID = F, StudentCourseRequestSectionLengthSubsetID = F, SectionLengthSubsetID = F, SectionlengthSubsetCode = F, CourseLengthCode = F, TempStudentEnrollmentRecordID = F, Selected = F, Failed = F, ErrorMessage = F, WorkflowAction = F, StudentSectionID = F, TempStudentID = F, SectionCode = F, searchConditionsGroupType = "And", searchSortFieldNamesList = NULL, searchSortFieldNamesDescendingList = NULL, entityID = 1, schoolYearID = NULL, page = 1, pageSize = 100000, flatten = T, ...){

		params <- as.list(environment()) %>% append(list(...))

		searchFields <- params %>% keep(names(params) %>% str_sub(1,1) == names(params) %>% str_sub(1,1) %>% str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- "all", searchFields <- searchFields %>% keep(~.x) %>% names())

		listSkyObjects(module = "Scheduling", objectName = "TempStudentCourseRequest", schoolYearID = schoolYearID, searchFields = searchFields, page = page, pageSize = pageSize, SearchConditionsList = searchConditionsList, SearchConditionsGroupType = searchConditionsGroupType, SearchSortFieldNamesList = searchSortFieldNamesList, SearchSortFieldNamesDescendingList = searchSortFieldNamesDescendingList, entityId = entityID, flatten = flatten)
	}


	listTempSectionLengths <- function(searchConditionsList = NULL, TempSectionLengthID = F, SectionLengthID = F, Code = F, StartDate = F, EndDate = F, OriginalStartDate = F, OriginalEndDate = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, CourseLengthCode = F, IsUpdated = F, ProcessAction = F, searchConditionsGroupType = "And", searchSortFieldNamesList = NULL, searchSortFieldNamesDescendingList = NULL, entityID = 1, schoolYearID = NULL, page = 1, pageSize = 100000, flatten = T, ...){

		params <- as.list(environment()) %>% append(list(...))

		searchFields <- params %>% keep(names(params) %>% str_sub(1,1) == names(params) %>% str_sub(1,1) %>% str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- "all", searchFields <- searchFields %>% keep(~.x) %>% names())

		listSkyObjects(module = "Scheduling", objectName = "TempSectionLength", schoolYearID = schoolYearID, searchFields = searchFields, page = page, pageSize = pageSize, SearchConditionsList = searchConditionsList, SearchConditionsGroupType = searchConditionsGroupType, SearchSortFieldNamesList = searchSortFieldNamesList, SearchSortFieldNamesDescendingList = searchSortFieldNamesDescendingList, entityId = entityID, flatten = flatten)
	}


	listCourseGradeReferences <- function(searchConditionsList = NULL, CourseGradeReferenceID = F, CourseID = F, GradeReferenceID = F, EntityGroupKey = F, CourseGradeReferenceIDClonedFrom = F, NumberOfAlternateCourseRequests = F, NumberOfCourseRequests = F, NumberOfCourseRequestsFemales = F, NumberOfCourseRequestsMales = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, NumberOfAlternateCourseRequestsEntity = F, NumberOfCourseRequestsEntity = F, NumberOfCourseRequestsFemalesEntity = F, NumberOfCourseRequestsMalesEntity = F, searchConditionsGroupType = "And", searchSortFieldNamesList = NULL, searchSortFieldNamesDescendingList = NULL, entityID = 1, schoolYearID = NULL, page = 1, pageSize = 100000, flatten = T, ...){

		params <- as.list(environment()) %>% append(list(...))

		searchFields <- params %>% keep(names(params) %>% str_sub(1,1) == names(params) %>% str_sub(1,1) %>% str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- "all", searchFields <- searchFields %>% keep(~.x) %>% names())

		listSkyObjects(module = "Scheduling", objectName = "CourseGradeReference", schoolYearID = schoolYearID, searchFields = searchFields, page = page, pageSize = pageSize, SearchConditionsList = searchConditionsList, SearchConditionsGroupType = searchConditionsGroupType, SearchSortFieldNamesList = searchSortFieldNamesList, SearchSortFieldNamesDescendingList = searchSortFieldNamesDescendingList, entityId = entityID, flatten = flatten)
	}


	listTempSectionLengthSubsets <- function(searchConditionsList = NULL, TempSectionLengthSubsetID = F, SectionLengthSubsetID = F, CourseLengthCode = F, SectionLengthID = F, SectionLengthCode = F, Code = F, Description = F, StartDate = F, EndDate = F, OriginalStartDate = F, OriginalEndDate = F, IsFullSectionLength = F, ObjectIsDirty = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, CourseLengthID = F, CourseLengthCodeDescription = F, SectionLengthCodeDescription = F, CodeDescription = F, SectionLengthStartDate = F, SectionLengthEndDate = F, IsUpdated = F, ProcessAction = F, searchConditionsGroupType = "And", searchSortFieldNamesList = NULL, searchSortFieldNamesDescendingList = NULL, entityID = 1, schoolYearID = NULL, page = 1, pageSize = 100000, flatten = T, ...){

		params <- as.list(environment()) %>% append(list(...))

		searchFields <- params %>% keep(names(params) %>% str_sub(1,1) == names(params) %>% str_sub(1,1) %>% str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- "all", searchFields <- searchFields %>% keep(~.x) %>% names())

		listSkyObjects(module = "Scheduling", objectName = "TempSectionLengthSubset", schoolYearID = schoolYearID, searchFields = searchFields, page = page, pageSize = pageSize, SearchConditionsList = searchConditionsList, SearchConditionsGroupType = searchConditionsGroupType, SearchSortFieldNamesList = searchSortFieldNamesList, SearchSortFieldNamesDescendingList = searchSortFieldNamesDescendingList, entityId = entityID, flatten = flatten)
	}


	listSchedulingTempStudentSections <- function(searchConditionsList = NULL, TempStudentSectionID = F, StudentSectionID = F, StudentCourseRequestID = F, StudentNameLFM = F, StudentNumber = F, CourseCodeDescription = F, SectionCode = F, SectionID = F, StudentID = F, StartDate = F, EndDate = F, GradeReferenceID = F, StudentGradeLevelCode = F, StudentGradYear = F, StudentGenderCode = F, CourseGradeLevelSummary = F, EarlyExitReasonCodeDescription = F, Note = F, StudentSectionTransactionIDToUpdate = F, RenderCheckbox = F, WorkflowAction = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, CourseID = F, EntityIDCourse = F, SchoolYearIDCourse = F, CourseEntityOfferedToID = F, EntityIDCountsAs = F, TempStudentID = F, CourseCode = F, CourseDescription = F, SectionLengthID = F, SectionLengthCode = F, SectionLengthStartDate = F, SectionLengthEndDate = F, SectionLengthSubsetID = F, RowIsSelected = F, RowIsReadOnly = F, AutomaticRequestSetting = F, AutomaticScheduleSetting = F, SectionCorequisiteGroupName = F, ScheduleAllSectionsInGroupOrNone = F, searchConditionsGroupType = "And", searchSortFieldNamesList = NULL, searchSortFieldNamesDescendingList = NULL, entityID = 1, schoolYearID = NULL, page = 1, pageSize = 100000, flatten = T, ...){

		params <- as.list(environment()) %>% append(list(...))

		searchFields <- params %>% keep(names(params) %>% str_sub(1,1) == names(params) %>% str_sub(1,1) %>% str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- "all", searchFields <- searchFields %>% keep(~.x) %>% names())

		listSkyObjects(module = "Scheduling", objectName = "TempStudentSection", schoolYearID = schoolYearID, searchFields = searchFields, page = page, pageSize = pageSize, SearchConditionsList = searchConditionsList, SearchConditionsGroupType = searchConditionsGroupType, SearchSortFieldNamesList = searchSortFieldNamesList, SearchSortFieldNamesDescendingList = searchSortFieldNamesDescendingList, entityId = entityID, flatten = flatten)
	}


	listEarlyExitReasons <- function(searchConditionsList = NULL, EarlyExitReasonID = F, EntityID = F, EntityGroupKey = F, SchoolYearID = F, Code = F, Description = F, EarlyExitReasonIDClonedFrom = F, CodeDescription = F, IsConsideredDrop = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, searchConditionsGroupType = "And", searchSortFieldNamesList = NULL, searchSortFieldNamesDescendingList = NULL, entityID = 1, schoolYearID = NULL, page = 1, pageSize = 100000, flatten = T, ...){

		params <- as.list(environment()) %>% append(list(...))

		searchFields <- params %>% keep(names(params) %>% str_sub(1,1) == names(params) %>% str_sub(1,1) %>% str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- "all", searchFields <- searchFields %>% keep(~.x) %>% names())

		listSkyObjects(module = "Scheduling", objectName = "EarlyExitReason", schoolYearID = schoolYearID, searchFields = searchFields, page = page, pageSize = pageSize, SearchConditionsList = searchConditionsList, SearchConditionsGroupType = searchConditionsGroupType, SearchSortFieldNamesList = searchSortFieldNamesList, SearchSortFieldNamesDescendingList = searchSortFieldNamesDescendingList, entityId = entityID, flatten = flatten)
	}


	listSectionSchedulingTeams <- function(searchConditionsList = NULL, SectionSchedulingTeamID = F, SectionID = F, SchedulingTeamID = F, SectionSchedulingTeamIDClonedFrom = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, searchConditionsGroupType = "And", searchSortFieldNamesList = NULL, searchSortFieldNamesDescendingList = NULL, entityID = 1, schoolYearID = NULL, page = 1, pageSize = 100000, flatten = T, ...){

		params <- as.list(environment()) %>% append(list(...))

		searchFields <- params %>% keep(names(params) %>% str_sub(1,1) == names(params) %>% str_sub(1,1) %>% str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- "all", searchFields <- searchFields %>% keep(~.x) %>% names())

		listSkyObjects(module = "Scheduling", objectName = "SectionSchedulingTeam", schoolYearID = schoolYearID, searchFields = searchFields, page = page, pageSize = pageSize, SearchConditionsList = searchConditionsList, SearchConditionsGroupType = searchConditionsGroupType, SearchSortFieldNamesList = searchSortFieldNamesList, SearchSortFieldNamesDescendingList = searchSortFieldNamesDescendingList, entityId = entityID, flatten = flatten)
	}


	listDateRangePresets <- function(searchConditionsList = NULL, DateRangePresetID = F, EntityID = F, SchoolYearID = F, Code = F, Description = F, LowDate = F, HighDate = F, EntityGroupKey = F, DateRangePresetIDClonedFrom = F, CodeDescription = F, DateRangePresetIDClonedTo = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, searchConditionsGroupType = "And", searchSortFieldNamesList = NULL, searchSortFieldNamesDescendingList = NULL, entityID = 1, schoolYearID = NULL, page = 1, pageSize = 100000, flatten = T, ...){

		params <- as.list(environment()) %>% append(list(...))

		searchFields <- params %>% keep(names(params) %>% str_sub(1,1) == names(params) %>% str_sub(1,1) %>% str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- "all", searchFields <- searchFields %>% keep(~.x) %>% names())

		listSkyObjects(module = "Scheduling", objectName = "DateRangePreset", schoolYearID = schoolYearID, searchFields = searchFields, page = page, pageSize = pageSize, SearchConditionsList = searchConditionsList, SearchConditionsGroupType = searchConditionsGroupType, SearchSortFieldNamesList = searchSortFieldNamesList, SearchSortFieldNamesDescendingList = searchSortFieldNamesDescendingList, entityId = entityID, flatten = flatten)
	}


	listAvailabilityFilterCourseStudents <- function(searchConditionsList = NULL, AvailabilityFilterCourseStudentID = F, Description = F, AvailabilityCourseFilterID = F, AvailabilityStudentFilterID = F, AvailabilityFilterCourseStudentIDClonedFrom = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, ExcludeAvailabilityListInNightlyUpdateTask = F, ExcludeAvailabilityListBoolUpdatedManually = F, UseForCourseRequests = F, UseForArenaScheduling = F, CourseRequestStartDate = F, ArenaSchedulingStartDate = F, searchConditionsGroupType = "And", searchSortFieldNamesList = NULL, searchSortFieldNamesDescendingList = NULL, entityID = 1, schoolYearID = NULL, page = 1, pageSize = 100000, flatten = T, ...){

		params <- as.list(environment()) %>% append(list(...))

		searchFields <- params %>% keep(names(params) %>% str_sub(1,1) == names(params) %>% str_sub(1,1) %>% str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- "all", searchFields <- searchFields %>% keep(~.x) %>% names())

		listSkyObjects(module = "Scheduling", objectName = "AvailabilityFilterCourseStudent", schoolYearID = schoolYearID, searchFields = searchFields, page = page, pageSize = pageSize, SearchConditionsList = searchConditionsList, SearchConditionsGroupType = searchConditionsGroupType, SearchSortFieldNamesList = searchSortFieldNamesList, SearchSortFieldNamesDescendingList = searchSortFieldNamesDescendingList, entityId = entityID, flatten = flatten)
	}


	listAvailabilityStudentFilters <- function(searchConditionsList = NULL, AvailabilityStudentFilterID = F, Code = F, Description = F, Filter = F, EntityID = F, SchoolYearID = F, SkywardID = F, GradeReferenceIDInclusionList = F, StudentTypeIDInclusionList = F, AvailabilityStudentFilterIDClonedFrom = F, CodeDescription = F, AvailabilityStudentFilterIDClonedTo = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, searchConditionsGroupType = "And", searchSortFieldNamesList = NULL, searchSortFieldNamesDescendingList = NULL, entityID = 1, schoolYearID = NULL, page = 1, pageSize = 100000, flatten = T, ...){

		params <- as.list(environment()) %>% append(list(...))

		searchFields <- params %>% keep(names(params) %>% str_sub(1,1) == names(params) %>% str_sub(1,1) %>% str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- "all", searchFields <- searchFields %>% keep(~.x) %>% names())

		listSkyObjects(module = "Scheduling", objectName = "AvailabilityStudentFilter", schoolYearID = schoolYearID, searchFields = searchFields, page = page, pageSize = pageSize, SearchConditionsList = searchConditionsList, SearchConditionsGroupType = searchConditionsGroupType, SearchSortFieldNamesList = searchSortFieldNamesList, SearchSortFieldNamesDescendingList = searchSortFieldNamesDescendingList, entityId = entityID, flatten = flatten)
	}


	listAvailabilityCourseFilters <- function(searchConditionsList = NULL, AvailabilityCourseFilterID = F, Code = F, Description = F, Filter = F, EntityID = F, SchoolYearID = F, SkywardID = F, IncludeCoursesWithNoCourseType = F, CourseTypeIDInclusionList = F, IncludeCategoryLunch = F, IncludeCategoryRegular = F, IncludeCategoryStudyHall = F, IncludeElective = F, IncludeRequired = F, IncludeSchedulingTypeNormal = F, IncludeSchedulingTypeManuallyScheduled = F, IncludeSchedulingTypeSpecialEducation = F, IncludeOfferedCourses = F, AvailabilityCourseFilterIDClonedFrom = F, CodeDescription = F, AvailabilityCourseFilterIDClonedTo = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, IncludeInactiveCourses = F, GradeLevelFilterType = F, GradeReferenceIDInclusionList = F, searchConditionsGroupType = "And", searchSortFieldNamesList = NULL, searchSortFieldNamesDescendingList = NULL, entityID = 1, schoolYearID = NULL, page = 1, pageSize = 100000, flatten = T, ...){

		params <- as.list(environment()) %>% append(list(...))

		searchFields <- params %>% keep(names(params) %>% str_sub(1,1) == names(params) %>% str_sub(1,1) %>% str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- "all", searchFields <- searchFields %>% keep(~.x) %>% names())

		listSkyObjects(module = "Scheduling", objectName = "AvailabilityCourseFilter", schoolYearID = schoolYearID, searchFields = searchFields, page = page, pageSize = pageSize, SearchConditionsList = searchConditionsList, SearchConditionsGroupType = searchConditionsGroupType, SearchSortFieldNamesList = searchSortFieldNamesList, SearchSortFieldNamesDescendingList = searchSortFieldNamesDescendingList, entityId = entityID, flatten = flatten)
	}


	listScheduleRestorePoints <- function(searchConditionsList = NULL, ScheduleRestorePointID = F, Name = F, Description = F, EntityID = F, SchoolYearID = F, RestorePointDateTime = F, NameDescription = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, ReportQueueID = F, searchConditionsGroupType = "And", searchSortFieldNamesList = NULL, searchSortFieldNamesDescendingList = NULL, entityID = 1, schoolYearID = NULL, page = 1, pageSize = 100000, flatten = T, ...){

		params <- as.list(environment()) %>% append(list(...))

		searchFields <- params %>% keep(names(params) %>% str_sub(1,1) == names(params) %>% str_sub(1,1) %>% str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- "all", searchFields <- searchFields %>% keep(~.x) %>% names())

		listSkyObjects(module = "Scheduling", objectName = "ScheduleRestorePoint", schoolYearID = schoolYearID, searchFields = searchFields, page = page, pageSize = pageSize, SearchConditionsList = searchConditionsList, SearchConditionsGroupType = searchConditionsGroupType, SearchSortFieldNamesList = searchSortFieldNamesList, SearchSortFieldNamesDescendingList = searchSortFieldNamesDescendingList, entityId = entityID, flatten = flatten)
	}


	listCourseAlternates <- function(searchConditionsList = NULL, CourseAlternateID = F, CourseIDPrimary = F, CourseIDAlternate = F, CourseAlternateIDClonedFrom = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, searchConditionsGroupType = "And", searchSortFieldNamesList = NULL, searchSortFieldNamesDescendingList = NULL, entityID = 1, schoolYearID = NULL, page = 1, pageSize = 100000, flatten = T, ...){

		params <- as.list(environment()) %>% append(list(...))

		searchFields <- params %>% keep(names(params) %>% str_sub(1,1) == names(params) %>% str_sub(1,1) %>% str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- "all", searchFields <- searchFields %>% keep(~.x) %>% names())

		listSkyObjects(module = "Scheduling", objectName = "CourseAlternate", schoolYearID = schoolYearID, searchFields = searchFields, page = page, pageSize = pageSize, SearchConditionsList = searchConditionsList, SearchConditionsGroupType = searchConditionsGroupType, SearchSortFieldNamesList = searchSortFieldNamesList, SearchSortFieldNamesDescendingList = searchSortFieldNamesDescendingList, entityId = entityID, flatten = flatten)
	}


	listSectionSchedulerCourseConstraints <- function(searchConditionsList = NULL, SectionSchedulerCourseConstraintID = F, CourseID = F, CourseIDLinked = F, SectionIDCurrentCourseSelected = F, SectionIDLinkedCourseSelected = F, LinkedCourse = F, Rule = F, CurrentCourseSection = F, LinkedCourseSection = F, IsActive = F, ScheduleLinkedCourseFirst = F, CurrentCourseTopSectionCount = F, LinkedCourseTopSectionCount = F, SectionSchedulerCourseConstraintIDClonedFrom = F, SectionSchedulerCourseConstraintIDClonedTo = F, ScheduleFirst = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, searchConditionsGroupType = "And", searchSortFieldNamesList = NULL, searchSortFieldNamesDescendingList = NULL, entityID = 1, schoolYearID = NULL, page = 1, pageSize = 100000, flatten = T, ...){

		params <- as.list(environment()) %>% append(list(...))

		searchFields <- params %>% keep(names(params) %>% str_sub(1,1) == names(params) %>% str_sub(1,1) %>% str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- "all", searchFields <- searchFields %>% keep(~.x) %>% names())

		listSkyObjects(module = "Scheduling", objectName = "SectionSchedulerCourseConstraint", schoolYearID = schoolYearID, searchFields = searchFields, page = page, pageSize = pageSize, SearchConditionsList = searchConditionsList, SearchConditionsGroupType = searchConditionsGroupType, SearchSortFieldNamesList = searchSortFieldNamesList, SearchSortFieldNamesDescendingList = searchSortFieldNamesDescendingList, entityId = entityID, flatten = flatten)
	}


	listMeetRequirements <- function(searchConditionsList = NULL, MeetRequirementID = F, DaysPerRotation = F, MinutesPerDay = F, AllowMultiplePeriodsPerDayRotation = F, ForceConsecutivePeriods = F, MeetRequirementIDClonedFrom = F, MeetRequirementIDClonedTo = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, CourseID = F, TimeSpanAnalysisType = F, searchConditionsGroupType = "And", searchSortFieldNamesList = NULL, searchSortFieldNamesDescendingList = NULL, entityID = 1, schoolYearID = NULL, page = 1, pageSize = 100000, flatten = T, ...){

		params <- as.list(environment()) %>% append(list(...))

		searchFields <- params %>% keep(names(params) %>% str_sub(1,1) == names(params) %>% str_sub(1,1) %>% str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- "all", searchFields <- searchFields %>% keep(~.x) %>% names())

		listSkyObjects(module = "Scheduling", objectName = "MeetRequirement", schoolYearID = schoolYearID, searchFields = searchFields, page = page, pageSize = pageSize, SearchConditionsList = searchConditionsList, SearchConditionsGroupType = searchConditionsGroupType, SearchSortFieldNamesList = searchSortFieldNamesList, SearchSortFieldNamesDescendingList = searchSortFieldNamesDescendingList, entityId = entityID, flatten = flatten)
	}


	listStudentCourseRequests <- function(searchConditionsList = NULL, StudentCourseRequestID = F, StudentID = F, CourseID = F, EntityIDRequestedFrom = F, CourseEntityOfferedToID = F, IsAlternate = F, AlternateRank = F, StudentCourseRequestIDAlternateFor = F, StudentSectionID = F, RequestStatus = F, CourseNotScheduled = F, CourseRequested = F, CourseScheduled = F, CourseConflict = F, CourseScheduledAndInProgress = F, CourseScheduledAndIsBeforeOrInProgress = F, RequestSource = F, SchedulingMethod = F, SectionLengthSubsetSummary = F, SectionLengthSubsetCode = F, SectionLengthSubsetDescription = F, CountsAgainstRequestLimit = F, EarnedCreditsRequested = F, EarnedCreditsPossibleAnticipated = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, PrerequisiteMet = F, CourseScheduledAndAllTransactionsCountTowardsViewingEntity = F, RequestedFromViewingEntity = F, DisplayToViewingEntity = F, CourseNotScheduledAndIsRequestedFromViewingEntity = F, CourseScheduledAndIsRequestedFromOfferingEntityAndAllTransactionsCountTowardsViewingEntity = F, CourseScheduledAndCountsTowardsViewingEntity = F, ViewingFromOfferingEntity = F, RequestedFromOfferingEntity = F, CourseScheduledAndIsBeforeOrInProgressAndNotHasAtleastOneTransactionNotCountTowardsViewingEntity = F, CourseScheduledAndInProgressAndIsEffectiveToViewingEntity = F, CourseScheduledAndAllTransactionsCountTowardsViewingEntityAndCourseIsNotDropOrTransfer = F, CourseScheduledAndRequestedFromViewingEntityAndAllTransactionsCountTowardsViewingEntity = F, CrsIsNotDrpOrTransferAndNotScheduledOrCrsScheduledAndRequestedFromViewingEntityAndAllTransactionsCntTowardsViewingEntity = F, CourseIsNotDroppedManualOrTransfer = F, searchConditionsGroupType = "And", searchSortFieldNamesList = NULL, searchSortFieldNamesDescendingList = NULL, entityID = 1, schoolYearID = NULL, page = 1, pageSize = 100000, flatten = T, ...){

		params <- as.list(environment()) %>% append(list(...))

		searchFields <- params %>% keep(names(params) %>% str_sub(1,1) == names(params) %>% str_sub(1,1) %>% str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- "all", searchFields <- searchFields %>% keep(~.x) %>% names())

		listSkyObjects(module = "Scheduling", objectName = "StudentCourseRequest", schoolYearID = schoolYearID, searchFields = searchFields, page = page, pageSize = pageSize, SearchConditionsList = searchConditionsList, SearchConditionsGroupType = searchConditionsGroupType, SearchSortFieldNamesList = searchSortFieldNamesList, SearchSortFieldNamesDescendingList = searchSortFieldNamesDescendingList, entityId = entityID, flatten = flatten)
	}


	listCourseCorequisiteGroupCourses <- function(searchConditionsList = NULL, CourseCorequisiteGroupCourseID = F, CourseCorequisiteGroupID = F, CourseID = F, EntityGroupKey = F, CourseCorequisiteGroupCourseIDClonedFrom = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, searchConditionsGroupType = "And", searchSortFieldNamesList = NULL, searchSortFieldNamesDescendingList = NULL, entityID = 1, schoolYearID = NULL, page = 1, pageSize = 100000, flatten = T, ...){

		params <- as.list(environment()) %>% append(list(...))

		searchFields <- params %>% keep(names(params) %>% str_sub(1,1) == names(params) %>% str_sub(1,1) %>% str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- "all", searchFields <- searchFields %>% keep(~.x) %>% names())

		listSkyObjects(module = "Scheduling", objectName = "CourseCorequisiteGroupCourse", schoolYearID = schoolYearID, searchFields = searchFields, page = page, pageSize = pageSize, SearchConditionsList = searchConditionsList, SearchConditionsGroupType = searchConditionsGroupType, SearchSortFieldNamesList = searchSortFieldNamesList, SearchSortFieldNamesDescendingList = searchSortFieldNamesDescendingList, entityId = entityID, flatten = flatten)
	}


	listMeetSummaries <- function(searchConditionsList = NULL, MeetSummaryID = F, MeetID = F, Period = F, IsPrimary = F, Days = F, EntityIDViewing = F, CalendarID = F, IsDefaultCalendar = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, HasOnlyHiddenDetails = F, CalculatedPeriod = F, CalculatedDays = F, CalculatedPeriodStudent = F, CalculatedDaysStudent = F, searchConditionsGroupType = "And", searchSortFieldNamesList = NULL, searchSortFieldNamesDescendingList = NULL, entityID = 1, schoolYearID = NULL, page = 1, pageSize = 100000, flatten = T, ...){

		params <- as.list(environment()) %>% append(list(...))

		searchFields <- params %>% keep(names(params) %>% str_sub(1,1) == names(params) %>% str_sub(1,1) %>% str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- "all", searchFields <- searchFields %>% keep(~.x) %>% names())

		listSkyObjects(module = "Scheduling", objectName = "MeetSummary", schoolYearID = schoolYearID, searchFields = searchFields, page = page, pageSize = pageSize, SearchConditionsList = searchConditionsList, SearchConditionsGroupType = searchConditionsGroupType, SearchSortFieldNamesList = searchSortFieldNamesList, SearchSortFieldNamesDescendingList = searchSortFieldNamesDescendingList, entityId = entityID, flatten = flatten)
	}


	listCourseCustomRequirements <- function(searchConditionsList = NULL, CourseCustomRequirementID = F, CourseID = F, CustomRequirementID = F, EntityGroupKey = F, CourseCustomRequirementIDClonedFrom = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, searchConditionsGroupType = "And", searchSortFieldNamesList = NULL, searchSortFieldNamesDescendingList = NULL, entityID = 1, schoolYearID = NULL, page = 1, pageSize = 100000, flatten = T, ...){

		params <- as.list(environment()) %>% append(list(...))

		searchFields <- params %>% keep(names(params) %>% str_sub(1,1) == names(params) %>% str_sub(1,1) %>% str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- "all", searchFields <- searchFields %>% keep(~.x) %>% names())

		listSkyObjects(module = "Scheduling", objectName = "CourseCustomRequirement", schoolYearID = schoolYearID, searchFields = searchFields, page = page, pageSize = pageSize, SearchConditionsList = searchConditionsList, SearchConditionsGroupType = searchConditionsGroupType, SearchSortFieldNamesList = searchSortFieldNamesList, SearchSortFieldNamesDescendingList = searchSortFieldNamesDescendingList, entityId = entityID, flatten = flatten)
	}


	listCourseCorequisiteGroups <- function(searchConditionsList = NULL, CourseCorequisiteGroupID = F, Name = F, Description = F, DisplayPeriodMatch = F, StaffMatch = F, Overlap = F, EntityID = F, SchoolYearID = F, EntityGroupKey = F, CourseCorequisiteGroupIDClonedFrom = F, AutomaticRequestSetting = F, ScheduleAllCoursesInGroupOrNone = F, NameDescription = F, CourseCorequisiteGroupIDClonedTo = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, searchConditionsGroupType = "And", searchSortFieldNamesList = NULL, searchSortFieldNamesDescendingList = NULL, entityID = 1, schoolYearID = NULL, page = 1, pageSize = 100000, flatten = T, ...){

		params <- as.list(environment()) %>% append(list(...))

		searchFields <- params %>% keep(names(params) %>% str_sub(1,1) == names(params) %>% str_sub(1,1) %>% str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- "all", searchFields <- searchFields %>% keep(~.x) %>% names())

		listSkyObjects(module = "Scheduling", objectName = "CourseCorequisiteGroup", schoolYearID = schoolYearID, searchFields = searchFields, page = page, pageSize = pageSize, SearchConditionsList = searchConditionsList, SearchConditionsGroupType = searchConditionsGroupType, SearchSortFieldNamesList = searchSortFieldNamesList, SearchSortFieldNamesDescendingList = searchSortFieldNamesDescendingList, entityId = entityID, flatten = flatten)
	}


	listCourseGroups <- function(searchConditionsList = NULL, CourseGroupID = F, Code = F, Description = F, DistrictID = F, DistrictGroupKey = F, CodeDescription = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, searchConditionsGroupType = "And", searchSortFieldNamesList = NULL, searchSortFieldNamesDescendingList = NULL, entityID = 1, schoolYearID = NULL, page = 1, pageSize = 100000, flatten = T, ...){

		params <- as.list(environment()) %>% append(list(...))

		searchFields <- params %>% keep(names(params) %>% str_sub(1,1) == names(params) %>% str_sub(1,1) %>% str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- "all", searchFields <- searchFields %>% keep(~.x) %>% names())

		listSkyObjects(module = "Scheduling", objectName = "CourseGroup", schoolYearID = schoolYearID, searchFields = searchFields, page = page, pageSize = pageSize, SearchConditionsList = searchConditionsList, SearchConditionsGroupType = searchConditionsGroupType, SearchSortFieldNamesList = searchSortFieldNamesList, SearchSortFieldNamesDescendingList = searchSortFieldNamesDescendingList, entityId = entityID, flatten = flatten)
	}


	listSectionCorequisiteGroups <- function(searchConditionsList = NULL, SectionCorequisiteGroupID = F, Name = F, Description = F, EntityID = F, AutomaticRequestSetting = F, AutomaticScheduleSetting = F, ScheduleAllSectionsInGroupOrNone = F, NameDescription = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, SectionCorequisiteGroupIDClonedTo = F, searchConditionsGroupType = "And", searchSortFieldNamesList = NULL, searchSortFieldNamesDescendingList = NULL, entityID = 1, schoolYearID = NULL, page = 1, pageSize = 100000, flatten = T, ...){

		params <- as.list(environment()) %>% append(list(...))

		searchFields <- params %>% keep(names(params) %>% str_sub(1,1) == names(params) %>% str_sub(1,1) %>% str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- "all", searchFields <- searchFields %>% keep(~.x) %>% names())

		listSkyObjects(module = "Scheduling", objectName = "SectionCorequisiteGroup", schoolYearID = schoolYearID, searchFields = searchFields, page = page, pageSize = pageSize, SearchConditionsList = searchConditionsList, SearchConditionsGroupType = searchConditionsGroupType, SearchSortFieldNamesList = searchSortFieldNamesList, SearchSortFieldNamesDescendingList = searchSortFieldNamesDescendingList, entityId = entityID, flatten = flatten)
	}


	listSectionCustomRequirements <- function(searchConditionsList = NULL, SectionCustomRequirementID = F, SectionID = F, CustomRequirementID = F, SectionCustomRequirementIDClonedFrom = F, SectionCustomRequirementIDClonedTo = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, searchConditionsGroupType = "And", searchSortFieldNamesList = NULL, searchSortFieldNamesDescendingList = NULL, entityID = 1, schoolYearID = NULL, page = 1, pageSize = 100000, flatten = T, ...){

		params <- as.list(environment()) %>% append(list(...))

		searchFields <- params %>% keep(names(params) %>% str_sub(1,1) == names(params) %>% str_sub(1,1) %>% str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- "all", searchFields <- searchFields %>% keep(~.x) %>% names())

		listSkyObjects(module = "Scheduling", objectName = "SectionCustomRequirement", schoolYearID = schoolYearID, searchFields = searchFields, page = page, pageSize = pageSize, SearchConditionsList = searchConditionsList, SearchConditionsGroupType = searchConditionsGroupType, SearchSortFieldNamesList = searchSortFieldNamesList, SearchSortFieldNamesDescendingList = searchSortFieldNamesDescendingList, entityId = entityID, flatten = flatten)
	}


	listSectionCorequisiteGroupSections <- function(searchConditionsList = NULL, SectionCorequisiteGroupSectionID = F, SectionCorequisiteGroupID = F, SectionID = F, SectionCorequisiteGroupSectionIDClonedFrom = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, searchConditionsGroupType = "And", searchSortFieldNamesList = NULL, searchSortFieldNamesDescendingList = NULL, entityID = 1, schoolYearID = NULL, page = 1, pageSize = 100000, flatten = T, ...){

		params <- as.list(environment()) %>% append(list(...))

		searchFields <- params %>% keep(names(params) %>% str_sub(1,1) == names(params) %>% str_sub(1,1) %>% str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- "all", searchFields <- searchFields %>% keep(~.x) %>% names())

		listSkyObjects(module = "Scheduling", objectName = "SectionCorequisiteGroupSection", schoolYearID = schoolYearID, searchFields = searchFields, page = page, pageSize = pageSize, SearchConditionsList = searchConditionsList, SearchConditionsGroupType = searchConditionsGroupType, SearchSortFieldNamesList = searchSortFieldNamesList, SearchSortFieldNamesDescendingList = searchSortFieldNamesDescendingList, entityId = entityID, flatten = flatten)
	}


	listCourseGroupCourses <- function(searchConditionsList = NULL, CourseGroupCourseID = F, CourseID = F, CourseGroupID = F, EntityGroupKey = F, CourseGroupCourseIDClonedFrom = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, searchConditionsGroupType = "And", searchSortFieldNamesList = NULL, searchSortFieldNamesDescendingList = NULL, entityID = 1, schoolYearID = NULL, page = 1, pageSize = 100000, flatten = T, ...){

		params <- as.list(environment()) %>% append(list(...))

		searchFields <- params %>% keep(names(params) %>% str_sub(1,1) == names(params) %>% str_sub(1,1) %>% str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- "all", searchFields <- searchFields %>% keep(~.x) %>% names())

		listSkyObjects(module = "Scheduling", objectName = "CourseGroupCourse", schoolYearID = schoolYearID, searchFields = searchFields, page = page, pageSize = pageSize, SearchConditionsList = searchConditionsList, SearchConditionsGroupType = searchConditionsGroupType, SearchSortFieldNamesList = searchSortFieldNamesList, SearchSortFieldNamesDescendingList = searchSortFieldNamesDescendingList, entityId = entityID, flatten = flatten)
	}


	listCustomRequirements <- function(searchConditionsList = NULL, CustomRequirementID = F, DistrictID = F, Code = F, Description = F, StudentCondition = F, DistrictGroupKey = F, CodeDescription = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, searchConditionsGroupType = "And", searchSortFieldNamesList = NULL, searchSortFieldNamesDescendingList = NULL, entityID = 1, schoolYearID = NULL, page = 1, pageSize = 100000, flatten = T, ...){

		params <- as.list(environment()) %>% append(list(...))

		searchFields <- params %>% keep(names(params) %>% str_sub(1,1) == names(params) %>% str_sub(1,1) %>% str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- "all", searchFields <- searchFields %>% keep(~.x) %>% names())

		listSkyObjects(module = "Scheduling", objectName = "CustomRequirement", schoolYearID = schoolYearID, searchFields = searchFields, page = page, pageSize = pageSize, SearchConditionsList = searchConditionsList, SearchConditionsGroupType = searchConditionsGroupType, SearchSortFieldNamesList = searchSortFieldNamesList, SearchSortFieldNamesDescendingList = searchSortFieldNamesDescendingList, entityId = entityID, flatten = flatten)
	}


	listCourseSubjects <- function(searchConditionsList = NULL, CourseSubjectID = F, Code = F, Description = F, EntityID = F, SchoolYearID = F, EntityGroupKey = F, CourseSubjectIDClonedFrom = F, CodeDescription = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, CourseSubjectIDClonedTo = F, searchConditionsGroupType = "And", searchSortFieldNamesList = NULL, searchSortFieldNamesDescendingList = NULL, entityID = 1, schoolYearID = NULL, page = 1, pageSize = 100000, flatten = T, ...){

		params <- as.list(environment()) %>% append(list(...))

		searchFields <- params %>% keep(names(params) %>% str_sub(1,1) == names(params) %>% str_sub(1,1) %>% str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- "all", searchFields <- searchFields %>% keep(~.x) %>% names())

		listSkyObjects(module = "Scheduling", objectName = "CourseSubject", schoolYearID = schoolYearID, searchFields = searchFields, page = page, pageSize = pageSize, SearchConditionsList = searchConditionsList, SearchConditionsGroupType = searchConditionsGroupType, SearchSortFieldNamesList = searchSortFieldNamesList, SearchSortFieldNamesDescendingList = searchSortFieldNamesDescendingList, entityId = entityID, flatten = flatten)
	}


	listCourseTypes <- function(searchConditionsList = NULL, CourseTypeID = F, Code = F, Description = F, EntityID = F, SchoolYearID = F, EntityGroupKey = F, CourseTypeIDClonedFrom = F, CourseTypeIDClonedTo = F, CodeDescription = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, searchConditionsGroupType = "And", searchSortFieldNamesList = NULL, searchSortFieldNamesDescendingList = NULL, entityID = 1, schoolYearID = NULL, page = 1, pageSize = 100000, flatten = T, ...){

		params <- as.list(environment()) %>% append(list(...))

		searchFields <- params %>% keep(names(params) %>% str_sub(1,1) == names(params) %>% str_sub(1,1) %>% str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- "all", searchFields <- searchFields %>% keep(~.x) %>% names())

		listSkyObjects(module = "Scheduling", objectName = "CourseType", schoolYearID = schoolYearID, searchFields = searchFields, page = page, pageSize = pageSize, SearchConditionsList = searchConditionsList, SearchConditionsGroupType = searchConditionsGroupType, SearchSortFieldNamesList = searchSortFieldNamesList, SearchSortFieldNamesDescendingList = searchSortFieldNamesDescendingList, entityId = entityID, flatten = flatten)
	}


	listCourseLengths <- function(searchConditionsList = NULL, CourseLengthID = F, EntityGroupKey = F, Code = F, Description = F, DefaultEarnedCredits = F, EntityID = F, SchoolYearID = F, CourseLengthIDClonedFrom = F, CodeDescription = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, CourseLengthIDClonedTo = F, SectionLengthCount = F, searchConditionsGroupType = "And", searchSortFieldNamesList = NULL, searchSortFieldNamesDescendingList = NULL, entityID = 1, schoolYearID = NULL, page = 1, pageSize = 100000, flatten = T, ...){

		params <- as.list(environment()) %>% append(list(...))

		searchFields <- params %>% keep(names(params) %>% str_sub(1,1) == names(params) %>% str_sub(1,1) %>% str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- "all", searchFields <- searchFields %>% keep(~.x) %>% names())

		listSkyObjects(module = "Scheduling", objectName = "CourseLength", schoolYearID = schoolYearID, searchFields = searchFields, page = page, pageSize = pageSize, SearchConditionsList = searchConditionsList, SearchConditionsGroupType = searchConditionsGroupType, SearchSortFieldNamesList = searchSortFieldNamesList, SearchSortFieldNamesDescendingList = searchSortFieldNamesDescendingList, entityId = entityID, flatten = flatten)
	}


	listSectionDefaults <- function(searchConditionsList = NULL, SectionDefaultID = F, CourseID = F, MinimumStudentCount = F, MaximumStudentCount = F, OptimalStudentCount = F, SectionDefaultIDClonedFrom = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, searchConditionsGroupType = "And", searchSortFieldNamesList = NULL, searchSortFieldNamesDescendingList = NULL, entityID = 1, schoolYearID = NULL, page = 1, pageSize = 100000, flatten = T, ...){

		params <- as.list(environment()) %>% append(list(...))

		searchFields <- params %>% keep(names(params) %>% str_sub(1,1) == names(params) %>% str_sub(1,1) %>% str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- "all", searchFields <- searchFields %>% keep(~.x) %>% names())

		listSkyObjects(module = "Scheduling", objectName = "SectionDefault", schoolYearID = schoolYearID, searchFields = searchFields, page = page, pageSize = pageSize, SearchConditionsList = searchConditionsList, SearchConditionsGroupType = searchConditionsGroupType, SearchSortFieldNamesList = searchSortFieldNamesList, SearchSortFieldNamesDescendingList = searchSortFieldNamesDescendingList, entityId = entityID, flatten = flatten)
	}


	listMeets <- function(searchConditionsList = NULL, MeetID = F, SectionID = F, EntityID = F, SchoolYearID = F, RoomID = F, Type = F, StartDate = F, EndDate = F, OverridePeriodStartTime = F, OverridePeriodEndTime = F, IsPrimary = F, LockAllMeetDisplayPeriodsFromSectionScheduler = F, MeetIDClonedFrom = F, DurationInMinutes = F, TotalMeetDisplayPeriodCount = F, IsAssignedToAHomeroomRoom = F, TotalStaffMeetCount = F, MeetIDClonedTo = F, ViewingFromOfferingEntity = F, StartDateEndDateBuildingCodeRoomNumber = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, DisplayPeriodRotationID = F, HasDisplayPeriodRotationAssigned = F, LockAllStaffMeetsFromSectionScheduler = F, LockRoomFromSectionScheduler = F, LockAllMeetDayRotationsFromSectionScheduler = F, CalendarIDStaff = F, RoomSeatsAvailable = F, searchConditionsGroupType = "And", searchSortFieldNamesList = NULL, searchSortFieldNamesDescendingList = NULL, entityID = 1, schoolYearID = NULL, page = 1, pageSize = 100000, flatten = T, ...){

		params <- as.list(environment()) %>% append(list(...))

		searchFields <- params %>% keep(names(params) %>% str_sub(1,1) == names(params) %>% str_sub(1,1) %>% str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- "all", searchFields <- searchFields %>% keep(~.x) %>% names())

		listSkyObjects(module = "Scheduling", objectName = "Meet", schoolYearID = schoolYearID, searchFields = searchFields, page = page, pageSize = pageSize, SearchConditionsList = searchConditionsList, SearchConditionsGroupType = searchConditionsGroupType, SearchSortFieldNamesList = searchSortFieldNamesList, SearchSortFieldNamesDescendingList = searchSortFieldNamesDescendingList, entityId = entityID, flatten = flatten)
	}


	listMeetDisplayPeriods <- function(searchConditionsList = NULL, MeetDisplayPeriodID = F, MeetID = F, DisplayPeriodID = F, IsPrimary = F, HideMeetDisplayPeriod = F, LockDisplayPeriod = F, ScheduledBySectionScheduler = F, MeetDisplayPeriodIDClonedFrom = F, ViewingFromOfferingEntity = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, MeetDisplayPeriodStartTime = F, MeetDisplayPeriodEndTime = F, searchConditionsGroupType = "And", searchSortFieldNamesList = NULL, searchSortFieldNamesDescendingList = NULL, entityID = 1, schoolYearID = NULL, page = 1, pageSize = 100000, flatten = T, ...){

		params <- as.list(environment()) %>% append(list(...))

		searchFields <- params %>% keep(names(params) %>% str_sub(1,1) == names(params) %>% str_sub(1,1) %>% str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- "all", searchFields <- searchFields %>% keep(~.x) %>% names())

		listSkyObjects(module = "Scheduling", objectName = "MeetDisplayPeriod", schoolYearID = schoolYearID, searchFields = searchFields, page = page, pageSize = pageSize, SearchConditionsList = searchConditionsList, SearchConditionsGroupType = searchConditionsGroupType, SearchSortFieldNamesList = searchSortFieldNamesList, SearchSortFieldNamesDescendingList = searchSortFieldNamesDescendingList, entityId = entityID, flatten = flatten)
	}


	listStaffMeets <- function(searchConditionsList = NULL, StaffMeetID = F, MeetID = F, StaffID = F, IsPrimary = F, CanMakeClosedGradingPeriodGradeChange = F, ApplyClosedGradingPeriodGradeChangePermission = F, NameMeetDescription = F, HasGradebookAccess = F, HasAttendanceAccess = F, EffectiveStartDate = F, EffectiveEndDate = F, GradebookLastAccessedTime = F, StaffMeetIDClonedFrom = F, MeetsToday = F, ViewingFromOfferingEntity = F, MeetIsCurrent = F, AssignmentCount = F, ClosedGradingPeriodGradeChangeCount = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, IsSubstitute = F, StaffMeetIDSubstituteFor = F, HasAttendanceAccessAsOfDate = F, SectionID = F, SchoolYearID = F, IsStaffCertified = F, ScheduledBySectionScheduler = F, LockStaffFromSectionScheduler = F, IsLongTermSubstitute = F, searchConditionsGroupType = "And", searchSortFieldNamesList = NULL, searchSortFieldNamesDescendingList = NULL, entityID = 1, schoolYearID = NULL, page = 1, pageSize = 100000, flatten = T, ...){

		params <- as.list(environment()) %>% append(list(...))

		searchFields <- params %>% keep(names(params) %>% str_sub(1,1) == names(params) %>% str_sub(1,1) %>% str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- "all", searchFields <- searchFields %>% keep(~.x) %>% names())

		listSkyObjects(module = "Scheduling", objectName = "StaffMeet", schoolYearID = schoolYearID, searchFields = searchFields, page = page, pageSize = pageSize, SearchConditionsList = searchConditionsList, SearchConditionsGroupType = searchConditionsGroupType, SearchSortFieldNamesList = searchSortFieldNamesList, SearchSortFieldNamesDescendingList = searchSortFieldNamesDescendingList, entityId = entityID, flatten = flatten)
	}


	listSectionSchedulingCategories <- function(searchConditionsList = NULL, SectionSchedulingCategoryID = F, SchedulingCategoryID = F, SectionID = F, SectionSchedulingCategoryIDClonedFrom = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, searchConditionsGroupType = "And", searchSortFieldNamesList = NULL, searchSortFieldNamesDescendingList = NULL, entityID = 1, schoolYearID = NULL, page = 1, pageSize = 100000, flatten = T, ...){

		params <- as.list(environment()) %>% append(list(...))

		searchFields <- params %>% keep(names(params) %>% str_sub(1,1) == names(params) %>% str_sub(1,1) %>% str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- "all", searchFields <- searchFields %>% keep(~.x) %>% names())

		listSkyObjects(module = "Scheduling", objectName = "SectionSchedulingCategory", schoolYearID = schoolYearID, searchFields = searchFields, page = page, pageSize = pageSize, SearchConditionsList = searchConditionsList, SearchConditionsGroupType = searchConditionsGroupType, SearchSortFieldNamesList = searchSortFieldNamesList, SearchSortFieldNamesDescendingList = searchSortFieldNamesDescendingList, entityId = entityID, flatten = flatten)
	}


	listSchedulingPeriodDisplayPeriods <- function(searchConditionsList = NULL, SchedulingPeriodDisplayPeriodID = F, SchedulingPeriodID = F, DisplayPeriodID = F, EntityGroupKey = F, SchedulingPeriodDisplayPeriodIDClonedFrom = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, searchConditionsGroupType = "And", searchSortFieldNamesList = NULL, searchSortFieldNamesDescendingList = NULL, entityID = 1, schoolYearID = NULL, page = 1, pageSize = 100000, flatten = T, ...){

		params <- as.list(environment()) %>% append(list(...))

		searchFields <- params %>% keep(names(params) %>% str_sub(1,1) == names(params) %>% str_sub(1,1) %>% str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- "all", searchFields <- searchFields %>% keep(~.x) %>% names())

		listSkyObjects(module = "Scheduling", objectName = "SchedulingPeriodDisplayPeriod", schoolYearID = schoolYearID, searchFields = searchFields, page = page, pageSize = pageSize, SearchConditionsList = searchConditionsList, SearchConditionsGroupType = searchConditionsGroupType, SearchSortFieldNamesList = searchSortFieldNamesList, SearchSortFieldNamesDescendingList = searchSortFieldNamesDescendingList, entityId = entityID, flatten = flatten)
	}


	listStudentEntityYearSchedulingCategories <- function(searchConditionsList = NULL, StudentEntityYearSchedulingCategoryID = F, SchedulingCategoryID = F, StudentEntityYearID = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, searchConditionsGroupType = "And", searchSortFieldNamesList = NULL, searchSortFieldNamesDescendingList = NULL, entityID = 1, schoolYearID = NULL, page = 1, pageSize = 100000, flatten = T, ...){

		params <- as.list(environment()) %>% append(list(...))

		searchFields <- params %>% keep(names(params) %>% str_sub(1,1) == names(params) %>% str_sub(1,1) %>% str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- "all", searchFields <- searchFields %>% keep(~.x) %>% names())

		listSkyObjects(module = "Scheduling", objectName = "StudentEntityYearSchedulingCategory", schoolYearID = schoolYearID, searchFields = searchFields, page = page, pageSize = pageSize, SearchConditionsList = searchConditionsList, SearchConditionsGroupType = searchConditionsGroupType, SearchSortFieldNamesList = searchSortFieldNamesList, SearchSortFieldNamesDescendingList = searchSortFieldNamesDescendingList, entityId = entityID, flatten = flatten)
	}


	listStudentSectionTransactions <- function(searchConditionsList = NULL, StudentSectionTransactionID = F, StudentSectionID = F, SectionLengthSubsetID = F, EntityIDCountsAs = F, StartDate = F, EndDate = F, NameIDRequestedBy = F, EarlyExitReasonID = F, HideNewStudentButton = F, SectionID = F, IsCECE = F, CalendarID = F, IsInProgressAsOfToday = F, EndsAfterSectionLengthStartDate = F, IsInProgress = F, OverlapsSectionLength = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, CountsTowardsViewingEntity = F, StudentID = F, searchConditionsGroupType = "And", searchSortFieldNamesList = NULL, searchSortFieldNamesDescendingList = NULL, entityID = 1, schoolYearID = NULL, page = 1, pageSize = 100000, flatten = T, ...){

		params <- as.list(environment()) %>% append(list(...))

		searchFields <- params %>% keep(names(params) %>% str_sub(1,1) == names(params) %>% str_sub(1,1) %>% str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- "all", searchFields <- searchFields %>% keep(~.x) %>% names())

		listSkyObjects(module = "Scheduling", objectName = "StudentSectionTransaction", schoolYearID = schoolYearID, searchFields = searchFields, page = page, pageSize = pageSize, SearchConditionsList = searchConditionsList, SearchConditionsGroupType = searchConditionsGroupType, SearchSortFieldNamesList = searchSortFieldNamesList, SearchSortFieldNamesDescendingList = searchSortFieldNamesDescendingList, entityId = entityID, flatten = flatten)
	}


	listStudentSections <- function(searchConditionsList = NULL, StudentSectionID = F, StudentID = F, SectionID = F, ExcludeFromReportCardsAndTranscripts = F, IsTransferCourse = F, UseEarnedCreditOverride = F, EarnedCreditOverride = F, RenderTransferGradesRowButton = F, StudentSectionLink = F, ExcludeFromStudentSectionLink = F, EntityIDCourse = F, SchoolYearIDCourse = F, SectionLengthSubsetSummary = F, FailedCredits = F, EarnedCredits = F, EarnedCreditAttempted = F, LinkedStudentSectionsEarnedCredit = F, LinkedStudentSectionsFailedCredit = F, LinkedStudentSectionsEarnedCreditAttempted = F, DisplayLinkedStudentSection = F, IsWorkInProgress = F, UseEarnedCreditTotalOverride = F, TotalEarnedCreditOverride = F, TotalFailedCreditOverride = F, StartDate = F, EndDate = F, MultipleTransactions = F, Status = F, LastStudentSectionTransactionConsideredDropped = F, InProgress = F, IsBeforeOrInProgress = F, EarnedCreditsPossible = F, MissingAssignmentCount = F, IsFlaggedAsMissingAssignmentCount = F, UnscoredPastDueAssignmentCount = F, FilteredMissingAssignmentCount = F, FilteredGradedAssignmentCount = F, FilteredUnGradedAssignmentCount = F, StudentSectionNoteCountForCurrentUser = F, TotalDaysAbsent = F, TotalDaysExcused = F, TotalDaysOther = F, TotalDaysUnexcused = F, TotalDaysTardy = F, GradebookStudentNameToUse = F, HasLinkingConflicts = F, ViewingFromOfferingEntity = F, CountsForViewingEntity = F, TransactionCountsForViewingEntity = F, DisplayToViewingEntity = F, GradeReferenceID = F, TransferCourseName = F, StudentSectionCode = F, CourseOrTransferDescription = F, IsCurrentStudentSection = F, AssignmentDueDateAttendance = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, HasAtLeastOneCrossEntityStudentSectionTransaction = F, StudentSectionMNID = F, IsTSAProficient = F, HasStudentSectionNoteForCurrentUser = F, IsStudentSectionScheduledToMeet = F, IsForCurrentSchoolYear = F, IsActiveAsOfDate = F, IsActiveForTodayOrForSectionStartOrEnd = F, GradebookSortCode = F, HasStudentGradingScaleGroupForGradingPeriodGradeBucket = F, ActiveStudentGroups = F, IsAvailableForAssignmentStudentGroup = F, IsEffectiveForViewingEntity = F, HasAtleastOneTransactionNotCountTowardsViewingEntity = F, AllTransactionsCountTowardsViewingEntity = F, IsBeforeOrInProgressAndNotHasAtleastOneTransactionNotCountTowardsViewingEntity = F, RequestedFromOfferingEntityAndAllTransactionsCountTowardsViewingEntity = F, InProgressAndIsEffectiveForViewingEntity = F, ExistsStudentSectionGPAMethods = F, RequestedFromViewingEntityAndAllTransactionsCountTowardsViewingEntity = F, EarnedCreditsCompleted = F, FailedCreditsCompleted = F, EarnedCreditAttemptedCompleted = F, EarnedCreditsPossibleCompleted = F, EarnedCreditsAllEntered = F, FailedCreditsAllEntered = F, EarnedCreditAttemptedAllEntered = F, EarnedCreditsPossibleAllEntered = F, ConversionKey = F, StudentSectionTypeOverride = F, IsAlternativeCreditCourse = F, searchConditionsGroupType = "And", searchSortFieldNamesList = NULL, searchSortFieldNamesDescendingList = NULL, entityID = 1, schoolYearID = NULL, page = 1, pageSize = 100000, flatten = T, ...){

		params <- as.list(environment()) %>% append(list(...))

		searchFields <- params %>% keep(names(params) %>% str_sub(1,1) == names(params) %>% str_sub(1,1) %>% str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- "all", searchFields <- searchFields %>% keep(~.x) %>% names())

		listSkyObjects(module = "Scheduling", objectName = "StudentSection", schoolYearID = schoolYearID, searchFields = searchFields, page = page, pageSize = pageSize, SearchConditionsList = searchConditionsList, SearchConditionsGroupType = searchConditionsGroupType, SearchSortFieldNamesList = searchSortFieldNamesList, SearchSortFieldNamesDescendingList = searchSortFieldNamesDescendingList, entityId = entityID, flatten = flatten)
	}


	listSchedulingTeams <- function(searchConditionsList = NULL, SchedulingTeamID = F, Code = F, Description = F, EntityID = F, SchoolYearID = F, EntityGroupKey = F, SchedulingTeamIDClonedFrom = F, CodeDescription = F, SchedulingTeamIDClonedTo = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, searchConditionsGroupType = "And", searchSortFieldNamesList = NULL, searchSortFieldNamesDescendingList = NULL, entityID = 1, schoolYearID = NULL, page = 1, pageSize = 100000, flatten = T, ...){

		params <- as.list(environment()) %>% append(list(...))

		searchFields <- params %>% keep(names(params) %>% str_sub(1,1) == names(params) %>% str_sub(1,1) %>% str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- "all", searchFields <- searchFields %>% keep(~.x) %>% names())

		listSkyObjects(module = "Scheduling", objectName = "SchedulingTeam", schoolYearID = schoolYearID, searchFields = searchFields, page = page, pageSize = pageSize, SearchConditionsList = searchConditionsList, SearchConditionsGroupType = searchConditionsGroupType, SearchSortFieldNamesList = searchSortFieldNamesList, SearchSortFieldNamesDescendingList = searchSortFieldNamesDescendingList, entityId = entityID, flatten = flatten)
	}


	listSchedulingPeriods <- function(searchConditionsList = NULL, SchedulingPeriodID = F, DayRotationID = F, CodeNumber = F, EntityGroupKey = F, SchedulingPeriodIDClonedFrom = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, SchedulingPeriodIDClonedTo = F, DynamicRelationshipID = F, searchConditionsGroupType = "And", searchSortFieldNamesList = NULL, searchSortFieldNamesDescendingList = NULL, entityID = 1, schoolYearID = NULL, page = 1, pageSize = 100000, flatten = T, ...){

		params <- as.list(environment()) %>% append(list(...))

		searchFields <- params %>% keep(names(params) %>% str_sub(1,1) == names(params) %>% str_sub(1,1) %>% str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- "all", searchFields <- searchFields %>% keep(~.x) %>% names())

		listSkyObjects(module = "Scheduling", objectName = "SchedulingPeriod", schoolYearID = schoolYearID, searchFields = searchFields, page = page, pageSize = pageSize, SearchConditionsList = searchConditionsList, SearchConditionsGroupType = searchConditionsGroupType, SearchSortFieldNamesList = searchSortFieldNamesList, SearchSortFieldNamesDescendingList = searchSortFieldNamesDescendingList, entityId = entityID, flatten = flatten)
	}


	listSchedulingCategories <- function(searchConditionsList = NULL, SchedulingCategoryID = F, Code = F, Description = F, EntityID = F, SchoolYearID = F, EntityGroupKey = F, SchedulingCategoryIDClonedFrom = F, CodeDescription = F, SchedulingCategoryIDClonedTo = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, searchConditionsGroupType = "And", searchSortFieldNamesList = NULL, searchSortFieldNamesDescendingList = NULL, entityID = 1, schoolYearID = NULL, page = 1, pageSize = 100000, flatten = T, ...){

		params <- as.list(environment()) %>% append(list(...))

		searchFields <- params %>% keep(names(params) %>% str_sub(1,1) == names(params) %>% str_sub(1,1) %>% str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- "all", searchFields <- searchFields %>% keep(~.x) %>% names())

		listSkyObjects(module = "Scheduling", objectName = "SchedulingCategory", schoolYearID = schoolYearID, searchFields = searchFields, page = page, pageSize = pageSize, SearchConditionsList = searchConditionsList, SearchConditionsGroupType = searchConditionsGroupType, SearchSortFieldNamesList = searchSortFieldNamesList, SearchSortFieldNamesDescendingList = searchSortFieldNamesDescendingList, entityId = entityID, flatten = flatten)
	}


	listDisplayPeriodConflicts <- function(searchConditionsList = NULL, DisplayPeriodConflictID = F, DisplayPeriodIDBase = F, DisplayPeriodIDConflicting = F, EntityGroupKey = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, searchConditionsGroupType = "And", searchSortFieldNamesList = NULL, searchSortFieldNamesDescendingList = NULL, entityID = 1, schoolYearID = NULL, page = 1, pageSize = 100000, flatten = T, ...){

		params <- as.list(environment()) %>% append(list(...))

		searchFields <- params %>% keep(names(params) %>% str_sub(1,1) == names(params) %>% str_sub(1,1) %>% str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- "all", searchFields <- searchFields %>% keep(~.x) %>% names())

		listSkyObjects(module = "Scheduling", objectName = "DisplayPeriodConflict", schoolYearID = schoolYearID, searchFields = searchFields, page = page, pageSize = pageSize, SearchConditionsList = searchConditionsList, SearchConditionsGroupType = searchConditionsGroupType, SearchSortFieldNamesList = searchSortFieldNamesList, SearchSortFieldNamesDescendingList = searchSortFieldNamesDescendingList, entityId = entityID, flatten = flatten)
	}


	listDisplayPeriods <- function(searchConditionsList = NULL, DisplayPeriodID = F, DayRotationID = F, SortNumber = F, Code = F, Description = F, AttendancePeriodID = F, EntityGroupKey = F, DisplayPeriodIDClonedFrom = F, IsOutsideRegularSchoolDay = F, IsLunchPeriod = F, TeachingHourEquivalent = F, CodeDescription = F, CodeDescriptionDayRotation = F, DisplayPeriodStartTime = F, DisplayPeriodEndTime = F, DisplayPeriodIDClonedTo = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, DisplayPeriodCodeDayRotationCode = F, searchConditionsGroupType = "And", searchSortFieldNamesList = NULL, searchSortFieldNamesDescendingList = NULL, entityID = 1, schoolYearID = NULL, page = 1, pageSize = 100000, flatten = T, ...){

		params <- as.list(environment()) %>% append(list(...))

		searchFields <- params %>% keep(names(params) %>% str_sub(1,1) == names(params) %>% str_sub(1,1) %>% str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- "all", searchFields <- searchFields %>% keep(~.x) %>% names())

		listSkyObjects(module = "Scheduling", objectName = "DisplayPeriod", schoolYearID = schoolYearID, searchFields = searchFields, page = page, pageSize = pageSize, SearchConditionsList = searchConditionsList, SearchConditionsGroupType = searchConditionsGroupType, SearchSortFieldNamesList = searchSortFieldNamesList, SearchSortFieldNamesDescendingList = searchSortFieldNamesDescendingList, entityId = entityID, flatten = flatten)
	}


	listDayRotations <- function(searchConditionsList = NULL, DayRotationID = F, SortNumber = F, Code = F, EntityID = F, SchoolYearID = F, EntityGroupKey = F, ConsecutiveTeachingHourLimit = F, MaximumTeachingHourLimit = F, DayRotationIDClonedFrom = F, DayRotationIDClonedTo = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, searchConditionsGroupType = "And", searchSortFieldNamesList = NULL, searchSortFieldNamesDescendingList = NULL, entityID = 1, schoolYearID = NULL, page = 1, pageSize = 100000, flatten = T, ...){

		params <- as.list(environment()) %>% append(list(...))

		searchFields <- params %>% keep(names(params) %>% str_sub(1,1) == names(params) %>% str_sub(1,1) %>% str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- "all", searchFields <- searchFields %>% keep(~.x) %>% names())

		listSkyObjects(module = "Scheduling", objectName = "DayRotation", schoolYearID = schoolYearID, searchFields = searchFields, page = page, pageSize = pageSize, SearchConditionsList = searchConditionsList, SearchConditionsGroupType = searchConditionsGroupType, SearchSortFieldNamesList = searchSortFieldNamesList, SearchSortFieldNamesDescendingList = searchSortFieldNamesDescendingList, entityId = entityID, flatten = flatten)
	}


	listCourseSectionLengthExcludes <- function(searchConditionsList = NULL, CourseSectionLengthExcludeID = F, CourseID = F, SectionLengthID = F, DistributionPercent = F, EntityGroupKey = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, searchConditionsGroupType = "And", searchSortFieldNamesList = NULL, searchSortFieldNamesDescendingList = NULL, entityID = 1, schoolYearID = NULL, page = 1, pageSize = 100000, flatten = T, ...){

		params <- as.list(environment()) %>% append(list(...))

		searchFields <- params %>% keep(names(params) %>% str_sub(1,1) == names(params) %>% str_sub(1,1) %>% str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- "all", searchFields <- searchFields %>% keep(~.x) %>% names())

		listSkyObjects(module = "Scheduling", objectName = "CourseSectionLengthExclude", schoolYearID = schoolYearID, searchFields = searchFields, page = page, pageSize = pageSize, SearchConditionsList = searchConditionsList, SearchConditionsGroupType = searchConditionsGroupType, SearchSortFieldNamesList = searchSortFieldNamesList, SearchSortFieldNamesDescendingList = searchSortFieldNamesDescendingList, entityId = entityID, flatten = flatten)
	}


	listTempCourseEntityOfferedToSections <- function(searchConditionsList = NULL, TempCourseEntityOfferedToSectionID = F, CourseEntityOfferedToSectionID = F, CourseEntityOfferedToID = F, EntityOfferedToCode = F, EntityOfferedToName = F, SectionID = F, SectionCode = F, SectionIsActive = F, SectionMaximumStudentCount = F, SectionReservedSeatCount = F, SectionSectionLengthCode = F, IsActiveOverride = F, MaximumStudentCount = F, SeatsAvailable = F, OriginalMaximumStudentCount = F, CourseEntityOfferedToSectionRecordExists = F, RowIsSelected = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, CourseID = F, EntityIDOfferedTo = F, SchoolYearID = F, searchConditionsGroupType = "And", searchSortFieldNamesList = NULL, searchSortFieldNamesDescendingList = NULL, entityID = 1, schoolYearID = NULL, page = 1, pageSize = 100000, flatten = T, ...){

		params <- as.list(environment()) %>% append(list(...))

		searchFields <- params %>% keep(names(params) %>% str_sub(1,1) == names(params) %>% str_sub(1,1) %>% str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- "all", searchFields <- searchFields %>% keep(~.x) %>% names())

		listSkyObjects(module = "Scheduling", objectName = "TempCourseEntityOfferedToSection", schoolYearID = schoolYearID, searchFields = searchFields, page = page, pageSize = pageSize, SearchConditionsList = searchConditionsList, SearchConditionsGroupType = searchConditionsGroupType, SearchSortFieldNamesList = searchSortFieldNamesList, SearchSortFieldNamesDescendingList = searchSortFieldNamesDescendingList, entityId = entityID, flatten = flatten)
	}


	listMeetSummaryDetails <- function(searchConditionsList = NULL, MeetSummaryDetailID = F, DisplayPeriodID = F, MeetSummaryID = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, IsPrimary = F, searchConditionsGroupType = "And", searchSortFieldNamesList = NULL, searchSortFieldNamesDescendingList = NULL, entityID = 1, schoolYearID = NULL, page = 1, pageSize = 100000, flatten = T, ...){

		params <- as.list(environment()) %>% append(list(...))

		searchFields <- params %>% keep(names(params) %>% str_sub(1,1) == names(params) %>% str_sub(1,1) %>% str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- "all", searchFields <- searchFields %>% keep(~.x) %>% names())

		listSkyObjects(module = "Scheduling", objectName = "MeetSummaryDetail", schoolYearID = schoolYearID, searchFields = searchFields, page = page, pageSize = pageSize, SearchConditionsList = searchConditionsList, SearchConditionsGroupType = searchConditionsGroupType, SearchSortFieldNamesList = searchSortFieldNamesList, SearchSortFieldNamesDescendingList = searchSortFieldNamesDescendingList, entityId = entityID, flatten = flatten)
	}


	listTempSubstituteAssignments <- function(searchConditionsList = NULL, TempSubstituteAssignmentID = F, SubstituteStaffID = F, SourceStaffID = F, MeetID = F, CourseCodeSectionCode = F, Period = F, Date = F, HasGradebookAccess = F, HasAttendanceAccess = F, DisplayPeriodSortNumber = F, DisplayPeriodID = F, StaffMeetID = F, EntityID = F, SchoolYearID = F, Conflicts = F, SectionAlreadyCovered = F, StaffName = F, HasConflicts = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, SectionID = F, IsLongTermSubstitute = F, searchConditionsGroupType = "And", searchSortFieldNamesList = NULL, searchSortFieldNamesDescendingList = NULL, entityID = 1, schoolYearID = NULL, page = 1, pageSize = 100000, flatten = T, ...){

		params <- as.list(environment()) %>% append(list(...))

		searchFields <- params %>% keep(names(params) %>% str_sub(1,1) == names(params) %>% str_sub(1,1) %>% str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- "all", searchFields <- searchFields %>% keep(~.x) %>% names())

		listSkyObjects(module = "Scheduling", objectName = "TempSubstituteAssignment", schoolYearID = schoolYearID, searchFields = searchFields, page = page, pageSize = pageSize, SearchConditionsList = searchConditionsList, SearchConditionsGroupType = searchConditionsGroupType, SearchSortFieldNamesList = searchSortFieldNamesList, SearchSortFieldNamesDescendingList = searchSortFieldNamesDescendingList, entityId = entityID, flatten = flatten)
	}


	listDisplayPeriodRotationDisplayPeriods <- function(searchConditionsList = NULL, DisplayPeriodRotationDisplayPeriodID = F, DisplayPeriodRotationID = F, DisplayPeriodID = F, IsPrimary = F, HideMeetDisplayPeriod = F, DisplayPeriodRotationDisplayPeriodIDClonedFrom = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, searchConditionsGroupType = "And", searchSortFieldNamesList = NULL, searchSortFieldNamesDescendingList = NULL, entityID = 1, schoolYearID = NULL, page = 1, pageSize = 100000, flatten = T, ...){

		params <- as.list(environment()) %>% append(list(...))

		searchFields <- params %>% keep(names(params) %>% str_sub(1,1) == names(params) %>% str_sub(1,1) %>% str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- "all", searchFields <- searchFields %>% keep(~.x) %>% names())

		listSkyObjects(module = "Scheduling", objectName = "DisplayPeriodRotationDisplayPeriod", schoolYearID = schoolYearID, searchFields = searchFields, page = page, pageSize = pageSize, SearchConditionsList = searchConditionsList, SearchConditionsGroupType = searchConditionsGroupType, SearchSortFieldNamesList = searchSortFieldNamesList, SearchSortFieldNamesDescendingList = searchSortFieldNamesDescendingList, entityId = entityID, flatten = flatten)
	}


	listDisplayPeriodRotations <- function(searchConditionsList = NULL, DisplayPeriodRotationID = F, EntityID = F, SchoolYearID = F, Code = F, Description = F, DisplayPeriodSummary = F, DisplayPeriodRotationIDClonedFrom = F, CodeDescription = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, DisplayPeriodRotationIDClonedTo = F, searchConditionsGroupType = "And", searchSortFieldNamesList = NULL, searchSortFieldNamesDescendingList = NULL, entityID = 1, schoolYearID = NULL, page = 1, pageSize = 100000, flatten = T, ...){

		params <- as.list(environment()) %>% append(list(...))

		searchFields <- params %>% keep(names(params) %>% str_sub(1,1) == names(params) %>% str_sub(1,1) %>% str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- "all", searchFields <- searchFields %>% keep(~.x) %>% names())

		listSkyObjects(module = "Scheduling", objectName = "DisplayPeriodRotation", schoolYearID = schoolYearID, searchFields = searchFields, page = page, pageSize = pageSize, SearchConditionsList = searchConditionsList, SearchConditionsGroupType = searchConditionsGroupType, SearchSortFieldNamesList = searchSortFieldNamesList, SearchSortFieldNamesDescendingList = searchSortFieldNamesDescendingList, entityId = entityID, flatten = flatten)
	}


	listAvailabilityFilterCourseStudentStudents <- function(searchConditionsList = NULL, AvailabilityFilterCourseStudentStudentID = F, AvailabilityFilterCourseStudentID = F, StudentID = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, ArenaSchedulingStatus = F, searchConditionsGroupType = "And", searchSortFieldNamesList = NULL, searchSortFieldNamesDescendingList = NULL, entityID = 1, schoolYearID = NULL, page = 1, pageSize = 100000, flatten = T, ...){

		params <- as.list(environment()) %>% append(list(...))

		searchFields <- params %>% keep(names(params) %>% str_sub(1,1) == names(params) %>% str_sub(1,1) %>% str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- "all", searchFields <- searchFields %>% keep(~.x) %>% names())

		listSkyObjects(module = "Scheduling", objectName = "AvailabilityFilterCourseStudentStudent", schoolYearID = schoolYearID, searchFields = searchFields, page = page, pageSize = pageSize, SearchConditionsList = searchConditionsList, SearchConditionsGroupType = searchConditionsGroupType, SearchSortFieldNamesList = searchSortFieldNamesList, SearchSortFieldNamesDescendingList = searchSortFieldNamesDescendingList, entityId = entityID, flatten = flatten)
	}


	listAvailabilityFilterCourseStudentCourses <- function(searchConditionsList = NULL, AvailabilityFilterCourseStudentCourseID = F, AvailabilityFilterCourseStudentID = F, CourseID = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, searchConditionsGroupType = "And", searchSortFieldNamesList = NULL, searchSortFieldNamesDescendingList = NULL, entityID = 1, schoolYearID = NULL, page = 1, pageSize = 100000, flatten = T, ...){

		params <- as.list(environment()) %>% append(list(...))

		searchFields <- params %>% keep(names(params) %>% str_sub(1,1) == names(params) %>% str_sub(1,1) %>% str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- "all", searchFields <- searchFields %>% keep(~.x) %>% names())

		listSkyObjects(module = "Scheduling", objectName = "AvailabilityFilterCourseStudentCourse", schoolYearID = schoolYearID, searchFields = searchFields, page = page, pageSize = pageSize, SearchConditionsList = searchConditionsList, SearchConditionsGroupType = searchConditionsGroupType, SearchSortFieldNamesList = searchSortFieldNamesList, SearchSortFieldNamesDescendingList = searchSortFieldNamesDescendingList, entityId = entityID, flatten = flatten)
	}


	listTempDayRotations <- function(searchConditionsList = NULL, TempDayRotationID = F, DayRotationID = F, Code = F, ObjectIsDirty = F, ObjectName = F, Note = F, RecordsUpdated = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, searchConditionsGroupType = "And", searchSortFieldNamesList = NULL, searchSortFieldNamesDescendingList = NULL, entityID = 1, schoolYearID = NULL, page = 1, pageSize = 100000, flatten = T, ...){

		params <- as.list(environment()) %>% append(list(...))

		searchFields <- params %>% keep(names(params) %>% str_sub(1,1) == names(params) %>% str_sub(1,1) %>% str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- "all", searchFields <- searchFields %>% keep(~.x) %>% names())

		listSkyObjects(module = "Scheduling", objectName = "TempDayRotation", schoolYearID = schoolYearID, searchFields = searchFields, page = page, pageSize = pageSize, SearchConditionsList = searchConditionsList, SearchConditionsGroupType = searchConditionsGroupType, SearchSortFieldNamesList = searchSortFieldNamesList, SearchSortFieldNamesDescendingList = searchSortFieldNamesDescendingList, entityId = entityID, flatten = flatten)
	}


	listTempFailedDayRotations <- function(searchConditionsList = NULL, TempFailedDayRotationID = F, TempDayRotationID = F, DayRotationID = F, Code = F, ObjectIsDirty = F, ObjectName = F, Note = F, RecordsUpdated = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, searchConditionsGroupType = "And", searchSortFieldNamesList = NULL, searchSortFieldNamesDescendingList = NULL, entityID = 1, schoolYearID = NULL, page = 1, pageSize = 100000, flatten = T, ...){

		params <- as.list(environment()) %>% append(list(...))

		searchFields <- params %>% keep(names(params) %>% str_sub(1,1) == names(params) %>% str_sub(1,1) %>% str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- "all", searchFields <- searchFields %>% keep(~.x) %>% names())

		listSkyObjects(module = "Scheduling", objectName = "TempFailedDayRotation", schoolYearID = schoolYearID, searchFields = searchFields, page = page, pageSize = pageSize, SearchConditionsList = searchConditionsList, SearchConditionsGroupType = searchConditionsGroupType, SearchSortFieldNamesList = searchSortFieldNamesList, SearchSortFieldNamesDescendingList = searchSortFieldNamesDescendingList, entityId = entityID, flatten = flatten)
	}


	listSchedulingConfigEntityYears <- function(searchConditionsList = NULL, ConfigEntityYearID = F, EntityID = F, SchoolYearID = F, ConfigEntityYearIDClonedFrom = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, EnableValidationOfRoomCapacityDuringStudentSectionEnrollment = F, EnableArenaSchedulingConfirmationScreen = F, ArenaSchedulingConfirmationScreenMessage = F, searchConditionsGroupType = "And", searchSortFieldNamesList = NULL, searchSortFieldNamesDescendingList = NULL, entityID = 1, schoolYearID = NULL, page = 1, pageSize = 100000, flatten = T, ...){

		params <- as.list(environment()) %>% append(list(...))

		searchFields <- params %>% keep(names(params) %>% str_sub(1,1) == names(params) %>% str_sub(1,1) %>% str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- "all", searchFields <- searchFields %>% keep(~.x) %>% names())

		listSkyObjects(module = "Scheduling", objectName = "ConfigEntityYear", schoolYearID = schoolYearID, searchFields = searchFields, page = page, pageSize = pageSize, SearchConditionsList = searchConditionsList, SearchConditionsGroupType = searchConditionsGroupType, SearchSortFieldNamesList = searchSortFieldNamesList, SearchSortFieldNamesDescendingList = searchSortFieldNamesDescendingList, entityId = entityID, flatten = flatten)
	}


	listSchedulingBoardFilters <- function(searchConditionsList = NULL, SchedulingBoardFilterID = F, Description = F, DisplayOrder = F, Type = F, SkywardID = F, SkywardHash = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, searchConditionsGroupType = "And", searchSortFieldNamesList = NULL, searchSortFieldNamesDescendingList = NULL, entityID = 1, schoolYearID = NULL, page = 1, pageSize = 100000, flatten = T, ...){

		params <- as.list(environment()) %>% append(list(...))

		searchFields <- params %>% keep(names(params) %>% str_sub(1,1) == names(params) %>% str_sub(1,1) %>% str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- "all", searchFields <- searchFields %>% keep(~.x) %>% names())

		listSkyObjects(module = "Scheduling", objectName = "SchedulingBoardFilter", schoolYearID = schoolYearID, searchFields = searchFields, page = page, pageSize = pageSize, SearchConditionsList = searchConditionsList, SearchConditionsGroupType = searchConditionsGroupType, SearchSortFieldNamesList = searchSortFieldNamesList, SearchSortFieldNamesDescendingList = searchSortFieldNamesDescendingList, entityId = entityID, flatten = flatten)
	}


	listMassPrintStudentScheduleRunHistories <- function(searchConditionsList = NULL, MassPrintStudentScheduleRunHistoryID = F, EntityID = F, SchoolYearID = F, RunDescription = F, MediaID = F, WorkflowInstanceID = F, RequestIdentifier = F, SendMessageOnComplete = F, StudentSelectType = F, ParameterData = F, ParameterDescription = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, Comment = F, PrintLockerInfo = F, SortType = F, searchConditionsGroupType = "And", searchSortFieldNamesList = NULL, searchSortFieldNamesDescendingList = NULL, entityID = 1, schoolYearID = NULL, page = 1, pageSize = 100000, flatten = T, ...){

		params <- as.list(environment()) %>% append(list(...))

		searchFields <- params %>% keep(names(params) %>% str_sub(1,1) == names(params) %>% str_sub(1,1) %>% str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- "all", searchFields <- searchFields %>% keep(~.x) %>% names())

		listSkyObjects(module = "Scheduling", objectName = "MassPrintStudentScheduleRunHistory", schoolYearID = schoolYearID, searchFields = searchFields, page = page, pageSize = pageSize, SearchConditionsList = searchConditionsList, SearchConditionsGroupType = searchConditionsGroupType, SearchSortFieldNamesList = searchSortFieldNamesList, SearchSortFieldNamesDescendingList = searchSortFieldNamesDescendingList, entityId = entityID, flatten = flatten)
	}


	listSchedulingTempExceptions <- function(searchConditionsList = NULL, TempExceptionID = F, FailedRecordPrimaryKey = F, ErrorFieldName = F, ErrorDetail = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, searchConditionsGroupType = "And", searchSortFieldNamesList = NULL, searchSortFieldNamesDescendingList = NULL, entityID = 1, schoolYearID = NULL, page = 1, pageSize = 100000, flatten = T, ...){

		params <- as.list(environment()) %>% append(list(...))

		searchFields <- params %>% keep(names(params) %>% str_sub(1,1) == names(params) %>% str_sub(1,1) %>% str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- "all", searchFields <- searchFields %>% keep(~.x) %>% names())

		listSkyObjects(module = "Scheduling", objectName = "TempException", schoolYearID = schoolYearID, searchFields = searchFields, page = page, pageSize = pageSize, SearchConditionsList = searchConditionsList, SearchConditionsGroupType = searchConditionsGroupType, SearchSortFieldNamesList = searchSortFieldNamesList, SearchSortFieldNamesDescendingList = searchSortFieldNamesDescendingList, entityId = entityID, flatten = flatten)
	}


	listTempMeets <- function(searchConditionsList = NULL, TempMeetID = F, MeetID = F, CourseCode = F, SectionCode = F, CourseDescription = F, StartDate = F, EndDate = F, PrimaryDisplayPeriod = F, PrimaryDays = F, PrimaryStaffFullNameFML = F, RoomNumberDescription = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, SectionID = F, SectionLengthCode = F, NewSectionLengthCode = F, NewStartDate = F, NewEndDate = F, searchConditionsGroupType = "And", searchSortFieldNamesList = NULL, searchSortFieldNamesDescendingList = NULL, entityID = 1, schoolYearID = NULL, page = 1, pageSize = 100000, flatten = T, ...){

		params <- as.list(environment()) %>% append(list(...))

		searchFields <- params %>% keep(names(params) %>% str_sub(1,1) == names(params) %>% str_sub(1,1) %>% str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- "all", searchFields <- searchFields %>% keep(~.x) %>% names())

		listSkyObjects(module = "Scheduling", objectName = "TempMeet", schoolYearID = schoolYearID, searchFields = searchFields, page = page, pageSize = pageSize, SearchConditionsList = searchConditionsList, SearchConditionsGroupType = searchConditionsGroupType, SearchSortFieldNamesList = searchSortFieldNamesList, SearchSortFieldNamesDescendingList = searchSortFieldNamesDescendingList, entityId = entityID, flatten = flatten)
	}


	listSectionSchedulerProposedMeetDisplayPeriods <- function(searchConditionsList = NULL, SectionSchedulerProposedMeetDisplayPeriodID = F, SectionSchedulerProposedMeetID = F, DisplayPeriodID = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, searchConditionsGroupType = "And", searchSortFieldNamesList = NULL, searchSortFieldNamesDescendingList = NULL, entityID = 1, schoolYearID = NULL, page = 1, pageSize = 100000, flatten = T, ...){

		params <- as.list(environment()) %>% append(list(...))

		searchFields <- params %>% keep(names(params) %>% str_sub(1,1) == names(params) %>% str_sub(1,1) %>% str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- "all", searchFields <- searchFields %>% keep(~.x) %>% names())

		listSkyObjects(module = "Scheduling", objectName = "SectionSchedulerProposedMeetDisplayPeriod", schoolYearID = schoolYearID, searchFields = searchFields, page = page, pageSize = pageSize, SearchConditionsList = searchConditionsList, SearchConditionsGroupType = searchConditionsGroupType, SearchSortFieldNamesList = searchSortFieldNamesList, SearchSortFieldNamesDescendingList = searchSortFieldNamesDescendingList, entityId = entityID, flatten = flatten)
	}


	listSectionSchedulerProposedMeets <- function(searchConditionsList = NULL, SectionSchedulerProposedMeetID = F, SectionSchedulerRunAnalysisID = F, DisplayPeriodRotationID = F, MeetDisplayPeriodSummary = F, NumberOfActualConflicts = F, NumberOfEstimatedConflicts = F, PrimaryStaffMeetFullNameLFM = F, Rank = F, RankValue = F, RoomID = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, TotalActualConflictsPointsEarned = F, TotalDisplayPeriodPointsEarned = F, TotalEstimatedConflictsPointsEarned = F, TotalRoomPointsEarned = F, TotalStaffPointsEarned = F, SumTotalOfMaximumStudentCountForScheduledSections = F, SumTotalOfOptimalStudentCountForScheduledSections = F, TotalSumOfMaximumStudentCountForScheduledSectionsPointsEarned = F, TotalSumOfOptimalStudentCountForScheduledSectionsPointsEarned = F, NumberOfProposedMeetCourseConflicts = F, NumberOfProposedMeetRoomConflicts = F, NumberOfProposedMeetStaffConflicts = F, EndDate = F, SectionLengthID = F, StartDate = F, NumberOfProposedMeetRoomAndStaffConflicts = F, searchConditionsGroupType = "And", searchSortFieldNamesList = NULL, searchSortFieldNamesDescendingList = NULL, entityID = 1, schoolYearID = NULL, page = 1, pageSize = 100000, flatten = T, ...){

		params <- as.list(environment()) %>% append(list(...))

		searchFields <- params %>% keep(names(params) %>% str_sub(1,1) == names(params) %>% str_sub(1,1) %>% str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- "all", searchFields <- searchFields %>% keep(~.x) %>% names())

		listSkyObjects(module = "Scheduling", objectName = "SectionSchedulerProposedMeet", schoolYearID = schoolYearID, searchFields = searchFields, page = page, pageSize = pageSize, SearchConditionsList = searchConditionsList, SearchConditionsGroupType = searchConditionsGroupType, SearchSortFieldNamesList = searchSortFieldNamesList, SearchSortFieldNamesDescendingList = searchSortFieldNamesDescendingList, entityId = entityID, flatten = flatten)
	}


	listSectionSchedulerRunAnalyses <- function(searchConditionsList = NULL, SectionSchedulerRunAnalysisID = F, EntityID = F, SchoolYearID = F, MeetID = F, PageStateID = F, UserIDPerformer = F, AnalysisMethod = F, StartTimeAnalysis = F, EndTimeAnalysis = F, AcceptedMeetReverted = F, SectionSchedulerProposedMeetIDAccepted = F, AnalysisDuration = F, CountOfSectionSchedulerProposedMeetRecords = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, TotalActualConflictsPointsPossible = F, TotalDisplayPeriodPointsPossible = F, TotalEstimatedConflictsPointsPossible = F, TotalRoomPointsPossible = F, TotalStaffPointsPossible = F, TotalSumOfMaximumStudentCountForScheduledSectionsPointsPossible = F, TotalSumOfOptimalStudentCountForScheduledSectionsPointsPossible = F, AnalyzeMeetDisplayPeriods = F, AnalyzeRoom = F, AnalyzeSectionLength = F, AnalyzeStaffMeets = F, ExcludeMeetsPreviouslyScheduled = F, RunReason = F, AnalyzeDayRotations = F, searchConditionsGroupType = "And", searchSortFieldNamesList = NULL, searchSortFieldNamesDescendingList = NULL, entityID = 1, schoolYearID = NULL, page = 1, pageSize = 100000, flatten = T, ...){

		params <- as.list(environment()) %>% append(list(...))

		searchFields <- params %>% keep(names(params) %>% str_sub(1,1) == names(params) %>% str_sub(1,1) %>% str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- "all", searchFields <- searchFields %>% keep(~.x) %>% names())

		listSkyObjects(module = "Scheduling", objectName = "SectionSchedulerRunAnalysis", schoolYearID = schoolYearID, searchFields = searchFields, page = page, pageSize = pageSize, SearchConditionsList = searchConditionsList, SearchConditionsGroupType = searchConditionsGroupType, SearchSortFieldNamesList = searchSortFieldNamesList, SearchSortFieldNamesDescendingList = searchSortFieldNamesDescendingList, entityId = entityID, flatten = flatten)
	}


	listSectionSchedulerRoomTypeForCourses <- function(searchConditionsList = NULL, SectionSchedulerRoomTypeForCourseID = F, CourseID = F, RoomTypeID = F, SectionSchedulerRoomTypeForCourseIDClonedFrom = F, SectionSchedulerRoomTypeForCourseIDClonedTo = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, searchConditionsGroupType = "And", searchSortFieldNamesList = NULL, searchSortFieldNamesDescendingList = NULL, entityID = 1, schoolYearID = NULL, page = 1, pageSize = 100000, flatten = T, ...){

		params <- as.list(environment()) %>% append(list(...))

		searchFields <- params %>% keep(names(params) %>% str_sub(1,1) == names(params) %>% str_sub(1,1) %>% str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- "all", searchFields <- searchFields %>% keep(~.x) %>% names())

		listSkyObjects(module = "Scheduling", objectName = "SectionSchedulerRoomTypeForCourse", schoolYearID = schoolYearID, searchFields = searchFields, page = page, pageSize = pageSize, SearchConditionsList = searchConditionsList, SearchConditionsGroupType = searchConditionsGroupType, SearchSortFieldNamesList = searchSortFieldNamesList, SearchSortFieldNamesDescendingList = searchSortFieldNamesDescendingList, entityId = entityID, flatten = flatten)
	}


	listSectionSchedulerStaffForCourses <- function(searchConditionsList = NULL, SectionSchedulerStaffForCourseID = F, CourseID = F, StaffID = F, SectionSchedulerStaffForCourseIDClonedFrom = F, SectionSchedulerStaffForCourseIDClonedTo = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, searchConditionsGroupType = "And", searchSortFieldNamesList = NULL, searchSortFieldNamesDescendingList = NULL, entityID = 1, schoolYearID = NULL, page = 1, pageSize = 100000, flatten = T, ...){

		params <- as.list(environment()) %>% append(list(...))

		searchFields <- params %>% keep(names(params) %>% str_sub(1,1) == names(params) %>% str_sub(1,1) %>% str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- "all", searchFields <- searchFields %>% keep(~.x) %>% names())

		listSkyObjects(module = "Scheduling", objectName = "SectionSchedulerStaffForCourse", schoolYearID = schoolYearID, searchFields = searchFields, page = page, pageSize = pageSize, SearchConditionsList = searchConditionsList, SearchConditionsGroupType = searchConditionsGroupType, SearchSortFieldNamesList = searchSortFieldNamesList, SearchSortFieldNamesDescendingList = searchSortFieldNamesDescendingList, entityId = entityID, flatten = flatten)
	}


	listSectionSchedulerProposedMeetConflicts <- function(searchConditionsList = NULL, SectionSchedulerProposedMeetConflictID = F, SectionSchedulerProposedMeetID = F, ConflictType = F, Description = F, Name = F, Severity = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, CourseID = F, NumberOfActualConflicts = F, NumberOfCommonRequests = F, NumberOfEstimatedConflicts = F, searchConditionsGroupType = "And", searchSortFieldNamesList = NULL, searchSortFieldNamesDescendingList = NULL, entityID = 1, schoolYearID = NULL, page = 1, pageSize = 100000, flatten = T, ...){

		params <- as.list(environment()) %>% append(list(...))

		searchFields <- params %>% keep(names(params) %>% str_sub(1,1) == names(params) %>% str_sub(1,1) %>% str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- "all", searchFields <- searchFields %>% keep(~.x) %>% names())

		listSkyObjects(module = "Scheduling", objectName = "SectionSchedulerProposedMeetConflict", schoolYearID = schoolYearID, searchFields = searchFields, page = page, pageSize = pageSize, SearchConditionsList = searchConditionsList, SearchConditionsGroupType = searchConditionsGroupType, SearchSortFieldNamesList = searchSortFieldNamesList, SearchSortFieldNamesDescendingList = searchSortFieldNamesDescendingList, entityId = entityID, flatten = flatten)
	}


	listTempFailedStudentCourseRequestToReactivateDetails <- function(searchConditionsList = NULL, TempFailedStudentCourseRequestToReactivateDetailID = F, TempRecordToReactivatePrimaryKeyValue = F, Note = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, searchConditionsGroupType = "And", searchSortFieldNamesList = NULL, searchSortFieldNamesDescendingList = NULL, entityID = 1, schoolYearID = NULL, page = 1, pageSize = 100000, flatten = T, ...){

		params <- as.list(environment()) %>% append(list(...))

		searchFields <- params %>% keep(names(params) %>% str_sub(1,1) == names(params) %>% str_sub(1,1) %>% str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- "all", searchFields <- searchFields %>% keep(~.x) %>% names())

		listSkyObjects(module = "Scheduling", objectName = "TempFailedStudentCourseRequestToReactivateDetail", schoolYearID = schoolYearID, searchFields = searchFields, page = page, pageSize = pageSize, SearchConditionsList = searchConditionsList, SearchConditionsGroupType = searchConditionsGroupType, SearchSortFieldNamesList = searchSortFieldNamesList, SearchSortFieldNamesDescendingList = searchSortFieldNamesDescendingList, entityId = entityID, flatten = flatten)
	}


	listTempFailedStudentCourseRequestToReactivates <- function(searchConditionsList = NULL, TempFailedStudentCourseRequestToReactivateID = F, TempRecordToReactivatePrimaryKeyValue = F, Note = F, StudentID = F, DateFrom = F, DateTo = F, StudentCourseRequestID = F, IsAlternate = F, AlternateRank = F, StudentCourseRequestIDAlternateFor = F, RequestStatus = F, RequestSource = F, SchedulingMethod = F, CourseID = F, EntityIDRequestedFrom = F, CourseEntityOfferedToID = F, CourseCode = F, CourseDescription = F, CourseCodeDescription = F, EarnedCredits = F, CourseGradeLevelSummary = F, AuditRecordIsSchedulable = F, SectionID = F, SectionLengthID = F, ExcludeFromReportCardsAndTranscripts = F, IsTransferCourse = F, UseEarnedCreditOverride = F, EarnedCreditOverride = F, UseEarnedCreditTotalOverride = F, TotalEarnedCreditOverride = F, TotalFailedCreditOverride = F, GradeReferenceID = F, TransferCourseName = F, SectionCode = F, CurrentEnrollment = F, MaximumStudentCount = F, Period = F, Days = F, StaffFullNameFML = F, ExcludeFromStudentSectionLink = F, EntityIDCourse = F, SchoolYearIDCourse = F, StartDate = F, EndDate = F, SectionLengthSubsetID = F, EarlyExitReasonID = F, EarlyExitReasonCodeDescription = F, NameIDRequestedBy = F, NameRequestedByLFM = F, EntityIDCountsAs = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, PreventReactivateCheckboxFromBeingRendered = F, AuditRecordIsRequestable = F, StudentSectionID = F, StudentSectionTransactionID = F, SectionLengthSubsetCode = F, SectionLengthSubsetDescription = F, searchConditionsGroupType = "And", searchSortFieldNamesList = NULL, searchSortFieldNamesDescendingList = NULL, entityID = 1, schoolYearID = NULL, page = 1, pageSize = 100000, flatten = T, ...){

		params <- as.list(environment()) %>% append(list(...))

		searchFields <- params %>% keep(names(params) %>% str_sub(1,1) == names(params) %>% str_sub(1,1) %>% str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- "all", searchFields <- searchFields %>% keep(~.x) %>% names())

		listSkyObjects(module = "Scheduling", objectName = "TempFailedStudentCourseRequestToReactivate", schoolYearID = schoolYearID, searchFields = searchFields, page = page, pageSize = pageSize, SearchConditionsList = searchConditionsList, SearchConditionsGroupType = searchConditionsGroupType, SearchSortFieldNamesList = searchSortFieldNamesList, SearchSortFieldNamesDescendingList = searchSortFieldNamesDescendingList, entityId = entityID, flatten = flatten)
	}


	listTempStudentCourseRequestToReactivateMNS <- function(searchConditionsList = NULL, TempStudentCourseRequestToReactivateMNID = F, IsTSAProficient = F, StudentID = F, DateFrom = F, DateTo = F, StudentCourseRequestID = F, IsAlternate = F, AlternateRank = F, StudentCourseRequestIDAlternateFor = F, RequestStatus = F, RequestSource = F, SchedulingMethod = F, CourseID = F, EntityIDRequestedFrom = F, CourseEntityOfferedToID = F, CourseCode = F, CourseDescription = F, CourseCodeDescription = F, EarnedCredits = F, CourseGradeLevelSummary = F, AuditRecordIsSchedulable = F, SectionID = F, SectionLengthID = F, ExcludeFromReportCardsAndTranscripts = F, IsTransferCourse = F, UseEarnedCreditOverride = F, EarnedCreditOverride = F, UseEarnedCreditTotalOverride = F, TotalEarnedCreditOverride = F, TotalFailedCreditOverride = F, GradeReferenceID = F, TransferCourseName = F, SectionCode = F, CurrentEnrollment = F, MaximumStudentCount = F, Period = F, Days = F, StaffFullNameFML = F, ExcludeFromStudentSectionLink = F, EntityIDCourse = F, SchoolYearIDCourse = F, StartDate = F, EndDate = F, SectionLengthSubsetID = F, EarlyExitReasonID = F, EarlyExitReasonCodeDescription = F, NameIDRequestedBy = F, NameRequestedByLFM = F, EntityIDCountsAs = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, PreventReactivateCheckboxFromBeingRendered = F, AuditRecordIsRequestable = F, StudentSectionID = F, StudentSectionTransactionID = F, SectionLengthSubsetCode = F, SectionLengthSubsetDescription = F, searchConditionsGroupType = "And", searchSortFieldNamesList = NULL, searchSortFieldNamesDescendingList = NULL, entityID = 1, schoolYearID = NULL, page = 1, pageSize = 100000, flatten = T, ...){

		params <- as.list(environment()) %>% append(list(...))

		searchFields <- params %>% keep(names(params) %>% str_sub(1,1) == names(params) %>% str_sub(1,1) %>% str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- "all", searchFields <- searchFields %>% keep(~.x) %>% names())

		listSkyObjects(module = "Scheduling", objectName = "TempStudentCourseRequestToReactivateMN", schoolYearID = schoolYearID, searchFields = searchFields, page = page, pageSize = pageSize, SearchConditionsList = searchConditionsList, SearchConditionsGroupType = searchConditionsGroupType, SearchSortFieldNamesList = searchSortFieldNamesList, SearchSortFieldNamesDescendingList = searchSortFieldNamesDescendingList, entityId = entityID, flatten = flatten)
	}


	listSectionSchedulerPatternExcludedForMeetRequirements <- function(searchConditionsList = NULL, SectionSchedulerPatternExcludedForMeetRequirementID = F, MeetRequirementID = F, SectionSchedulerPatternID = F, SectionSchedulerPatternExcludedForMeetRequirementIDClonedFrom = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, searchConditionsGroupType = "And", searchSortFieldNamesList = NULL, searchSortFieldNamesDescendingList = NULL, entityID = 1, schoolYearID = NULL, page = 1, pageSize = 100000, flatten = T, ...){

		params <- as.list(environment()) %>% append(list(...))

		searchFields <- params %>% keep(names(params) %>% str_sub(1,1) == names(params) %>% str_sub(1,1) %>% str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- "all", searchFields <- searchFields %>% keep(~.x) %>% names())

		listSkyObjects(module = "Scheduling", objectName = "SectionSchedulerPatternExcludedForMeetRequirement", schoolYearID = schoolYearID, searchFields = searchFields, page = page, pageSize = pageSize, SearchConditionsList = searchConditionsList, SearchConditionsGroupType = searchConditionsGroupType, SearchSortFieldNamesList = searchSortFieldNamesList, SearchSortFieldNamesDescendingList = searchSortFieldNamesDescendingList, entityId = entityID, flatten = flatten)
	}


	listSectionSchedulerPatternDayRotations <- function(searchConditionsList = NULL, SectionSchedulerPatternDayRotationID = F, SectionSchedulerPatternID = F, DayRotationID = F, SectionSchedulerPatternDayRotationIDClonedFrom = F, SectionSchedulerPatternDayRotationClonedTo = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, searchConditionsGroupType = "And", searchSortFieldNamesList = NULL, searchSortFieldNamesDescendingList = NULL, entityID = 1, schoolYearID = NULL, page = 1, pageSize = 100000, flatten = T, ...){

		params <- as.list(environment()) %>% append(list(...))

		searchFields <- params %>% keep(names(params) %>% str_sub(1,1) == names(params) %>% str_sub(1,1) %>% str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- "all", searchFields <- searchFields %>% keep(~.x) %>% names())

		listSkyObjects(module = "Scheduling", objectName = "SectionSchedulerPatternDayRotation", schoolYearID = schoolYearID, searchFields = searchFields, page = page, pageSize = pageSize, SearchConditionsList = searchConditionsList, SearchConditionsGroupType = searchConditionsGroupType, SearchSortFieldNamesList = searchSortFieldNamesList, SearchSortFieldNamesDescendingList = searchSortFieldNamesDescendingList, entityId = entityID, flatten = flatten)
	}


	listSectionSchedulerPatterns <- function(searchConditionsList = NULL, SectionSchedulerPatternID = F, EntityID = F, SchoolYearID = F, Code = F, Description = F, DayRotationSummary = F, SectionSchedulerPatternIDClonedFrom = F, CodeDescription = F, SectionSchedulerPatternIDClonedTo = F, HasDayRotations = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, DayRotationCount = F, searchConditionsGroupType = "And", searchSortFieldNamesList = NULL, searchSortFieldNamesDescendingList = NULL, entityID = 1, schoolYearID = NULL, page = 1, pageSize = 100000, flatten = T, ...){

		params <- as.list(environment()) %>% append(list(...))

		searchFields <- params %>% keep(names(params) %>% str_sub(1,1) == names(params) %>% str_sub(1,1) %>% str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- "all", searchFields <- searchFields %>% keep(~.x) %>% names())

		listSkyObjects(module = "Scheduling", objectName = "SectionSchedulerPattern", schoolYearID = schoolYearID, searchFields = searchFields, page = page, pageSize = pageSize, SearchConditionsList = searchConditionsList, SearchConditionsGroupType = searchConditionsGroupType, SearchSortFieldNamesList = searchSortFieldNamesList, SearchSortFieldNamesDescendingList = searchSortFieldNamesDescendingList, entityId = entityID, flatten = flatten)
	}


	listCourseMasterMassUpdates <- function(searchConditionsList = NULL, CourseMasterMassUpdateID = F, UserIDRanBy = F, EntityID = F, SchoolYearID = F, RunReason = F, FilterParameters = F, ValueParameters = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, searchConditionsGroupType = "And", searchSortFieldNamesList = NULL, searchSortFieldNamesDescendingList = NULL, entityID = 1, schoolYearID = NULL, page = 1, pageSize = 100000, flatten = T, ...){

		params <- as.list(environment()) %>% append(list(...))

		searchFields <- params %>% keep(names(params) %>% str_sub(1,1) == names(params) %>% str_sub(1,1) %>% str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- "all", searchFields <- searchFields %>% keep(~.x) %>% names())

		listSkyObjects(module = "Scheduling", objectName = "CourseMasterMassUpdate", schoolYearID = schoolYearID, searchFields = searchFields, page = page, pageSize = pageSize, SearchConditionsList = searchConditionsList, SearchConditionsGroupType = searchConditionsGroupType, SearchSortFieldNamesList = searchSortFieldNamesList, SearchSortFieldNamesDescendingList = searchSortFieldNamesDescendingList, entityId = entityID, flatten = flatten)
	}


	listTempCourseMasterMassUpdateErrors <- function(searchConditionsList = NULL, TempCourseMasterMassUpdateErrorID = F, SourceDescription = F, BaseTableName = F, TableName = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, searchConditionsGroupType = "And", searchSortFieldNamesList = NULL, searchSortFieldNamesDescendingList = NULL, entityID = 1, schoolYearID = NULL, page = 1, pageSize = 100000, flatten = T, ...){

		params <- as.list(environment()) %>% append(list(...))

		searchFields <- params %>% keep(names(params) %>% str_sub(1,1) == names(params) %>% str_sub(1,1) %>% str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- "all", searchFields <- searchFields %>% keep(~.x) %>% names())

		listSkyObjects(module = "Scheduling", objectName = "TempCourseMasterMassUpdateError", schoolYearID = schoolYearID, searchFields = searchFields, page = page, pageSize = pageSize, SearchConditionsList = searchConditionsList, SearchConditionsGroupType = searchConditionsGroupType, SearchSortFieldNamesList = searchSortFieldNamesList, SearchSortFieldNamesDescendingList = searchSortFieldNamesDescendingList, entityId = entityID, flatten = flatten)
	}


	listTempCourseMasterMassUpdateFields <- function(searchConditionsList = NULL, TempCourseMasterMassUpdateFieldID = F, CourseID = F, AffectedPrimaryKey = F, SourceDescription = F, FieldName = F, OriginalValue = F, FriendlyOriginalValue = F, UpdatedValue = F, FriendlyUpdatedValue = F, TableName = F, BaseTableName = F, UpdateRank = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, FieldDisplayName = F, searchConditionsGroupType = "And", searchSortFieldNamesList = NULL, searchSortFieldNamesDescendingList = NULL, entityID = 1, schoolYearID = NULL, page = 1, pageSize = 100000, flatten = T, ...){

		params <- as.list(environment()) %>% append(list(...))

		searchFields <- params %>% keep(names(params) %>% str_sub(1,1) == names(params) %>% str_sub(1,1) %>% str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- "all", searchFields <- searchFields %>% keep(~.x) %>% names())

		listSkyObjects(module = "Scheduling", objectName = "TempCourseMasterMassUpdateField", schoolYearID = schoolYearID, searchFields = searchFields, page = page, pageSize = pageSize, SearchConditionsList = searchConditionsList, SearchConditionsGroupType = searchConditionsGroupType, SearchSortFieldNamesList = searchSortFieldNamesList, SearchSortFieldNamesDescendingList = searchSortFieldNamesDescendingList, entityId = entityID, flatten = flatten)
	}


	listSectionSchedulerProposedStaffMeets <- function(searchConditionsList = NULL, SectionSchedulerProposedStaffMeetID = F, SectionSchedulerProposedMeetID = F, StaffID = F, EffectiveStartDate = F, EffectiveEndDate = F, IsPrimary = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, searchConditionsGroupType = "And", searchSortFieldNamesList = NULL, searchSortFieldNamesDescendingList = NULL, entityID = 1, schoolYearID = NULL, page = 1, pageSize = 100000, flatten = T, ...){

		params <- as.list(environment()) %>% append(list(...))

		searchFields <- params %>% keep(names(params) %>% str_sub(1,1) == names(params) %>% str_sub(1,1) %>% str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- "all", searchFields <- searchFields %>% keep(~.x) %>% names())

		listSkyObjects(module = "Scheduling", objectName = "SectionSchedulerProposedStaffMeet", schoolYearID = schoolYearID, searchFields = searchFields, page = page, pageSize = pageSize, SearchConditionsList = searchConditionsList, SearchConditionsGroupType = searchConditionsGroupType, SearchSortFieldNamesList = searchSortFieldNamesList, SearchSortFieldNamesDescendingList = searchSortFieldNamesDescendingList, entityId = entityID, flatten = flatten)
	}


	listStaffStudents <- function(searchConditionsList = NULL, StudentID = F, StaffID = F, EntityID = F, SchoolYearID = F, searchConditionsGroupType = "And", searchSortFieldNamesList = NULL, searchSortFieldNamesDescendingList = NULL, entityID = 1, schoolYearID = NULL, page = 1, pageSize = 100000, flatten = T, ...){

		params <- as.list(environment()) %>% append(list(...))

		searchFields <- params %>% keep(names(params) %>% str_sub(1,1) == names(params) %>% str_sub(1,1) %>% str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- "all", searchFields <- searchFields %>% keep(~.x) %>% names())

		listSkyObjects(module = "Scheduling", objectName = "StaffStudent", schoolYearID = schoolYearID, searchFields = searchFields, page = page, pageSize = pageSize, SearchConditionsList = searchConditionsList, SearchConditionsGroupType = searchConditionsGroupType, SearchSortFieldNamesList = searchSortFieldNamesList, SearchSortFieldNamesDescendingList = searchSortFieldNamesDescendingList, entityId = entityID, flatten = flatten)
	}


	listTempCourseMasterMassUpdateErrorDetails <- function(searchConditionsList = NULL, TempCourseMasterMassUpdateErrorDetailID = F, TempCourseMasterMassUpdateErrorID = F, ErrorName = F, Error = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, searchConditionsGroupType = "And", searchSortFieldNamesList = NULL, searchSortFieldNamesDescendingList = NULL, entityID = 1, schoolYearID = NULL, page = 1, pageSize = 100000, flatten = T, ...){

		params <- as.list(environment()) %>% append(list(...))

		searchFields <- params %>% keep(names(params) %>% str_sub(1,1) == names(params) %>% str_sub(1,1) %>% str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- "all", searchFields <- searchFields %>% keep(~.x) %>% names())

		listSkyObjects(module = "Scheduling", objectName = "TempCourseMasterMassUpdateErrorDetail", schoolYearID = schoolYearID, searchFields = searchFields, page = page, pageSize = pageSize, SearchConditionsList = searchConditionsList, SearchConditionsGroupType = searchConditionsGroupType, SearchSortFieldNamesList = searchSortFieldNamesList, SearchSortFieldNamesDescendingList = searchSortFieldNamesDescendingList, entityId = entityID, flatten = flatten)
	}


	listSectionEnrollmentTotalForSectionLengthSubsets <- function(searchConditionsList = NULL, SectionID = F, SectionLengthSubsetID = F, TotalEnrollmentCount = F, searchConditionsGroupType = "And", searchSortFieldNamesList = NULL, searchSortFieldNamesDescendingList = NULL, entityID = 1, schoolYearID = NULL, page = 1, pageSize = 100000, flatten = T, ...){

		params <- as.list(environment()) %>% append(list(...))

		searchFields <- params %>% keep(names(params) %>% str_sub(1,1) == names(params) %>% str_sub(1,1) %>% str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- "all", searchFields <- searchFields %>% keep(~.x) %>% names())

		listSkyObjects(module = "Scheduling", objectName = "SectionEnrollmentTotalForSectionLengthSubset", schoolYearID = schoolYearID, searchFields = searchFields, page = page, pageSize = pageSize, SearchConditionsList = searchConditionsList, SearchConditionsGroupType = searchConditionsGroupType, SearchSortFieldNamesList = searchSortFieldNamesList, SearchSortFieldNamesDescendingList = searchSortFieldNamesDescendingList, entityId = entityID, flatten = flatten)
	}


	listTempStaffMeets <- function(searchConditionsList = NULL, TempStaffMeetID = F, StaffMeetID = F, MeetID = F, SectionID = F, CourseCode = F, SectionCode = F, CourseDescription = F, EffectiveStartDate = F, EffectiveEndDate = F, IsPrimary = F, IsSubstitute = F, StaffID = F, StaffFullNameFML = F, NewEffectiveStartDate = F, NewEffectiveEndDate = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, IsLongTermSubstitute = F, HasAttendanceAccess = F, HasGradebookAccess = F, IsChecked = F, NewStaffID = F, NewStaffFullNameFML = F, WorkflowAction = F, Note = F, Conflicts = F, HasConflicts = F, searchConditionsGroupType = "And", searchSortFieldNamesList = NULL, searchSortFieldNamesDescendingList = NULL, entityID = 1, schoolYearID = NULL, page = 1, pageSize = 100000, flatten = T, ...){

		params <- as.list(environment()) %>% append(list(...))

		searchFields <- params %>% keep(names(params) %>% str_sub(1,1) == names(params) %>% str_sub(1,1) %>% str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- "all", searchFields <- searchFields %>% keep(~.x) %>% names())

		listSkyObjects(module = "Scheduling", objectName = "TempStaffMeet", schoolYearID = schoolYearID, searchFields = searchFields, page = page, pageSize = pageSize, SearchConditionsList = searchConditionsList, SearchConditionsGroupType = searchConditionsGroupType, SearchSortFieldNamesList = searchSortFieldNamesList, SearchSortFieldNamesDescendingList = searchSortFieldNamesDescendingList, entityId = entityID, flatten = flatten)
	}


	listSectionEnrollmentTotalForSectionLengthSubsetAndEntities <- function(searchConditionsList = NULL, SectionID = F, EntityIDCountsAs = F, SectionLengthSubsetID = F, TotalEnrollmentCount = F, searchConditionsGroupType = "And", searchSortFieldNamesList = NULL, searchSortFieldNamesDescendingList = NULL, entityID = 1, schoolYearID = NULL, page = 1, pageSize = 100000, flatten = T, ...){

		params <- as.list(environment()) %>% append(list(...))

		searchFields <- params %>% keep(names(params) %>% str_sub(1,1) == names(params) %>% str_sub(1,1) %>% str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- "all", searchFields <- searchFields %>% keep(~.x) %>% names())

		listSkyObjects(module = "Scheduling", objectName = "SectionEnrollmentTotalForSectionLengthSubsetAndEntity", schoolYearID = schoolYearID, searchFields = searchFields, page = page, pageSize = pageSize, SearchConditionsList = searchConditionsList, SearchConditionsGroupType = searchConditionsGroupType, SearchSortFieldNamesList = searchSortFieldNamesList, SearchSortFieldNamesDescendingList = searchSortFieldNamesDescendingList, entityId = entityID, flatten = flatten)
	}


	listSectionMeetSummaries <- function(searchConditionsList = NULL, SectionMeetSummaryID = F, SectionID = F, EntityIDViewing = F, CalendarID = F, IsDefaultCalendar = F, PeriodDaySummary = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, searchConditionsGroupType = "And", searchSortFieldNamesList = NULL, searchSortFieldNamesDescendingList = NULL, entityID = 1, schoolYearID = NULL, page = 1, pageSize = 100000, flatten = T, ...){

		params <- as.list(environment()) %>% append(list(...))

		searchFields <- params %>% keep(names(params) %>% str_sub(1,1) == names(params) %>% str_sub(1,1) %>% str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- "all", searchFields <- searchFields %>% keep(~.x) %>% names())

		listSkyObjects(module = "Scheduling", objectName = "SectionMeetSummary", schoolYearID = schoolYearID, searchFields = searchFields, page = page, pageSize = pageSize, SearchConditionsList = searchConditionsList, SearchConditionsGroupType = searchConditionsGroupType, SearchSortFieldNamesList = searchSortFieldNamesList, SearchSortFieldNamesDescendingList = searchSortFieldNamesDescendingList, entityId = entityID, flatten = flatten)
	}


	listTempStudents <- function(searchConditionsList = NULL, TempStudentID = F, StudentID = F, FullNameLFM = F, StudentNumber = F, GradeReferenceID = F, GradeLevelCode = F, GradYear = F, StudentTypeCodeDescription = F, CurrentActive = F, GenderCode = F, SchoolName = F, HomeRoomCode = F, IsSelected = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, HasConflictedStudentCourseRequest = F, CourseRequestCount = F, StudentSectionCount = F, HasFailedUpdate = F, searchConditionsGroupType = "And", searchSortFieldNamesList = NULL, searchSortFieldNamesDescendingList = NULL, entityID = 1, schoolYearID = NULL, page = 1, pageSize = 100000, flatten = T, ...){

		params <- as.list(environment()) %>% append(list(...))

		searchFields <- params %>% keep(names(params) %>% str_sub(1,1) == names(params) %>% str_sub(1,1) %>% str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- "all", searchFields <- searchFields %>% keep(~.x) %>% names())

		listSkyObjects(module = "Scheduling", objectName = "TempStudent", schoolYearID = schoolYearID, searchFields = searchFields, page = page, pageSize = pageSize, SearchConditionsList = searchConditionsList, SearchConditionsGroupType = searchConditionsGroupType, SearchSortFieldNamesList = searchSortFieldNamesList, SearchSortFieldNamesDescendingList = searchSortFieldNamesDescendingList, entityId = entityID, flatten = flatten)
	}


	listSectionSchedulerDayRotationForMeets <- function(searchConditionsList = NULL, SectionSchedulerDayRotationForMeetID = F, MeetID = F, DayRotationID = F, SectionSchedulerDayRotationForMeetIDClonedFrom = F, SectionSchedulerDayRotationForMeetClonedTo = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, searchConditionsGroupType = "And", searchSortFieldNamesList = NULL, searchSortFieldNamesDescendingList = NULL, entityID = 1, schoolYearID = NULL, page = 1, pageSize = 100000, flatten = T, ...){

		params <- as.list(environment()) %>% append(list(...))

		searchFields <- params %>% keep(names(params) %>% str_sub(1,1) == names(params) %>% str_sub(1,1) %>% str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- "all", searchFields <- searchFields %>% keep(~.x) %>% names())

		listSkyObjects(module = "Scheduling", objectName = "SectionSchedulerDayRotationForMeet", schoolYearID = schoolYearID, searchFields = searchFields, page = page, pageSize = pageSize, SearchConditionsList = searchConditionsList, SearchConditionsGroupType = searchConditionsGroupType, SearchSortFieldNamesList = searchSortFieldNamesList, SearchSortFieldNamesDescendingList = searchSortFieldNamesDescendingList, entityId = entityID, flatten = flatten)
	}


	listAppleSchoolManagerConfigs <- function(searchConditionsList = NULL, AppleSchoolManagerConfigID = F, DistrictID = F, SchoolYearID = F, AsOfDateSetting = F, EntityIDSelectedList = F, SpecifiedAsOfDate = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, ZipFileName = F, MediaID = F, FileDestinationIDSelectedList = F, searchConditionsGroupType = "And", searchSortFieldNamesList = NULL, searchSortFieldNamesDescendingList = NULL, entityID = 1, schoolYearID = NULL, page = 1, pageSize = 100000, flatten = T, ...){

		params <- as.list(environment()) %>% append(list(...))

		searchFields <- params %>% keep(names(params) %>% str_sub(1,1) == names(params) %>% str_sub(1,1) %>% str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- "all", searchFields <- searchFields %>% keep(~.x) %>% names())

		listSkyObjects(module = "Scheduling", objectName = "AppleSchoolManagerConfig", schoolYearID = schoolYearID, searchFields = searchFields, page = page, pageSize = pageSize, SearchConditionsList = searchConditionsList, SearchConditionsGroupType = searchConditionsGroupType, SearchSortFieldNamesList = searchSortFieldNamesList, SearchSortFieldNamesDescendingList = searchSortFieldNamesDescendingList, entityId = entityID, flatten = flatten)
	}


	listTempFailedStaffMeets <- function(searchConditionsList = NULL, TempFailedStaffMeetID = F, TempStaffMeetID = F, ErrorCount = F, HasErrors = F, StaffMeetID = F, MeetID = F, SectionID = F, CourseCode = F, SectionCode = F, CourseDescription = F, EffectiveStartDate = F, EffectiveEndDate = F, HasAttendanceAccess = F, HasGradebookAccess = F, IsPrimary = F, IsChecked = F, IsSubstitute = F, IsLongTermSubstitute = F, StaffID = F, StaffFullNameFML = F, NewEffectiveStartDate = F, NewEffectiveEndDate = F, NewStaffID = F, NewStaffFullNameFML = F, WorkflowAction = F, Note = F, Conflicts = F, HasConflicts = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, searchConditionsGroupType = "And", searchSortFieldNamesList = NULL, searchSortFieldNamesDescendingList = NULL, entityID = 1, schoolYearID = NULL, page = 1, pageSize = 100000, flatten = T, ...){

		params <- as.list(environment()) %>% append(list(...))

		searchFields <- params %>% keep(names(params) %>% str_sub(1,1) == names(params) %>% str_sub(1,1) %>% str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- "all", searchFields <- searchFields %>% keep(~.x) %>% names())

		listSkyObjects(module = "Scheduling", objectName = "TempFailedStaffMeet", schoolYearID = schoolYearID, searchFields = searchFields, page = page, pageSize = pageSize, SearchConditionsList = searchConditionsList, SearchConditionsGroupType = searchConditionsGroupType, SearchSortFieldNamesList = searchSortFieldNamesList, SearchSortFieldNamesDescendingList = searchSortFieldNamesDescendingList, entityId = entityID, flatten = flatten)
	}


	listQueuedStudentSections <- function(searchConditionsList = NULL, QueuedStudentSectionID = F, StudentSectionID = F, IsComplete = F, SourceProcess = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, searchConditionsGroupType = "And", searchSortFieldNamesList = NULL, searchSortFieldNamesDescendingList = NULL, entityID = 1, schoolYearID = NULL, page = 1, pageSize = 100000, flatten = T, ...){

		params <- as.list(environment()) %>% append(list(...))

		searchFields <- params %>% keep(names(params) %>% str_sub(1,1) == names(params) %>% str_sub(1,1) %>% str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- "all", searchFields <- searchFields %>% keep(~.x) %>% names())

		listSkyObjects(module = "Scheduling", objectName = "QueuedStudentSection", schoolYearID = schoolYearID, searchFields = searchFields, page = page, pageSize = pageSize, SearchConditionsList = searchConditionsList, SearchConditionsGroupType = searchConditionsGroupType, SearchSortFieldNamesList = searchSortFieldNamesList, SearchSortFieldNamesDescendingList = searchSortFieldNamesDescendingList, entityId = entityID, flatten = flatten)
	}


	listStudentSectionTransactionRoomPeriods <- function(searchConditionsList = NULL, StudentSectionTransactionID = F, StudentSectionID = F, StudentID = F, MeetID = F, SectionID = F, StartDateMeet = F, EndDateMeet = F, StartDateStudentSectionTransaction = F, EndDateSectionSectionTransaction = F, EntityIDCountsAs = F, EntityIDCourse = F, EntityIDFor = F, EntityIDViewingCalculated = F, SchoolYearID = F, RoomID = F, UseRoomOverride = F, MeetSummaryID = F, EntityIDViewingMeetSummary = F, CalendarID = F, IsDefaultCalendar = F, DisplayPeriodID = F, SchedulingPeriodID = F, searchConditionsGroupType = "And", searchSortFieldNamesList = NULL, searchSortFieldNamesDescendingList = NULL, entityID = 1, schoolYearID = NULL, page = 1, pageSize = 100000, flatten = T, ...){

		params <- as.list(environment()) %>% append(list(...))

		searchFields <- params %>% keep(names(params) %>% str_sub(1,1) == names(params) %>% str_sub(1,1) %>% str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- "all", searchFields <- searchFields %>% keep(~.x) %>% names())

		listSkyObjects(module = "Scheduling", objectName = "StudentSectionTransactionRoomPeriod", schoolYearID = schoolYearID, searchFields = searchFields, page = page, pageSize = pageSize, SearchConditionsList = searchConditionsList, SearchConditionsGroupType = searchConditionsGroupType, SearchSortFieldNamesList = searchSortFieldNamesList, SearchSortFieldNamesDescendingList = searchSortFieldNamesDescendingList, entityId = entityID, flatten = flatten)
	}


	listMeetEntityRoomPeriods <- function(searchConditionsList = NULL, MeetID = F, SectionID = F, StartDate = F, EndDate = F, EntityIDCourse = F, EntityIDFor = F, EntityIDViewingCalculated = F, SchoolYearID = F, RoomID = F, UseRoomOverride = F, MeetSummaryID = F, EntityIDViewingMeetSummary = F, CalendarID = F, IsDefaultCalendar = F, DisplayPeriodID = F, SchedulingPeriodID = F, searchConditionsGroupType = "And", searchSortFieldNamesList = NULL, searchSortFieldNamesDescendingList = NULL, entityID = 1, schoolYearID = NULL, page = 1, pageSize = 100000, flatten = T, ...){

		params <- as.list(environment()) %>% append(list(...))

		searchFields <- params %>% keep(names(params) %>% str_sub(1,1) == names(params) %>% str_sub(1,1) %>% str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- "all", searchFields <- searchFields %>% keep(~.x) %>% names())

		listSkyObjects(module = "Scheduling", objectName = "MeetEntityRoomPeriod", schoolYearID = schoolYearID, searchFields = searchFields, page = page, pageSize = pageSize, SearchConditionsList = searchConditionsList, SearchConditionsGroupType = searchConditionsGroupType, SearchSortFieldNamesList = searchSortFieldNamesList, SearchSortFieldNamesDescendingList = searchSortFieldNamesDescendingList, entityId = entityID, flatten = flatten)
	}


	listMeetEntityRoomPeriodDateSeatsAvailables <- function(searchConditionsList = NULL, MeetID = F, SectionID = F, StartDate = F, EndDate = F, EntityIDCourse = F, EntityIDFor = F, EntityIDViewingCalculated = F, IsOfferedMeet = F, SchoolYearID = F, RoomIDMeet = F, RoomIDMeetOverride = F, RoomID = F, UseRoomOverride = F, MaxSeatsMeet = F, MaxSeatsMeetOverride = F, MaxSeats = F, MeetSummaryID = F, EntityIDViewingMeetSummary = F, CalendarID = F, DisplayPeriodID = F, SchedulingPeriodID = F, Date = F, RoomSeatsAvailable = F, searchConditionsGroupType = "And", searchSortFieldNamesList = NULL, searchSortFieldNamesDescendingList = NULL, entityID = 1, schoolYearID = NULL, page = 1, pageSize = 100000, flatten = T, ...){

		params <- as.list(environment()) %>% append(list(...))

		searchFields <- params %>% keep(names(params) %>% str_sub(1,1) == names(params) %>% str_sub(1,1) %>% str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- "all", searchFields <- searchFields %>% keep(~.x) %>% names())

		listSkyObjects(module = "Scheduling", objectName = "MeetEntityRoomPeriodDateSeatsAvailable", schoolYearID = schoolYearID, searchFields = searchFields, page = page, pageSize = pageSize, SearchConditionsList = searchConditionsList, SearchConditionsGroupType = searchConditionsGroupType, SearchSortFieldNamesList = searchSortFieldNamesList, SearchSortFieldNamesDescendingList = searchSortFieldNamesDescendingList, entityId = entityID, flatten = flatten)
	}


	listCoursePrerequisiteCurriculumCourseStudentCourseRequests <- function(searchConditionsList = NULL, CoursePrerequisiteCurriculumCourseStudentCourseRequestID = F, PrerequisiteID = F, CurriculumIDFor = F, CurriculumIDRequired = F, CourseIDFor = F, StudentCourseRequestID = F, StudentSectionID = F, CourseIDRequired = F, StudentID = F, Status = F, searchConditionsGroupType = "And", searchSortFieldNamesList = NULL, searchSortFieldNamesDescendingList = NULL, entityID = 1, schoolYearID = NULL, page = 1, pageSize = 100000, flatten = T, ...){

		params <- as.list(environment()) %>% append(list(...))

		searchFields <- params %>% keep(names(params) %>% str_sub(1,1) == names(params) %>% str_sub(1,1) %>% str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- "all", searchFields <- searchFields %>% keep(~.x) %>% names())

		listSkyObjects(module = "Scheduling", objectName = "CoursePrerequisiteCurriculumCourseStudentCourseRequest", schoolYearID = schoolYearID, searchFields = searchFields, page = page, pageSize = pageSize, SearchConditionsList = searchConditionsList, SearchConditionsGroupType = searchConditionsGroupType, SearchSortFieldNamesList = searchSortFieldNamesList, SearchSortFieldNamesDescendingList = searchSortFieldNamesDescendingList, entityId = entityID, flatten = flatten)
	}


	listCoursePrerequisiteCurriculumCourses <- function(searchConditionsList = NULL, CoursePrerequisiteCurriculumCourseID = F, CourseIDFor = F, EntityIDFor = F, SchoolYearIDFor = F, NumericYearCourse = F, NumericYearCurrentFor = F, CurriculumIDFor = F, PrerequisiteCurriculumID = F, PrerequisiteID = F, CurriculumIDRequired = F, CourseIDRequired = F, SchoolYearIDRequired = F, EntityIDRequired = F, NumericYearRequired = F, searchConditionsGroupType = "And", searchSortFieldNamesList = NULL, searchSortFieldNamesDescendingList = NULL, entityID = 1, schoolYearID = NULL, page = 1, pageSize = 100000, flatten = T, ...){

		params <- as.list(environment()) %>% append(list(...))

		searchFields <- params %>% keep(names(params) %>% str_sub(1,1) == names(params) %>% str_sub(1,1) %>% str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- "all", searchFields <- searchFields %>% keep(~.x) %>% names())

		listSkyObjects(module = "Scheduling", objectName = "CoursePrerequisiteCurriculumCourse", schoolYearID = schoolYearID, searchFields = searchFields, page = page, pageSize = pageSize, SearchConditionsList = searchConditionsList, SearchConditionsGroupType = searchConditionsGroupType, SearchSortFieldNamesList = searchSortFieldNamesList, SearchSortFieldNamesDescendingList = searchSortFieldNamesDescendingList, entityId = entityID, flatten = flatten)
	}


	listCoursePrerequisites <- function(searchConditionsList = NULL, CoursePrerequisiteID = F, CourseID = F, EntityID = F, SchoolYearID = F, NumericYearCourse = F, NumericYearCurrent = F, CurriculumID = F, PrerequisiteID = F, EarnedCredits = F, SchoolYearLow = F, SchoolYearHigh = F, PrerequisiteCode = F, CurriculumCode = F, CurriculumDescription = F, HasPrequisiteCurriculums = F, searchConditionsGroupType = "And", searchSortFieldNamesList = NULL, searchSortFieldNamesDescendingList = NULL, entityID = 1, schoolYearID = NULL, page = 1, pageSize = 100000, flatten = T, ...){

		params <- as.list(environment()) %>% append(list(...))

		searchFields <- params %>% keep(names(params) %>% str_sub(1,1) == names(params) %>% str_sub(1,1) %>% str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- "all", searchFields <- searchFields %>% keep(~.x) %>% names())

		listSkyObjects(module = "Scheduling", objectName = "CoursePrerequisite", schoolYearID = schoolYearID, searchFields = searchFields, page = page, pageSize = pageSize, SearchConditionsList = searchConditionsList, SearchConditionsGroupType = searchConditionsGroupType, SearchSortFieldNamesList = searchSortFieldNamesList, SearchSortFieldNamesDescendingList = searchSortFieldNamesDescendingList, entityId = entityID, flatten = flatten)
	}


	listPrerequisiteCurriculumCourses <- function(searchConditionsList = NULL, PrerequisiteCurriculumCourseID = F, PrerequisiteCurriculumID = F, PrerequisiteID = F, CurriculumIDFor = F, CurriculumIDRequired = F, CourseID = F, SchoolYearID = F, EntityID = F, NumericYear = F, searchConditionsGroupType = "And", searchSortFieldNamesList = NULL, searchSortFieldNamesDescendingList = NULL, entityID = 1, schoolYearID = NULL, page = 1, pageSize = 100000, flatten = T, ...){

		params <- as.list(environment()) %>% append(list(...))

		searchFields <- params %>% keep(names(params) %>% str_sub(1,1) == names(params) %>% str_sub(1,1) %>% str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- "all", searchFields <- searchFields %>% keep(~.x) %>% names())

		listSkyObjects(module = "Scheduling", objectName = "PrerequisiteCurriculumCourse", schoolYearID = schoolYearID, searchFields = searchFields, page = page, pageSize = pageSize, SearchConditionsList = searchConditionsList, SearchConditionsGroupType = searchConditionsGroupType, SearchSortFieldNamesList = searchSortFieldNamesList, SearchSortFieldNamesDescendingList = searchSortFieldNamesDescendingList, entityId = entityID, flatten = flatten)
	}


	listAvailabilityFilterArenaSchedulingSettings <- function(searchConditionsList = NULL, AvailabilityFilterArenaSchedulingSettingID = F, AvailableStartDate = F, AvailableEndDate = F, SchedulingEntryStartDateTime = F, SchedulingEntryEndDateTime = F, AvailabilityFilterCourseStudentID = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, HideStaff = F, HideRoom = F, ShowAllClasses = F, ShowMyRequests = F, ShowMyAlternateRequests = F, LockAfterSubmission = F, EnableAutoSchedule = F, AutoSchedulerCourseSelection = F, searchConditionsGroupType = "And", searchSortFieldNamesList = NULL, searchSortFieldNamesDescendingList = NULL, entityID = 1, schoolYearID = NULL, page = 1, pageSize = 100000, flatten = T, ...){

		params <- as.list(environment()) %>% append(list(...))

		searchFields <- params %>% keep(names(params) %>% str_sub(1,1) == names(params) %>% str_sub(1,1) %>% str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- "all", searchFields <- searchFields %>% keep(~.x) %>% names())

		listSkyObjects(module = "Scheduling", objectName = "AvailabilityFilterArenaSchedulingSetting", schoolYearID = schoolYearID, searchFields = searchFields, page = page, pageSize = pageSize, SearchConditionsList = searchConditionsList, SearchConditionsGroupType = searchConditionsGroupType, SearchSortFieldNamesList = searchSortFieldNamesList, SearchSortFieldNamesDescendingList = searchSortFieldNamesDescendingList, entityId = entityID, flatten = flatten)
	}


	listAvailabilityFilterCourseRequestSettings <- function(searchConditionsList = NULL, AvailabilityFilterCourseRequestSettingID = F, AvailableStartDate = F, AvailableEndDate = F, RequestEntryStartDateTime = F, RequestEntryEndDateTime = F, UseCreditsMaximum = F, UseCourseRequestCountMaximum = F, MaximumCredits = F, MaximumAlternateCredits = F, MaximumCourseRequests = F, MaximumAlternateCourseRequests = F, AvailabilityFilterCourseStudentID = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, searchConditionsGroupType = "And", searchSortFieldNamesList = NULL, searchSortFieldNamesDescendingList = NULL, entityID = 1, schoolYearID = NULL, page = 1, pageSize = 100000, flatten = T, ...){

		params <- as.list(environment()) %>% append(list(...))

		searchFields <- params %>% keep(names(params) %>% str_sub(1,1) == names(params) %>% str_sub(1,1) %>% str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- "all", searchFields <- searchFields %>% keep(~.x) %>% names())

		listSkyObjects(module = "Scheduling", objectName = "AvailabilityFilterCourseRequestSetting", schoolYearID = schoolYearID, searchFields = searchFields, page = page, pageSize = pageSize, SearchConditionsList = searchConditionsList, SearchConditionsGroupType = searchConditionsGroupType, SearchSortFieldNamesList = searchSortFieldNamesList, SearchSortFieldNamesDescendingList = searchSortFieldNamesDescendingList, entityId = entityID, flatten = flatten)
	}


	listMassUpdateSystemDateRunHistoryDetails <- function(searchConditionsList = NULL, MassUpdateSystemDateRunHistoryDetailID = F, MassUpdateSystemDateRunHistoryID = F, SortNumber = F, TableType = F, StartDateRecordsProcessedCount = F, StartDateSuccessCount = F, StartDateFailedCount = F, EndDateRecordsProcessedCount = F, EndDateSuccessCount = F, EndDateFailedCount = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, searchConditionsGroupType = "And", searchSortFieldNamesList = NULL, searchSortFieldNamesDescendingList = NULL, entityID = 1, schoolYearID = NULL, page = 1, pageSize = 100000, flatten = T, ...){

		params <- as.list(environment()) %>% append(list(...))

		searchFields <- params %>% keep(names(params) %>% str_sub(1,1) == names(params) %>% str_sub(1,1) %>% str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- "all", searchFields <- searchFields %>% keep(~.x) %>% names())

		listSkyObjects(module = "Scheduling", objectName = "MassUpdateSystemDateRunHistoryDetail", schoolYearID = schoolYearID, searchFields = searchFields, page = page, pageSize = pageSize, SearchConditionsList = searchConditionsList, SearchConditionsGroupType = searchConditionsGroupType, SearchSortFieldNamesList = searchSortFieldNamesList, SearchSortFieldNamesDescendingList = searchSortFieldNamesDescendingList, entityId = entityID, flatten = flatten)
	}


	listMassUpdateSystemDateRunHistoryErrors <- function(searchConditionsList = NULL, MassUpdateSystemDateRunHistoryErrorID = F, MassUpdateSystemDateRunHistoryID = F, ParentObjectID = F, ObjectID = F, Description = F, ErrorDetail = F, TableType = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, searchConditionsGroupType = "And", searchSortFieldNamesList = NULL, searchSortFieldNamesDescendingList = NULL, entityID = 1, schoolYearID = NULL, page = 1, pageSize = 100000, flatten = T, ...){

		params <- as.list(environment()) %>% append(list(...))

		searchFields <- params %>% keep(names(params) %>% str_sub(1,1) == names(params) %>% str_sub(1,1) %>% str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- "all", searchFields <- searchFields %>% keep(~.x) %>% names())

		listSkyObjects(module = "Scheduling", objectName = "MassUpdateSystemDateRunHistoryError", schoolYearID = schoolYearID, searchFields = searchFields, page = page, pageSize = pageSize, SearchConditionsList = searchConditionsList, SearchConditionsGroupType = searchConditionsGroupType, SearchSortFieldNamesList = searchSortFieldNamesList, SearchSortFieldNamesDescendingList = searchSortFieldNamesDescendingList, entityId = entityID, flatten = flatten)
	}


	listMassUpdateSystemDateRunHistories <- function(searchConditionsList = NULL, MassUpdateSystemDateRunHistoryID = F, RunReason = F, RunDescription = F, DateTemplateXML = F, SchoolYearID = F, EntityID = F, StartTime = F, EndTime = F, Duration = F, Location = F, SourceID = F, HasErrors = F, ErrorCount = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, searchConditionsGroupType = "And", searchSortFieldNamesList = NULL, searchSortFieldNamesDescendingList = NULL, entityID = 1, schoolYearID = NULL, page = 1, pageSize = 100000, flatten = T, ...){

		params <- as.list(environment()) %>% append(list(...))

		searchFields <- params %>% keep(names(params) %>% str_sub(1,1) == names(params) %>% str_sub(1,1) %>% str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- "all", searchFields <- searchFields %>% keep(~.x) %>% names())

		listSkyObjects(module = "Scheduling", objectName = "MassUpdateSystemDateRunHistory", schoolYearID = schoolYearID, searchFields = searchFields, page = page, pageSize = pageSize, SearchConditionsList = searchConditionsList, SearchConditionsGroupType = searchConditionsGroupType, SearchSortFieldNamesList = searchSortFieldNamesList, SearchSortFieldNamesDescendingList = searchSortFieldNamesDescendingList, entityId = entityID, flatten = flatten)
	}


	listTempMasterDateChangeDetails <- function(searchConditionsList = NULL, TempMasterDateChangeDetailID = F, CurrentDate = F, NewDate = F, UsedBy = F, DateDescriptor = F, DisplayLowHighDates = F, LowDate = F, HighDate = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, searchConditionsGroupType = "And", searchSortFieldNamesList = NULL, searchSortFieldNamesDescendingList = NULL, entityID = 1, schoolYearID = NULL, page = 1, pageSize = 100000, flatten = T, ...){

		params <- as.list(environment()) %>% append(list(...))

		searchFields <- params %>% keep(names(params) %>% str_sub(1,1) == names(params) %>% str_sub(1,1) %>% str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- "all", searchFields <- searchFields %>% keep(~.x) %>% names())

		listSkyObjects(module = "Scheduling", objectName = "TempMasterDateChangeDetail", schoolYearID = schoolYearID, searchFields = searchFields, page = page, pageSize = pageSize, SearchConditionsList = searchConditionsList, SearchConditionsGroupType = searchConditionsGroupType, SearchSortFieldNamesList = searchSortFieldNamesList, SearchSortFieldNamesDescendingList = searchSortFieldNamesDescendingList, entityId = entityID, flatten = flatten)
	}


	listCourseGradeReferenceSummaries <- function(searchConditionsList = NULL, CourseID = F, GradeLevelIDSummary = F, searchConditionsGroupType = "And", searchSortFieldNamesList = NULL, searchSortFieldNamesDescendingList = NULL, entityID = 1, schoolYearID = NULL, page = 1, pageSize = 100000, flatten = T, ...){

		params <- as.list(environment()) %>% append(list(...))

		searchFields <- params %>% keep(names(params) %>% str_sub(1,1) == names(params) %>% str_sub(1,1) %>% str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- "all", searchFields <- searchFields %>% keep(~.x) %>% names())

		listSkyObjects(module = "Scheduling", objectName = "CourseGradeReferenceSummary", schoolYearID = schoolYearID, searchFields = searchFields, page = page, pageSize = pageSize, SearchConditionsList = searchConditionsList, SearchConditionsGroupType = searchConditionsGroupType, SearchSortFieldNamesList = searchSortFieldNamesList, SearchSortFieldNamesDescendingList = searchSortFieldNamesDescendingList, entityId = entityID, flatten = flatten)
	}


	listCourseCorequisites <- function(searchConditionsList = NULL, CourseCorequisiteID = F, CourseIDParent = F, CourseCorequisiteGroupID = F, CourseIDMember = F, AutomaticRequestSetting = F, searchConditionsGroupType = "And", searchSortFieldNamesList = NULL, searchSortFieldNamesDescendingList = NULL, entityID = 1, schoolYearID = NULL, page = 1, pageSize = 100000, flatten = T, ...){

		params <- as.list(environment()) %>% append(list(...))

		searchFields <- params %>% keep(names(params) %>% str_sub(1,1) == names(params) %>% str_sub(1,1) %>% str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- "all", searchFields <- searchFields %>% keep(~.x) %>% names())

		listSkyObjects(module = "Scheduling", objectName = "CourseCorequisite", schoolYearID = schoolYearID, searchFields = searchFields, page = page, pageSize = pageSize, SearchConditionsList = searchConditionsList, SearchConditionsGroupType = searchConditionsGroupType, SearchSortFieldNamesList = searchSortFieldNamesList, SearchSortFieldNamesDescendingList = searchSortFieldNamesDescendingList, entityId = entityID, flatten = flatten)
	}


	listAppleSchoolManagerConfigFileDestinations <- function(searchConditionsList = NULL, AppleSchoolManagerConfigFileDestinationID = F, AppleSchoolManagerConfigID = F, FileDestinationID = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, searchConditionsGroupType = "And", searchSortFieldNamesList = NULL, searchSortFieldNamesDescendingList = NULL, entityID = 1, schoolYearID = NULL, page = 1, pageSize = 100000, flatten = T, ...){

		params <- as.list(environment()) %>% append(list(...))

		searchFields <- params %>% keep(names(params) %>% str_sub(1,1) == names(params) %>% str_sub(1,1) %>% str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- "all", searchFields <- searchFields %>% keep(~.x) %>% names())

		listSkyObjects(module = "Scheduling", objectName = "AppleSchoolManagerConfigFileDestination", schoolYearID = schoolYearID, searchFields = searchFields, page = page, pageSize = pageSize, SearchConditionsList = searchConditionsList, SearchConditionsGroupType = searchConditionsGroupType, SearchSortFieldNamesList = searchSortFieldNamesList, SearchSortFieldNamesDescendingList = searchSortFieldNamesDescendingList, entityId = entityID, flatten = flatten)
	}
