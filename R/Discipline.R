

	listActionAttendanceTypes <- function(searchConditionsList = NULL, ActionAttendanceTypeID = F, ActionID = F, AttendanceTypeID = F, EntityGroupKey = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, IsReadOnlyHistoricalRecord = F, ActionAttendanceTypeIDClonedFrom = F, searchConditionsGroupType = "And", searchSortFieldNamesList = NULL, searchSortFieldNamesDescendingList = NULL, entityID = 1, schoolYearID = NULL, page = 1, pageSize = 100000, flatten = T, ...){

		params <- as.list(environment()) %>% append(list(...))

		searchFields <- params %>% keep(names(params) %>% str_sub(1,1) == names(params) %>% str_sub(1,1) %>% str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- "all", searchFields <- searchFields %>% keep(~.x) %>% names())

		listSkyObjects(module = "Discipline", objectName = "ActionAttendanceType", schoolYearID = schoolYearID, searchFields = searchFields, page = page, pageSize = pageSize, SearchConditionsList = searchConditionsList, SearchConditionsGroupType = searchConditionsGroupType, SearchSortFieldNamesList = searchSortFieldNamesList, SearchSortFieldNamesDescendingList = searchSortFieldNamesDescendingList, entityId = entityID, flatten = flatten)
	}


	listIncidentOffenseNameActionDetailPeriods <- function(searchConditionsList = NULL, IncidentOffenseNameActionDetailPeriodID = F, IncidentOffenseNameActionDetailID = F, AttendancePeriodID = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, IsReadOnlyHistoricalRecord = F, searchConditionsGroupType = "And", searchSortFieldNamesList = NULL, searchSortFieldNamesDescendingList = NULL, entityID = 1, schoolYearID = NULL, page = 1, pageSize = 100000, flatten = T, ...){

		params <- as.list(environment()) %>% append(list(...))

		searchFields <- params %>% keep(names(params) %>% str_sub(1,1) == names(params) %>% str_sub(1,1) %>% str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- "all", searchFields <- searchFields %>% keep(~.x) %>% names())

		listSkyObjects(module = "Discipline", objectName = "IncidentOffenseNameActionDetailPeriod", schoolYearID = schoolYearID, searchFields = searchFields, page = page, pageSize = pageSize, SearchConditionsList = searchConditionsList, SearchConditionsGroupType = searchConditionsGroupType, SearchSortFieldNamesList = searchSortFieldNamesList, SearchSortFieldNamesDescendingList = searchSortFieldNamesDescendingList, entityId = entityID, flatten = flatten)
	}


	listDisciplineConfigEntityGroupYears <- function(searchConditionsList = NULL, ConfigEntityGroupYearID = F, EntityID = F, EntityGroupKey = F, SchoolYearID = F, UseAlternateActionDetails = F, DetentionsOnMon = F, DetentionsOnTue = F, DetentionsOnWed = F, DetentionsOnThu = F, DetentionsOnFri = F, DetentionsOnSat = F, DetentionsOnSun = F, InSchoolSuspensionsOnMon = F, InSchoolSuspensionsOnTue = F, InSchoolSuspensionsOnWed = F, InSchoolSuspensionsOnThu = F, InSchoolSuspensionsOnFri = F, InSchoolSuspensionsOnSat = F, InSchoolSuspensionsOnSun = F, OutOfSchoolSuspensionsOnMon = F, OutOfSchoolSuspensionsOnTue = F, OutOfSchoolSuspensionsOnWed = F, OutOfSchoolSuspensionsOnThu = F, OutOfSchoolSuspensionsOnFri = F, OutOfSchoolSuspensionsOnSat = F, OutOfSchoolSuspensionsOnSun = F, ExpulsionsOnMon = F, ExpulsionsOnTue = F, ExpulsionsOnWed = F, ExpulsionsOnThu = F, ExpulsionsOnFri = F, ExpulsionsOnSat = F, ExpulsionsOnSun = F, DefaultActionStatusCode = F, TardyKioskDisciplineSlipTitle = F, IncludeStudentNameAndOrNumberOnLetter = F, IncludeGradeLevelOnLetter = F, IncludeSchoolOrCampusOnLetter = F, IncludeParentNameAndOrPhoneNumberOnLetter = F, IncludeDisciplinaryActionAndDetailsOnLetter = F, IncludeSignatureLineForParentOnLetter = F, IncludeSignatureLineForStudentOnLetter = F, IncludeSignatureLineForOfficeOnLetter = F, ActionStatusDefaultValue = F, ConfigEntityGroupYearIDClonedFrom = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, IsReadOnlyHistoricalRecord = F, IncludeOffenseDescriptionOnLetter = F, IncludeIncidentDateAndTimeOnLetter = F, IncludeIncidentDescriptionOnLetter = F, IncludeReferredByOnLetter = F, searchConditionsGroupType = "And", searchSortFieldNamesList = NULL, searchSortFieldNamesDescendingList = NULL, entityID = 1, schoolYearID = NULL, page = 1, pageSize = 100000, flatten = T, ...){

		params <- as.list(environment()) %>% append(list(...))

		searchFields <- params %>% keep(names(params) %>% str_sub(1,1) == names(params) %>% str_sub(1,1) %>% str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- "all", searchFields <- searchFields %>% keep(~.x) %>% names())

		listSkyObjects(module = "Discipline", objectName = "ConfigEntityGroupYear", schoolYearID = schoolYearID, searchFields = searchFields, page = page, pageSize = pageSize, SearchConditionsList = searchConditionsList, SearchConditionsGroupType = searchConditionsGroupType, SearchSortFieldNamesList = searchSortFieldNamesList, SearchSortFieldNamesDescendingList = searchSortFieldNamesDescendingList, entityId = entityID, flatten = flatten)
	}


	listTempIncidentOffenseNameActionDetails <- function(searchConditionsList = NULL, TempIncidentOffenseNameActionDetailID = F, IncidentOffenseNameActionDetailID = F, FullName = F, InvolvementType = F, OffenseCodeDescription = F, IsPrimaryOffense = F, ActionCodeDescription = F, DurationToServe = F, DurationType = F, CreateAttendance = F, IsAlternate = F, LocationID = F, PartialDayPeriods = F, OldStatus = F, NewStatus = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, TempIncidentOffenseNameActionID = F, ScheduledTime = F, DurationServed = F, StaffIDFollowUpOfficer = F, Status = F, IsGuardianNotified = F, searchConditionsGroupType = "And", searchSortFieldNamesList = NULL, searchSortFieldNamesDescendingList = NULL, entityID = 1, schoolYearID = NULL, page = 1, pageSize = 100000, flatten = T, ...){

		params <- as.list(environment()) %>% append(list(...))

		searchFields <- params %>% keep(names(params) %>% str_sub(1,1) == names(params) %>% str_sub(1,1) %>% str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- "all", searchFields <- searchFields %>% keep(~.x) %>% names())

		listSkyObjects(module = "Discipline", objectName = "TempIncidentOffenseNameActionDetail", schoolYearID = schoolYearID, searchFields = searchFields, page = page, pageSize = pageSize, SearchConditionsList = searchConditionsList, SearchConditionsGroupType = searchConditionsGroupType, SearchSortFieldNamesList = searchSortFieldNamesList, SearchSortFieldNamesDescendingList = searchSortFieldNamesDescendingList, entityId = entityID, flatten = flatten)
	}


	listTempIncidentOffenseNameActions <- function(searchConditionsList = NULL, FullName = F, InvolvementType = F, OffenseCodeDescription = F, IsPrimaryOffense = F, ActionCodeDescription = F, DurationToServe = F, StaffIDAuthorizedByName = F, PerceivedMotivationCodeDescription = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, Comment = F, StudentNumber = F, TempIncidentOffenseNameActionID = F, EntityID = F, ActionTypeID = F, ActionTypeCode = F, ActionID = F, Status = F, OrderedDate = F, DurationType = F, StaffIDAuthorizedBy = F, StaffIDFollowUpOfficer = F, LocationID = F, BuildingID = F, RoomID = F, TempIncidentOffenseNameID = F, IsGuardianNotified = F, StartTime = F, InternalComment = F, searchConditionsGroupType = "And", searchSortFieldNamesList = NULL, searchSortFieldNamesDescendingList = NULL, entityID = 1, schoolYearID = NULL, page = 1, pageSize = 100000, flatten = T, ...){

		params <- as.list(environment()) %>% append(list(...))

		searchFields <- params %>% keep(names(params) %>% str_sub(1,1) == names(params) %>% str_sub(1,1) %>% str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- "all", searchFields <- searchFields %>% keep(~.x) %>% names())

		listSkyObjects(module = "Discipline", objectName = "TempIncidentOffenseNameAction", schoolYearID = schoolYearID, searchFields = searchFields, page = page, pageSize = pageSize, SearchConditionsList = searchConditionsList, SearchConditionsGroupType = searchConditionsGroupType, SearchSortFieldNamesList = searchSortFieldNamesList, SearchSortFieldNamesDescendingList = searchSortFieldNamesDescendingList, entityId = entityID, flatten = flatten)
	}


	listIncidents <- function(searchConditionsList = NULL, IncidentID = F, DistrictID = F, EntityID = F, SchoolYearID = F, IncidentNumber = F, DateTime = F, Type = F, LocationID = F, BuildingID = F, RoomID = F, Description = F, DamageCost = F, ActionIDRecommended = F, ReferredByType = F, ReferredByNameID = F, ReferredByFreeformName = F, ReferredByName = F, IncidentNumberValue = F, HasActions = F, HasActionsForOffenders = F, HasOpenActions = F, HasOverdueActionDetails = F, IsReferralOrWarning = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, IncidentMNID = F, StateDIRSTimeMNID = F, ReportedToLawEnforcement = F, NumberOfNonEnrolledOffenders = F, NumberOfNonEnrolledVictims = F, IsIncidentOrWarning = F, IsSuppressed = F, HasWeapons = F, HasDrugs = F, IsReadOnlyHistoricalRecord = F, DateBeforeLastEffectiveRun = F, searchConditionsGroupType = "And", searchSortFieldNamesList = NULL, searchSortFieldNamesDescendingList = NULL, entityID = 1, schoolYearID = NULL, page = 1, pageSize = 100000, flatten = T, ...){

		params <- as.list(environment()) %>% append(list(...))

		searchFields <- params %>% keep(names(params) %>% str_sub(1,1) == names(params) %>% str_sub(1,1) %>% str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- "all", searchFields <- searchFields %>% keep(~.x) %>% names())

		listSkyObjects(module = "Discipline", objectName = "Incident", schoolYearID = schoolYearID, searchFields = searchFields, page = page, pageSize = pageSize, SearchConditionsList = searchConditionsList, SearchConditionsGroupType = searchConditionsGroupType, SearchSortFieldNamesList = searchSortFieldNamesList, SearchSortFieldNamesDescendingList = searchSortFieldNamesDescendingList, entityId = entityID, flatten = flatten)
	}


	listIncidentOffenses <- function(searchConditionsList = NULL, IncidentOffenseID = F, IncidentID = F, OffenseID = F, IsPrimaryOffense = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, HasActions = F, HasWeapons = F, HasDrugs = F, IsReadOnlyHistoricalRecord = F, searchConditionsGroupType = "And", searchSortFieldNamesList = NULL, searchSortFieldNamesDescendingList = NULL, entityID = 1, schoolYearID = NULL, page = 1, pageSize = 100000, flatten = T, ...){

		params <- as.list(environment()) %>% append(list(...))

		searchFields <- params %>% keep(names(params) %>% str_sub(1,1) == names(params) %>% str_sub(1,1) %>% str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- "all", searchFields <- searchFields %>% keep(~.x) %>% names())

		listSkyObjects(module = "Discipline", objectName = "IncidentOffense", schoolYearID = schoolYearID, searchFields = searchFields, page = page, pageSize = pageSize, SearchConditionsList = searchConditionsList, SearchConditionsGroupType = searchConditionsGroupType, SearchSortFieldNamesList = searchSortFieldNamesList, SearchSortFieldNamesDescendingList = searchSortFieldNamesDescendingList, entityId = entityID, flatten = flatten)
	}


	listIncidentOffenseNames <- function(searchConditionsList = NULL, IncidentOffenseNameID = F, IncidentOffenseID = F, NameID = F, IncidentOffenseNameType = F, FreeformName = F, InvolvementType = F, StaffIDDisciplineOfficer = F, Statement = F, IsGuardianNotified = F, PerceivedMotivationID = F, OffenseLevelID = F, PersonalName = F, HasActions = F, HasOpenActions = F, HasOverdueActionDetails = F, IsStudentOffender = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, IncidentOffenseNameMNID = F, StateVictimTypeMNID = F, StateVictimCostMNID = F, InjuryOccured = F, WasSeriousBodilyInjury = F, AttachmentCount = F, HasWeapons = F, HasDrugs = F, ReportedToLawEnforcement = F, StateOffenderActivityMNID = F, HasDangerousWeapons = F, IsPhysicalAssault = F, IsPhysicalAssaultState = F, FirstDrugCodeforNorthEastExport = F, OffenderArrestedByLawEnforcement = F, IsReadOnlyHistoricalRecord = F, DisciplineThresholdID = F, InternalComment = F, MultipleVictimCount = F, EstimatedVictimsEnrolled = F, EstimatedVictimsNotEnrolled = F, CreatedBeforeLastEffectiveRunAndWasWithinDelay = F, searchConditionsGroupType = "And", searchSortFieldNamesList = NULL, searchSortFieldNamesDescendingList = NULL, entityID = 1, schoolYearID = NULL, page = 1, pageSize = 100000, flatten = T, ...){

		params <- as.list(environment()) %>% append(list(...))

		searchFields <- params %>% keep(names(params) %>% str_sub(1,1) == names(params) %>% str_sub(1,1) %>% str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- "all", searchFields <- searchFields %>% keep(~.x) %>% names())

		listSkyObjects(module = "Discipline", objectName = "IncidentOffenseName", schoolYearID = schoolYearID, searchFields = searchFields, page = page, pageSize = pageSize, SearchConditionsList = searchConditionsList, SearchConditionsGroupType = searchConditionsGroupType, SearchSortFieldNamesList = searchSortFieldNamesList, SearchSortFieldNamesDescendingList = searchSortFieldNamesDescendingList, entityId = entityID, flatten = flatten)
	}


	listIncidentOffenseNameActions <- function(searchConditionsList = NULL, IncidentOffenseNameActionID = F, IncidentOffenseNameID = F, ActionID = F, Status = F, OrderedDate = F, DurationToServe = F, EntityID = F, LocationID = F, BuildingID = F, RoomID = F, StaffIDAuthorizedBy = F, StaffIDFollowUpOfficer = F, IsGuardianNotified = F, Comment = F, DurationServedOverride = F, DurationServed = F, ActualDurationServed = F, TotalDurationServed = F, DurationToServeWithLabel = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, IncidentOffenseNameActionMNID = F, StateDIRSAESTypeMNID = F, DateExpulsionExclusionEnds = F, ReturnBeforeYearEnd = F, ExclusionThroughYearEnd = F, ExpulsionModified = F, ExpulsionThroughYearEnd = F, DIRSActionExplanation = F, FirstActionDetailDateNorthEastExport = F, LastActionDetailDateNorthEastExport = F, ActionTypeID = F, DurationType = F, StartTime = F, IsReadOnlyHistoricalRecord = F, InternalComment = F, NoServiceProvidedExplanation = F, MeetsLastRunDates = F, searchConditionsGroupType = "And", searchSortFieldNamesList = NULL, searchSortFieldNamesDescendingList = NULL, entityID = 1, schoolYearID = NULL, page = 1, pageSize = 100000, flatten = T, ...){

		params <- as.list(environment()) %>% append(list(...))

		searchFields <- params %>% keep(names(params) %>% str_sub(1,1) == names(params) %>% str_sub(1,1) %>% str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- "all", searchFields <- searchFields %>% keep(~.x) %>% names())

		listSkyObjects(module = "Discipline", objectName = "IncidentOffenseNameAction", schoolYearID = schoolYearID, searchFields = searchFields, page = page, pageSize = pageSize, SearchConditionsList = searchConditionsList, SearchConditionsGroupType = searchConditionsGroupType, SearchSortFieldNamesList = searchSortFieldNamesList, SearchSortFieldNamesDescendingList = searchSortFieldNamesDescendingList, entityId = entityID, flatten = flatten)
	}


	listIncidentOffenseNameActionDetails <- function(searchConditionsList = NULL, IncidentOffenseNameActionDetailID = F, IncidentOffenseNameActionID = F, Status = F, ScheduledTime = F, IsAlternate = F, DurationToServe = F, DurationServed = F, LocationID = F, BuildingID = F, RoomID = F, StaffIDFollowUpOfficer = F, IsGuardianNotified = F, Comment = F, LastAlternate = F, RenderReissueOption = F, DurationToServeWithLabel = F, DurationServedWithLabel = F, Overdue = F, PartialDayPeriods = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, IsReadOnlyHistoricalRecord = F, InternalComment = F, searchConditionsGroupType = "And", searchSortFieldNamesList = NULL, searchSortFieldNamesDescendingList = NULL, entityID = 1, schoolYearID = NULL, page = 1, pageSize = 100000, flatten = T, ...){

		params <- as.list(environment()) %>% append(list(...))

		searchFields <- params %>% keep(names(params) %>% str_sub(1,1) == names(params) %>% str_sub(1,1) %>% str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- "all", searchFields <- searchFields %>% keep(~.x) %>% names())

		listSkyObjects(module = "Discipline", objectName = "IncidentOffenseNameActionDetail", schoolYearID = schoolYearID, searchFields = searchFields, page = page, pageSize = pageSize, SearchConditionsList = searchConditionsList, SearchConditionsGroupType = searchConditionsGroupType, SearchSortFieldNamesList = searchSortFieldNamesList, SearchSortFieldNamesDescendingList = searchSortFieldNamesDescendingList, entityId = entityID, flatten = flatten)
	}


	listNextIncidentNumbers <- function(searchConditionsList = NULL, NextIncidentNumberID = F, DistrictID = F, SchoolYearID = F, SequenceNumber = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, searchConditionsGroupType = "And", searchSortFieldNamesList = NULL, searchSortFieldNamesDescendingList = NULL, entityID = 1, schoolYearID = NULL, page = 1, pageSize = 100000, flatten = T, ...){

		params <- as.list(environment()) %>% append(list(...))

		searchFields <- params %>% keep(names(params) %>% str_sub(1,1) == names(params) %>% str_sub(1,1) %>% str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- "all", searchFields <- searchFields %>% keep(~.x) %>% names())

		listSkyObjects(module = "Discipline", objectName = "NextIncidentNumber", schoolYearID = schoolYearID, searchFields = searchFields, page = page, pageSize = pageSize, SearchConditionsList = searchConditionsList, SearchConditionsGroupType = searchConditionsGroupType, SearchSortFieldNamesList = searchSortFieldNamesList, SearchSortFieldNamesDescendingList = searchSortFieldNamesDescendingList, entityId = entityID, flatten = flatten)
	}


	listTempIncidentOffenseNameActionDetailRecords <- function(searchConditionsList = NULL, TempIncidentOffenseNameActionDetailRecordID = F, IncidentOffenseNameActionID = F, StatusCode = F, Status = F, ScheduledTime = F, IsAlternate = F, DurationToServe = F, DurationServed = F, DurationType = F, LocationID = F, Location = F, BuildingID = F, Building = F, RoomID = F, RoomNumber = F, StaffIDFollowUpOfficer = F, StaffFollowUpOfficerName = F, IsGuardianNotified = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, searchConditionsGroupType = "And", searchSortFieldNamesList = NULL, searchSortFieldNamesDescendingList = NULL, entityID = 1, schoolYearID = NULL, page = 1, pageSize = 100000, flatten = T, ...){

		params <- as.list(environment()) %>% append(list(...))

		searchFields <- params %>% keep(names(params) %>% str_sub(1,1) == names(params) %>% str_sub(1,1) %>% str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- "all", searchFields <- searchFields %>% keep(~.x) %>% names())

		listSkyObjects(module = "Discipline", objectName = "TempIncidentOffenseNameActionDetailRecord", schoolYearID = schoolYearID, searchFields = searchFields, page = page, pageSize = pageSize, SearchConditionsList = searchConditionsList, SearchConditionsGroupType = searchConditionsGroupType, SearchSortFieldNamesList = searchSortFieldNamesList, SearchSortFieldNamesDescendingList = searchSortFieldNamesDescendingList, entityId = entityID, flatten = flatten)
	}


	listDisciplineActions <- function(searchConditionsList = NULL, ActionID = F, DistrictID = F, SchoolYearID = F, Code = F, Description = F, DurationType = F, DefaultDuration = F, DefaultLocationID = F, SuppressCreationOfActionDetails = F, CodeDescription = F, CreateAttendanceForActionDetail = F, ActionIDClonedFrom = F, FederalDisciplineCategoryID = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, ActionMNID = F, StateActionTypeMNID = F, ActionTypeID = F, RestraintSeclusion = F, TransferToAlternativeSchool = F, IsReadOnlyHistoricalRecord = F, searchConditionsGroupType = "And", searchSortFieldNamesList = NULL, searchSortFieldNamesDescendingList = NULL, entityID = 1, schoolYearID = NULL, page = 1, pageSize = 100000, flatten = T, ...){

		params <- as.list(environment()) %>% append(list(...))

		searchFields <- params %>% keep(names(params) %>% str_sub(1,1) == names(params) %>% str_sub(1,1) %>% str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- "all", searchFields <- searchFields %>% keep(~.x) %>% names())

		listSkyObjects(module = "Discipline", objectName = "Action", schoolYearID = schoolYearID, searchFields = searchFields, page = page, pageSize = pageSize, SearchConditionsList = searchConditionsList, SearchConditionsGroupType = searchConditionsGroupType, SearchSortFieldNamesList = searchSortFieldNamesList, SearchSortFieldNamesDescendingList = searchSortFieldNamesDescendingList, entityId = entityID, flatten = flatten)
	}


	listDisciplineConfigDistrictYears <- function(searchConditionsList = NULL, ConfigDistrictYearID = F, DistrictID = F, SchoolYearID = F, RestartIncidentNumberThisYear = F, AllowOnlyOneOffensePerIncident = F, DefaultDisciplineScreenDateAndTimes = F, DefaultOffenseValueFromPreviousPerson = F, DefaultActionValueFromPreviousPerson = F, UsePerceivedMotivation = F, UseIncidentBuildingAndRoom = F, ConfigDistrictYearIDClonedFrom = F, AllowUseOfWarning = F, AllowActionRecommendationsOnReferrals = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, DisplayWarningsInFamilyAndStudentAccess = F, DaysToDelayDisplayOfIncidentsInFamilyAndStudentAccess = F, AllowActionTypeUpdate = F, DisplayStudentOffensesForAllEntities = F, DisplayInvolvedPersonsFromAllEntities = F, AllowDurationTypeUpdate = F, DefaultActionStatus = F, DefaultGuardianNotifiedOnActionDetailFromAction = F, IsReadOnlyHistoricalRecord = F, AllowInternalComments = F, searchConditionsGroupType = "And", searchSortFieldNamesList = NULL, searchSortFieldNamesDescendingList = NULL, entityID = 1, schoolYearID = NULL, page = 1, pageSize = 100000, flatten = T, ...){

		params <- as.list(environment()) %>% append(list(...))

		searchFields <- params %>% keep(names(params) %>% str_sub(1,1) == names(params) %>% str_sub(1,1) %>% str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- "all", searchFields <- searchFields %>% keep(~.x) %>% names())

		listSkyObjects(module = "Discipline", objectName = "ConfigDistrictYear", schoolYearID = schoolYearID, searchFields = searchFields, page = page, pageSize = pageSize, SearchConditionsList = searchConditionsList, SearchConditionsGroupType = searchConditionsGroupType, SearchSortFieldNamesList = searchSortFieldNamesList, SearchSortFieldNamesDescendingList = searchSortFieldNamesDescendingList, entityId = entityID, flatten = flatten)
	}


	listPerceivedMotivations <- function(searchConditionsList = NULL, PerceivedMotivationID = F, DistrictID = F, SchoolYearID = F, Code = F, Description = F, CodeDescription = F, PerceivedMotivationIDClonedFrom = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, IsReadOnlyHistoricalRecord = F, searchConditionsGroupType = "And", searchSortFieldNamesList = NULL, searchSortFieldNamesDescendingList = NULL, entityID = 1, schoolYearID = NULL, page = 1, pageSize = 100000, flatten = T, ...){

		params <- as.list(environment()) %>% append(list(...))

		searchFields <- params %>% keep(names(params) %>% str_sub(1,1) == names(params) %>% str_sub(1,1) %>% str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- "all", searchFields <- searchFields %>% keep(~.x) %>% names())

		listSkyObjects(module = "Discipline", objectName = "PerceivedMotivation", schoolYearID = schoolYearID, searchFields = searchFields, page = page, pageSize = pageSize, SearchConditionsList = searchConditionsList, SearchConditionsGroupType = searchConditionsGroupType, SearchSortFieldNamesList = searchSortFieldNamesList, SearchSortFieldNamesDescendingList = searchSortFieldNamesDescendingList, entityId = entityID, flatten = flatten)
	}


	listDisciplineLocations <- function(searchConditionsList = NULL, LocationID = F, DistrictID = F, Code = F, Description = F, EdFiIncidentLocationID = F, CodeDescription = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, LocationMNID = F, StateIncidentLocationMNID = F, searchConditionsGroupType = "And", searchSortFieldNamesList = NULL, searchSortFieldNamesDescendingList = NULL, entityID = 1, schoolYearID = NULL, page = 1, pageSize = 100000, flatten = T, ...){

		params <- as.list(environment()) %>% append(list(...))

		searchFields <- params %>% keep(names(params) %>% str_sub(1,1) == names(params) %>% str_sub(1,1) %>% str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- "all", searchFields <- searchFields %>% keep(~.x) %>% names())

		listSkyObjects(module = "Discipline", objectName = "Location", schoolYearID = schoolYearID, searchFields = searchFields, page = page, pageSize = pageSize, SearchConditionsList = searchConditionsList, SearchConditionsGroupType = searchConditionsGroupType, SearchSortFieldNamesList = searchSortFieldNamesList, SearchSortFieldNamesDescendingList = searchSortFieldNamesDescendingList, entityId = entityID, flatten = flatten)
	}


	listOffenses <- function(searchConditionsList = NULL, OffenseID = F, DistrictID = F, SchoolYearID = F, Code = F, Description = F, RestrictActions = F, DefaultActionID = F, UseForReferral = F, AllowActionRecommendations = F, OffenseLevelIDDefault = F, CodeDescription = F, OffenseIDClonedFrom = F, FederalOffenseCategoryID = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, IsDrugRelated = F, IsWeaponRelated = F, IsInjuryThreat = F, HarassmentBullying = F, IsReadOnlyHistoricalRecord = F, searchConditionsGroupType = "And", searchSortFieldNamesList = NULL, searchSortFieldNamesDescendingList = NULL, entityID = 1, schoolYearID = NULL, page = 1, pageSize = 100000, flatten = T, ...){

		params <- as.list(environment()) %>% append(list(...))

		searchFields <- params %>% keep(names(params) %>% str_sub(1,1) == names(params) %>% str_sub(1,1) %>% str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- "all", searchFields <- searchFields %>% keep(~.x) %>% names())

		listSkyObjects(module = "Discipline", objectName = "Offense", schoolYearID = schoolYearID, searchFields = searchFields, page = page, pageSize = pageSize, SearchConditionsList = searchConditionsList, SearchConditionsGroupType = searchConditionsGroupType, SearchSortFieldNamesList = searchSortFieldNamesList, SearchSortFieldNamesDescendingList = searchSortFieldNamesDescendingList, entityId = entityID, flatten = flatten)
	}


	listOffenseActions <- function(searchConditionsList = NULL, OffenseActionID = F, OffenseID = F, ActionID = F, OffenseActionIDClonedFrom = F, IsReferralAction = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, IsReadOnlyHistoricalRecord = F, searchConditionsGroupType = "And", searchSortFieldNamesList = NULL, searchSortFieldNamesDescendingList = NULL, entityID = 1, schoolYearID = NULL, page = 1, pageSize = 100000, flatten = T, ...){

		params <- as.list(environment()) %>% append(list(...))

		searchFields <- params %>% keep(names(params) %>% str_sub(1,1) == names(params) %>% str_sub(1,1) %>% str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- "all", searchFields <- searchFields %>% keep(~.x) %>% names())

		listSkyObjects(module = "Discipline", objectName = "OffenseAction", schoolYearID = schoolYearID, searchFields = searchFields, page = page, pageSize = pageSize, SearchConditionsList = searchConditionsList, SearchConditionsGroupType = searchConditionsGroupType, SearchSortFieldNamesList = searchSortFieldNamesList, SearchSortFieldNamesDescendingList = searchSortFieldNamesDescendingList, entityId = entityID, flatten = flatten)
	}


	listOffenseLevels <- function(searchConditionsList = NULL, OffenseLevelID = F, DistrictID = F, SchoolYearID = F, Code = F, Description = F, CodeDescription = F, OffenseLevelIDClonedFrom = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, IsReadOnlyHistoricalRecord = F, searchConditionsGroupType = "And", searchSortFieldNamesList = NULL, searchSortFieldNamesDescendingList = NULL, entityID = 1, schoolYearID = NULL, page = 1, pageSize = 100000, flatten = T, ...){

		params <- as.list(environment()) %>% append(list(...))

		searchFields <- params %>% keep(names(params) %>% str_sub(1,1) == names(params) %>% str_sub(1,1) %>% str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- "all", searchFields <- searchFields %>% keep(~.x) %>% names())

		listSkyObjects(module = "Discipline", objectName = "OffenseLevel", schoolYearID = schoolYearID, searchFields = searchFields, page = page, pageSize = pageSize, SearchConditionsList = searchConditionsList, SearchConditionsGroupType = searchConditionsGroupType, SearchSortFieldNamesList = searchSortFieldNamesList, SearchSortFieldNamesDescendingList = searchSortFieldNamesDescendingList, entityId = entityID, flatten = flatten)
	}


	listDrugs <- function(searchConditionsList = NULL, DrugMNID = F, StateDrugTypeMNID = F, DrugID = F, DistrictID = F, SchoolYearID = F, Code = F, Description = F, DrugIDClonedFrom = F, CodeDescription = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, IsReadOnlyHistoricalRecord = F, searchConditionsGroupType = "And", searchSortFieldNamesList = NULL, searchSortFieldNamesDescendingList = NULL, entityID = 1, schoolYearID = NULL, page = 1, pageSize = 100000, flatten = T, ...){

		params <- as.list(environment()) %>% append(list(...))

		searchFields <- params %>% keep(names(params) %>% str_sub(1,1) == names(params) %>% str_sub(1,1) %>% str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- "all", searchFields <- searchFields %>% keep(~.x) %>% names())

		listSkyObjects(module = "Discipline", objectName = "Drug", schoolYearID = schoolYearID, searchFields = searchFields, page = page, pageSize = pageSize, SearchConditionsList = searchConditionsList, SearchConditionsGroupType = searchConditionsGroupType, SearchSortFieldNamesList = searchSortFieldNamesList, SearchSortFieldNamesDescendingList = searchSortFieldNamesDescendingList, entityId = entityID, flatten = flatten)
	}


	listIncidentOffenseNameDrugs <- function(searchConditionsList = NULL, IncidentOffenseNameDrugID = F, IncidentOffenseNameID = F, DrugID = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, IsReadOnlyHistoricalRecord = F, searchConditionsGroupType = "And", searchSortFieldNamesList = NULL, searchSortFieldNamesDescendingList = NULL, entityID = 1, schoolYearID = NULL, page = 1, pageSize = 100000, flatten = T, ...){

		params <- as.list(environment()) %>% append(list(...))

		searchFields <- params %>% keep(names(params) %>% str_sub(1,1) == names(params) %>% str_sub(1,1) %>% str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- "all", searchFields <- searchFields %>% keep(~.x) %>% names())

		listSkyObjects(module = "Discipline", objectName = "IncidentOffenseNameDrug", schoolYearID = schoolYearID, searchFields = searchFields, page = page, pageSize = pageSize, SearchConditionsList = searchConditionsList, SearchConditionsGroupType = searchConditionsGroupType, SearchSortFieldNamesList = searchSortFieldNamesList, SearchSortFieldNamesDescendingList = searchSortFieldNamesDescendingList, entityId = entityID, flatten = flatten)
	}


	listIncidentOffenseNameWeapons <- function(searchConditionsList = NULL, IncidentOffenseNameWeaponID = F, IncidentOffenseNameID = F, WeaponID = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, IncidentOffenseNameWeaponMNID = F, MeetsFederalStatuteOfDangerousWeapon = F, MeetsStateStatuteOfDangerousWeapon = F, GunWasLoaded = F, GunWasInCase = F, GunFoundInTrunk = F, WeaponCount = F, IsReadOnlyHistoricalRecord = F, searchConditionsGroupType = "And", searchSortFieldNamesList = NULL, searchSortFieldNamesDescendingList = NULL, entityID = 1, schoolYearID = NULL, page = 1, pageSize = 100000, flatten = T, ...){

		params <- as.list(environment()) %>% append(list(...))

		searchFields <- params %>% keep(names(params) %>% str_sub(1,1) == names(params) %>% str_sub(1,1) %>% str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- "all", searchFields <- searchFields %>% keep(~.x) %>% names())

		listSkyObjects(module = "Discipline", objectName = "IncidentOffenseNameWeapon", schoolYearID = schoolYearID, searchFields = searchFields, page = page, pageSize = pageSize, SearchConditionsList = searchConditionsList, SearchConditionsGroupType = searchConditionsGroupType, SearchSortFieldNamesList = searchSortFieldNamesList, SearchSortFieldNamesDescendingList = searchSortFieldNamesDescendingList, entityId = entityID, flatten = flatten)
	}


	listWeapons <- function(searchConditionsList = NULL, WeaponMNID = F, StateWeaponTypeMNID = F, StatePelletGunTypeMNID = F, WeaponID = F, DistrictID = F, SchoolYearID = F, Code = F, Description = F, WeaponIDClonedFrom = F, CodeDescription = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, IsReadOnlyHistoricalRecord = F, searchConditionsGroupType = "And", searchSortFieldNamesList = NULL, searchSortFieldNamesDescendingList = NULL, entityID = 1, schoolYearID = NULL, page = 1, pageSize = 100000, flatten = T, ...){

		params <- as.list(environment()) %>% append(list(...))

		searchFields <- params %>% keep(names(params) %>% str_sub(1,1) == names(params) %>% str_sub(1,1) %>% str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- "all", searchFields <- searchFields %>% keep(~.x) %>% names())

		listSkyObjects(module = "Discipline", objectName = "Weapon", schoolYearID = schoolYearID, searchFields = searchFields, page = page, pageSize = pageSize, SearchConditionsList = searchConditionsList, SearchConditionsGroupType = searchConditionsGroupType, SearchSortFieldNamesList = searchSortFieldNamesList, SearchSortFieldNamesDescendingList = searchSortFieldNamesDescendingList, entityId = entityID, flatten = flatten)
	}


	listActionTypes <- function(searchConditionsList = NULL, ActionTypeID = F, Code = F, Description = F, CodeDescription = F, ValidYearLow = F, ValidYearHigh = F, IsInvalid = F, SkywardID = F, SkywardHash = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, IsReadOnlyHistoricalRecord = F, EdFiDisciplineDescriptorID = F, searchConditionsGroupType = "And", searchSortFieldNamesList = NULL, searchSortFieldNamesDescendingList = NULL, entityID = 1, schoolYearID = NULL, page = 1, pageSize = 100000, flatten = T, ...){

		params <- as.list(environment()) %>% append(list(...))

		searchFields <- params %>% keep(names(params) %>% str_sub(1,1) == names(params) %>% str_sub(1,1) %>% str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- "all", searchFields <- searchFields %>% keep(~.x) %>% names())

		listSkyObjects(module = "Discipline", objectName = "ActionType", schoolYearID = schoolYearID, searchFields = searchFields, page = page, pageSize = pageSize, SearchConditionsList = searchConditionsList, SearchConditionsGroupType = searchConditionsGroupType, SearchSortFieldNamesList = searchSortFieldNamesList, SearchSortFieldNamesDescendingList = searchSortFieldNamesDescendingList, entityId = entityID, flatten = flatten)
	}


	listDisciplineConfigSystems <- function(searchConditionsList = NULL, ConfigSystemID = F, AllowIncidentSuppression = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, ReportIDDisciplineLetter = F, AllowUpdateHistoricalData = F, searchConditionsGroupType = "And", searchSortFieldNamesList = NULL, searchSortFieldNamesDescendingList = NULL, entityID = 1, schoolYearID = NULL, page = 1, pageSize = 100000, flatten = T, ...){

		params <- as.list(environment()) %>% append(list(...))

		searchFields <- params %>% keep(names(params) %>% str_sub(1,1) == names(params) %>% str_sub(1,1) %>% str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- "all", searchFields <- searchFields %>% keep(~.x) %>% names())

		listSkyObjects(module = "Discipline", objectName = "ConfigSystem", schoolYearID = schoolYearID, searchFields = searchFields, page = page, pageSize = pageSize, SearchConditionsList = searchConditionsList, SearchConditionsGroupType = searchConditionsGroupType, SearchSortFieldNamesList = searchSortFieldNamesList, SearchSortFieldNamesDescendingList = searchSortFieldNamesDescendingList, entityId = entityID, flatten = flatten)
	}


	listDisciplineLetterTemplates <- function(searchConditionsList = NULL, DisciplineLetterTemplateID = F, Description = F, Header = F, Body = F, Footer = F, DistrictID = F, SchoolYearID = F, DisciplineLetterTemplateIDClonedFrom = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, MediaIDLogo = F, GuardianNameFormat = F, StudentNameFormat = F, AdvisorNameFormat = F, SuperintendentNameFormat = F, ColumnHeaderLabel1 = F, ColumnHeaderLabel2 = F, ColumnHeaderLabel3 = F, ColumnHeaderLabel4 = F, ColumnHeaderLabel5 = F, ColumnHeaderLabel6 = F, ColumnHeaderLabel7 = F, ColumnHeaderLabel8 = F, ColumnHeaderLabel9 = F, ColumnHeaderLabel10 = F, IsDefault = F, ForCurrentEntity = F, IsReadOnlyHistoricalRecord = F, searchConditionsGroupType = "And", searchSortFieldNamesList = NULL, searchSortFieldNamesDescendingList = NULL, entityID = 1, schoolYearID = NULL, page = 1, pageSize = 100000, flatten = T, ...){

		params <- as.list(environment()) %>% append(list(...))

		searchFields <- params %>% keep(names(params) %>% str_sub(1,1) == names(params) %>% str_sub(1,1) %>% str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- "all", searchFields <- searchFields %>% keep(~.x) %>% names())

		listSkyObjects(module = "Discipline", objectName = "DisciplineLetterTemplate", schoolYearID = schoolYearID, searchFields = searchFields, page = page, pageSize = pageSize, SearchConditionsList = searchConditionsList, SearchConditionsGroupType = searchConditionsGroupType, SearchSortFieldNamesList = searchSortFieldNamesList, SearchSortFieldNamesDescendingList = searchSortFieldNamesDescendingList, entityId = entityID, flatten = flatten)
	}


	listDisciplineLetterRunHistoryActions <- function(searchConditionsList = NULL, DisciplineLetterRunHistoryActionID = F, ActionID = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, DisciplineLetterRunHistoryID = F, searchConditionsGroupType = "And", searchSortFieldNamesList = NULL, searchSortFieldNamesDescendingList = NULL, entityID = 1, schoolYearID = NULL, page = 1, pageSize = 100000, flatten = T, ...){

		params <- as.list(environment()) %>% append(list(...))

		searchFields <- params %>% keep(names(params) %>% str_sub(1,1) == names(params) %>% str_sub(1,1) %>% str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- "all", searchFields <- searchFields %>% keep(~.x) %>% names())

		listSkyObjects(module = "Discipline", objectName = "DisciplineLetterRunHistoryAction", schoolYearID = schoolYearID, searchFields = searchFields, page = page, pageSize = pageSize, SearchConditionsList = searchConditionsList, SearchConditionsGroupType = searchConditionsGroupType, SearchSortFieldNamesList = searchSortFieldNamesList, SearchSortFieldNamesDescendingList = searchSortFieldNamesDescendingList, entityId = entityID, flatten = flatten)
	}


	listDisciplineLetterTemplateEntities <- function(searchConditionsList = NULL, DisciplineLetterTemplateEntityID = F, DisciplineLetterTemplateID = F, EntityID = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, searchConditionsGroupType = "And", searchSortFieldNamesList = NULL, searchSortFieldNamesDescendingList = NULL, entityID = 1, schoolYearID = NULL, page = 1, pageSize = 100000, flatten = T, ...){

		params <- as.list(environment()) %>% append(list(...))

		searchFields <- params %>% keep(names(params) %>% str_sub(1,1) == names(params) %>% str_sub(1,1) %>% str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- "all", searchFields <- searchFields %>% keep(~.x) %>% names())

		listSkyObjects(module = "Discipline", objectName = "DisciplineLetterTemplateEntity", schoolYearID = schoolYearID, searchFields = searchFields, page = page, pageSize = pageSize, SearchConditionsList = searchConditionsList, SearchConditionsGroupType = searchConditionsGroupType, SearchSortFieldNamesList = searchSortFieldNamesList, SearchSortFieldNamesDescendingList = searchSortFieldNamesDescendingList, entityId = entityID, flatten = flatten)
	}


	listDisciplineLetterRunHistoryOffenses <- function(searchConditionsList = NULL, DisciplineLetterRunHistoryOffenseID = F, DisciplineLetterRunHistoryID = F, OffenseID = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, searchConditionsGroupType = "And", searchSortFieldNamesList = NULL, searchSortFieldNamesDescendingList = NULL, entityID = 1, schoolYearID = NULL, page = 1, pageSize = 100000, flatten = T, ...){

		params <- as.list(environment()) %>% append(list(...))

		searchFields <- params %>% keep(names(params) %>% str_sub(1,1) == names(params) %>% str_sub(1,1) %>% str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- "all", searchFields <- searchFields %>% keep(~.x) %>% names())

		listSkyObjects(module = "Discipline", objectName = "DisciplineLetterRunHistoryOffense", schoolYearID = schoolYearID, searchFields = searchFields, page = page, pageSize = pageSize, SearchConditionsList = searchConditionsList, SearchConditionsGroupType = searchConditionsGroupType, SearchSortFieldNamesList = searchSortFieldNamesList, SearchSortFieldNamesDescendingList = searchSortFieldNamesDescendingList, entityId = entityID, flatten = flatten)
	}


	listIncidentOffenseNameReportRunHistories <- function(searchConditionsList = NULL, IncidentOffenseNameReportRunHistoryID = F, DisciplineLetterRunHistoryID = F, IncidentOffenseNameID = F, StudentID = F, IsActive = F, AttachmentID = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, ColumnHeaderData1 = F, ColumnHeaderData2 = F, ColumnHeaderData3 = F, ColumnHeaderData4 = F, ColumnHeaderData5 = F, ColumnHeaderData6 = F, ColumnHeaderData7 = F, ColumnHeaderData8 = F, ColumnHeaderData9 = F, ColumnHeaderData10 = F, UnboundHeader = F, UnboundBody = F, UnboundFooter = F, IsReadOnlyHistoricalRecord = F, searchConditionsGroupType = "And", searchSortFieldNamesList = NULL, searchSortFieldNamesDescendingList = NULL, entityID = 1, schoolYearID = NULL, page = 1, pageSize = 100000, flatten = T, ...){

		params <- as.list(environment()) %>% append(list(...))

		searchFields <- params %>% keep(names(params) %>% str_sub(1,1) == names(params) %>% str_sub(1,1) %>% str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- "all", searchFields <- searchFields %>% keep(~.x) %>% names())

		listSkyObjects(module = "Discipline", objectName = "IncidentOffenseNameReportRunHistory", schoolYearID = schoolYearID, searchFields = searchFields, page = page, pageSize = pageSize, SearchConditionsList = searchConditionsList, SearchConditionsGroupType = searchConditionsGroupType, SearchSortFieldNamesList = searchSortFieldNamesList, SearchSortFieldNamesDescendingList = searchSortFieldNamesDescendingList, entityId = entityID, flatten = flatten)
	}


	listDisciplineLetterTemplateHeaderColumns <- function(searchConditionsList = NULL, DisciplineLetterTemplateHeaderColumnID = F, DisciplineLetterTemplateHeaderRowID = F, SortNumber = F, FieldType = F, LabelOverride = F, FreeformText = F, DisciplineLetterTemplateHeaderColumnIDClonedFrom = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, ColumnNumber = F, searchConditionsGroupType = "And", searchSortFieldNamesList = NULL, searchSortFieldNamesDescendingList = NULL, entityID = 1, schoolYearID = NULL, page = 1, pageSize = 100000, flatten = T, ...){

		params <- as.list(environment()) %>% append(list(...))

		searchFields <- params %>% keep(names(params) %>% str_sub(1,1) == names(params) %>% str_sub(1,1) %>% str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- "all", searchFields <- searchFields %>% keep(~.x) %>% names())

		listSkyObjects(module = "Discipline", objectName = "DisciplineLetterTemplateHeaderColumn", schoolYearID = schoolYearID, searchFields = searchFields, page = page, pageSize = pageSize, SearchConditionsList = searchConditionsList, SearchConditionsGroupType = searchConditionsGroupType, SearchSortFieldNamesList = searchSortFieldNamesList, SearchSortFieldNamesDescendingList = searchSortFieldNamesDescendingList, entityId = entityID, flatten = flatten)
	}


	listDisciplineLetterRunHistories <- function(searchConditionsList = NULL, DisciplineLetterRunHistoryID = F, DisciplineLetterTemplateID = F, EntityID = F, RunDescription = F, ParameterData = F, ParameterDescription = F, Date = F, FilterType = F, IncidentType = F, GracePeriod = F, IsActive = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, StartDate = F, EndDate = F, ReportRunInfoID = F, SchoolYearID = F, FiscalYearID = F, CachedFiscalYear = F, SectionID = F, EntityIDList = F, SchoolYearNumericYearOrCurrent = F, CachedSchoolYear = F, CachedEntity = F, IsReadOnlyHistoricalRecord = F, PostToFASA = F, AttachmentDisplayName = F, searchConditionsGroupType = "And", searchSortFieldNamesList = NULL, searchSortFieldNamesDescendingList = NULL, entityID = 1, schoolYearID = NULL, page = 1, pageSize = 100000, flatten = T, ...){

		params <- as.list(environment()) %>% append(list(...))

		searchFields <- params %>% keep(names(params) %>% str_sub(1,1) == names(params) %>% str_sub(1,1) %>% str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- "all", searchFields <- searchFields %>% keep(~.x) %>% names())

		listSkyObjects(module = "Discipline", objectName = "DisciplineLetterRunHistory", schoolYearID = schoolYearID, searchFields = searchFields, page = page, pageSize = pageSize, SearchConditionsList = searchConditionsList, SearchConditionsGroupType = searchConditionsGroupType, SearchSortFieldNamesList = searchSortFieldNamesList, SearchSortFieldNamesDescendingList = searchSortFieldNamesDescendingList, entityId = entityID, flatten = flatten)
	}


	listDisciplineLetterTemplateHeaderRows <- function(searchConditionsList = NULL, DisciplineLetterTemplateHeaderRowID = F, DisciplineLetterTemplateID = F, SortNumber = F, ColumnCount = F, DisciplineLetterTemplateHeaderRowIDClonedFrom = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, searchConditionsGroupType = "And", searchSortFieldNamesList = NULL, searchSortFieldNamesDescendingList = NULL, entityID = 1, schoolYearID = NULL, page = 1, pageSize = 100000, flatten = T, ...){

		params <- as.list(environment()) %>% append(list(...))

		searchFields <- params %>% keep(names(params) %>% str_sub(1,1) == names(params) %>% str_sub(1,1) %>% str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- "all", searchFields <- searchFields %>% keep(~.x) %>% names())

		listSkyObjects(module = "Discipline", objectName = "DisciplineLetterTemplateHeaderRow", schoolYearID = schoolYearID, searchFields = searchFields, page = page, pageSize = pageSize, SearchConditionsList = searchConditionsList, SearchConditionsGroupType = searchConditionsGroupType, SearchSortFieldNamesList = searchSortFieldNamesList, SearchSortFieldNamesDescendingList = searchSortFieldNamesDescendingList, entityId = entityID, flatten = flatten)
	}


	listTempIncidentOffenseNameReportRunHistoryRecords <- function(searchConditionsList = NULL, TempIncidentOffenseNameReportRunHistoryRecordID = F, StudentID = F, StudentName = F, IncidentOffenseNameID = F, DateTime = F, Offense = F, Incident = F, IncidentNumber = F, ColumnHeader1 = F, ColumnHeader2 = F, ColumnHeader3 = F, ColumnHeader4 = F, ColumnHeader5 = F, ColumnHeader6 = F, ColumnHeader7 = F, ColumnHeader8 = F, ColumnHeader9 = F, ColumnHeader10 = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, UnboundHeader = F, UnboundBody = F, UnboundFooter = F, searchConditionsGroupType = "And", searchSortFieldNamesList = NULL, searchSortFieldNamesDescendingList = NULL, entityID = 1, schoolYearID = NULL, page = 1, pageSize = 100000, flatten = T, ...){

		params <- as.list(environment()) %>% append(list(...))

		searchFields <- params %>% keep(names(params) %>% str_sub(1,1) == names(params) %>% str_sub(1,1) %>% str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- "all", searchFields <- searchFields %>% keep(~.x) %>% names())

		listSkyObjects(module = "Discipline", objectName = "TempIncidentOffenseNameReportRunHistoryRecord", schoolYearID = schoolYearID, searchFields = searchFields, page = page, pageSize = pageSize, SearchConditionsList = searchConditionsList, SearchConditionsGroupType = searchConditionsGroupType, SearchSortFieldNamesList = searchSortFieldNamesList, SearchSortFieldNamesDescendingList = searchSortFieldNamesDescendingList, entityId = entityID, flatten = flatten)
	}


	listWhiteListFieldPaths <- function(searchConditionsList = NULL, WhiteListFieldPathID = F, FriendlyName = F, Description = F, FieldPath = F, SkywardID = F, SkywardHash = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, UsedForType = F, searchConditionsGroupType = "And", searchSortFieldNamesList = NULL, searchSortFieldNamesDescendingList = NULL, entityID = 1, schoolYearID = NULL, page = 1, pageSize = 100000, flatten = T, ...){

		params <- as.list(environment()) %>% append(list(...))

		searchFields <- params %>% keep(names(params) %>% str_sub(1,1) == names(params) %>% str_sub(1,1) %>% str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- "all", searchFields <- searchFields %>% keep(~.x) %>% names())

		listSkyObjects(module = "Discipline", objectName = "WhiteListFieldPath", schoolYearID = schoolYearID, searchFields = searchFields, page = page, pageSize = pageSize, SearchConditionsList = searchConditionsList, SearchConditionsGroupType = searchConditionsGroupType, SearchSortFieldNamesList = searchSortFieldNamesList, SearchSortFieldNamesDescendingList = searchSortFieldNamesDescendingList, entityId = entityID, flatten = flatten)
	}


	listDisciplineConfigEntityYears <- function(searchConditionsList = NULL, ConfigEntityYearID = F, EntityID = F, SchoolYearID = F, ConfigEntityYearIDClonedFrom = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, IsReadOnlyHistoricalRecord = F, searchConditionsGroupType = "And", searchSortFieldNamesList = NULL, searchSortFieldNamesDescendingList = NULL, entityID = 1, schoolYearID = NULL, page = 1, pageSize = 100000, flatten = T, ...){

		params <- as.list(environment()) %>% append(list(...))

		searchFields <- params %>% keep(names(params) %>% str_sub(1,1) == names(params) %>% str_sub(1,1) %>% str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- "all", searchFields <- searchFields %>% keep(~.x) %>% names())

		listSkyObjects(module = "Discipline", objectName = "ConfigEntityYear", schoolYearID = schoolYearID, searchFields = searchFields, page = page, pageSize = pageSize, SearchConditionsList = searchConditionsList, SearchConditionsGroupType = searchConditionsGroupType, SearchSortFieldNamesList = searchSortFieldNamesList, SearchSortFieldNamesDescendingList = searchSortFieldNamesDescendingList, entityId = entityID, flatten = flatten)
	}


	listTempIncidentOffenseNames <- function(searchConditionsList = NULL, TempIncidentOffenseNameID = F, TempIncidentInvolvedPersonID = F, TempIncidentOffenseID = F, OffenseID = F, ExistingIncidentOffenseNameID = F, InvolvementType = F, PerceivedMotivationID = F, OffenseLevelID = F, FullName = F, StudentNumber = F, OffenseCodeDescription = F, IsPrimaryOffense = F, PerceivedMotivationCodeDescription = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, InternalComment = F, searchConditionsGroupType = "And", searchSortFieldNamesList = NULL, searchSortFieldNamesDescendingList = NULL, entityID = 1, schoolYearID = NULL, page = 1, pageSize = 100000, flatten = T, ...){

		params <- as.list(environment()) %>% append(list(...))

		searchFields <- params %>% keep(names(params) %>% str_sub(1,1) == names(params) %>% str_sub(1,1) %>% str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- "all", searchFields <- searchFields %>% keep(~.x) %>% names())

		listSkyObjects(module = "Discipline", objectName = "TempIncidentOffenseName", schoolYearID = schoolYearID, searchFields = searchFields, page = page, pageSize = pageSize, SearchConditionsList = searchConditionsList, SearchConditionsGroupType = searchConditionsGroupType, SearchSortFieldNamesList = searchSortFieldNamesList, SearchSortFieldNamesDescendingList = searchSortFieldNamesDescendingList, entityId = entityID, flatten = flatten)
	}


	listTempIncidentOffenses <- function(searchConditionsList = NULL, TempIncidentOffenseID = F, ExistingIncidentID = F, OffenseID = F, IsPrimaryOffense = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, searchConditionsGroupType = "And", searchSortFieldNamesList = NULL, searchSortFieldNamesDescendingList = NULL, entityID = 1, schoolYearID = NULL, page = 1, pageSize = 100000, flatten = T, ...){

		params <- as.list(environment()) %>% append(list(...))

		searchFields <- params %>% keep(names(params) %>% str_sub(1,1) == names(params) %>% str_sub(1,1) %>% str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- "all", searchFields <- searchFields %>% keep(~.x) %>% names())

		listSkyObjects(module = "Discipline", objectName = "TempIncidentOffense", schoolYearID = schoolYearID, searchFields = searchFields, page = page, pageSize = pageSize, SearchConditionsList = searchConditionsList, SearchConditionsGroupType = searchConditionsGroupType, SearchSortFieldNamesList = searchSortFieldNamesList, SearchSortFieldNamesDescendingList = searchSortFieldNamesDescendingList, entityId = entityID, flatten = flatten)
	}


	listTempIncidentOffenseNameParentalInvolvementPAS <- function(searchConditionsList = NULL, TempIncidentOffenseNameParentalInvolvementPAID = F, IncidentOffenseNameParentalInvolvementPAID = F, TempIncidentOffenseNameID = F, StateParentalInvolvementPAID = F, Comment = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, searchConditionsGroupType = "And", searchSortFieldNamesList = NULL, searchSortFieldNamesDescendingList = NULL, entityID = 1, schoolYearID = NULL, page = 1, pageSize = 100000, flatten = T, ...){

		params <- as.list(environment()) %>% append(list(...))

		searchFields <- params %>% keep(names(params) %>% str_sub(1,1) == names(params) %>% str_sub(1,1) %>% str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- "all", searchFields <- searchFields %>% keep(~.x) %>% names())

		listSkyObjects(module = "Discipline", objectName = "TempIncidentOffenseNameParentalInvolvementPA", schoolYearID = schoolYearID, searchFields = searchFields, page = page, pageSize = pageSize, SearchConditionsList = searchConditionsList, SearchConditionsGroupType = searchConditionsGroupType, SearchSortFieldNamesList = searchSortFieldNamesList, SearchSortFieldNamesDescendingList = searchSortFieldNamesDescendingList, entityId = entityID, flatten = flatten)
	}


	listTempIncidentOffenseNameDrugs <- function(searchConditionsList = NULL, TempIncidentOffenseNameDrugID = F, IncidentOffenseNameDrugID = F, TempIncidentOffenseNameID = F, DrugID = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, searchConditionsGroupType = "And", searchSortFieldNamesList = NULL, searchSortFieldNamesDescendingList = NULL, entityID = 1, schoolYearID = NULL, page = 1, pageSize = 100000, flatten = T, ...){

		params <- as.list(environment()) %>% append(list(...))

		searchFields <- params %>% keep(names(params) %>% str_sub(1,1) == names(params) %>% str_sub(1,1) %>% str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- "all", searchFields <- searchFields %>% keep(~.x) %>% names())

		listSkyObjects(module = "Discipline", objectName = "TempIncidentOffenseNameDrug", schoolYearID = schoolYearID, searchFields = searchFields, page = page, pageSize = pageSize, SearchConditionsList = searchConditionsList, SearchConditionsGroupType = searchConditionsGroupType, SearchSortFieldNamesList = searchSortFieldNamesList, SearchSortFieldNamesDescendingList = searchSortFieldNamesDescendingList, entityId = entityID, flatten = flatten)
	}


	listTempIncidentOffenseNameWeaponMNS <- function(searchConditionsList = NULL, MeetsFederalStatuteOfDangerousWeapon = F, MeetsStateStatuteOfDangerousWeapon = F, GunWasLoaded = F, GunWasInCase = F, GunFoundInTrunk = F, TempIncidentOffenseNameWeaponID = F, IncidentOffenseNameWeaponID = F, TempIncidentOffenseNameID = F, WeaponID = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, WeaponCount = F, searchConditionsGroupType = "And", searchSortFieldNamesList = NULL, searchSortFieldNamesDescendingList = NULL, entityID = 1, schoolYearID = NULL, page = 1, pageSize = 100000, flatten = T, ...){

		params <- as.list(environment()) %>% append(list(...))

		searchFields <- params %>% keep(names(params) %>% str_sub(1,1) == names(params) %>% str_sub(1,1) %>% str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- "all", searchFields <- searchFields %>% keep(~.x) %>% names())

		listSkyObjects(module = "Discipline", objectName = "TempIncidentOffenseNameWeaponMN", schoolYearID = schoolYearID, searchFields = searchFields, page = page, pageSize = pageSize, SearchConditionsList = searchConditionsList, SearchConditionsGroupType = searchConditionsGroupType, SearchSortFieldNamesList = searchSortFieldNamesList, SearchSortFieldNamesDescendingList = searchSortFieldNamesDescendingList, entityId = entityID, flatten = flatten)
	}


	listTempIncidentOffenseNameWeapons <- function(searchConditionsList = NULL, TempIncidentOffenseNameWeaponID = F, IncidentOffenseNameWeaponID = F, TempIncidentOffenseNameID = F, WeaponID = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, WeaponCount = F, searchConditionsGroupType = "And", searchSortFieldNamesList = NULL, searchSortFieldNamesDescendingList = NULL, entityID = 1, schoolYearID = NULL, page = 1, pageSize = 100000, flatten = T, ...){

		params <- as.list(environment()) %>% append(list(...))

		searchFields <- params %>% keep(names(params) %>% str_sub(1,1) == names(params) %>% str_sub(1,1) %>% str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- "all", searchFields <- searchFields %>% keep(~.x) %>% names())

		listSkyObjects(module = "Discipline", objectName = "TempIncidentOffenseNameWeapon", schoolYearID = schoolYearID, searchFields = searchFields, page = page, pageSize = pageSize, SearchConditionsList = searchConditionsList, SearchConditionsGroupType = searchConditionsGroupType, SearchSortFieldNamesList = searchSortFieldNamesList, SearchSortFieldNamesDescendingList = searchSortFieldNamesDescendingList, entityId = entityID, flatten = flatten)
	}


	listTempIncidentOffenseNameActionWIS <- function(searchConditionsList = NULL, StateModifiedTermWIID = F, HasEarlyReinstatementCondition = F, BehaviorDetailedDescription = F, IAESRemovalType = F, CausedSeriousBodilyInjury = F, TempIncidentOffenseNameActionID = F, EntityID = F, FullName = F, StudentNumber = F, InvolvementType = F, OffenseCodeDescription = F, IsPrimaryOffense = F, ActionTypeID = F, ActionTypeCode = F, ActionID = F, ActionCodeDescription = F, Status = F, OrderedDate = F, DurationType = F, DurationToServe = F, StaffIDAuthorizedBy = F, StaffIDAuthorizedByName = F, StaffIDFollowUpOfficer = F, LocationID = F, BuildingID = F, RoomID = F, PerceivedMotivationCodeDescription = F, Comment = F, TempIncidentOffenseNameID = F, IsGuardianNotified = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, StartTime = F, InternalComment = F, searchConditionsGroupType = "And", searchSortFieldNamesList = NULL, searchSortFieldNamesDescendingList = NULL, entityID = 1, schoolYearID = NULL, page = 1, pageSize = 100000, flatten = T, ...){

		params <- as.list(environment()) %>% append(list(...))

		searchFields <- params %>% keep(names(params) %>% str_sub(1,1) == names(params) %>% str_sub(1,1) %>% str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- "all", searchFields <- searchFields %>% keep(~.x) %>% names())

		listSkyObjects(module = "Discipline", objectName = "TempIncidentOffenseNameActionWI", schoolYearID = schoolYearID, searchFields = searchFields, page = page, pageSize = pageSize, SearchConditionsList = searchConditionsList, SearchConditionsGroupType = searchConditionsGroupType, SearchSortFieldNamesList = searchSortFieldNamesList, SearchSortFieldNamesDescendingList = searchSortFieldNamesDescendingList, entityId = entityID, flatten = flatten)
	}


	listTempIncidentOffenseNameActionWAS <- function(searchConditionsList = NULL, PlacedInInterimAlternativeEducationalSetting = F, DateReadmissionPetitionSubmitted = F, DateReadmissionPetitionGranted = F, DateReengagementMeetingHeld = F, StateAcademicServiceWAID = F, StateBehaviorServiceWAID = F, StatePetitionExtensionExpulsionWAID = F, StateReengagementPlanWAID = F, StateAppealCodeWAID = F, TotalAmountOfExclusionaryTime = F, DurationOfExclusionaryActionDays = F, TempIncidentOffenseNameActionID = F, EntityID = F, FullName = F, StudentNumber = F, InvolvementType = F, OffenseCodeDescription = F, IsPrimaryOffense = F, ActionTypeID = F, ActionTypeCode = F, ActionID = F, ActionCodeDescription = F, Status = F, OrderedDate = F, DurationType = F, DurationToServe = F, StaffIDAuthorizedBy = F, StaffIDAuthorizedByName = F, StaffIDFollowUpOfficer = F, LocationID = F, BuildingID = F, RoomID = F, PerceivedMotivationCodeDescription = F, Comment = F, TempIncidentOffenseNameID = F, IsGuardianNotified = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, StartTime = F, InternalComment = F, searchConditionsGroupType = "And", searchSortFieldNamesList = NULL, searchSortFieldNamesDescendingList = NULL, entityID = 1, schoolYearID = NULL, page = 1, pageSize = 100000, flatten = T, ...){

		params <- as.list(environment()) %>% append(list(...))

		searchFields <- params %>% keep(names(params) %>% str_sub(1,1) == names(params) %>% str_sub(1,1) %>% str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- "all", searchFields <- searchFields %>% keep(~.x) %>% names())

		listSkyObjects(module = "Discipline", objectName = "TempIncidentOffenseNameActionWA", schoolYearID = schoolYearID, searchFields = searchFields, page = page, pageSize = pageSize, SearchConditionsList = searchConditionsList, SearchConditionsGroupType = searchConditionsGroupType, SearchSortFieldNamesList = searchSortFieldNamesList, SearchSortFieldNamesDescendingList = searchSortFieldNamesDescendingList, entityId = entityID, flatten = flatten)
	}


	listTempIncidentOffenseNameActionPAS <- function(searchConditionsList = NULL, ReceivedEducationalServices = F, AssignedAlternativeEducation = F, TempIncidentOffenseNameActionID = F, EntityID = F, FullName = F, StudentNumber = F, InvolvementType = F, OffenseCodeDescription = F, IsPrimaryOffense = F, ActionTypeID = F, ActionTypeCode = F, ActionID = F, ActionCodeDescription = F, Status = F, OrderedDate = F, DurationType = F, DurationToServe = F, StaffIDAuthorizedBy = F, StaffIDAuthorizedByName = F, StaffIDFollowUpOfficer = F, LocationID = F, BuildingID = F, RoomID = F, PerceivedMotivationCodeDescription = F, Comment = F, TempIncidentOffenseNameID = F, IsGuardianNotified = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, StartTime = F, InternalComment = F, searchConditionsGroupType = "And", searchSortFieldNamesList = NULL, searchSortFieldNamesDescendingList = NULL, entityID = 1, schoolYearID = NULL, page = 1, pageSize = 100000, flatten = T, ...){

		params <- as.list(environment()) %>% append(list(...))

		searchFields <- params %>% keep(names(params) %>% str_sub(1,1) == names(params) %>% str_sub(1,1) %>% str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- "all", searchFields <- searchFields %>% keep(~.x) %>% names())

		listSkyObjects(module = "Discipline", objectName = "TempIncidentOffenseNameActionPA", schoolYearID = schoolYearID, searchFields = searchFields, page = page, pageSize = pageSize, SearchConditionsList = searchConditionsList, SearchConditionsGroupType = searchConditionsGroupType, SearchSortFieldNamesList = searchSortFieldNamesList, SearchSortFieldNamesDescendingList = searchSortFieldNamesDescendingList, entityId = entityID, flatten = flatten)
	}


	listTempIncidentOffenseNameActionMNS <- function(searchConditionsList = NULL, StateDIRSAESTypeMNID = F, DateExpulsionExclusionEnds = F, ReturnBeforeYearEnd = F, ExclusionThroughYearEnd = F, ExpulsionModified = F, ExpulsionThroughYearEnd = F, DIRSActionExplanation = F, TempIncidentOffenseNameActionID = F, EntityID = F, FullName = F, StudentNumber = F, InvolvementType = F, OffenseCodeDescription = F, IsPrimaryOffense = F, ActionTypeID = F, ActionTypeCode = F, ActionID = F, ActionCodeDescription = F, Status = F, OrderedDate = F, DurationType = F, DurationToServe = F, StaffIDAuthorizedBy = F, StaffIDAuthorizedByName = F, StaffIDFollowUpOfficer = F, LocationID = F, BuildingID = F, RoomID = F, PerceivedMotivationCodeDescription = F, Comment = F, TempIncidentOffenseNameID = F, IsGuardianNotified = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, StartTime = F, InternalComment = F, NoServiceProvidedExplanation = F, searchConditionsGroupType = "And", searchSortFieldNamesList = NULL, searchSortFieldNamesDescendingList = NULL, entityID = 1, schoolYearID = NULL, page = 1, pageSize = 100000, flatten = T, ...){

		params <- as.list(environment()) %>% append(list(...))

		searchFields <- params %>% keep(names(params) %>% str_sub(1,1) == names(params) %>% str_sub(1,1) %>% str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- "all", searchFields <- searchFields %>% keep(~.x) %>% names())

		listSkyObjects(module = "Discipline", objectName = "TempIncidentOffenseNameActionMN", schoolYearID = schoolYearID, searchFields = searchFields, page = page, pageSize = pageSize, SearchConditionsList = searchConditionsList, SearchConditionsGroupType = searchConditionsGroupType, SearchSortFieldNamesList = searchSortFieldNamesList, SearchSortFieldNamesDescendingList = searchSortFieldNamesDescendingList, entityId = entityID, flatten = flatten)
	}


	listTempIncidentOffenseNameActionINS <- function(searchConditionsList = NULL, StateSuspensionReasonINID = F, StateEducationalServiceProvidedINID = F, TempIncidentOffenseNameActionID = F, EntityID = F, FullName = F, StudentNumber = F, InvolvementType = F, OffenseCodeDescription = F, IsPrimaryOffense = F, ActionTypeID = F, ActionTypeCode = F, ActionID = F, ActionCodeDescription = F, Status = F, OrderedDate = F, DurationType = F, DurationToServe = F, StaffIDAuthorizedBy = F, StaffIDAuthorizedByName = F, StaffIDFollowUpOfficer = F, LocationID = F, BuildingID = F, RoomID = F, PerceivedMotivationCodeDescription = F, Comment = F, TempIncidentOffenseNameID = F, IsGuardianNotified = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, StartTime = F, IsPrimaryAction = F, InternalComment = F, searchConditionsGroupType = "And", searchSortFieldNamesList = NULL, searchSortFieldNamesDescendingList = NULL, entityID = 1, schoolYearID = NULL, page = 1, pageSize = 100000, flatten = T, ...){

		params <- as.list(environment()) %>% append(list(...))

		searchFields <- params %>% keep(names(params) %>% str_sub(1,1) == names(params) %>% str_sub(1,1) %>% str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- "all", searchFields <- searchFields %>% keep(~.x) %>% names())

		listSkyObjects(module = "Discipline", objectName = "TempIncidentOffenseNameActionIN", schoolYearID = schoolYearID, searchFields = searchFields, page = page, pageSize = pageSize, SearchConditionsList = searchConditionsList, SearchConditionsGroupType = searchConditionsGroupType, SearchSortFieldNamesList = searchSortFieldNamesList, SearchSortFieldNamesDescendingList = searchSortFieldNamesDescendingList, entityId = entityID, flatten = flatten)
	}


	listTempIncidentInvolvedPersonWAS <- function(searchConditionsList = NULL, StateReportedOffense = F, TempIncidentInvolvedPersonID = F, IncidentOffenseNameKey = F, PrimaryOffenseID = F, SecondaryOffensesBackingData = F, ExcludePrimaryOffense = F, ExistingIncidentOffenseNamesToDelete = F, InvolvementType = F, IncidentOffenseNameType = F, FreeformName = F, StudentID = F, StaffID = F, NameID = F, StaffIDDisciplineOfficer = F, PerceivedMotivationID = F, FullName = F, StudentNumber = F, PerceivedMotivationCodeDescription = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, OSSCount = F, OSSPartialCount = F, ISSCount = F, ISSPartialCount = F, InternalComment = F, searchConditionsGroupType = "And", searchSortFieldNamesList = NULL, searchSortFieldNamesDescendingList = NULL, entityID = 1, schoolYearID = NULL, page = 1, pageSize = 100000, flatten = T, ...){

		params <- as.list(environment()) %>% append(list(...))

		searchFields <- params %>% keep(names(params) %>% str_sub(1,1) == names(params) %>% str_sub(1,1) %>% str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- "all", searchFields <- searchFields %>% keep(~.x) %>% names())

		listSkyObjects(module = "Discipline", objectName = "TempIncidentInvolvedPersonWA", schoolYearID = schoolYearID, searchFields = searchFields, page = page, pageSize = pageSize, SearchConditionsList = searchConditionsList, SearchConditionsGroupType = searchConditionsGroupType, SearchSortFieldNamesList = searchSortFieldNamesList, SearchSortFieldNamesDescendingList = searchSortFieldNamesDescendingList, entityId = entityID, flatten = flatten)
	}


	listTempIncidentInvolvedPersonTXES <- function(searchConditionsList = NULL, CampusIDOfDisciplinaryResponsibility = F, TempIncidentInvolvedPersonID = F, IncidentOffenseNameKey = F, PrimaryOffenseID = F, SecondaryOffensesBackingData = F, ExcludePrimaryOffense = F, ExistingIncidentOffenseNamesToDelete = F, InvolvementType = F, IncidentOffenseNameType = F, FreeformName = F, StudentID = F, StaffID = F, NameID = F, StaffIDDisciplineOfficer = F, PerceivedMotivationID = F, FullName = F, StudentNumber = F, PerceivedMotivationCodeDescription = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, OSSCount = F, OSSPartialCount = F, ISSCount = F, ISSPartialCount = F, InternalComment = F, searchConditionsGroupType = "And", searchSortFieldNamesList = NULL, searchSortFieldNamesDescendingList = NULL, entityID = 1, schoolYearID = NULL, page = 1, pageSize = 100000, flatten = T, ...){

		params <- as.list(environment()) %>% append(list(...))

		searchFields <- params %>% keep(names(params) %>% str_sub(1,1) == names(params) %>% str_sub(1,1) %>% str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- "all", searchFields <- searchFields %>% keep(~.x) %>% names())

		listSkyObjects(module = "Discipline", objectName = "TempIncidentInvolvedPersonTX", schoolYearID = schoolYearID, searchFields = searchFields, page = page, pageSize = pageSize, SearchConditionsList = searchConditionsList, SearchConditionsGroupType = searchConditionsGroupType, SearchSortFieldNamesList = searchSortFieldNamesList, SearchSortFieldNamesDescendingList = searchSortFieldNamesDescendingList, entityId = entityID, flatten = flatten)
	}


	listTempIncidentInvolvedPersonPAS <- function(searchConditionsList = NULL, StateVictimTypePAID = F, StudentAssistanceProgramReferral = F, MedicalTreatmentRequired = F, IncidentVictimComment = F, StateInjurySeverityPAID = F, StateOffenderTypePAID = F, StateArrestedPAID = F, StateAdjudicationPAID = F, StateWeaponDetectedMethodPAID = F, WeaponDetectionComment = F, LocalLawEnforcementNotified = F, NameOfLocalLawEnforcementContacted = F, TempIncidentInvolvedPersonID = F, IncidentOffenseNameKey = F, PrimaryOffenseID = F, SecondaryOffensesBackingData = F, ExcludePrimaryOffense = F, ExistingIncidentOffenseNamesToDelete = F, InvolvementType = F, IncidentOffenseNameType = F, FreeformName = F, StudentID = F, StaffID = F, NameID = F, StaffIDDisciplineOfficer = F, PerceivedMotivationID = F, FullName = F, StudentNumber = F, PerceivedMotivationCodeDescription = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, IsResidentialPlacementByNonEdAgency = F, LLEIncidentNumber = F, OSSCount = F, OSSPartialCount = F, ISSCount = F, ISSPartialCount = F, InternalComment = F, StateDistrictPAIDPerson = F, PASecureID = F, FirstName = F, LastName = F, RaceEthnicities = F, Genders = F, SendingOrCharterTypes = F, SendingDistrictOrCharterAUNID = F, AgeAtTimeOfIncident = F, StateGradeLevelPIMSPAID = F, searchConditionsGroupType = "And", searchSortFieldNamesList = NULL, searchSortFieldNamesDescendingList = NULL, entityID = 1, schoolYearID = NULL, page = 1, pageSize = 100000, flatten = T, ...){

		params <- as.list(environment()) %>% append(list(...))

		searchFields <- params %>% keep(names(params) %>% str_sub(1,1) == names(params) %>% str_sub(1,1) %>% str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- "all", searchFields <- searchFields %>% keep(~.x) %>% names())

		listSkyObjects(module = "Discipline", objectName = "TempIncidentInvolvedPersonPA", schoolYearID = schoolYearID, searchFields = searchFields, page = page, pageSize = pageSize, SearchConditionsList = searchConditionsList, SearchConditionsGroupType = searchConditionsGroupType, SearchSortFieldNamesList = searchSortFieldNamesList, SearchSortFieldNamesDescendingList = searchSortFieldNamesDescendingList, entityId = entityID, flatten = flatten)
	}


	listTempIncidentInvolvedPersonMNS <- function(searchConditionsList = NULL, StateVictimTypeMNID = F, StateVictimCostMNID = F, InjuryOccured = F, WasSeriousBodilyInjury = F, ReportedToLawEnforcement = F, OffenderArrestedByLawEnforcement = F, StateOffenderActivityMNID = F, IsPhysicalAssault = F, IsPhysicalAssaultState = F, TempIncidentInvolvedPersonID = F, IncidentOffenseNameKey = F, PrimaryOffenseID = F, SecondaryOffensesBackingData = F, ExcludePrimaryOffense = F, ExistingIncidentOffenseNamesToDelete = F, InvolvementType = F, IncidentOffenseNameType = F, FreeformName = F, StudentID = F, StaffID = F, NameID = F, StaffIDDisciplineOfficer = F, PerceivedMotivationID = F, FullName = F, StudentNumber = F, PerceivedMotivationCodeDescription = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, OSSCount = F, OSSPartialCount = F, ISSCount = F, ISSPartialCount = F, InternalComment = F, MultipleVictimCount = F, EstimatedVictimsEnrolled = F, EstimatedVictimsNotEnrolled = F, searchConditionsGroupType = "And", searchSortFieldNamesList = NULL, searchSortFieldNamesDescendingList = NULL, entityID = 1, schoolYearID = NULL, page = 1, pageSize = 100000, flatten = T, ...){

		params <- as.list(environment()) %>% append(list(...))

		searchFields <- params %>% keep(names(params) %>% str_sub(1,1) == names(params) %>% str_sub(1,1) %>% str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- "all", searchFields <- searchFields %>% keep(~.x) %>% names())

		listSkyObjects(module = "Discipline", objectName = "TempIncidentInvolvedPersonMN", schoolYearID = schoolYearID, searchFields = searchFields, page = page, pageSize = pageSize, SearchConditionsList = searchConditionsList, SearchConditionsGroupType = searchConditionsGroupType, SearchSortFieldNamesList = searchSortFieldNamesList, SearchSortFieldNamesDescendingList = searchSortFieldNamesDescendingList, entityId = entityID, flatten = flatten)
	}


	listTempIncidentInvolvedPersonINS <- function(searchConditionsList = NULL, StateCriminalEventINID = F, StateArrestTypeINID = F, StateArrestReasonINID = F, IsChemicalRestraint = F, IsMechanicalRestraint = F, IsPhysicalRestraint = F, IsSeclusion = F, StaffIDResourceOfficer = F, TempIncidentInvolvedPersonID = F, IncidentOffenseNameKey = F, PrimaryOffenseID = F, SecondaryOffensesBackingData = F, ExcludePrimaryOffense = F, ExistingIncidentOffenseNamesToDelete = F, InvolvementType = F, IncidentOffenseNameType = F, FreeformName = F, StudentID = F, StaffID = F, NameID = F, StaffIDDisciplineOfficer = F, PerceivedMotivationID = F, FullName = F, StudentNumber = F, PerceivedMotivationCodeDescription = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, OSSCount = F, OSSPartialCount = F, ISSCount = F, ISSPartialCount = F, InternalComment = F, searchConditionsGroupType = "And", searchSortFieldNamesList = NULL, searchSortFieldNamesDescendingList = NULL, entityID = 1, schoolYearID = NULL, page = 1, pageSize = 100000, flatten = T, ...){

		params <- as.list(environment()) %>% append(list(...))

		searchFields <- params %>% keep(names(params) %>% str_sub(1,1) == names(params) %>% str_sub(1,1) %>% str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- "all", searchFields <- searchFields %>% keep(~.x) %>% names())

		listSkyObjects(module = "Discipline", objectName = "TempIncidentInvolvedPersonIN", schoolYearID = schoolYearID, searchFields = searchFields, page = page, pageSize = pageSize, SearchConditionsList = searchConditionsList, SearchConditionsGroupType = searchConditionsGroupType, SearchSortFieldNamesList = searchSortFieldNamesList, SearchSortFieldNamesDescendingList = searchSortFieldNamesDescendingList, entityId = entityID, flatten = flatten)
	}


	listTempIncidentInvolvedPeople <- function(searchConditionsList = NULL, TempIncidentInvolvedPersonID = F, IncidentOffenseNameKey = F, PrimaryOffenseID = F, SecondaryOffensesBackingData = F, ExcludePrimaryOffense = F, ExistingIncidentOffenseNamesToDelete = F, InvolvementType = F, IncidentOffenseNameType = F, FreeformName = F, StudentID = F, StaffID = F, NameID = F, StaffIDDisciplineOfficer = F, PerceivedMotivationID = F, FullName = F, StudentNumber = F, PerceivedMotivationCodeDescription = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, OSSCount = F, OSSPartialCount = F, ISSCount = F, ISSPartialCount = F, InternalComment = F, searchConditionsGroupType = "And", searchSortFieldNamesList = NULL, searchSortFieldNamesDescendingList = NULL, entityID = 1, schoolYearID = NULL, page = 1, pageSize = 100000, flatten = T, ...){

		params <- as.list(environment()) %>% append(list(...))

		searchFields <- params %>% keep(names(params) %>% str_sub(1,1) == names(params) %>% str_sub(1,1) %>% str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- "all", searchFields <- searchFields %>% keep(~.x) %>% names())

		listSkyObjects(module = "Discipline", objectName = "TempIncidentInvolvedPerson", schoolYearID = schoolYearID, searchFields = searchFields, page = page, pageSize = pageSize, SearchConditionsList = searchConditionsList, SearchConditionsGroupType = searchConditionsGroupType, SearchSortFieldNamesList = searchSortFieldNamesList, SearchSortFieldNamesDescendingList = searchSortFieldNamesDescendingList, entityId = entityID, flatten = flatten)
	}
