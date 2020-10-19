

	listCivilRightsDataCollectionExceptions <- function(searchConditionsList = NULL, CivilRightsDataCollectionExceptionID = F, CivilRightsDataCollectionID = F, CivilRightsDataCollectionRunHistoryID = F, DistrictID = F, SchoolID = F, MessageType = F, Message = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, searchConditionsGroupType = "And", searchSortFieldNamesList = NULL, searchSortFieldNamesDescendingList = NULL, entityID = 1, schoolYearID = NULL, page = 1, pageSize = 100000, flatten = T, ...){

		params <- as.list(environment()) %>% append(list(...))

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- "all", searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		listSkyObjects(module = "StateReportingFederal", objectName = "CivilRightsDataCollectionException", schoolYearID = schoolYearID, searchFields = searchFields, page = page, pageSize = pageSize, SearchConditionsList = searchConditionsList, SearchConditionsGroupType = searchConditionsGroupType, SearchSortFieldNamesList = searchSortFieldNamesList, SearchSortFieldNamesDescendingList = searchSortFieldNamesDescendingList, entityId = entityID, flatten = flatten)
	}


	listFederalOffenseCategories <- function(searchConditionsList = NULL, FederalOffenseCategoryID = F, Description = F, ValidYearLow = F, ValidYearHigh = F, IsInvalid = F, SkywardID = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, SkywardHash = F, searchConditionsGroupType = "And", searchSortFieldNamesList = NULL, searchSortFieldNamesDescendingList = NULL, entityID = 1, schoolYearID = NULL, page = 1, pageSize = 100000, flatten = T, ...){

		params <- as.list(environment()) %>% append(list(...))

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- "all", searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		listSkyObjects(module = "StateReportingFederal", objectName = "FederalOffenseCategory", schoolYearID = schoolYearID, searchFields = searchFields, page = page, pageSize = pageSize, SearchConditionsList = searchConditionsList, SearchConditionsGroupType = searchConditionsGroupType, SearchSortFieldNamesList = searchSortFieldNamesList, SearchSortFieldNamesDescendingList = searchSortFieldNamesDescendingList, entityId = entityID, flatten = flatten)
	}


	listFederalDisciplineCategories <- function(searchConditionsList = NULL, FederalDisciplineCategoryID = F, Description = F, ValidYearLow = F, ValidYearHigh = F, IsInvalid = F, SkywardID = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, SkywardHash = F, searchConditionsGroupType = "And", searchSortFieldNamesList = NULL, searchSortFieldNamesDescendingList = NULL, entityID = 1, schoolYearID = NULL, page = 1, pageSize = 100000, flatten = T, ...){

		params <- as.list(environment()) %>% append(list(...))

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- "all", searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		listSkyObjects(module = "StateReportingFederal", objectName = "FederalDisciplineCategory", schoolYearID = schoolYearID, searchFields = searchFields, page = page, pageSize = pageSize, SearchConditionsList = searchConditionsList, SearchConditionsGroupType = searchConditionsGroupType, SearchSortFieldNamesList = searchSortFieldNamesList, SearchSortFieldNamesDescendingList = searchSortFieldNamesDescendingList, entityId = entityID, flatten = flatten)
	}


	listFederalAdvancedPlacementCourseTypes <- function(searchConditionsList = NULL, FederalAdvancedPlacementCourseTypeID = F, Description = F, ValidYearLow = F, ValidYearHigh = F, IsInvalid = F, SkywardID = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, SkywardHash = F, searchConditionsGroupType = "And", searchSortFieldNamesList = NULL, searchSortFieldNamesDescendingList = NULL, entityID = 1, schoolYearID = NULL, page = 1, pageSize = 100000, flatten = T, ...){

		params <- as.list(environment()) %>% append(list(...))

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- "all", searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		listSkyObjects(module = "StateReportingFederal", objectName = "FederalAdvancedPlacementCourseType", schoolYearID = schoolYearID, searchFields = searchFields, page = page, pageSize = pageSize, SearchConditionsList = searchConditionsList, SearchConditionsGroupType = searchConditionsGroupType, SearchSortFieldNamesList = searchSortFieldNamesList, SearchSortFieldNamesDescendingList = searchSortFieldNamesDescendingList, entityId = entityID, flatten = flatten)
	}


	listFederalSubjectTypes <- function(searchConditionsList = NULL, FederalSubjectTypeID = F, Description = F, ValidYearLow = F, ValidYearHigh = F, IsInvalid = F, SkywardID = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, SkywardHash = F, searchConditionsGroupType = "And", searchSortFieldNamesList = NULL, searchSortFieldNamesDescendingList = NULL, entityID = 1, schoolYearID = NULL, page = 1, pageSize = 100000, flatten = T, ...){

		params <- as.list(environment()) %>% append(list(...))

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- "all", searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		listSkyObjects(module = "StateReportingFederal", objectName = "FederalSubjectType", schoolYearID = schoolYearID, searchFields = searchFields, page = page, pageSize = pageSize, SearchConditionsList = searchConditionsList, SearchConditionsGroupType = searchConditionsGroupType, SearchSortFieldNamesList = searchSortFieldNamesList, SearchSortFieldNamesDescendingList = searchSortFieldNamesDescendingList, entityId = entityID, flatten = flatten)
	}


	listACAYearExceptions <- function(searchConditionsList = NULL, ACAYearExceptionID = F, ACAYearID = F, EmployeeID = F, IsFatalException = F, ExceptionType = F, Description = F, SourceType = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, ACAYearEmployeeV1ID = F, searchConditionsGroupType = "And", searchSortFieldNamesList = NULL, searchSortFieldNamesDescendingList = NULL, entityID = 1, schoolYearID = NULL, page = 1, pageSize = 100000, flatten = T, ...){

		params <- as.list(environment()) %>% append(list(...))

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- "all", searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		listSkyObjects(module = "StateReportingFederal", objectName = "ACAYearException", schoolYearID = schoolYearID, searchFields = searchFields, page = page, pageSize = pageSize, SearchConditionsList = searchConditionsList, SearchConditionsGroupType = searchConditionsGroupType, SearchSortFieldNamesList = searchSortFieldNamesList, SearchSortFieldNamesDescendingList = searchSortFieldNamesDescendingList, entityId = entityID, flatten = flatten)
	}


	listCivilRightsDataCollections <- function(searchConditionsList = NULL, CivilRightsDataCollectionID = F, DistrictID = F, HasLEAFile = F, HasSchoolFile = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, NumericYear = F, searchConditionsGroupType = "And", searchSortFieldNamesList = NULL, searchSortFieldNamesDescendingList = NULL, entityID = 1, schoolYearID = NULL, page = 1, pageSize = 100000, flatten = T, ...){

		params <- as.list(environment()) %>% append(list(...))

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- "all", searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		listSkyObjects(module = "StateReportingFederal", objectName = "CivilRightsDataCollection", schoolYearID = schoolYearID, searchFields = searchFields, page = page, pageSize = pageSize, SearchConditionsList = searchConditionsList, SearchConditionsGroupType = searchConditionsGroupType, SearchSortFieldNamesList = searchSortFieldNamesList, SearchSortFieldNamesDescendingList = searchSortFieldNamesDescendingList, entityId = entityID, flatten = flatten)
	}


	listCivilRightsDataCollectionRunHistories <- function(searchConditionsList = NULL, CivilRightsDataCollectionRunHistoryID = F, CivilRightsDataCollectionID = F, CollectionType = F, HasMediaID = F, StartDateTime = F, EndDateTime = F, Type = F, MediaID = F, IsExport = F, IsLocked = F, Status = F, RunData = F, UserIDCanceledBy = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, ScopeAccessAllowed = F, HasValidMedia = F, CanAccessMedia = F, RunParameters = F, searchConditionsGroupType = "And", searchSortFieldNamesList = NULL, searchSortFieldNamesDescendingList = NULL, entityID = 1, schoolYearID = NULL, page = 1, pageSize = 100000, flatten = T, ...){

		params <- as.list(environment()) %>% append(list(...))

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- "all", searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		listSkyObjects(module = "StateReportingFederal", objectName = "CivilRightsDataCollectionRunHistory", schoolYearID = schoolYearID, searchFields = searchFields, page = page, pageSize = pageSize, SearchConditionsList = searchConditionsList, SearchConditionsGroupType = searchConditionsGroupType, SearchSortFieldNamesList = searchSortFieldNamesList, SearchSortFieldNamesDescendingList = searchSortFieldNamesDescendingList, entityId = entityID, flatten = flatten)
	}


	listCRDCLEACoordinatorV1s <- function(searchConditionsList = NULL, CRDCLEACoordinatorV1ID = F, DistrictID = F, DistrictSchoolYearID = F, NameIDSex = F, SexFirstName = F, SexLastName = F, SexPhone = F, SexEmail = F, NameIDRace = F, RaceFirstName = F, RaceLastName = F, RacePhone = F, RaceEmail = F, NameIDDisability = F, DisabilityFirstName = F, DisabilityLastName = F, DisabilityPhone = F, DisabilityEmail = F, HasGender = F, HasRace = F, HasDisability = F, FormattedSexPhoneNumber = F, FormattedRacePhoneNumber = F, FormattedDisabilityPhoneNumber = F, CivilRightsDataCollectionID = F, CivilRightsDataCollectionRunHistoryID = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, searchConditionsGroupType = "And", searchSortFieldNamesList = NULL, searchSortFieldNamesDescendingList = NULL, entityID = 1, schoolYearID = NULL, page = 1, pageSize = 100000, flatten = T, ...){

		params <- as.list(environment()) %>% append(list(...))

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- "all", searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		listSkyObjects(module = "StateReportingFederal", objectName = "CRDCLEACoordinatorV1", schoolYearID = schoolYearID, searchFields = searchFields, page = page, pageSize = pageSize, SearchConditionsList = searchConditionsList, SearchConditionsGroupType = searchConditionsGroupType, SearchSortFieldNamesList = searchSortFieldNamesList, SearchSortFieldNamesDescendingList = searchSortFieldNamesDescendingList, entityId = entityID, flatten = flatten)
	}


	listCRDCLEACountV1s <- function(searchConditionsList = NULL, CRDCLEACountV1ID = F, DistrictID = F, DistrictSchoolYearID = F, NCESIDCode = F, SchoolCount = F, StudentNonLEACount = F, CivilRightsDataCollectionID = F, CivilRightsDataCollectionRunHistoryID = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, StudentOverallCount = F, searchConditionsGroupType = "And", searchSortFieldNamesList = NULL, searchSortFieldNamesDescendingList = NULL, entityID = 1, schoolYearID = NULL, page = 1, pageSize = 100000, flatten = T, ...){

		params <- as.list(environment()) %>% append(list(...))

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- "all", searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		listSkyObjects(module = "StateReportingFederal", objectName = "CRDCLEACountV1", schoolYearID = schoolYearID, searchFields = searchFields, page = page, pageSize = pageSize, SearchConditionsList = searchConditionsList, SearchConditionsGroupType = searchConditionsGroupType, SearchSortFieldNamesList = searchSortFieldNamesList, SearchSortFieldNamesDescendingList = searchSortFieldNamesDescendingList, entityId = entityID, flatten = flatten)
	}


	listCRDCLEAHarassmentV1s <- function(searchConditionsList = NULL, CRDCLEAHarassmentV1ID = F, DistrictID = F, DistrictSchoolYearID = F, HasDesegregationPlan = F, HasHarassmentPolicy = F, HasHarassmentPolicyWebLink = F, HarassmentPolicyWebLink = F, CivilRightsDataCollectionID = F, CivilRightsDataCollectionRunHistoryID = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, searchConditionsGroupType = "And", searchSortFieldNamesList = NULL, searchSortFieldNamesDescendingList = NULL, entityID = 1, schoolYearID = NULL, page = 1, pageSize = 100000, flatten = T, ...){

		params <- as.list(environment()) %>% append(list(...))

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- "all", searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		listSkyObjects(module = "StateReportingFederal", objectName = "CRDCLEAHarassmentV1", schoolYearID = schoolYearID, searchFields = searchFields, page = page, pageSize = pageSize, SearchConditionsList = searchConditionsList, SearchConditionsGroupType = searchConditionsGroupType, SearchSortFieldNamesList = searchSortFieldNamesList, SearchSortFieldNamesDescendingList = searchSortFieldNamesDescendingList, entityId = entityID, flatten = flatten)
	}


	listCRDCSchoolAlternativeTransferWithDisabilityV1s <- function(searchConditionsList = NULL, CRDCSchoolAlternativeTransferWithDisabilityV1ID = F, MaleHispanicCount = F, MaleAlaskanCount = F, MaleAsianCount = F, MaleHawaiianCount = F, MaleBlackCount = F, MaleWhiteCount = F, MaleMultiRaceCount = F, MaleLEPCount = F, MaleSection504Count = F, FemaleHispanicCount = F, FemaleAlaskanCount = F, FemaleAsianCount = F, FemaleHawaiianCount = F, FemaleBlackCount = F, FemaleWhiteCount = F, FemaleMultiRaceCount = F, FemaleLEPCount = F, FemaleSection504Count = F, CivilRightsDataCollectionID = F, CivilRightsDataCollectionRunHistoryID = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, FederalNCESSchoolID = F, searchConditionsGroupType = "And", searchSortFieldNamesList = NULL, searchSortFieldNamesDescendingList = NULL, entityID = 1, schoolYearID = NULL, page = 1, pageSize = 100000, flatten = T, ...){

		params <- as.list(environment()) %>% append(list(...))

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- "all", searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		listSkyObjects(module = "StateReportingFederal", objectName = "CRDCSchoolAlternativeTransferWithDisabilityV1", schoolYearID = schoolYearID, searchFields = searchFields, page = page, pageSize = pageSize, SearchConditionsList = searchConditionsList, SearchConditionsGroupType = searchConditionsGroupType, SearchSortFieldNamesList = searchSortFieldNamesList, SearchSortFieldNamesDescendingList = searchSortFieldNamesDescendingList, entityId = entityID, flatten = flatten)
	}


	listCRDCSchoolAlternativeTransferWithoutDisabilityV1s <- function(searchConditionsList = NULL, CRDCSchoolAlternativeTransferWithoutDisabilityV1ID = F, MaleHispanicCount = F, MaleAlaskanCount = F, MaleAsianCount = F, MaleHawaiianCount = F, MaleBlackCount = F, MaleWhiteCount = F, MaleMultiRaceCount = F, MaleLEPCount = F, FemaleHispanicCount = F, FemaleAlaskanCount = F, FemaleAsianCount = F, FemaleHawaiianCount = F, FemaleBlackCount = F, FemaleWhiteCount = F, FemaleMultiRaceCount = F, FemaleLEPCount = F, CivilRightsDataCollectionID = F, CivilRightsDataCollectionRunHistoryID = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, FederalNCESSchoolID = F, searchConditionsGroupType = "And", searchSortFieldNamesList = NULL, searchSortFieldNamesDescendingList = NULL, entityID = 1, schoolYearID = NULL, page = 1, pageSize = 100000, flatten = T, ...){

		params <- as.list(environment()) %>% append(list(...))

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- "all", searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		listSkyObjects(module = "StateReportingFederal", objectName = "CRDCSchoolAlternativeTransferWithoutDisabilityV1", schoolYearID = schoolYearID, searchFields = searchFields, page = page, pageSize = pageSize, SearchConditionsList = searchConditionsList, SearchConditionsGroupType = searchConditionsGroupType, SearchSortFieldNamesList = searchSortFieldNamesList, SearchSortFieldNamesDescendingList = searchSortFieldNamesDescendingList, entityId = entityID, flatten = flatten)
	}


	listCRDCSchoolAPCourseV1s <- function(searchConditionsList = NULL, CRDCSchoolAPCourseV1ID = F, HasStudents = F, HasSelfSelection = F, CourseCount = F, MaleHispanicCount = F, MaleAlaskanCount = F, MaleAsianCount = F, MaleBlackCount = F, MaleHawaiianCount = F, MaleWhiteCount = F, MaleMultiRaceCount = F, MaleIDEACount = F, MaleLEPCount = F, FemaleHispanicCount = F, FemaleAlaskanCount = F, FemaleAsianCount = F, FemaleBlackCount = F, FemaleHawaiianCount = F, FemaleWhiteCount = F, FemaleMultiRaceCount = F, FemaleIDEACount = F, FemaleLEPCount = F, CivilRightsDataCollectionID = F, CivilRightsDataCollectionRunHistoryID = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, FederalNCESSchoolID = F, searchConditionsGroupType = "And", searchSortFieldNamesList = NULL, searchSortFieldNamesDescendingList = NULL, entityID = 1, schoolYearID = NULL, page = 1, pageSize = 100000, flatten = T, ...){

		params <- as.list(environment()) %>% append(list(...))

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- "all", searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		listSkyObjects(module = "StateReportingFederal", objectName = "CRDCSchoolAPCourseV1", schoolYearID = schoolYearID, searchFields = searchFields, page = page, pageSize = pageSize, SearchConditionsList = searchConditionsList, SearchConditionsGroupType = searchConditionsGroupType, SearchSortFieldNamesList = searchSortFieldNamesList, SearchSortFieldNamesDescendingList = searchSortFieldNamesDescendingList, entityId = entityID, flatten = flatten)
	}


	listCRDCSchoolAPMathV1s <- function(searchConditionsList = NULL, CRDCSchoolAPMathV1ID = F, HasStudents = F, MaleHispanicCount = F, MaleAlaskanCount = F, MaleAsianCount = F, MaleBlackCount = F, MaleHawaiianCount = F, MaleWhiteCount = F, MaleMultiRaceCount = F, MaleIDEACount = F, MaleLEPCount = F, FemaleHispanicCount = F, FemaleAlaskanCount = F, FemaleAsianCount = F, FemaleBlackCount = F, FemaleHawaiianCount = F, FemaleWhiteCount = F, FemaleMultiRaceCount = F, FemaleIDEACount = F, FemaleLEPCount = F, CivilRightsDataCollectionID = F, CivilRightsDataCollectionRunHistoryID = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, FederalNCESSchoolID = F, searchConditionsGroupType = "And", searchSortFieldNamesList = NULL, searchSortFieldNamesDescendingList = NULL, entityID = 1, schoolYearID = NULL, page = 1, pageSize = 100000, flatten = T, ...){

		params <- as.list(environment()) %>% append(list(...))

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- "all", searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		listSkyObjects(module = "StateReportingFederal", objectName = "CRDCSchoolAPMathV1", schoolYearID = schoolYearID, searchFields = searchFields, page = page, pageSize = pageSize, SearchConditionsList = searchConditionsList, SearchConditionsGroupType = searchConditionsGroupType, SearchSortFieldNamesList = searchSortFieldNamesList, SearchSortFieldNamesDescendingList = searchSortFieldNamesDescendingList, entityId = entityID, flatten = flatten)
	}


	listCRDCSchoolAPOtherV1s <- function(searchConditionsList = NULL, CRDCSchoolAPOtherV1ID = F, HasStudents = F, MaleHispanicCount = F, MaleAlaskanCount = F, MaleAsianCount = F, MaleBlackCount = F, MaleHawaiianCount = F, MaleWhiteCount = F, MaleMultiRaceCount = F, MaleIDEACount = F, MaleLEPCount = F, FemaleHispanicCount = F, FemaleAlaskanCount = F, FemaleAsianCount = F, FemaleBlackCount = F, FemaleHawaiianCount = F, FemaleWhiteCount = F, FemaleMultiRaceCount = F, FemaleIDEACount = F, FemaleLEPCount = F, CivilRightsDataCollectionID = F, CivilRightsDataCollectionRunHistoryID = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, FederalNCESSchoolID = F, searchConditionsGroupType = "And", searchSortFieldNamesList = NULL, searchSortFieldNamesDescendingList = NULL, entityID = 1, schoolYearID = NULL, page = 1, pageSize = 100000, flatten = T, ...){

		params <- as.list(environment()) %>% append(list(...))

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- "all", searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		listSkyObjects(module = "StateReportingFederal", objectName = "CRDCSchoolAPOtherV1", schoolYearID = schoolYearID, searchFields = searchFields, page = page, pageSize = pageSize, SearchConditionsList = searchConditionsList, SearchConditionsGroupType = searchConditionsGroupType, SearchSortFieldNamesList = searchSortFieldNamesList, SearchSortFieldNamesDescendingList = searchSortFieldNamesDescendingList, entityId = entityID, flatten = flatten)
	}


	listCRDCSchoolAPScienceV1s <- function(searchConditionsList = NULL, CRDCSchoolAPScienceV1ID = F, HasStudents = F, MaleHispanicCount = F, MaleAlaskanCount = F, MaleAsianCount = F, MaleBlackCount = F, MaleHawaiianCount = F, MaleWhiteCount = F, MaleMultiRaceCount = F, MaleIDEACount = F, MaleLEPCount = F, FemaleHispanicCount = F, FemaleAlaskanCount = F, FemaleAsianCount = F, FemaleBlackCount = F, FemaleHawaiianCount = F, FemaleWhiteCount = F, FemaleMultiRaceCount = F, FemaleIDEACount = F, FemaleLEPCount = F, CivilRightsDataCollectionID = F, CivilRightsDataCollectionRunHistoryID = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, FederalNCESSchoolID = F, searchConditionsGroupType = "And", searchSortFieldNamesList = NULL, searchSortFieldNamesDescendingList = NULL, entityID = 1, schoolYearID = NULL, page = 1, pageSize = 100000, flatten = T, ...){

		params <- as.list(environment()) %>% append(list(...))

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- "all", searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		listSkyObjects(module = "StateReportingFederal", objectName = "CRDCSchoolAPScienceV1", schoolYearID = schoolYearID, searchFields = searchFields, page = page, pageSize = pageSize, SearchConditionsList = searchConditionsList, SearchConditionsGroupType = searchConditionsGroupType, SearchSortFieldNamesList = searchSortFieldNamesList, SearchSortFieldNamesDescendingList = searchSortFieldNamesDescendingList, entityId = entityID, flatten = flatten)
	}


	listCRDCSchoolDisciplineWithoutDisabilityV1s <- function(searchConditionsList = NULL, CRDCSchoolDisciplineWithoutDisabilityV1ID = F, HasCorporalPunishment = F, CorporalMaleHispanicCount = F, CorporalMaleAlaskanCount = F, CorporalMaleAsianCount = F, CorporalMaleHawaiianCount = F, CorporalMaleBlackCount = F, CorporalMaleWhiteCount = F, CorporalMaleMultiRaceCount = F, CorporalMaleLEPCount = F, CorporalFemaleHispanicCount = F, CorporalFemaleAlaskanCount = F, CorporalFemaleAsianCount = F, CorporalFemaleHawaiianCount = F, CorporalFemaleBlackCount = F, CorporalFemaleWhiteCount = F, CorporalFemaleMultiRaceCount = F, CorporalFemaleLEPCount = F, InSchoolMaleHispanicCount = F, InSchoolMaleAlaskanCount = F, InSchoolMaleAsianCount = F, InSchoolMaleHawaiianCount = F, InSchoolMaleBlackCount = F, InSchoolMaleWhiteCount = F, InSchoolMaleMultiRaceCount = F, InSchoolMaleLEPCount = F, InSchoolFemaleHispanicCount = F, InSchoolFemaleAlaskanCount = F, InSchoolFemaleAsianCount = F, InSchoolFemaleHawaiianCount = F, InSchoolFemaleBlackCount = F, InSchoolFemaleWhiteCount = F, InSchoolFemaleMultiRaceCount = F, InSchoolFemaleLEPCount = F, OneOutOfSchoolMaleHispanicCount = F, OneOutOfSchoolMaleAlaskanCount = F, OneOutOfSchoolMaleAsianCount = F, OneOutOfSchoolMaleHawaiianCount = F, OneOutOfSchoolMaleBlackCount = F, OneOutOfSchoolMaleWhiteCount = F, OneOutOfSchoolMaleMultiRaceCount = F, OneOutOfSchoolMaleLEPCount = F, OneOutOfSchoolFemaleHispanicCount = F, OneOutOfSchoolFemaleAlaskanCount = F, OneOutOfSchoolFemaleAsianCount = F, OneOutOfSchoolFemaleHawaiianCount = F, OneOutOfSchoolFemaleBlackCount = F, OneOutOfSchoolFemaleWhiteCount = F, OneOutOfSchoolFemaleMultiRaceCount = F, OneOutOfSchoolFemaleLEPCount = F, MultipleOutOfSchoolMaleHispanicCount = F, MultipleOutOfSchoolMaleAlaskanCount = F, MultipleOutOfSchoolMaleAsianCount = F, MultipleOutOfSchoolMaleHawaiianCount = F, MultipleOutOfSchoolMaleBlackCount = F, MultipleOutOfSchoolMaleWhiteCount = F, MultipleOutOfSchoolMaleMultiRaceCount = F, MultipleOutOfSchoolMaleLEPCount = F, MultipleOutOfSchoolFemaleHispanicCount = F, MultipleOutOfSchoolFemaleAlaskanCount = F, MultipleOutOfSchoolFemaleAsianCount = F, MultipleOutOfSchoolFemaleHawaiianCount = F, MultipleOutOfSchoolFemaleBlackCount = F, MultipleOutOfSchoolFemaleWhiteCount = F, MultipleOutOfSchoolFemaleMultiRaceCount = F, MultipleOutOfSchoolFemaleLEPCount = F, ExpulsionWithServicesMaleHispanicCount = F, ExpulsionWithServicesMaleAlaskanCount = F, ExpulsionWithServicesMaleAsianCount = F, ExpulsionWithServicesMaleHawaiianCount = F, ExpulsionWithServicesMaleBlackCount = F, ExpulsionWithServicesMaleWhiteCount = F, ExpulsionWithServicesMaleMultiRaceCount = F, ExpulsionWithServicesMaleLEPCount = F, ExpulsionWithServicesFemaleHispanicCount = F, ExpulsionWithServicesFemaleAlaskanCount = F, ExpulsionWithServicesFemaleAsianCount = F, ExpulsionWithServicesFemaleHawaiianCount = F, ExpulsionWithServicesFemaleBlackCount = F, ExpulsionWithServicesFemaleWhiteCount = F, ExpulsionWithServicesFemaleMultiRaceCount = F, ExpulsionWithServicesFemaleLEPCount = F, ExpulsionNoServicesMaleHispanicCount = F, ExpulsionNoServicesMaleAlaskanCount = F, ExpulsionNoServicesMaleAsianCount = F, ExpulsionNoServicesMaleHawaiianCount = F, ExpulsionNoServicesMaleBlackCount = F, ExpulsionNoServicesMaleWhiteCount = F, ExpulsionNoServicesMaleMultiRaceCount = F, ExpulsionNoServicesMaleLEPCount = F, ExpulsionNoServicesFemaleHispanicCount = F, ExpulsionNoServicesFemaleAlaskanCount = F, ExpulsionNoServicesFemaleAsianCount = F, ExpulsionNoServicesFemaleHawaiianCount = F, ExpulsionNoServicesFemaleBlackCount = F, ExpulsionNoServicesFemaleWhiteCount = F, ExpulsionNoServicesFemaleMultiRaceCount = F, ExpulsionNoServicesFemaleLEPCount = F, ZeroToleranceMaleHispanicCount = F, ZeroToleranceMaleAlaskanCount = F, ZeroToleranceMaleAsianCount = F, ZeroToleranceMaleHawaiianCount = F, ZeroToleranceMaleBlackCount = F, ZeroToleranceMaleWhiteCount = F, ZeroToleranceMaleMultiRaceCount = F, ZeroToleranceMaleLEPCount = F, ZeroToleranceFemaleHispanicCount = F, ZeroToleranceFemaleAlaskanCount = F, ZeroToleranceFemaleAsianCount = F, ZeroToleranceFemaleHawaiianCount = F, ZeroToleranceFemaleBlackCount = F, ZeroToleranceFemaleWhiteCount = F, ZeroToleranceFemaleMultiRaceCount = F, ZeroToleranceFemaleLEPCount = F, LawEnforcementMaleHispanicCount = F, LawEnforcementMaleAlaskanCount = F, LawEnforcementMaleAsianCount = F, LawEnforcementMaleHawaiianCount = F, LawEnforcementMaleBlackCount = F, LawEnforcementMaleWhiteCount = F, LawEnforcementMaleMultiRaceCount = F, LawEnforcementMaleLEPCount = F, LawEnforcementFemaleHispanicCount = F, LawEnforcementFemaleAlaskanCount = F, LawEnforcementFemaleAsianCount = F, LawEnforcementFemaleHawaiianCount = F, LawEnforcementFemaleBlackCount = F, LawEnforcementFemaleWhiteCount = F, LawEnforcementFemaleMultiRaceCount = F, LawEnforcementFemaleLEPCount = F, ArrestMaleHispanicCount = F, ArrestMaleAlaskanCount = F, ArrestMaleAsianCount = F, ArrestMaleHawaiianCount = F, ArrestMaleBlackCount = F, ArrestMaleWhiteCount = F, ArrestMaleMultiRaceCount = F, ArrestMaleLEPCount = F, ArrestFemaleHispanicCount = F, ArrestFemaleAlaskanCount = F, ArrestFemaleAsianCount = F, ArrestFemaleHawaiianCount = F, ArrestFemaleBlackCount = F, ArrestFemaleWhiteCount = F, ArrestFemaleMultiRaceCount = F, ArrestFemaleLEPCount = F, CivilRightsDataCollectionID = F, CivilRightsDataCollectionRunHistoryID = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, FederalNCESSchoolID = F, searchConditionsGroupType = "And", searchSortFieldNamesList = NULL, searchSortFieldNamesDescendingList = NULL, entityID = 1, schoolYearID = NULL, page = 1, pageSize = 100000, flatten = T, ...){

		params <- as.list(environment()) %>% append(list(...))

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- "all", searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		listSkyObjects(module = "StateReportingFederal", objectName = "CRDCSchoolDisciplineWithoutDisabilityV1", schoolYearID = schoolYearID, searchFields = searchFields, page = page, pageSize = pageSize, SearchConditionsList = searchConditionsList, SearchConditionsGroupType = searchConditionsGroupType, SearchSortFieldNamesList = searchSortFieldNamesList, SearchSortFieldNamesDescendingList = searchSortFieldNamesDescendingList, entityId = entityID, flatten = flatten)
	}


	listCRDCSchoolIDEARestraintV1s <- function(searchConditionsList = NULL, CRDCSchoolIDEARestraintV1ID = F, MechanicalMaleHispanicCount = F, MechanicalMaleAlaskanCount = F, MechanicalMaleAsianCount = F, MechanicalMaleHawaiianCount = F, MechanicalMaleBlackCount = F, MechanicalMaleWhiteCount = F, MechanicalMaleMultiRaceCount = F, MechanicalMaleLEPCount = F, MechanicalMaleSection504Count = F, MechanicalFemaleHispanicCount = F, MechanicalFemaleAlaskanCount = F, MechanicalFemaleAsianCount = F, MechanicalFemaleHawaiianCount = F, MechanicalFemaleBlackCount = F, MechanicalFemaleWhiteCount = F, MechanicalFemaleMultiRaceCount = F, MechanicalFemaleLEPCount = F, MechanicalFemaleSection504Count = F, PhysicalMaleHispanicCount = F, PhysicalMaleAlaskanCount = F, PhysicalMaleAsianCount = F, PhysicalMaleHawaiianCount = F, PhysicalMaleBlackCount = F, PhysicalMaleWhiteCount = F, PhysicalMaleMultiRaceCount = F, PhysicalMaleLEPCount = F, PhysicalMaleSection504Count = F, PhysicalFemaleHispanicCount = F, PhysicalFemaleAlaskanCount = F, PhysicalFemaleAsianCount = F, PhysicalFemaleHawaiianCount = F, PhysicalFemaleBlackCount = F, PhysicalFemaleWhiteCount = F, PhysicalFemaleMultiRaceCount = F, PhysicalFemaleLEPCount = F, PhysicalFemaleSection504Count = F, SeclusionMaleHispanicCount = F, SeclusionMaleAlaskanCount = F, SeclusionMaleAsianCount = F, SeclusionMaleHawaiianCount = F, SeclusionMaleBlackCount = F, SeclusionMaleWhiteCount = F, SeclusionMaleMultiRaceCount = F, SeclusionMaleLEPCount = F, SeclusionMaleSection504Count = F, SeclusionFemaleHispanicCount = F, SeclusionFemaleAlaskanCount = F, SeclusionFemaleAsianCount = F, SeclusionFemaleHawaiianCount = F, SeclusionFemaleBlackCount = F, SeclusionFemaleWhiteCount = F, SeclusionFemaleMultiRaceCount = F, SeclusionFemaleLEPCount = F, SeclusionFemaleSection504Count = F, CivilRightsDataCollectionID = F, CivilRightsDataCollectionRunHistoryID = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, FederalNCESSchoolID = F, searchConditionsGroupType = "And", searchSortFieldNamesList = NULL, searchSortFieldNamesDescendingList = NULL, entityID = 1, schoolYearID = NULL, page = 1, pageSize = 100000, flatten = T, ...){

		params <- as.list(environment()) %>% append(list(...))

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- "all", searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		listSkyObjects(module = "StateReportingFederal", objectName = "CRDCSchoolIDEARestraintV1", schoolYearID = schoolYearID, searchFields = searchFields, page = page, pageSize = pageSize, SearchConditionsList = searchConditionsList, SearchConditionsGroupType = searchConditionsGroupType, SearchSortFieldNamesList = searchSortFieldNamesList, SearchSortFieldNamesDescendingList = searchSortFieldNamesDescendingList, entityId = entityID, flatten = flatten)
	}


	listCRDCSchoolNonIDEARestraintV1s <- function(searchConditionsList = NULL, CRDCSchoolNonIDEARestraintV1ID = F, MechanicalMaleHispanicCount = F, MechanicalMaleAlaskanCount = F, MechanicalMaleAsianCount = F, MechanicalMaleHawaiianCount = F, MechanicalMaleBlackCount = F, MechanicalMaleWhiteCount = F, MechanicalMaleMultiRaceCount = F, MechanicalMaleLEPCount = F, MechanicalMaleSection504Count = F, MechanicalFemaleHispanicCount = F, MechanicalFemaleAlaskanCount = F, MechanicalFemaleAsianCount = F, MechanicalFemaleHawaiianCount = F, MechanicalFemaleBlackCount = F, MechanicalFemaleWhiteCount = F, MechanicalFemaleMultiRaceCount = F, MechanicalFemaleLEPCount = F, MechanicalFemaleSection504Count = F, PhysicalMaleHispanicCount = F, PhysicalMaleAlaskanCount = F, PhysicalMaleAsianCount = F, PhysicalMaleHawaiianCount = F, PhysicalMaleBlackCount = F, PhysicalMaleWhiteCount = F, PhysicalMaleMultiRaceCount = F, PhysicalMaleLEPCount = F, PhysicalMaleSection504Count = F, PhysicalFemaleHispanicCount = F, PhysicalFemaleAlaskanCount = F, PhysicalFemaleAsianCount = F, PhysicalFemaleHawaiianCount = F, PhysicalFemaleBlackCount = F, PhysicalFemaleWhiteCount = F, PhysicalFemaleMultiRaceCount = F, PhysicalFemaleLEPCount = F, PhysicalFemaleSection504Count = F, SeclusionMaleHispanicCount = F, SeclusionMaleAlaskanCount = F, SeclusionMaleAsianCount = F, SeclusionMaleHawaiianCount = F, SeclusionMaleBlackCount = F, SeclusionMaleWhiteCount = F, SeclusionMaleMultiRaceCount = F, SeclusionMaleLEPCount = F, SeclusionMaleSection504Count = F, SeclusionFemaleHispanicCount = F, SeclusionFemaleAlaskanCount = F, SeclusionFemaleAsianCount = F, SeclusionFemaleHawaiianCount = F, SeclusionFemaleBlackCount = F, SeclusionFemaleWhiteCount = F, SeclusionFemaleMultiRaceCount = F, SeclusionFemaleLEPCount = F, SeclusionFemaleSection504Count = F, CivilRightsDataCollectionID = F, CivilRightsDataCollectionRunHistoryID = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, FederalNCESSchoolID = F, searchConditionsGroupType = "And", searchSortFieldNamesList = NULL, searchSortFieldNamesDescendingList = NULL, entityID = 1, schoolYearID = NULL, page = 1, pageSize = 100000, flatten = T, ...){

		params <- as.list(environment()) %>% append(list(...))

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- "all", searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		listSkyObjects(module = "StateReportingFederal", objectName = "CRDCSchoolNonIDEARestraintV1", schoolYearID = schoolYearID, searchFields = searchFields, page = page, pageSize = pageSize, SearchConditionsList = searchConditionsList, SearchConditionsGroupType = searchConditionsGroupType, SearchSortFieldNamesList = searchSortFieldNamesList, SearchSortFieldNamesDescendingList = searchSortFieldNamesDescendingList, entityId = entityID, flatten = flatten)
	}


	listCRDCSchoolFederalSalaryV1s <- function(searchConditionsList = NULL, CRDCSchoolFederalSalaryV1ID = F, TeacherAmount = F, InstructionalAideFTE = F, InstructionalAideAmount = F, SupportStaffFTE = F, SupportStaffAmount = F, AdministrationFTE = F, AdministrationAmount = F, TotalPersonnelAmount = F, NonPersonnelAmount = F, CivilRightsDataCollectionID = F, CivilRightsDataCollectionRunHistoryID = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, FederalNCESSchoolID = F, searchConditionsGroupType = "And", searchSortFieldNamesList = NULL, searchSortFieldNamesDescendingList = NULL, entityID = 1, schoolYearID = NULL, page = 1, pageSize = 100000, flatten = T, ...){

		params <- as.list(environment()) %>% append(list(...))

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- "all", searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		listSkyObjects(module = "StateReportingFederal", objectName = "CRDCSchoolFederalSalaryV1", schoolYearID = schoolYearID, searchFields = searchFields, page = page, pageSize = pageSize, SearchConditionsList = searchConditionsList, SearchConditionsGroupType = searchConditionsGroupType, SearchSortFieldNamesList = searchSortFieldNamesList, SearchSortFieldNamesDescendingList = searchSortFieldNamesDescendingList, entityId = entityID, flatten = flatten)
	}


	listCRDCSchoolJusticeFacilityV1s <- function(searchConditionsList = NULL, CRDCSchoolJusticeFacilityV1ID = F, FederalJusticeFacilityTypeID = F, FacilityType = F, DaysInSchoolYear = F, HoursPerWeek = F, LessThan15DaysStudentCount = F, Between15To30DaysStudentCount = F, Between31To90DaysStudentCount = F, Between91To180DaysStudentCount = F, MoreThan180DaysStudentCount = F, CivilRightsDataCollectionID = F, CivilRightsDataCollectionRunHistoryID = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, FederalNCESSchoolID = F, searchConditionsGroupType = "And", searchSortFieldNamesList = NULL, searchSortFieldNamesDescendingList = NULL, entityID = 1, schoolYearID = NULL, page = 1, pageSize = 100000, flatten = T, ...){

		params <- as.list(environment()) %>% append(list(...))

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- "all", searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		listSkyObjects(module = "StateReportingFederal", objectName = "CRDCSchoolJusticeFacilityV1", schoolYearID = schoolYearID, searchFields = searchFields, page = page, pageSize = pageSize, SearchConditionsList = searchConditionsList, SearchConditionsGroupType = searchConditionsGroupType, SearchSortFieldNamesList = searchSortFieldNamesList, SearchSortFieldNamesDescendingList = searchSortFieldNamesDescendingList, entityId = entityID, flatten = flatten)
	}


	listCRDCSchoolPKCorporalPunishmentV1s <- function(searchConditionsList = NULL, CRDCSchoolPKCorporalPunishmentV1ID = F, NumberOfInstances = F, IDEANumberOfInstances = F, MaleHispanicCount = F, MaleAlaskanCount = F, MaleAsianCount = F, MaleHawaiianCount = F, MaleBlackCount = F, MaleWhiteCount = F, MaleMultiRaceCount = F, MaleLEPCount = F, MaleIDEACount = F, FemaleHispanicCount = F, FemaleAlaskanCount = F, FemaleAsianCount = F, FemaleHawaiianCount = F, FemaleBlackCount = F, FemaleWhiteCount = F, FemaleMultiRaceCount = F, FemaleLEPCount = F, FemaleIDEACount = F, CivilRightsDataCollectionID = F, CivilRightsDataCollectionRunHistoryID = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, FederalNCESSchoolID = F, searchConditionsGroupType = "And", searchSortFieldNamesList = NULL, searchSortFieldNamesDescendingList = NULL, entityID = 1, schoolYearID = NULL, page = 1, pageSize = 100000, flatten = T, ...){

		params <- as.list(environment()) %>% append(list(...))

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- "all", searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		listSkyObjects(module = "StateReportingFederal", objectName = "CRDCSchoolPKCorporalPunishmentV1", schoolYearID = schoolYearID, searchFields = searchFields, page = page, pageSize = pageSize, SearchConditionsList = searchConditionsList, SearchConditionsGroupType = searchConditionsGroupType, SearchSortFieldNamesList = searchSortFieldNamesList, SearchSortFieldNamesDescendingList = searchSortFieldNamesDescendingList, entityId = entityID, flatten = flatten)
	}


	listCRDCSchoolPKSuspensionExpulsionV1s <- function(searchConditionsList = NULL, CRDCSchoolPKSuspensionExpulsionV1ID = F, NumberOfInstances = F, IDEANumberOfInstances = F, OneSuspensionMaleHispanicCount = F, OneSuspensionMaleAlaskanCount = F, OneSuspensionMaleAsianCount = F, OneSuspensionMaleHawaiianCount = F, OneSuspensionMaleBlackCount = F, OneSuspensionMaleWhiteCount = F, OneSuspensionMaleMultiRaceCount = F, OneSuspensionMaleLEPCount = F, OneSuspensionMaleIDEACount = F, OneSuspensionFemaleHispanicCount = F, OneSuspensionFemaleAlaskanCount = F, OneSuspensionFemaleAsianCount = F, OneSuspensionFemaleHawaiianCount = F, OneSuspensionFemaleBlackCount = F, OneSuspensionFemaleWhiteCount = F, OneSuspensionFemaleMultiRaceCount = F, OneSuspensionFemaleLEPCount = F, OneSuspensionFemaleIDEACount = F, MultipleSuspensionsMaleHispanicCount = F, MultipleSuspensionsMaleAlaskanCount = F, MultipleSuspensionsMaleAsianCount = F, MultipleSuspensionsMaleHawaiianCount = F, MultipleSuspensionsMaleBlackCount = F, MultipleSuspensionsMaleWhiteCount = F, MultipleSuspensionsMaleMultiRaceCount = F, MultipleSuspensionsMaleLEPCount = F, MultipleSuspensionsMaleIDEACount = F, MultipleSuspensionsFemaleHispanicCount = F, MultipleSuspensionsFemaleAlaskanCount = F, MultipleSuspensionsFemaleAsianCount = F, MultipleSuspensionsFemaleHawaiianCount = F, MultipleSuspensionsFemaleBlackCount = F, MultipleSuspensionsFemaleWhiteCount = F, MultipleSuspensionsFemaleMultiRaceCount = F, MultipleSuspensionsFemaleLEPCount = F, MultipleSuspensionsFemaleIDEACount = F, ExpulsionMaleHispanicCount = F, ExpulsionMaleAlaskanCount = F, ExpulsionMaleAsianCount = F, ExpulsionMaleHawaiianCount = F, ExpulsionMaleBlackCount = F, ExpulsionMaleWhiteCount = F, ExpulsionMaleMultiRaceCount = F, ExpulsionMaleLEPCount = F, ExpulsionMaleIDEACount = F, ExpulsionFemaleHispanicCount = F, ExpulsionFemaleAlaskanCount = F, ExpulsionFemaleAsianCount = F, ExpulsionFemaleHawaiianCount = F, ExpulsionFemaleBlackCount = F, ExpulsionFemaleWhiteCount = F, ExpulsionFemaleMultiRaceCount = F, ExpulsionFemaleLEPCount = F, ExpulsionFemaleIDEACount = F, CivilRightsDataCollectionID = F, CivilRightsDataCollectionRunHistoryID = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, FederalNCESSchoolID = F, searchConditionsGroupType = "And", searchSortFieldNamesList = NULL, searchSortFieldNamesDescendingList = NULL, entityID = 1, schoolYearID = NULL, page = 1, pageSize = 100000, flatten = T, ...){

		params <- as.list(environment()) %>% append(list(...))

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- "all", searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		listSkyObjects(module = "StateReportingFederal", objectName = "CRDCSchoolPKSuspensionExpulsionV1", schoolYearID = schoolYearID, searchFields = searchFields, page = page, pageSize = pageSize, SearchConditionsList = searchConditionsList, SearchConditionsGroupType = searchConditionsGroupType, SearchSortFieldNamesList = searchSortFieldNamesList, SearchSortFieldNamesDescendingList = searchSortFieldNamesDescendingList, entityId = entityID, flatten = flatten)
	}


	listCRDCSchoolDaysMissedSuspensionV1s <- function(searchConditionsList = NULL, CRDCSchoolDaysMissedSuspensionV1ID = F, MaleHispanicCount = F, MaleAlaskanCount = F, MaleAsianCount = F, MaleHawaiianCount = F, MaleBlackCount = F, MaleWhiteCount = F, MaleMultiRaceCount = F, MaleLEPCount = F, MaleIDEACount = F, MaleSection504Count = F, FemaleHispanicCount = F, FemaleAlaskanCount = F, FemaleAsianCount = F, FemaleHawaiianCount = F, FemaleBlackCount = F, FemaleWhiteCount = F, FemaleMultiRaceCount = F, FemaleLEPCount = F, FemaleIDEACount = F, FemaleSection504Count = F, CivilRightsDataCollectionID = F, CivilRightsDataCollectionRunHistoryID = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, FederalNCESSchoolID = F, searchConditionsGroupType = "And", searchSortFieldNamesList = NULL, searchSortFieldNamesDescendingList = NULL, entityID = 1, schoolYearID = NULL, page = 1, pageSize = 100000, flatten = T, ...){

		params <- as.list(environment()) %>% append(list(...))

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- "all", searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		listSkyObjects(module = "StateReportingFederal", objectName = "CRDCSchoolDaysMissedSuspensionV1", schoolYearID = schoolYearID, searchFields = searchFields, page = page, pageSize = pageSize, SearchConditionsList = searchConditionsList, SearchConditionsGroupType = searchConditionsGroupType, SearchSortFieldNamesList = searchSortFieldNamesList, SearchSortFieldNamesDescendingList = searchSortFieldNamesDescendingList, entityId = entityID, flatten = flatten)
	}


	listCRDCSchoolDisciplineCountV1s <- function(searchConditionsList = NULL, CRDCSchoolDisciplineCountV1ID = F, WithDisabilityCorporalCount = F, WithoutDisabilityCorporalCount = F, WithoutDisabilityOutOfSchoolCount = F, IDEAOutOfSchoolCount = F, Section504OutOfSchoolCount = F, CivilRightsDataCollectionID = F, CivilRightsDataCollectionRunHistoryID = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, FederalNCESSchoolID = F, searchConditionsGroupType = "And", searchSortFieldNamesList = NULL, searchSortFieldNamesDescendingList = NULL, entityID = 1, schoolYearID = NULL, page = 1, pageSize = 100000, flatten = T, ...){

		params <- as.list(environment()) %>% append(list(...))

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- "all", searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		listSkyObjects(module = "StateReportingFederal", objectName = "CRDCSchoolDisciplineCountV1", schoolYearID = schoolYearID, searchFields = searchFields, page = page, pageSize = pageSize, SearchConditionsList = searchConditionsList, SearchConditionsGroupType = searchConditionsGroupType, SearchSortFieldNamesList = searchSortFieldNamesList, SearchSortFieldNamesDescendingList = searchSortFieldNamesDescendingList, entityId = entityID, flatten = flatten)
	}


	listCRDCSchoolDisciplineWithDisabilityV1s <- function(searchConditionsList = NULL, CRDCSchoolDisciplineWithDisabilityV1ID = F, CorporalMaleHispanicCount = F, CorporalMaleAlaskanCount = F, CorporalMaleAsianCount = F, CorporalMaleHawaiianCount = F, CorporalMaleBlackCount = F, CorporalMaleWhiteCount = F, CorporalMaleMultiRaceCount = F, CorporalMaleLEPCount = F, CorporalMaleSection504Count = F, CorporalFemaleHispanicCount = F, CorporalFemaleAlaskanCount = F, CorporalFemaleAsianCount = F, CorporalFemaleHawaiianCount = F, CorporalFemaleBlackCount = F, CorporalFemaleWhiteCount = F, CorporalFemaleMultiRaceCount = F, CorporalFemaleLEPCount = F, CorporalFemaleSection504Count = F, InSchoolMaleHispanicCount = F, InSchoolMaleAlaskanCount = F, InSchoolMaleAsianCount = F, InSchoolMaleHawaiianCount = F, InSchoolMaleBlackCount = F, InSchoolMaleWhiteCount = F, InSchoolMaleMultiRaceCount = F, InSchoolMaleLEPCount = F, InSchoolMaleSection504Count = F, InSchoolFemaleHispanicCount = F, InSchoolFemaleAlaskanCount = F, InSchoolFemaleAsianCount = F, InSchoolFemaleHawaiianCount = F, InSchoolFemaleBlackCount = F, InSchoolFemaleWhiteCount = F, InSchoolFemaleMultiRaceCount = F, InSchoolFemaleLEPCount = F, InSchoolFemaleSection504Count = F, OneOutOfSchoolMaleHispanicCount = F, OneOutOfSchoolMaleAlaskanCount = F, OneOutOfSchoolMaleAsianCount = F, OneOutOfSchoolMaleHawaiianCount = F, OneOutOfSchoolMaleBlackCount = F, OneOutOfSchoolMaleWhiteCount = F, OneOutOfSchoolMaleMultiRaceCount = F, OneOutOfSchoolMaleLEPCount = F, OneOutOfSchoolMaleSection504Count = F, OneOutOfSchoolFemaleHispanicCount = F, OneOutOfSchoolFemaleAlaskanCount = F, OneOutOfSchoolFemaleAsianCount = F, OneOutOfSchoolFemaleHawaiianCount = F, OneOutOfSchoolFemaleBlackCount = F, OneOutOfSchoolFemaleWhiteCount = F, OneOutOfSchoolFemaleMultiRaceCount = F, OneOutOfSchoolFemaleLEPCount = F, OneOutOfSchoolFemaleSection504Count = F, MultipleOutOfSchoolMaleHispanicCount = F, MultipleOutOfSchoolMaleAlaskanCount = F, MultipleOutOfSchoolMaleAsianCount = F, MultipleOutOfSchoolMaleHawaiianCount = F, MultipleOutOfSchoolMaleBlackCount = F, MultipleOutOfSchoolMaleWhiteCount = F, MultipleOutOfSchoolMaleMultiRaceCount = F, MultipleOutOfSchoolMaleLEPCount = F, MultipleOutOfSchoolMaleSection504Count = F, MultipleOutOfSchoolFemaleHispanicCount = F, MultipleOutOfSchoolFemaleAlaskanCount = F, MultipleOutOfSchoolFemaleAsianCount = F, MultipleOutOfSchoolFemaleHawaiianCount = F, MultipleOutOfSchoolFemaleBlackCount = F, MultipleOutOfSchoolFemaleWhiteCount = F, MultipleOutOfSchoolFemaleMultiRaceCount = F, MultipleOutOfSchoolFemaleLEPCount = F, MultipleOutOfSchoolFemaleSection504Count = F, ExpulsionWithServicesMaleHispanicCount = F, ExpulsionWithServicesMaleAlaskanCount = F, ExpulsionWithServicesMaleAsianCount = F, ExpulsionWithServicesMaleHawaiianCount = F, ExpulsionWithServicesMaleBlackCount = F, ExpulsionWithServicesMaleWhiteCount = F, ExpulsionWithServicesMaleMultiRaceCount = F, ExpulsionWithServicesMaleLEPCount = F, ExpulsionWithServicesMaleSection504Count = F, ExpulsionWithServicesFemaleHispanicCount = F, ExpulsionWithServicesFemaleAlaskanCount = F, ExpulsionWithServicesFemaleAsianCount = F, ExpulsionWithServicesFemaleHawaiianCount = F, ExpulsionWithServicesFemaleBlackCount = F, ExpulsionWithServicesFemaleWhiteCount = F, ExpulsionWithServicesFemaleMultiRaceCount = F, ExpulsionWithServicesFemaleLEPCount = F, ExpulsionWithServicesFemaleSection504Count = F, ExpulsionNoServicesMaleHispanicCount = F, ExpulsionNoServicesMaleAlaskanCount = F, ExpulsionNoServicesMaleAsianCount = F, ExpulsionNoServicesMaleHawaiianCount = F, ExpulsionNoServicesMaleBlackCount = F, ExpulsionNoServicesMaleWhiteCount = F, ExpulsionNoServicesMaleMultiRaceCount = F, ExpulsionNoServicesMaleLEPCount = F, ExpulsionNoServicesMaleSection504Count = F, ExpulsionNoServicesFemaleHispanicCount = F, ExpulsionNoServicesFemaleAlaskanCount = F, ExpulsionNoServicesFemaleAsianCount = F, ExpulsionNoServicesFemaleHawaiianCount = F, ExpulsionNoServicesFemaleBlackCount = F, ExpulsionNoServicesFemaleWhiteCount = F, ExpulsionNoServicesFemaleMultiRaceCount = F, ExpulsionNoServicesFemaleLEPCount = F, ExpulsionNoServicesFemaleSection504Count = F, ZeroToleranceMaleHispanicCount = F, ZeroToleranceMaleAlaskanCount = F, ZeroToleranceMaleAsianCount = F, ZeroToleranceMaleHawaiianCount = F, ZeroToleranceMaleBlackCount = F, ZeroToleranceMaleWhiteCount = F, ZeroToleranceMaleMultiRaceCount = F, ZeroToleranceMaleLEPCount = F, ZeroToleranceMaleSection504Count = F, ZeroToleranceFemaleHispanicCount = F, ZeroToleranceFemaleAlaskanCount = F, ZeroToleranceFemaleAsianCount = F, ZeroToleranceFemaleHawaiianCount = F, ZeroToleranceFemaleBlackCount = F, ZeroToleranceFemaleWhiteCount = F, ZeroToleranceFemaleMultiRaceCount = F, ZeroToleranceFemaleLEPCount = F, ZeroToleranceFemaleSection504Count = F, LawEnforcementMaleHispanicCount = F, LawEnforcementMaleAlaskanCount = F, LawEnforcementMaleAsianCount = F, LawEnforcementMaleHawaiianCount = F, LawEnforcementMaleBlackCount = F, LawEnforcementMaleWhiteCount = F, LawEnforcementMaleMultiRaceCount = F, LawEnforcementMaleLEPCount = F, LawEnforcementMaleSection504Count = F, LawEnforcementFemaleHispanicCount = F, LawEnforcementFemaleAlaskanCount = F, LawEnforcementFemaleAsianCount = F, LawEnforcementFemaleHawaiianCount = F, LawEnforcementFemaleBlackCount = F, LawEnforcementFemaleWhiteCount = F, LawEnforcementFemaleMultiRaceCount = F, LawEnforcementFemaleLEPCount = F, LawEnforcementFemaleSection504Count = F, ArrestMaleHispanicCount = F, ArrestMaleAlaskanCount = F, ArrestMaleAsianCount = F, ArrestMaleHawaiianCount = F, ArrestMaleBlackCount = F, ArrestMaleWhiteCount = F, ArrestMaleMultiRaceCount = F, ArrestMaleLEPCount = F, ArrestMaleSection504Count = F, ArrestFemaleHispanicCount = F, ArrestFemaleAlaskanCount = F, ArrestFemaleAsianCount = F, ArrestFemaleHawaiianCount = F, ArrestFemaleBlackCount = F, ArrestFemaleWhiteCount = F, ArrestFemaleMultiRaceCount = F, ArrestFemaleLEPCount = F, ArrestFemaleSection504Count = F, CivilRightsDataCollectionID = F, CivilRightsDataCollectionRunHistoryID = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, FederalNCESSchoolID = F, searchConditionsGroupType = "And", searchSortFieldNamesList = NULL, searchSortFieldNamesDescendingList = NULL, entityID = 1, schoolYearID = NULL, page = 1, pageSize = 100000, flatten = T, ...){

		params <- as.list(environment()) %>% append(list(...))

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- "all", searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		listSkyObjects(module = "StateReportingFederal", objectName = "CRDCSchoolDisciplineWithDisabilityV1", schoolYearID = schoolYearID, searchFields = searchFields, page = page, pageSize = pageSize, SearchConditionsList = searchConditionsList, SearchConditionsGroupType = searchConditionsGroupType, SearchSortFieldNamesList = searchSortFieldNamesList, SearchSortFieldNamesDescendingList = searchSortFieldNamesDescendingList, entityId = entityID, flatten = flatten)
	}


	listCRDCSchoolStateSalaryV1s <- function(searchConditionsList = NULL, CRDCSchoolStateSalaryV1ID = F, TeacherFTE = F, TeacherAmount = F, InstructionalAideFTE = F, InstructionalAideAmount = F, SupportStaffFTE = F, SupportStaffAmount = F, AdministrationFTE = F, AdministrationAmount = F, TotalPersonnelAmount = F, NonPersonnelAmount = F, CivilRightsDataCollectionID = F, CivilRightsDataCollectionRunHistoryID = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, FederalNCESSchoolID = F, searchConditionsGroupType = "And", searchSortFieldNamesList = NULL, searchSortFieldNamesDescendingList = NULL, entityID = 1, schoolYearID = NULL, page = 1, pageSize = 100000, flatten = T, ...){

		params <- as.list(environment()) %>% append(list(...))

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- "all", searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		listSkyObjects(module = "StateReportingFederal", objectName = "CRDCSchoolStateSalaryV1", schoolYearID = schoolYearID, searchFields = searchFields, page = page, pageSize = pageSize, SearchConditionsList = searchConditionsList, SearchConditionsGroupType = searchConditionsGroupType, SearchSortFieldNamesList = searchSortFieldNamesList, SearchSortFieldNamesDescendingList = searchSortFieldNamesDescendingList, entityId = entityID, flatten = flatten)
	}


	listCRDCSchoolTeacherAbsenteeismV1s <- function(searchConditionsList = NULL, CRDCSchoolTeacherAbsenteeismV1ID = F, FTE = F, CivilRightsDataCollectionID = F, CivilRightsDataCollectionRunHistoryID = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, FederalNCESSchoolID = F, searchConditionsGroupType = "And", searchSortFieldNamesList = NULL, searchSortFieldNamesDescendingList = NULL, entityID = 1, schoolYearID = NULL, page = 1, pageSize = 100000, flatten = T, ...){

		params <- as.list(environment()) %>% append(list(...))

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- "all", searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		listSkyObjects(module = "StateReportingFederal", objectName = "CRDCSchoolTeacherAbsenteeismV1", schoolYearID = schoolYearID, searchFields = searchFields, page = page, pageSize = pageSize, SearchConditionsList = searchConditionsList, SearchConditionsGroupType = searchConditionsGroupType, SearchSortFieldNamesList = searchSortFieldNamesList, SearchSortFieldNamesDescendingList = searchSortFieldNamesDescendingList, entityId = entityID, flatten = flatten)
	}


	listCRDCSchoolOffenseSummaryV1s <- function(searchConditionsList = NULL, CRDCSchoolOffenseSummaryV1ID = F, HadFirearmUse = F, HadHomicide = F, RapeIncidents = F, SexualAssaultIncidents = F, RobberyWithWeaponIncidents = F, RobberyWithFirearmIncidents = F, RobberyWithoutWeaponIncidents = F, FightWithWeaponIncidents = F, FightWithFirearmIncidents = F, FightWithoutWeaponIncidents = F, ThreatWithWeaponIncidents = F, ThreatWithFirearmIncidents = F, ThreatWithoutWeaponIncidents = F, PossessionOfFirearmIncidents = F, CivilRightsDataCollectionID = F, CivilRightsDataCollectionRunHistoryID = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, FederalNCESSchoolID = F, searchConditionsGroupType = "And", searchSortFieldNamesList = NULL, searchSortFieldNamesDescendingList = NULL, entityID = 1, schoolYearID = NULL, page = 1, pageSize = 100000, flatten = T, ...){

		params <- as.list(environment()) %>% append(list(...))

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- "all", searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		listSkyObjects(module = "StateReportingFederal", objectName = "CRDCSchoolOffenseSummaryV1", schoolYearID = schoolYearID, searchFields = searchFields, page = page, pageSize = pageSize, SearchConditionsList = searchConditionsList, SearchConditionsGroupType = searchConditionsGroupType, SearchSortFieldNamesList = searchSortFieldNamesList, SearchSortFieldNamesDescendingList = searchSortFieldNamesDescendingList, entityId = entityID, flatten = flatten)
	}


	listCRDCSchoolHarassmentBullyingAllegationV1s <- function(searchConditionsList = NULL, CRDCSchoolHarassmentBullyingAllegationV1ID = F, SexBasedAllegationCount = F, RaceBasedAllegationCount = F, DisabilityBasedAllegationCount = F, OrientationBasedAllegationCount = F, ReligionBasedAllegationCount = F, CivilRightsDataCollectionID = F, CivilRightsDataCollectionRunHistoryID = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, FederalNCESSchoolID = F, searchConditionsGroupType = "And", searchSortFieldNamesList = NULL, searchSortFieldNamesDescendingList = NULL, entityID = 1, schoolYearID = NULL, page = 1, pageSize = 100000, flatten = T, ...){

		params <- as.list(environment()) %>% append(list(...))

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- "all", searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		listSkyObjects(module = "StateReportingFederal", objectName = "CRDCSchoolHarassmentBullyingAllegationV1", schoolYearID = schoolYearID, searchFields = searchFields, page = page, pageSize = pageSize, SearchConditionsList = searchConditionsList, SearchConditionsGroupType = searchConditionsGroupType, SearchSortFieldNamesList = searchSortFieldNamesList, SearchSortFieldNamesDescendingList = searchSortFieldNamesDescendingList, entityId = entityID, flatten = flatten)
	}


	listCRDCSchoolHarassmentBullyingDisciplineV1s <- function(searchConditionsList = NULL, CRDCSchoolHarassmentBullyingDisciplineV1ID = F, SexMaleHispanicCount = F, SexMaleAlaskanCount = F, SexMaleAsianCount = F, SexMaleHawaiianCount = F, SexMaleBlackCount = F, SexMaleWhiteCount = F, SexMaleMultiRaceCount = F, SexMaleLEPCount = F, SexMaleIDEACount = F, SexMaleSection504Count = F, SexFemaleHispanicCount = F, SexFemaleAlaskanCount = F, SexFemaleAsianCount = F, SexFemaleHawaiianCount = F, SexFemaleBlackCount = F, SexFemaleWhiteCount = F, SexFemaleMultiRaceCount = F, SexFemaleLEPCount = F, SexFemaleIDEACount = F, SexFemaleSection504Count = F, RaceMaleHispanicCount = F, RaceMaleAlaskanCount = F, RaceMaleAsianCount = F, RaceMaleHawaiianCount = F, RaceMaleBlackCount = F, RaceMaleWhiteCount = F, RaceMaleMultiRaceCount = F, RaceMaleLEPCount = F, RaceMaleIDEACount = F, RaceMaleSection504Count = F, RaceFemaleHispanicCount = F, RaceFemaleAlaskanCount = F, RaceFemaleAsianCount = F, RaceFemaleHawaiianCount = F, RaceFemaleBlackCount = F, RaceFemaleWhiteCount = F, RaceFemaleMultiRaceCount = F, RaceFemaleLEPCount = F, RaceFemaleIDEACount = F, RaceFemaleSection504Count = F, DisabilityMaleHispanicCount = F, DisabilityMaleAlaskanCount = F, DisabilityMaleAsianCount = F, DisabilityMaleHawaiianCount = F, DisabilityMaleBlackCount = F, DisabilityMaleWhiteCount = F, DisabilityMaleMultiRaceCount = F, DisabilityMaleLEPCount = F, DisabilityMaleIDEACount = F, DisabilityMaleSection504Count = F, DisabilityFemaleHispanicCount = F, DisabilityFemaleAlaskanCount = F, DisabilityFemaleAsianCount = F, DisabilityFemaleHawaiianCount = F, DisabilityFemaleBlackCount = F, DisabilityFemaleWhiteCount = F, DisabilityFemaleMultiRaceCount = F, DisabilityFemaleLEPCount = F, DisabilityFemaleIDEACount = F, DisabilityFemaleSection504Count = F, CivilRightsDataCollectionID = F, CivilRightsDataCollectionRunHistoryID = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, FederalNCESSchoolID = F, searchConditionsGroupType = "And", searchSortFieldNamesList = NULL, searchSortFieldNamesDescendingList = NULL, entityID = 1, schoolYearID = NULL, page = 1, pageSize = 100000, flatten = T, ...){

		params <- as.list(environment()) %>% append(list(...))

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- "all", searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		listSkyObjects(module = "StateReportingFederal", objectName = "CRDCSchoolHarassmentBullyingDisciplineV1", schoolYearID = schoolYearID, searchFields = searchFields, page = page, pageSize = pageSize, SearchConditionsList = searchConditionsList, SearchConditionsGroupType = searchConditionsGroupType, SearchSortFieldNamesList = searchSortFieldNamesList, SearchSortFieldNamesDescendingList = searchSortFieldNamesDescendingList, entityId = entityID, flatten = flatten)
	}


	listCRDCSchoolHarassmentBullyingReportedV1s <- function(searchConditionsList = NULL, CRDCSchoolHarassmentBullyingReportedV1ID = F, SexMaleHispanicCount = F, SexMaleAlaskanCount = F, SexMaleAsianCount = F, SexMaleHawaiianCount = F, SexMaleBlackCount = F, SexMaleWhiteCount = F, SexMaleMultiRaceCount = F, SexMaleLEPCount = F, SexMaleIDEACount = F, SexMaleSection504Count = F, SexFemaleHispanicCount = F, SexFemaleAlaskanCount = F, SexFemaleAsianCount = F, SexFemaleHawaiianCount = F, SexFemaleBlackCount = F, SexFemaleWhiteCount = F, SexFemaleMultiRaceCount = F, SexFemaleLEPCount = F, SexFemaleIDEACount = F, SexFemaleSection504Count = F, RaceMaleHispanicCount = F, RaceMaleAlaskanCount = F, RaceMaleAsianCount = F, RaceMaleHawaiianCount = F, RaceMaleBlackCount = F, RaceMaleWhiteCount = F, RaceMaleMultiRaceCount = F, RaceMaleLEPCount = F, RaceMaleIDEACount = F, RaceMaleSection504Count = F, RaceFemaleHispanicCount = F, RaceFemaleAlaskanCount = F, RaceFemaleAsianCount = F, RaceFemaleHawaiianCount = F, RaceFemaleBlackCount = F, RaceFemaleWhiteCount = F, RaceFemaleMultiRaceCount = F, RaceFemaleLEPCount = F, RaceFemaleIDEACount = F, RaceFemaleSection504Count = F, DisabilityMaleHispanicCount = F, DisabilityMaleAlaskanCount = F, DisabilityMaleAsianCount = F, DisabilityMaleHawaiianCount = F, DisabilityMaleBlackCount = F, DisabilityMaleWhiteCount = F, DisabilityMaleMultiRaceCount = F, DisabilityMaleLEPCount = F, DisabilityMaleIDEACount = F, DisabilityMaleSection504Count = F, DisabilityFemaleHispanicCount = F, DisabilityFemaleAlaskanCount = F, DisabilityFemaleAsianCount = F, DisabilityFemaleHawaiianCount = F, DisabilityFemaleBlackCount = F, DisabilityFemaleWhiteCount = F, DisabilityFemaleMultiRaceCount = F, DisabilityFemaleLEPCount = F, DisabilityFemaleIDEACount = F, DisabilityFemaleSection504Count = F, CivilRightsDataCollectionID = F, CivilRightsDataCollectionRunHistoryID = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, FederalNCESSchoolID = F, searchConditionsGroupType = "And", searchSortFieldNamesList = NULL, searchSortFieldNamesDescendingList = NULL, entityID = 1, schoolYearID = NULL, page = 1, pageSize = 100000, flatten = T, ...){

		params <- as.list(environment()) %>% append(list(...))

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- "all", searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		listSkyObjects(module = "StateReportingFederal", objectName = "CRDCSchoolHarassmentBullyingReportedV1", schoolYearID = schoolYearID, searchFields = searchFields, page = page, pageSize = pageSize, SearchConditionsList = searchConditionsList, SearchConditionsGroupType = searchConditionsGroupType, SearchSortFieldNamesList = searchSortFieldNamesList, SearchSortFieldNamesDescendingList = searchSortFieldNamesDescendingList, entityId = entityID, flatten = flatten)
	}


	listCRDCSchoolRestraintInstanceV1s <- function(searchConditionsList = NULL, CRDCSchoolRestraintInstanceV1ID = F, MechanicalWithoutDisabilityInstances = F, PhysicalWithoutDisabilityInstances = F, SeclusionWithoutDisabilityInstances = F, MechanicalIDEAInstances = F, PhysicalIDEAInstances = F, SeclusionIDEAInstances = F, MechanicalSection504Instances = F, PhysicalSection504Instances = F, SeclusionSection504Instances = F, CivilRightsDataCollectionID = F, CivilRightsDataCollectionRunHistoryID = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, FederalNCESSchoolID = F, searchConditionsGroupType = "And", searchSortFieldNamesList = NULL, searchSortFieldNamesDescendingList = NULL, entityID = 1, schoolYearID = NULL, page = 1, pageSize = 100000, flatten = T, ...){

		params <- as.list(environment()) %>% append(list(...))

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- "all", searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		listSkyObjects(module = "StateReportingFederal", objectName = "CRDCSchoolRestraintInstanceV1", schoolYearID = schoolYearID, searchFields = searchFields, page = page, pageSize = pageSize, SearchConditionsList = searchConditionsList, SearchConditionsGroupType = searchConditionsGroupType, SearchSortFieldNamesList = searchSortFieldNamesList, SearchSortFieldNamesDescendingList = searchSortFieldNamesDescendingList, entityId = entityID, flatten = flatten)
	}


	listCRDCStudentDetails <- function(searchConditionsList = NULL, CRDCStudentDetailID = F, StudentID = F, CivilRightsDataCollectionID = F, CivilRightsDataCollectionRunHistoryID = F, DistrictID = F, SchoolID = F, FullName = F, Age = F, GradeLevel = F, IsHispanic = F, IsAlaskan = F, IsAsian = F, IsBlack = F, IsHawaiian = F, IsWhite = F, IsMultiRace = F, IsLEP = F, IsLEPSnapshot = F, IsSection504 = F, IsSection504Snapshot = F, IsIDEA = F, IsIDEASnapshot = F, IsChronicallyAbsent = F, IsGiftedTalented = F, ReportDate = F, IDEADate = F, EnrollmentType = F, CumulativeEnrollmentPromotionStatusCode = F, IsNonLEA = F, HasTakenACT0912 = F, HasTakenSAT0912 = F, CorporalPunishmentIncidents = F, InSchoolSuspensionCount = F, OutOfSchoolSuspensionCount = F, ExpulsionWithServicesCount = F, ExpulsionWithoutServicesCount = F, ZeroToleranceWithServicesCount = F, ZeroToleranceWithoutServicesCount = F, LawEnforcementReferralCount = F, ArrestCount = F, OutOfSchoolSuspensionMissedDays = F, IsFederalDistanceEducation = F, IsFederalDualEnrollment = F, HasAlgebraIGrade07 = F, HasAlgebraIGrade08 = F, HasAlgebraIGrade0910 = F, HasAlgebraIGrade1112 = F, HasAlgebraIIGrade0912 = F, HasAdvancedMathGrade0912 = F, HasCalculusGrade0912 = F, HasGeometryGrade08 = F, HasGeometryGrade0912 = F, HasBiologyGrade0912 = F, HasChemistryGrade0912 = F, HasPhysicsGrade0912 = F, HasAPCourseGrade0912 = F, HasAPMathGrade0912 = F, HasAPScienceGrade0912 = F, HasAPOtherGrade0912 = F, IsEnrolled = F, HasPassedAlgebraIGrade07 = F, HasPassedAlgebraIGrade08 = F, HasPassedAlgebraIGrade0910 = F, HasPassedAlgebraIGrade1112 = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, Gender = F, FederalNCESSchoolID = F, HasComputerScienceGrade0912 = F, HasAPComputerScienceGrade0912 = F, HasCreditRecovery = F, SingleSexAthleticCount = F, ReportedBulliedSex = F, ReportedBulliedRace = F, ReportedBulliedDisability = F, DisciplinedBullyingSex = F, DisciplinedBullyingRace = F, DisciplinedBullyingDisability = F, MechanicalRestraintCount = F, PhysicalRestraintCount = F, SeclusionCount = F, TransferToAlternativeSchool = F, searchConditionsGroupType = "And", searchSortFieldNamesList = NULL, searchSortFieldNamesDescendingList = NULL, entityID = 1, schoolYearID = NULL, page = 1, pageSize = 100000, flatten = T, ...){

		params <- as.list(environment()) %>% append(list(...))

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- "all", searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		listSkyObjects(module = "StateReportingFederal", objectName = "CRDCStudentDetail", schoolYearID = schoolYearID, searchFields = searchFields, page = page, pageSize = pageSize, SearchConditionsList = searchConditionsList, SearchConditionsGroupType = searchConditionsGroupType, SearchSortFieldNamesList = searchSortFieldNamesList, SearchSortFieldNamesDescendingList = searchSortFieldNamesDescendingList, entityId = entityID, flatten = flatten)
	}


	listCRDCSchoolIDEASection504V1s <- function(searchConditionsList = NULL, CRDCSchoolIDEASection504V1ID = F, IDEAMaleHispanicCount = F, IDEAMaleAlaskanCount = F, IDEAMaleAsianCount = F, IDEAMaleBlackCount = F, IDEAMaleHawaiianCount = F, IDEAMaleWhiteCount = F, IDEAMaleMultiRaceCount = F, IDEAMaleLEPCount = F, IDEAFemaleHispanicCount = F, IDEAFemaleAlaskanCount = F, IDEAFemaleAsianCount = F, IDEAFemaleBlackCount = F, IDEAFemaleHawaiianCount = F, IDEAFemaleWhiteCount = F, IDEAFemaleMultiRaceCount = F, IDEAFemaleLEPCount = F, Section504MaleHispanicCount = F, Section504MaleAlaskanCount = F, Section504MaleAsianCount = F, Section504MaleBlackCount = F, Section504MaleHawaiianCount = F, Section504MaleWhiteCount = F, Section504MaleMultiRaceCount = F, Section504MaleLEPCount = F, Section504FemaleHispanicCount = F, Section504FemaleAlaskanCount = F, Section504FemaleAsianCount = F, Section504FemaleBlackCount = F, Section504FemaleHawaiianCount = F, Section504FemaleWhiteCount = F, Section504FemaleMultiRaceCount = F, Section504FemaleLEPCount = F, CivilRightsDataCollectionID = F, CivilRightsDataCollectionRunHistoryID = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, FederalNCESSchoolID = F, searchConditionsGroupType = "And", searchSortFieldNamesList = NULL, searchSortFieldNamesDescendingList = NULL, entityID = 1, schoolYearID = NULL, page = 1, pageSize = 100000, flatten = T, ...){

		params <- as.list(environment()) %>% append(list(...))

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- "all", searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		listSkyObjects(module = "StateReportingFederal", objectName = "CRDCSchoolIDEASection504V1", schoolYearID = schoolYearID, searchFields = searchFields, page = page, pageSize = pageSize, SearchConditionsList = searchConditionsList, SearchConditionsGroupType = searchConditionsGroupType, SearchSortFieldNamesList = searchSortFieldNamesList, SearchSortFieldNamesDescendingList = searchSortFieldNamesDescendingList, entityId = entityID, flatten = flatten)
	}


	listCRDCSchoolLEPV1s <- function(searchConditionsList = NULL, CRDCSchoolLEPV1ID = F, StudentsMaleHispanicCount = F, StudentsMaleAlaskanCount = F, StudentsMaleAsianCount = F, StudentsMaleBlackCount = F, StudentsMaleHawaiianCount = F, StudentsMaleWhiteCount = F, StudentsMaleMultiRaceCount = F, StudentsFemaleHispanicCount = F, StudentsFemaleAlaskanCount = F, StudentsFemaleAsianCount = F, StudentsFemaleBlackCount = F, StudentsFemaleHawaiianCount = F, StudentsFemaleWhiteCount = F, StudentsFemaleMultiRaceCount = F, EnrolledMaleHispanicCount = F, EnrolledMaleAlaskanCount = F, EnrolledMaleAsianCount = F, EnrolledMaleBlackCount = F, EnrolledMaleHawaiianCount = F, EnrolledMaleWhiteCount = F, EnrolledMaleMultiRaceCount = F, EnrolledMaleIDEACount = F, EnrolledFemaleHispanicCount = F, EnrolledFemaleAlaskanCount = F, EnrolledFemaleAsianCount = F, EnrolledFemaleBlackCount = F, EnrolledFemaleHawaiianCount = F, EnrolledFemaleWhiteCount = F, EnrolledFemaleMultiRaceCount = F, EnrolledFemaleIDEACount = F, CivilRightsDataCollectionID = F, CivilRightsDataCollectionRunHistoryID = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, FederalNCESSchoolID = F, searchConditionsGroupType = "And", searchSortFieldNamesList = NULL, searchSortFieldNamesDescendingList = NULL, entityID = 1, schoolYearID = NULL, page = 1, pageSize = 100000, flatten = T, ...){

		params <- as.list(environment()) %>% append(list(...))

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- "all", searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		listSkyObjects(module = "StateReportingFederal", objectName = "CRDCSchoolLEPV1", schoolYearID = schoolYearID, searchFields = searchFields, page = page, pageSize = pageSize, SearchConditionsList = searchConditionsList, SearchConditionsGroupType = searchConditionsGroupType, SearchSortFieldNamesList = searchSortFieldNamesList, SearchSortFieldNamesDescendingList = searchSortFieldNamesDescendingList, entityId = entityID, flatten = flatten)
	}


	listCRDCSchoolEnrollmentV1s <- function(searchConditionsList = NULL, CRDCSchoolEnrollmentV1ID = F, MaleHispanicCount = F, MaleAlaskanCount = F, MaleAsianCount = F, MaleBlackCount = F, MaleHawaiianCount = F, MaleWhiteCount = F, MaleMultiRaceCount = F, MaleIDEACount = F, MaleLEPCount = F, MaleSection504Count = F, FemaleHispanicCount = F, FemaleAlaskanCount = F, FemaleAsianCount = F, FemaleBlackCount = F, FemaleHawaiianCount = F, FemaleWhiteCount = F, FemaleMultiRaceCount = F, FemaleIDEACount = F, FemaleLEPCount = F, FemaleSection504Count = F, CivilRightsDataCollectionID = F, CivilRightsDataCollectionRunHistoryID = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, FederalNCESSchoolID = F, searchConditionsGroupType = "And", searchSortFieldNamesList = NULL, searchSortFieldNamesDescendingList = NULL, entityID = 1, schoolYearID = NULL, page = 1, pageSize = 100000, flatten = T, ...){

		params <- as.list(environment()) %>% append(list(...))

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- "all", searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		listSkyObjects(module = "StateReportingFederal", objectName = "CRDCSchoolEnrollmentV1", schoolYearID = schoolYearID, searchFields = searchFields, page = page, pageSize = pageSize, SearchConditionsList = searchConditionsList, SearchConditionsGroupType = searchConditionsGroupType, SearchSortFieldNamesList = searchSortFieldNamesList, SearchSortFieldNamesDescendingList = searchSortFieldNamesDescendingList, entityId = entityID, flatten = flatten)
	}


	listCRDCSchoolHSAlgebraGeometryV1s <- function(searchConditionsList = NULL, CRDCSchoolHSAlgebraGeometryV1ID = F, AlgebraIClassCount = F, GeometryClassCount = F, AlgebraIGrade0910MaleHispanicCount = F, AlgebraIGrade0910MaleAlaskanCount = F, AlgebraIGrade0910MaleAsianCount = F, AlgebraIGrade0910MaleBlackCount = F, AlgebraIGrade0910MaleHawaiianCount = F, AlgebraIGrade0910MaleWhiteCount = F, AlgebraIGrade0910MaleMultiRaceCount = F, AlgebraIGrade0910MaleLEPCount = F, AlgebraIGrade0910MaleIDEACount = F, AlgebraIGrade0910FemaleHispanicCount = F, AlgebraIGrade0910FemaleAlaskanCount = F, AlgebraIGrade0910FemaleAsianCount = F, AlgebraIGrade0910FemaleBlackCount = F, AlgebraIGrade0910FemaleHawaiianCount = F, AlgebraIGrade0910FemaleWhiteCount = F, AlgebraIGrade0910FemaleMultiRaceCount = F, AlgebraIGrade0910FemaleLEPCount = F, AlgebraIGrade0910FemaleIDEACount = F, AlgebraIGrade1112MaleHispanicCount = F, AlgebraIGrade1112MaleAlaskanCount = F, AlgebraIGrade1112MaleAsianCount = F, AlgebraIGrade1112MaleBlackCount = F, AlgebraIGrade1112MaleHawaiianCount = F, AlgebraIGrade1112MaleWhiteCount = F, AlgebraIGrade1112MaleMultiRaceCount = F, AlgebraIGrade1112MaleLEPCount = F, AlgebraIGrade1112MaleIDEACount = F, AlgebraIGrade1112FemaleHispanicCount = F, AlgebraIGrade1112FemaleAlaskanCount = F, AlgebraIGrade1112FemaleAsianCount = F, AlgebraIGrade1112FemaleBlackCount = F, AlgebraIGrade1112FemaleHawaiianCount = F, AlgebraIGrade1112FemaleWhiteCount = F, AlgebraIGrade1112FemaleMultiRaceCount = F, AlgebraIGrade1112FemaleLEPCount = F, AlgebraIGrade1112FemaleIDEACount = F, GeometryMaleHispanicCount = F, GeometryMaleAlaskanCount = F, GeometryMaleAsianCount = F, GeometryMaleBlackCount = F, GeometryMaleHawaiianCount = F, GeometryMaleWhiteCount = F, GeometryMaleMultiRaceCount = F, GeometryMaleLEPCount = F, GeometryMaleIDEACount = F, GeometryFemaleHispanicCount = F, GeometryFemaleAlaskanCount = F, GeometryFemaleAsianCount = F, GeometryFemaleBlackCount = F, GeometryFemaleHawaiianCount = F, GeometryFemaleWhiteCount = F, GeometryFemaleMultiRaceCount = F, GeometryFemaleLEPCount = F, GeometryFemaleIDEACount = F, CivilRightsDataCollectionID = F, CivilRightsDataCollectionRunHistoryID = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, FederalNCESSchoolID = F, searchConditionsGroupType = "And", searchSortFieldNamesList = NULL, searchSortFieldNamesDescendingList = NULL, entityID = 1, schoolYearID = NULL, page = 1, pageSize = 100000, flatten = T, ...){

		params <- as.list(environment()) %>% append(list(...))

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- "all", searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		listSkyObjects(module = "StateReportingFederal", objectName = "CRDCSchoolHSAlgebraGeometryV1", schoolYearID = schoolYearID, searchFields = searchFields, page = page, pageSize = pageSize, SearchConditionsList = searchConditionsList, SearchConditionsGroupType = searchConditionsGroupType, SearchSortFieldNamesList = searchSortFieldNamesList, SearchSortFieldNamesDescendingList = searchSortFieldNamesDescendingList, entityId = entityID, flatten = flatten)
	}


	listCRDCLEACredentialV1s <- function(searchConditionsList = NULL, CRDCLEACredentialV1ID = F, DistrictID = F, MaleHispanicCount = F, MaleAlaskanCount = F, MaleAsianCount = F, MaleHawaiianCount = F, MaleBlackCount = F, MaleWhiteCount = F, MaleMultiRaceCount = F, MaleLEPCount = F, MaleIDEACount = F, FemaleHispanicCount = F, FemaleAlaskanCount = F, FemaleAsianCount = F, FemaleHawaiianCount = F, FemaleBlackCount = F, FemaleWhiteCount = F, FemaleMultiRaceCount = F, FemaleLEPCount = F, FemaleIDEACount = F, CivilRightsDataCollectionID = F, CivilRightsDataCollectionRunHistoryID = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, searchConditionsGroupType = "And", searchSortFieldNamesList = NULL, searchSortFieldNamesDescendingList = NULL, entityID = 1, schoolYearID = NULL, page = 1, pageSize = 100000, flatten = T, ...){

		params <- as.list(environment()) %>% append(list(...))

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- "all", searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		listSkyObjects(module = "StateReportingFederal", objectName = "CRDCLEACredentialV1", schoolYearID = schoolYearID, searchFields = searchFields, page = page, pageSize = pageSize, SearchConditionsList = searchConditionsList, SearchConditionsGroupType = searchConditionsGroupType, SearchSortFieldNamesList = searchSortFieldNamesList, SearchSortFieldNamesDescendingList = searchSortFieldNamesDescendingList, entityId = entityID, flatten = flatten)
	}


	listCRDCLEAEquivalencyV1s <- function(searchConditionsList = NULL, CRDCLEAEquivalencyV1ID = F, DistrictID = F, HasPreparationProgram = F, MaleHispanicCount = F, MaleAlaskanCount = F, MaleAsianCount = F, MaleHawaiianCount = F, MaleBlackCount = F, MaleWhiteCount = F, MaleMultiRaceCount = F, MaleLEPCount = F, MaleIDEACount = F, FemaleHispanicCount = F, FemaleAlaskanCount = F, FemaleAsianCount = F, FemaleHawaiianCount = F, FemaleBlackCount = F, FemaleWhiteCount = F, FemaleMultiRaceCount = F, FemaleLEPCount = F, FemaleIDEACount = F, CivilRightsDataCollectionID = F, CivilRightsDataCollectionRunHistoryID = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, searchConditionsGroupType = "And", searchSortFieldNamesList = NULL, searchSortFieldNamesDescendingList = NULL, entityID = 1, schoolYearID = NULL, page = 1, pageSize = 100000, flatten = T, ...){

		params <- as.list(environment()) %>% append(list(...))

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- "all", searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		listSkyObjects(module = "StateReportingFederal", objectName = "CRDCLEAEquivalencyV1", schoolYearID = schoolYearID, searchFields = searchFields, page = page, pageSize = pageSize, SearchConditionsList = searchConditionsList, SearchConditionsGroupType = searchConditionsGroupType, SearchSortFieldNamesList = searchSortFieldNamesList, SearchSortFieldNamesDescendingList = searchSortFieldNamesDescendingList, entityId = entityID, flatten = flatten)
	}


	listCRDCSchoolCharacteristicV1s <- function(searchConditionsList = NULL, CRDCSchoolCharacteristicV1ID = F, FederalAlternativeSchoolDetailID = F, AlternativeSchoolFocus = F, HasPreschoolNonIDEAAge3 = F, HasPreschoolNonIDEAAge4 = F, HasPreschoolNonIDEAAge5 = F, HasUngradedElementary = F, HasUngradedMiddleSchool = F, HasUngradedHighSchool = F, IsSpecialEducation = F, IsMagnet = F, IsCharter = F, IsAlternative = F, IsEntireSchoolMagnet = F, HasUngradedElementaryMiddleSchool = F, HasUngradedMiddleSchoolHighSchool = F, HasUngradedElementaryMiddleSchoolHighSchool = F, IsAlternativeAcademic = F, IsAlternativeDiscipline = F, IsAlternativeAcademicDiscipline = F, CivilRightsDataCollectionID = F, CivilRightsDataCollectionRunHistoryID = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, FederalNCESSchoolID = F, searchConditionsGroupType = "And", searchSortFieldNamesList = NULL, searchSortFieldNamesDescendingList = NULL, entityID = 1, schoolYearID = NULL, page = 1, pageSize = 100000, flatten = T, ...){

		params <- as.list(environment()) %>% append(list(...))

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- "all", searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		listSkyObjects(module = "StateReportingFederal", objectName = "CRDCSchoolCharacteristicV1", schoolYearID = schoolYearID, searchFields = searchFields, page = page, pageSize = pageSize, SearchConditionsList = searchConditionsList, SearchConditionsGroupType = searchConditionsGroupType, SearchSortFieldNamesList = searchSortFieldNamesList, SearchSortFieldNamesDescendingList = searchSortFieldNamesDescendingList, entityId = entityID, flatten = flatten)
	}


	listCRDCSchoolGradeV1s <- function(searchConditionsList = NULL, CRDCSchoolGradeV1ID = F, NCESIDCode = F, HasPreschool = F, HasKindergarten = F, HasGrade01 = F, HasGrade02 = F, HasGrade03 = F, HasGrade04 = F, HasGrade05 = F, HasGrade06 = F, HasGrade07 = F, HasGrade08 = F, HasGrade09 = F, HasGrade10 = F, HasGrade11 = F, HasGrade12 = F, HasUngraded = F, CivilRightsDataCollectionID = F, CivilRightsDataCollectionRunHistoryID = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, FederalNCESSchoolID = F, searchConditionsGroupType = "And", searchSortFieldNamesList = NULL, searchSortFieldNamesDescendingList = NULL, entityID = 1, schoolYearID = NULL, page = 1, pageSize = 100000, flatten = T, ...){

		params <- as.list(environment()) %>% append(list(...))

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- "all", searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		listSkyObjects(module = "StateReportingFederal", objectName = "CRDCSchoolGradeV1", schoolYearID = schoolYearID, searchFields = searchFields, page = page, pageSize = pageSize, SearchConditionsList = searchConditionsList, SearchConditionsGroupType = searchConditionsGroupType, SearchSortFieldNamesList = searchSortFieldNamesList, SearchSortFieldNamesDescendingList = searchSortFieldNamesDescendingList, entityId = entityID, flatten = flatten)
	}


	listCRDCSchoolPreschoolV1s <- function(searchConditionsList = NULL, CRDCSchoolPreschoolV1ID = F, MaleHispanicCount = F, MaleAlaskanCount = F, MaleAsianCount = F, MaleHawaiianCount = F, MaleBlackCount = F, MaleWhiteCount = F, MaleMultiRaceCount = F, MaleLEPCount = F, MaleIDEACount = F, FemaleHispanicCount = F, FemaleAlaskanCount = F, FemaleAsianCount = F, FemaleHawaiianCount = F, FemaleBlackCount = F, FemaleWhiteCount = F, FemaleMultiRaceCount = F, FemaleLEPCount = F, FemaleIDEACount = F, CivilRightsDataCollectionID = F, CivilRightsDataCollectionRunHistoryID = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, FederalNCESSchoolID = F, searchConditionsGroupType = "And", searchSortFieldNamesList = NULL, searchSortFieldNamesDescendingList = NULL, entityID = 1, schoolYearID = NULL, page = 1, pageSize = 100000, flatten = T, ...){

		params <- as.list(environment()) %>% append(list(...))

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- "all", searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		listSkyObjects(module = "StateReportingFederal", objectName = "CRDCSchoolPreschoolV1", schoolYearID = schoolYearID, searchFields = searchFields, page = page, pageSize = pageSize, SearchConditionsList = searchConditionsList, SearchConditionsGroupType = searchConditionsGroupType, SearchSortFieldNamesList = searchSortFieldNamesList, SearchSortFieldNamesDescendingList = searchSortFieldNamesDescendingList, entityId = entityID, flatten = flatten)
	}


	listCRDCSchoolGiftedEnrollmentV1s <- function(searchConditionsList = NULL, CRDCSchoolGiftedEnrollmentV1ID = F, HasStudents = F, MaleHispanicCount = F, MaleAlaskanCount = F, MaleAsianCount = F, MaleHawaiianCount = F, MaleBlackCount = F, MaleWhiteCount = F, MaleMultiRaceCount = F, MaleLEPCount = F, MaleIDEACount = F, FemaleHispanicCount = F, FemaleAlaskanCount = F, FemaleAsianCount = F, FemaleHawaiianCount = F, FemaleBlackCount = F, FemaleWhiteCount = F, FemaleMultiRaceCount = F, FemaleLEPCount = F, FemaleIDEACount = F, CivilRightsDataCollectionID = F, CivilRightsDataCollectionRunHistoryID = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, FederalNCESSchoolID = F, searchConditionsGroupType = "And", searchSortFieldNamesList = NULL, searchSortFieldNamesDescendingList = NULL, entityID = 1, schoolYearID = NULL, page = 1, pageSize = 100000, flatten = T, ...){

		params <- as.list(environment()) %>% append(list(...))

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- "all", searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		listSkyObjects(module = "StateReportingFederal", objectName = "CRDCSchoolGiftedEnrollmentV1", schoolYearID = schoolYearID, searchFields = searchFields, page = page, pageSize = pageSize, SearchConditionsList = searchConditionsList, SearchConditionsGroupType = searchConditionsGroupType, SearchSortFieldNamesList = searchSortFieldNamesList, SearchSortFieldNamesDescendingList = searchSortFieldNamesDescendingList, entityId = entityID, flatten = flatten)
	}


	listCRDCSchoolMSMathV1s <- function(searchConditionsList = NULL, CRDCSchoolMSMathV1ID = F, AlgebraIClassCount = F, AlgebraICertifiedClassCount = F, HasAlgebraIGrade07 = F, AlgebraIGrade07StudentCount = F, HasAlgebraIGrade08 = F, AlgebraIGrade08StudentCount = F, HasGeometryGrade08 = F, GeometryGrade08StudentCount = F, AlgebraIGrade08MaleHispanicCount = F, AlgebraIGrade08MaleAlaskanCount = F, AlgebraIGrade08MaleAsianCount = F, AlgebraIGrade08MaleHawaiianCount = F, AlgebraIGrade08MaleBlackCount = F, AlgebraIGrade08MaleWhiteCount = F, AlgebraIGrade08MaleMultiRaceCount = F, AlgebraIGrade08MaleLEPCount = F, AlgebraIGrade08MaleIDEACount = F, AlgebraIGrade08FemaleHispanicCount = F, AlgebraIGrade08FemaleAlaskanCount = F, AlgebraIGrade08FemaleAsianCount = F, AlgebraIGrade08FemaleHawaiianCount = F, AlgebraIGrade08FemaleBlackCount = F, AlgebraIGrade08FemaleWhiteCount = F, AlgebraIGrade08FemaleMultiRaceCount = F, AlgebraIGrade08FemaleLEPCount = F, AlgebraIGrade08FemaleIDEACount = F, CivilRightsDataCollectionID = F, CivilRightsDataCollectionRunHistoryID = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, FederalNCESSchoolID = F, searchConditionsGroupType = "And", searchSortFieldNamesList = NULL, searchSortFieldNamesDescendingList = NULL, entityID = 1, schoolYearID = NULL, page = 1, pageSize = 100000, flatten = T, ...){

		params <- as.list(environment()) %>% append(list(...))

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- "all", searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		listSkyObjects(module = "StateReportingFederal", objectName = "CRDCSchoolMSMathV1", schoolYearID = schoolYearID, searchFields = searchFields, page = page, pageSize = pageSize, SearchConditionsList = searchConditionsList, SearchConditionsGroupType = searchConditionsGroupType, SearchSortFieldNamesList = searchSortFieldNamesList, SearchSortFieldNamesDescendingList = searchSortFieldNamesDescendingList, entityId = entityID, flatten = flatten)
	}


	listCRDCSchoolAdvancedMathV1s <- function(searchConditionsList = NULL, CRDCSchoolAdvancedMathV1ID = F, AlgebraICertifiedClassCount = F, GeometryCertifiedClassCount = F, AlgebraIIClassCount = F, AlgebraIICertifiedClassCount = F, AlgebraIIMaleHispanicCount = F, AlgebraIIMaleAlaskanCount = F, AlgebraIIMaleAsianCount = F, AlgebraIIMaleHawaiianCount = F, AlgebraIIMaleBlackCount = F, AlgebraIIMaleWhiteCount = F, AlgebraIIMaleMultiRaceCount = F, AlgebraIIMaleLEPCount = F, AlgebraIIMaleIDEACount = F, AlgebraIIFemaleHispanicCount = F, AlgebraIIFemaleAlaskanCount = F, AlgebraIIFemaleAsianCount = F, AlgebraIIFemaleHawaiianCount = F, AlgebraIIFemaleBlackCount = F, AlgebraIIFemaleWhiteCount = F, AlgebraIIFemaleMultiRaceCount = F, AlgebraIIFemaleLEPCount = F, AlgebraIIFemaleIDEACount = F, AdvancedClassCount = F, AdvancedCertifiedClassCount = F, AdvancedMaleHispanicCount = F, AdvancedMaleAlaskanCount = F, AdvancedMaleAsianCount = F, AdvancedMaleHawaiianCount = F, AdvancedMaleBlackCount = F, AdvancedMaleWhiteCount = F, AdvancedMaleMultiRaceCount = F, AdvancedMaleLEPCount = F, AdvancedMaleIDEACount = F, AdvancedFemaleHispanicCount = F, AdvancedFemaleAlaskanCount = F, AdvancedFemaleAsianCount = F, AdvancedFemaleHawaiianCount = F, AdvancedFemaleBlackCount = F, AdvancedFemaleWhiteCount = F, AdvancedFemaleMultiRaceCount = F, AdvancedFemaleLEPCount = F, AdvancedFemaleIDEACount = F, CalculusClassCount = F, CalculusCertifiedClassCount = F, CalculusMaleHispanicCount = F, CalculusMaleAlaskanCount = F, CalculusMaleAsianCount = F, CalculusMaleHawaiianCount = F, CalculusMaleBlackCount = F, CalculusMaleWhiteCount = F, CalculusMaleMultiRaceCount = F, CalculusMaleLEPCount = F, CalculusMaleIDEACount = F, CalculusFemaleHispanicCount = F, CalculusFemaleAlaskanCount = F, CalculusFemaleAsianCount = F, CalculusFemaleHawaiianCount = F, CalculusFemaleBlackCount = F, CalculusFemaleWhiteCount = F, CalculusFemaleMultiRaceCount = F, CalculusFemaleLEPCount = F, CalculusFemaleIDEACount = F, CivilRightsDataCollectionID = F, CivilRightsDataCollectionRunHistoryID = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, FederalNCESSchoolID = F, searchConditionsGroupType = "And", searchSortFieldNamesList = NULL, searchSortFieldNamesDescendingList = NULL, entityID = 1, schoolYearID = NULL, page = 1, pageSize = 100000, flatten = T, ...){

		params <- as.list(environment()) %>% append(list(...))

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- "all", searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		listSkyObjects(module = "StateReportingFederal", objectName = "CRDCSchoolAdvancedMathV1", schoolYearID = schoolYearID, searchFields = searchFields, page = page, pageSize = pageSize, SearchConditionsList = searchConditionsList, SearchConditionsGroupType = searchConditionsGroupType, SearchSortFieldNamesList = searchSortFieldNamesList, SearchSortFieldNamesDescendingList = searchSortFieldNamesDescendingList, entityId = entityID, flatten = flatten)
	}


	listCRDCSchoolRetentionCountV1s <- function(searchConditionsList = NULL, CRDCSchoolRetentionCountV1ID = F, KindergartenMaleHispanicCount = F, KindergartenMaleAlaskanCount = F, KindergartenMaleAsianCount = F, KindergartenMaleHawaiianCount = F, KindergartenMaleBlackCount = F, KindergartenMaleWhiteCount = F, KindergartenMaleMultiRaceCount = F, KindergartenMaleLEPCount = F, KindergartenMaleIDEACount = F, KindergartenMaleSection504Count = F, KindergartenFemaleHispanicCount = F, KindergartenFemaleAlaskanCount = F, KindergartenFemaleAsianCount = F, KindergartenFemaleHawaiianCount = F, KindergartenFemaleBlackCount = F, KindergartenFemaleWhiteCount = F, KindergartenFemaleMultiRaceCount = F, KindergartenFemaleLEPCount = F, KindergartenFemaleIDEACount = F, KindergartenFemaleSection504Count = F, Grade01MaleHispanicCount = F, Grade01MaleAlaskanCount = F, Grade01MaleAsianCount = F, Grade01MaleHawaiianCount = F, Grade01MaleBlackCount = F, Grade01MaleWhiteCount = F, Grade01MaleMultiRaceCount = F, Grade01MaleLEPCount = F, Grade01MaleIDEACount = F, Grade01MaleSection504Count = F, Grade01FemaleHispanicCount = F, Grade01FemaleAlaskanCount = F, Grade01FemaleAsianCount = F, Grade01FemaleHawaiianCount = F, Grade01FemaleBlackCount = F, Grade01FemaleWhiteCount = F, Grade01FemaleMultiRaceCount = F, Grade01FemaleLEPCount = F, Grade01FemaleIDEACount = F, Grade01FemaleSection504Count = F, Grade02MaleHispanicCount = F, Grade02MaleAlaskanCount = F, Grade02MaleAsianCount = F, Grade02MaleHawaiianCount = F, Grade02MaleBlackCount = F, Grade02MaleWhiteCount = F, Grade02MaleMultiRaceCount = F, Grade02MaleLEPCount = F, Grade02MaleIDEACount = F, Grade02MaleSection504Count = F, Grade02FemaleHispanicCount = F, Grade02FemaleAlaskanCount = F, Grade02FemaleAsianCount = F, Grade02FemaleHawaiianCount = F, Grade02FemaleBlackCount = F, Grade02FemaleWhiteCount = F, Grade02FemaleMultiRaceCount = F, Grade02FemaleLEPCount = F, Grade02FemaleIDEACount = F, Grade02FemaleSection504Count = F, Grade03MaleHispanicCount = F, Grade03MaleAlaskanCount = F, Grade03MaleAsianCount = F, Grade03MaleHawaiianCount = F, Grade03MaleBlackCount = F, Grade03MaleWhiteCount = F, Grade03MaleMultiRaceCount = F, Grade03MaleLEPCount = F, Grade03MaleIDEACount = F, Grade03MaleSection504Count = F, Grade03FemaleHispanicCount = F, Grade03FemaleAlaskanCount = F, Grade03FemaleAsianCount = F, Grade03FemaleHawaiianCount = F, Grade03FemaleBlackCount = F, Grade03FemaleWhiteCount = F, Grade03FemaleMultiRaceCount = F, Grade03FemaleLEPCount = F, Grade03FemaleIDEACount = F, Grade03FemaleSection504Count = F, Grade04MaleHispanicCount = F, Grade04MaleAlaskanCount = F, Grade04MaleAsianCount = F, Grade04MaleHawaiianCount = F, Grade04MaleBlackCount = F, Grade04MaleWhiteCount = F, Grade04MaleMultiRaceCount = F, Grade04MaleLEPCount = F, Grade04MaleIDEACount = F, Grade04MaleSection504Count = F, Grade04FemaleHispanicCount = F, Grade04FemaleAlaskanCount = F, Grade04FemaleAsianCount = F, Grade04FemaleHawaiianCount = F, Grade04FemaleBlackCount = F, Grade04FemaleWhiteCount = F, Grade04FemaleMultiRaceCount = F, Grade04FemaleLEPCount = F, Grade04FemaleIDEACount = F, Grade04FemaleSection504Count = F, Grade05MaleHispanicCount = F, Grade05MaleAlaskanCount = F, Grade05MaleAsianCount = F, Grade05MaleHawaiianCount = F, Grade05MaleBlackCount = F, Grade05MaleWhiteCount = F, Grade05MaleMultiRaceCount = F, Grade05MaleLEPCount = F, Grade05MaleIDEACount = F, Grade05MaleSection504Count = F, Grade05FemaleHispanicCount = F, Grade05FemaleAlaskanCount = F, Grade05FemaleAsianCount = F, Grade05FemaleHawaiianCount = F, Grade05FemaleBlackCount = F, Grade05FemaleWhiteCount = F, Grade05FemaleMultiRaceCount = F, Grade05FemaleLEPCount = F, Grade05FemaleIDEACount = F, Grade05FemaleSection504Count = F, Grade06MaleHispanicCount = F, Grade06MaleAlaskanCount = F, Grade06MaleAsianCount = F, Grade06MaleHawaiianCount = F, Grade06MaleBlackCount = F, Grade06MaleWhiteCount = F, Grade06MaleMultiRaceCount = F, Grade06MaleLEPCount = F, Grade06MaleIDEACount = F, Grade06MaleSection504Count = F, Grade06FemaleHispanicCount = F, Grade06FemaleAlaskanCount = F, Grade06FemaleAsianCount = F, Grade06FemaleHawaiianCount = F, Grade06FemaleBlackCount = F, Grade06FemaleWhiteCount = F, Grade06FemaleMultiRaceCount = F, Grade06FemaleLEPCount = F, Grade06FemaleIDEACount = F, Grade06FemaleSection504Count = F, Grade07MaleHispanicCount = F, Grade07MaleAlaskanCount = F, Grade07MaleAsianCount = F, Grade07MaleHawaiianCount = F, Grade07MaleBlackCount = F, Grade07MaleWhiteCount = F, Grade07MaleMultiRaceCount = F, Grade07MaleLEPCount = F, Grade07MaleIDEACount = F, Grade07MaleSection504Count = F, Grade07FemaleHispanicCount = F, Grade07FemaleAlaskanCount = F, Grade07FemaleAsianCount = F, Grade07FemaleHawaiianCount = F, Grade07FemaleBlackCount = F, Grade07FemaleWhiteCount = F, Grade07FemaleMultiRaceCount = F, Grade07FemaleLEPCount = F, Grade07FemaleIDEACount = F, Grade07FemaleSection504Count = F, Grade08MaleHispanicCount = F, Grade08MaleAlaskanCount = F, Grade08MaleAsianCount = F, Grade08MaleHawaiianCount = F, Grade08MaleBlackCount = F, Grade08MaleWhiteCount = F, Grade08MaleMultiRaceCount = F, Grade08MaleLEPCount = F, Grade08MaleIDEACount = F, Grade08MaleSection504Count = F, Grade08FemaleHispanicCount = F, Grade08FemaleAlaskanCount = F, Grade08FemaleAsianCount = F, Grade08FemaleHawaiianCount = F, Grade08FemaleBlackCount = F, Grade08FemaleWhiteCount = F, Grade08FemaleMultiRaceCount = F, Grade08FemaleLEPCount = F, Grade08FemaleIDEACount = F, Grade08FemaleSection504Count = F, Grade09MaleHispanicCount = F, Grade09MaleAlaskanCount = F, Grade09MaleAsianCount = F, Grade09MaleHawaiianCount = F, Grade09MaleBlackCount = F, Grade09MaleWhiteCount = F, Grade09MaleMultiRaceCount = F, Grade09MaleLEPCount = F, Grade09MaleIDEACount = F, Grade09MaleSection504Count = F, Grade09FemaleHispanicCount = F, Grade09FemaleAlaskanCount = F, Grade09FemaleAsianCount = F, Grade09FemaleHawaiianCount = F, Grade09FemaleBlackCount = F, Grade09FemaleWhiteCount = F, Grade09FemaleMultiRaceCount = F, Grade09FemaleLEPCount = F, Grade09FemaleIDEACount = F, Grade09FemaleSection504Count = F, Grade10MaleHispanicCount = F, Grade10MaleAlaskanCount = F, Grade10MaleAsianCount = F, Grade10MaleHawaiianCount = F, Grade10MaleBlackCount = F, Grade10MaleWhiteCount = F, Grade10MaleMultiRaceCount = F, Grade10MaleLEPCount = F, Grade10MaleIDEACount = F, Grade10MaleSection504Count = F, Grade10FemaleHispanicCount = F, Grade10FemaleAlaskanCount = F, Grade10FemaleAsianCount = F, Grade10FemaleHawaiianCount = F, Grade10FemaleBlackCount = F, Grade10FemaleWhiteCount = F, Grade10FemaleMultiRaceCount = F, Grade10FemaleLEPCount = F, Grade10FemaleIDEACount = F, Grade10FemaleSection504Count = F, Grade11MaleHispanicCount = F, Grade11MaleAlaskanCount = F, Grade11MaleAsianCount = F, Grade11MaleHawaiianCount = F, Grade11MaleBlackCount = F, Grade11MaleWhiteCount = F, Grade11MaleMultiRaceCount = F, Grade11MaleLEPCount = F, Grade11MaleIDEACount = F, Grade11MaleSection504Count = F, Grade11FemaleHispanicCount = F, Grade11FemaleAlaskanCount = F, Grade11FemaleAsianCount = F, Grade11FemaleHawaiianCount = F, Grade11FemaleBlackCount = F, Grade11FemaleWhiteCount = F, Grade11FemaleMultiRaceCount = F, Grade11FemaleLEPCount = F, Grade11FemaleIDEACount = F, Grade11FemaleSection504Count = F, Grade12MaleHispanicCount = F, Grade12MaleAlaskanCount = F, Grade12MaleAsianCount = F, Grade12MaleHawaiianCount = F, Grade12MaleBlackCount = F, Grade12MaleWhiteCount = F, Grade12MaleMultiRaceCount = F, Grade12MaleLEPCount = F, Grade12MaleIDEACount = F, Grade12MaleSection504Count = F, Grade12FemaleHispanicCount = F, Grade12FemaleAlaskanCount = F, Grade12FemaleAsianCount = F, Grade12FemaleHawaiianCount = F, Grade12FemaleBlackCount = F, Grade12FemaleWhiteCount = F, Grade12FemaleMultiRaceCount = F, Grade12FemaleLEPCount = F, Grade12FemaleIDEACount = F, Grade12FemaleSection504Count = F, CivilRightsDataCollectionID = F, CivilRightsDataCollectionRunHistoryID = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, FederalNCESSchoolID = F, searchConditionsGroupType = "And", searchSortFieldNamesList = NULL, searchSortFieldNamesDescendingList = NULL, entityID = 1, schoolYearID = NULL, page = 1, pageSize = 100000, flatten = T, ...){

		params <- as.list(environment()) %>% append(list(...))

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- "all", searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		listSkyObjects(module = "StateReportingFederal", objectName = "CRDCSchoolRetentionCountV1", schoolYearID = schoolYearID, searchFields = searchFields, page = page, pageSize = pageSize, SearchConditionsList = searchConditionsList, SearchConditionsGroupType = searchConditionsGroupType, SearchSortFieldNamesList = searchSortFieldNamesList, SearchSortFieldNamesDescendingList = searchSortFieldNamesDescendingList, entityId = entityID, flatten = flatten)
	}


	listCRDCSchoolRetentionIndicatorV1s <- function(searchConditionsList = NULL, CRDCSchoolRetentionIndicatorV1ID = F, HasKindergarten = F, HasGrade01 = F, HasGrade02 = F, HasGrade03 = F, HasGrade04 = F, HasGrade05 = F, HasGrade06 = F, HasGrade07 = F, HasGrade08 = F, HasGrade09 = F, HasGrade10 = F, HasGrade11 = F, HasGrade12 = F, CivilRightsDataCollectionID = F, CivilRightsDataCollectionRunHistoryID = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, FederalNCESSchoolID = F, searchConditionsGroupType = "And", searchSortFieldNamesList = NULL, searchSortFieldNamesDescendingList = NULL, entityID = 1, schoolYearID = NULL, page = 1, pageSize = 100000, flatten = T, ...){

		params <- as.list(environment()) %>% append(list(...))

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- "all", searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		listSkyObjects(module = "StateReportingFederal", objectName = "CRDCSchoolRetentionIndicatorV1", schoolYearID = schoolYearID, searchFields = searchFields, page = page, pageSize = pageSize, SearchConditionsList = searchConditionsList, SearchConditionsGroupType = searchConditionsGroupType, SearchSortFieldNamesList = searchSortFieldNamesList, SearchSortFieldNamesDescendingList = searchSortFieldNamesDescendingList, entityId = entityID, flatten = flatten)
	}


	listCRDCSchoolHSScienceV1s <- function(searchConditionsList = NULL, CRDCSchoolHSScienceV1ID = F, BiologyClassCount = F, BiologyCertifiedClassCount = F, BiologyMaleHispanicCount = F, BiologyMaleAlaskanCount = F, BiologyMaleAsianCount = F, BiologyMaleHawaiianCount = F, BiologyMaleBlackCount = F, BiologyMaleWhiteCount = F, BiologyMaleMultiRaceCount = F, BiologyMaleLEPCount = F, BiologyMaleIDEACount = F, BiologyFemaleHispanicCount = F, BiologyFemaleAlaskanCount = F, BiologyFemaleAsianCount = F, BiologyFemaleHawaiianCount = F, BiologyFemaleBlackCount = F, BiologyFemaleWhiteCount = F, BiologyFemaleMultiRaceCount = F, BiologyFemaleLEPCount = F, BiologyFemaleIDEACount = F, ChemistryClassCount = F, ChemistryCertifiedClassCount = F, ChemistryMaleHispanicCount = F, ChemistryMaleAlaskanCount = F, ChemistryMaleAsianCount = F, ChemistryMaleHawaiianCount = F, ChemistryMaleBlackCount = F, ChemistryMaleWhiteCount = F, ChemistryMaleMultiRaceCount = F, ChemistryMaleLEPCount = F, ChemistryMaleIDEACount = F, ChemistryFemaleHispanicCount = F, ChemistryFemaleAlaskanCount = F, ChemistryFemaleAsianCount = F, ChemistryFemaleHawaiianCount = F, ChemistryFemaleBlackCount = F, ChemistryFemaleWhiteCount = F, ChemistryFemaleMultiRaceCount = F, ChemistryFemaleLEPCount = F, ChemistryFemaleIDEACount = F, PhysicsClassCount = F, PhysicsCertifiedClassCount = F, PhysicsMaleHispanicCount = F, PhysicsMaleAlaskanCount = F, PhysicsMaleAsianCount = F, PhysicsMaleHawaiianCount = F, PhysicsMaleBlackCount = F, PhysicsMaleWhiteCount = F, PhysicsMaleMultiRaceCount = F, PhysicsMaleLEPCount = F, PhysicsMaleIDEACount = F, PhysicsFemaleHispanicCount = F, PhysicsFemaleAlaskanCount = F, PhysicsFemaleAsianCount = F, PhysicsFemaleHawaiianCount = F, PhysicsFemaleBlackCount = F, PhysicsFemaleWhiteCount = F, PhysicsFemaleMultiRaceCount = F, PhysicsFemaleLEPCount = F, PhysicsFemaleIDEACount = F, CivilRightsDataCollectionID = F, CivilRightsDataCollectionRunHistoryID = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, FederalNCESSchoolID = F, searchConditionsGroupType = "And", searchSortFieldNamesList = NULL, searchSortFieldNamesDescendingList = NULL, entityID = 1, schoolYearID = NULL, page = 1, pageSize = 100000, flatten = T, ...){

		params <- as.list(environment()) %>% append(list(...))

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- "all", searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		listSkyObjects(module = "StateReportingFederal", objectName = "CRDCSchoolHSScienceV1", schoolYearID = schoolYearID, searchFields = searchFields, page = page, pageSize = pageSize, SearchConditionsList = searchConditionsList, SearchConditionsGroupType = searchConditionsGroupType, SearchSortFieldNamesList = searchSortFieldNamesList, SearchSortFieldNamesDescendingList = searchSortFieldNamesDescendingList, entityId = entityID, flatten = flatten)
	}


	listCRDCSchoolDualEnrollmentV1s <- function(searchConditionsList = NULL, CRDCSchoolDualEnrollmentV1ID = F, HasStudents = F, MaleHispanicCount = F, MaleAlaskanCount = F, MaleAsianCount = F, MaleHawaiianCount = F, MaleBlackCount = F, MaleWhiteCount = F, MaleMultiRaceCount = F, MaleLEPCount = F, MaleIDEACount = F, FemaleHispanicCount = F, FemaleAlaskanCount = F, FemaleAsianCount = F, FemaleHawaiianCount = F, FemaleBlackCount = F, FemaleWhiteCount = F, FemaleMultiRaceCount = F, FemaleLEPCount = F, FemaleIDEACount = F, CivilRightsDataCollectionID = F, CivilRightsDataCollectionRunHistoryID = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, FederalNCESSchoolID = F, searchConditionsGroupType = "And", searchSortFieldNamesList = NULL, searchSortFieldNamesDescendingList = NULL, entityID = 1, schoolYearID = NULL, page = 1, pageSize = 100000, flatten = T, ...){

		params <- as.list(environment()) %>% append(list(...))

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- "all", searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		listSkyObjects(module = "StateReportingFederal", objectName = "CRDCSchoolDualEnrollmentV1", schoolYearID = schoolYearID, searchFields = searchFields, page = page, pageSize = pageSize, SearchConditionsList = searchConditionsList, SearchConditionsGroupType = searchConditionsGroupType, SearchSortFieldNamesList = searchSortFieldNamesList, SearchSortFieldNamesDescendingList = searchSortFieldNamesDescendingList, entityId = entityID, flatten = flatten)
	}


	listCRDCSchoolIBDiplomaV1s <- function(searchConditionsList = NULL, CRDCSchoolIBDiplomaV1ID = F, HasStudents = F, MaleHispanicCount = F, MaleAlaskanCount = F, MaleAsianCount = F, MaleHawaiianCount = F, MaleBlackCount = F, MaleWhiteCount = F, MaleMultiRaceCount = F, MaleLEPCount = F, MaleIDEACount = F, FemaleHispanicCount = F, FemaleAlaskanCount = F, FemaleAsianCount = F, FemaleHawaiianCount = F, FemaleBlackCount = F, FemaleWhiteCount = F, FemaleMultiRaceCount = F, FemaleLEPCount = F, FemaleIDEACount = F, CivilRightsDataCollectionID = F, CivilRightsDataCollectionRunHistoryID = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, FederalNCESSchoolID = F, searchConditionsGroupType = "And", searchSortFieldNamesList = NULL, searchSortFieldNamesDescendingList = NULL, entityID = 1, schoolYearID = NULL, page = 1, pageSize = 100000, flatten = T, ...){

		params <- as.list(environment()) %>% append(list(...))

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- "all", searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		listSkyObjects(module = "StateReportingFederal", objectName = "CRDCSchoolIBDiplomaV1", schoolYearID = schoolYearID, searchFields = searchFields, page = page, pageSize = pageSize, SearchConditionsList = searchConditionsList, SearchConditionsGroupType = searchConditionsGroupType, SearchSortFieldNamesList = searchSortFieldNamesList, SearchSortFieldNamesDescendingList = searchSortFieldNamesDescendingList, entityId = entityID, flatten = flatten)
	}


	listCRDCSchoolSingleSexClassV1s <- function(searchConditionsList = NULL, CRDCSchoolSingleSexClassV1ID = F, HasStudents = F, AlgebraGeometryMaleCount = F, AlgebraGeometryFemaleCount = F, OtherMathMaleCount = F, OtherMathFemaleCount = F, ScienceMaleCount = F, ScienceFemaleCount = F, EnglishMaleCount = F, EnglishFemaleCount = F, OtherSubjectMaleCount = F, OtherSubjectFemaleCount = F, CivilRightsDataCollectionID = F, CivilRightsDataCollectionRunHistoryID = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, FederalNCESSchoolID = F, searchConditionsGroupType = "And", searchSortFieldNamesList = NULL, searchSortFieldNamesDescendingList = NULL, entityID = 1, schoolYearID = NULL, page = 1, pageSize = 100000, flatten = T, ...){

		params <- as.list(environment()) %>% append(list(...))

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- "all", searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		listSkyObjects(module = "StateReportingFederal", objectName = "CRDCSchoolSingleSexClassV1", schoolYearID = schoolYearID, searchFields = searchFields, page = page, pageSize = pageSize, SearchConditionsList = searchConditionsList, SearchConditionsGroupType = searchConditionsGroupType, SearchSortFieldNamesList = searchSortFieldNamesList, SearchSortFieldNamesDescendingList = searchSortFieldNamesDescendingList, entityId = entityID, flatten = flatten)
	}


	listCRDCSchoolSupportStaffV1s <- function(searchConditionsList = NULL, CRDCSchoolSupportStaffV1ID = F, CounselorFTETotal = F, OfficerFTETotal = F, SecurityGuardFTETotal = F, NurseFTETotal = F, PsychologistFTETotal = F, SocialWorkerFTETotal = F, CivilRightsDataCollectionID = F, CivilRightsDataCollectionRunHistoryID = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, FederalNCESSchoolID = F, searchConditionsGroupType = "And", searchSortFieldNamesList = NULL, searchSortFieldNamesDescendingList = NULL, entityID = 1, schoolYearID = NULL, page = 1, pageSize = 100000, flatten = T, ...){

		params <- as.list(environment()) %>% append(list(...))

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- "all", searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		listSkyObjects(module = "StateReportingFederal", objectName = "CRDCSchoolSupportStaffV1", schoolYearID = schoolYearID, searchFields = searchFields, page = page, pageSize = pageSize, SearchConditionsList = searchConditionsList, SearchConditionsGroupType = searchConditionsGroupType, SearchSortFieldNamesList = searchSortFieldNamesList, SearchSortFieldNamesDescendingList = searchSortFieldNamesDescendingList, entityId = entityID, flatten = flatten)
	}


	listCRDCSchoolTeacherV1s <- function(searchConditionsList = NULL, CRDCSchoolTeacherV1ID = F, FTETotal = F, CertifiedFTETotal = F, NotCertifiedFTETotal = F, FirstYearFTETotal = F, SecondYearFTETotal = F, CurrentYearCount = F, PreviousYearCount = F, CivilRightsDataCollectionID = F, CivilRightsDataCollectionRunHistoryID = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, FederalNCESSchoolID = F, searchConditionsGroupType = "And", searchSortFieldNamesList = NULL, searchSortFieldNamesDescendingList = NULL, entityID = 1, schoolYearID = NULL, page = 1, pageSize = 100000, flatten = T, ...){

		params <- as.list(environment()) %>% append(list(...))

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- "all", searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		listSkyObjects(module = "StateReportingFederal", objectName = "CRDCSchoolTeacherV1", schoolYearID = schoolYearID, searchFields = searchFields, page = page, pageSize = pageSize, SearchConditionsList = searchConditionsList, SearchConditionsGroupType = searchConditionsGroupType, SearchSortFieldNamesList = searchSortFieldNamesList, SearchSortFieldNamesDescendingList = searchSortFieldNamesDescendingList, entityId = entityID, flatten = flatten)
	}


	listCRDCSchoolHSPassedAlgebraIV1s <- function(searchConditionsList = NULL, CRDCSchoolHSPassedAlgebraIV1ID = F, Grade0910MaleHispanicCount = F, Grade0910MaleAlaskanCount = F, Grade0910MaleAsianCount = F, Grade0910MaleHawaiianCount = F, Grade0910MaleBlackCount = F, Grade0910MaleWhiteCount = F, Grade0910MaleMultiRaceCount = F, Grade0910MaleLEPCount = F, Grade0910MaleIDEACount = F, Grade0910FemaleHispanicCount = F, Grade0910FemaleAlaskanCount = F, Grade0910FemaleAsianCount = F, Grade0910FemaleHawaiianCount = F, Grade0910FemaleBlackCount = F, Grade0910FemaleWhiteCount = F, Grade0910FemaleMultiRaceCount = F, Grade0910FemaleLEPCount = F, Grade0910FemaleIDEACount = F, Grade1112MaleHispanicCount = F, Grade1112MaleAlaskanCount = F, Grade1112MaleAsianCount = F, Grade1112MaleHawaiianCount = F, Grade1112MaleBlackCount = F, Grade1112MaleWhiteCount = F, Grade1112MaleMultiRaceCount = F, Grade1112MaleLEPCount = F, Grade1112MaleIDEACount = F, Grade1112FemaleHispanicCount = F, Grade1112FemaleAlaskanCount = F, Grade1112FemaleAsianCount = F, Grade1112FemaleHawaiianCount = F, Grade1112FemaleBlackCount = F, Grade1112FemaleWhiteCount = F, Grade1112FemaleMultiRaceCount = F, Grade1112FemaleLEPCount = F, Grade1112FemaleIDEACount = F, CivilRightsDataCollectionID = F, CivilRightsDataCollectionRunHistoryID = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, FederalNCESSchoolID = F, searchConditionsGroupType = "And", searchSortFieldNamesList = NULL, searchSortFieldNamesDescendingList = NULL, entityID = 1, schoolYearID = NULL, page = 1, pageSize = 100000, flatten = T, ...){

		params <- as.list(environment()) %>% append(list(...))

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- "all", searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		listSkyObjects(module = "StateReportingFederal", objectName = "CRDCSchoolHSPassedAlgebraIV1", schoolYearID = schoolYearID, searchFields = searchFields, page = page, pageSize = pageSize, SearchConditionsList = searchConditionsList, SearchConditionsGroupType = searchConditionsGroupType, SearchSortFieldNamesList = searchSortFieldNamesList, SearchSortFieldNamesDescendingList = searchSortFieldNamesDescendingList, entityId = entityID, flatten = flatten)
	}


	listCRDCSchoolMSPassedAlgebraIV1s <- function(searchConditionsList = NULL, CRDCSchoolMSPassedAlgebraIV1ID = F, Grade07StudentCount = F, MaleHispanicCount = F, MaleAlaskanCount = F, MaleAsianCount = F, MaleHawaiianCount = F, MaleBlackCount = F, MaleWhiteCount = F, MaleMultiRaceCount = F, MaleLEPCount = F, MaleIDEACount = F, FemaleHispanicCount = F, FemaleAlaskanCount = F, FemaleAsianCount = F, FemaleHawaiianCount = F, FemaleBlackCount = F, FemaleWhiteCount = F, FemaleMultiRaceCount = F, FemaleLEPCount = F, FemaleIDEACount = F, CivilRightsDataCollectionID = F, CivilRightsDataCollectionRunHistoryID = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, FederalNCESSchoolID = F, searchConditionsGroupType = "And", searchSortFieldNamesList = NULL, searchSortFieldNamesDescendingList = NULL, entityID = 1, schoolYearID = NULL, page = 1, pageSize = 100000, flatten = T, ...){

		params <- as.list(environment()) %>% append(list(...))

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- "all", searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		listSkyObjects(module = "StateReportingFederal", objectName = "CRDCSchoolMSPassedAlgebraIV1", schoolYearID = schoolYearID, searchFields = searchFields, page = page, pageSize = pageSize, SearchConditionsList = searchConditionsList, SearchConditionsGroupType = searchConditionsGroupType, SearchSortFieldNamesList = searchSortFieldNamesList, SearchSortFieldNamesDescendingList = searchSortFieldNamesDescendingList, entityId = entityID, flatten = flatten)
	}


	listCRDCSchoolAPExamScoreV1s <- function(searchConditionsList = NULL, CRDCSchoolAPExamScoreV1ID = F, PassedMaleHispanicCount = F, PassedMaleAlaskanCount = F, PassedMaleAsianCount = F, PassedMaleHawaiianCount = F, PassedMaleBlackCount = F, PassedMaleWhiteCount = F, PassedMaleMultiRaceCount = F, PassedMaleLEPCount = F, PassedMaleIDEACount = F, PassedFemaleHispanicCount = F, PassedFemaleAlaskanCount = F, PassedFemaleAsianCount = F, PassedFemaleHawaiianCount = F, PassedFemaleBlackCount = F, PassedFemaleWhiteCount = F, PassedFemaleMultiRaceCount = F, PassedFemaleLEPCount = F, PassedFemaleIDEACount = F, FailedMaleHispanicCount = F, FailedMaleAlaskanCount = F, FailedMaleAsianCount = F, FailedMaleHawaiianCount = F, FailedMaleBlackCount = F, FailedMaleWhiteCount = F, FailedMaleMultiRaceCount = F, FailedMaleLEPCount = F, FailedMaleIDEACount = F, FailedFemaleHispanicCount = F, FailedFemaleAlaskanCount = F, FailedFemaleAsianCount = F, FailedFemaleHawaiianCount = F, FailedFemaleBlackCount = F, FailedFemaleWhiteCount = F, FailedFemaleMultiRaceCount = F, FailedFemaleLEPCount = F, FailedFemaleIDEACount = F, CivilRightsDataCollectionID = F, CivilRightsDataCollectionRunHistoryID = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, FederalNCESSchoolID = F, searchConditionsGroupType = "And", searchSortFieldNamesList = NULL, searchSortFieldNamesDescendingList = NULL, entityID = 1, schoolYearID = NULL, page = 1, pageSize = 100000, flatten = T, ...){

		params <- as.list(environment()) %>% append(list(...))

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- "all", searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		listSkyObjects(module = "StateReportingFederal", objectName = "CRDCSchoolAPExamScoreV1", schoolYearID = schoolYearID, searchFields = searchFields, page = page, pageSize = pageSize, SearchConditionsList = searchConditionsList, SearchConditionsGroupType = searchConditionsGroupType, SearchSortFieldNamesList = searchSortFieldNamesList, SearchSortFieldNamesDescendingList = searchSortFieldNamesDescendingList, entityId = entityID, flatten = flatten)
	}


	listCRDCSchoolAPExamV1s <- function(searchConditionsList = NULL, CRDCSchoolAPExamV1ID = F, ExamTakenMaleHispanicCount = F, ExamTakenMaleAlaskanCount = F, ExamTakenMaleAsianCount = F, ExamTakenMaleHawaiianCount = F, ExamTakenMaleBlackCount = F, ExamTakenMaleWhiteCount = F, ExamTakenMaleMultiRaceCount = F, ExamTakenMaleLEPCount = F, ExamTakenMaleIDEACount = F, ExamTakenFemaleHispanicCount = F, ExamTakenFemaleAlaskanCount = F, ExamTakenFemaleAsianCount = F, ExamTakenFemaleHawaiianCount = F, ExamTakenFemaleBlackCount = F, ExamTakenFemaleWhiteCount = F, ExamTakenFemaleMultiRaceCount = F, ExamTakenFemaleLEPCount = F, ExamTakenFemaleIDEACount = F, EnrolledMaleHispanicCount = F, EnrolledMaleAlaskanCount = F, EnrolledMaleAsianCount = F, EnrolledMaleHawaiianCount = F, EnrolledMaleBlackCount = F, EnrolledMaleWhiteCount = F, EnrolledMaleMultiRaceCount = F, EnrolledMaleLEPCount = F, EnrolledMaleIDEACount = F, EnrolledFemaleHispanicCount = F, EnrolledFemaleAlaskanCount = F, EnrolledFemaleAsianCount = F, EnrolledFemaleHawaiianCount = F, EnrolledFemaleBlackCount = F, EnrolledFemaleWhiteCount = F, EnrolledFemaleMultiRaceCount = F, EnrolledFemaleLEPCount = F, EnrolledFemaleIDEACount = F, CivilRightsDataCollectionID = F, CivilRightsDataCollectionRunHistoryID = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, FederalNCESSchoolID = F, searchConditionsGroupType = "And", searchSortFieldNamesList = NULL, searchSortFieldNamesDescendingList = NULL, entityID = 1, schoolYearID = NULL, page = 1, pageSize = 100000, flatten = T, ...){

		params <- as.list(environment()) %>% append(list(...))

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- "all", searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		listSkyObjects(module = "StateReportingFederal", objectName = "CRDCSchoolAPExamV1", schoolYearID = schoolYearID, searchFields = searchFields, page = page, pageSize = pageSize, SearchConditionsList = searchConditionsList, SearchConditionsGroupType = searchConditionsGroupType, SearchSortFieldNamesList = searchSortFieldNamesList, SearchSortFieldNamesDescendingList = searchSortFieldNamesDescendingList, entityId = entityID, flatten = flatten)
	}


	listCRDCSchoolCreditRecoveryV1s <- function(searchConditionsList = NULL, CRDCSchoolCreditRecoveryV1ID = F, HasProgram = F, StudentCount = F, CivilRightsDataCollectionID = F, CivilRightsDataCollectionRunHistoryID = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, FederalNCESSchoolID = F, searchConditionsGroupType = "And", searchSortFieldNamesList = NULL, searchSortFieldNamesDescendingList = NULL, entityID = 1, schoolYearID = NULL, page = 1, pageSize = 100000, flatten = T, ...){

		params <- as.list(environment()) %>% append(list(...))

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- "all", searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		listSkyObjects(module = "StateReportingFederal", objectName = "CRDCSchoolCreditRecoveryV1", schoolYearID = schoolYearID, searchFields = searchFields, page = page, pageSize = pageSize, SearchConditionsList = searchConditionsList, SearchConditionsGroupType = searchConditionsGroupType, SearchSortFieldNamesList = searchSortFieldNamesList, SearchSortFieldNamesDescendingList = searchSortFieldNamesDescendingList, entityId = entityID, flatten = flatten)
	}


	listCRDCSchoolSATACTV1s <- function(searchConditionsList = NULL, CRDCSchoolSATACTV1ID = F, MaleHispanicCount = F, MaleAlaskanCount = F, MaleAsianCount = F, MaleHawaiianCount = F, MaleBlackCount = F, MaleWhiteCount = F, MaleMultiRaceCount = F, MaleLEPCount = F, MaleIDEACount = F, FemaleHispanicCount = F, FemaleAlaskanCount = F, FemaleAsianCount = F, FemaleHawaiianCount = F, FemaleBlackCount = F, FemaleWhiteCount = F, FemaleMultiRaceCount = F, FemaleLEPCount = F, FemaleIDEACount = F, CivilRightsDataCollectionID = F, CivilRightsDataCollectionRunHistoryID = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, FederalNCESSchoolID = F, searchConditionsGroupType = "And", searchSortFieldNamesList = NULL, searchSortFieldNamesDescendingList = NULL, entityID = 1, schoolYearID = NULL, page = 1, pageSize = 100000, flatten = T, ...){

		params <- as.list(environment()) %>% append(list(...))

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- "all", searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		listSkyObjects(module = "StateReportingFederal", objectName = "CRDCSchoolSATACTV1", schoolYearID = schoolYearID, searchFields = searchFields, page = page, pageSize = pageSize, SearchConditionsList = searchConditionsList, SearchConditionsGroupType = searchConditionsGroupType, SearchSortFieldNamesList = searchSortFieldNamesList, SearchSortFieldNamesDescendingList = searchSortFieldNamesDescendingList, entityId = entityID, flatten = flatten)
	}


	listCRDCSchoolAbsenteeismV1s <- function(searchConditionsList = NULL, CRDCSchoolAbsenteeismV1ID = F, MaleHispanicCount = F, MaleAlaskanCount = F, MaleAsianCount = F, MaleHawaiianCount = F, MaleBlackCount = F, MaleWhiteCount = F, MaleMultiRaceCount = F, MaleLEPCount = F, MaleIDEACount = F, MaleSection504Count = F, FemaleHispanicCount = F, FemaleAlaskanCount = F, FemaleAsianCount = F, FemaleHawaiianCount = F, FemaleBlackCount = F, FemaleWhiteCount = F, FemaleMultiRaceCount = F, FemaleLEPCount = F, FemaleIDEACount = F, FemaleSection504Count = F, CivilRightsDataCollectionID = F, CivilRightsDataCollectionRunHistoryID = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, FederalNCESSchoolID = F, searchConditionsGroupType = "And", searchSortFieldNamesList = NULL, searchSortFieldNamesDescendingList = NULL, entityID = 1, schoolYearID = NULL, page = 1, pageSize = 100000, flatten = T, ...){

		params <- as.list(environment()) %>% append(list(...))

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- "all", searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		listSkyObjects(module = "StateReportingFederal", objectName = "CRDCSchoolAbsenteeismV1", schoolYearID = schoolYearID, searchFields = searchFields, page = page, pageSize = pageSize, SearchConditionsList = searchConditionsList, SearchConditionsGroupType = searchConditionsGroupType, SearchSortFieldNamesList = searchSortFieldNamesList, SearchSortFieldNamesDescendingList = searchSortFieldNamesDescendingList, entityId = entityID, flatten = flatten)
	}


	listCRDCSchoolSingleSexAthleticV1s <- function(searchConditionsList = NULL, CRDCSchoolSingleSexAthleticV1ID = F, HasStudents = F, MaleOnlySportsCount = F, FemaleOnlySportsCount = F, MaleOnlyTeamsCount = F, FemaleOnlyTeamsCount = F, MaleOnlyParticipantCount = F, FemaleOnlyParticipantCount = F, CivilRightsDataCollectionID = F, CivilRightsDataCollectionRunHistoryID = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, FederalNCESSchoolID = F, searchConditionsGroupType = "And", searchSortFieldNamesList = NULL, searchSortFieldNamesDescendingList = NULL, entityID = 1, schoolYearID = NULL, page = 1, pageSize = 100000, flatten = T, ...){

		params <- as.list(environment()) %>% append(list(...))

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- "all", searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		listSkyObjects(module = "StateReportingFederal", objectName = "CRDCSchoolSingleSexAthleticV1", schoolYearID = schoolYearID, searchFields = searchFields, page = page, pageSize = pageSize, SearchConditionsList = searchConditionsList, SearchConditionsGroupType = searchConditionsGroupType, SearchSortFieldNamesList = searchSortFieldNamesList, SearchSortFieldNamesDescendingList = searchSortFieldNamesDescendingList, entityId = entityID, flatten = flatten)
	}


	listFederalAlternativeSchoolDetails <- function(searchConditionsList = NULL, FederalAlternativeSchoolDetailID = F, Code = F, Description = F, CodeDescription = F, ValidYearLow = F, ValidYearHigh = F, IsInvalid = F, SkywardID = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, SkywardHash = F, searchConditionsGroupType = "And", searchSortFieldNamesList = NULL, searchSortFieldNamesDescendingList = NULL, entityID = 1, schoolYearID = NULL, page = 1, pageSize = 100000, flatten = T, ...){

		params <- as.list(environment()) %>% append(list(...))

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- "all", searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		listSkyObjects(module = "StateReportingFederal", objectName = "FederalAlternativeSchoolDetail", schoolYearID = schoolYearID, searchFields = searchFields, page = page, pageSize = pageSize, SearchConditionsList = searchConditionsList, SearchConditionsGroupType = searchConditionsGroupType, SearchSortFieldNamesList = searchSortFieldNamesList, SearchSortFieldNamesDescendingList = searchSortFieldNamesDescendingList, entityId = entityID, flatten = flatten)
	}


	listFederalJusticeFacilityTypes <- function(searchConditionsList = NULL, FederalJusticeFacilityTypeID = F, Code = F, Description = F, CodeDescription = F, ValidYearLow = F, ValidYearHigh = F, IsInvalid = F, SkywardID = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, SkywardHash = F, searchConditionsGroupType = "And", searchSortFieldNamesList = NULL, searchSortFieldNamesDescendingList = NULL, entityID = 1, schoolYearID = NULL, page = 1, pageSize = 100000, flatten = T, ...){

		params <- as.list(environment()) %>% append(list(...))

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- "all", searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		listSkyObjects(module = "StateReportingFederal", objectName = "FederalJusticeFacilityType", schoolYearID = schoolYearID, searchFields = searchFields, page = page, pageSize = pageSize, SearchConditionsList = searchConditionsList, SearchConditionsGroupType = searchConditionsGroupType, SearchSortFieldNamesList = searchSortFieldNamesList, SearchSortFieldNamesDescendingList = searchSortFieldNamesDescendingList, entityId = entityID, flatten = flatten)
	}


	listCRDCLEAEarlyLearningV1s <- function(searchConditionsList = NULL, CRDCLEAEarlyLearningV1ID = F, DistrictID = F, HasEarlyChildhood = F, HasEarlyChildhoodNonIDEA = F, HasPreschool = F, HasPreschoolFullDayFree = F, HasPreschoolFullDayCost = F, HasPreschoolPartDayFree = F, HasPreschoolPartDayCost = F, HasPreschoolNonIDEAAge3 = F, HasPreschoolNonIDEAAge4 = F, HasPreschoolNonIDEAAge5 = F, HasPreschoolAllChildren = F, HasPreschoolIDEA = F, HasPreschoolTitleI = F, HasPreschoolLowIncome = F, HasKindergarten = F, HasKindergartenFullDayFree = F, HasKindergartenFullDayCost = F, HasKindergartenPartDayFree = F, HasKindergartenPartDayCost = F, PreschoolChildrenServedAge2 = F, PreschoolChildrenServedAge3 = F, PreschoolChildrenServedAge4 = F, PreschoolChildrenServedAge5 = F, CivilRightsDataCollectionID = F, CivilRightsDataCollectionRunHistoryID = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, searchConditionsGroupType = "And", searchSortFieldNamesList = NULL, searchSortFieldNamesDescendingList = NULL, entityID = 1, schoolYearID = NULL, page = 1, pageSize = 100000, flatten = T, ...){

		params <- as.list(environment()) %>% append(list(...))

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- "all", searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		listSkyObjects(module = "StateReportingFederal", objectName = "CRDCLEAEarlyLearningV1", schoolYearID = schoolYearID, searchFields = searchFields, page = page, pageSize = pageSize, SearchConditionsList = searchConditionsList, SearchConditionsGroupType = searchConditionsGroupType, SearchSortFieldNamesList = searchSortFieldNamesList, SearchSortFieldNamesDescendingList = searchSortFieldNamesDescendingList, entityId = entityID, flatten = flatten)
	}


	listCRDCLEADistanceEducationV1s <- function(searchConditionsList = NULL, CRDCLEADistanceEducationV1ID = F, DistrictID = F, HasDistanceEducation = F, MaleHispanicCount = F, MaleAlaskanCount = F, MaleAsianCount = F, MaleHawaiianCount = F, MaleBlackCount = F, MaleWhiteCount = F, MaleMultiRaceCount = F, MaleLEPCount = F, MaleIDEACount = F, FemaleHispanicCount = F, FemaleAlaskanCount = F, FemaleAsianCount = F, FemaleHawaiianCount = F, FemaleBlackCount = F, FemaleWhiteCount = F, FemaleMultiRaceCount = F, FemaleLEPCount = F, FemaleIDEACount = F, CivilRightsDataCollectionID = F, CivilRightsDataCollectionRunHistoryID = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, searchConditionsGroupType = "And", searchSortFieldNamesList = NULL, searchSortFieldNamesDescendingList = NULL, entityID = 1, schoolYearID = NULL, page = 1, pageSize = 100000, flatten = T, ...){

		params <- as.list(environment()) %>% append(list(...))

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- "all", searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		listSkyObjects(module = "StateReportingFederal", objectName = "CRDCLEADistanceEducationV1", schoolYearID = schoolYearID, searchFields = searchFields, page = page, pageSize = pageSize, SearchConditionsList = searchConditionsList, SearchConditionsGroupType = searchConditionsGroupType, SearchSortFieldNamesList = searchSortFieldNamesList, SearchSortFieldNamesDescendingList = searchSortFieldNamesDescendingList, entityId = entityID, flatten = flatten)
	}


	listACAYearExportPrintFiles <- function(searchConditionsList = NULL, ACAYearExportPrintFileID = F, ACAYearID = F, MediaID = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, searchConditionsGroupType = "And", searchSortFieldNamesList = NULL, searchSortFieldNamesDescendingList = NULL, entityID = 1, schoolYearID = NULL, page = 1, pageSize = 100000, flatten = T, ...){

		params <- as.list(environment()) %>% append(list(...))

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- "all", searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		listSkyObjects(module = "StateReportingFederal", objectName = "ACAYearExportPrintFile", schoolYearID = schoolYearID, searchFields = searchFields, page = page, pageSize = pageSize, SearchConditionsList = searchConditionsList, SearchConditionsGroupType = searchConditionsGroupType, SearchSortFieldNamesList = searchSortFieldNamesList, SearchSortFieldNamesDescendingList = searchSortFieldNamesDescendingList, entityId = entityID, flatten = flatten)
	}


	listForm1099MYearExceptions <- function(searchConditionsList = NULL, Form1099MYearExceptionID = F, Form1099MYearID = F, VendorID = F, IsFatalException = F, ExceptionType = F, Description = F, SourceType = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, PreserveException = F, searchConditionsGroupType = "And", searchSortFieldNamesList = NULL, searchSortFieldNamesDescendingList = NULL, entityID = 1, schoolYearID = NULL, page = 1, pageSize = 100000, flatten = T, ...){

		params <- as.list(environment()) %>% append(list(...))

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- "all", searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		listSkyObjects(module = "StateReportingFederal", objectName = "Form1099MYearException", schoolYearID = schoolYearID, searchFields = searchFields, page = page, pageSize = pageSize, SearchConditionsList = searchConditionsList, SearchConditionsGroupType = searchConditionsGroupType, SearchSortFieldNamesList = searchSortFieldNamesList, SearchSortFieldNamesDescendingList = searchSortFieldNamesDescendingList, entityId = entityID, flatten = flatten)
	}


	listForm1099MExportPrintFiles <- function(searchConditionsList = NULL, Form1099MExportPrintFileID = F, Form1099MYearID = F, MediaID = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, searchConditionsGroupType = "And", searchSortFieldNamesList = NULL, searchSortFieldNamesDescendingList = NULL, entityID = 1, schoolYearID = NULL, page = 1, pageSize = 100000, flatten = T, ...){

		params <- as.list(environment()) %>% append(list(...))

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- "all", searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		listSkyObjects(module = "StateReportingFederal", objectName = "Form1099MExportPrintFile", schoolYearID = schoolYearID, searchFields = searchFields, page = page, pageSize = pageSize, SearchConditionsList = searchConditionsList, SearchConditionsGroupType = searchConditionsGroupType, SearchSortFieldNamesList = searchSortFieldNamesList, SearchSortFieldNamesDescendingList = searchSortFieldNamesDescendingList, entityId = entityID, flatten = flatten)
	}


	listACAYearALEMemberMonthV1s <- function(searchConditionsList = NULL, ACAYearALEMemberMonthV1ID = F, ACAYearID = F, OffersMinimumEssentialCoverage = F, FullTimeEmployeeCount = F, TotalEmployeeCount = F, IsAggregatedGroup = F, Section4980HTransitionRelief = F, Month = F, SkywardID = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, searchConditionsGroupType = "And", searchSortFieldNamesList = NULL, searchSortFieldNamesDescendingList = NULL, entityID = 1, schoolYearID = NULL, page = 1, pageSize = 100000, flatten = T, ...){

		params <- as.list(environment()) %>% append(list(...))

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- "all", searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		listSkyObjects(module = "StateReportingFederal", objectName = "ACAYearALEMemberMonthV1", schoolYearID = schoolYearID, searchFields = searchFields, page = page, pageSize = pageSize, SearchConditionsList = searchConditionsList, SearchConditionsGroupType = searchConditionsGroupType, SearchSortFieldNamesList = searchSortFieldNamesList, SearchSortFieldNamesDescendingList = searchSortFieldNamesDescendingList, entityId = entityID, flatten = flatten)
	}


	listACAYearOtherALEMemberOfAggregatedGroupV1s <- function(searchConditionsList = NULL, ACAYearOtherALEMemberOfAggregatedGroupV1ID = F, ACAYearID = F, EmployerID = F, EmployerBusinessNamePrinted = F, EmployerBusinessNameLine1ElectronicFile = F, EmployerBusinessNameLine2ElectronicFile = F, EmployerIdentificationNumber = F, SkywardID = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, searchConditionsGroupType = "And", searchSortFieldNamesList = NULL, searchSortFieldNamesDescendingList = NULL, entityID = 1, schoolYearID = NULL, page = 1, pageSize = 100000, flatten = T, ...){

		params <- as.list(environment()) %>% append(list(...))

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- "all", searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		listSkyObjects(module = "StateReportingFederal", objectName = "ACAYearOtherALEMemberOfAggregatedGroupV1", schoolYearID = schoolYearID, searchFields = searchFields, page = page, pageSize = pageSize, SearchConditionsList = searchConditionsList, SearchConditionsGroupType = searchConditionsGroupType, SearchSortFieldNamesList = searchSortFieldNamesList, SearchSortFieldNamesDescendingList = searchSortFieldNamesDescendingList, entityId = entityID, flatten = flatten)
	}


	listStateCalendarYears <- function(searchConditionsList = NULL, StateCalendarYearID = F, SkywardID = F, CombinedStateFederalFilingCode = F, CalendarYear = F, StateID = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, SkywardHash = F, searchConditionsGroupType = "And", searchSortFieldNamesList = NULL, searchSortFieldNamesDescendingList = NULL, entityID = 1, schoolYearID = NULL, page = 1, pageSize = 100000, flatten = T, ...){

		params <- as.list(environment()) %>% append(list(...))

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- "all", searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		listSkyObjects(module = "StateReportingFederal", objectName = "StateCalendarYear", schoolYearID = schoolYearID, searchFields = searchFields, page = page, pageSize = pageSize, SearchConditionsList = searchConditionsList, SearchConditionsGroupType = searchConditionsGroupType, SearchSortFieldNamesList = searchSortFieldNamesList, SearchSortFieldNamesDescendingList = searchSortFieldNamesDescendingList, entityId = entityID, flatten = flatten)
	}


	listW2ExportPrintFiles <- function(searchConditionsList = NULL, W2ExportPrintFileID = F, W2YearID = F, MediaID = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, searchConditionsGroupType = "And", searchSortFieldNamesList = NULL, searchSortFieldNamesDescendingList = NULL, entityID = 1, schoolYearID = NULL, page = 1, pageSize = 100000, flatten = T, ...){

		params <- as.list(environment()) %>% append(list(...))

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- "all", searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		listSkyObjects(module = "StateReportingFederal", objectName = "W2ExportPrintFile", schoolYearID = schoolYearID, searchFields = searchFields, page = page, pageSize = pageSize, SearchConditionsList = searchConditionsList, SearchConditionsGroupType = searchConditionsGroupType, SearchSortFieldNamesList = searchSortFieldNamesList, SearchSortFieldNamesDescendingList = searchSortFieldNamesDescendingList, entityId = entityID, flatten = flatten)
	}


	listForm1099MExportElectronicFiles <- function(searchConditionsList = NULL, Form1099MExportElectronicFileID = F, Form1099MYearID = F, TransmitterControlCode = F, IsCombinedStateFederal = F, IsTestFile = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, searchConditionsGroupType = "And", searchSortFieldNamesList = NULL, searchSortFieldNamesDescendingList = NULL, entityID = 1, schoolYearID = NULL, page = 1, pageSize = 100000, flatten = T, ...){

		params <- as.list(environment()) %>% append(list(...))

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- "all", searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		listSkyObjects(module = "StateReportingFederal", objectName = "Form1099MExportElectronicFile", schoolYearID = schoolYearID, searchFields = searchFields, page = page, pageSize = pageSize, SearchConditionsList = searchConditionsList, SearchConditionsGroupType = searchConditionsGroupType, SearchSortFieldNamesList = searchSortFieldNamesList, SearchSortFieldNamesDescendingList = searchSortFieldNamesDescendingList, entityId = entityID, flatten = flatten)
	}


	listACAYears <- function(searchConditionsList = NULL, ACAYearID = F, DistrictID = F, CalendarYear = F, EmployerID = F, EmployerBusinessNamePrinted = F, EmployerAddressLine1Printed = F, EmployerAddressLine2Printed = F, EmployerCityPrinted = F, EmployerIdentificationNumber = F, EmployerContactPersonFirstName = F, EmployerContactPersonLastName = F, EmployerContactPersonMiddleName = F, EmployerContactPersonSuffix = F, EmployerContactTelephoneNumber = F, EmployerStateIDPrinted = F, EmployerStateCodePrinted = F, EmployerZipCodePrinted = F, EmployerZipCodeExtensionPrinted = F, IsTransmittalForThisALEMember = F, IsALEMemberPartOfAggregatedALEGroup = F, IsQualifyingOfferMethod = F, IsQualifyingOfferMethodTransitionRelief = F, IsSection4980HTransitionRelief = F, Is98PercentOfferMethod = F, TotalForm1095CFiledByALEMember = F, SkywardID = F, TestScenarioIdentification = F, DesignatedGovernmentEntityBusinessNameLine1 = F, DesignatedGovernmentEntityBusinessNameLine2 = F, DesignatedGovernmentEntityEmployerIdentificationNumber = F, DesignatedGovernmentEntityAddressLine1 = F, DesignatedGovernmentEntityAddressLine2 = F, DesignatedGovernmentEntityCity = F, DesignatedGovernmentEntityStateID = F, DesignatedGovernmentEntityStateCode = F, DesignatedGovernmentEntityZipCode = F, DesignatedGovernmentEntityZipCodeExtension = F, DesignatedGovernmentEntityContactPersonFirstName = F, DesignatedGovernmentEntityContactPersonMiddleName = F, DesignatedGovernmentEntityContactPersonLastName = F, DesignatedGovernmentEntityContactPersonSuffix = F, DesignatedGovernmentEntityContactPhoneNumber = F, OffersMinimumEssentialCoverage = F, FullTimeEmployeeCount = F, TotalEmployeeCount = F, IsAggregatedGroup = F, Section4980HTransitionReliefCode = F, IsAvailableInEmployeeAccess = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, searchConditionsGroupType = "And", searchSortFieldNamesList = NULL, searchSortFieldNamesDescendingList = NULL, entityID = 1, schoolYearID = NULL, page = 1, pageSize = 100000, flatten = T, ...){

		params <- as.list(environment()) %>% append(list(...))

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- "all", searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		listSkyObjects(module = "StateReportingFederal", objectName = "ACAYear", schoolYearID = schoolYearID, searchFields = searchFields, page = page, pageSize = pageSize, SearchConditionsList = searchConditionsList, SearchConditionsGroupType = searchConditionsGroupType, SearchSortFieldNamesList = searchSortFieldNamesList, SearchSortFieldNamesDescendingList = searchSortFieldNamesDescendingList, entityId = entityID, flatten = flatten)
	}


	listACAYearEmployeeCoveredIndividualV1s <- function(searchConditionsList = NULL, ACAYearEmployeeCoveredIndividualV1ID = F, ACAYearEmployeeV1ID = F, NameID = F, CoveredIndividualNamePrinted = F, CoveredIndividualFirstNameElectronicFile = F, CoveredIndividualLastNameElectronicFile = F, CoveredIndividualMiddleNameElectronicFile = F, CoveredIndividualSuffixElectronicFile = F, SocialSecurityNumber = F, BirthDate = F, IsCoveredJanuary = F, IsCoveredFebruary = F, IsCoveredMarch = F, IsCoveredApril = F, IsCoveredMay = F, IsCoveredJune = F, IsCoveredJuly = F, IsCoveredAugust = F, IsCoveredSeptember = F, IsCoveredOctober = F, IsCoveredNovember = F, IsCoveredDecember = F, SkywardID = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, searchConditionsGroupType = "And", searchSortFieldNamesList = NULL, searchSortFieldNamesDescendingList = NULL, entityID = 1, schoolYearID = NULL, page = 1, pageSize = 100000, flatten = T, ...){

		params <- as.list(environment()) %>% append(list(...))

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- "all", searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		listSkyObjects(module = "StateReportingFederal", objectName = "ACAYearEmployeeCoveredIndividualV1", schoolYearID = schoolYearID, searchFields = searchFields, page = page, pageSize = pageSize, SearchConditionsList = searchConditionsList, SearchConditionsGroupType = searchConditionsGroupType, SearchSortFieldNamesList = searchSortFieldNamesList, SearchSortFieldNamesDescendingList = searchSortFieldNamesDescendingList, entityId = entityID, flatten = flatten)
	}


	listACAYearEmployeeOfferAndCoverageV1s <- function(searchConditionsList = NULL, ACAYearEmployeeOfferAndCoverageV1ID = F, ACAYearEmployeeV1ID = F, FederalACAOfferAndCoverageID = F, FederalACAOfferAndCoverageCode = F, FederalACASafeHarborID = F, FederalACASafeHarborCode = F, EmployeeShareOfLowestCostMonthlyPremium = F, Month = F, SkywardID = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, searchConditionsGroupType = "And", searchSortFieldNamesList = NULL, searchSortFieldNamesDescendingList = NULL, entityID = 1, schoolYearID = NULL, page = 1, pageSize = 100000, flatten = T, ...){

		params <- as.list(environment()) %>% append(list(...))

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- "all", searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		listSkyObjects(module = "StateReportingFederal", objectName = "ACAYearEmployeeOfferAndCoverageV1", schoolYearID = schoolYearID, searchFields = searchFields, page = page, pageSize = pageSize, SearchConditionsList = searchConditionsList, SearchConditionsGroupType = searchConditionsGroupType, SearchSortFieldNamesList = searchSortFieldNamesList, SearchSortFieldNamesDescendingList = searchSortFieldNamesDescendingList, entityId = entityID, flatten = flatten)
	}


	listACAYearEmployeeV1s <- function(searchConditionsList = NULL, ACAYearEmployeeV1ID = F, ACAYearID = F, EmployeeID = F, NamePrinted = F, SocialSecurityNumber = F, AddressLine1Printed = F, AddressLine2Printed = F, CityPrinted = F, FirstNameElectronicFile = F, LastNameElectronicFile = F, MiddleNameElectronicFile = F, SuffixElectronicFile = F, AddressLine1ElectronicFile = F, AddressLine2ElectronicFile = F, CityElectronicFile = F, StateID = F, StateCode = F, ZipCode = F, ZipCodeExtension = F, FederalACAOfferAndCoverageID = F, FederalACAOfferAndCoverageCode = F, FederalACASafeHarborID = F, FederalACASafeHarborCode = F, EmployeeShareOfLowestCostMonthlyPremium = F, SkywardID = F, TestScenarioIdentification = F, ContactPhoneNumber = F, PlanStartMonth = F, EmployerOfferedSelfInsuredCoverage = F, MediaIDFullSSN = F, MediaIDMaskedSSN = F, XMLData = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, AttachmentCount = F, searchConditionsGroupType = "And", searchSortFieldNamesList = NULL, searchSortFieldNamesDescendingList = NULL, entityID = 1, schoolYearID = NULL, page = 1, pageSize = 100000, flatten = T, ...){

		params <- as.list(environment()) %>% append(list(...))

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- "all", searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		listSkyObjects(module = "StateReportingFederal", objectName = "ACAYearEmployeeV1", schoolYearID = schoolYearID, searchFields = searchFields, page = page, pageSize = pageSize, SearchConditionsList = searchConditionsList, SearchConditionsGroupType = searchConditionsGroupType, SearchSortFieldNamesList = searchSortFieldNamesList, SearchSortFieldNamesDescendingList = searchSortFieldNamesDescendingList, entityId = entityID, flatten = flatten)
	}


	listW2YearExceptions <- function(searchConditionsList = NULL, W2YearExceptionID = F, W2YearID = F, EmployeeID = F, IsFatalException = F, ExceptionType = F, Description = F, SourceType = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, W2YearEmployeeV1ID = F, searchConditionsGroupType = "And", searchSortFieldNamesList = NULL, searchSortFieldNamesDescendingList = NULL, entityID = 1, schoolYearID = NULL, page = 1, pageSize = 100000, flatten = T, ...){

		params <- as.list(environment()) %>% append(list(...))

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- "all", searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		listSkyObjects(module = "StateReportingFederal", objectName = "W2YearException", schoolYearID = schoolYearID, searchFields = searchFields, page = page, pageSize = pageSize, SearchConditionsList = searchConditionsList, SearchConditionsGroupType = searchConditionsGroupType, SearchSortFieldNamesList = searchSortFieldNamesList, SearchSortFieldNamesDescendingList = searchSortFieldNamesDescendingList, entityId = entityID, flatten = flatten)
	}


	listACAYearExportElectronicFiles <- function(searchConditionsList = NULL, ACAYearExportElectronicFileID = F, ACAYearID = F, MediaIDManifest = F, IsPriorYearData = F, TransmissionType = F, OriginalReceiptIdentification = F, TransmitterControlCode = F, TransmitterEmployerIdentificationNumber = F, TransmitterBusinessNameLine1 = F, TransmitterBusinessNameLine2 = F, CompanyName = F, CompanyAddressLine1 = F, CompanyAddressLine2 = F, CompanyCity = F, CompanyStateID = F, CompanyStateCode = F, CompanyZipCode = F, CompanyZipCodeExtension = F, CompanyContactPersonFirstName = F, CompanyContactPersonLastName = F, CompanyContactPersonMiddleName = F, CompanyContactPersonSuffix = F, CompanyContactPhoneNumber = F, EmployerBusinessNameLine1 = F, EmployerBusinessNameLine2 = F, EmployerAddressLine1 = F, EmployerAddressLine2 = F, EmployerCity = F, EmployerStateID = F, EmployerStateCode = F, EmployerZipCode = F, EmployerZipCodeExtension = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, MediaID = F, Status = F, searchConditionsGroupType = "And", searchSortFieldNamesList = NULL, searchSortFieldNamesDescendingList = NULL, entityID = 1, schoolYearID = NULL, page = 1, pageSize = 100000, flatten = T, ...){

		params <- as.list(environment()) %>% append(list(...))

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- "all", searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		listSkyObjects(module = "StateReportingFederal", objectName = "ACAYearExportElectronicFile", schoolYearID = schoolYearID, searchFields = searchFields, page = page, pageSize = pageSize, SearchConditionsList = searchConditionsList, SearchConditionsGroupType = searchConditionsGroupType, SearchSortFieldNamesList = searchSortFieldNamesList, SearchSortFieldNamesDescendingList = searchSortFieldNamesDescendingList, entityId = entityID, flatten = flatten)
	}


	listFederalW2KindOfEmployers <- function(searchConditionsList = NULL, FederalW2KindOfEmployerID = F, SkywardID = F, Code = F, Description = F, CodeDescription = F, Year = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, SkywardHash = F, searchConditionsGroupType = "And", searchSortFieldNamesList = NULL, searchSortFieldNamesDescendingList = NULL, entityID = 1, schoolYearID = NULL, page = 1, pageSize = 100000, flatten = T, ...){

		params <- as.list(environment()) %>% append(list(...))

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- "all", searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		listSkyObjects(module = "StateReportingFederal", objectName = "FederalW2KindOfEmployer", schoolYearID = schoolYearID, searchFields = searchFields, page = page, pageSize = pageSize, SearchConditionsList = searchConditionsList, SearchConditionsGroupType = searchConditionsGroupType, SearchSortFieldNamesList = searchSortFieldNamesList, SearchSortFieldNamesDescendingList = searchSortFieldNamesDescendingList, entityId = entityID, flatten = flatten)
	}


	listW2ExportElectronicFiles <- function(searchConditionsList = NULL, W2ExportElectronicFileID = F, W2YearID = F, MediaID = F, SubmitterUserIdentification = F, HasEmployerThirdPartySickPay = F, IncomeTaxWithheldByPayerOfThirdPartySickPay = F, IsResubmission = F, ResubmissionWageFileIdentifier = F, Status = F, EmployerName = F, EmployerLocationAddress = F, EmployerDeliveryAddress = F, EmployerCity = F, EmployerStateID = F, EmployerStateCode = F, EmployerZipCode = F, EmployerZipCodeExtension = F, EmployerContactName = F, EmployerContactPhoneNumber = F, EmployerContactPhoneExtension = F, EmployerContactEmailAddress = F, EmployerContactFax = F, CompanyName = F, CompanyLocationAddress = F, CompanyDeliveryAddress = F, CompanyCity = F, CompanyStateID = F, CompanyStateCode = F, CompanyZipCode = F, CompanyZipCodeExtension = F, SubmitterEmployerIdentificationNumber = F, SubmitterName = F, SubmitterLocationAddress = F, SubmitterDeliveryAddress = F, SubmitterCity = F, SubmitterStateID = F, SubmitterStateCode = F, SubmitterZipCode = F, SubmitterZipCodeExtension = F, SubmitterContactName = F, SubmitterContactPhoneNumber = F, SubmitterContactPhoneExtension = F, SubmitterContactEmailAddress = F, SubmitterContactFax = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, Type = F, IowaBusinessEFileNumber = F, EmployerAddressType = F, EmployerPostalCode = F, EmployerCountryCode = F, EmployerW2CountryID = F, EmployerInternationalStateOrProvinceCode = F, EmployerInternationalProvinceID = F, CompanyAddressType = F, CompanyPostalCode = F, CompanyCountryCode = F, CompanyW2CountryID = F, CompanyInternationalStateOrProvinceCode = F, CompanyInternationalProvinceID = F, SubmitterAddressType = F, SubmitterPostalCode = F, SubmitterCountryCode = F, SubmitterW2CountryID = F, SubmitterInternationalStateOrProvinceCode = F, SubmitterInternationalProvinceID = F, LocalityName = F, searchConditionsGroupType = "And", searchSortFieldNamesList = NULL, searchSortFieldNamesDescendingList = NULL, entityID = 1, schoolYearID = NULL, page = 1, pageSize = 100000, flatten = T, ...){

		params <- as.list(environment()) %>% append(list(...))

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- "all", searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		listSkyObjects(module = "StateReportingFederal", objectName = "W2ExportElectronicFile", schoolYearID = schoolYearID, searchFields = searchFields, page = page, pageSize = pageSize, SearchConditionsList = searchConditionsList, SearchConditionsGroupType = searchConditionsGroupType, SearchSortFieldNamesList = searchSortFieldNamesList, SearchSortFieldNamesDescendingList = searchSortFieldNamesDescendingList, entityId = entityID, flatten = flatten)
	}


	listW2YearEmployeeStateV1s <- function(searchConditionsList = NULL, W2YearEmployeeStateV1ID = F, W2YearEmployeeV1ID = F, W2YearTaxStateIDBox15 = F, Box16StateWages = F, Box17StateIncomeTax = F, IsOriginalExtractedValue = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, StateAllowance = F, searchConditionsGroupType = "And", searchSortFieldNamesList = NULL, searchSortFieldNamesDescendingList = NULL, entityID = 1, schoolYearID = NULL, page = 1, pageSize = 100000, flatten = T, ...){

		params <- as.list(environment()) %>% append(list(...))

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- "all", searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		listSkyObjects(module = "StateReportingFederal", objectName = "W2YearEmployeeStateV1", schoolYearID = schoolYearID, searchFields = searchFields, page = page, pageSize = pageSize, SearchConditionsList = searchConditionsList, SearchConditionsGroupType = searchConditionsGroupType, SearchSortFieldNamesList = searchSortFieldNamesList, SearchSortFieldNamesDescendingList = searchSortFieldNamesDescendingList, entityId = entityID, flatten = flatten)
	}


	listW2YearEmployeeBox14V1s <- function(searchConditionsList = NULL, W2YearEmployeeBox14V1ID = F, W2YearEmployeeV1ID = F, W2YearBox14SetID = F, Description = F, Amount = F, IsOriginalExtractedValue = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, FederalW2BoxID = F, Code = F, OregonTransitTaxWages = F, searchConditionsGroupType = "And", searchSortFieldNamesList = NULL, searchSortFieldNamesDescendingList = NULL, entityID = 1, schoolYearID = NULL, page = 1, pageSize = 100000, flatten = T, ...){

		params <- as.list(environment()) %>% append(list(...))

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- "all", searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		listSkyObjects(module = "StateReportingFederal", objectName = "W2YearEmployeeBox14V1", schoolYearID = schoolYearID, searchFields = searchFields, page = page, pageSize = pageSize, SearchConditionsList = searchConditionsList, SearchConditionsGroupType = searchConditionsGroupType, SearchSortFieldNamesList = searchSortFieldNamesList, SearchSortFieldNamesDescendingList = searchSortFieldNamesDescendingList, entityId = entityID, flatten = flatten)
	}


	listW2YearEmployeeBox12V1s <- function(searchConditionsList = NULL, W2YearEmployeeBox12V1ID = F, W2YearEmployeeV1ID = F, FederalW2BoxID = F, Code = F, Amount = F, IsOriginalExtractedValue = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, searchConditionsGroupType = "And", searchSortFieldNamesList = NULL, searchSortFieldNamesDescendingList = NULL, entityID = 1, schoolYearID = NULL, page = 1, pageSize = 100000, flatten = T, ...){

		params <- as.list(environment()) %>% append(list(...))

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- "all", searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		listSkyObjects(module = "StateReportingFederal", objectName = "W2YearEmployeeBox12V1", schoolYearID = schoolYearID, searchFields = searchFields, page = page, pageSize = pageSize, SearchConditionsList = searchConditionsList, SearchConditionsGroupType = searchConditionsGroupType, SearchSortFieldNamesList = searchSortFieldNamesList, SearchSortFieldNamesDescendingList = searchSortFieldNamesDescendingList, entityId = entityID, flatten = flatten)
	}


	listW2YearEmployeeV1s <- function(searchConditionsList = NULL, W2YearEmployeeV1ID = F, W2YearID = F, EmployeeID = F, AddressID = F, LocationAddressPrinted = F, DeliveryAddressPrinted = F, CityPrinted = F, StateID = F, StateCode = F, ZipCode = F, ZipCodeExtension = F, LocationAddressElectronicFile = F, DeliveryAddressElectronicFile = F, CityElectronicFile = F, EmployeeNamePrinted = F, FirstNameElectronicFile = F, MiddleNameElectronicFile = F, LastNameElectronicFile = F, SuffixElectronicFile = F, EmployeeSocialSecurityNumber = F, MaskedEmployeeSocialSecurityNumber = F, Box1FederalWages = F, Box1FederalWagesExtracted = F, Box2FederalIncomeTaxWithheld = F, Box2FederalIncomeTaxWithheldExtracted = F, Box3SocialSecurityWages = F, Box3SocialSecurityWagesExtracted = F, Box4SocialSecurityTaxWithheld = F, Box4SocialSecurityTaxWithheldExtracted = F, Box5MedicareWages = F, Box5MedicareWagesExtracted = F, Box6MedicareTaxWithheld = F, Box6MedicareTaxWithheldExtracted = F, Box7SocialSecurityTips = F, Box9AdvanceEmployeeIncomeCredit = F, Box10DependentCareBenefits = F, Box10DependentCareBenefitsExtracted = F, Box11NonQualifiedPlansSection457b = F, Box11NonQualifiedPlansSection457bExtracted = F, Box11NonQualifiedPlansNonSection457b = F, Box11NonQualifiedPlansNonSection457bExtracted = F, IsBox13StatutoryEmployee = F, IsBox13RetirementPlan = F, IsBox13ThirdPartySickPay = F, MediaIDFullSSN = F, MediaIDMaskedSSN = F, XMLData = F, FatalExceptionCount = F, NonFatalExceptionCount = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, HasZeroWages = F, Box9VerificationCode = F, InternationalProvinceID = F, InternationalStateOrProvinceCode = F, AddressType = F, PostalCode = F, CountryCode = F, W2CountryID = F, searchConditionsGroupType = "And", searchSortFieldNamesList = NULL, searchSortFieldNamesDescendingList = NULL, entityID = 1, schoolYearID = NULL, page = 1, pageSize = 100000, flatten = T, ...){

		params <- as.list(environment()) %>% append(list(...))

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- "all", searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		listSkyObjects(module = "StateReportingFederal", objectName = "W2YearEmployeeV1", schoolYearID = schoolYearID, searchFields = searchFields, page = page, pageSize = pageSize, SearchConditionsList = searchConditionsList, SearchConditionsGroupType = searchConditionsGroupType, SearchSortFieldNamesList = searchSortFieldNamesList, SearchSortFieldNamesDescendingList = searchSortFieldNamesDescendingList, entityId = entityID, flatten = flatten)
	}


	listForm1099MYearVendors <- function(searchConditionsList = NULL, Form1099MYearVendorID = F, Form1099MYearID = F, VendorID = F, TaxIdentificationType = F, RecipientIdentificationNumber = F, HasPayerMade5000DirectSales = F, AddressID = F, AddressLine1Printed = F, AddressLine1ElectronicFile = F, AddressLine2Printed = F, AddressLine2ElectronicFile = F, CityPrinted = F, CityElectronicFile = F, StateID = F, StateCode = F, ZipCode = F, ZipCodeAddOn = F, VendorNamePrintedLine1 = F, VendorNameElectronicFileLine1 = F, MediaID = F, XMLData = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, VendorNamePrintedLine2 = F, VendorNameElectronicFileLine2 = F, searchConditionsGroupType = "And", searchSortFieldNamesList = NULL, searchSortFieldNamesDescendingList = NULL, entityID = 1, schoolYearID = NULL, page = 1, pageSize = 100000, flatten = T, ...){

		params <- as.list(environment()) %>% append(list(...))

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- "all", searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		listSkyObjects(module = "StateReportingFederal", objectName = "Form1099MYearVendor", schoolYearID = schoolYearID, searchFields = searchFields, page = page, pageSize = pageSize, SearchConditionsList = searchConditionsList, SearchConditionsGroupType = searchConditionsGroupType, SearchSortFieldNamesList = searchSortFieldNamesList, SearchSortFieldNamesDescendingList = searchSortFieldNamesDescendingList, entityId = entityID, flatten = flatten)
	}


	listForm1099MYears <- function(searchConditionsList = NULL, Form1099MYearID = F, DistrictID = F, CalendarYear = F, EmployerID = F, FederalIDNumberEmployer = F, AddressIDEmployer = F, AddressLine1Printed = F, AddressLine1ElectronicFile = F, AddressLine2Printed = F, AddressLine2ElectronicFile = F, CityPrinted = F, CityElectronicFile = F, StateID = F, StateCode = F, ZipCode = F, ZipCodeAddOn = F, EmployerNamePrinted = F, EmployerNameElectronicFile = F, PhoneNumberPrinted = F, PhoneNumberElectronicFile = F, PhoneExtensionPrinted = F, PhoneExtensionElectronicFile = F, PhoneNumberIsInternational = F, ContactPersonElectronicFile = F, FormattedPhoneNumber = F, IsLocked = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, FatalExceptionCount = F, NonFatalExceptionCount = F, ExtractedVendorCount = F, LatestExport = F, WithholdingTaxAccountNumberPA = F, ReportsPAIncome = F, ReportsWIIncome = F, ReportsILIncome = F, StateIdentificationNumberDE = F, WithholdingAccountNumberGA = F, WithholdingPermitNumberIA = F, WithholdingAccountNumberID = F, WithholdingTaxAccountNumberKY = F, CentralRegistrationNumberMD = F, WithholdingAccountNumberNC = F, IdentificationNumberNE = F, CombinedReportingSystemIdentificationNumberNM = F, BusinessIdentificationNumberOR = F, WithholdingFileNumberSC = F, WithholdingAccountNumberUT = F, WithholdingTaxNumberWI = F, TaxPeriodYear = F, TaxPeriodMonth = F, FilingCycle = F, ReportsCAIncome = F, ReportsDEIncome = F, ReportsGAIncome = F, ReportsIAIncome = F, ReportsIDIncome = F, ReportsKYIncome = F, ReportsMDIncome = F, ReportsNCIncome = F, ReportsNEIncome = F, ReportsNMIncome = F, ReportsORIncome = F, ReportsSCIncome = F, ReportsUTIncome = F, ReportsWVIncome = F, searchConditionsGroupType = "And", searchSortFieldNamesList = NULL, searchSortFieldNamesDescendingList = NULL, entityID = 1, schoolYearID = NULL, page = 1, pageSize = 100000, flatten = T, ...){

		params <- as.list(environment()) %>% append(list(...))

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- "all", searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		listSkyObjects(module = "StateReportingFederal", objectName = "Form1099MYear", schoolYearID = schoolYearID, searchFields = searchFields, page = page, pageSize = pageSize, SearchConditionsList = searchConditionsList, SearchConditionsGroupType = searchConditionsGroupType, SearchSortFieldNamesList = searchSortFieldNamesList, SearchSortFieldNamesDescendingList = searchSortFieldNamesDescendingList, entityId = entityID, flatten = flatten)
	}


	listW2YearPayTypes <- function(searchConditionsList = NULL, W2YearPayTypeID = F, W2YearID = F, PayTypeID = F, FederalW2BoxID = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, searchConditionsGroupType = "And", searchSortFieldNamesList = NULL, searchSortFieldNamesDescendingList = NULL, entityID = 1, schoolYearID = NULL, page = 1, pageSize = 100000, flatten = T, ...){

		params <- as.list(environment()) %>% append(list(...))

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- "all", searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		listSkyObjects(module = "StateReportingFederal", objectName = "W2YearPayType", schoolYearID = schoolYearID, searchFields = searchFields, page = page, pageSize = pageSize, SearchConditionsList = searchConditionsList, SearchConditionsGroupType = searchConditionsGroupType, SearchSortFieldNamesList = searchSortFieldNamesList, SearchSortFieldNamesDescendingList = searchSortFieldNamesDescendingList, entityId = entityID, flatten = flatten)
	}


	listW2YearBenefits <- function(searchConditionsList = NULL, W2YearBenefitID = F, W2YearID = F, BenefitID = F, FederalW2BoxID = F, W2YearBox14SetID = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, searchConditionsGroupType = "And", searchSortFieldNamesList = NULL, searchSortFieldNamesDescendingList = NULL, entityID = 1, schoolYearID = NULL, page = 1, pageSize = 100000, flatten = T, ...){

		params <- as.list(environment()) %>% append(list(...))

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- "all", searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		listSkyObjects(module = "StateReportingFederal", objectName = "W2YearBenefit", schoolYearID = schoolYearID, searchFields = searchFields, page = page, pageSize = pageSize, SearchConditionsList = searchConditionsList, SearchConditionsGroupType = searchConditionsGroupType, SearchSortFieldNamesList = searchSortFieldNamesList, SearchSortFieldNamesDescendingList = searchSortFieldNamesDescendingList, entityId = entityID, flatten = flatten)
	}


	listW2YearDeductions <- function(searchConditionsList = NULL, W2YearDeductionID = F, W2YearID = F, DeductionID = F, FederalW2BoxID = F, W2YearBox14SetID = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, searchConditionsGroupType = "And", searchSortFieldNamesList = NULL, searchSortFieldNamesDescendingList = NULL, entityID = 1, schoolYearID = NULL, page = 1, pageSize = 100000, flatten = T, ...){

		params <- as.list(environment()) %>% append(list(...))

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- "all", searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		listSkyObjects(module = "StateReportingFederal", objectName = "W2YearDeduction", schoolYearID = schoolYearID, searchFields = searchFields, page = page, pageSize = pageSize, SearchConditionsList = searchConditionsList, SearchConditionsGroupType = searchConditionsGroupType, SearchSortFieldNamesList = searchSortFieldNamesList, SearchSortFieldNamesDescendingList = searchSortFieldNamesDescendingList, entityId = entityID, flatten = flatten)
	}


	listW2YearTaxStates <- function(searchConditionsList = NULL, W2YearTaxStateID = F, W2YearID = F, TaxStateID = F, State = F, EmployerIdentificationNumberState = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, searchConditionsGroupType = "And", searchSortFieldNamesList = NULL, searchSortFieldNamesDescendingList = NULL, entityID = 1, schoolYearID = NULL, page = 1, pageSize = 100000, flatten = T, ...){

		params <- as.list(environment()) %>% append(list(...))

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- "all", searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		listSkyObjects(module = "StateReportingFederal", objectName = "W2YearTaxState", schoolYearID = schoolYearID, searchFields = searchFields, page = page, pageSize = pageSize, SearchConditionsList = searchConditionsList, SearchConditionsGroupType = searchConditionsGroupType, SearchSortFieldNamesList = searchSortFieldNamesList, SearchSortFieldNamesDescendingList = searchSortFieldNamesDescendingList, entityId = entityID, flatten = flatten)
	}


	listW2YearBox14Sets <- function(searchConditionsList = NULL, W2YearBox14SetID = F, W2YearID = F, W2Description = F, W2Box14SetID = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, searchConditionsGroupType = "And", searchSortFieldNamesList = NULL, searchSortFieldNamesDescendingList = NULL, entityID = 1, schoolYearID = NULL, page = 1, pageSize = 100000, flatten = T, ...){

		params <- as.list(environment()) %>% append(list(...))

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- "all", searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		listSkyObjects(module = "StateReportingFederal", objectName = "W2YearBox14Set", schoolYearID = schoolYearID, searchFields = searchFields, page = page, pageSize = pageSize, SearchConditionsList = searchConditionsList, SearchConditionsGroupType = searchConditionsGroupType, SearchSortFieldNamesList = searchSortFieldNamesList, SearchSortFieldNamesDescendingList = searchSortFieldNamesDescendingList, entityId = entityID, flatten = flatten)
	}


	listW2Years <- function(searchConditionsList = NULL, W2YearID = F, DistrictID = F, CalendarYear = F, FederalW2KindOfEmployerID = F, EmployerID = F, EmployerNamePrinted = F, EmployerLocationAddressPrinted = F, EmployerDeliveryAddressPrinted = F, EmployerCityPrinted = F, EmployerStateIDPrinted = F, EmployerStateCodePrinted = F, EmployerZipCodePrinted = F, EmployerZipCodeExtensionPrinted = F, EmployerIdentificationNumber = F, IsAvailableInEmployeeAccess = F, Box13RetirementOption = F, IsLocked = F, FatalExceptionCount = F, NonFatalExceptionCount = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, ExtractedEmployeeCount = F, LatestExport = F, Exclude0WageEmployeesWithOnlyEmployerSponsoredHealthCoverage = F, IsW2Audit = F, EmployerAddressType = F, EmployerInternationalProvinceIDPrinted = F, EmployerInternationalStateOrProvinceCodePrinted = F, EmployerPostalCodePrinted = F, EmployerCountryCodePrinted = F, EmployerW2CountryIDPrinted = F, IsSubmitted = F, searchConditionsGroupType = "And", searchSortFieldNamesList = NULL, searchSortFieldNamesDescendingList = NULL, entityID = 1, schoolYearID = NULL, page = 1, pageSize = 100000, flatten = T, ...){

		params <- as.list(environment()) %>% append(list(...))

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- "all", searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		listSkyObjects(module = "StateReportingFederal", objectName = "W2Year", schoolYearID = schoolYearID, searchFields = searchFields, page = page, pageSize = pageSize, SearchConditionsList = searchConditionsList, SearchConditionsGroupType = searchConditionsGroupType, SearchSortFieldNamesList = searchSortFieldNamesList, SearchSortFieldNamesDescendingList = searchSortFieldNamesDescendingList, entityId = entityID, flatten = flatten)
	}


	listAffordableCareActEmployeeWeekV1s <- function(searchConditionsList = NULL, AffordableCareActEmployeeWeekV1ID = F, AffordableCareActExtractRunID = F, EmployeeID = F, WeekStartDate = F, Amount = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, searchConditionsGroupType = "And", searchSortFieldNamesList = NULL, searchSortFieldNamesDescendingList = NULL, entityID = 1, schoolYearID = NULL, page = 1, pageSize = 100000, flatten = T, ...){

		params <- as.list(environment()) %>% append(list(...))

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- "all", searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		listSkyObjects(module = "StateReportingFederal", objectName = "AffordableCareActEmployeeWeekV1", schoolYearID = schoolYearID, searchFields = searchFields, page = page, pageSize = pageSize, SearchConditionsList = searchConditionsList, SearchConditionsGroupType = searchConditionsGroupType, SearchSortFieldNamesList = searchSortFieldNamesList, SearchSortFieldNamesDescendingList = searchSortFieldNamesDescendingList, entityId = entityID, flatten = flatten)
	}


	listAffordableCareActExtractRuns <- function(searchConditionsList = NULL, AffordableCareActExtractRunID = F, Version = F, Description = F, DistrictID = F, StartDate = F, EndDate = F, XMLFilter = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, searchConditionsGroupType = "And", searchSortFieldNamesList = NULL, searchSortFieldNamesDescendingList = NULL, entityID = 1, schoolYearID = NULL, page = 1, pageSize = 100000, flatten = T, ...){

		params <- as.list(environment()) %>% append(list(...))

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- "all", searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		listSkyObjects(module = "StateReportingFederal", objectName = "AffordableCareActExtractRun", schoolYearID = schoolYearID, searchFields = searchFields, page = page, pageSize = pageSize, SearchConditionsList = searchConditionsList, SearchConditionsGroupType = searchConditionsGroupType, SearchSortFieldNamesList = searchSortFieldNamesList, SearchSortFieldNamesDescendingList = searchSortFieldNamesDescendingList, entityId = entityID, flatten = flatten)
	}


	listForm941Exports <- function(searchConditionsList = NULL, Form941ExportID = F, Form941ExtractRunID = F, Form = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, MediaID = F, Status = F, searchConditionsGroupType = "And", searchSortFieldNamesList = NULL, searchSortFieldNamesDescendingList = NULL, entityID = 1, schoolYearID = NULL, page = 1, pageSize = 100000, flatten = T, ...){

		params <- as.list(environment()) %>% append(list(...))

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- "all", searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		listSkyObjects(module = "StateReportingFederal", objectName = "Form941Export", schoolYearID = schoolYearID, searchFields = searchFields, page = page, pageSize = pageSize, SearchConditionsList = searchConditionsList, SearchConditionsGroupType = searchConditionsGroupType, SearchSortFieldNamesList = searchSortFieldNamesList, SearchSortFieldNamesDescendingList = searchSortFieldNamesDescendingList, entityId = entityID, flatten = flatten)
	}


	listForm941V1Days <- function(searchConditionsList = NULL, Form941V1DayID = F, Form941V1ID = F, Date = F, TaxLiability = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, NonRefundableEmployeeRetentionCredit = F, NonRefundableQualifiedSickAndFamilyLeaveCredit = F, ReportedTaxLiability = F, searchConditionsGroupType = "And", searchSortFieldNamesList = NULL, searchSortFieldNamesDescendingList = NULL, entityID = 1, schoolYearID = NULL, page = 1, pageSize = 100000, flatten = T, ...){

		params <- as.list(environment()) %>% append(list(...))

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- "all", searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		listSkyObjects(module = "StateReportingFederal", objectName = "Form941V1Day", schoolYearID = schoolYearID, searchFields = searchFields, page = page, pageSize = pageSize, SearchConditionsList = searchConditionsList, SearchConditionsGroupType = searchConditionsGroupType, SearchSortFieldNamesList = searchSortFieldNamesList, SearchSortFieldNamesDescendingList = searchSortFieldNamesDescendingList, entityId = entityID, flatten = flatten)
	}


	listForm941ExtractRunIncludedPayrolls <- function(searchConditionsList = NULL, Form941ExtractRunIncludedPayrollID = F, Form941ExtractRunID = F, PayrollRunID = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, searchConditionsGroupType = "And", searchSortFieldNamesList = NULL, searchSortFieldNamesDescendingList = NULL, entityID = 1, schoolYearID = NULL, page = 1, pageSize = 100000, flatten = T, ...){

		params <- as.list(environment()) %>% append(list(...))

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- "all", searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		listSkyObjects(module = "StateReportingFederal", objectName = "Form941ExtractRunIncludedPayroll", schoolYearID = schoolYearID, searchFields = searchFields, page = page, pageSize = pageSize, SearchConditionsList = searchConditionsList, SearchConditionsGroupType = searchConditionsGroupType, SearchSortFieldNamesList = searchSortFieldNamesList, SearchSortFieldNamesDescendingList = searchSortFieldNamesDescendingList, entityId = entityID, flatten = flatten)
	}


	listForm941ExtractRuns <- function(searchConditionsList = NULL, Form941ExtractRunID = F, DistrictIDExtractedFor = F, PostingQuarter = F, PostingYear = F, XMLFilter = F, PostingQuarterYear = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, Version = F, FatalExceptionCount = F, NonFatalExceptionCount = F, LatestExport = F, LatestSentOrAcceptedExportTime = F, LatestForm941Export = F, LatestForm941ScheduleBExport = F, searchConditionsGroupType = "And", searchSortFieldNamesList = NULL, searchSortFieldNamesDescendingList = NULL, entityID = 1, schoolYearID = NULL, page = 1, pageSize = 100000, flatten = T, ...){

		params <- as.list(environment()) %>% append(list(...))

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- "all", searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		listSkyObjects(module = "StateReportingFederal", objectName = "Form941ExtractRun", schoolYearID = schoolYearID, searchFields = searchFields, page = page, pageSize = pageSize, SearchConditionsList = searchConditionsList, SearchConditionsGroupType = searchConditionsGroupType, SearchSortFieldNamesList = searchSortFieldNamesList, SearchSortFieldNamesDescendingList = searchSortFieldNamesDescendingList, entityId = entityID, flatten = flatten)
	}


	listForm941V1s <- function(searchConditionsList = NULL, Form941V1ID = F, EmployerIdentificationNumber = F, EmployerName = F, EmployerPrimaryAddressLine = F, EmployerCity = F, EmployerState = F, EmployerZip = F, Form941ExtractRunID = F, Quarter = F, Status = F, EmployeesPaidOn12th = F, TaxableFederalWages = F, FederalTaxWithheld = F, TaxableSocialSecurityWages = F, SocialSecurityTaxLiability = F, TaxableMedicareWages = F, MedicareTaxLiability = F, AdditionalTaxableMedicareWages = F, TaxLiabilityMonth1 = F, TaxLiabilityMonth2 = F, TaxLiabilityMonth3 = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, NonRefundableEmployeeRetentionCreditTotal = F, NonRefundableQualifiedSickAndFamilyLeaveCreditTotal = F, searchConditionsGroupType = "And", searchSortFieldNamesList = NULL, searchSortFieldNamesDescendingList = NULL, entityID = 1, schoolYearID = NULL, page = 1, pageSize = 100000, flatten = T, ...){

		params <- as.list(environment()) %>% append(list(...))

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- "all", searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		listSkyObjects(module = "StateReportingFederal", objectName = "Form941V1", schoolYearID = schoolYearID, searchFields = searchFields, page = page, pageSize = pageSize, SearchConditionsList = searchConditionsList, SearchConditionsGroupType = searchConditionsGroupType, SearchSortFieldNamesList = searchSortFieldNamesList, SearchSortFieldNamesDescendingList = searchSortFieldNamesDescendingList, entityId = entityID, flatten = flatten)
	}


	listFederalEEO4Functions <- function(searchConditionsList = NULL, FederalEEO4FunctionID = F, Code = F, Description = F, CodeDescription = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, SkywardID = F, SkywardHash = F, ValidYearLow = F, ValidYearHigh = F, IsInvalid = F, searchConditionsGroupType = "And", searchSortFieldNamesList = NULL, searchSortFieldNamesDescendingList = NULL, entityID = 1, schoolYearID = NULL, page = 1, pageSize = 100000, flatten = T, ...){

		params <- as.list(environment()) %>% append(list(...))

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- "all", searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		listSkyObjects(module = "StateReportingFederal", objectName = "FederalEEO4Function", schoolYearID = schoolYearID, searchFields = searchFields, page = page, pageSize = pageSize, SearchConditionsList = searchConditionsList, SearchConditionsGroupType = searchConditionsGroupType, SearchSortFieldNamesList = searchSortFieldNamesList, SearchSortFieldNamesDescendingList = searchSortFieldNamesDescendingList, entityId = entityID, flatten = flatten)
	}


	listFederalEEOCJobCategories <- function(searchConditionsList = NULL, FederalEEOCJobCategoryID = F, Code = F, Description = F, CodeDescription = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, SkywardID = F, SkywardHash = F, ValidYearLow = F, ValidYearHigh = F, IsInvalid = F, EEOType = F, searchConditionsGroupType = "And", searchSortFieldNamesList = NULL, searchSortFieldNamesDescendingList = NULL, entityID = 1, schoolYearID = NULL, page = 1, pageSize = 100000, flatten = T, ...){

		params <- as.list(environment()) %>% append(list(...))

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- "all", searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		listSkyObjects(module = "StateReportingFederal", objectName = "FederalEEOCJobCategory", schoolYearID = schoolYearID, searchFields = searchFields, page = page, pageSize = pageSize, SearchConditionsList = searchConditionsList, SearchConditionsGroupType = searchConditionsGroupType, SearchSortFieldNamesList = searchSortFieldNamesList, SearchSortFieldNamesDescendingList = searchSortFieldNamesDescendingList, entityId = entityID, flatten = flatten)
	}


	listEEOCEmployeeV1s <- function(searchConditionsList = NULL, EEOCEmployeeV1ID = F, EmployeeID = F, EEOCExtractRunID = F, FirstNameLegal = F, MiddleNameLegal = F, LastNameLegal = F, Gender = F, IsHispanicOrLatino = F, IsAmericanIndianOrAlaskanNative = F, IsAsian = F, IsNativeHawaiianOrPacificIslander = F, IsBlackOrAfricanAmerican = F, IsWhite = F, IsMultipleRaces = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, searchConditionsGroupType = "And", searchSortFieldNamesList = NULL, searchSortFieldNamesDescendingList = NULL, entityID = 1, schoolYearID = NULL, page = 1, pageSize = 100000, flatten = T, ...){

		params <- as.list(environment()) %>% append(list(...))

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- "all", searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		listSkyObjects(module = "StateReportingFederal", objectName = "EEOCEmployeeV1", schoolYearID = schoolYearID, searchFields = searchFields, page = page, pageSize = pageSize, SearchConditionsList = searchConditionsList, SearchConditionsGroupType = searchConditionsGroupType, SearchSortFieldNamesList = searchSortFieldNamesList, SearchSortFieldNamesDescendingList = searchSortFieldNamesDescendingList, entityId = entityID, flatten = flatten)
	}


	listEEOCEmploymentV1s <- function(searchConditionsList = NULL, EEOCEmploymentV1ID = F, EEOCEmployeeV1ID = F, AnnualSalary = F, IsFullTime = F, IsNewHire = F, FederalEEOCJobCategoryID = F, FederalEEOCJobCategoryValue = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, PositionTypeDescription = F, searchConditionsGroupType = "And", searchSortFieldNamesList = NULL, searchSortFieldNamesDescendingList = NULL, entityID = 1, schoolYearID = NULL, page = 1, pageSize = 100000, flatten = T, ...){

		params <- as.list(environment()) %>% append(list(...))

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- "all", searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		listSkyObjects(module = "StateReportingFederal", objectName = "EEOCEmploymentV1", schoolYearID = schoolYearID, searchFields = searchFields, page = page, pageSize = pageSize, SearchConditionsList = searchConditionsList, SearchConditionsGroupType = searchConditionsGroupType, SearchSortFieldNamesList = searchSortFieldNamesList, SearchSortFieldNamesDescendingList = searchSortFieldNamesDescendingList, entityId = entityID, flatten = flatten)
	}


	listEEOCExtractRunIncludedPayrollRuns <- function(searchConditionsList = NULL, EEOCExtractRunIncludedPayrollRunID = F, EEOCExtractRunID = F, PayrollRunID = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, searchConditionsGroupType = "And", searchSortFieldNamesList = NULL, searchSortFieldNamesDescendingList = NULL, entityID = 1, schoolYearID = NULL, page = 1, pageSize = 100000, flatten = T, ...){

		params <- as.list(environment()) %>% append(list(...))

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- "all", searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		listSkyObjects(module = "StateReportingFederal", objectName = "EEOCExtractRunIncludedPayrollRun", schoolYearID = schoolYearID, searchFields = searchFields, page = page, pageSize = pageSize, SearchConditionsList = searchConditionsList, SearchConditionsGroupType = searchConditionsGroupType, SearchSortFieldNamesList = searchSortFieldNamesList, SearchSortFieldNamesDescendingList = searchSortFieldNamesDescendingList, entityId = entityID, flatten = flatten)
	}


	listEEOCExtractRuns <- function(searchConditionsList = NULL, EEOCExtractRunID = F, FiscalYearID = F, EmployerID = F, OfficeOfSchoolNumber = F, ControlNumber = F, FederalEEO4FunctionID = F, FederalEEO4FunctionValue = F, JurisdictionName = F, SchoolDistrictName = F, TitleOfCertifyingOfficial = F, NameOfCertifyingOfficial = F, TelephoneNumber = F, EmailAddress = F, AddressID = F, Street = F, POBox = F, City = F, County = F, State = F, ZipCode = F, TotalOfSchools = F, NumberOfAnnexes = F, Enrollment = F, AuthorityWithheld = F, Type = F, Version = F, FatalExceptionCount = F, NonFatalExceptionCount = F, LatestExport = F, LatestSentOrAcceptedExportTime = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, ExtractedEmployeeCount = F, searchConditionsGroupType = "And", searchSortFieldNamesList = NULL, searchSortFieldNamesDescendingList = NULL, entityID = 1, schoolYearID = NULL, page = 1, pageSize = 100000, flatten = T, ...){

		params <- as.list(environment()) %>% append(list(...))

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- "all", searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		listSkyObjects(module = "StateReportingFederal", objectName = "EEOCExtractRun", schoolYearID = schoolYearID, searchFields = searchFields, page = page, pageSize = pageSize, SearchConditionsList = searchConditionsList, SearchConditionsGroupType = searchConditionsGroupType, SearchSortFieldNamesList = searchSortFieldNamesList, SearchSortFieldNamesDescendingList = searchSortFieldNamesDescendingList, entityId = entityID, flatten = flatten)
	}


	listEEOCExceptions <- function(searchConditionsList = NULL, EEOCExceptionID = F, EEOCExtractRunID = F, EEOCEmploymentV1ID = F, Source = F, IsFatal = F, Type = F, Message = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, EEOCEmployeeV1ID = F, PreserveException = F, searchConditionsGroupType = "And", searchSortFieldNamesList = NULL, searchSortFieldNamesDescendingList = NULL, entityID = 1, schoolYearID = NULL, page = 1, pageSize = 100000, flatten = T, ...){

		params <- as.list(environment()) %>% append(list(...))

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- "all", searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		listSkyObjects(module = "StateReportingFederal", objectName = "EEOCException", schoolYearID = schoolYearID, searchFields = searchFields, page = page, pageSize = pageSize, SearchConditionsList = searchConditionsList, SearchConditionsGroupType = searchConditionsGroupType, SearchSortFieldNamesList = searchSortFieldNamesList, SearchSortFieldNamesDescendingList = searchSortFieldNamesDescendingList, entityId = entityID, flatten = flatten)
	}


	listEEOCExports <- function(searchConditionsList = NULL, EEOCExportID = F, EEOCExtractRunID = F, MediaID = F, Status = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, searchConditionsGroupType = "And", searchSortFieldNamesList = NULL, searchSortFieldNamesDescendingList = NULL, entityID = 1, schoolYearID = NULL, page = 1, pageSize = 100000, flatten = T, ...){

		params <- as.list(environment()) %>% append(list(...))

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- "all", searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		listSkyObjects(module = "StateReportingFederal", objectName = "EEOCExport", schoolYearID = schoolYearID, searchFields = searchFields, page = page, pageSize = pageSize, SearchConditionsList = searchConditionsList, SearchConditionsGroupType = searchConditionsGroupType, SearchSortFieldNamesList = searchSortFieldNamesList, SearchSortFieldNamesDescendingList = searchSortFieldNamesDescendingList, entityId = entityID, flatten = flatten)
	}


	listVCRSubmissionRunHistories <- function(searchConditionsList = NULL, VCRSubmissionRunHistoryID = F, VCRSubmissionID = F, StartDateTime = F, EndDateTime = F, Type = F, MediaID = F, IsExport = F, IsLocked = F, Status = F, RunData = F, UserIDCanceledBy = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, ScopeAccessAllowed = F, HasValidMedia = F, CanAccessMedia = F, RunParameters = F, searchConditionsGroupType = "And", searchSortFieldNamesList = NULL, searchSortFieldNamesDescendingList = NULL, entityID = 1, schoolYearID = NULL, page = 1, pageSize = 100000, flatten = T, ...){

		params <- as.list(environment()) %>% append(list(...))

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- "all", searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		listSkyObjects(module = "StateReportingFederal", objectName = "VCRSubmissionRunHistory", schoolYearID = schoolYearID, searchFields = searchFields, page = page, pageSize = pageSize, SearchConditionsList = searchConditionsList, SearchConditionsGroupType = searchConditionsGroupType, SearchSortFieldNamesList = searchSortFieldNamesList, SearchSortFieldNamesDescendingList = searchSortFieldNamesDescendingList, entityId = entityID, flatten = flatten)
	}


	listVCRSubmissions <- function(searchConditionsList = NULL, VCRSubmissionID = F, SchoolYearID = F, DistrictID = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, ApplicationFreeCategoricalCount = F, StudentFreeCategoricalCount = F, ApplicationFreeCount = F, StudentFreeCount = F, ApplicationReducedCount = F, StudentReducedCount = F, TotalStudentFreeCount = F, StudentSNAPCertifiedCount = F, StudentOtherCertifiedCount = F, StudentSNAPLetterCertifiedCount = F, TotalErrorProneApplicationCount = F, ApplicationSelectedCount = F, DirectVerificationApplicationCount = F, DirectVerificationStudentCount = F, CauseVerifiedApplicationCount = F, CategoricalNoChangeApplicationCount = F, CategoricalNoChangeStudentCount = F, CategoricalReducedApplicationCount = F, CategoricalReducedStudentCount = F, CategoricalPaidApplicationCount = F, CategoricalPaidStudentCount = F, CategoricalNoResponseApplicationCount = F, CategoricalNoResponseStudentCount = F, IncomeNoChangeApplicationCount = F, IncomeNoChangeStudentCount = F, IncomeReducedApplicationCount = F, IncomeReducedStudentCount = F, IncomePaidApplicationCount = F, IncomePaidStudentCount = F, IncomeNoResponseApplicationCount = F, IncomeNoResponseStudentCount = F, ReducedNoChangeApplicationCount = F, ReducedNoChangeStudentCount = F, ReducedFreeApplicationCount = F, ReducedFreeStudentCount = F, ReducedPaidApplicationCount = F, ReducedPaidStudentCount = F, ReducedNoResponseApplicationCount = F, ReducedNoResponseStudentCount = F, StudentCertifiedFreeMedicaidCount = F, StudentCertifiedReducedMedicaidCount = F, TotalStudentReducedCount = F, searchConditionsGroupType = "And", searchSortFieldNamesList = NULL, searchSortFieldNamesDescendingList = NULL, entityID = 1, schoolYearID = NULL, page = 1, pageSize = 100000, flatten = T, ...){

		params <- as.list(environment()) %>% append(list(...))

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- "all", searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		listSkyObjects(module = "StateReportingFederal", objectName = "VCRSubmission", schoolYearID = schoolYearID, searchFields = searchFields, page = page, pageSize = pageSize, SearchConditionsList = searchConditionsList, SearchConditionsGroupType = searchConditionsGroupType, SearchSortFieldNamesList = searchSortFieldNamesList, SearchSortFieldNamesDescendingList = searchSortFieldNamesDescendingList, entityId = entityID, flatten = flatten)
	}


	listVCRSection4Exceptions <- function(searchConditionsList = NULL, VCRSection4ExceptionID = F, VCRSection4V1ID = F, VCRSubmissionID = F, VCRSubmissionRunHistoryID = F, DistrictID = F, SchoolYearID = F, MessageType = F, Message = F, ProcessType = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, searchConditionsGroupType = "And", searchSortFieldNamesList = NULL, searchSortFieldNamesDescendingList = NULL, entityID = 1, schoolYearID = NULL, page = 1, pageSize = 100000, flatten = T, ...){

		params <- as.list(environment()) %>% append(list(...))

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- "all", searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		listSkyObjects(module = "StateReportingFederal", objectName = "VCRSection4Exception", schoolYearID = schoolYearID, searchFields = searchFields, page = page, pageSize = pageSize, SearchConditionsList = searchConditionsList, SearchConditionsGroupType = searchConditionsGroupType, SearchSortFieldNamesList = searchSortFieldNamesList, SearchSortFieldNamesDescendingList = searchSortFieldNamesDescendingList, entityId = entityID, flatten = flatten)
	}


	listVCRSection3Exceptions <- function(searchConditionsList = NULL, VCRSection3ExceptionID = F, VCRSection3V1ID = F, VCRSubmissionID = F, VCRSubmissionRunHistoryID = F, DistrictID = F, SchoolYearID = F, MessageType = F, Message = F, ProcessType = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, searchConditionsGroupType = "And", searchSortFieldNamesList = NULL, searchSortFieldNamesDescendingList = NULL, entityID = 1, schoolYearID = NULL, page = 1, pageSize = 100000, flatten = T, ...){

		params <- as.list(environment()) %>% append(list(...))

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- "all", searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		listSkyObjects(module = "StateReportingFederal", objectName = "VCRSection3Exception", schoolYearID = schoolYearID, searchFields = searchFields, page = page, pageSize = pageSize, SearchConditionsList = searchConditionsList, SearchConditionsGroupType = searchConditionsGroupType, SearchSortFieldNamesList = searchSortFieldNamesList, SearchSortFieldNamesDescendingList = searchSortFieldNamesDescendingList, entityId = entityID, flatten = flatten)
	}


	listVCRSection4V1s <- function(searchConditionsList = NULL, VCRSection4V1ID = F, ApplicationCategoricalFreeCount = F, ApplicationFreeCount = F, ApplicationReducedCount = F, StudentCategoricalFreeCount = F, StudentFreeCount = F, StudentReducedCount = F, TotalFreeCount = F, TotalReducedCount = F, VCRSubmissionID = F, VCRSubmissionRunHistoryID = F, DistrictID = F, SchoolYearID = F, HasErrors = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, KeyHash = F, UpdateHash = F, searchConditionsGroupType = "And", searchSortFieldNamesList = NULL, searchSortFieldNamesDescendingList = NULL, entityID = 1, schoolYearID = NULL, page = 1, pageSize = 100000, flatten = T, ...){

		params <- as.list(environment()) %>% append(list(...))

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- "all", searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		listSkyObjects(module = "StateReportingFederal", objectName = "VCRSection4V1", schoolYearID = schoolYearID, searchFields = searchFields, page = page, pageSize = pageSize, SearchConditionsList = searchConditionsList, SearchConditionsGroupType = searchConditionsGroupType, SearchSortFieldNamesList = searchSortFieldNamesList, SearchSortFieldNamesDescendingList = searchSortFieldNamesDescendingList, entityId = entityID, flatten = flatten)
	}


	listVCRSection3V1s <- function(searchConditionsList = NULL, VCRSection3V1ID = F, SNAPCertifiedCount = F, OtherCertifiedCount = F, SNAPLetterCertifiedCount = F, VCRSubmissionID = F, VCRSubmissionRunHistoryID = F, DistrictID = F, SchoolYearID = F, HasErrors = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, KeyHash = F, UpdateHash = F, CertifiedFreeMedicaidCount = F, CertifiedReducedMedicaidCount = F, searchConditionsGroupType = "And", searchSortFieldNamesList = NULL, searchSortFieldNamesDescendingList = NULL, entityID = 1, schoolYearID = NULL, page = 1, pageSize = 100000, flatten = T, ...){

		params <- as.list(environment()) %>% append(list(...))

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- "all", searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		listSkyObjects(module = "StateReportingFederal", objectName = "VCRSection3V1", schoolYearID = schoolYearID, searchFields = searchFields, page = page, pageSize = pageSize, SearchConditionsList = searchConditionsList, SearchConditionsGroupType = searchConditionsGroupType, SearchSortFieldNamesList = searchSortFieldNamesList, SearchSortFieldNamesDescendingList = searchSortFieldNamesDescendingList, entityId = entityID, flatten = flatten)
	}


	listACATransmissions <- function(searchConditionsList = NULL, ACATransmissionID = F, ACATransmissionIDOriginal = F, ACATransmissionIDPrevious = F, ACATransmissionIDReceiptProvider = F, DistrictID = F, CalendarYear = F, ReceiptIdentifier = F, IsTestTransmission = F, Type = F, Status = F, HasAnExport = F, IncludedACAEmployeeCount = F, TransmissionNumber = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, IsAvailableInEmployeeAccess = F, AllowEmployeeAccess = F, AllowEmployerEdit = F, AllowEmployerAddOrDelete = F, AllowEmployeeEdit = F, AllowEmployeeAddOrDelete = F, CalculationDay = F, EmploymentStartDate = F, IsFinalized = F, CalculatedTransmissionIdentifier = F, FederalACASoftwareIdentifierID = F, searchConditionsGroupType = "And", searchSortFieldNamesList = NULL, searchSortFieldNamesDescendingList = NULL, entityID = 1, schoolYearID = NULL, page = 1, pageSize = 100000, flatten = T, ...){

		params <- as.list(environment()) %>% append(list(...))

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- "all", searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		listSkyObjects(module = "StateReportingFederal", objectName = "ACATransmission", schoolYearID = schoolYearID, searchFields = searchFields, page = page, pageSize = pageSize, SearchConditionsList = searchConditionsList, SearchConditionsGroupType = searchConditionsGroupType, SearchSortFieldNamesList = searchSortFieldNamesList, SearchSortFieldNamesDescendingList = searchSortFieldNamesDescendingList, entityId = entityID, flatten = flatten)
	}


	listACASubmissionRunHistories <- function(searchConditionsList = NULL, ACASubmissionRunHistoryID = F, ACATransmissionID = F, StartDateTime = F, EndDateTime = F, Type = F, MediaID = F, IsExport = F, IsLocked = F, Status = F, RunData = F, UserIDCanceledBy = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, ScopeAccessAllowed = F, HasValidMedia = F, CanAccessMedia = F, ExportFormat = F, RunParameters = F, searchConditionsGroupType = "And", searchSortFieldNamesList = NULL, searchSortFieldNamesDescendingList = NULL, entityID = 1, schoolYearID = NULL, page = 1, pageSize = 100000, flatten = T, ...){

		params <- as.list(environment()) %>% append(list(...))

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- "all", searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		listSkyObjects(module = "StateReportingFederal", objectName = "ACASubmissionRunHistory", schoolYearID = schoolYearID, searchFields = searchFields, page = page, pageSize = pageSize, SearchConditionsList = searchConditionsList, SearchConditionsGroupType = searchConditionsGroupType, SearchSortFieldNamesList = searchSortFieldNamesList, SearchSortFieldNamesDescendingList = searchSortFieldNamesDescendingList, entityId = entityID, flatten = flatten)
	}


	listACACoveredIndividualExceptions <- function(searchConditionsList = NULL, ACACoveredIndividualExceptionID = F, ACACoveredIndividualV2ID = F, DistrictID = F, EmployeeID = F, ACATransmissionID = F, ACASubmissionRunHistoryID = F, MessageType = F, RuleNumber = F, Message = F, ProcessType = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, NameID = F, searchConditionsGroupType = "And", searchSortFieldNamesList = NULL, searchSortFieldNamesDescendingList = NULL, entityID = 1, schoolYearID = NULL, page = 1, pageSize = 100000, flatten = T, ...){

		params <- as.list(environment()) %>% append(list(...))

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- "all", searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		listSkyObjects(module = "StateReportingFederal", objectName = "ACACoveredIndividualException", schoolYearID = schoolYearID, searchFields = searchFields, page = page, pageSize = pageSize, SearchConditionsList = searchConditionsList, SearchConditionsGroupType = searchConditionsGroupType, SearchSortFieldNamesList = searchSortFieldNamesList, SearchSortFieldNamesDescendingList = searchSortFieldNamesDescendingList, entityId = entityID, flatten = flatten)
	}


	listACAEmployeeExceptions <- function(searchConditionsList = NULL, ACAEmployeeExceptionID = F, ACAEmployeeV2ID = F, DistrictID = F, EmployeeID = F, ACATransmissionID = F, ACASubmissionRunHistoryID = F, MessageType = F, RuleNumber = F, Message = F, ProcessType = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, searchConditionsGroupType = "And", searchSortFieldNamesList = NULL, searchSortFieldNamesDescendingList = NULL, entityID = 1, schoolYearID = NULL, page = 1, pageSize = 100000, flatten = T, ...){

		params <- as.list(environment()) %>% append(list(...))

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- "all", searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		listSkyObjects(module = "StateReportingFederal", objectName = "ACAEmployeeException", schoolYearID = schoolYearID, searchFields = searchFields, page = page, pageSize = pageSize, SearchConditionsList = searchConditionsList, SearchConditionsGroupType = searchConditionsGroupType, SearchSortFieldNamesList = searchSortFieldNamesList, SearchSortFieldNamesDescendingList = searchSortFieldNamesDescendingList, entityId = entityID, flatten = flatten)
	}


	listACAEmployerMonthExceptions <- function(searchConditionsList = NULL, ACAEmployerMonthExceptionID = F, ACAEmployerMonthV2ID = F, DistrictID = F, EmployerID = F, ACAMonth = F, ACATransmissionID = F, ACASubmissionRunHistoryID = F, MessageType = F, RuleNumber = F, Message = F, ProcessType = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, searchConditionsGroupType = "And", searchSortFieldNamesList = NULL, searchSortFieldNamesDescendingList = NULL, entityID = 1, schoolYearID = NULL, page = 1, pageSize = 100000, flatten = T, ...){

		params <- as.list(environment()) %>% append(list(...))

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- "all", searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		listSkyObjects(module = "StateReportingFederal", objectName = "ACAEmployerMonthException", schoolYearID = schoolYearID, searchFields = searchFields, page = page, pageSize = pageSize, SearchConditionsList = searchConditionsList, SearchConditionsGroupType = searchConditionsGroupType, SearchSortFieldNamesList = searchSortFieldNamesList, SearchSortFieldNamesDescendingList = searchSortFieldNamesDescendingList, entityId = entityID, flatten = flatten)
	}


	listACAEmployerExceptions <- function(searchConditionsList = NULL, ACAEmployerExceptionID = F, ACAEmployerV2ID = F, DistrictID = F, EmployerID = F, ACATransmissionID = F, ACASubmissionRunHistoryID = F, MessageType = F, RuleNumber = F, Message = F, ProcessType = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, searchConditionsGroupType = "And", searchSortFieldNamesList = NULL, searchSortFieldNamesDescendingList = NULL, entityID = 1, schoolYearID = NULL, page = 1, pageSize = 100000, flatten = T, ...){

		params <- as.list(environment()) %>% append(list(...))

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- "all", searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		listSkyObjects(module = "StateReportingFederal", objectName = "ACAEmployerException", schoolYearID = schoolYearID, searchFields = searchFields, page = page, pageSize = pageSize, SearchConditionsList = searchConditionsList, SearchConditionsGroupType = searchConditionsGroupType, SearchSortFieldNamesList = searchSortFieldNamesList, SearchSortFieldNamesDescendingList = searchSortFieldNamesDescendingList, entityId = entityID, flatten = flatten)
	}


	listACAIRSExceptions <- function(searchConditionsList = NULL, ACAIRSExceptionID = F, ACATransmissionID = F, ACASubmissionRunHistoryID = F, MessageType = F, RuleNumber = F, Message = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, searchConditionsGroupType = "And", searchSortFieldNamesList = NULL, searchSortFieldNamesDescendingList = NULL, entityID = 1, schoolYearID = NULL, page = 1, pageSize = 100000, flatten = T, ...){

		params <- as.list(environment()) %>% append(list(...))

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- "all", searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		listSkyObjects(module = "StateReportingFederal", objectName = "ACAIRSException", schoolYearID = schoolYearID, searchFields = searchFields, page = page, pageSize = pageSize, SearchConditionsList = searchConditionsList, SearchConditionsGroupType = searchConditionsGroupType, SearchSortFieldNamesList = searchSortFieldNamesList, SearchSortFieldNamesDescendingList = searchSortFieldNamesDescendingList, entityId = entityID, flatten = flatten)
	}


	listACAEmployeeMonthExceptions <- function(searchConditionsList = NULL, ACAEmployeeMonthExceptionID = F, ACAEmployeeMonthV2ID = F, DistrictID = F, EmployeeID = F, ACAMonth = F, ACATransmissionID = F, ACASubmissionRunHistoryID = F, MessageType = F, RuleNumber = F, Message = F, ProcessType = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, searchConditionsGroupType = "And", searchSortFieldNamesList = NULL, searchSortFieldNamesDescendingList = NULL, entityID = 1, schoolYearID = NULL, page = 1, pageSize = 100000, flatten = T, ...){

		params <- as.list(environment()) %>% append(list(...))

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- "all", searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		listSkyObjects(module = "StateReportingFederal", objectName = "ACAEmployeeMonthException", schoolYearID = schoolYearID, searchFields = searchFields, page = page, pageSize = pageSize, SearchConditionsList = searchConditionsList, SearchConditionsGroupType = searchConditionsGroupType, SearchSortFieldNamesList = searchSortFieldNamesList, SearchSortFieldNamesDescendingList = searchSortFieldNamesDescendingList, entityId = entityID, flatten = flatten)
	}


	listACAEmployerMonthV2s <- function(searchConditionsList = NULL, ACAEmployerMonthV2ID = F, ACATransmissionID = F, ACASubmissionRunHistoryID = F, DistrictID = F, EmployerID = F, ACAMonth = F, OffersMinimumEssentialCoverage = F, TotalEmployeeCount = F, IsAggregatedGroup = F, UniqueExportIdentifierParent = F, UniqueExportIdentifierSelf = F, FullTimeEmployeeCount = F, HasErrors = F, HasSkywardFatalError = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, KeyHash = F, UpdateHash = F, searchConditionsGroupType = "And", searchSortFieldNamesList = NULL, searchSortFieldNamesDescendingList = NULL, entityID = 1, schoolYearID = NULL, page = 1, pageSize = 100000, flatten = T, ...){

		params <- as.list(environment()) %>% append(list(...))

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- "all", searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		listSkyObjects(module = "StateReportingFederal", objectName = "ACAEmployerMonthV2", schoolYearID = schoolYearID, searchFields = searchFields, page = page, pageSize = pageSize, SearchConditionsList = searchConditionsList, SearchConditionsGroupType = searchConditionsGroupType, SearchSortFieldNamesList = searchSortFieldNamesList, SearchSortFieldNamesDescendingList = searchSortFieldNamesDescendingList, entityId = entityID, flatten = flatten)
	}


	listACAEmployeeV2s <- function(searchConditionsList = NULL, ACAEmployeeV2ID = F, ACATransmissionID = F, ACATransmissionIDReceiptProvider = F, ACASubmissionRunHistoryID = F, DistrictID = F, EmployeeID = F, AcceptedByIRS = F, SocialSecurityNumber = F, FirstNameElectronicFile = F, MiddleNameElectronicFile = F, LastNameElectronicFile = F, SuffixElectronicFile = F, NamePrinted = F, AddressLine1ElectronicFile = F, AddressLine1Printed = F, AddressLine2ElectronicFile = F, AddressLine2Printed = F, CityElectronicFile = F, CityPrinted = F, ZipCode = F, ZipCodeExtension = F, EmployerProvidedSelfInsuredCoverage = F, EmployeeAccessPostedTime = F, UniqueExportIdentifierParent = F, UniqueExportIdentifierSelf = F, HasErrors = F, HasSkywardFatalError = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, PlanStartMonth = F, FullNameLFMS = F, MediaIDFullSSN = F, XMLData = F, IsAcceptedAndLatestInTransmissionSet = F, IsAvailableInEmployeeAccessAndLatest = F, KeyHash = F, UpdateHash = F, EmployerPrinted1095 = F, StateIDOrProvince = F, StateOrProvinceCode = F, StateOrProvinceName = F, AddressType = F, PostalCode = F, W2CountryID = F, W2CountryCode = F, CountryName = F, IsActive = F, searchConditionsGroupType = "And", searchSortFieldNamesList = NULL, searchSortFieldNamesDescendingList = NULL, entityID = 1, schoolYearID = NULL, page = 1, pageSize = 100000, flatten = T, ...){

		params <- as.list(environment()) %>% append(list(...))

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- "all", searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		listSkyObjects(module = "StateReportingFederal", objectName = "ACAEmployeeV2", schoolYearID = schoolYearID, searchFields = searchFields, page = page, pageSize = pageSize, SearchConditionsList = searchConditionsList, SearchConditionsGroupType = searchConditionsGroupType, SearchSortFieldNamesList = searchSortFieldNamesList, SearchSortFieldNamesDescendingList = searchSortFieldNamesDescendingList, entityId = entityID, flatten = flatten)
	}


	listACAEmployerV2s <- function(searchConditionsList = NULL, ACAEmployerV2ID = F, ACATransmissionID = F, ACATransmissionIDReceiptProvider = F, ACASubmissionRunHistoryID = F, DistrictID = F, EmployerID = F, IsMainEmployer = F, AcceptedByIRS = F, EmployerIdentificationNumber = F, NameLine1ElectronicFile = F, NameLine2ElectronicFile = F, NamePrinted = F, AddressLine1ElectronicFile = F, AddressLine1Printed = F, AddressLine2ElectronicFile = F, AddressLine2Printed = F, CityElectronicFile = F, CityPrinted = F, ZipCode = F, ZipCodeExtension = F, ContactPhoneNumber = F, ContactPersonFirstName = F, ContactPersonMiddleName = F, ContactPersonLastName = F, ContactPersonSuffix = F, GovernmentEntityEmployerIdentificationNumber = F, GovernmentEntityNameLine1 = F, GovernmentEntityNameLine2 = F, GovernmentEntityAddressLine1 = F, GovernmentEntityAddressLine2 = F, GovernmentEntityCity = F, GovernmentEntityStateCode = F, GovernmentEntityZipCode = F, GovernmentEntityZipCodeExtension = F, GovernmentEntityContactPhoneNumber = F, GovernmentEntityContactPersonFirstName = F, GovernmentEntityContactPersonMiddleName = F, GovernmentEntityContactPersonLastName = F, GovernmentEntityContactPersonSuffix = F, IsAuthoritativeTransmittal = F, TotalForms1095CFiled = F, IsMemberOfAggregatedALEGroup = F, IsQualifyingOfferMethod = F, Is98PercentOfferMethod = F, TransmitterControlCode = F, UniqueExportIdentifierSelf = F, HasErrors = F, HasSkywardFatalError = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, IsLatestInOriginalTransmissionSet = F, KeyHash = F, UpdateHash = F, StateIDOrProvince = F, StateOrProvinceCode = F, StateOrProvinceName = F, AddressType = F, PostalCode = F, W2CountryID = F, W2CountryCode = F, CountryName = F, searchConditionsGroupType = "And", searchSortFieldNamesList = NULL, searchSortFieldNamesDescendingList = NULL, entityID = 1, schoolYearID = NULL, page = 1, pageSize = 100000, flatten = T, ...){

		params <- as.list(environment()) %>% append(list(...))

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- "all", searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		listSkyObjects(module = "StateReportingFederal", objectName = "ACAEmployerV2", schoolYearID = schoolYearID, searchFields = searchFields, page = page, pageSize = pageSize, SearchConditionsList = searchConditionsList, SearchConditionsGroupType = searchConditionsGroupType, SearchSortFieldNamesList = searchSortFieldNamesList, SearchSortFieldNamesDescendingList = searchSortFieldNamesDescendingList, entityId = entityID, flatten = flatten)
	}


	listACAEmployeeMonthV2s <- function(searchConditionsList = NULL, ACAEmployeeMonthV2ID = F, ACATransmissionID = F, ACASubmissionRunHistoryID = F, DistrictID = F, EmployeeID = F, ACAMonth = F, FederalACAOfferOfCoverageID = F, FederalACAOfferOfCoverageCode = F, ACAEmployeeRequiredContribution = F, FederalACASafeHarborID = F, FederalACASafeHarborCode = F, UniqueExportIdentifierParent = F, UniqueExportIdentifierSelf = F, HasErrors = F, HasSkywardFatalError = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, KeyHash = F, UpdateHash = F, searchConditionsGroupType = "And", searchSortFieldNamesList = NULL, searchSortFieldNamesDescendingList = NULL, entityID = 1, schoolYearID = NULL, page = 1, pageSize = 100000, flatten = T, ...){

		params <- as.list(environment()) %>% append(list(...))

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- "all", searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		listSkyObjects(module = "StateReportingFederal", objectName = "ACAEmployeeMonthV2", schoolYearID = schoolYearID, searchFields = searchFields, page = page, pageSize = pageSize, SearchConditionsList = searchConditionsList, SearchConditionsGroupType = searchConditionsGroupType, SearchSortFieldNamesList = searchSortFieldNamesList, SearchSortFieldNamesDescendingList = searchSortFieldNamesDescendingList, entityId = entityID, flatten = flatten)
	}


	listACACoveredIndividualV2s <- function(searchConditionsList = NULL, ACACoveredIndividualV2ID = F, ACATransmissionID = F, ACASubmissionRunHistoryID = F, DistrictID = F, EmployeeID = F, SocialSecurityNumber = F, FirstNameElectronicFile = F, MiddleNameElectronicFile = F, LastNameElectronicFile = F, SuffixElectronicFile = F, NamePrinted = F, BirthDate = F, IsCoveredAll12Months = F, IsCoveredJanuary = F, IsCoveredFebruary = F, IsCoveredMarch = F, IsCoveredApril = F, IsCoveredMay = F, IsCoveredJune = F, IsCoveredJuly = F, IsCoveredAugust = F, IsCoveredSeptember = F, IsCoveredOctober = F, IsCoveredNovember = F, IsCoveredDecember = F, UniqueExportIdentifierParent = F, UniqueExportIdentifierSelf = F, HasErrors = F, HasSkywardFatalError = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, FullNameLFMS = F, NameID = F, KeyHash = F, UpdateHash = F, searchConditionsGroupType = "And", searchSortFieldNamesList = NULL, searchSortFieldNamesDescendingList = NULL, entityID = 1, schoolYearID = NULL, page = 1, pageSize = 100000, flatten = T, ...){

		params <- as.list(environment()) %>% append(list(...))

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- "all", searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		listSkyObjects(module = "StateReportingFederal", objectName = "ACACoveredIndividualV2", schoolYearID = schoolYearID, searchFields = searchFields, page = page, pageSize = pageSize, SearchConditionsList = searchConditionsList, SearchConditionsGroupType = searchConditionsGroupType, SearchSortFieldNamesList = searchSortFieldNamesList, SearchSortFieldNamesDescendingList = searchSortFieldNamesDescendingList, entityId = entityID, flatten = flatten)
	}


	listVCRSection5Exceptions <- function(searchConditionsList = NULL, VCRSection5ExceptionID = F, VCRSection5V1ID = F, VCRSubmissionID = F, VCRSubmissionRunHistoryID = F, DistrictID = F, SchoolYearID = F, MessageType = F, Message = F, ProcessType = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, searchConditionsGroupType = "And", searchSortFieldNamesList = NULL, searchSortFieldNamesDescendingList = NULL, entityID = 1, schoolYearID = NULL, page = 1, pageSize = 100000, flatten = T, ...){

		params <- as.list(environment()) %>% append(list(...))

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- "all", searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		listSkyObjects(module = "StateReportingFederal", objectName = "VCRSection5Exception", schoolYearID = schoolYearID, searchFields = searchFields, page = page, pageSize = pageSize, SearchConditionsList = searchConditionsList, SearchConditionsGroupType = searchConditionsGroupType, SearchSortFieldNamesList = searchSortFieldNamesList, SearchSortFieldNamesDescendingList = searchSortFieldNamesDescendingList, entityId = entityID, flatten = flatten)
	}


	listVCRSection5V1s <- function(searchConditionsList = NULL, VCRSection5V1ID = F, TotalErrorProneApplicationsCount = F, ApplicationsSelectedCount = F, DirectVerificationApplicationsCount = F, DirectVerificationStudentCount = F, CauseVerifiedApplicationCount = F, CategoricalNoChangeApplicationCount = F, CategoricalReducedApplicationCount = F, CategoricalPaidApplicationCount = F, CategoricalNoResponseApplicationCount = F, CategoricalNoChangeStudentCount = F, CategoricalReducedStudentCount = F, CategoricalPaidStudentCount = F, CategoricalNoResponseStudentCount = F, IncomeNoChangeApplicationCount = F, IncomeReducedApplicationCount = F, IncomePaidApplicationCount = F, IncomeNoResponseApplicationCount = F, IncomeNoChangeStudentCount = F, IncomeReducedStudentCount = F, IncomePaidStudentCount = F, IncomeNoResponseStudentCount = F, ReducedNoChangeApplicationCount = F, ReducedFreeApplicationCount = F, ReducedPaidApplicationCount = F, ReducedNoResponseApplicationCount = F, ReducedNoChangeStudentCount = F, ReducedFreeStudentCount = F, ReducedPaidStudentCount = F, ReducedNoResponseStudentCount = F, VCRSubmissionID = F, VCRSubmissionRunHistoryID = F, DistrictID = F, SchoolYearID = F, HasErrors = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, KeyHash = F, UpdateHash = F, searchConditionsGroupType = "And", searchSortFieldNamesList = NULL, searchSortFieldNamesDescendingList = NULL, entityID = 1, schoolYearID = NULL, page = 1, pageSize = 100000, flatten = T, ...){

		params <- as.list(environment()) %>% append(list(...))

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- "all", searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		listSkyObjects(module = "StateReportingFederal", objectName = "VCRSection5V1", schoolYearID = schoolYearID, searchFields = searchFields, page = page, pageSize = pageSize, SearchConditionsList = searchConditionsList, SearchConditionsGroupType = searchConditionsGroupType, SearchSortFieldNamesList = searchSortFieldNamesList, SearchSortFieldNamesDescendingList = searchSortFieldNamesDescendingList, entityId = entityID, flatten = flatten)
	}


	listVCRApplicationExceptions <- function(searchConditionsList = NULL, VCRApplicationExceptionID = F, VCRApplicationV1ID = F, NameIDPayor = F, VCRSubmissionID = F, VCRSubmissionRunHistoryID = F, DistrictID = F, SchoolYearID = F, MessageType = F, Message = F, ProcessType = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, ApplicationID = F, NameIDGuardian = F, searchConditionsGroupType = "And", searchSortFieldNamesList = NULL, searchSortFieldNamesDescendingList = NULL, entityID = 1, schoolYearID = NULL, page = 1, pageSize = 100000, flatten = T, ...){

		params <- as.list(environment()) %>% append(list(...))

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- "all", searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		listSkyObjects(module = "StateReportingFederal", objectName = "VCRApplicationException", schoolYearID = schoolYearID, searchFields = searchFields, page = page, pageSize = pageSize, SearchConditionsList = searchConditionsList, SearchConditionsGroupType = searchConditionsGroupType, SearchSortFieldNamesList = searchSortFieldNamesList, SearchSortFieldNamesDescendingList = searchSortFieldNamesDescendingList, entityId = entityID, flatten = flatten)
	}


	listVCRStudentExceptions <- function(searchConditionsList = NULL, VCRStudentExceptionID = F, VCRStudentV1ID = F, NameIDStudent = F, VCRSubmissionID = F, VCRSubmissionRunHistoryID = F, DistrictID = F, SchoolYearID = F, MessageType = F, Message = F, ProcessType = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, CustomerCategoryID = F, NameIDGuardian = F, searchConditionsGroupType = "And", searchSortFieldNamesList = NULL, searchSortFieldNamesDescendingList = NULL, entityID = 1, schoolYearID = NULL, page = 1, pageSize = 100000, flatten = T, ...){

		params <- as.list(environment()) %>% append(list(...))

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- "all", searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		listSkyObjects(module = "StateReportingFederal", objectName = "VCRStudentException", schoolYearID = schoolYearID, searchFields = searchFields, page = page, pageSize = pageSize, SearchConditionsList = searchConditionsList, SearchConditionsGroupType = searchConditionsGroupType, SearchSortFieldNamesList = searchSortFieldNamesList, SearchSortFieldNamesDescendingList = searchSortFieldNamesDescendingList, entityId = entityID, flatten = flatten)
	}


	listVCRApplicationV1s <- function(searchConditionsList = NULL, VCRApplicationV1ID = F, ApplicationID = F, NameIDPayor = F, NameIDGuardian = F, IsCategoricalFree = F, IsFree = F, IsReduced = F, IsErrorProne = F, IsSelected = F, IsDirectVerification = F, IsCauseVerified = F, IsCategoricalNoChange = F, IsCategoricalReduced = F, IsCategoricalPaid = F, IsCategoricalNoResponse = F, IsIncomeNoChange = F, IsIncomeReduced = F, IsIncomePaid = F, IsIncomeNoResponse = F, IsReducedNoChange = F, IsReducedFree = F, IsReducedPaid = F, IsReducedNoResponse = F, VCRSubmissionID = F, VCRSubmissionRunHistoryID = F, DistrictID = F, SchoolYearID = F, HasErrors = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, KeyHash = F, UpdateHash = F, searchConditionsGroupType = "And", searchSortFieldNamesList = NULL, searchSortFieldNamesDescendingList = NULL, entityID = 1, schoolYearID = NULL, page = 1, pageSize = 100000, flatten = T, ...){

		params <- as.list(environment()) %>% append(list(...))

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- "all", searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		listSkyObjects(module = "StateReportingFederal", objectName = "VCRApplicationV1", schoolYearID = schoolYearID, searchFields = searchFields, page = page, pageSize = pageSize, SearchConditionsList = searchConditionsList, SearchConditionsGroupType = searchConditionsGroupType, SearchSortFieldNamesList = searchSortFieldNamesList, SearchSortFieldNamesDescendingList = searchSortFieldNamesDescendingList, entityId = entityID, flatten = flatten)
	}


	listVCRStudentV1s <- function(searchConditionsList = NULL, VCRStudentV1ID = F, CustomerCategoryID = F, NameIDStudent = F, NameIDGuardian = F, Status = F, IsSNAPCertified = F, IsOtherCertified = F, IsSNAPLetterCertified = F, IsCategoricalFree = F, IsFree = F, IsReduced = F, IsDirectVerification = F, IsCategoricalNoChange = F, IsCategoricalReduced = F, IsCategoricalPaid = F, IsCategoricalNoResponse = F, IsIncomeNoChange = F, IsIncomeReduced = F, IsIncomePaid = F, IsIncomeNoResponse = F, IsReducedNoChange = F, IsReducedFree = F, IsReducedPaid = F, IsReducedNoResponse = F, VCRSubmissionID = F, VCRSubmissionRunHistoryID = F, DistrictID = F, SchoolYearID = F, HasErrors = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, AddressID = F, WithdrawalDate = F, KeyHash = F, UpdateHash = F, IsCertifiedFreeMedicaid = F, IsCertifiedReducedMedicaid = F, searchConditionsGroupType = "And", searchSortFieldNamesList = NULL, searchSortFieldNamesDescendingList = NULL, entityID = 1, schoolYearID = NULL, page = 1, pageSize = 100000, flatten = T, ...){

		params <- as.list(environment()) %>% append(list(...))

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- "all", searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		listSkyObjects(module = "StateReportingFederal", objectName = "VCRStudentV1", schoolYearID = schoolYearID, searchFields = searchFields, page = page, pageSize = pageSize, SearchConditionsList = searchConditionsList, SearchConditionsGroupType = searchConditionsGroupType, SearchSortFieldNamesList = searchSortFieldNamesList, SearchSortFieldNamesDescendingList = searchSortFieldNamesDescendingList, entityId = entityID, flatten = flatten)
	}


	listACAIndividualPDFS <- function(searchConditionsList = NULL, EmployeeID = F, DistrictID = F, MediaIDFullSSN = F, CalendarYear = F, EmployerName = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, ACAEmployeeV2ID = F, ACAYearEmployeeV1ID = F, searchConditionsGroupType = "And", searchSortFieldNamesList = NULL, searchSortFieldNamesDescendingList = NULL, entityID = 1, schoolYearID = NULL, page = 1, pageSize = 100000, flatten = T, ...){

		params <- as.list(environment()) %>% append(list(...))

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- "all", searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		listSkyObjects(module = "StateReportingFederal", objectName = "ACAIndividualPDF", schoolYearID = schoolYearID, searchFields = searchFields, page = page, pageSize = pageSize, SearchConditionsList = searchConditionsList, SearchConditionsGroupType = searchConditionsGroupType, SearchSortFieldNamesList = searchSortFieldNamesList, SearchSortFieldNamesDescendingList = searchSortFieldNamesDescendingList, entityId = entityID, flatten = flatten)
	}


	listACADependentReports <- function(searchConditionsList = NULL, DependentID = F, EmployeeID = F, DistrictID = F, SocialSecurityNumber = F, BirthDate = F, EmployeeName = F, ReportingYear = F, HasJanuaryCoverage = F, HasFebruaryCoverage = F, HasMarchCoverage = F, HasAprilCoverage = F, HasMayCoverage = F, HasJuneCoverage = F, HasJulyCoverage = F, HasAugustCoverage = F, HasSeptemberCoverage = F, HasOctoberCoverage = F, HasNovemberCoverage = F, HasDecemberCoverage = F, searchConditionsGroupType = "And", searchSortFieldNamesList = NULL, searchSortFieldNamesDescendingList = NULL, entityID = 1, schoolYearID = NULL, page = 1, pageSize = 100000, flatten = T, ...){

		params <- as.list(environment()) %>% append(list(...))

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- "all", searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		listSkyObjects(module = "StateReportingFederal", objectName = "ACADependentReport", schoolYearID = schoolYearID, searchFields = searchFields, page = page, pageSize = pageSize, SearchConditionsList = searchConditionsList, SearchConditionsGroupType = searchConditionsGroupType, SearchSortFieldNamesList = searchSortFieldNamesList, SearchSortFieldNamesDescendingList = searchSortFieldNamesDescendingList, entityId = entityID, flatten = flatten)
	}


	listEEOCSummaries <- function(searchConditionsList = NULL, EEOCExtractRunID = F, IsEEO4 = F, CategoryNumber = F, CategoryDescription = F, CategoryAnnualSalary = F, HispanicOrLatinoMale = F, HispanicOrLatinoFemale = F, WhiteMale = F, WhiteFemale = F, BlackOrAfricanAmericanMale = F, BlackOrAfricanAmericanFemale = F, AsianMale = F, AsianFemale = F, NativeHawaiianOrPacificIslanderMale = F, NativeHawaiianOrPacificIslanderFemale = F, AmericanIndianOrAlaskanNativeMale = F, AmericanIndianOrAlaskanNativeFemale = F, MultipleRacesMale = F, MultipleRacesFemale = F, TotalColumn = F, searchConditionsGroupType = "And", searchSortFieldNamesList = NULL, searchSortFieldNamesDescendingList = NULL, entityID = 1, schoolYearID = NULL, page = 1, pageSize = 100000, flatten = T, ...){

		params <- as.list(environment()) %>% append(list(...))

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- "all", searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		listSkyObjects(module = "StateReportingFederal", objectName = "EEOCSummary", schoolYearID = schoolYearID, searchFields = searchFields, page = page, pageSize = pageSize, SearchConditionsList = searchConditionsList, SearchConditionsGroupType = searchConditionsGroupType, SearchSortFieldNamesList = searchSortFieldNamesList, SearchSortFieldNamesDescendingList = searchSortFieldNamesDescendingList, entityId = entityID, flatten = flatten)
	}


	listACAEmployeeReports <- function(searchConditionsList = NULL, EmployeeID = F, DistrictID = F, FullNameLFM = F, EmployeeNumber = F, ReportingYear = F, ACAOfferOfCoverageCodeJanuary = F, ACASafeHarborCodeJanuary = F, ACAEmployeeRequiredContributionJanuary = F, ACAOfferOfCoverageCodeFebruary = F, ACASafeHarborCodeFebruary = F, ACAEmployeeRequiredContributionFebruary = F, ACAOfferOfCoverageCodeMarch = F, ACASafeHarborCodeMarch = F, ACAEmployeeRequiredContributionMarch = F, ACAOfferOfCoverageCodeApril = F, ACASafeHarborCodeApril = F, ACAEmployeeRequiredContributionApril = F, ACAOfferOfCoverageCodeMay = F, ACASafeHarborCodeMay = F, ACAEmployeeRequiredContributionMay = F, ACAOfferOfCoverageCodeJune = F, ACASafeHarborCodeJune = F, ACAEmployeeRequiredContributionJune = F, ACAOfferOfCoverageCodeJuly = F, ACASafeHarborCodeJuly = F, ACAEmployeeRequiredContributionJuly = F, ACAOfferOfCoverageCodeAugust = F, ACASafeHarborCodeAugust = F, ACAEmployeeRequiredContributionAugust = F, ACAOfferOfCoverageCodeSeptember = F, ACASafeHarborCodeSeptember = F, ACAEmployeeRequiredContributionSeptember = F, ACAOfferOfCoverageCodeOctober = F, ACASafeHarborCodeOctober = F, ACAEmployeeRequiredContributionOctober = F, ACAOfferOfCoverageCodeNovember = F, ACASafeHarborCodeNovember = F, ACAEmployeeRequiredContributionNovember = F, ACAOfferOfCoverageCodeDecember = F, ACASafeHarborCodeDecember = F, ACAEmployeeRequiredContributionDecember = F, searchConditionsGroupType = "And", searchSortFieldNamesList = NULL, searchSortFieldNamesDescendingList = NULL, entityID = 1, schoolYearID = NULL, page = 1, pageSize = 100000, flatten = T, ...){

		params <- as.list(environment()) %>% append(list(...))

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- "all", searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		listSkyObjects(module = "StateReportingFederal", objectName = "ACAEmployeeReport", schoolYearID = schoolYearID, searchFields = searchFields, page = page, pageSize = pageSize, SearchConditionsList = searchConditionsList, SearchConditionsGroupType = searchConditionsGroupType, SearchSortFieldNamesList = searchSortFieldNamesList, SearchSortFieldNamesDescendingList = searchSortFieldNamesDescendingList, entityId = entityID, flatten = flatten)
	}


	listFederalNCESSchools <- function(searchConditionsList = NULL, FederalNCESSchoolID = F, DistrictID = F, Code = F, Description = F, CodeDescription = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, searchConditionsGroupType = "And", searchSortFieldNamesList = NULL, searchSortFieldNamesDescendingList = NULL, entityID = 1, schoolYearID = NULL, page = 1, pageSize = 100000, flatten = T, ...){

		params <- as.list(environment()) %>% append(list(...))

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- "all", searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		listSkyObjects(module = "StateReportingFederal", objectName = "FederalNCESSchool", schoolYearID = schoolYearID, searchFields = searchFields, page = page, pageSize = pageSize, SearchConditionsList = searchConditionsList, SearchConditionsGroupType = searchConditionsGroupType, SearchSortFieldNamesList = searchSortFieldNamesList, SearchSortFieldNamesDescendingList = searchSortFieldNamesDescendingList, entityId = entityID, flatten = flatten)
	}


	listFederalCRDCStaffTypes <- function(searchConditionsList = NULL, FederalCRDCStaffTypeID = F, Code = F, Description = F, IsTotalPersonnel = F, CRDCStaffCategoryType = F, SkywardID = F, SkywardHash = F, ValidYearLow = F, ValidYearHigh = F, IsInvalid = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, searchConditionsGroupType = "And", searchSortFieldNamesList = NULL, searchSortFieldNamesDescendingList = NULL, entityID = 1, schoolYearID = NULL, page = 1, pageSize = 100000, flatten = T, ...){

		params <- as.list(environment()) %>% append(list(...))

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- "all", searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		listSkyObjects(module = "StateReportingFederal", objectName = "FederalCRDCStaffType", schoolYearID = schoolYearID, searchFields = searchFields, page = page, pageSize = pageSize, SearchConditionsList = searchConditionsList, SearchConditionsGroupType = searchConditionsGroupType, SearchSortFieldNamesList = searchSortFieldNamesList, SearchSortFieldNamesDescendingList = searchSortFieldNamesDescendingList, entityId = entityID, flatten = flatten)
	}


	listCRDCStaffDetails <- function(searchConditionsList = NULL, CRDCStaffDetailID = F, EmployeeID = F, CivilRightsDataCollectionID = F, CivilRightsDataCollectionRunHistoryID = F, DistrictID = F, FederalNCESSchoolID = F, FederalCRDCStaffTypeID = F, FullName = F, FTE = F, IsTeacherCertified = F, IsTeacherFirstYear = F, IsTeacherSecondYear = F, IsCurrentYear = F, IsPriorYear = F, FederalFundsSalary = F, StateLocalSalary = F, DaysAbsent = F, FederalCRDCStaffTypeCode = F, IsTotalPersonnel = F, CRDCStaffCategoryType = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, searchConditionsGroupType = "And", searchSortFieldNamesList = NULL, searchSortFieldNamesDescendingList = NULL, entityID = 1, schoolYearID = NULL, page = 1, pageSize = 100000, flatten = T, ...){

		params <- as.list(environment()) %>% append(list(...))

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- "all", searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		listSkyObjects(module = "StateReportingFederal", objectName = "CRDCStaffDetail", schoolYearID = schoolYearID, searchFields = searchFields, page = page, pageSize = pageSize, SearchConditionsList = searchConditionsList, SearchConditionsGroupType = searchConditionsGroupType, SearchSortFieldNamesList = searchSortFieldNamesList, SearchSortFieldNamesDescendingList = searchSortFieldNamesDescendingList, entityId = entityID, flatten = flatten)
	}


	listStateReportingFederalConfigDistricts <- function(searchConditionsList = NULL, ConfigDistrictID = F, DistrictID = F, FirstYearTeacherYearsExperienceLow = F, FirstYearTeacherYearsExperienceHigh = F, SecondYearTeacherYearsExperienceLow = F, SecondYearTeacherYearsExperienceHigh = F, CRDCFTEType = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, CreateLocalityFiles = F, searchConditionsGroupType = "And", searchSortFieldNamesList = NULL, searchSortFieldNamesDescendingList = NULL, entityID = 1, schoolYearID = NULL, page = 1, pageSize = 100000, flatten = T, ...){

		params <- as.list(environment()) %>% append(list(...))

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- "all", searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		listSkyObjects(module = "StateReportingFederal", objectName = "ConfigDistrict", schoolYearID = schoolYearID, searchFields = searchFields, page = page, pageSize = pageSize, SearchConditionsList = searchConditionsList, SearchConditionsGroupType = searchConditionsGroupType, SearchSortFieldNamesList = searchSortFieldNamesList, SearchSortFieldNamesDescendingList = searchSortFieldNamesDescendingList, entityId = entityID, flatten = flatten)
	}


	listCRDCSchoolAPComputerScienceV1s <- function(searchConditionsList = NULL, CRDCSchoolAPComputerScienceV1ID = F, HasStudents = F, MaleHispanicCount = F, MaleAlaskanCount = F, MaleAsianCount = F, MaleBlackCount = F, MaleHawaiianCount = F, MaleWhiteCount = F, MaleMultiRaceCount = F, MaleIDEACount = F, MaleLEPCount = F, FemaleHispanicCount = F, FemaleAlaskanCount = F, FemaleAsianCount = F, FemaleBlackCount = F, FemaleHawaiianCount = F, FemaleWhiteCount = F, FemaleMultiRaceCount = F, FemaleIDEACount = F, FemaleLEPCount = F, FederalNCESSchoolID = F, CivilRightsDataCollectionID = F, CivilRightsDataCollectionRunHistoryID = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, searchConditionsGroupType = "And", searchSortFieldNamesList = NULL, searchSortFieldNamesDescendingList = NULL, entityID = 1, schoolYearID = NULL, page = 1, pageSize = 100000, flatten = T, ...){

		params <- as.list(environment()) %>% append(list(...))

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- "all", searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		listSkyObjects(module = "StateReportingFederal", objectName = "CRDCSchoolAPComputerScienceV1", schoolYearID = schoolYearID, searchFields = searchFields, page = page, pageSize = pageSize, SearchConditionsList = searchConditionsList, SearchConditionsGroupType = searchConditionsGroupType, SearchSortFieldNamesList = searchSortFieldNamesList, SearchSortFieldNamesDescendingList = searchSortFieldNamesDescendingList, entityId = entityID, flatten = flatten)
	}


	listCRDCSchoolComputerScienceV1s <- function(searchConditionsList = NULL, CRDCSchoolComputerScienceV1ID = F, ClassCount = F, CertifiedClassCount = F, MaleHispanicCount = F, MaleAlaskanCount = F, MaleAsianCount = F, MaleHawaiianCount = F, MaleBlackCount = F, MaleWhiteCount = F, MaleMultiRaceCount = F, MaleLEPCount = F, MaleIDEACount = F, FemaleHispanicCount = F, FemaleAlaskanCount = F, FemaleAsianCount = F, FemaleHawaiianCount = F, FemaleBlackCount = F, FemaleWhiteCount = F, FemaleMultiRaceCount = F, FemaleLEPCount = F, FemaleIDEACount = F, FederalNCESSchoolID = F, CivilRightsDataCollectionID = F, CivilRightsDataCollectionRunHistoryID = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, searchConditionsGroupType = "And", searchSortFieldNamesList = NULL, searchSortFieldNamesDescendingList = NULL, entityID = 1, schoolYearID = NULL, page = 1, pageSize = 100000, flatten = T, ...){

		params <- as.list(environment()) %>% append(list(...))

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- "all", searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		listSkyObjects(module = "StateReportingFederal", objectName = "CRDCSchoolComputerScienceV1", schoolYearID = schoolYearID, searchFields = searchFields, page = page, pageSize = pageSize, SearchConditionsList = searchConditionsList, SearchConditionsGroupType = searchConditionsGroupType, SearchSortFieldNamesList = searchSortFieldNamesList, SearchSortFieldNamesDescendingList = searchSortFieldNamesDescendingList, entityId = entityID, flatten = flatten)
	}


	listCRDCSchoolInternetV1s <- function(searchConditionsList = NULL, CRDCSchoolInternetV1ID = F, HasFiberOptic = F, HasWiFi = F, AllowsSchoolDevices = F, AllowsStudentDevices = F, NumberWiFiDevices = F, FederalNCESSchoolID = F, CivilRightsDataCollectionID = F, CivilRightsDataCollectionRunHistoryID = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, searchConditionsGroupType = "And", searchSortFieldNamesList = NULL, searchSortFieldNamesDescendingList = NULL, entityID = 1, schoolYearID = NULL, page = 1, pageSize = 100000, flatten = T, ...){

		params <- as.list(environment()) %>% append(list(...))

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- "all", searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		listSkyObjects(module = "StateReportingFederal", objectName = "CRDCSchoolInternetV1", schoolYearID = schoolYearID, searchFields = searchFields, page = page, pageSize = pageSize, SearchConditionsList = searchConditionsList, SearchConditionsGroupType = searchConditionsGroupType, SearchSortFieldNamesList = searchSortFieldNamesList, SearchSortFieldNamesDescendingList = searchSortFieldNamesDescendingList, entityId = entityID, flatten = flatten)
	}


	listCRDCForms <- function(searchConditionsList = NULL, CRDCFormID = F, Code = F, Description = F, SkywardID = F, SkywardHash = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, searchConditionsGroupType = "And", searchSortFieldNamesList = NULL, searchSortFieldNamesDescendingList = NULL, entityID = 1, schoolYearID = NULL, page = 1, pageSize = 100000, flatten = T, ...){

		params <- as.list(environment()) %>% append(list(...))

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- "all", searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		listSkyObjects(module = "StateReportingFederal", objectName = "CRDCForm", schoolYearID = schoolYearID, searchFields = searchFields, page = page, pageSize = pageSize, SearchConditionsList = searchConditionsList, SearchConditionsGroupType = searchConditionsGroupType, SearchSortFieldNamesList = searchSortFieldNamesList, SearchSortFieldNamesDescendingList = searchSortFieldNamesDescendingList, entityId = entityID, flatten = flatten)
	}


	listCRDCClassDetails <- function(searchConditionsList = NULL, CRDCClassDetailID = F, CivilRightsDataCollectionID = F, CivilRightsDataCollectionRunHistoryID = F, FederalNCESSchoolID = F, EntityID = F, SectionID = F, IsAlgebraIGrade0708 = F, IsAlgebraICertifiedGrade0708 = F, IsAlgebraIGrade0912 = F, IsAlgebraICertifiedGrade0912 = F, IsAlgebraIIGrade0912 = F, IsAlgebraIICertifiedGrade0912 = F, IsGeometryGrade0912 = F, IsGeometryCertifiedGrade0912 = F, IsAdvancedMathGrade0912 = F, IsAdvancedMathCertifiedGrade0912 = F, IsCalculusGrade0912 = F, IsCalculusCertifiedGrade0912 = F, IsBiologyGrade0912 = F, IsBiologyCertifiedGrade0912 = F, IsChemistryGrade0912 = F, IsChemistryCertifiedGrade0912 = F, IsPhysicsGrade0912 = F, IsPhysicsCertifiedGrade0912 = F, IsAlgebraGeometryMaleOnly = F, IsAlgebraGeometryFemaleOnly = F, IsOtherMathMaleOnly = F, IsOtherMathFemaleOnly = F, IsScienceMaleOnly = F, IsScienceFemaleOnly = F, IsEnglishMaleOnly = F, IsEnglishFemaleOnly = F, IsOtherSubjectsMaleOnly = F, IsOtherSubjectsFemaleOnly = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, searchConditionsGroupType = "And", searchSortFieldNamesList = NULL, searchSortFieldNamesDescendingList = NULL, entityID = 1, schoolYearID = NULL, page = 1, pageSize = 100000, flatten = T, ...){

		params <- as.list(environment()) %>% append(list(...))

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- "all", searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		listSkyObjects(module = "StateReportingFederal", objectName = "CRDCClassDetail", schoolYearID = schoolYearID, searchFields = searchFields, page = page, pageSize = pageSize, SearchConditionsList = searchConditionsList, SearchConditionsGroupType = searchConditionsGroupType, SearchSortFieldNamesList = searchSortFieldNamesList, SearchSortFieldNamesDescendingList = searchSortFieldNamesDescendingList, entityId = entityID, flatten = flatten)
	}


	listCRDCLEAExportFields <- function(searchConditionsList = NULL, CRDCLEAExportFieldID = F, Header = F, Section = F, Table = F, Field = F, Order = F, SkywardID = F, SkywardHash = F, ValidYearLow = F, ValidYearHigh = F, IsInvalid = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, searchConditionsGroupType = "And", searchSortFieldNamesList = NULL, searchSortFieldNamesDescendingList = NULL, entityID = 1, schoolYearID = NULL, page = 1, pageSize = 100000, flatten = T, ...){

		params <- as.list(environment()) %>% append(list(...))

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- "all", searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		listSkyObjects(module = "StateReportingFederal", objectName = "CRDCLEAExportField", schoolYearID = schoolYearID, searchFields = searchFields, page = page, pageSize = pageSize, SearchConditionsList = searchConditionsList, SearchConditionsGroupType = searchConditionsGroupType, SearchSortFieldNamesList = searchSortFieldNamesList, SearchSortFieldNamesDescendingList = searchSortFieldNamesDescendingList, entityId = entityID, flatten = flatten)
	}


	listCRDCSchoolExportFields <- function(searchConditionsList = NULL, CRDCSchoolExportFieldID = F, Header = F, Section = F, Table = F, Field = F, Order = F, SkywardID = F, SkywardHash = F, ValidYearLow = F, ValidYearHigh = F, IsInvalid = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, searchConditionsGroupType = "And", searchSortFieldNamesList = NULL, searchSortFieldNamesDescendingList = NULL, entityID = 1, schoolYearID = NULL, page = 1, pageSize = 100000, flatten = T, ...){

		params <- as.list(environment()) %>% append(list(...))

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- "all", searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		listSkyObjects(module = "StateReportingFederal", objectName = "CRDCSchoolExportField", schoolYearID = schoolYearID, searchFields = searchFields, page = page, pageSize = pageSize, SearchConditionsList = searchConditionsList, SearchConditionsGroupType = searchConditionsGroupType, SearchSortFieldNamesList = searchSortFieldNamesList, SearchSortFieldNamesDescendingList = searchSortFieldNamesDescendingList, entityId = entityID, flatten = flatten)
	}


	listW2Countries <- function(searchConditionsList = NULL, W2CountryID = F, Code = F, DisplayName = F, SkywardID = F, SkywardHash = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, searchConditionsGroupType = "And", searchSortFieldNamesList = NULL, searchSortFieldNamesDescendingList = NULL, entityID = 1, schoolYearID = NULL, page = 1, pageSize = 100000, flatten = T, ...){

		params <- as.list(environment()) %>% append(list(...))

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- "all", searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		listSkyObjects(module = "StateReportingFederal", objectName = "W2Country", schoolYearID = schoolYearID, searchFields = searchFields, page = page, pageSize = pageSize, SearchConditionsList = searchConditionsList, SearchConditionsGroupType = searchConditionsGroupType, SearchSortFieldNamesList = searchSortFieldNamesList, SearchSortFieldNamesDescendingList = searchSortFieldNamesDescendingList, entityId = entityID, flatten = flatten)
	}


	listFederalACASoftwareIdentifiers <- function(searchConditionsList = NULL, FederalACASoftwareIdentifierID = F, NumericYear = F, SoftwareIdentifier = F, SkywardID = F, SkywardHash = F, ValidYearLow = F, ValidYearHigh = F, IsInvalid = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, searchConditionsGroupType = "And", searchSortFieldNamesList = NULL, searchSortFieldNamesDescendingList = NULL, entityID = 1, schoolYearID = NULL, page = 1, pageSize = 100000, flatten = T, ...){

		params <- as.list(environment()) %>% append(list(...))

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- "all", searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		listSkyObjects(module = "StateReportingFederal", objectName = "FederalACASoftwareIdentifier", schoolYearID = schoolYearID, searchFields = searchFields, page = page, pageSize = pageSize, SearchConditionsList = searchConditionsList, SearchConditionsGroupType = searchConditionsGroupType, SearchSortFieldNamesList = searchSortFieldNamesList, SearchSortFieldNamesDescendingList = searchSortFieldNamesDescendingList, entityId = entityID, flatten = flatten)
	}


	listNelcoForms <- function(searchConditionsList = NULL, NelcoFormID = F, SkywardID = F, SkywardHash = F, Code = F, Description = F, ProcessType = F, FormShape = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, searchConditionsGroupType = "And", searchSortFieldNamesList = NULL, searchSortFieldNamesDescendingList = NULL, entityID = 1, schoolYearID = NULL, page = 1, pageSize = 100000, flatten = T, ...){

		params <- as.list(environment()) %>% append(list(...))

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- "all", searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		listSkyObjects(module = "StateReportingFederal", objectName = "NelcoForm", schoolYearID = schoolYearID, searchFields = searchFields, page = page, pageSize = pageSize, SearchConditionsList = searchConditionsList, SearchConditionsGroupType = searchConditionsGroupType, SearchSortFieldNamesList = searchSortFieldNamesList, SearchSortFieldNamesDescendingList = searchSortFieldNamesDescendingList, entityId = entityID, flatten = flatten)
	}


	listNelcoFormFields <- function(searchConditionsList = NULL, NelcoFormFieldID = F, NelcoFormID = F, SkywardID = F, SkywardHash = F, Code = F, Description = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, searchConditionsGroupType = "And", searchSortFieldNamesList = NULL, searchSortFieldNamesDescendingList = NULL, entityID = 1, schoolYearID = NULL, page = 1, pageSize = 100000, flatten = T, ...){

		params <- as.list(environment()) %>% append(list(...))

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- "all", searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		listSkyObjects(module = "StateReportingFederal", objectName = "NelcoFormField", schoolYearID = schoolYearID, searchFields = searchFields, page = page, pageSize = pageSize, SearchConditionsList = searchConditionsList, SearchConditionsGroupType = searchConditionsGroupType, SearchSortFieldNamesList = searchSortFieldNamesList, SearchSortFieldNamesDescendingList = searchSortFieldNamesDescendingList, entityId = entityID, flatten = flatten)
	}


	listNelcoFormFieldPositions <- function(searchConditionsList = NULL, NelcoFormFieldPositionID = F, NelcoFormFieldID = F, SkywardID = F, SkywardHash = F, Code = F, Description = F, ValidYearLow = F, ValidYearHigh = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, searchConditionsGroupType = "And", searchSortFieldNamesList = NULL, searchSortFieldNamesDescendingList = NULL, entityID = 1, schoolYearID = NULL, page = 1, pageSize = 100000, flatten = T, ...){

		params <- as.list(environment()) %>% append(list(...))

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- "all", searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		listSkyObjects(module = "StateReportingFederal", objectName = "NelcoFormFieldPosition", schoolYearID = schoolYearID, searchFields = searchFields, page = page, pageSize = pageSize, SearchConditionsList = searchConditionsList, SearchConditionsGroupType = searchConditionsGroupType, SearchSortFieldNamesList = searchSortFieldNamesList, SearchSortFieldNamesDescendingList = searchSortFieldNamesDescendingList, entityId = entityID, flatten = flatten)
	}


	listForm941EmployeeV1s <- function(searchConditionsList = NULL, Form941EmployeeV1ID = F, Form941ExtractRunID = F, EmployeeID = F, AdditionalTaxableMedicareWages = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, FirstName = F, LastName = F, IsPaidOn12th = F, searchConditionsGroupType = "And", searchSortFieldNamesList = NULL, searchSortFieldNamesDescendingList = NULL, entityID = 1, schoolYearID = NULL, page = 1, pageSize = 100000, flatten = T, ...){

		params <- as.list(environment()) %>% append(list(...))

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- "all", searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		listSkyObjects(module = "StateReportingFederal", objectName = "Form941EmployeeV1", schoolYearID = schoolYearID, searchFields = searchFields, page = page, pageSize = pageSize, SearchConditionsList = searchConditionsList, SearchConditionsGroupType = searchConditionsGroupType, SearchSortFieldNamesList = searchSortFieldNamesList, SearchSortFieldNamesDescendingList = searchSortFieldNamesDescendingList, entityId = entityID, flatten = flatten)
	}


	listACADelimitedFileFormats <- function(searchConditionsList = NULL, ACADelimitedFileFormatID = F, SkywardID = F, SkywardHash = F, ACAThirdPartyFormatID = F, NumberOfHeaderRows = F, DelimiterType = F, DelimiterCharacter = F, EmployeeIdentifierColumnNumber = F, OfferOfCoverageCodeColumnNumber = F, EmployeeRequiredContributionColumnNumber = F, SafeHarborColumnNumber = F, EmployerOfferedSelfInsuredCoverageColumnNumber = F, ReportEmployeeAsCoveredIndividualColumnNumber = F, LastNameColumnNumber = F, FirstNameColumnNumber = F, MiddleNameColumnNumber = F, SSNColumnNumber = F, BirthDateColumnNumber = F, CommentColumnNumber = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, searchConditionsGroupType = "And", searchSortFieldNamesList = NULL, searchSortFieldNamesDescendingList = NULL, entityID = 1, schoolYearID = NULL, page = 1, pageSize = 100000, flatten = T, ...){

		params <- as.list(environment()) %>% append(list(...))

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- "all", searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		listSkyObjects(module = "StateReportingFederal", objectName = "ACADelimitedFileFormat", schoolYearID = schoolYearID, searchFields = searchFields, page = page, pageSize = pageSize, SearchConditionsList = searchConditionsList, SearchConditionsGroupType = searchConditionsGroupType, SearchSortFieldNamesList = searchSortFieldNamesList, SearchSortFieldNamesDescendingList = searchSortFieldNamesDescendingList, entityId = entityID, flatten = flatten)
	}


	listACAFixedLengthFileFormats <- function(searchConditionsList = NULL, ACAFixedLengthFileFormatID = F, SkywardID = F, ACAThirdPartyFormatID = F, NumberOfHeaderRows = F, SkywardHash = F, EmployeeIdentifierStartPosition = F, EmployeeIdentifierLength = F, OfferOfCoverageCodeStartPosition = F, OfferOfCoverageCodeLength = F, EmployeeRequiredContributionStartPosition = F, EmployeeRequiredContributionLength = F, SafeHarborStartPosition = F, SafeHarborLength = F, EmployerOfferedSelfInsuredCoverageStartPosition = F, EmployerOfferedSelfInsuredCoverageLength = F, ReportEmployeeAsCoveredIndividualStartPosition = F, ReportEmployeeAsCoveredIndividualLength = F, LastNameStartPosition = F, LastNameLength = F, FirstNameStartPosition = F, FirstNameLength = F, MiddleNameStartPosition = F, MiddleNameLength = F, SSNStartPosition = F, SSNLength = F, BirthDateStartPosition = F, BirthDateLength = F, CommentStartPosition = F, CommentLength = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, searchConditionsGroupType = "And", searchSortFieldNamesList = NULL, searchSortFieldNamesDescendingList = NULL, entityID = 1, schoolYearID = NULL, page = 1, pageSize = 100000, flatten = T, ...){

		params <- as.list(environment()) %>% append(list(...))

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- "all", searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		listSkyObjects(module = "StateReportingFederal", objectName = "ACAFixedLengthFileFormat", schoolYearID = schoolYearID, searchFields = searchFields, page = page, pageSize = pageSize, SearchConditionsList = searchConditionsList, SearchConditionsGroupType = searchConditionsGroupType, SearchSortFieldNamesList = searchSortFieldNamesList, SearchSortFieldNamesDescendingList = searchSortFieldNamesDescendingList, entityId = entityID, flatten = flatten)
	}


	listACAThirdPartyFormats <- function(searchConditionsList = NULL, ACAThirdPartyFormatID = F, SkywardID = F, SkywardIDClonedFrom = F, SkywardHash = F, DistrictID = F, CalendarYearID = F, Code = F, Description = F, MarkAllAsEmployerOfferedSelfInsuredCoverage = F, MarkAllAsReportEmployeeAsCoveredIndividual = F, CodeDescription = F, IsSystemLoaded = F, ImportType = F, ImportRecordType = F, ExistingRecordOverride = F, ExistingCoveredIndividualHandlingType = F, EmployeeIdentification = F, DateFormat = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, searchConditionsGroupType = "And", searchSortFieldNamesList = NULL, searchSortFieldNamesDescendingList = NULL, entityID = 1, schoolYearID = NULL, page = 1, pageSize = 100000, flatten = T, ...){

		params <- as.list(environment()) %>% append(list(...))

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- "all", searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		listSkyObjects(module = "StateReportingFederal", objectName = "ACAThirdPartyFormat", schoolYearID = schoolYearID, searchFields = searchFields, page = page, pageSize = pageSize, SearchConditionsList = searchConditionsList, SearchConditionsGroupType = searchConditionsGroupType, SearchSortFieldNamesList = searchSortFieldNamesList, SearchSortFieldNamesDescendingList = searchSortFieldNamesDescendingList, entityId = entityID, flatten = flatten)
	}


	listACAThirdPartyFormatOfferOfCoverages <- function(searchConditionsList = NULL, ACAThirdPartyFormatOfferOfCoverageID = F, ACAThirdPartyFormatID = F, FederalACAOfferAndCoverageID = F, ImportValue = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, searchConditionsGroupType = "And", searchSortFieldNamesList = NULL, searchSortFieldNamesDescendingList = NULL, entityID = 1, schoolYearID = NULL, page = 1, pageSize = 100000, flatten = T, ...){

		params <- as.list(environment()) %>% append(list(...))

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- "all", searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		listSkyObjects(module = "StateReportingFederal", objectName = "ACAThirdPartyFormatOfferOfCoverage", schoolYearID = schoolYearID, searchFields = searchFields, page = page, pageSize = pageSize, SearchConditionsList = searchConditionsList, SearchConditionsGroupType = searchConditionsGroupType, SearchSortFieldNamesList = searchSortFieldNamesList, SearchSortFieldNamesDescendingList = searchSortFieldNamesDescendingList, entityId = entityID, flatten = flatten)
	}


	listACAThirdPartyFormatSafeHarbors <- function(searchConditionsList = NULL, ACAThirdPartyFormatSafeHarborID = F, ACAThirdPartyFormatID = F, FederalACASafeHarborID = F, ImportValue = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, searchConditionsGroupType = "And", searchSortFieldNamesList = NULL, searchSortFieldNamesDescendingList = NULL, entityID = 1, schoolYearID = NULL, page = 1, pageSize = 100000, flatten = T, ...){

		params <- as.list(environment()) %>% append(list(...))

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- "all", searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		listSkyObjects(module = "StateReportingFederal", objectName = "ACAThirdPartyFormatSafeHarbor", schoolYearID = schoolYearID, searchFields = searchFields, page = page, pageSize = pageSize, SearchConditionsList = searchConditionsList, SearchConditionsGroupType = searchConditionsGroupType, SearchSortFieldNamesList = searchSortFieldNamesList, SearchSortFieldNamesDescendingList = searchSortFieldNamesDescendingList, entityId = entityID, flatten = flatten)
	}


	listACAThirdPartyImports <- function(searchConditionsList = NULL, ACAThirdPartyImportID = F, ACAThirdPartyFormatID = F, ImportTime = F, CalendarYear = F, Month = F, ImportData = F, MediaID = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, searchConditionsGroupType = "And", searchSortFieldNamesList = NULL, searchSortFieldNamesDescendingList = NULL, entityID = 1, schoolYearID = NULL, page = 1, pageSize = 100000, flatten = T, ...){

		params <- as.list(environment()) %>% append(list(...))

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- "all", searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		listSkyObjects(module = "StateReportingFederal", objectName = "ACAThirdPartyImport", schoolYearID = schoolYearID, searchFields = searchFields, page = page, pageSize = pageSize, SearchConditionsList = searchConditionsList, SearchConditionsGroupType = searchConditionsGroupType, SearchSortFieldNamesList = searchSortFieldNamesList, SearchSortFieldNamesDescendingList = searchSortFieldNamesDescendingList, entityId = entityID, flatten = flatten)
	}


	listFederalEEOCSalaryRanges <- function(searchConditionsList = NULL, FederalEEOCSalaryRangeID = F, Code = F, SalaryLow = F, SalaryHigh = F, SkywardID = F, SkywardHash = F, ValidYearLow = F, ValidYearHigh = F, IsInvalid = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, searchConditionsGroupType = "And", searchSortFieldNamesList = NULL, searchSortFieldNamesDescendingList = NULL, entityID = 1, schoolYearID = NULL, page = 1, pageSize = 100000, flatten = T, ...){

		params <- as.list(environment()) %>% append(list(...))

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- "all", searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		listSkyObjects(module = "StateReportingFederal", objectName = "FederalEEOCSalaryRange", schoolYearID = schoolYearID, searchFields = searchFields, page = page, pageSize = pageSize, SearchConditionsList = searchConditionsList, SearchConditionsGroupType = searchConditionsGroupType, SearchSortFieldNamesList = searchSortFieldNamesList, SearchSortFieldNamesDescendingList = searchSortFieldNamesDescendingList, entityId = entityID, flatten = flatten)
	}


	listForm1099MPermittedVendorFields <- function(searchConditionsList = NULL, Form1099MPermittedVendorFieldID = F, FieldName = F, SkywardID = F, SkywardHash = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, searchConditionsGroupType = "And", searchSortFieldNamesList = NULL, searchSortFieldNamesDescendingList = NULL, entityID = 1, schoolYearID = NULL, page = 1, pageSize = 100000, flatten = T, ...){

		params <- as.list(environment()) %>% append(list(...))

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- "all", searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		listSkyObjects(module = "StateReportingFederal", objectName = "Form1099MPermittedVendorField", schoolYearID = schoolYearID, searchFields = searchFields, page = page, pageSize = pageSize, SearchConditionsList = searchConditionsList, SearchConditionsGroupType = searchConditionsGroupType, SearchSortFieldNamesList = searchSortFieldNamesList, SearchSortFieldNamesDescendingList = searchSortFieldNamesDescendingList, entityId = entityID, flatten = flatten)
	}


	listW2YearEmployeeLocalityV1s <- function(searchConditionsList = NULL, W2YearEmployeeLocalityV1ID = F, W2YearEmployeeV1ID = F, W2YearLocalityID = F, Box18LocalWages = F, Box19LocalIncomeTax = F, IsOriginalExtractedValue = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, searchConditionsGroupType = "And", searchSortFieldNamesList = NULL, searchSortFieldNamesDescendingList = NULL, entityID = 1, schoolYearID = NULL, page = 1, pageSize = 100000, flatten = T, ...){

		params <- as.list(environment()) %>% append(list(...))

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- "all", searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		listSkyObjects(module = "StateReportingFederal", objectName = "W2YearEmployeeLocalityV1", schoolYearID = schoolYearID, searchFields = searchFields, page = page, pageSize = pageSize, SearchConditionsList = searchConditionsList, SearchConditionsGroupType = searchConditionsGroupType, SearchSortFieldNamesList = searchSortFieldNamesList, SearchSortFieldNamesDescendingList = searchSortFieldNamesDescendingList, entityId = entityID, flatten = flatten)
	}


	listW2YearLocalities <- function(searchConditionsList = NULL, W2YearLocalityID = F, W2YearID = F, Code = F, Description = F, Box20LocalityName = F, CodeDescription = F, LocalTaxID = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, searchConditionsGroupType = "And", searchSortFieldNamesList = NULL, searchSortFieldNamesDescendingList = NULL, entityID = 1, schoolYearID = NULL, page = 1, pageSize = 100000, flatten = T, ...){

		params <- as.list(environment()) %>% append(list(...))

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- "all", searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		listSkyObjects(module = "StateReportingFederal", objectName = "W2YearLocality", schoolYearID = schoolYearID, searchFields = searchFields, page = page, pageSize = pageSize, SearchConditionsList = searchConditionsList, SearchConditionsGroupType = searchConditionsGroupType, SearchSortFieldNamesList = searchSortFieldNamesList, SearchSortFieldNamesDescendingList = searchSortFieldNamesDescendingList, entityId = entityID, flatten = flatten)
	}


	listForm1099MExportElectronicFileMedias <- function(searchConditionsList = NULL, Form1099MExportElectronicFileMediaID = F, Form1099MExportElectronicFileID = F, MediaID = F, StateID = F, Status = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, searchConditionsGroupType = "And", searchSortFieldNamesList = NULL, searchSortFieldNamesDescendingList = NULL, entityID = 1, schoolYearID = NULL, page = 1, pageSize = 100000, flatten = T, ...){

		params <- as.list(environment()) %>% append(list(...))

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- "all", searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		listSkyObjects(module = "StateReportingFederal", objectName = "Form1099MExportElectronicFileMedia", schoolYearID = schoolYearID, searchFields = searchFields, page = page, pageSize = pageSize, SearchConditionsList = searchConditionsList, SearchConditionsGroupType = searchConditionsGroupType, SearchSortFieldNamesList = searchSortFieldNamesList, SearchSortFieldNamesDescendingList = searchSortFieldNamesDescendingList, entityId = entityID, flatten = flatten)
	}


	listForm1099MYearStateCalendarYears <- function(searchConditionsList = NULL, Form1099MYearStateCalendarYearID = F, Form1099MYearID = F, StateCalendarYearID = F, Vendors = F, searchConditionsGroupType = "And", searchSortFieldNamesList = NULL, searchSortFieldNamesDescendingList = NULL, entityID = 1, schoolYearID = NULL, page = 1, pageSize = 100000, flatten = T, ...){

		params <- as.list(environment()) %>% append(list(...))

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- "all", searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		listSkyObjects(module = "StateReportingFederal", objectName = "Form1099MYearStateCalendarYear", schoolYearID = schoolYearID, searchFields = searchFields, page = page, pageSize = pageSize, SearchConditionsList = searchConditionsList, SearchConditionsGroupType = searchConditionsGroupType, SearchSortFieldNamesList = searchSortFieldNamesList, SearchSortFieldNamesDescendingList = searchSortFieldNamesDescendingList, entityId = entityID, flatten = flatten)
	}


	listGECCountries <- function(searchConditionsList = NULL, GECCountryID = F, Code = F, Description = F, CodeDescription = F, SkywardID = F, SkywardHash = F, ValidYearLow = F, ValidYearHigh = F, IsInvalid = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, searchConditionsGroupType = "And", searchSortFieldNamesList = NULL, searchSortFieldNamesDescendingList = NULL, entityID = 1, schoolYearID = NULL, page = 1, pageSize = 100000, flatten = T, ...){

		params <- as.list(environment()) %>% append(list(...))

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- "all", searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		listSkyObjects(module = "StateReportingFederal", objectName = "GECCountry", schoolYearID = schoolYearID, searchFields = searchFields, page = page, pageSize = pageSize, SearchConditionsList = searchConditionsList, SearchConditionsGroupType = searchConditionsGroupType, SearchSortFieldNamesList = searchSortFieldNamesList, SearchSortFieldNamesDescendingList = searchSortFieldNamesDescendingList, entityId = entityID, flatten = flatten)
	}


	listForm1099BoxThresholds <- function(searchConditionsList = NULL, Form1099BoxThresholdID = F, ConfigCalendarYearID = F, Form1099TypeID = F, Amount = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, searchConditionsGroupType = "And", searchSortFieldNamesList = NULL, searchSortFieldNamesDescendingList = NULL, entityID = 1, schoolYearID = NULL, page = 1, pageSize = 100000, flatten = T, ...){

		params <- as.list(environment()) %>% append(list(...))

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- "all", searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		listSkyObjects(module = "StateReportingFederal", objectName = "Form1099BoxThreshold", schoolYearID = schoolYearID, searchFields = searchFields, page = page, pageSize = pageSize, SearchConditionsList = searchConditionsList, SearchConditionsGroupType = searchConditionsGroupType, SearchSortFieldNamesList = searchSortFieldNamesList, SearchSortFieldNamesDescendingList = searchSortFieldNamesDescendingList, entityId = entityID, flatten = flatten)
	}


	listForm1099MYearStateBoxValues <- function(searchConditionsList = NULL, Form1099MYearStateBoxValueID = F, Form1099MYearID = F, StateID = F, BoxNumber = F, Description = F, Amount = F, searchConditionsGroupType = "And", searchSortFieldNamesList = NULL, searchSortFieldNamesDescendingList = NULL, entityID = 1, schoolYearID = NULL, page = 1, pageSize = 100000, flatten = T, ...){

		params <- as.list(environment()) %>% append(list(...))

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- "all", searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		listSkyObjects(module = "StateReportingFederal", objectName = "Form1099MYearStateBoxValue", schoolYearID = schoolYearID, searchFields = searchFields, page = page, pageSize = pageSize, SearchConditionsList = searchConditionsList, SearchConditionsGroupType = searchConditionsGroupType, SearchSortFieldNamesList = searchSortFieldNamesList, SearchSortFieldNamesDescendingList = searchSortFieldNamesDescendingList, entityId = entityID, flatten = flatten)
	}


	listForm1099MYearVendorBoxes <- function(searchConditionsList = NULL, Form1099MYearVendorBoxID = F, Form1099MYearVendorID = F, Form1099TypeID = F, Amount = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, searchConditionsGroupType = "And", searchSortFieldNamesList = NULL, searchSortFieldNamesDescendingList = NULL, entityID = 1, schoolYearID = NULL, page = 1, pageSize = 100000, flatten = T, ...){

		params <- as.list(environment()) %>% append(list(...))

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- "all", searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		listSkyObjects(module = "StateReportingFederal", objectName = "Form1099MYearVendorBox", schoolYearID = schoolYearID, searchFields = searchFields, page = page, pageSize = pageSize, SearchConditionsList = searchConditionsList, SearchConditionsGroupType = searchConditionsGroupType, SearchSortFieldNamesList = searchSortFieldNamesList, SearchSortFieldNamesDescendingList = searchSortFieldNamesDescendingList, entityId = entityID, flatten = flatten)
	}


	listConfigCalendarYears <- function(searchConditionsList = NULL, ConfigCalendarYearID = F, CalendarYearID = F, DistrictID = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, searchConditionsGroupType = "And", searchSortFieldNamesList = NULL, searchSortFieldNamesDescendingList = NULL, entityID = 1, schoolYearID = NULL, page = 1, pageSize = 100000, flatten = T, ...){

		params <- as.list(environment()) %>% append(list(...))

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- "all", searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		listSkyObjects(module = "StateReportingFederal", objectName = "ConfigCalendarYear", schoolYearID = schoolYearID, searchFields = searchFields, page = page, pageSize = pageSize, SearchConditionsList = searchConditionsList, SearchConditionsGroupType = searchConditionsGroupType, SearchSortFieldNamesList = searchSortFieldNamesList, SearchSortFieldNamesDescendingList = searchSortFieldNamesDescendingList, entityId = entityID, flatten = flatten)
	}
