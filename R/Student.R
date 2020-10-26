
	#' List Students
	#'
	#' This function returns a dataframe or json object of Students
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given Students. Defaults to FALSE for all return fields which, for convenience, returns all fields for the Students.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object listed as text. Run \code{\link{getSchemaForObjects}}('Student') to get more field paths.
	#' @param searchConditionsList A list of search conditions to filter results which are joined by the searchConditionsGroupType. Of the form {FieldName} {ConditionType} {SearchCondition}. For example, c('StudentID LessEqual 500', 'LastName Like Ander\%'). Run \code{\link{listSearchConditionTypes}} for a list of ConditionTypes. Defaults to NULL (unfiltered).
	#' @param searchConditionsGroupType The conjunction which joins multiple searchConditions in the searchConditionsList. Either 'Or' or 'And'. Defaults to 'And'.
	#' @param searchSortFieldNamesList The list of fields sort results by. Defaults to NULL (unsorted).
	#' @param searchSortFieldNamesDescendingList A list of T/F values corresponding to whether to sort each field in searchSortFieldNamesList in descending order. Defaults to F for each FieldName in searchSortFieldNamesList.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param query Queries to be added to the request. Ex. list(SchoolYearID = 1). Defaults to NULL.
	#' @param page Results are paginated. The page of results to return. Default is 1.
	#' @param pageSize Results are paginated. The number of records per page to return. Default is 100,000 (essentially all records for most objects).
	#' @param flatten Whether to flatten results into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Student
	#' @return A list of Students
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	listStudents <- function(searchConditionsList = NULL, StudentMNID = F, StudentStateID = F, StateEthnicityRaceCodeMNID = F, HasSection504Indicator = F, HasActiveSection504 = F, StudentID = F, NameID = F, FullNameLFM = F, FullNameFL = F, FullNameFML = F, AllowMediaDistribution = F, AllowTripsDistribution = F, AllowDistrictDistribution = F, AllowHigherEdDistribution = F, AllowLocalDistribution = F, AllowMilitaryDistribution = F, AllowPublicDistribution = F, AllowVendorsDistribution = F, ConversionKey = F, IsGraduated = F, StudentNumber = F, GraduationDate = F, GradYear = F, GraduationRequirementYear = F, IsCurrentActive = F, IsActiveAsOfDate = F, SpecifiedEntityYearNoShow = F, LanguageIDNative = F, EdFiCountryIDBirth = F, EarnedCredits = F, EarnedCreditsPossible = F, FailedCredits = F, EntityConfigFailedCredits = F, EntityConfigEarnedCredits = F, HasActiveAlert = F, HasActiveCriticalAlert = F, HasActiveHealthCondition = F, MMRStatus = F, HasActiveIHP = F, HasActiveGeneralNote = F, HasActiveParentalConsentNote = F, HealthProfessionalIDPrimaryPhysician = F, IsHealthProfessionalPrimaryPhysician = F, NameIDHospital = F, HealthProfessionalIDDentist = F, IsHealthProfessionalDentist = F, NameIDDentalPractice = F, NameIDHealthInsuranceCompany = F, HealthPolicyNumber = F, NameIDDentalInsuranceCompany = F, DentalPolicyNumber = F, HasAlertIndicator = F, HasCriticalAlertIndicator = F, HasHealthIndicator = F, HasIHPIndicator = F, HasGeneralNoteIndicator = F, HasParentalConsentNoteIndicator = F, CalculatedGradYear = F, CurrentDefaultEntityIsActive = F, CalculatedEntityYearIsActive = F, CalculatedGrade = F, GradeNumeric = F, Grade = F, Location = F, IsIDEASnapshot = F, IsIDEA = F, IsLEPSnapshot = F, IsLEP = F, IsGiftedTalentedSnapshot = F, IsSection504 = F, IsSection504Snapshot = F, IsChronicallyAbsent = F, HasTakenACT0912 = F, HasTakenSAT0912 = F, CorporalPunishmentIncidents = F, InSchoolSuspensionCount = F, OutOfSchoolSuspensionCount = F, ExpulsionWithServicesCount = F, ExpulsionWithoutServicesCount = F, ZeroToleranceWithServicesCount = F, ZeroToleranceWithoutServicesCount = F, LawEnforcementReferralCount = F, ArrestCount = F, OutOfSchoolSuspensionMissedDays = F, IsFederalDualEnrollment = F, IsFederalDistanceEducation = F, HasAlgebraIGrade07 = F, HasAlgebraIGrade08 = F, HasAlgebraIGrade0910 = F, HasAlgebraIGrade1112 = F, HasAlgebraIIGrade0912 = F, HasAdvancedMathGrade0912 = F, HasCalculusGrade0912 = F, HasGeometryGrade08 = F, HasGeometryGrade0912 = F, HasBiologyGrade0912 = F, HasChemistryGrade0912 = F, HasPhysicsGrade0912 = F, HasAPCourseGrade0912 = F, HasAPMathGrade0912 = F, HasAPScienceGrade0912 = F, HasAPOtherGrade0912 = F, HasPassedAlgebraIGrade07 = F, HasPassedAlgebraIGrade08 = F, HasPassedAlgebraIGrade0910 = F, HasPassedAlgebraIGrade1112 = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, IndicatorsXML = F, HasActiveStudentGuardianRestrictedAccess = F, FirstName = F, MiddleName = F, LastName = F, LanguageIDPrimary = F, HasStudentGuardianWithAllowFamilyAccessButNoSecurity = F, HasStudentEntityYearForCurrentSchoolYear = F, HasAllowStudentAccessButNoSecurity = F, EarliestDistrictEnrollmentDate = F, AllowStudentAccess = F, OverrideFeeOnlinePaymentAccess = F, FeeOnlinePaymentOverrideType = F, OverrideFoodServiceOnlinePaymentAccess = F, FoodServiceOnlinePaymentOverrideType = F, DateOfFirstEntryWithdrawalInEntity = F, MedicaidNumber = F, SchoolPathExpectedSchoolName = F, IsIBDiplomaProgramme = F, FormattedVaccinationDates = F, SchoolPathExpectedSchoolCode = F, CalculatedStudentStateID = F, HasDuplicateStudentNumber = F, IsCurrentlyTransported = F, SpecifiedCohortNumericSchoolYear = F, SchoolYearIDSpecifiedCohort = F, StudentRankSort = F, HasComputerScienceGrade0912 = F, HasAPComputerScienceGrade0912 = F, CalculatedPrimaryFormattedPhoneNumber = F, HasCreditRecovery = F, SingleSexAthleticCount = F, ReportedBulliedSex = F, ReportedBulliedRace = F, ReportedBulliedDisability = F, DisciplinedBullyingSex = F, DisciplinedBullyingRace = F, DisciplinedBullyingDisability = F, MechanicalRestraintCount = F, PhysicalRestraintCount = F, SeclusionCount = F, TransferToAlternativeSchool = F, EntryWithdrawalIDDefaultEntityToday = F, AttachmentCount = F, LocationDateToUse = F, LocationEntityID = F, LocationSchoolYearID = F, HasTakenAPExam0912 = F, CurrentEconomicIndicator = F, IsStateReportingUnknownGender = F, HasOneNormalEnrollmentInEntityDuringSchoolYear = F, TotalCommunityServiceHours = F, HasActiveInterventionPlan = F, HasInterventionPlanIndicator = F, InSpecifiedDirectCertificationImport = F, EligibilityCategoryForDirectCertificationImport = F, EffectiveDateForDirectCertificationImport = F, HasActiveMethodOfInstruction = F, HasMethodOfInstructionIndicator = F, HasSpecialEducationIndicator = F, HasActiveSpecialEducation = F, CountryIDBirth = F, fieldPaths = NULL, searchConditionsGroupType = "And", searchSortFieldNamesList = NULL, searchSortFieldNamesDescendingList = NULL, entityId = 1, query = NULL, page = 1, pageSize = 100000, flatten = T, returnResponse = F){

		params <- as.list(environment())

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		listSkyObjects(module = "Student", objectName = "Student", searchFields = searchFields %>% append(fieldPaths), page = page, pageSize = pageSize, SearchConditionsList = searchConditionsList, SearchConditionsGroupType = searchConditionsGroupType, SearchSortFieldNamesList = searchSortFieldNamesList, SearchSortFieldNamesDescendingList = searchSortFieldNamesDescendingList, entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' Get a Student
	#'
	#' This function returns a dataframe or json object of a Student
	#' @param StudentID The ID of the Student to return.
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given Student. Defaults to FALSE for all return fields which, for convenience, returns all fields for the Student.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object listed as text. Run \code{\link{getSchemaForObjects}}('Student') to get more field paths.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param query Queries to be added to the request. Ex. list(SchoolYearID = 1). Defaults to NULL.
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Student
	#' @return A dataframe or of Student
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	getStudent <- function(StudentID, StudentMNID = F, StudentStateID = F, StateEthnicityRaceCodeMNID = F, HasSection504Indicator = F, HasActiveSection504 = F, NameID = F, FullNameLFM = F, FullNameFL = F, FullNameFML = F, AllowMediaDistribution = F, AllowTripsDistribution = F, AllowDistrictDistribution = F, AllowHigherEdDistribution = F, AllowLocalDistribution = F, AllowMilitaryDistribution = F, AllowPublicDistribution = F, AllowVendorsDistribution = F, ConversionKey = F, IsGraduated = F, StudentNumber = F, GraduationDate = F, GradYear = F, GraduationRequirementYear = F, IsCurrentActive = F, IsActiveAsOfDate = F, SpecifiedEntityYearNoShow = F, LanguageIDNative = F, EdFiCountryIDBirth = F, EarnedCredits = F, EarnedCreditsPossible = F, FailedCredits = F, EntityConfigFailedCredits = F, EntityConfigEarnedCredits = F, HasActiveAlert = F, HasActiveCriticalAlert = F, HasActiveHealthCondition = F, MMRStatus = F, HasActiveIHP = F, HasActiveGeneralNote = F, HasActiveParentalConsentNote = F, HealthProfessionalIDPrimaryPhysician = F, IsHealthProfessionalPrimaryPhysician = F, NameIDHospital = F, HealthProfessionalIDDentist = F, IsHealthProfessionalDentist = F, NameIDDentalPractice = F, NameIDHealthInsuranceCompany = F, HealthPolicyNumber = F, NameIDDentalInsuranceCompany = F, DentalPolicyNumber = F, HasAlertIndicator = F, HasCriticalAlertIndicator = F, HasHealthIndicator = F, HasIHPIndicator = F, HasGeneralNoteIndicator = F, HasParentalConsentNoteIndicator = F, CalculatedGradYear = F, CurrentDefaultEntityIsActive = F, CalculatedEntityYearIsActive = F, CalculatedGrade = F, GradeNumeric = F, Grade = F, Location = F, IsIDEASnapshot = F, IsIDEA = F, IsLEPSnapshot = F, IsLEP = F, IsGiftedTalentedSnapshot = F, IsSection504 = F, IsSection504Snapshot = F, IsChronicallyAbsent = F, HasTakenACT0912 = F, HasTakenSAT0912 = F, CorporalPunishmentIncidents = F, InSchoolSuspensionCount = F, OutOfSchoolSuspensionCount = F, ExpulsionWithServicesCount = F, ExpulsionWithoutServicesCount = F, ZeroToleranceWithServicesCount = F, ZeroToleranceWithoutServicesCount = F, LawEnforcementReferralCount = F, ArrestCount = F, OutOfSchoolSuspensionMissedDays = F, IsFederalDualEnrollment = F, IsFederalDistanceEducation = F, HasAlgebraIGrade07 = F, HasAlgebraIGrade08 = F, HasAlgebraIGrade0910 = F, HasAlgebraIGrade1112 = F, HasAlgebraIIGrade0912 = F, HasAdvancedMathGrade0912 = F, HasCalculusGrade0912 = F, HasGeometryGrade08 = F, HasGeometryGrade0912 = F, HasBiologyGrade0912 = F, HasChemistryGrade0912 = F, HasPhysicsGrade0912 = F, HasAPCourseGrade0912 = F, HasAPMathGrade0912 = F, HasAPScienceGrade0912 = F, HasAPOtherGrade0912 = F, HasPassedAlgebraIGrade07 = F, HasPassedAlgebraIGrade08 = F, HasPassedAlgebraIGrade0910 = F, HasPassedAlgebraIGrade1112 = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, IndicatorsXML = F, HasActiveStudentGuardianRestrictedAccess = F, FirstName = F, MiddleName = F, LastName = F, LanguageIDPrimary = F, HasStudentGuardianWithAllowFamilyAccessButNoSecurity = F, HasStudentEntityYearForCurrentSchoolYear = F, HasAllowStudentAccessButNoSecurity = F, EarliestDistrictEnrollmentDate = F, AllowStudentAccess = F, OverrideFeeOnlinePaymentAccess = F, FeeOnlinePaymentOverrideType = F, OverrideFoodServiceOnlinePaymentAccess = F, FoodServiceOnlinePaymentOverrideType = F, DateOfFirstEntryWithdrawalInEntity = F, MedicaidNumber = F, SchoolPathExpectedSchoolName = F, IsIBDiplomaProgramme = F, FormattedVaccinationDates = F, SchoolPathExpectedSchoolCode = F, CalculatedStudentStateID = F, HasDuplicateStudentNumber = F, IsCurrentlyTransported = F, SpecifiedCohortNumericSchoolYear = F, SchoolYearIDSpecifiedCohort = F, StudentRankSort = F, HasComputerScienceGrade0912 = F, HasAPComputerScienceGrade0912 = F, CalculatedPrimaryFormattedPhoneNumber = F, HasCreditRecovery = F, SingleSexAthleticCount = F, ReportedBulliedSex = F, ReportedBulliedRace = F, ReportedBulliedDisability = F, DisciplinedBullyingSex = F, DisciplinedBullyingRace = F, DisciplinedBullyingDisability = F, MechanicalRestraintCount = F, PhysicalRestraintCount = F, SeclusionCount = F, TransferToAlternativeSchool = F, EntryWithdrawalIDDefaultEntityToday = F, AttachmentCount = F, LocationDateToUse = F, LocationEntityID = F, LocationSchoolYearID = F, HasTakenAPExam0912 = F, CurrentEconomicIndicator = F, IsStateReportingUnknownGender = F, HasOneNormalEnrollmentInEntityDuringSchoolYear = F, TotalCommunityServiceHours = F, HasActiveInterventionPlan = F, HasInterventionPlanIndicator = F, InSpecifiedDirectCertificationImport = F, EligibilityCategoryForDirectCertificationImport = F, EffectiveDateForDirectCertificationImport = F, HasActiveMethodOfInstruction = F, HasMethodOfInstructionIndicator = F, HasSpecialEducationIndicator = F, HasActiveSpecialEducation = F, CountryIDBirth = F, fieldPaths = NULL, entityId = 1, query = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment()) %>% purrr::keep(names(.) != "StudentID")

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		getSkyObject(module = "Student", objectName = "Student", objectId = StudentID, searchFields = searchFields, entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' Delete a Student
	#'
	#' This function deletes a Student
	#' @param StudentID The ID of the Student to delete
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param query Queries to be added to the request. Ex. list(SchoolYearID = 1). Defaults to NULL.
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Student
	#' @return The StudentID of the deleted Student.
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	deleteStudent <- function(StudentID, ignoreWarnings = F, entityId = 1, query = NULL, flatten = T, returnResponse = F){

		deleteSkyObject(module = "Student", objectName = "Student", objectId = StudentID, ignoreWarnings = ignoreWarnings, entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' Create a Student
	#'
	#' This function creates a Student
	#' @param fieldNames The field values to give the created Student. Each defaults to NULL.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param query Queries to be added to the request. Ex. list(SchoolYearID = 1). Defaults to NULL.
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Student
	#' @return A newly created Student
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	createStudent <- function(StudentStateID = NULL, StateEthnicityRaceCodeMNID = NULL, HasSection504Indicator = NULL, NameID = NULL, AllowMediaDistribution = NULL, AllowTripsDistribution = NULL, AllowDistrictDistribution = NULL, AllowHigherEdDistribution = NULL, AllowLocalDistribution = NULL, AllowMilitaryDistribution = NULL, AllowPublicDistribution = NULL, AllowVendorsDistribution = NULL, ConversionKey = NULL, IsGraduated = NULL, StudentNumber = NULL, GraduationDate = NULL, GradYear = NULL, GraduationRequirementYear = NULL, LanguageIDNative = NULL, EdFiCountryIDBirth = NULL, HealthProfessionalIDPrimaryPhysician = NULL, NameIDHospital = NULL, HealthProfessionalIDDentist = NULL, NameIDDentalPractice = NULL, NameIDHealthInsuranceCompany = NULL, HealthPolicyNumber = NULL, NameIDDentalInsuranceCompany = NULL, DentalPolicyNumber = NULL, HasAlertIndicator = NULL, HasCriticalAlertIndicator = NULL, HasHealthIndicator = NULL, HasIHPIndicator = NULL, HasGeneralNoteIndicator = NULL, HasParentalConsentNoteIndicator = NULL, GradeNumeric = NULL, Grade = NULL, IndicatorsXML = NULL, LanguageIDPrimary = NULL, AllowStudentAccess = NULL, OverrideFeeOnlinePaymentAccess = NULL, FeeOnlinePaymentOverrideType = NULL, OverrideFoodServiceOnlinePaymentAccess = NULL, FoodServiceOnlinePaymentOverrideType = NULL, MedicaidNumber = NULL, IsIBDiplomaProgramme = NULL, IsStateReportingUnknownGender = NULL, HasInterventionPlanIndicator = NULL, HasMethodOfInstructionIndicator = NULL, HasSpecialEducationIndicator = NULL, CountryIDBirth = NULL, entityId = 1, query = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment())

		body <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper()) %>% purrr::compact()

		createSkyObject(module = "Student", objectName = "Student", body = list(DataObject = body), searchFields = append("StudentID", body %>% names()), entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' Modify a Student
	#'
	#' This function modifies a Student
	#' @param fieldNames The field values to give the modified Student. Each defaults to NULL.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param query Queries to be added to the request. Ex. list(SchoolYearID = 1). Defaults to NULL.
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Student
	#' @return The modified Student
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	modifyStudent <- function(StudentID, StudentStateID = NULL, StateEthnicityRaceCodeMNID = NULL, HasSection504Indicator = NULL, NameID = NULL, AllowMediaDistribution = NULL, AllowTripsDistribution = NULL, AllowDistrictDistribution = NULL, AllowHigherEdDistribution = NULL, AllowLocalDistribution = NULL, AllowMilitaryDistribution = NULL, AllowPublicDistribution = NULL, AllowVendorsDistribution = NULL, ConversionKey = NULL, IsGraduated = NULL, StudentNumber = NULL, GraduationDate = NULL, GradYear = NULL, GraduationRequirementYear = NULL, LanguageIDNative = NULL, EdFiCountryIDBirth = NULL, HealthProfessionalIDPrimaryPhysician = NULL, NameIDHospital = NULL, HealthProfessionalIDDentist = NULL, NameIDDentalPractice = NULL, NameIDHealthInsuranceCompany = NULL, HealthPolicyNumber = NULL, NameIDDentalInsuranceCompany = NULL, DentalPolicyNumber = NULL, HasAlertIndicator = NULL, HasCriticalAlertIndicator = NULL, HasHealthIndicator = NULL, HasIHPIndicator = NULL, HasGeneralNoteIndicator = NULL, HasParentalConsentNoteIndicator = NULL, GradeNumeric = NULL, Grade = NULL, IndicatorsXML = NULL, LanguageIDPrimary = NULL, AllowStudentAccess = NULL, OverrideFeeOnlinePaymentAccess = NULL, FeeOnlinePaymentOverrideType = NULL, OverrideFoodServiceOnlinePaymentAccess = NULL, FoodServiceOnlinePaymentOverrideType = NULL, MedicaidNumber = NULL, IsIBDiplomaProgramme = NULL, IsStateReportingUnknownGender = NULL, HasInterventionPlanIndicator = NULL, HasMethodOfInstructionIndicator = NULL, HasSpecialEducationIndicator = NULL, CountryIDBirth = NULL, entityId = 1, query = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment())

		body <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper()) %>% purrr::compact()

		modifySkyObject(module = "Student", objectName = "Student", objectId = StudentID, body = list(DataObject = body), searchFields = append("StudentID", body %>% names()), entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' List NoteTypes
	#'
	#' This function returns a dataframe or json object of NoteTypes
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given NoteTypes. Defaults to FALSE for all return fields which, for convenience, returns all fields for the NoteTypes.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object listed as text. Run \code{\link{getSchemaForObjects}}('NoteType') to get more field paths.
	#' @param searchConditionsList A list of search conditions to filter results which are joined by the searchConditionsGroupType. Of the form {FieldName} {ConditionType} {SearchCondition}. For example, c('StudentID LessEqual 500', 'LastName Like Ander\%'). Run \code{\link{listSearchConditionTypes}} for a list of ConditionTypes. Defaults to NULL (unfiltered).
	#' @param searchConditionsGroupType The conjunction which joins multiple searchConditions in the searchConditionsList. Either 'Or' or 'And'. Defaults to 'And'.
	#' @param searchSortFieldNamesList The list of fields sort results by. Defaults to NULL (unsorted).
	#' @param searchSortFieldNamesDescendingList A list of T/F values corresponding to whether to sort each field in searchSortFieldNamesList in descending order. Defaults to F for each FieldName in searchSortFieldNamesList.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param query Queries to be added to the request. Ex. list(SchoolYearID = 1). Defaults to NULL.
	#' @param page Results are paginated. The page of results to return. Default is 1.
	#' @param pageSize Results are paginated. The number of records per page to return. Default is 100,000 (essentially all records for most objects).
	#' @param flatten Whether to flatten results into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Student
	#' @return A list of NoteTypes
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	listNoteTypes <- function(searchConditionsList = NULL, NoteTypeID = F, Code = F, Description = F, IsParentalConsent = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, IndicatorID = F, IsActive = F, SkywardID = F, SkywardHash = F, CodeDescription = F, fieldPaths = NULL, searchConditionsGroupType = "And", searchSortFieldNamesList = NULL, searchSortFieldNamesDescendingList = NULL, entityId = 1, query = NULL, page = 1, pageSize = 100000, flatten = T, returnResponse = F){

		params <- as.list(environment())

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		listSkyObjects(module = "Student", objectName = "NoteType", searchFields = searchFields %>% append(fieldPaths), page = page, pageSize = pageSize, SearchConditionsList = searchConditionsList, SearchConditionsGroupType = searchConditionsGroupType, SearchSortFieldNamesList = searchSortFieldNamesList, SearchSortFieldNamesDescendingList = searchSortFieldNamesDescendingList, entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' Get a NoteType
	#'
	#' This function returns a dataframe or json object of a NoteType
	#' @param NoteTypeID The ID of the NoteType to return.
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given NoteType. Defaults to FALSE for all return fields which, for convenience, returns all fields for the NoteType.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object listed as text. Run \code{\link{getSchemaForObjects}}('NoteType') to get more field paths.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param query Queries to be added to the request. Ex. list(SchoolYearID = 1). Defaults to NULL.
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Student
	#' @return A dataframe or of NoteType
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	getNoteType <- function(NoteTypeID, Code = F, Description = F, IsParentalConsent = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, IndicatorID = F, IsActive = F, SkywardID = F, SkywardHash = F, CodeDescription = F, fieldPaths = NULL, entityId = 1, query = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment()) %>% purrr::keep(names(.) != "NoteTypeID")

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		getSkyObject(module = "Student", objectName = "NoteType", objectId = NoteTypeID, searchFields = searchFields, entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' Delete a NoteType
	#'
	#' This function deletes a NoteType
	#' @param NoteTypeID The ID of the NoteType to delete
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param query Queries to be added to the request. Ex. list(SchoolYearID = 1). Defaults to NULL.
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Student
	#' @return The NoteTypeID of the deleted NoteType.
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	deleteNoteType <- function(NoteTypeID, ignoreWarnings = F, entityId = 1, query = NULL, flatten = T, returnResponse = F){

		deleteSkyObject(module = "Student", objectName = "NoteType", objectId = NoteTypeID, ignoreWarnings = ignoreWarnings, entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' Create a NoteType
	#'
	#' This function creates a NoteType
	#' @param fieldNames The field values to give the created NoteType. Each defaults to NULL.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param query Queries to be added to the request. Ex. list(SchoolYearID = 1). Defaults to NULL.
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Student
	#' @return A newly created NoteType
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	createNoteType <- function(Code = NULL, Description = NULL, IsParentalConsent = NULL, IndicatorID = NULL, IsActive = NULL, entityId = 1, query = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment())

		body <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper()) %>% purrr::compact()

		createSkyObject(module = "Student", objectName = "NoteType", body = list(DataObject = body), searchFields = append("NoteTypeID", body %>% names()), entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' Modify a NoteType
	#'
	#' This function modifies a NoteType
	#' @param fieldNames The field values to give the modified NoteType. Each defaults to NULL.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param query Queries to be added to the request. Ex. list(SchoolYearID = 1). Defaults to NULL.
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Student
	#' @return The modified NoteType
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	modifyNoteType <- function(NoteTypeID, Code = NULL, Description = NULL, IsParentalConsent = NULL, IndicatorID = NULL, IsActive = NULL, entityId = 1, query = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment())

		body <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper()) %>% purrr::compact()

		modifySkyObject(module = "Student", objectName = "NoteType", objectId = NoteTypeID, body = list(DataObject = body), searchFields = append("NoteTypeID", body %>% names()), entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' List StudentNotes
	#'
	#' This function returns a dataframe or json object of StudentNotes
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given StudentNotes. Defaults to FALSE for all return fields which, for convenience, returns all fields for the StudentNotes.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object listed as text. Run \code{\link{getSchemaForObjects}}('StudentNote') to get more field paths.
	#' @param searchConditionsList A list of search conditions to filter results which are joined by the searchConditionsGroupType. Of the form {FieldName} {ConditionType} {SearchCondition}. For example, c('StudentID LessEqual 500', 'LastName Like Ander\%'). Run \code{\link{listSearchConditionTypes}} for a list of ConditionTypes. Defaults to NULL (unfiltered).
	#' @param searchConditionsGroupType The conjunction which joins multiple searchConditions in the searchConditionsList. Either 'Or' or 'And'. Defaults to 'And'.
	#' @param searchSortFieldNamesList The list of fields sort results by. Defaults to NULL (unsorted).
	#' @param searchSortFieldNamesDescendingList A list of T/F values corresponding to whether to sort each field in searchSortFieldNamesList in descending order. Defaults to F for each FieldName in searchSortFieldNamesList.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param query Queries to be added to the request. Ex. list(SchoolYearID = 1). Defaults to NULL.
	#' @param page Results are paginated. The page of results to return. Default is 1.
	#' @param pageSize Results are paginated. The number of records per page to return. Default is 100,000 (essentially all records for most objects).
	#' @param flatten Whether to flatten results into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Student
	#' @return A list of StudentNotes
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	listStudentNotes <- function(searchConditionsList = NULL, StudentNoteID = F, StudentID = F, NoteTypeID = F, StartDate = F, EndDate = F, Description = F, IsActive = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, AttachmentCount = F, StudentTransportationID = F, fieldPaths = NULL, searchConditionsGroupType = "And", searchSortFieldNamesList = NULL, searchSortFieldNamesDescendingList = NULL, entityId = 1, query = NULL, page = 1, pageSize = 100000, flatten = T, returnResponse = F){

		params <- as.list(environment())

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		listSkyObjects(module = "Student", objectName = "StudentNote", searchFields = searchFields %>% append(fieldPaths), page = page, pageSize = pageSize, SearchConditionsList = searchConditionsList, SearchConditionsGroupType = searchConditionsGroupType, SearchSortFieldNamesList = searchSortFieldNamesList, SearchSortFieldNamesDescendingList = searchSortFieldNamesDescendingList, entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' Get a StudentNote
	#'
	#' This function returns a dataframe or json object of a StudentNote
	#' @param StudentNoteID The ID of the StudentNote to return.
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given StudentNote. Defaults to FALSE for all return fields which, for convenience, returns all fields for the StudentNote.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object listed as text. Run \code{\link{getSchemaForObjects}}('StudentNote') to get more field paths.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param query Queries to be added to the request. Ex. list(SchoolYearID = 1). Defaults to NULL.
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Student
	#' @return A dataframe or of StudentNote
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	getStudentNote <- function(StudentNoteID, StudentID = F, NoteTypeID = F, StartDate = F, EndDate = F, Description = F, IsActive = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, AttachmentCount = F, StudentTransportationID = F, fieldPaths = NULL, entityId = 1, query = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment()) %>% purrr::keep(names(.) != "StudentNoteID")

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		getSkyObject(module = "Student", objectName = "StudentNote", objectId = StudentNoteID, searchFields = searchFields, entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' Delete a StudentNote
	#'
	#' This function deletes a StudentNote
	#' @param StudentNoteID The ID of the StudentNote to delete
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param query Queries to be added to the request. Ex. list(SchoolYearID = 1). Defaults to NULL.
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Student
	#' @return The StudentNoteID of the deleted StudentNote.
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	deleteStudentNote <- function(StudentNoteID, ignoreWarnings = F, entityId = 1, query = NULL, flatten = T, returnResponse = F){

		deleteSkyObject(module = "Student", objectName = "StudentNote", objectId = StudentNoteID, ignoreWarnings = ignoreWarnings, entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' Create a StudentNote
	#'
	#' This function creates a StudentNote
	#' @param fieldNames The field values to give the created StudentNote. Each defaults to NULL.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param query Queries to be added to the request. Ex. list(SchoolYearID = 1). Defaults to NULL.
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Student
	#' @return A newly created StudentNote
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	createStudentNote <- function(StudentID = NULL, NoteTypeID = NULL, StartDate = NULL, EndDate = NULL, Description = NULL, StudentTransportationID = NULL, entityId = 1, query = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment())

		body <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper()) %>% purrr::compact()

		createSkyObject(module = "Student", objectName = "StudentNote", body = list(DataObject = body), searchFields = append("StudentNoteID", body %>% names()), entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' Modify a StudentNote
	#'
	#' This function modifies a StudentNote
	#' @param fieldNames The field values to give the modified StudentNote. Each defaults to NULL.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param query Queries to be added to the request. Ex. list(SchoolYearID = 1). Defaults to NULL.
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Student
	#' @return The modified StudentNote
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	modifyStudentNote <- function(StudentNoteID, StudentID = NULL, NoteTypeID = NULL, StartDate = NULL, EndDate = NULL, Description = NULL, StudentTransportationID = NULL, entityId = 1, query = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment())

		body <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper()) %>% purrr::compact()

		modifySkyObject(module = "Student", objectName = "StudentNote", objectId = StudentNoteID, body = list(DataObject = body), searchFields = append("StudentNoteID", body %>% names()), entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' List TempFitnessGrams
	#'
	#' This function returns a dataframe or json object of TempFitnessGrams
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given TempFitnessGrams. Defaults to FALSE for all return fields which, for convenience, returns all fields for the TempFitnessGrams.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object listed as text. Run \code{\link{getSchemaForObjects}}('TempFitnessGram') to get more field paths.
	#' @param searchConditionsList A list of search conditions to filter results which are joined by the searchConditionsGroupType. Of the form {FieldName} {ConditionType} {SearchCondition}. For example, c('StudentID LessEqual 500', 'LastName Like Ander\%'). Run \code{\link{listSearchConditionTypes}} for a list of ConditionTypes. Defaults to NULL (unfiltered).
	#' @param searchConditionsGroupType The conjunction which joins multiple searchConditions in the searchConditionsList. Either 'Or' or 'And'. Defaults to 'And'.
	#' @param searchSortFieldNamesList The list of fields sort results by. Defaults to NULL (unsorted).
	#' @param searchSortFieldNamesDescendingList A list of T/F values corresponding to whether to sort each field in searchSortFieldNamesList in descending order. Defaults to F for each FieldName in searchSortFieldNamesList.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param query Queries to be added to the request. Ex. list(SchoolYearID = 1). Defaults to NULL.
	#' @param page Results are paginated. The page of results to return. Default is 1.
	#' @param pageSize Results are paginated. The number of records per page to return. Default is 100,000 (essentially all records for most objects).
	#' @param flatten Whether to flatten results into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Student
	#' @return A list of TempFitnessGrams
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	listTempFitnessGrams <- function(searchConditionsList = NULL, TempFitnessGramID = F, HasMissingData = F, Message = F, SchoolID = F, StudentID = F, StudentFirstName = F, StudentLastName = F, StudentBirthdate = F, StudentGrade = F, StudentGender = F, StudentPassword = F, ClassName = F, ClassID = F, ClassStartDate = F, ClassEndDate = F, TeacherID = F, TeacherFirstName = F, TeacherLastName = F, TeacherBirthDate = F, TeacherPassword = F, TeacherEmail = F, StudentMiddleInitial = F, StudentReportEmail = F, ParentReportEmail1 = F, ParentReportEmail2 = F, ClassDescription = F, TeacherMiddleInitial = F, CourseCodeDescription = F, SectionCode = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, StudentSSOID = F, TeacherSSOID = F, fieldPaths = NULL, searchConditionsGroupType = "And", searchSortFieldNamesList = NULL, searchSortFieldNamesDescendingList = NULL, entityId = 1, query = NULL, page = 1, pageSize = 100000, flatten = T, returnResponse = F){

		params <- as.list(environment())

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		listSkyObjects(module = "Student", objectName = "TempFitnessGram", searchFields = searchFields %>% append(fieldPaths), page = page, pageSize = pageSize, SearchConditionsList = searchConditionsList, SearchConditionsGroupType = searchConditionsGroupType, SearchSortFieldNamesList = searchSortFieldNamesList, SearchSortFieldNamesDescendingList = searchSortFieldNamesDescendingList, entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' Get a TempFitnessGram
	#'
	#' This function returns a dataframe or json object of a TempFitnessGram
	#' @param TempFitnessGramID The ID of the TempFitnessGram to return.
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given TempFitnessGram. Defaults to FALSE for all return fields which, for convenience, returns all fields for the TempFitnessGram.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object listed as text. Run \code{\link{getSchemaForObjects}}('TempFitnessGram') to get more field paths.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param query Queries to be added to the request. Ex. list(SchoolYearID = 1). Defaults to NULL.
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Student
	#' @return A dataframe or of TempFitnessGram
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	getTempFitnessGram <- function(TempFitnessGramID, HasMissingData = F, Message = F, SchoolID = F, StudentID = F, StudentFirstName = F, StudentLastName = F, StudentBirthdate = F, StudentGrade = F, StudentGender = F, StudentPassword = F, ClassName = F, ClassID = F, ClassStartDate = F, ClassEndDate = F, TeacherID = F, TeacherFirstName = F, TeacherLastName = F, TeacherBirthDate = F, TeacherPassword = F, TeacherEmail = F, StudentMiddleInitial = F, StudentReportEmail = F, ParentReportEmail1 = F, ParentReportEmail2 = F, ClassDescription = F, TeacherMiddleInitial = F, CourseCodeDescription = F, SectionCode = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, StudentSSOID = F, TeacherSSOID = F, fieldPaths = NULL, entityId = 1, query = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment()) %>% purrr::keep(names(.) != "TempFitnessGramID")

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		getSkyObject(module = "Student", objectName = "TempFitnessGram", objectId = TempFitnessGramID, searchFields = searchFields, entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' Delete a TempFitnessGram
	#'
	#' This function deletes a TempFitnessGram
	#' @param TempFitnessGramID The ID of the TempFitnessGram to delete
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param query Queries to be added to the request. Ex. list(SchoolYearID = 1). Defaults to NULL.
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Student
	#' @return The TempFitnessGramID of the deleted TempFitnessGram.
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	deleteTempFitnessGram <- function(TempFitnessGramID, ignoreWarnings = F, entityId = 1, query = NULL, flatten = T, returnResponse = F){

		deleteSkyObject(module = "Student", objectName = "TempFitnessGram", objectId = TempFitnessGramID, ignoreWarnings = ignoreWarnings, entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' Create a TempFitnessGram
	#'
	#' This function creates a TempFitnessGram
	#' @param fieldNames The field values to give the created TempFitnessGram. Each defaults to NULL.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param query Queries to be added to the request. Ex. list(SchoolYearID = 1). Defaults to NULL.
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Student
	#' @return A newly created TempFitnessGram
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	createTempFitnessGram <- function(HasMissingData = NULL, Message = NULL, SchoolID = NULL, StudentID = NULL, StudentFirstName = NULL, StudentLastName = NULL, StudentBirthdate = NULL, StudentGrade = NULL, StudentGender = NULL, StudentPassword = NULL, ClassName = NULL, ClassID = NULL, ClassStartDate = NULL, ClassEndDate = NULL, TeacherID = NULL, TeacherFirstName = NULL, TeacherLastName = NULL, TeacherBirthDate = NULL, TeacherPassword = NULL, TeacherEmail = NULL, StudentMiddleInitial = NULL, StudentReportEmail = NULL, ParentReportEmail1 = NULL, ParentReportEmail2 = NULL, ClassDescription = NULL, TeacherMiddleInitial = NULL, CourseCodeDescription = NULL, SectionCode = NULL, StudentSSOID = NULL, TeacherSSOID = NULL, entityId = 1, query = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment())

		body <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper()) %>% purrr::compact()

		createSkyObject(module = "Student", objectName = "TempFitnessGram", body = list(DataObject = body), searchFields = append("TempFitnessGramID", body %>% names()), entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' Modify a TempFitnessGram
	#'
	#' This function modifies a TempFitnessGram
	#' @param fieldNames The field values to give the modified TempFitnessGram. Each defaults to NULL.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param query Queries to be added to the request. Ex. list(SchoolYearID = 1). Defaults to NULL.
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Student
	#' @return The modified TempFitnessGram
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	modifyTempFitnessGram <- function(TempFitnessGramID, HasMissingData = NULL, Message = NULL, SchoolID = NULL, StudentID = NULL, StudentFirstName = NULL, StudentLastName = NULL, StudentBirthdate = NULL, StudentGrade = NULL, StudentGender = NULL, StudentPassword = NULL, ClassName = NULL, ClassID = NULL, ClassStartDate = NULL, ClassEndDate = NULL, TeacherID = NULL, TeacherFirstName = NULL, TeacherLastName = NULL, TeacherBirthDate = NULL, TeacherPassword = NULL, TeacherEmail = NULL, StudentMiddleInitial = NULL, StudentReportEmail = NULL, ParentReportEmail1 = NULL, ParentReportEmail2 = NULL, ClassDescription = NULL, TeacherMiddleInitial = NULL, CourseCodeDescription = NULL, SectionCode = NULL, StudentSSOID = NULL, TeacherSSOID = NULL, entityId = 1, query = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment())

		body <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper()) %>% purrr::compact()

		modifySkyObject(module = "Student", objectName = "TempFitnessGram", objectId = TempFitnessGramID, body = list(DataObject = body), searchFields = append("TempFitnessGramID", body %>% names()), entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' List Indicators
	#'
	#' This function returns a dataframe or json object of Indicators
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given Indicators. Defaults to FALSE for all return fields which, for convenience, returns all fields for the Indicators.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object listed as text. Run \code{\link{getSchemaForObjects}}('Indicator') to get more field paths.
	#' @param searchConditionsList A list of search conditions to filter results which are joined by the searchConditionsGroupType. Of the form {FieldName} {ConditionType} {SearchCondition}. For example, c('StudentID LessEqual 500', 'LastName Like Ander\%'). Run \code{\link{listSearchConditionTypes}} for a list of ConditionTypes. Defaults to NULL (unfiltered).
	#' @param searchConditionsGroupType The conjunction which joins multiple searchConditions in the searchConditionsList. Either 'Or' or 'And'. Defaults to 'And'.
	#' @param searchSortFieldNamesList The list of fields sort results by. Defaults to NULL (unsorted).
	#' @param searchSortFieldNamesDescendingList A list of T/F values corresponding to whether to sort each field in searchSortFieldNamesList in descending order. Defaults to F for each FieldName in searchSortFieldNamesList.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param query Queries to be added to the request. Ex. list(SchoolYearID = 1). Defaults to NULL.
	#' @param page Results are paginated. The page of results to return. Default is 1.
	#' @param pageSize Results are paginated. The number of records per page to return. Default is 100,000 (essentially all records for most objects).
	#' @param flatten Whether to flatten results into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Student
	#' @return A list of Indicators
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	listIndicators <- function(searchConditionsList = NULL, IndicatorID = F, Name = F, Rank = F, IsActive = F, SkywardID = F, Image = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, SkywardHash = F, fieldPaths = NULL, searchConditionsGroupType = "And", searchSortFieldNamesList = NULL, searchSortFieldNamesDescendingList = NULL, entityId = 1, query = NULL, page = 1, pageSize = 100000, flatten = T, returnResponse = F){

		params <- as.list(environment())

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		listSkyObjects(module = "Student", objectName = "Indicator", searchFields = searchFields %>% append(fieldPaths), page = page, pageSize = pageSize, SearchConditionsList = searchConditionsList, SearchConditionsGroupType = searchConditionsGroupType, SearchSortFieldNamesList = searchSortFieldNamesList, SearchSortFieldNamesDescendingList = searchSortFieldNamesDescendingList, entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' Get an Indicator
	#'
	#' This function returns a dataframe or json object of an Indicator
	#' @param IndicatorID The ID of the Indicator to return.
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given Indicator. Defaults to FALSE for all return fields which, for convenience, returns all fields for the Indicator.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object listed as text. Run \code{\link{getSchemaForObjects}}('Indicator') to get more field paths.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param query Queries to be added to the request. Ex. list(SchoolYearID = 1). Defaults to NULL.
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Student
	#' @return A dataframe or of Indicator
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	getIndicator <- function(IndicatorID, Name = F, Rank = F, IsActive = F, SkywardID = F, Image = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, SkywardHash = F, fieldPaths = NULL, entityId = 1, query = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment()) %>% purrr::keep(names(.) != "IndicatorID")

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		getSkyObject(module = "Student", objectName = "Indicator", objectId = IndicatorID, searchFields = searchFields, entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' Delete an Indicator
	#'
	#' This function deletes an Indicator
	#' @param IndicatorID The ID of the Indicator to delete
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param query Queries to be added to the request. Ex. list(SchoolYearID = 1). Defaults to NULL.
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Student
	#' @return The IndicatorID of the deleted Indicator.
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	deleteIndicator <- function(IndicatorID, ignoreWarnings = F, entityId = 1, query = NULL, flatten = T, returnResponse = F){

		deleteSkyObject(module = "Student", objectName = "Indicator", objectId = IndicatorID, ignoreWarnings = ignoreWarnings, entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' Create an Indicator
	#'
	#' This function creates an Indicator
	#' @param fieldNames The field values to give the created Indicator. Each defaults to NULL.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param query Queries to be added to the request. Ex. list(SchoolYearID = 1). Defaults to NULL.
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Student
	#' @return A newly created Indicator
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	createIndicator <- function(Name = NULL, Rank = NULL, IsActive = NULL, Image = NULL, entityId = 1, query = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment())

		body <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper()) %>% purrr::compact()

		createSkyObject(module = "Student", objectName = "Indicator", body = list(DataObject = body), searchFields = append("IndicatorID", body %>% names()), entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' Modify an Indicator
	#'
	#' This function modifies an Indicator
	#' @param fieldNames The field values to give the modified Indicator. Each defaults to NULL.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param query Queries to be added to the request. Ex. list(SchoolYearID = 1). Defaults to NULL.
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Student
	#' @return The modified Indicator
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	modifyIndicator <- function(IndicatorID, Name = NULL, Rank = NULL, IsActive = NULL, Image = NULL, entityId = 1, query = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment())

		body <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper()) %>% purrr::compact()

		modifySkyObject(module = "Student", objectName = "Indicator", objectId = IndicatorID, body = list(DataObject = body), searchFields = append("IndicatorID", body %>% names()), entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' List SkylertAttendanceExportAttendanceTypes
	#'
	#' This function returns a dataframe or json object of SkylertAttendanceExportAttendanceTypes
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given SkylertAttendanceExportAttendanceTypes. Defaults to FALSE for all return fields which, for convenience, returns all fields for the SkylertAttendanceExportAttendanceTypes.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object listed as text. Run \code{\link{getSchemaForObjects}}('SkylertAttendanceExportAttendanceType') to get more field paths.
	#' @param searchConditionsList A list of search conditions to filter results which are joined by the searchConditionsGroupType. Of the form {FieldName} {ConditionType} {SearchCondition}. For example, c('StudentID LessEqual 500', 'LastName Like Ander\%'). Run \code{\link{listSearchConditionTypes}} for a list of ConditionTypes. Defaults to NULL (unfiltered).
	#' @param searchConditionsGroupType The conjunction which joins multiple searchConditions in the searchConditionsList. Either 'Or' or 'And'. Defaults to 'And'.
	#' @param searchSortFieldNamesList The list of fields sort results by. Defaults to NULL (unsorted).
	#' @param searchSortFieldNamesDescendingList A list of T/F values corresponding to whether to sort each field in searchSortFieldNamesList in descending order. Defaults to F for each FieldName in searchSortFieldNamesList.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param query Queries to be added to the request. Ex. list(SchoolYearID = 1). Defaults to NULL.
	#' @param page Results are paginated. The page of results to return. Default is 1.
	#' @param pageSize Results are paginated. The number of records per page to return. Default is 100,000 (essentially all records for most objects).
	#' @param flatten Whether to flatten results into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Student
	#' @return A list of SkylertAttendanceExportAttendanceTypes
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	listSkylertAttendanceExportAttendanceTypes <- function(searchConditionsList = NULL, SkylertAttendanceExportAttendanceTypeID = F, SkylertAttendanceExportSettingID = F, AttendanceTypeID = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, AttendanceTypeCodeOverride = F, fieldPaths = NULL, searchConditionsGroupType = "And", searchSortFieldNamesList = NULL, searchSortFieldNamesDescendingList = NULL, entityId = 1, query = NULL, page = 1, pageSize = 100000, flatten = T, returnResponse = F){

		params <- as.list(environment())

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		listSkyObjects(module = "Student", objectName = "SkylertAttendanceExportAttendanceType", searchFields = searchFields %>% append(fieldPaths), page = page, pageSize = pageSize, SearchConditionsList = searchConditionsList, SearchConditionsGroupType = searchConditionsGroupType, SearchSortFieldNamesList = searchSortFieldNamesList, SearchSortFieldNamesDescendingList = searchSortFieldNamesDescendingList, entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' Get a SkylertAttendanceExportAttendanceType
	#'
	#' This function returns a dataframe or json object of a SkylertAttendanceExportAttendanceType
	#' @param SkylertAttendanceExportAttendanceTypeID The ID of the SkylertAttendanceExportAttendanceType to return.
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given SkylertAttendanceExportAttendanceType. Defaults to FALSE for all return fields which, for convenience, returns all fields for the SkylertAttendanceExportAttendanceType.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object listed as text. Run \code{\link{getSchemaForObjects}}('SkylertAttendanceExportAttendanceType') to get more field paths.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param query Queries to be added to the request. Ex. list(SchoolYearID = 1). Defaults to NULL.
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Student
	#' @return A dataframe or of SkylertAttendanceExportAttendanceType
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	getSkylertAttendanceExportAttendanceType <- function(SkylertAttendanceExportAttendanceTypeID, SkylertAttendanceExportSettingID = F, AttendanceTypeID = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, AttendanceTypeCodeOverride = F, fieldPaths = NULL, entityId = 1, query = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment()) %>% purrr::keep(names(.) != "SkylertAttendanceExportAttendanceTypeID")

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		getSkyObject(module = "Student", objectName = "SkylertAttendanceExportAttendanceType", objectId = SkylertAttendanceExportAttendanceTypeID, searchFields = searchFields, entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' Delete a SkylertAttendanceExportAttendanceType
	#'
	#' This function deletes a SkylertAttendanceExportAttendanceType
	#' @param SkylertAttendanceExportAttendanceTypeID The ID of the SkylertAttendanceExportAttendanceType to delete
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param query Queries to be added to the request. Ex. list(SchoolYearID = 1). Defaults to NULL.
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Student
	#' @return The SkylertAttendanceExportAttendanceTypeID of the deleted SkylertAttendanceExportAttendanceType.
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	deleteSkylertAttendanceExportAttendanceType <- function(SkylertAttendanceExportAttendanceTypeID, ignoreWarnings = F, entityId = 1, query = NULL, flatten = T, returnResponse = F){

		deleteSkyObject(module = "Student", objectName = "SkylertAttendanceExportAttendanceType", objectId = SkylertAttendanceExportAttendanceTypeID, ignoreWarnings = ignoreWarnings, entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' Create a SkylertAttendanceExportAttendanceType
	#'
	#' This function creates a SkylertAttendanceExportAttendanceType
	#' @param fieldNames The field values to give the created SkylertAttendanceExportAttendanceType. Each defaults to NULL.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param query Queries to be added to the request. Ex. list(SchoolYearID = 1). Defaults to NULL.
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Student
	#' @return A newly created SkylertAttendanceExportAttendanceType
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	createSkylertAttendanceExportAttendanceType <- function(SkylertAttendanceExportSettingID = NULL, AttendanceTypeID = NULL, AttendanceTypeCodeOverride = NULL, entityId = 1, query = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment())

		body <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper()) %>% purrr::compact()

		createSkyObject(module = "Student", objectName = "SkylertAttendanceExportAttendanceType", body = list(DataObject = body), searchFields = append("SkylertAttendanceExportAttendanceTypeID", body %>% names()), entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' Modify a SkylertAttendanceExportAttendanceType
	#'
	#' This function modifies a SkylertAttendanceExportAttendanceType
	#' @param fieldNames The field values to give the modified SkylertAttendanceExportAttendanceType. Each defaults to NULL.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param query Queries to be added to the request. Ex. list(SchoolYearID = 1). Defaults to NULL.
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Student
	#' @return The modified SkylertAttendanceExportAttendanceType
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	modifySkylertAttendanceExportAttendanceType <- function(SkylertAttendanceExportAttendanceTypeID, SkylertAttendanceExportSettingID = NULL, AttendanceTypeID = NULL, AttendanceTypeCodeOverride = NULL, entityId = 1, query = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment())

		body <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper()) %>% purrr::compact()

		modifySkyObject(module = "Student", objectName = "SkylertAttendanceExportAttendanceType", objectId = SkylertAttendanceExportAttendanceTypeID, body = list(DataObject = body), searchFields = append("SkylertAttendanceExportAttendanceTypeID", body %>% names()), entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' List SkylertAttendanceExportSettings
	#'
	#' This function returns a dataframe or json object of SkylertAttendanceExportSettings
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given SkylertAttendanceExportSettings. Defaults to FALSE for all return fields which, for convenience, returns all fields for the SkylertAttendanceExportSettings.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object listed as text. Run \code{\link{getSchemaForObjects}}('SkylertAttendanceExportSetting') to get more field paths.
	#' @param searchConditionsList A list of search conditions to filter results which are joined by the searchConditionsGroupType. Of the form {FieldName} {ConditionType} {SearchCondition}. For example, c('StudentID LessEqual 500', 'LastName Like Ander\%'). Run \code{\link{listSearchConditionTypes}} for a list of ConditionTypes. Defaults to NULL (unfiltered).
	#' @param searchConditionsGroupType The conjunction which joins multiple searchConditions in the searchConditionsList. Either 'Or' or 'And'. Defaults to 'And'.
	#' @param searchSortFieldNamesList The list of fields sort results by. Defaults to NULL (unsorted).
	#' @param searchSortFieldNamesDescendingList A list of T/F values corresponding to whether to sort each field in searchSortFieldNamesList in descending order. Defaults to F for each FieldName in searchSortFieldNamesList.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param query Queries to be added to the request. Ex. list(SchoolYearID = 1). Defaults to NULL.
	#' @param page Results are paginated. The page of results to return. Default is 1.
	#' @param pageSize Results are paginated. The number of records per page to return. Default is 100,000 (essentially all records for most objects).
	#' @param flatten Whether to flatten results into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Student
	#' @return A list of SkylertAttendanceExportSettings
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	listSkylertAttendanceExportSettings <- function(searchConditionsList = NULL, SkylertAttendanceExportSettingID = F, EntityID = F, SchoolYearID = F, ParentNotifiedOption = F, MinimumPeriodsAbsent = F, AttendancePeriodIDLow = F, AttendancePeriodIDHigh = F, FileSequence = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, fieldPaths = NULL, searchConditionsGroupType = "And", searchSortFieldNamesList = NULL, searchSortFieldNamesDescendingList = NULL, entityId = 1, query = NULL, page = 1, pageSize = 100000, flatten = T, returnResponse = F){

		params <- as.list(environment())

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		listSkyObjects(module = "Student", objectName = "SkylertAttendanceExportSetting", searchFields = searchFields %>% append(fieldPaths), page = page, pageSize = pageSize, SearchConditionsList = searchConditionsList, SearchConditionsGroupType = searchConditionsGroupType, SearchSortFieldNamesList = searchSortFieldNamesList, SearchSortFieldNamesDescendingList = searchSortFieldNamesDescendingList, entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' Get a SkylertAttendanceExportSetting
	#'
	#' This function returns a dataframe or json object of a SkylertAttendanceExportSetting
	#' @param SkylertAttendanceExportSettingID The ID of the SkylertAttendanceExportSetting to return.
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given SkylertAttendanceExportSetting. Defaults to FALSE for all return fields which, for convenience, returns all fields for the SkylertAttendanceExportSetting.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object listed as text. Run \code{\link{getSchemaForObjects}}('SkylertAttendanceExportSetting') to get more field paths.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param query Queries to be added to the request. Ex. list(SchoolYearID = 1). Defaults to NULL.
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Student
	#' @return A dataframe or of SkylertAttendanceExportSetting
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	getSkylertAttendanceExportSetting <- function(SkylertAttendanceExportSettingID, EntityID = F, SchoolYearID = F, ParentNotifiedOption = F, MinimumPeriodsAbsent = F, AttendancePeriodIDLow = F, AttendancePeriodIDHigh = F, FileSequence = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, fieldPaths = NULL, entityId = 1, query = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment()) %>% purrr::keep(names(.) != "SkylertAttendanceExportSettingID")

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		getSkyObject(module = "Student", objectName = "SkylertAttendanceExportSetting", objectId = SkylertAttendanceExportSettingID, searchFields = searchFields, entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' Delete a SkylertAttendanceExportSetting
	#'
	#' This function deletes a SkylertAttendanceExportSetting
	#' @param SkylertAttendanceExportSettingID The ID of the SkylertAttendanceExportSetting to delete
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param query Queries to be added to the request. Ex. list(SchoolYearID = 1). Defaults to NULL.
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Student
	#' @return The SkylertAttendanceExportSettingID of the deleted SkylertAttendanceExportSetting.
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	deleteSkylertAttendanceExportSetting <- function(SkylertAttendanceExportSettingID, ignoreWarnings = F, entityId = 1, query = NULL, flatten = T, returnResponse = F){

		deleteSkyObject(module = "Student", objectName = "SkylertAttendanceExportSetting", objectId = SkylertAttendanceExportSettingID, ignoreWarnings = ignoreWarnings, entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' Create a SkylertAttendanceExportSetting
	#'
	#' This function creates a SkylertAttendanceExportSetting
	#' @param fieldNames The field values to give the created SkylertAttendanceExportSetting. Each defaults to NULL.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param query Queries to be added to the request. Ex. list(SchoolYearID = 1). Defaults to NULL.
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Student
	#' @return A newly created SkylertAttendanceExportSetting
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	createSkylertAttendanceExportSetting <- function(EntityID = NULL, SchoolYearID = NULL, ParentNotifiedOption = NULL, MinimumPeriodsAbsent = NULL, AttendancePeriodIDLow = NULL, AttendancePeriodIDHigh = NULL, FileSequence = NULL, entityId = 1, query = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment())

		body <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper()) %>% purrr::compact()

		createSkyObject(module = "Student", objectName = "SkylertAttendanceExportSetting", body = list(DataObject = body), searchFields = append("SkylertAttendanceExportSettingID", body %>% names()), entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' Modify a SkylertAttendanceExportSetting
	#'
	#' This function modifies a SkylertAttendanceExportSetting
	#' @param fieldNames The field values to give the modified SkylertAttendanceExportSetting. Each defaults to NULL.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param query Queries to be added to the request. Ex. list(SchoolYearID = 1). Defaults to NULL.
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Student
	#' @return The modified SkylertAttendanceExportSetting
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	modifySkylertAttendanceExportSetting <- function(SkylertAttendanceExportSettingID, EntityID = NULL, SchoolYearID = NULL, ParentNotifiedOption = NULL, MinimumPeriodsAbsent = NULL, AttendancePeriodIDLow = NULL, AttendancePeriodIDHigh = NULL, FileSequence = NULL, entityId = 1, query = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment())

		body <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper()) %>% purrr::compact()

		modifySkyObject(module = "Student", objectName = "SkylertAttendanceExportSetting", objectId = SkylertAttendanceExportSettingID, body = list(DataObject = body), searchFields = append("SkylertAttendanceExportSettingID", body %>% names()), entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' List StudentMedias
	#'
	#' This function returns a dataframe or json object of StudentMedias
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given StudentMedias. Defaults to FALSE for all return fields which, for convenience, returns all fields for the StudentMedias.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object listed as text. Run \code{\link{getSchemaForObjects}}('StudentMedia') to get more field paths.
	#' @param searchConditionsList A list of search conditions to filter results which are joined by the searchConditionsGroupType. Of the form {FieldName} {ConditionType} {SearchCondition}. For example, c('StudentID LessEqual 500', 'LastName Like Ander\%'). Run \code{\link{listSearchConditionTypes}} for a list of ConditionTypes. Defaults to NULL (unfiltered).
	#' @param searchConditionsGroupType The conjunction which joins multiple searchConditions in the searchConditionsList. Either 'Or' or 'And'. Defaults to 'And'.
	#' @param searchSortFieldNamesList The list of fields sort results by. Defaults to NULL (unsorted).
	#' @param searchSortFieldNamesDescendingList A list of T/F values corresponding to whether to sort each field in searchSortFieldNamesList in descending order. Defaults to F for each FieldName in searchSortFieldNamesList.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param query Queries to be added to the request. Ex. list(SchoolYearID = 1). Defaults to NULL.
	#' @param page Results are paginated. The page of results to return. Default is 1.
	#' @param pageSize Results are paginated. The number of records per page to return. Default is 100,000 (essentially all records for most objects).
	#' @param flatten Whether to flatten results into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Student
	#' @return A list of StudentMedias
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	listStudentMedias <- function(searchConditionsList = NULL, StudentMediaID = F, StudentID = F, MediaID = F, DisplayName = F, DisplayInTeacherAccess = F, DisplayNameOrMediaName = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, AttachmentTypeID = F, DisplayOnFamilyAccessPortfolio = F, DisplayOnStudentAccessPortfolio = F, fieldPaths = NULL, searchConditionsGroupType = "And", searchSortFieldNamesList = NULL, searchSortFieldNamesDescendingList = NULL, entityId = 1, query = NULL, page = 1, pageSize = 100000, flatten = T, returnResponse = F){

		params <- as.list(environment())

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		listSkyObjects(module = "Student", objectName = "StudentMedia", searchFields = searchFields %>% append(fieldPaths), page = page, pageSize = pageSize, SearchConditionsList = searchConditionsList, SearchConditionsGroupType = searchConditionsGroupType, SearchSortFieldNamesList = searchSortFieldNamesList, SearchSortFieldNamesDescendingList = searchSortFieldNamesDescendingList, entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' Get a StudentMedia
	#'
	#' This function returns a dataframe or json object of a StudentMedia
	#' @param StudentMediaID The ID of the StudentMedia to return.
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given StudentMedia. Defaults to FALSE for all return fields which, for convenience, returns all fields for the StudentMedia.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object listed as text. Run \code{\link{getSchemaForObjects}}('StudentMedia') to get more field paths.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param query Queries to be added to the request. Ex. list(SchoolYearID = 1). Defaults to NULL.
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Student
	#' @return A dataframe or of StudentMedia
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	getStudentMedia <- function(StudentMediaID, StudentID = F, MediaID = F, DisplayName = F, DisplayInTeacherAccess = F, DisplayNameOrMediaName = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, AttachmentTypeID = F, DisplayOnFamilyAccessPortfolio = F, DisplayOnStudentAccessPortfolio = F, fieldPaths = NULL, entityId = 1, query = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment()) %>% purrr::keep(names(.) != "StudentMediaID")

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		getSkyObject(module = "Student", objectName = "StudentMedia", objectId = StudentMediaID, searchFields = searchFields, entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' Delete a StudentMedia
	#'
	#' This function deletes a StudentMedia
	#' @param StudentMediaID The ID of the StudentMedia to delete
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param query Queries to be added to the request. Ex. list(SchoolYearID = 1). Defaults to NULL.
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Student
	#' @return The StudentMediaID of the deleted StudentMedia.
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	deleteStudentMedia <- function(StudentMediaID, ignoreWarnings = F, entityId = 1, query = NULL, flatten = T, returnResponse = F){

		deleteSkyObject(module = "Student", objectName = "StudentMedia", objectId = StudentMediaID, ignoreWarnings = ignoreWarnings, entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' Create a StudentMedia
	#'
	#' This function creates a StudentMedia
	#' @param fieldNames The field values to give the created StudentMedia. Each defaults to NULL.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param query Queries to be added to the request. Ex. list(SchoolYearID = 1). Defaults to NULL.
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Student
	#' @return A newly created StudentMedia
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	createStudentMedia <- function(StudentID = NULL, MediaID = NULL, DisplayName = NULL, DisplayInTeacherAccess = NULL, AttachmentTypeID = NULL, DisplayOnFamilyAccessPortfolio = NULL, DisplayOnStudentAccessPortfolio = NULL, entityId = 1, query = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment())

		body <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper()) %>% purrr::compact()

		createSkyObject(module = "Student", objectName = "StudentMedia", body = list(DataObject = body), searchFields = append("StudentMediaID", body %>% names()), entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' Modify a StudentMedia
	#'
	#' This function modifies a StudentMedia
	#' @param fieldNames The field values to give the modified StudentMedia. Each defaults to NULL.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param query Queries to be added to the request. Ex. list(SchoolYearID = 1). Defaults to NULL.
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Student
	#' @return The modified StudentMedia
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	modifyStudentMedia <- function(StudentMediaID, StudentID = NULL, MediaID = NULL, DisplayName = NULL, DisplayInTeacherAccess = NULL, AttachmentTypeID = NULL, DisplayOnFamilyAccessPortfolio = NULL, DisplayOnStudentAccessPortfolio = NULL, entityId = 1, query = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment())

		body <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper()) %>% purrr::compact()

		modifySkyObject(module = "Student", objectName = "StudentMedia", objectId = StudentMediaID, body = list(DataObject = body), searchFields = append("StudentMediaID", body %>% names()), entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' List TempStudentWithoutLockerRecords
	#'
	#' This function returns a dataframe or json object of TempStudentWithoutLockerRecords
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given TempStudentWithoutLockerRecords. Defaults to FALSE for all return fields which, for convenience, returns all fields for the TempStudentWithoutLockerRecords.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object listed as text. Run \code{\link{getSchemaForObjects}}('TempStudentWithoutLockerRecord') to get more field paths.
	#' @param searchConditionsList A list of search conditions to filter results which are joined by the searchConditionsGroupType. Of the form {FieldName} {ConditionType} {SearchCondition}. For example, c('StudentID LessEqual 500', 'LastName Like Ander\%'). Run \code{\link{listSearchConditionTypes}} for a list of ConditionTypes. Defaults to NULL (unfiltered).
	#' @param searchConditionsGroupType The conjunction which joins multiple searchConditions in the searchConditionsList. Either 'Or' or 'And'. Defaults to 'And'.
	#' @param searchSortFieldNamesList The list of fields sort results by. Defaults to NULL (unsorted).
	#' @param searchSortFieldNamesDescendingList A list of T/F values corresponding to whether to sort each field in searchSortFieldNamesList in descending order. Defaults to F for each FieldName in searchSortFieldNamesList.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param query Queries to be added to the request. Ex. list(SchoolYearID = 1). Defaults to NULL.
	#' @param page Results are paginated. The page of results to return. Default is 1.
	#' @param pageSize Results are paginated. The number of records per page to return. Default is 100,000 (essentially all records for most objects).
	#' @param flatten Whether to flatten results into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Student
	#' @return A list of TempStudentWithoutLockerRecords
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	listTempStudentWithoutLockerRecords <- function(searchConditionsList = NULL, TempStudentWithoutLockerRecordID = F, StudentID = F, FullName = F, StudentNumber = F, Gender = F, Age = F, GradYear = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, fieldPaths = NULL, searchConditionsGroupType = "And", searchSortFieldNamesList = NULL, searchSortFieldNamesDescendingList = NULL, entityId = 1, query = NULL, page = 1, pageSize = 100000, flatten = T, returnResponse = F){

		params <- as.list(environment())

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		listSkyObjects(module = "Student", objectName = "TempStudentWithoutLockerRecord", searchFields = searchFields %>% append(fieldPaths), page = page, pageSize = pageSize, SearchConditionsList = searchConditionsList, SearchConditionsGroupType = searchConditionsGroupType, SearchSortFieldNamesList = searchSortFieldNamesList, SearchSortFieldNamesDescendingList = searchSortFieldNamesDescendingList, entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' Get a TempStudentWithoutLockerRecord
	#'
	#' This function returns a dataframe or json object of a TempStudentWithoutLockerRecord
	#' @param TempStudentWithoutLockerRecordID The ID of the TempStudentWithoutLockerRecord to return.
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given TempStudentWithoutLockerRecord. Defaults to FALSE for all return fields which, for convenience, returns all fields for the TempStudentWithoutLockerRecord.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object listed as text. Run \code{\link{getSchemaForObjects}}('TempStudentWithoutLockerRecord') to get more field paths.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param query Queries to be added to the request. Ex. list(SchoolYearID = 1). Defaults to NULL.
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Student
	#' @return A dataframe or of TempStudentWithoutLockerRecord
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	getTempStudentWithoutLockerRecord <- function(TempStudentWithoutLockerRecordID, StudentID = F, FullName = F, StudentNumber = F, Gender = F, Age = F, GradYear = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, fieldPaths = NULL, entityId = 1, query = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment()) %>% purrr::keep(names(.) != "TempStudentWithoutLockerRecordID")

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		getSkyObject(module = "Student", objectName = "TempStudentWithoutLockerRecord", objectId = TempStudentWithoutLockerRecordID, searchFields = searchFields, entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' Delete a TempStudentWithoutLockerRecord
	#'
	#' This function deletes a TempStudentWithoutLockerRecord
	#' @param TempStudentWithoutLockerRecordID The ID of the TempStudentWithoutLockerRecord to delete
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param query Queries to be added to the request. Ex. list(SchoolYearID = 1). Defaults to NULL.
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Student
	#' @return The TempStudentWithoutLockerRecordID of the deleted TempStudentWithoutLockerRecord.
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	deleteTempStudentWithoutLockerRecord <- function(TempStudentWithoutLockerRecordID, ignoreWarnings = F, entityId = 1, query = NULL, flatten = T, returnResponse = F){

		deleteSkyObject(module = "Student", objectName = "TempStudentWithoutLockerRecord", objectId = TempStudentWithoutLockerRecordID, ignoreWarnings = ignoreWarnings, entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' Create a TempStudentWithoutLockerRecord
	#'
	#' This function creates a TempStudentWithoutLockerRecord
	#' @param fieldNames The field values to give the created TempStudentWithoutLockerRecord. Each defaults to NULL.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param query Queries to be added to the request. Ex. list(SchoolYearID = 1). Defaults to NULL.
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Student
	#' @return A newly created TempStudentWithoutLockerRecord
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	createTempStudentWithoutLockerRecord <- function(StudentID = NULL, FullName = NULL, StudentNumber = NULL, Gender = NULL, Age = NULL, GradYear = NULL, entityId = 1, query = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment())

		body <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper()) %>% purrr::compact()

		createSkyObject(module = "Student", objectName = "TempStudentWithoutLockerRecord", body = list(DataObject = body), searchFields = append("TempStudentWithoutLockerRecordID", body %>% names()), entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' Modify a TempStudentWithoutLockerRecord
	#'
	#' This function modifies a TempStudentWithoutLockerRecord
	#' @param fieldNames The field values to give the modified TempStudentWithoutLockerRecord. Each defaults to NULL.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param query Queries to be added to the request. Ex. list(SchoolYearID = 1). Defaults to NULL.
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Student
	#' @return The modified TempStudentWithoutLockerRecord
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	modifyTempStudentWithoutLockerRecord <- function(TempStudentWithoutLockerRecordID, StudentID = NULL, FullName = NULL, StudentNumber = NULL, Gender = NULL, Age = NULL, GradYear = NULL, entityId = 1, query = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment())

		body <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper()) %>% purrr::compact()

		modifySkyObject(module = "Student", objectName = "TempStudentWithoutLockerRecord", objectId = TempStudentWithoutLockerRecordID, body = list(DataObject = body), searchFields = append("TempStudentWithoutLockerRecordID", body %>% names()), entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' List Alerts
	#'
	#' This function returns a dataframe or json object of Alerts
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given Alerts. Defaults to FALSE for all return fields which, for convenience, returns all fields for the Alerts.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object listed as text. Run \code{\link{getSchemaForObjects}}('Alert') to get more field paths.
	#' @param searchConditionsList A list of search conditions to filter results which are joined by the searchConditionsGroupType. Of the form {FieldName} {ConditionType} {SearchCondition}. For example, c('StudentID LessEqual 500', 'LastName Like Ander\%'). Run \code{\link{listSearchConditionTypes}} for a list of ConditionTypes. Defaults to NULL (unfiltered).
	#' @param searchConditionsGroupType The conjunction which joins multiple searchConditions in the searchConditionsList. Either 'Or' or 'And'. Defaults to 'And'.
	#' @param searchSortFieldNamesList The list of fields sort results by. Defaults to NULL (unsorted).
	#' @param searchSortFieldNamesDescendingList A list of T/F values corresponding to whether to sort each field in searchSortFieldNamesList in descending order. Defaults to F for each FieldName in searchSortFieldNamesList.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param query Queries to be added to the request. Ex. list(SchoolYearID = 1). Defaults to NULL.
	#' @param page Results are paginated. The page of results to return. Default is 1.
	#' @param pageSize Results are paginated. The number of records per page to return. Default is 100,000 (essentially all records for most objects).
	#' @param flatten Whether to flatten results into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Student
	#' @return A list of Alerts
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	listAlerts <- function(searchConditionsList = NULL, AlertID = F, Information = F, StudentID = F, StartDate = F, EndDate = F, IsActive = F, IsCritical = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, fieldPaths = NULL, searchConditionsGroupType = "And", searchSortFieldNamesList = NULL, searchSortFieldNamesDescendingList = NULL, entityId = 1, query = NULL, page = 1, pageSize = 100000, flatten = T, returnResponse = F){

		params <- as.list(environment())

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		listSkyObjects(module = "Student", objectName = "Alert", searchFields = searchFields %>% append(fieldPaths), page = page, pageSize = pageSize, SearchConditionsList = searchConditionsList, SearchConditionsGroupType = searchConditionsGroupType, SearchSortFieldNamesList = searchSortFieldNamesList, SearchSortFieldNamesDescendingList = searchSortFieldNamesDescendingList, entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' Get an Alert
	#'
	#' This function returns a dataframe or json object of an Alert
	#' @param AlertID The ID of the Alert to return.
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given Alert. Defaults to FALSE for all return fields which, for convenience, returns all fields for the Alert.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object listed as text. Run \code{\link{getSchemaForObjects}}('Alert') to get more field paths.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param query Queries to be added to the request. Ex. list(SchoolYearID = 1). Defaults to NULL.
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Student
	#' @return A dataframe or of Alert
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	getAlert <- function(AlertID, Information = F, StudentID = F, StartDate = F, EndDate = F, IsActive = F, IsCritical = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, fieldPaths = NULL, entityId = 1, query = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment()) %>% purrr::keep(names(.) != "AlertID")

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		getSkyObject(module = "Student", objectName = "Alert", objectId = AlertID, searchFields = searchFields, entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' Delete an Alert
	#'
	#' This function deletes an Alert
	#' @param AlertID The ID of the Alert to delete
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param query Queries to be added to the request. Ex. list(SchoolYearID = 1). Defaults to NULL.
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Student
	#' @return The AlertID of the deleted Alert.
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	deleteAlert <- function(AlertID, ignoreWarnings = F, entityId = 1, query = NULL, flatten = T, returnResponse = F){

		deleteSkyObject(module = "Student", objectName = "Alert", objectId = AlertID, ignoreWarnings = ignoreWarnings, entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' Create an Alert
	#'
	#' This function creates an Alert
	#' @param fieldNames The field values to give the created Alert. Each defaults to NULL.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param query Queries to be added to the request. Ex. list(SchoolYearID = 1). Defaults to NULL.
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Student
	#' @return A newly created Alert
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	createAlert <- function(Information = NULL, StudentID = NULL, StartDate = NULL, EndDate = NULL, IsCritical = NULL, entityId = 1, query = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment())

		body <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper()) %>% purrr::compact()

		createSkyObject(module = "Student", objectName = "Alert", body = list(DataObject = body), searchFields = append("AlertID", body %>% names()), entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' Modify an Alert
	#'
	#' This function modifies an Alert
	#' @param fieldNames The field values to give the modified Alert. Each defaults to NULL.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param query Queries to be added to the request. Ex. list(SchoolYearID = 1). Defaults to NULL.
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Student
	#' @return The modified Alert
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	modifyAlert <- function(AlertID, Information = NULL, StudentID = NULL, StartDate = NULL, EndDate = NULL, IsCritical = NULL, entityId = 1, query = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment())

		body <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper()) %>% purrr::compact()

		modifySkyObject(module = "Student", objectName = "Alert", objectId = AlertID, body = list(DataObject = body), searchFields = append("AlertID", body %>% names()), entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' List TempStudentErrors
	#'
	#' This function returns a dataframe or json object of TempStudentErrors
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given TempStudentErrors. Defaults to FALSE for all return fields which, for convenience, returns all fields for the TempStudentErrors.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object listed as text. Run \code{\link{getSchemaForObjects}}('TempStudentError') to get more field paths.
	#' @param searchConditionsList A list of search conditions to filter results which are joined by the searchConditionsGroupType. Of the form {FieldName} {ConditionType} {SearchCondition}. For example, c('StudentID LessEqual 500', 'LastName Like Ander\%'). Run \code{\link{listSearchConditionTypes}} for a list of ConditionTypes. Defaults to NULL (unfiltered).
	#' @param searchConditionsGroupType The conjunction which joins multiple searchConditions in the searchConditionsList. Either 'Or' or 'And'. Defaults to 'And'.
	#' @param searchSortFieldNamesList The list of fields sort results by. Defaults to NULL (unsorted).
	#' @param searchSortFieldNamesDescendingList A list of T/F values corresponding to whether to sort each field in searchSortFieldNamesList in descending order. Defaults to F for each FieldName in searchSortFieldNamesList.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param query Queries to be added to the request. Ex. list(SchoolYearID = 1). Defaults to NULL.
	#' @param page Results are paginated. The page of results to return. Default is 1.
	#' @param pageSize Results are paginated. The number of records per page to return. Default is 100,000 (essentially all records for most objects).
	#' @param flatten Whether to flatten results into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Student
	#' @return A list of TempStudentErrors
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	listTempStudentErrors <- function(searchConditionsList = NULL, TempStudentErrorID = F, StudentID = F, FullStudentNameLFM = F, StudentNumber = F, GradYear = F, GradeLevelCode = F, StudentTypeCode = F, Note = F, LockerNumber = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, GraduationRequirementYear = F, ErrorCount = F, StudentTypeDescription = F, IsCurrentActive = F, GenderCode = F, fieldPaths = NULL, searchConditionsGroupType = "And", searchSortFieldNamesList = NULL, searchSortFieldNamesDescendingList = NULL, entityId = 1, query = NULL, page = 1, pageSize = 100000, flatten = T, returnResponse = F){

		params <- as.list(environment())

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		listSkyObjects(module = "Student", objectName = "TempStudentError", searchFields = searchFields %>% append(fieldPaths), page = page, pageSize = pageSize, SearchConditionsList = searchConditionsList, SearchConditionsGroupType = searchConditionsGroupType, SearchSortFieldNamesList = searchSortFieldNamesList, SearchSortFieldNamesDescendingList = searchSortFieldNamesDescendingList, entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' Get a TempStudentError
	#'
	#' This function returns a dataframe or json object of a TempStudentError
	#' @param TempStudentErrorID The ID of the TempStudentError to return.
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given TempStudentError. Defaults to FALSE for all return fields which, for convenience, returns all fields for the TempStudentError.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object listed as text. Run \code{\link{getSchemaForObjects}}('TempStudentError') to get more field paths.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param query Queries to be added to the request. Ex. list(SchoolYearID = 1). Defaults to NULL.
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Student
	#' @return A dataframe or of TempStudentError
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	getTempStudentError <- function(TempStudentErrorID, StudentID = F, FullStudentNameLFM = F, StudentNumber = F, GradYear = F, GradeLevelCode = F, StudentTypeCode = F, Note = F, LockerNumber = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, GraduationRequirementYear = F, ErrorCount = F, StudentTypeDescription = F, IsCurrentActive = F, GenderCode = F, fieldPaths = NULL, entityId = 1, query = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment()) %>% purrr::keep(names(.) != "TempStudentErrorID")

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		getSkyObject(module = "Student", objectName = "TempStudentError", objectId = TempStudentErrorID, searchFields = searchFields, entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' Delete a TempStudentError
	#'
	#' This function deletes a TempStudentError
	#' @param TempStudentErrorID The ID of the TempStudentError to delete
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param query Queries to be added to the request. Ex. list(SchoolYearID = 1). Defaults to NULL.
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Student
	#' @return The TempStudentErrorID of the deleted TempStudentError.
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	deleteTempStudentError <- function(TempStudentErrorID, ignoreWarnings = F, entityId = 1, query = NULL, flatten = T, returnResponse = F){

		deleteSkyObject(module = "Student", objectName = "TempStudentError", objectId = TempStudentErrorID, ignoreWarnings = ignoreWarnings, entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' Create a TempStudentError
	#'
	#' This function creates a TempStudentError
	#' @param fieldNames The field values to give the created TempStudentError. Each defaults to NULL.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param query Queries to be added to the request. Ex. list(SchoolYearID = 1). Defaults to NULL.
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Student
	#' @return A newly created TempStudentError
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	createTempStudentError <- function(StudentID = NULL, FullStudentNameLFM = NULL, StudentNumber = NULL, GradYear = NULL, GradeLevelCode = NULL, StudentTypeCode = NULL, Note = NULL, LockerNumber = NULL, GraduationRequirementYear = NULL, ErrorCount = NULL, StudentTypeDescription = NULL, IsCurrentActive = NULL, GenderCode = NULL, entityId = 1, query = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment())

		body <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper()) %>% purrr::compact()

		createSkyObject(module = "Student", objectName = "TempStudentError", body = list(DataObject = body), searchFields = append("TempStudentErrorID", body %>% names()), entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' Modify a TempStudentError
	#'
	#' This function modifies a TempStudentError
	#' @param fieldNames The field values to give the modified TempStudentError. Each defaults to NULL.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param query Queries to be added to the request. Ex. list(SchoolYearID = 1). Defaults to NULL.
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Student
	#' @return The modified TempStudentError
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	modifyTempStudentError <- function(TempStudentErrorID, StudentID = NULL, FullStudentNameLFM = NULL, StudentNumber = NULL, GradYear = NULL, GradeLevelCode = NULL, StudentTypeCode = NULL, Note = NULL, LockerNumber = NULL, GraduationRequirementYear = NULL, ErrorCount = NULL, StudentTypeDescription = NULL, IsCurrentActive = NULL, GenderCode = NULL, entityId = 1, query = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment())

		body <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper()) %>% purrr::compact()

		modifySkyObject(module = "Student", objectName = "TempStudentError", objectId = TempStudentErrorID, body = list(DataObject = body), searchFields = append("TempStudentErrorID", body %>% names()), entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' List StudentCustomCodes
	#'
	#' This function returns a dataframe or json object of StudentCustomCodes
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given StudentCustomCodes. Defaults to FALSE for all return fields which, for convenience, returns all fields for the StudentCustomCodes.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object listed as text. Run \code{\link{getSchemaForObjects}}('StudentCustomCode') to get more field paths.
	#' @param searchConditionsList A list of search conditions to filter results which are joined by the searchConditionsGroupType. Of the form {FieldName} {ConditionType} {SearchCondition}. For example, c('StudentID LessEqual 500', 'LastName Like Ander\%'). Run \code{\link{listSearchConditionTypes}} for a list of ConditionTypes. Defaults to NULL (unfiltered).
	#' @param searchConditionsGroupType The conjunction which joins multiple searchConditions in the searchConditionsList. Either 'Or' or 'And'. Defaults to 'And'.
	#' @param searchSortFieldNamesList The list of fields sort results by. Defaults to NULL (unsorted).
	#' @param searchSortFieldNamesDescendingList A list of T/F values corresponding to whether to sort each field in searchSortFieldNamesList in descending order. Defaults to F for each FieldName in searchSortFieldNamesList.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param query Queries to be added to the request. Ex. list(SchoolYearID = 1). Defaults to NULL.
	#' @param page Results are paginated. The page of results to return. Default is 1.
	#' @param pageSize Results are paginated. The number of records per page to return. Default is 100,000 (essentially all records for most objects).
	#' @param flatten Whether to flatten results into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Student
	#' @return A list of StudentCustomCodes
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	listStudentCustomCodes <- function(searchConditionsList = NULL, StudentCustomCodeID = F, CustomCodeID = F, StudentID = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, fieldPaths = NULL, searchConditionsGroupType = "And", searchSortFieldNamesList = NULL, searchSortFieldNamesDescendingList = NULL, entityId = 1, query = NULL, page = 1, pageSize = 100000, flatten = T, returnResponse = F){

		params <- as.list(environment())

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		listSkyObjects(module = "Student", objectName = "StudentCustomCode", searchFields = searchFields %>% append(fieldPaths), page = page, pageSize = pageSize, SearchConditionsList = searchConditionsList, SearchConditionsGroupType = searchConditionsGroupType, SearchSortFieldNamesList = searchSortFieldNamesList, SearchSortFieldNamesDescendingList = searchSortFieldNamesDescendingList, entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' Get a StudentCustomCode
	#'
	#' This function returns a dataframe or json object of a StudentCustomCode
	#' @param StudentCustomCodeID The ID of the StudentCustomCode to return.
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given StudentCustomCode. Defaults to FALSE for all return fields which, for convenience, returns all fields for the StudentCustomCode.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object listed as text. Run \code{\link{getSchemaForObjects}}('StudentCustomCode') to get more field paths.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param query Queries to be added to the request. Ex. list(SchoolYearID = 1). Defaults to NULL.
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Student
	#' @return A dataframe or of StudentCustomCode
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	getStudentCustomCode <- function(StudentCustomCodeID, CustomCodeID = F, StudentID = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, fieldPaths = NULL, entityId = 1, query = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment()) %>% purrr::keep(names(.) != "StudentCustomCodeID")

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		getSkyObject(module = "Student", objectName = "StudentCustomCode", objectId = StudentCustomCodeID, searchFields = searchFields, entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' Delete a StudentCustomCode
	#'
	#' This function deletes a StudentCustomCode
	#' @param StudentCustomCodeID The ID of the StudentCustomCode to delete
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param query Queries to be added to the request. Ex. list(SchoolYearID = 1). Defaults to NULL.
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Student
	#' @return The StudentCustomCodeID of the deleted StudentCustomCode.
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	deleteStudentCustomCode <- function(StudentCustomCodeID, ignoreWarnings = F, entityId = 1, query = NULL, flatten = T, returnResponse = F){

		deleteSkyObject(module = "Student", objectName = "StudentCustomCode", objectId = StudentCustomCodeID, ignoreWarnings = ignoreWarnings, entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' Create a StudentCustomCode
	#'
	#' This function creates a StudentCustomCode
	#' @param fieldNames The field values to give the created StudentCustomCode. Each defaults to NULL.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param query Queries to be added to the request. Ex. list(SchoolYearID = 1). Defaults to NULL.
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Student
	#' @return A newly created StudentCustomCode
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	createStudentCustomCode <- function(CustomCodeID = NULL, StudentID = NULL, entityId = 1, query = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment())

		body <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper()) %>% purrr::compact()

		createSkyObject(module = "Student", objectName = "StudentCustomCode", body = list(DataObject = body), searchFields = append("StudentCustomCodeID", body %>% names()), entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' Modify a StudentCustomCode
	#'
	#' This function modifies a StudentCustomCode
	#' @param fieldNames The field values to give the modified StudentCustomCode. Each defaults to NULL.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param query Queries to be added to the request. Ex. list(SchoolYearID = 1). Defaults to NULL.
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Student
	#' @return The modified StudentCustomCode
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	modifyStudentCustomCode <- function(StudentCustomCodeID, CustomCodeID = NULL, StudentID = NULL, entityId = 1, query = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment())

		body <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper()) %>% purrr::compact()

		modifySkyObject(module = "Student", objectName = "StudentCustomCode", objectId = StudentCustomCodeID, body = list(DataObject = body), searchFields = append("StudentCustomCodeID", body %>% names()), entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' List CustomCodes
	#'
	#' This function returns a dataframe or json object of CustomCodes
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given CustomCodes. Defaults to FALSE for all return fields which, for convenience, returns all fields for the CustomCodes.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object listed as text. Run \code{\link{getSchemaForObjects}}('CustomCode') to get more field paths.
	#' @param searchConditionsList A list of search conditions to filter results which are joined by the searchConditionsGroupType. Of the form {FieldName} {ConditionType} {SearchCondition}. For example, c('StudentID LessEqual 500', 'LastName Like Ander\%'). Run \code{\link{listSearchConditionTypes}} for a list of ConditionTypes. Defaults to NULL (unfiltered).
	#' @param searchConditionsGroupType The conjunction which joins multiple searchConditions in the searchConditionsList. Either 'Or' or 'And'. Defaults to 'And'.
	#' @param searchSortFieldNamesList The list of fields sort results by. Defaults to NULL (unsorted).
	#' @param searchSortFieldNamesDescendingList A list of T/F values corresponding to whether to sort each field in searchSortFieldNamesList in descending order. Defaults to F for each FieldName in searchSortFieldNamesList.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param query Queries to be added to the request. Ex. list(SchoolYearID = 1). Defaults to NULL.
	#' @param page Results are paginated. The page of results to return. Default is 1.
	#' @param pageSize Results are paginated. The number of records per page to return. Default is 100,000 (essentially all records for most objects).
	#' @param flatten Whether to flatten results into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Student
	#' @return A list of CustomCodes
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	listCustomCodes <- function(searchConditionsList = NULL, CustomCodeID = F, DistrictGroupKey = F, CustomCodeTypeID = F, Code = F, Description = F, CodeDescription = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, fieldPaths = NULL, searchConditionsGroupType = "And", searchSortFieldNamesList = NULL, searchSortFieldNamesDescendingList = NULL, entityId = 1, query = NULL, page = 1, pageSize = 100000, flatten = T, returnResponse = F){

		params <- as.list(environment())

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		listSkyObjects(module = "Student", objectName = "CustomCode", searchFields = searchFields %>% append(fieldPaths), page = page, pageSize = pageSize, SearchConditionsList = searchConditionsList, SearchConditionsGroupType = searchConditionsGroupType, SearchSortFieldNamesList = searchSortFieldNamesList, SearchSortFieldNamesDescendingList = searchSortFieldNamesDescendingList, entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' Get a CustomCode
	#'
	#' This function returns a dataframe or json object of a CustomCode
	#' @param CustomCodeID The ID of the CustomCode to return.
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given CustomCode. Defaults to FALSE for all return fields which, for convenience, returns all fields for the CustomCode.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object listed as text. Run \code{\link{getSchemaForObjects}}('CustomCode') to get more field paths.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param query Queries to be added to the request. Ex. list(SchoolYearID = 1). Defaults to NULL.
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Student
	#' @return A dataframe or of CustomCode
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	getCustomCode <- function(CustomCodeID, DistrictGroupKey = F, CustomCodeTypeID = F, Code = F, Description = F, CodeDescription = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, fieldPaths = NULL, entityId = 1, query = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment()) %>% purrr::keep(names(.) != "CustomCodeID")

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		getSkyObject(module = "Student", objectName = "CustomCode", objectId = CustomCodeID, searchFields = searchFields, entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' Delete a CustomCode
	#'
	#' This function deletes a CustomCode
	#' @param CustomCodeID The ID of the CustomCode to delete
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param query Queries to be added to the request. Ex. list(SchoolYearID = 1). Defaults to NULL.
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Student
	#' @return The CustomCodeID of the deleted CustomCode.
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	deleteCustomCode <- function(CustomCodeID, ignoreWarnings = F, entityId = 1, query = NULL, flatten = T, returnResponse = F){

		deleteSkyObject(module = "Student", objectName = "CustomCode", objectId = CustomCodeID, ignoreWarnings = ignoreWarnings, entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' Create a CustomCode
	#'
	#' This function creates a CustomCode
	#' @param fieldNames The field values to give the created CustomCode. Each defaults to NULL.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param query Queries to be added to the request. Ex. list(SchoolYearID = 1). Defaults to NULL.
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Student
	#' @return A newly created CustomCode
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	createCustomCode <- function(DistrictGroupKey = NULL, CustomCodeTypeID = NULL, Code = NULL, Description = NULL, entityId = 1, query = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment())

		body <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper()) %>% purrr::compact()

		createSkyObject(module = "Student", objectName = "CustomCode", body = list(DataObject = body), searchFields = append("CustomCodeID", body %>% names()), entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' Modify a CustomCode
	#'
	#' This function modifies a CustomCode
	#' @param fieldNames The field values to give the modified CustomCode. Each defaults to NULL.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param query Queries to be added to the request. Ex. list(SchoolYearID = 1). Defaults to NULL.
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Student
	#' @return The modified CustomCode
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	modifyCustomCode <- function(CustomCodeID, DistrictGroupKey = NULL, CustomCodeTypeID = NULL, Code = NULL, Description = NULL, entityId = 1, query = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment())

		body <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper()) %>% purrr::compact()

		modifySkyObject(module = "Student", objectName = "CustomCode", objectId = CustomCodeID, body = list(DataObject = body), searchFields = append("CustomCodeID", body %>% names()), entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' List CustomCodeTypes
	#'
	#' This function returns a dataframe or json object of CustomCodeTypes
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given CustomCodeTypes. Defaults to FALSE for all return fields which, for convenience, returns all fields for the CustomCodeTypes.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object listed as text. Run \code{\link{getSchemaForObjects}}('CustomCodeType') to get more field paths.
	#' @param searchConditionsList A list of search conditions to filter results which are joined by the searchConditionsGroupType. Of the form {FieldName} {ConditionType} {SearchCondition}. For example, c('StudentID LessEqual 500', 'LastName Like Ander\%'). Run \code{\link{listSearchConditionTypes}} for a list of ConditionTypes. Defaults to NULL (unfiltered).
	#' @param searchConditionsGroupType The conjunction which joins multiple searchConditions in the searchConditionsList. Either 'Or' or 'And'. Defaults to 'And'.
	#' @param searchSortFieldNamesList The list of fields sort results by. Defaults to NULL (unsorted).
	#' @param searchSortFieldNamesDescendingList A list of T/F values corresponding to whether to sort each field in searchSortFieldNamesList in descending order. Defaults to F for each FieldName in searchSortFieldNamesList.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param query Queries to be added to the request. Ex. list(SchoolYearID = 1). Defaults to NULL.
	#' @param page Results are paginated. The page of results to return. Default is 1.
	#' @param pageSize Results are paginated. The number of records per page to return. Default is 100,000 (essentially all records for most objects).
	#' @param flatten Whether to flatten results into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Student
	#' @return A list of CustomCodeTypes
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	listCustomCodeTypes <- function(searchConditionsList = NULL, CustomCodeTypeID = F, DistrictID = F, DistrictGroupKey = F, Code = F, Description = F, CodeDescription = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, fieldPaths = NULL, searchConditionsGroupType = "And", searchSortFieldNamesList = NULL, searchSortFieldNamesDescendingList = NULL, entityId = 1, query = NULL, page = 1, pageSize = 100000, flatten = T, returnResponse = F){

		params <- as.list(environment())

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		listSkyObjects(module = "Student", objectName = "CustomCodeType", searchFields = searchFields %>% append(fieldPaths), page = page, pageSize = pageSize, SearchConditionsList = searchConditionsList, SearchConditionsGroupType = searchConditionsGroupType, SearchSortFieldNamesList = searchSortFieldNamesList, SearchSortFieldNamesDescendingList = searchSortFieldNamesDescendingList, entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' Get a CustomCodeType
	#'
	#' This function returns a dataframe or json object of a CustomCodeType
	#' @param CustomCodeTypeID The ID of the CustomCodeType to return.
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given CustomCodeType. Defaults to FALSE for all return fields which, for convenience, returns all fields for the CustomCodeType.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object listed as text. Run \code{\link{getSchemaForObjects}}('CustomCodeType') to get more field paths.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param query Queries to be added to the request. Ex. list(SchoolYearID = 1). Defaults to NULL.
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Student
	#' @return A dataframe or of CustomCodeType
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	getCustomCodeType <- function(CustomCodeTypeID, DistrictID = F, DistrictGroupKey = F, Code = F, Description = F, CodeDescription = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, fieldPaths = NULL, entityId = 1, query = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment()) %>% purrr::keep(names(.) != "CustomCodeTypeID")

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		getSkyObject(module = "Student", objectName = "CustomCodeType", objectId = CustomCodeTypeID, searchFields = searchFields, entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' Delete a CustomCodeType
	#'
	#' This function deletes a CustomCodeType
	#' @param CustomCodeTypeID The ID of the CustomCodeType to delete
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param query Queries to be added to the request. Ex. list(SchoolYearID = 1). Defaults to NULL.
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Student
	#' @return The CustomCodeTypeID of the deleted CustomCodeType.
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	deleteCustomCodeType <- function(CustomCodeTypeID, ignoreWarnings = F, entityId = 1, query = NULL, flatten = T, returnResponse = F){

		deleteSkyObject(module = "Student", objectName = "CustomCodeType", objectId = CustomCodeTypeID, ignoreWarnings = ignoreWarnings, entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' Create a CustomCodeType
	#'
	#' This function creates a CustomCodeType
	#' @param fieldNames The field values to give the created CustomCodeType. Each defaults to NULL.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param query Queries to be added to the request. Ex. list(SchoolYearID = 1). Defaults to NULL.
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Student
	#' @return A newly created CustomCodeType
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	createCustomCodeType <- function(DistrictID = NULL, DistrictGroupKey = NULL, Code = NULL, Description = NULL, entityId = 1, query = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment())

		body <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper()) %>% purrr::compact()

		createSkyObject(module = "Student", objectName = "CustomCodeType", body = list(DataObject = body), searchFields = append("CustomCodeTypeID", body %>% names()), entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' Modify a CustomCodeType
	#'
	#' This function modifies a CustomCodeType
	#' @param fieldNames The field values to give the modified CustomCodeType. Each defaults to NULL.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param query Queries to be added to the request. Ex. list(SchoolYearID = 1). Defaults to NULL.
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Student
	#' @return The modified CustomCodeType
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	modifyCustomCodeType <- function(CustomCodeTypeID, DistrictID = NULL, DistrictGroupKey = NULL, Code = NULL, Description = NULL, entityId = 1, query = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment())

		body <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper()) %>% purrr::compact()

		modifySkyObject(module = "Student", objectName = "CustomCodeType", objectId = CustomCodeTypeID, body = list(DataObject = body), searchFields = append("CustomCodeTypeID", body %>% names()), entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' List FeederSchools
	#'
	#' This function returns a dataframe or json object of FeederSchools
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given FeederSchools. Defaults to FALSE for all return fields which, for convenience, returns all fields for the FeederSchools.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object listed as text. Run \code{\link{getSchemaForObjects}}('FeederSchool') to get more field paths.
	#' @param searchConditionsList A list of search conditions to filter results which are joined by the searchConditionsGroupType. Of the form {FieldName} {ConditionType} {SearchCondition}. For example, c('StudentID LessEqual 500', 'LastName Like Ander\%'). Run \code{\link{listSearchConditionTypes}} for a list of ConditionTypes. Defaults to NULL (unfiltered).
	#' @param searchConditionsGroupType The conjunction which joins multiple searchConditions in the searchConditionsList. Either 'Or' or 'And'. Defaults to 'And'.
	#' @param searchSortFieldNamesList The list of fields sort results by. Defaults to NULL (unsorted).
	#' @param searchSortFieldNamesDescendingList A list of T/F values corresponding to whether to sort each field in searchSortFieldNamesList in descending order. Defaults to F for each FieldName in searchSortFieldNamesList.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param query Queries to be added to the request. Ex. list(SchoolYearID = 1). Defaults to NULL.
	#' @param page Results are paginated. The page of results to return. Default is 1.
	#' @param pageSize Results are paginated. The number of records per page to return. Default is 100,000 (essentially all records for most objects).
	#' @param flatten Whether to flatten results into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Student
	#' @return A list of FeederSchools
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	listFeederSchools <- function(searchConditionsList = NULL, FeederSchoolID = F, DistrictID = F, Code = F, Description = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, fieldPaths = NULL, searchConditionsGroupType = "And", searchSortFieldNamesList = NULL, searchSortFieldNamesDescendingList = NULL, entityId = 1, query = NULL, page = 1, pageSize = 100000, flatten = T, returnResponse = F){

		params <- as.list(environment())

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		listSkyObjects(module = "Student", objectName = "FeederSchool", searchFields = searchFields %>% append(fieldPaths), page = page, pageSize = pageSize, SearchConditionsList = searchConditionsList, SearchConditionsGroupType = searchConditionsGroupType, SearchSortFieldNamesList = searchSortFieldNamesList, SearchSortFieldNamesDescendingList = searchSortFieldNamesDescendingList, entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' Get a FeederSchool
	#'
	#' This function returns a dataframe or json object of a FeederSchool
	#' @param FeederSchoolID The ID of the FeederSchool to return.
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given FeederSchool. Defaults to FALSE for all return fields which, for convenience, returns all fields for the FeederSchool.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object listed as text. Run \code{\link{getSchemaForObjects}}('FeederSchool') to get more field paths.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param query Queries to be added to the request. Ex. list(SchoolYearID = 1). Defaults to NULL.
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Student
	#' @return A dataframe or of FeederSchool
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	getFeederSchool <- function(FeederSchoolID, DistrictID = F, Code = F, Description = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, fieldPaths = NULL, entityId = 1, query = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment()) %>% purrr::keep(names(.) != "FeederSchoolID")

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		getSkyObject(module = "Student", objectName = "FeederSchool", objectId = FeederSchoolID, searchFields = searchFields, entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' Delete a FeederSchool
	#'
	#' This function deletes a FeederSchool
	#' @param FeederSchoolID The ID of the FeederSchool to delete
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param query Queries to be added to the request. Ex. list(SchoolYearID = 1). Defaults to NULL.
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Student
	#' @return The FeederSchoolID of the deleted FeederSchool.
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	deleteFeederSchool <- function(FeederSchoolID, ignoreWarnings = F, entityId = 1, query = NULL, flatten = T, returnResponse = F){

		deleteSkyObject(module = "Student", objectName = "FeederSchool", objectId = FeederSchoolID, ignoreWarnings = ignoreWarnings, entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' Create a FeederSchool
	#'
	#' This function creates a FeederSchool
	#' @param fieldNames The field values to give the created FeederSchool. Each defaults to NULL.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param query Queries to be added to the request. Ex. list(SchoolYearID = 1). Defaults to NULL.
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Student
	#' @return A newly created FeederSchool
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	createFeederSchool <- function(DistrictID = NULL, Code = NULL, Description = NULL, entityId = 1, query = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment())

		body <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper()) %>% purrr::compact()

		createSkyObject(module = "Student", objectName = "FeederSchool", body = list(DataObject = body), searchFields = append("FeederSchoolID", body %>% names()), entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' Modify a FeederSchool
	#'
	#' This function modifies a FeederSchool
	#' @param fieldNames The field values to give the modified FeederSchool. Each defaults to NULL.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param query Queries to be added to the request. Ex. list(SchoolYearID = 1). Defaults to NULL.
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Student
	#' @return The modified FeederSchool
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	modifyFeederSchool <- function(FeederSchoolID, DistrictID = NULL, Code = NULL, Description = NULL, entityId = 1, query = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment())

		body <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper()) %>% purrr::compact()

		modifySkyObject(module = "Student", objectName = "FeederSchool", objectId = FeederSchoolID, body = list(DataObject = body), searchFields = append("FeederSchoolID", body %>% names()), entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' List TempStudentAssignedLockCombinationRecords
	#'
	#' This function returns a dataframe or json object of TempStudentAssignedLockCombinationRecords
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given TempStudentAssignedLockCombinationRecords. Defaults to FALSE for all return fields which, for convenience, returns all fields for the TempStudentAssignedLockCombinationRecords.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object listed as text. Run \code{\link{getSchemaForObjects}}('TempStudentAssignedLockCombinationRecord') to get more field paths.
	#' @param searchConditionsList A list of search conditions to filter results which are joined by the searchConditionsGroupType. Of the form {FieldName} {ConditionType} {SearchCondition}. For example, c('StudentID LessEqual 500', 'LastName Like Ander\%'). Run \code{\link{listSearchConditionTypes}} for a list of ConditionTypes. Defaults to NULL (unfiltered).
	#' @param searchConditionsGroupType The conjunction which joins multiple searchConditions in the searchConditionsList. Either 'Or' or 'And'. Defaults to 'And'.
	#' @param searchSortFieldNamesList The list of fields sort results by. Defaults to NULL (unsorted).
	#' @param searchSortFieldNamesDescendingList A list of T/F values corresponding to whether to sort each field in searchSortFieldNamesList in descending order. Defaults to F for each FieldName in searchSortFieldNamesList.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param query Queries to be added to the request. Ex. list(SchoolYearID = 1). Defaults to NULL.
	#' @param page Results are paginated. The page of results to return. Default is 1.
	#' @param pageSize Results are paginated. The number of records per page to return. Default is 100,000 (essentially all records for most objects).
	#' @param flatten Whether to flatten results into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Student
	#' @return A list of TempStudentAssignedLockCombinationRecords
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	listTempStudentAssignedLockCombinationRecords <- function(searchConditionsList = NULL, TempStudentAssignedLockCombinationRecordID = F, StudentID = F, LockID = F, LockerID = F, FullName = F, StudentNumber = F, Gender = F, BirthDate = F, Age = F, GradYear = F, Grade = F, Building = F, LockerArea = F, LockerNumber = F, OldLockCombinationID = F, OldCombination = F, OldCombinationNumber = F, NewLockCombinationID = F, NewCombination = F, NewCombinationNumber = F, UnoccupiedLockersOnly = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, fieldPaths = NULL, searchConditionsGroupType = "And", searchSortFieldNamesList = NULL, searchSortFieldNamesDescendingList = NULL, entityId = 1, query = NULL, page = 1, pageSize = 100000, flatten = T, returnResponse = F){

		params <- as.list(environment())

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		listSkyObjects(module = "Student", objectName = "TempStudentAssignedLockCombinationRecord", searchFields = searchFields %>% append(fieldPaths), page = page, pageSize = pageSize, SearchConditionsList = searchConditionsList, SearchConditionsGroupType = searchConditionsGroupType, SearchSortFieldNamesList = searchSortFieldNamesList, SearchSortFieldNamesDescendingList = searchSortFieldNamesDescendingList, entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' Get a TempStudentAssignedLockCombinationRecord
	#'
	#' This function returns a dataframe or json object of a TempStudentAssignedLockCombinationRecord
	#' @param TempStudentAssignedLockCombinationRecordID The ID of the TempStudentAssignedLockCombinationRecord to return.
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given TempStudentAssignedLockCombinationRecord. Defaults to FALSE for all return fields which, for convenience, returns all fields for the TempStudentAssignedLockCombinationRecord.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object listed as text. Run \code{\link{getSchemaForObjects}}('TempStudentAssignedLockCombinationRecord') to get more field paths.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param query Queries to be added to the request. Ex. list(SchoolYearID = 1). Defaults to NULL.
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Student
	#' @return A dataframe or of TempStudentAssignedLockCombinationRecord
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	getTempStudentAssignedLockCombinationRecord <- function(TempStudentAssignedLockCombinationRecordID, StudentID = F, LockID = F, LockerID = F, FullName = F, StudentNumber = F, Gender = F, BirthDate = F, Age = F, GradYear = F, Grade = F, Building = F, LockerArea = F, LockerNumber = F, OldLockCombinationID = F, OldCombination = F, OldCombinationNumber = F, NewLockCombinationID = F, NewCombination = F, NewCombinationNumber = F, UnoccupiedLockersOnly = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, fieldPaths = NULL, entityId = 1, query = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment()) %>% purrr::keep(names(.) != "TempStudentAssignedLockCombinationRecordID")

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		getSkyObject(module = "Student", objectName = "TempStudentAssignedLockCombinationRecord", objectId = TempStudentAssignedLockCombinationRecordID, searchFields = searchFields, entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' Delete a TempStudentAssignedLockCombinationRecord
	#'
	#' This function deletes a TempStudentAssignedLockCombinationRecord
	#' @param TempStudentAssignedLockCombinationRecordID The ID of the TempStudentAssignedLockCombinationRecord to delete
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param query Queries to be added to the request. Ex. list(SchoolYearID = 1). Defaults to NULL.
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Student
	#' @return The TempStudentAssignedLockCombinationRecordID of the deleted TempStudentAssignedLockCombinationRecord.
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	deleteTempStudentAssignedLockCombinationRecord <- function(TempStudentAssignedLockCombinationRecordID, ignoreWarnings = F, entityId = 1, query = NULL, flatten = T, returnResponse = F){

		deleteSkyObject(module = "Student", objectName = "TempStudentAssignedLockCombinationRecord", objectId = TempStudentAssignedLockCombinationRecordID, ignoreWarnings = ignoreWarnings, entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' Create a TempStudentAssignedLockCombinationRecord
	#'
	#' This function creates a TempStudentAssignedLockCombinationRecord
	#' @param fieldNames The field values to give the created TempStudentAssignedLockCombinationRecord. Each defaults to NULL.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param query Queries to be added to the request. Ex. list(SchoolYearID = 1). Defaults to NULL.
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Student
	#' @return A newly created TempStudentAssignedLockCombinationRecord
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	createTempStudentAssignedLockCombinationRecord <- function(StudentID = NULL, LockID = NULL, LockerID = NULL, FullName = NULL, StudentNumber = NULL, Gender = NULL, BirthDate = NULL, Age = NULL, GradYear = NULL, Grade = NULL, Building = NULL, LockerArea = NULL, LockerNumber = NULL, OldLockCombinationID = NULL, OldCombination = NULL, OldCombinationNumber = NULL, NewLockCombinationID = NULL, NewCombination = NULL, NewCombinationNumber = NULL, UnoccupiedLockersOnly = NULL, entityId = 1, query = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment())

		body <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper()) %>% purrr::compact()

		createSkyObject(module = "Student", objectName = "TempStudentAssignedLockCombinationRecord", body = list(DataObject = body), searchFields = append("TempStudentAssignedLockCombinationRecordID", body %>% names()), entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' Modify a TempStudentAssignedLockCombinationRecord
	#'
	#' This function modifies a TempStudentAssignedLockCombinationRecord
	#' @param fieldNames The field values to give the modified TempStudentAssignedLockCombinationRecord. Each defaults to NULL.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param query Queries to be added to the request. Ex. list(SchoolYearID = 1). Defaults to NULL.
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Student
	#' @return The modified TempStudentAssignedLockCombinationRecord
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	modifyTempStudentAssignedLockCombinationRecord <- function(TempStudentAssignedLockCombinationRecordID, StudentID = NULL, LockID = NULL, LockerID = NULL, FullName = NULL, StudentNumber = NULL, Gender = NULL, BirthDate = NULL, Age = NULL, GradYear = NULL, Grade = NULL, Building = NULL, LockerArea = NULL, LockerNumber = NULL, OldLockCombinationID = NULL, OldCombination = NULL, OldCombinationNumber = NULL, NewLockCombinationID = NULL, NewCombination = NULL, NewCombinationNumber = NULL, UnoccupiedLockersOnly = NULL, entityId = 1, query = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment())

		body <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper()) %>% purrr::compact()

		modifySkyObject(module = "Student", objectName = "TempStudentAssignedLockCombinationRecord", objectId = TempStudentAssignedLockCombinationRecordID, body = list(DataObject = body), searchFields = append("TempStudentAssignedLockCombinationRecordID", body %>% names()), entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' List Locks
	#'
	#' This function returns a dataframe or json object of Locks
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given Locks. Defaults to FALSE for all return fields which, for convenience, returns all fields for the Locks.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object listed as text. Run \code{\link{getSchemaForObjects}}('Lock') to get more field paths.
	#' @param searchConditionsList A list of search conditions to filter results which are joined by the searchConditionsGroupType. Of the form {FieldName} {ConditionType} {SearchCondition}. For example, c('StudentID LessEqual 500', 'LastName Like Ander\%'). Run \code{\link{listSearchConditionTypes}} for a list of ConditionTypes. Defaults to NULL (unfiltered).
	#' @param searchConditionsGroupType The conjunction which joins multiple searchConditions in the searchConditionsList. Either 'Or' or 'And'. Defaults to 'And'.
	#' @param searchSortFieldNamesList The list of fields sort results by. Defaults to NULL (unsorted).
	#' @param searchSortFieldNamesDescendingList A list of T/F values corresponding to whether to sort each field in searchSortFieldNamesList in descending order. Defaults to F for each FieldName in searchSortFieldNamesList.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param query Queries to be added to the request. Ex. list(SchoolYearID = 1). Defaults to NULL.
	#' @param page Results are paginated. The page of results to return. Default is 1.
	#' @param pageSize Results are paginated. The number of records per page to return. Default is 100,000 (essentially all records for most objects).
	#' @param flatten Whether to flatten results into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Student
	#' @return A list of Locks
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	listLocks <- function(searchConditionsList = NULL, LockID = F, SerialNumber = F, OwnedBySchool = F, IsBuiltInLock = F, LockerID = F, StudentID = F, LockMakeID = F, BuildingID = F, RenderRemoveLock = F, IsAvailable = F, IsAttached = F, IsAssigned = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, fieldPaths = NULL, searchConditionsGroupType = "And", searchSortFieldNamesList = NULL, searchSortFieldNamesDescendingList = NULL, entityId = 1, query = NULL, page = 1, pageSize = 100000, flatten = T, returnResponse = F){

		params <- as.list(environment())

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		listSkyObjects(module = "Student", objectName = "Lock", searchFields = searchFields %>% append(fieldPaths), page = page, pageSize = pageSize, SearchConditionsList = searchConditionsList, SearchConditionsGroupType = searchConditionsGroupType, SearchSortFieldNamesList = searchSortFieldNamesList, SearchSortFieldNamesDescendingList = searchSortFieldNamesDescendingList, entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' Get a Lock
	#'
	#' This function returns a dataframe or json object of a Lock
	#' @param LockID The ID of the Lock to return.
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given Lock. Defaults to FALSE for all return fields which, for convenience, returns all fields for the Lock.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object listed as text. Run \code{\link{getSchemaForObjects}}('Lock') to get more field paths.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param query Queries to be added to the request. Ex. list(SchoolYearID = 1). Defaults to NULL.
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Student
	#' @return A dataframe or of Lock
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	getLock <- function(LockID, SerialNumber = F, OwnedBySchool = F, IsBuiltInLock = F, LockerID = F, StudentID = F, LockMakeID = F, BuildingID = F, RenderRemoveLock = F, IsAvailable = F, IsAttached = F, IsAssigned = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, fieldPaths = NULL, entityId = 1, query = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment()) %>% purrr::keep(names(.) != "LockID")

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		getSkyObject(module = "Student", objectName = "Lock", objectId = LockID, searchFields = searchFields, entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' Delete a Lock
	#'
	#' This function deletes a Lock
	#' @param LockID The ID of the Lock to delete
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param query Queries to be added to the request. Ex. list(SchoolYearID = 1). Defaults to NULL.
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Student
	#' @return The LockID of the deleted Lock.
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	deleteLock <- function(LockID, ignoreWarnings = F, entityId = 1, query = NULL, flatten = T, returnResponse = F){

		deleteSkyObject(module = "Student", objectName = "Lock", objectId = LockID, ignoreWarnings = ignoreWarnings, entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' Create a Lock
	#'
	#' This function creates a Lock
	#' @param fieldNames The field values to give the created Lock. Each defaults to NULL.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param query Queries to be added to the request. Ex. list(SchoolYearID = 1). Defaults to NULL.
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Student
	#' @return A newly created Lock
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	createLock <- function(SerialNumber = NULL, OwnedBySchool = NULL, IsBuiltInLock = NULL, LockerID = NULL, StudentID = NULL, LockMakeID = NULL, BuildingID = NULL, entityId = 1, query = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment())

		body <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper()) %>% purrr::compact()

		createSkyObject(module = "Student", objectName = "Lock", body = list(DataObject = body), searchFields = append("LockID", body %>% names()), entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' Modify a Lock
	#'
	#' This function modifies a Lock
	#' @param fieldNames The field values to give the modified Lock. Each defaults to NULL.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param query Queries to be added to the request. Ex. list(SchoolYearID = 1). Defaults to NULL.
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Student
	#' @return The modified Lock
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	modifyLock <- function(LockID, SerialNumber = NULL, OwnedBySchool = NULL, IsBuiltInLock = NULL, LockerID = NULL, StudentID = NULL, LockMakeID = NULL, BuildingID = NULL, entityId = 1, query = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment())

		body <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper()) %>% purrr::compact()

		modifySkyObject(module = "Student", objectName = "Lock", objectId = LockID, body = list(DataObject = body), searchFields = append("LockID", body %>% names()), entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' List LockCombinations
	#'
	#' This function returns a dataframe or json object of LockCombinations
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given LockCombinations. Defaults to FALSE for all return fields which, for convenience, returns all fields for the LockCombinations.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object listed as text. Run \code{\link{getSchemaForObjects}}('LockCombination') to get more field paths.
	#' @param searchConditionsList A list of search conditions to filter results which are joined by the searchConditionsGroupType. Of the form {FieldName} {ConditionType} {SearchCondition}. For example, c('StudentID LessEqual 500', 'LastName Like Ander\%'). Run \code{\link{listSearchConditionTypes}} for a list of ConditionTypes. Defaults to NULL (unfiltered).
	#' @param searchConditionsGroupType The conjunction which joins multiple searchConditions in the searchConditionsList. Either 'Or' or 'And'. Defaults to 'And'.
	#' @param searchSortFieldNamesList The list of fields sort results by. Defaults to NULL (unsorted).
	#' @param searchSortFieldNamesDescendingList A list of T/F values corresponding to whether to sort each field in searchSortFieldNamesList in descending order. Defaults to F for each FieldName in searchSortFieldNamesList.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param query Queries to be added to the request. Ex. list(SchoolYearID = 1). Defaults to NULL.
	#' @param page Results are paginated. The page of results to return. Default is 1.
	#' @param pageSize Results are paginated. The number of records per page to return. Default is 100,000 (essentially all records for most objects).
	#' @param flatten Whether to flatten results into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Student
	#' @return A list of LockCombinations
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	listLockCombinations <- function(searchConditionsList = NULL, LockCombinationID = F, Combination = F, IsCurrentCombination = F, CombinationNumber = F, LockID = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, fieldPaths = NULL, searchConditionsGroupType = "And", searchSortFieldNamesList = NULL, searchSortFieldNamesDescendingList = NULL, entityId = 1, query = NULL, page = 1, pageSize = 100000, flatten = T, returnResponse = F){

		params <- as.list(environment())

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		listSkyObjects(module = "Student", objectName = "LockCombination", searchFields = searchFields %>% append(fieldPaths), page = page, pageSize = pageSize, SearchConditionsList = searchConditionsList, SearchConditionsGroupType = searchConditionsGroupType, SearchSortFieldNamesList = searchSortFieldNamesList, SearchSortFieldNamesDescendingList = searchSortFieldNamesDescendingList, entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' Get a LockCombination
	#'
	#' This function returns a dataframe or json object of a LockCombination
	#' @param LockCombinationID The ID of the LockCombination to return.
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given LockCombination. Defaults to FALSE for all return fields which, for convenience, returns all fields for the LockCombination.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object listed as text. Run \code{\link{getSchemaForObjects}}('LockCombination') to get more field paths.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param query Queries to be added to the request. Ex. list(SchoolYearID = 1). Defaults to NULL.
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Student
	#' @return A dataframe or of LockCombination
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	getLockCombination <- function(LockCombinationID, Combination = F, IsCurrentCombination = F, CombinationNumber = F, LockID = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, fieldPaths = NULL, entityId = 1, query = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment()) %>% purrr::keep(names(.) != "LockCombinationID")

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		getSkyObject(module = "Student", objectName = "LockCombination", objectId = LockCombinationID, searchFields = searchFields, entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' Delete a LockCombination
	#'
	#' This function deletes a LockCombination
	#' @param LockCombinationID The ID of the LockCombination to delete
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param query Queries to be added to the request. Ex. list(SchoolYearID = 1). Defaults to NULL.
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Student
	#' @return The LockCombinationID of the deleted LockCombination.
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	deleteLockCombination <- function(LockCombinationID, ignoreWarnings = F, entityId = 1, query = NULL, flatten = T, returnResponse = F){

		deleteSkyObject(module = "Student", objectName = "LockCombination", objectId = LockCombinationID, ignoreWarnings = ignoreWarnings, entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' Create a LockCombination
	#'
	#' This function creates a LockCombination
	#' @param fieldNames The field values to give the created LockCombination. Each defaults to NULL.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param query Queries to be added to the request. Ex. list(SchoolYearID = 1). Defaults to NULL.
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Student
	#' @return A newly created LockCombination
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	createLockCombination <- function(Combination = NULL, IsCurrentCombination = NULL, CombinationNumber = NULL, LockID = NULL, entityId = 1, query = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment())

		body <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper()) %>% purrr::compact()

		createSkyObject(module = "Student", objectName = "LockCombination", body = list(DataObject = body), searchFields = append("LockCombinationID", body %>% names()), entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' Modify a LockCombination
	#'
	#' This function modifies a LockCombination
	#' @param fieldNames The field values to give the modified LockCombination. Each defaults to NULL.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param query Queries to be added to the request. Ex. list(SchoolYearID = 1). Defaults to NULL.
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Student
	#' @return The modified LockCombination
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	modifyLockCombination <- function(LockCombinationID, Combination = NULL, IsCurrentCombination = NULL, CombinationNumber = NULL, LockID = NULL, entityId = 1, query = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment())

		body <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper()) %>% purrr::compact()

		modifySkyObject(module = "Student", objectName = "LockCombination", objectId = LockCombinationID, body = list(DataObject = body), searchFields = append("LockCombinationID", body %>% names()), entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' List Lockers
	#'
	#' This function returns a dataframe or json object of Lockers
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given Lockers. Defaults to FALSE for all return fields which, for convenience, returns all fields for the Lockers.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object listed as text. Run \code{\link{getSchemaForObjects}}('Locker') to get more field paths.
	#' @param searchConditionsList A list of search conditions to filter results which are joined by the searchConditionsGroupType. Of the form {FieldName} {ConditionType} {SearchCondition}. For example, c('StudentID LessEqual 500', 'LastName Like Ander\%'). Run \code{\link{listSearchConditionTypes}} for a list of ConditionTypes. Defaults to NULL (unfiltered).
	#' @param searchConditionsGroupType The conjunction which joins multiple searchConditions in the searchConditionsList. Either 'Or' or 'And'. Defaults to 'And'.
	#' @param searchSortFieldNamesList The list of fields sort results by. Defaults to NULL (unsorted).
	#' @param searchSortFieldNamesDescendingList A list of T/F values corresponding to whether to sort each field in searchSortFieldNamesList in descending order. Defaults to F for each FieldName in searchSortFieldNamesList.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param query Queries to be added to the request. Ex. list(SchoolYearID = 1). Defaults to NULL.
	#' @param page Results are paginated. The page of results to return. Default is 1.
	#' @param pageSize Results are paginated. The number of records per page to return. Default is 100,000 (essentially all records for most objects).
	#' @param flatten Whether to flatten results into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Student
	#' @return A list of Lockers
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	listLockers <- function(searchConditionsList = NULL, LockerID = F, LockerNumber = F, IsActive = F, IsDamaged = F, HasBuiltInLock = F, Comment = F, LockerAreaID = F, StudentsPerLocker = F, IsAvailable = F, CurrentCombination = F, GenderCode = F, GenderCount = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, NeedsLock = F, fieldPaths = NULL, searchConditionsGroupType = "And", searchSortFieldNamesList = NULL, searchSortFieldNamesDescendingList = NULL, entityId = 1, query = NULL, page = 1, pageSize = 100000, flatten = T, returnResponse = F){

		params <- as.list(environment())

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		listSkyObjects(module = "Student", objectName = "Locker", searchFields = searchFields %>% append(fieldPaths), page = page, pageSize = pageSize, SearchConditionsList = searchConditionsList, SearchConditionsGroupType = searchConditionsGroupType, SearchSortFieldNamesList = searchSortFieldNamesList, SearchSortFieldNamesDescendingList = searchSortFieldNamesDescendingList, entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' Get a Locker
	#'
	#' This function returns a dataframe or json object of a Locker
	#' @param LockerID The ID of the Locker to return.
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given Locker. Defaults to FALSE for all return fields which, for convenience, returns all fields for the Locker.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object listed as text. Run \code{\link{getSchemaForObjects}}('Locker') to get more field paths.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param query Queries to be added to the request. Ex. list(SchoolYearID = 1). Defaults to NULL.
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Student
	#' @return A dataframe or of Locker
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	getLocker <- function(LockerID, LockerNumber = F, IsActive = F, IsDamaged = F, HasBuiltInLock = F, Comment = F, LockerAreaID = F, StudentsPerLocker = F, IsAvailable = F, CurrentCombination = F, GenderCode = F, GenderCount = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, NeedsLock = F, fieldPaths = NULL, entityId = 1, query = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment()) %>% purrr::keep(names(.) != "LockerID")

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		getSkyObject(module = "Student", objectName = "Locker", objectId = LockerID, searchFields = searchFields, entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' Delete a Locker
	#'
	#' This function deletes a Locker
	#' @param LockerID The ID of the Locker to delete
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param query Queries to be added to the request. Ex. list(SchoolYearID = 1). Defaults to NULL.
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Student
	#' @return The LockerID of the deleted Locker.
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	deleteLocker <- function(LockerID, ignoreWarnings = F, entityId = 1, query = NULL, flatten = T, returnResponse = F){

		deleteSkyObject(module = "Student", objectName = "Locker", objectId = LockerID, ignoreWarnings = ignoreWarnings, entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' Create a Locker
	#'
	#' This function creates a Locker
	#' @param fieldNames The field values to give the created Locker. Each defaults to NULL.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param query Queries to be added to the request. Ex. list(SchoolYearID = 1). Defaults to NULL.
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Student
	#' @return A newly created Locker
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	createLocker <- function(LockerNumber = NULL, IsActive = NULL, IsDamaged = NULL, HasBuiltInLock = NULL, Comment = NULL, LockerAreaID = NULL, entityId = 1, query = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment())

		body <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper()) %>% purrr::compact()

		createSkyObject(module = "Student", objectName = "Locker", body = list(DataObject = body), searchFields = append("LockerID", body %>% names()), entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' Modify a Locker
	#'
	#' This function modifies a Locker
	#' @param fieldNames The field values to give the modified Locker. Each defaults to NULL.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param query Queries to be added to the request. Ex. list(SchoolYearID = 1). Defaults to NULL.
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Student
	#' @return The modified Locker
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	modifyLocker <- function(LockerID, LockerNumber = NULL, IsActive = NULL, IsDamaged = NULL, HasBuiltInLock = NULL, Comment = NULL, LockerAreaID = NULL, entityId = 1, query = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment())

		body <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper()) %>% purrr::compact()

		modifySkyObject(module = "Student", objectName = "Locker", objectId = LockerID, body = list(DataObject = body), searchFields = append("LockerID", body %>% names()), entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' List LockerAreas
	#'
	#' This function returns a dataframe or json object of LockerAreas
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given LockerAreas. Defaults to FALSE for all return fields which, for convenience, returns all fields for the LockerAreas.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object listed as text. Run \code{\link{getSchemaForObjects}}('LockerArea') to get more field paths.
	#' @param searchConditionsList A list of search conditions to filter results which are joined by the searchConditionsGroupType. Of the form {FieldName} {ConditionType} {SearchCondition}. For example, c('StudentID LessEqual 500', 'LastName Like Ander\%'). Run \code{\link{listSearchConditionTypes}} for a list of ConditionTypes. Defaults to NULL (unfiltered).
	#' @param searchConditionsGroupType The conjunction which joins multiple searchConditions in the searchConditionsList. Either 'Or' or 'And'. Defaults to 'And'.
	#' @param searchSortFieldNamesList The list of fields sort results by. Defaults to NULL (unsorted).
	#' @param searchSortFieldNamesDescendingList A list of T/F values corresponding to whether to sort each field in searchSortFieldNamesList in descending order. Defaults to F for each FieldName in searchSortFieldNamesList.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param query Queries to be added to the request. Ex. list(SchoolYearID = 1). Defaults to NULL.
	#' @param page Results are paginated. The page of results to return. Default is 1.
	#' @param pageSize Results are paginated. The number of records per page to return. Default is 100,000 (essentially all records for most objects).
	#' @param flatten Whether to flatten results into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Student
	#' @return A list of LockerAreas
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	listLockerAreas <- function(searchConditionsList = NULL, LockerAreaID = F, Code = F, Description = F, HasBuiltInLockDefault = F, MaximumStudentsPerLocker = F, AllowCoedLocker = F, BuildingID = F, CodeDescription = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, fieldPaths = NULL, searchConditionsGroupType = "And", searchSortFieldNamesList = NULL, searchSortFieldNamesDescendingList = NULL, entityId = 1, query = NULL, page = 1, pageSize = 100000, flatten = T, returnResponse = F){

		params <- as.list(environment())

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		listSkyObjects(module = "Student", objectName = "LockerArea", searchFields = searchFields %>% append(fieldPaths), page = page, pageSize = pageSize, SearchConditionsList = searchConditionsList, SearchConditionsGroupType = searchConditionsGroupType, SearchSortFieldNamesList = searchSortFieldNamesList, SearchSortFieldNamesDescendingList = searchSortFieldNamesDescendingList, entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' Get a LockerArea
	#'
	#' This function returns a dataframe or json object of a LockerArea
	#' @param LockerAreaID The ID of the LockerArea to return.
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given LockerArea. Defaults to FALSE for all return fields which, for convenience, returns all fields for the LockerArea.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object listed as text. Run \code{\link{getSchemaForObjects}}('LockerArea') to get more field paths.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param query Queries to be added to the request. Ex. list(SchoolYearID = 1). Defaults to NULL.
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Student
	#' @return A dataframe or of LockerArea
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	getLockerArea <- function(LockerAreaID, Code = F, Description = F, HasBuiltInLockDefault = F, MaximumStudentsPerLocker = F, AllowCoedLocker = F, BuildingID = F, CodeDescription = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, fieldPaths = NULL, entityId = 1, query = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment()) %>% purrr::keep(names(.) != "LockerAreaID")

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		getSkyObject(module = "Student", objectName = "LockerArea", objectId = LockerAreaID, searchFields = searchFields, entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' Delete a LockerArea
	#'
	#' This function deletes a LockerArea
	#' @param LockerAreaID The ID of the LockerArea to delete
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param query Queries to be added to the request. Ex. list(SchoolYearID = 1). Defaults to NULL.
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Student
	#' @return The LockerAreaID of the deleted LockerArea.
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	deleteLockerArea <- function(LockerAreaID, ignoreWarnings = F, entityId = 1, query = NULL, flatten = T, returnResponse = F){

		deleteSkyObject(module = "Student", objectName = "LockerArea", objectId = LockerAreaID, ignoreWarnings = ignoreWarnings, entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' Create a LockerArea
	#'
	#' This function creates a LockerArea
	#' @param fieldNames The field values to give the created LockerArea. Each defaults to NULL.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param query Queries to be added to the request. Ex. list(SchoolYearID = 1). Defaults to NULL.
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Student
	#' @return A newly created LockerArea
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	createLockerArea <- function(Code = NULL, Description = NULL, HasBuiltInLockDefault = NULL, MaximumStudentsPerLocker = NULL, AllowCoedLocker = NULL, BuildingID = NULL, entityId = 1, query = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment())

		body <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper()) %>% purrr::compact()

		createSkyObject(module = "Student", objectName = "LockerArea", body = list(DataObject = body), searchFields = append("LockerAreaID", body %>% names()), entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' Modify a LockerArea
	#'
	#' This function modifies a LockerArea
	#' @param fieldNames The field values to give the modified LockerArea. Each defaults to NULL.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param query Queries to be added to the request. Ex. list(SchoolYearID = 1). Defaults to NULL.
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Student
	#' @return The modified LockerArea
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	modifyLockerArea <- function(LockerAreaID, Code = NULL, Description = NULL, HasBuiltInLockDefault = NULL, MaximumStudentsPerLocker = NULL, AllowCoedLocker = NULL, BuildingID = NULL, entityId = 1, query = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment())

		body <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper()) %>% purrr::compact()

		modifySkyObject(module = "Student", objectName = "LockerArea", objectId = LockerAreaID, body = list(DataObject = body), searchFields = append("LockerAreaID", body %>% names()), entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' List LockMakes
	#'
	#' This function returns a dataframe or json object of LockMakes
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given LockMakes. Defaults to FALSE for all return fields which, for convenience, returns all fields for the LockMakes.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object listed as text. Run \code{\link{getSchemaForObjects}}('LockMake') to get more field paths.
	#' @param searchConditionsList A list of search conditions to filter results which are joined by the searchConditionsGroupType. Of the form {FieldName} {ConditionType} {SearchCondition}. For example, c('StudentID LessEqual 500', 'LastName Like Ander\%'). Run \code{\link{listSearchConditionTypes}} for a list of ConditionTypes. Defaults to NULL (unfiltered).
	#' @param searchConditionsGroupType The conjunction which joins multiple searchConditions in the searchConditionsList. Either 'Or' or 'And'. Defaults to 'And'.
	#' @param searchSortFieldNamesList The list of fields sort results by. Defaults to NULL (unsorted).
	#' @param searchSortFieldNamesDescendingList A list of T/F values corresponding to whether to sort each field in searchSortFieldNamesList in descending order. Defaults to F for each FieldName in searchSortFieldNamesList.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param query Queries to be added to the request. Ex. list(SchoolYearID = 1). Defaults to NULL.
	#' @param page Results are paginated. The page of results to return. Default is 1.
	#' @param pageSize Results are paginated. The number of records per page to return. Default is 100,000 (essentially all records for most objects).
	#' @param flatten Whether to flatten results into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Student
	#' @return A list of LockMakes
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	listLockMakes <- function(searchConditionsList = NULL, LockMakeID = F, Code = F, Description = F, CodeDescription = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, fieldPaths = NULL, searchConditionsGroupType = "And", searchSortFieldNamesList = NULL, searchSortFieldNamesDescendingList = NULL, entityId = 1, query = NULL, page = 1, pageSize = 100000, flatten = T, returnResponse = F){

		params <- as.list(environment())

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		listSkyObjects(module = "Student", objectName = "LockMake", searchFields = searchFields %>% append(fieldPaths), page = page, pageSize = pageSize, SearchConditionsList = searchConditionsList, SearchConditionsGroupType = searchConditionsGroupType, SearchSortFieldNamesList = searchSortFieldNamesList, SearchSortFieldNamesDescendingList = searchSortFieldNamesDescendingList, entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' Get a LockMake
	#'
	#' This function returns a dataframe or json object of a LockMake
	#' @param LockMakeID The ID of the LockMake to return.
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given LockMake. Defaults to FALSE for all return fields which, for convenience, returns all fields for the LockMake.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object listed as text. Run \code{\link{getSchemaForObjects}}('LockMake') to get more field paths.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param query Queries to be added to the request. Ex. list(SchoolYearID = 1). Defaults to NULL.
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Student
	#' @return A dataframe or of LockMake
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	getLockMake <- function(LockMakeID, Code = F, Description = F, CodeDescription = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, fieldPaths = NULL, entityId = 1, query = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment()) %>% purrr::keep(names(.) != "LockMakeID")

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		getSkyObject(module = "Student", objectName = "LockMake", objectId = LockMakeID, searchFields = searchFields, entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' Delete a LockMake
	#'
	#' This function deletes a LockMake
	#' @param LockMakeID The ID of the LockMake to delete
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param query Queries to be added to the request. Ex. list(SchoolYearID = 1). Defaults to NULL.
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Student
	#' @return The LockMakeID of the deleted LockMake.
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	deleteLockMake <- function(LockMakeID, ignoreWarnings = F, entityId = 1, query = NULL, flatten = T, returnResponse = F){

		deleteSkyObject(module = "Student", objectName = "LockMake", objectId = LockMakeID, ignoreWarnings = ignoreWarnings, entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' Create a LockMake
	#'
	#' This function creates a LockMake
	#' @param fieldNames The field values to give the created LockMake. Each defaults to NULL.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param query Queries to be added to the request. Ex. list(SchoolYearID = 1). Defaults to NULL.
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Student
	#' @return A newly created LockMake
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	createLockMake <- function(Code = NULL, Description = NULL, entityId = 1, query = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment())

		body <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper()) %>% purrr::compact()

		createSkyObject(module = "Student", objectName = "LockMake", body = list(DataObject = body), searchFields = append("LockMakeID", body %>% names()), entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' Modify a LockMake
	#'
	#' This function modifies a LockMake
	#' @param fieldNames The field values to give the modified LockMake. Each defaults to NULL.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param query Queries to be added to the request. Ex. list(SchoolYearID = 1). Defaults to NULL.
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Student
	#' @return The modified LockMake
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	modifyLockMake <- function(LockMakeID, Code = NULL, Description = NULL, entityId = 1, query = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment())

		body <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper()) %>% purrr::compact()

		modifySkyObject(module = "Student", objectName = "LockMake", objectId = LockMakeID, body = list(DataObject = body), searchFields = append("LockMakeID", body %>% names()), entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' List StudentDistricts
	#'
	#' This function returns a dataframe or json object of StudentDistricts
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given StudentDistricts. Defaults to FALSE for all return fields which, for convenience, returns all fields for the StudentDistricts.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object listed as text. Run \code{\link{getSchemaForObjects}}('StudentDistrict') to get more field paths.
	#' @param searchConditionsList A list of search conditions to filter results which are joined by the searchConditionsGroupType. Of the form {FieldName} {ConditionType} {SearchCondition}. For example, c('StudentID LessEqual 500', 'LastName Like Ander\%'). Run \code{\link{listSearchConditionTypes}} for a list of ConditionTypes. Defaults to NULL (unfiltered).
	#' @param searchConditionsGroupType The conjunction which joins multiple searchConditions in the searchConditionsList. Either 'Or' or 'And'. Defaults to 'And'.
	#' @param searchSortFieldNamesList The list of fields sort results by. Defaults to NULL (unsorted).
	#' @param searchSortFieldNamesDescendingList A list of T/F values corresponding to whether to sort each field in searchSortFieldNamesList in descending order. Defaults to F for each FieldName in searchSortFieldNamesList.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param query Queries to be added to the request. Ex. list(SchoolYearID = 1). Defaults to NULL.
	#' @param page Results are paginated. The page of results to return. Default is 1.
	#' @param pageSize Results are paginated. The number of records per page to return. Default is 100,000 (essentially all records for most objects).
	#' @param flatten Whether to flatten results into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Student
	#' @return A list of StudentDistricts
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	listStudentDistricts <- function(searchConditionsList = NULL, StudentDistrictID = F, StudentID = F, DistrictID = F, FeederSchoolID = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, FirstName = F, MiddleName = F, LastName = F, NameID = F, Grade = F, StudentNumber = F, fieldPaths = NULL, searchConditionsGroupType = "And", searchSortFieldNamesList = NULL, searchSortFieldNamesDescendingList = NULL, entityId = 1, query = NULL, page = 1, pageSize = 100000, flatten = T, returnResponse = F){

		params <- as.list(environment())

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		listSkyObjects(module = "Student", objectName = "StudentDistrict", searchFields = searchFields %>% append(fieldPaths), page = page, pageSize = pageSize, SearchConditionsList = searchConditionsList, SearchConditionsGroupType = searchConditionsGroupType, SearchSortFieldNamesList = searchSortFieldNamesList, SearchSortFieldNamesDescendingList = searchSortFieldNamesDescendingList, entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' Get a StudentDistrict
	#'
	#' This function returns a dataframe or json object of a StudentDistrict
	#' @param StudentDistrictID The ID of the StudentDistrict to return.
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given StudentDistrict. Defaults to FALSE for all return fields which, for convenience, returns all fields for the StudentDistrict.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object listed as text. Run \code{\link{getSchemaForObjects}}('StudentDistrict') to get more field paths.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param query Queries to be added to the request. Ex. list(SchoolYearID = 1). Defaults to NULL.
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Student
	#' @return A dataframe or of StudentDistrict
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	getStudentDistrict <- function(StudentDistrictID, StudentID = F, DistrictID = F, FeederSchoolID = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, FirstName = F, MiddleName = F, LastName = F, NameID = F, Grade = F, StudentNumber = F, fieldPaths = NULL, entityId = 1, query = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment()) %>% purrr::keep(names(.) != "StudentDistrictID")

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		getSkyObject(module = "Student", objectName = "StudentDistrict", objectId = StudentDistrictID, searchFields = searchFields, entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' Delete a StudentDistrict
	#'
	#' This function deletes a StudentDistrict
	#' @param StudentDistrictID The ID of the StudentDistrict to delete
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param query Queries to be added to the request. Ex. list(SchoolYearID = 1). Defaults to NULL.
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Student
	#' @return The StudentDistrictID of the deleted StudentDistrict.
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	deleteStudentDistrict <- function(StudentDistrictID, ignoreWarnings = F, entityId = 1, query = NULL, flatten = T, returnResponse = F){

		deleteSkyObject(module = "Student", objectName = "StudentDistrict", objectId = StudentDistrictID, ignoreWarnings = ignoreWarnings, entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' Create a StudentDistrict
	#'
	#' This function creates a StudentDistrict
	#' @param fieldNames The field values to give the created StudentDistrict. Each defaults to NULL.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param query Queries to be added to the request. Ex. list(SchoolYearID = 1). Defaults to NULL.
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Student
	#' @return A newly created StudentDistrict
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	createStudentDistrict <- function(StudentID = NULL, DistrictID = NULL, FeederSchoolID = NULL, NameID = NULL, Grade = NULL, StudentNumber = NULL, entityId = 1, query = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment())

		body <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper()) %>% purrr::compact()

		createSkyObject(module = "Student", objectName = "StudentDistrict", body = list(DataObject = body), searchFields = append("StudentDistrictID", body %>% names()), entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' Modify a StudentDistrict
	#'
	#' This function modifies a StudentDistrict
	#' @param fieldNames The field values to give the modified StudentDistrict. Each defaults to NULL.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param query Queries to be added to the request. Ex. list(SchoolYearID = 1). Defaults to NULL.
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Student
	#' @return The modified StudentDistrict
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	modifyStudentDistrict <- function(StudentDistrictID, StudentID = NULL, DistrictID = NULL, FeederSchoolID = NULL, NameID = NULL, Grade = NULL, StudentNumber = NULL, entityId = 1, query = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment())

		body <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper()) %>% purrr::compact()

		modifySkyObject(module = "Student", objectName = "StudentDistrict", objectId = StudentDistrictID, body = list(DataObject = body), searchFields = append("StudentDistrictID", body %>% names()), entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' List StudentLockers
	#'
	#' This function returns a dataframe or json object of StudentLockers
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given StudentLockers. Defaults to FALSE for all return fields which, for convenience, returns all fields for the StudentLockers.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object listed as text. Run \code{\link{getSchemaForObjects}}('StudentLocker') to get more field paths.
	#' @param searchConditionsList A list of search conditions to filter results which are joined by the searchConditionsGroupType. Of the form {FieldName} {ConditionType} {SearchCondition}. For example, c('StudentID LessEqual 500', 'LastName Like Ander\%'). Run \code{\link{listSearchConditionTypes}} for a list of ConditionTypes. Defaults to NULL (unfiltered).
	#' @param searchConditionsGroupType The conjunction which joins multiple searchConditions in the searchConditionsList. Either 'Or' or 'And'. Defaults to 'And'.
	#' @param searchSortFieldNamesList The list of fields sort results by. Defaults to NULL (unsorted).
	#' @param searchSortFieldNamesDescendingList A list of T/F values corresponding to whether to sort each field in searchSortFieldNamesList in descending order. Defaults to F for each FieldName in searchSortFieldNamesList.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param query Queries to be added to the request. Ex. list(SchoolYearID = 1). Defaults to NULL.
	#' @param page Results are paginated. The page of results to return. Default is 1.
	#' @param pageSize Results are paginated. The number of records per page to return. Default is 100,000 (essentially all records for most objects).
	#' @param flatten Whether to flatten results into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Student
	#' @return A list of StudentLockers
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	listStudentLockers <- function(searchConditionsList = NULL, StudentLockerID = F, IsPrimaryLocker = F, LockerID = F, StudentID = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, fieldPaths = NULL, searchConditionsGroupType = "And", searchSortFieldNamesList = NULL, searchSortFieldNamesDescendingList = NULL, entityId = 1, query = NULL, page = 1, pageSize = 100000, flatten = T, returnResponse = F){

		params <- as.list(environment())

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		listSkyObjects(module = "Student", objectName = "StudentLocker", searchFields = searchFields %>% append(fieldPaths), page = page, pageSize = pageSize, SearchConditionsList = searchConditionsList, SearchConditionsGroupType = searchConditionsGroupType, SearchSortFieldNamesList = searchSortFieldNamesList, SearchSortFieldNamesDescendingList = searchSortFieldNamesDescendingList, entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' Get a StudentLocker
	#'
	#' This function returns a dataframe or json object of a StudentLocker
	#' @param StudentLockerID The ID of the StudentLocker to return.
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given StudentLocker. Defaults to FALSE for all return fields which, for convenience, returns all fields for the StudentLocker.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object listed as text. Run \code{\link{getSchemaForObjects}}('StudentLocker') to get more field paths.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param query Queries to be added to the request. Ex. list(SchoolYearID = 1). Defaults to NULL.
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Student
	#' @return A dataframe or of StudentLocker
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	getStudentLocker <- function(StudentLockerID, IsPrimaryLocker = F, LockerID = F, StudentID = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, fieldPaths = NULL, entityId = 1, query = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment()) %>% purrr::keep(names(.) != "StudentLockerID")

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		getSkyObject(module = "Student", objectName = "StudentLocker", objectId = StudentLockerID, searchFields = searchFields, entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' Delete a StudentLocker
	#'
	#' This function deletes a StudentLocker
	#' @param StudentLockerID The ID of the StudentLocker to delete
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param query Queries to be added to the request. Ex. list(SchoolYearID = 1). Defaults to NULL.
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Student
	#' @return The StudentLockerID of the deleted StudentLocker.
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	deleteStudentLocker <- function(StudentLockerID, ignoreWarnings = F, entityId = 1, query = NULL, flatten = T, returnResponse = F){

		deleteSkyObject(module = "Student", objectName = "StudentLocker", objectId = StudentLockerID, ignoreWarnings = ignoreWarnings, entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' Create a StudentLocker
	#'
	#' This function creates a StudentLocker
	#' @param fieldNames The field values to give the created StudentLocker. Each defaults to NULL.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param query Queries to be added to the request. Ex. list(SchoolYearID = 1). Defaults to NULL.
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Student
	#' @return A newly created StudentLocker
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	createStudentLocker <- function(IsPrimaryLocker = NULL, LockerID = NULL, StudentID = NULL, entityId = 1, query = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment())

		body <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper()) %>% purrr::compact()

		createSkyObject(module = "Student", objectName = "StudentLocker", body = list(DataObject = body), searchFields = append("StudentLockerID", body %>% names()), entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' Modify a StudentLocker
	#'
	#' This function modifies a StudentLocker
	#' @param fieldNames The field values to give the modified StudentLocker. Each defaults to NULL.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param query Queries to be added to the request. Ex. list(SchoolYearID = 1). Defaults to NULL.
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Student
	#' @return The modified StudentLocker
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	modifyStudentLocker <- function(StudentLockerID, IsPrimaryLocker = NULL, LockerID = NULL, StudentID = NULL, entityId = 1, query = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment())

		body <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper()) %>% purrr::compact()

		modifySkyObject(module = "Student", objectName = "StudentLocker", objectId = StudentLockerID, body = list(DataObject = body), searchFields = append("StudentLockerID", body %>% names()), entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' List TempStudentAssignedLockerRecords
	#'
	#' This function returns a dataframe or json object of TempStudentAssignedLockerRecords
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given TempStudentAssignedLockerRecords. Defaults to FALSE for all return fields which, for convenience, returns all fields for the TempStudentAssignedLockerRecords.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object listed as text. Run \code{\link{getSchemaForObjects}}('TempStudentAssignedLockerRecord') to get more field paths.
	#' @param searchConditionsList A list of search conditions to filter results which are joined by the searchConditionsGroupType. Of the form {FieldName} {ConditionType} {SearchCondition}. For example, c('StudentID LessEqual 500', 'LastName Like Ander\%'). Run \code{\link{listSearchConditionTypes}} for a list of ConditionTypes. Defaults to NULL (unfiltered).
	#' @param searchConditionsGroupType The conjunction which joins multiple searchConditions in the searchConditionsList. Either 'Or' or 'And'. Defaults to 'And'.
	#' @param searchSortFieldNamesList The list of fields sort results by. Defaults to NULL (unsorted).
	#' @param searchSortFieldNamesDescendingList A list of T/F values corresponding to whether to sort each field in searchSortFieldNamesList in descending order. Defaults to F for each FieldName in searchSortFieldNamesList.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param query Queries to be added to the request. Ex. list(SchoolYearID = 1). Defaults to NULL.
	#' @param page Results are paginated. The page of results to return. Default is 1.
	#' @param pageSize Results are paginated. The number of records per page to return. Default is 100,000 (essentially all records for most objects).
	#' @param flatten Whether to flatten results into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Student
	#' @return A list of TempStudentAssignedLockerRecords
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	listTempStudentAssignedLockerRecords <- function(searchConditionsList = NULL, TempStudentAssignedLockerRecordID = F, StudentID = F, LockerID = F, FullName = F, StudentNumber = F, Gender = F, BirthDate = F, Age = F, GradYear = F, Grade = F, Building = F, LockerArea = F, LockerNumber = F, Combination = F, IsStudentAccessUser = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, fieldPaths = NULL, searchConditionsGroupType = "And", searchSortFieldNamesList = NULL, searchSortFieldNamesDescendingList = NULL, entityId = 1, query = NULL, page = 1, pageSize = 100000, flatten = T, returnResponse = F){

		params <- as.list(environment())

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		listSkyObjects(module = "Student", objectName = "TempStudentAssignedLockerRecord", searchFields = searchFields %>% append(fieldPaths), page = page, pageSize = pageSize, SearchConditionsList = searchConditionsList, SearchConditionsGroupType = searchConditionsGroupType, SearchSortFieldNamesList = searchSortFieldNamesList, SearchSortFieldNamesDescendingList = searchSortFieldNamesDescendingList, entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' Get a TempStudentAssignedLockerRecord
	#'
	#' This function returns a dataframe or json object of a TempStudentAssignedLockerRecord
	#' @param TempStudentAssignedLockerRecordID The ID of the TempStudentAssignedLockerRecord to return.
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given TempStudentAssignedLockerRecord. Defaults to FALSE for all return fields which, for convenience, returns all fields for the TempStudentAssignedLockerRecord.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object listed as text. Run \code{\link{getSchemaForObjects}}('TempStudentAssignedLockerRecord') to get more field paths.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param query Queries to be added to the request. Ex. list(SchoolYearID = 1). Defaults to NULL.
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Student
	#' @return A dataframe or of TempStudentAssignedLockerRecord
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	getTempStudentAssignedLockerRecord <- function(TempStudentAssignedLockerRecordID, StudentID = F, LockerID = F, FullName = F, StudentNumber = F, Gender = F, BirthDate = F, Age = F, GradYear = F, Grade = F, Building = F, LockerArea = F, LockerNumber = F, Combination = F, IsStudentAccessUser = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, fieldPaths = NULL, entityId = 1, query = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment()) %>% purrr::keep(names(.) != "TempStudentAssignedLockerRecordID")

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		getSkyObject(module = "Student", objectName = "TempStudentAssignedLockerRecord", objectId = TempStudentAssignedLockerRecordID, searchFields = searchFields, entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' Delete a TempStudentAssignedLockerRecord
	#'
	#' This function deletes a TempStudentAssignedLockerRecord
	#' @param TempStudentAssignedLockerRecordID The ID of the TempStudentAssignedLockerRecord to delete
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param query Queries to be added to the request. Ex. list(SchoolYearID = 1). Defaults to NULL.
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Student
	#' @return The TempStudentAssignedLockerRecordID of the deleted TempStudentAssignedLockerRecord.
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	deleteTempStudentAssignedLockerRecord <- function(TempStudentAssignedLockerRecordID, ignoreWarnings = F, entityId = 1, query = NULL, flatten = T, returnResponse = F){

		deleteSkyObject(module = "Student", objectName = "TempStudentAssignedLockerRecord", objectId = TempStudentAssignedLockerRecordID, ignoreWarnings = ignoreWarnings, entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' Create a TempStudentAssignedLockerRecord
	#'
	#' This function creates a TempStudentAssignedLockerRecord
	#' @param fieldNames The field values to give the created TempStudentAssignedLockerRecord. Each defaults to NULL.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param query Queries to be added to the request. Ex. list(SchoolYearID = 1). Defaults to NULL.
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Student
	#' @return A newly created TempStudentAssignedLockerRecord
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	createTempStudentAssignedLockerRecord <- function(StudentID = NULL, LockerID = NULL, FullName = NULL, StudentNumber = NULL, Gender = NULL, BirthDate = NULL, Age = NULL, GradYear = NULL, Grade = NULL, Building = NULL, LockerArea = NULL, LockerNumber = NULL, Combination = NULL, IsStudentAccessUser = NULL, entityId = 1, query = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment())

		body <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper()) %>% purrr::compact()

		createSkyObject(module = "Student", objectName = "TempStudentAssignedLockerRecord", body = list(DataObject = body), searchFields = append("TempStudentAssignedLockerRecordID", body %>% names()), entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' Modify a TempStudentAssignedLockerRecord
	#'
	#' This function modifies a TempStudentAssignedLockerRecord
	#' @param fieldNames The field values to give the modified TempStudentAssignedLockerRecord. Each defaults to NULL.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param query Queries to be added to the request. Ex. list(SchoolYearID = 1). Defaults to NULL.
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Student
	#' @return The modified TempStudentAssignedLockerRecord
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	modifyTempStudentAssignedLockerRecord <- function(TempStudentAssignedLockerRecordID, StudentID = NULL, LockerID = NULL, FullName = NULL, StudentNumber = NULL, Gender = NULL, BirthDate = NULL, Age = NULL, GradYear = NULL, Grade = NULL, Building = NULL, LockerArea = NULL, LockerNumber = NULL, Combination = NULL, IsStudentAccessUser = NULL, entityId = 1, query = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment())

		body <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper()) %>% purrr::compact()

		modifySkyObject(module = "Student", objectName = "TempStudentAssignedLockerRecord", objectId = TempStudentAssignedLockerRecordID, body = list(DataObject = body), searchFields = append("TempStudentAssignedLockerRecordID", body %>% names()), entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' List StudentConfigSystems
	#'
	#' This function returns a dataframe or json object of StudentConfigSystems
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given StudentConfigSystems. Defaults to FALSE for all return fields which, for convenience, returns all fields for the StudentConfigSystems.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object listed as text. Run \code{\link{getSchemaForObjects}}('StudentConfigSystem') to get more field paths.
	#' @param searchConditionsList A list of search conditions to filter results which are joined by the searchConditionsGroupType. Of the form {FieldName} {ConditionType} {SearchCondition}. For example, c('StudentID LessEqual 500', 'LastName Like Ander\%'). Run \code{\link{listSearchConditionTypes}} for a list of ConditionTypes. Defaults to NULL (unfiltered).
	#' @param searchConditionsGroupType The conjunction which joins multiple searchConditions in the searchConditionsList. Either 'Or' or 'And'. Defaults to 'And'.
	#' @param searchSortFieldNamesList The list of fields sort results by. Defaults to NULL (unsorted).
	#' @param searchSortFieldNamesDescendingList A list of T/F values corresponding to whether to sort each field in searchSortFieldNamesList in descending order. Defaults to F for each FieldName in searchSortFieldNamesList.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param query Queries to be added to the request. Ex. list(SchoolYearID = 1). Defaults to NULL.
	#' @param page Results are paginated. The page of results to return. Default is 1.
	#' @param pageSize Results are paginated. The number of records per page to return. Default is 100,000 (essentially all records for most objects).
	#' @param flatten Whether to flatten results into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Student
	#' @return A list of StudentConfigSystems
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	listStudentConfigSystems <- function(searchConditionsList = NULL, ConfigSystemID = F, StudentNumberMask = F, StudentNumberStartValue = F, AllowStudentAccessDefault = F, EmailTypeIDDefault = F, EmailDomain = F, AutoGenerateEmail = F, AutoGenerateSecurityUser = F, StudentAttachmentVisibility = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, EasyIEPSpecEdImportFileName = F, EasyIEPSection504ImportFileName = F, FileDestinationIDEasyIEPSpecEdImport = F, FileDestinationIDEasyIEPSection504Import = F, fieldPaths = NULL, searchConditionsGroupType = "And", searchSortFieldNamesList = NULL, searchSortFieldNamesDescendingList = NULL, entityId = 1, query = NULL, page = 1, pageSize = 100000, flatten = T, returnResponse = F){

		params <- as.list(environment())

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		listSkyObjects(module = "Student", objectName = "ConfigSystem", searchFields = searchFields %>% append(fieldPaths), page = page, pageSize = pageSize, SearchConditionsList = searchConditionsList, SearchConditionsGroupType = searchConditionsGroupType, SearchSortFieldNamesList = searchSortFieldNamesList, SearchSortFieldNamesDescendingList = searchSortFieldNamesDescendingList, entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' Get a StudentConfigSystem
	#'
	#' This function returns a dataframe or json object of a StudentConfigSystem
	#' @param StudentConfigSystemID The ID of the StudentConfigSystem to return.
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given StudentConfigSystem. Defaults to FALSE for all return fields which, for convenience, returns all fields for the StudentConfigSystem.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object listed as text. Run \code{\link{getSchemaForObjects}}('StudentConfigSystem') to get more field paths.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param query Queries to be added to the request. Ex. list(SchoolYearID = 1). Defaults to NULL.
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Student
	#' @return A dataframe or of StudentConfigSystem
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	getStudentConfigSystem <- function(StudentConfigSystemID, ConfigSystemID = F, StudentNumberMask = F, StudentNumberStartValue = F, AllowStudentAccessDefault = F, EmailTypeIDDefault = F, EmailDomain = F, AutoGenerateEmail = F, AutoGenerateSecurityUser = F, StudentAttachmentVisibility = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, EasyIEPSpecEdImportFileName = F, EasyIEPSection504ImportFileName = F, FileDestinationIDEasyIEPSpecEdImport = F, FileDestinationIDEasyIEPSection504Import = F, fieldPaths = NULL, entityId = 1, query = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment()) %>% purrr::keep(names(.) != "StudentConfigSystemID")

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		getSkyObject(module = "Student", objectName = "ConfigSystem", objectId = StudentConfigSystemID, searchFields = searchFields, entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' Delete a StudentConfigSystem
	#'
	#' This function deletes a StudentConfigSystem
	#' @param StudentConfigSystemID The ID of the StudentConfigSystem to delete
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param query Queries to be added to the request. Ex. list(SchoolYearID = 1). Defaults to NULL.
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Student
	#' @return The StudentConfigSystemID of the deleted StudentConfigSystem.
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	deleteStudentConfigSystem <- function(StudentConfigSystemID, ignoreWarnings = F, entityId = 1, query = NULL, flatten = T, returnResponse = F){

		deleteSkyObject(module = "Student", objectName = "ConfigSystem", objectId = StudentConfigSystemID, ignoreWarnings = ignoreWarnings, entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' Create a StudentConfigSystem
	#'
	#' This function creates a StudentConfigSystem
	#' @param fieldNames The field values to give the created StudentConfigSystem. Each defaults to NULL.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param query Queries to be added to the request. Ex. list(SchoolYearID = 1). Defaults to NULL.
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Student
	#' @return A newly created StudentConfigSystem
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	createStudentConfigSystem <- function(StudentNumberMask = NULL, StudentNumberStartValue = NULL, AllowStudentAccessDefault = NULL, EmailTypeIDDefault = NULL, EmailDomain = NULL, AutoGenerateEmail = NULL, AutoGenerateSecurityUser = NULL, StudentAttachmentVisibility = NULL, EasyIEPSpecEdImportFileName = NULL, EasyIEPSection504ImportFileName = NULL, FileDestinationIDEasyIEPSpecEdImport = NULL, FileDestinationIDEasyIEPSection504Import = NULL, entityId = 1, query = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment())

		body <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper()) %>% purrr::compact()

		createSkyObject(module = "Student", objectName = "ConfigSystem", body = list(DataObject = body), searchFields = append("ConfigSystemID", body %>% names()), entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' Modify a StudentConfigSystem
	#'
	#' This function modifies a StudentConfigSystem
	#' @param fieldNames The field values to give the modified StudentConfigSystem. Each defaults to NULL.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param query Queries to be added to the request. Ex. list(SchoolYearID = 1). Defaults to NULL.
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Student
	#' @return The modified StudentConfigSystem
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	modifyStudentConfigSystem <- function(ConfigSystemID, StudentNumberMask = NULL, StudentNumberStartValue = NULL, AllowStudentAccessDefault = NULL, EmailTypeIDDefault = NULL, EmailDomain = NULL, AutoGenerateEmail = NULL, AutoGenerateSecurityUser = NULL, StudentAttachmentVisibility = NULL, EasyIEPSpecEdImportFileName = NULL, EasyIEPSection504ImportFileName = NULL, FileDestinationIDEasyIEPSpecEdImport = NULL, FileDestinationIDEasyIEPSection504Import = NULL, entityId = 1, query = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment())

		body <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper()) %>% purrr::compact()

		modifySkyObject(module = "Student", objectName = "ConfigSystem", objectId = ConfigSystemID, body = list(DataObject = body), searchFields = append("ConfigSystemID", body %>% names()), entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' List NextStudentNumbers
	#'
	#' This function returns a dataframe or json object of NextStudentNumbers
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given NextStudentNumbers. Defaults to FALSE for all return fields which, for convenience, returns all fields for the NextStudentNumbers.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object listed as text. Run \code{\link{getSchemaForObjects}}('NextStudentNumber') to get more field paths.
	#' @param searchConditionsList A list of search conditions to filter results which are joined by the searchConditionsGroupType. Of the form {FieldName} {ConditionType} {SearchCondition}. For example, c('StudentID LessEqual 500', 'LastName Like Ander\%'). Run \code{\link{listSearchConditionTypes}} for a list of ConditionTypes. Defaults to NULL (unfiltered).
	#' @param searchConditionsGroupType The conjunction which joins multiple searchConditions in the searchConditionsList. Either 'Or' or 'And'. Defaults to 'And'.
	#' @param searchSortFieldNamesList The list of fields sort results by. Defaults to NULL (unsorted).
	#' @param searchSortFieldNamesDescendingList A list of T/F values corresponding to whether to sort each field in searchSortFieldNamesList in descending order. Defaults to F for each FieldName in searchSortFieldNamesList.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param query Queries to be added to the request. Ex. list(SchoolYearID = 1). Defaults to NULL.
	#' @param page Results are paginated. The page of results to return. Default is 1.
	#' @param pageSize Results are paginated. The number of records per page to return. Default is 100,000 (essentially all records for most objects).
	#' @param flatten Whether to flatten results into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Student
	#' @return A list of NextStudentNumbers
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	listNextStudentNumbers <- function(searchConditionsList = NULL, NextStudentNumberID = F, MaskPrefix = F, SequenceNumber = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, IsForStateID = F, fieldPaths = NULL, searchConditionsGroupType = "And", searchSortFieldNamesList = NULL, searchSortFieldNamesDescendingList = NULL, entityId = 1, query = NULL, page = 1, pageSize = 100000, flatten = T, returnResponse = F){

		params <- as.list(environment())

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		listSkyObjects(module = "Student", objectName = "NextStudentNumber", searchFields = searchFields %>% append(fieldPaths), page = page, pageSize = pageSize, SearchConditionsList = searchConditionsList, SearchConditionsGroupType = searchConditionsGroupType, SearchSortFieldNamesList = searchSortFieldNamesList, SearchSortFieldNamesDescendingList = searchSortFieldNamesDescendingList, entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' Get a NextStudentNumber
	#'
	#' This function returns a dataframe or json object of a NextStudentNumber
	#' @param NextStudentNumberID The ID of the NextStudentNumber to return.
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given NextStudentNumber. Defaults to FALSE for all return fields which, for convenience, returns all fields for the NextStudentNumber.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object listed as text. Run \code{\link{getSchemaForObjects}}('NextStudentNumber') to get more field paths.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param query Queries to be added to the request. Ex. list(SchoolYearID = 1). Defaults to NULL.
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Student
	#' @return A dataframe or of NextStudentNumber
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	getNextStudentNumber <- function(NextStudentNumberID, MaskPrefix = F, SequenceNumber = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, IsForStateID = F, fieldPaths = NULL, entityId = 1, query = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment()) %>% purrr::keep(names(.) != "NextStudentNumberID")

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		getSkyObject(module = "Student", objectName = "NextStudentNumber", objectId = NextStudentNumberID, searchFields = searchFields, entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' Delete a NextStudentNumber
	#'
	#' This function deletes a NextStudentNumber
	#' @param NextStudentNumberID The ID of the NextStudentNumber to delete
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param query Queries to be added to the request. Ex. list(SchoolYearID = 1). Defaults to NULL.
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Student
	#' @return The NextStudentNumberID of the deleted NextStudentNumber.
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	deleteNextStudentNumber <- function(NextStudentNumberID, ignoreWarnings = F, entityId = 1, query = NULL, flatten = T, returnResponse = F){

		deleteSkyObject(module = "Student", objectName = "NextStudentNumber", objectId = NextStudentNumberID, ignoreWarnings = ignoreWarnings, entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' Create a NextStudentNumber
	#'
	#' This function creates a NextStudentNumber
	#' @param fieldNames The field values to give the created NextStudentNumber. Each defaults to NULL.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param query Queries to be added to the request. Ex. list(SchoolYearID = 1). Defaults to NULL.
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Student
	#' @return A newly created NextStudentNumber
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	createNextStudentNumber <- function(MaskPrefix = NULL, SequenceNumber = NULL, IsForStateID = NULL, entityId = 1, query = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment())

		body <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper()) %>% purrr::compact()

		createSkyObject(module = "Student", objectName = "NextStudentNumber", body = list(DataObject = body), searchFields = append("NextStudentNumberID", body %>% names()), entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' Modify a NextStudentNumber
	#'
	#' This function modifies a NextStudentNumber
	#' @param fieldNames The field values to give the modified NextStudentNumber. Each defaults to NULL.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param query Queries to be added to the request. Ex. list(SchoolYearID = 1). Defaults to NULL.
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Student
	#' @return The modified NextStudentNumber
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	modifyNextStudentNumber <- function(NextStudentNumberID, MaskPrefix = NULL, SequenceNumber = NULL, IsForStateID = NULL, entityId = 1, query = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment())

		body <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper()) %>% purrr::compact()

		modifySkyObject(module = "Student", objectName = "NextStudentNumber", objectId = NextStudentNumberID, body = list(DataObject = body), searchFields = append("NextStudentNumberID", body %>% names()), entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' List TempStudentPermits
	#'
	#' This function returns a dataframe or json object of TempStudentPermits
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given TempStudentPermits. Defaults to FALSE for all return fields which, for convenience, returns all fields for the TempStudentPermits.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object listed as text. Run \code{\link{getSchemaForObjects}}('TempStudentPermit') to get more field paths.
	#' @param searchConditionsList A list of search conditions to filter results which are joined by the searchConditionsGroupType. Of the form {FieldName} {ConditionType} {SearchCondition}. For example, c('StudentID LessEqual 500', 'LastName Like Ander\%'). Run \code{\link{listSearchConditionTypes}} for a list of ConditionTypes. Defaults to NULL (unfiltered).
	#' @param searchConditionsGroupType The conjunction which joins multiple searchConditions in the searchConditionsList. Either 'Or' or 'And'. Defaults to 'And'.
	#' @param searchSortFieldNamesList The list of fields sort results by. Defaults to NULL (unsorted).
	#' @param searchSortFieldNamesDescendingList A list of T/F values corresponding to whether to sort each field in searchSortFieldNamesList in descending order. Defaults to F for each FieldName in searchSortFieldNamesList.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param query Queries to be added to the request. Ex. list(SchoolYearID = 1). Defaults to NULL.
	#' @param page Results are paginated. The page of results to return. Default is 1.
	#' @param pageSize Results are paginated. The number of records per page to return. Default is 100,000 (essentially all records for most objects).
	#' @param flatten Whether to flatten results into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Student
	#' @return A list of TempStudentPermits
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	listTempStudentPermits <- function(searchConditionsList = NULL, TempStudentPermitID = F, StudentID = F, PermitID = F, FullNameLFM = F, StudentNumber = F, GenderCode = F, Age = F, GradYear = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, AddressID = F, EntityName = F, HasExceptions = F, ExceptionNote = F, SchoolYearID = F, DistrictID = F, fieldPaths = NULL, searchConditionsGroupType = "And", searchSortFieldNamesList = NULL, searchSortFieldNamesDescendingList = NULL, entityId = 1, query = NULL, page = 1, pageSize = 100000, flatten = T, returnResponse = F){

		params <- as.list(environment())

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		listSkyObjects(module = "Student", objectName = "TempStudentPermit", searchFields = searchFields %>% append(fieldPaths), page = page, pageSize = pageSize, SearchConditionsList = searchConditionsList, SearchConditionsGroupType = searchConditionsGroupType, SearchSortFieldNamesList = searchSortFieldNamesList, SearchSortFieldNamesDescendingList = searchSortFieldNamesDescendingList, entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' Get a TempStudentPermit
	#'
	#' This function returns a dataframe or json object of a TempStudentPermit
	#' @param TempStudentPermitID The ID of the TempStudentPermit to return.
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given TempStudentPermit. Defaults to FALSE for all return fields which, for convenience, returns all fields for the TempStudentPermit.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object listed as text. Run \code{\link{getSchemaForObjects}}('TempStudentPermit') to get more field paths.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param query Queries to be added to the request. Ex. list(SchoolYearID = 1). Defaults to NULL.
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Student
	#' @return A dataframe or of TempStudentPermit
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	getTempStudentPermit <- function(TempStudentPermitID, StudentID = F, PermitID = F, FullNameLFM = F, StudentNumber = F, GenderCode = F, Age = F, GradYear = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, AddressID = F, EntityName = F, HasExceptions = F, ExceptionNote = F, SchoolYearID = F, DistrictID = F, fieldPaths = NULL, entityId = 1, query = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment()) %>% purrr::keep(names(.) != "TempStudentPermitID")

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		getSkyObject(module = "Student", objectName = "TempStudentPermit", objectId = TempStudentPermitID, searchFields = searchFields, entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' Delete a TempStudentPermit
	#'
	#' This function deletes a TempStudentPermit
	#' @param TempStudentPermitID The ID of the TempStudentPermit to delete
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param query Queries to be added to the request. Ex. list(SchoolYearID = 1). Defaults to NULL.
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Student
	#' @return The TempStudentPermitID of the deleted TempStudentPermit.
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	deleteTempStudentPermit <- function(TempStudentPermitID, ignoreWarnings = F, entityId = 1, query = NULL, flatten = T, returnResponse = F){

		deleteSkyObject(module = "Student", objectName = "TempStudentPermit", objectId = TempStudentPermitID, ignoreWarnings = ignoreWarnings, entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' Create a TempStudentPermit
	#'
	#' This function creates a TempStudentPermit
	#' @param fieldNames The field values to give the created TempStudentPermit. Each defaults to NULL.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param query Queries to be added to the request. Ex. list(SchoolYearID = 1). Defaults to NULL.
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Student
	#' @return A newly created TempStudentPermit
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	createTempStudentPermit <- function(StudentID = NULL, PermitID = NULL, FullNameLFM = NULL, StudentNumber = NULL, GenderCode = NULL, Age = NULL, GradYear = NULL, AddressID = NULL, EntityName = NULL, HasExceptions = NULL, ExceptionNote = NULL, SchoolYearID = NULL, DistrictID = NULL, entityId = 1, query = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment())

		body <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper()) %>% purrr::compact()

		createSkyObject(module = "Student", objectName = "TempStudentPermit", body = list(DataObject = body), searchFields = append("TempStudentPermitID", body %>% names()), entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' Modify a TempStudentPermit
	#'
	#' This function modifies a TempStudentPermit
	#' @param fieldNames The field values to give the modified TempStudentPermit. Each defaults to NULL.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param query Queries to be added to the request. Ex. list(SchoolYearID = 1). Defaults to NULL.
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Student
	#' @return The modified TempStudentPermit
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	modifyTempStudentPermit <- function(TempStudentPermitID, StudentID = NULL, PermitID = NULL, FullNameLFM = NULL, StudentNumber = NULL, GenderCode = NULL, Age = NULL, GradYear = NULL, AddressID = NULL, EntityName = NULL, HasExceptions = NULL, ExceptionNote = NULL, SchoolYearID = NULL, DistrictID = NULL, entityId = 1, query = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment())

		body <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper()) %>% purrr::compact()

		modifySkyObject(module = "Student", objectName = "TempStudentPermit", objectId = TempStudentPermitID, body = list(DataObject = body), searchFields = append("TempStudentPermitID", body %>% names()), entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' List StudentConfigEntityGroupYears
	#'
	#' This function returns a dataframe or json object of StudentConfigEntityGroupYears
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given StudentConfigEntityGroupYears. Defaults to FALSE for all return fields which, for convenience, returns all fields for the StudentConfigEntityGroupYears.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object listed as text. Run \code{\link{getSchemaForObjects}}('StudentConfigEntityGroupYear') to get more field paths.
	#' @param searchConditionsList A list of search conditions to filter results which are joined by the searchConditionsGroupType. Of the form {FieldName} {ConditionType} {SearchCondition}. For example, c('StudentID LessEqual 500', 'LastName Like Ander\%'). Run \code{\link{listSearchConditionTypes}} for a list of ConditionTypes. Defaults to NULL (unfiltered).
	#' @param searchConditionsGroupType The conjunction which joins multiple searchConditions in the searchConditionsList. Either 'Or' or 'And'. Defaults to 'And'.
	#' @param searchSortFieldNamesList The list of fields sort results by. Defaults to NULL (unsorted).
	#' @param searchSortFieldNamesDescendingList A list of T/F values corresponding to whether to sort each field in searchSortFieldNamesList in descending order. Defaults to F for each FieldName in searchSortFieldNamesList.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param query Queries to be added to the request. Ex. list(SchoolYearID = 1). Defaults to NULL.
	#' @param page Results are paginated. The page of results to return. Default is 1.
	#' @param pageSize Results are paginated. The number of records per page to return. Default is 100,000 (essentially all records for most objects).
	#' @param flatten Whether to flatten results into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Student
	#' @return A list of StudentConfigEntityGroupYears
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	listStudentConfigEntityGroupYears <- function(searchConditionsList = NULL, ConfigEntityGroupYearID = F, EntityID = F, SchoolYearID = F, EntityGroupKey = F, DefaultAllowFeeManagementOnlinePaymentAccess = F, DefaultAllowFoodServiceOnlinePaymentAccess = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, FollettDestinyCustomEntityCode = F, StudentAccessAccountInfoEmailSubject = F, StudentAccessAccountInfoEmailBody = F, StudentAccessAccountInfoEmailIncludeResetPasswordText = F, FollettDestinyRoomTeacherType = F, FollettDestinyRoomTeacherPeriod = F, CourseSubjectIDFollettDestinyRoomTeacher = F, ConfigEntityGroupYearIDClonedFrom = F, fieldPaths = NULL, searchConditionsGroupType = "And", searchSortFieldNamesList = NULL, searchSortFieldNamesDescendingList = NULL, entityId = 1, query = NULL, page = 1, pageSize = 100000, flatten = T, returnResponse = F){

		params <- as.list(environment())

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		listSkyObjects(module = "Student", objectName = "ConfigEntityGroupYear", searchFields = searchFields %>% append(fieldPaths), page = page, pageSize = pageSize, SearchConditionsList = searchConditionsList, SearchConditionsGroupType = searchConditionsGroupType, SearchSortFieldNamesList = searchSortFieldNamesList, SearchSortFieldNamesDescendingList = searchSortFieldNamesDescendingList, entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' Get a StudentConfigEntityGroupYear
	#'
	#' This function returns a dataframe or json object of a StudentConfigEntityGroupYear
	#' @param StudentConfigEntityGroupYearID The ID of the StudentConfigEntityGroupYear to return.
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given StudentConfigEntityGroupYear. Defaults to FALSE for all return fields which, for convenience, returns all fields for the StudentConfigEntityGroupYear.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object listed as text. Run \code{\link{getSchemaForObjects}}('StudentConfigEntityGroupYear') to get more field paths.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param query Queries to be added to the request. Ex. list(SchoolYearID = 1). Defaults to NULL.
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Student
	#' @return A dataframe or of StudentConfigEntityGroupYear
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	getStudentConfigEntityGroupYear <- function(StudentConfigEntityGroupYearID, ConfigEntityGroupYearID = F, EntityID = F, SchoolYearID = F, EntityGroupKey = F, DefaultAllowFeeManagementOnlinePaymentAccess = F, DefaultAllowFoodServiceOnlinePaymentAccess = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, FollettDestinyCustomEntityCode = F, StudentAccessAccountInfoEmailSubject = F, StudentAccessAccountInfoEmailBody = F, StudentAccessAccountInfoEmailIncludeResetPasswordText = F, FollettDestinyRoomTeacherType = F, FollettDestinyRoomTeacherPeriod = F, CourseSubjectIDFollettDestinyRoomTeacher = F, ConfigEntityGroupYearIDClonedFrom = F, fieldPaths = NULL, entityId = 1, query = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment()) %>% purrr::keep(names(.) != "StudentConfigEntityGroupYearID")

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		getSkyObject(module = "Student", objectName = "ConfigEntityGroupYear", objectId = StudentConfigEntityGroupYearID, searchFields = searchFields, entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' Delete a StudentConfigEntityGroupYear
	#'
	#' This function deletes a StudentConfigEntityGroupYear
	#' @param StudentConfigEntityGroupYearID The ID of the StudentConfigEntityGroupYear to delete
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param query Queries to be added to the request. Ex. list(SchoolYearID = 1). Defaults to NULL.
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Student
	#' @return The StudentConfigEntityGroupYearID of the deleted StudentConfigEntityGroupYear.
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	deleteStudentConfigEntityGroupYear <- function(StudentConfigEntityGroupYearID, ignoreWarnings = F, entityId = 1, query = NULL, flatten = T, returnResponse = F){

		deleteSkyObject(module = "Student", objectName = "ConfigEntityGroupYear", objectId = StudentConfigEntityGroupYearID, ignoreWarnings = ignoreWarnings, entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' Create a StudentConfigEntityGroupYear
	#'
	#' This function creates a StudentConfigEntityGroupYear
	#' @param fieldNames The field values to give the created StudentConfigEntityGroupYear. Each defaults to NULL.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param query Queries to be added to the request. Ex. list(SchoolYearID = 1). Defaults to NULL.
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Student
	#' @return A newly created StudentConfigEntityGroupYear
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	createStudentConfigEntityGroupYear <- function(EntityID = NULL, SchoolYearID = NULL, EntityGroupKey = NULL, DefaultAllowFeeManagementOnlinePaymentAccess = NULL, DefaultAllowFoodServiceOnlinePaymentAccess = NULL, FollettDestinyCustomEntityCode = NULL, StudentAccessAccountInfoEmailSubject = NULL, StudentAccessAccountInfoEmailBody = NULL, StudentAccessAccountInfoEmailIncludeResetPasswordText = NULL, FollettDestinyRoomTeacherType = NULL, FollettDestinyRoomTeacherPeriod = NULL, CourseSubjectIDFollettDestinyRoomTeacher = NULL, ConfigEntityGroupYearIDClonedFrom = NULL, entityId = 1, query = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment())

		body <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper()) %>% purrr::compact()

		createSkyObject(module = "Student", objectName = "ConfigEntityGroupYear", body = list(DataObject = body), searchFields = append("ConfigEntityGroupYearID", body %>% names()), entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' Modify a StudentConfigEntityGroupYear
	#'
	#' This function modifies a StudentConfigEntityGroupYear
	#' @param fieldNames The field values to give the modified StudentConfigEntityGroupYear. Each defaults to NULL.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param query Queries to be added to the request. Ex. list(SchoolYearID = 1). Defaults to NULL.
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Student
	#' @return The modified StudentConfigEntityGroupYear
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	modifyStudentConfigEntityGroupYear <- function(ConfigEntityGroupYearID, EntityID = NULL, SchoolYearID = NULL, EntityGroupKey = NULL, DefaultAllowFeeManagementOnlinePaymentAccess = NULL, DefaultAllowFoodServiceOnlinePaymentAccess = NULL, FollettDestinyCustomEntityCode = NULL, StudentAccessAccountInfoEmailSubject = NULL, StudentAccessAccountInfoEmailBody = NULL, StudentAccessAccountInfoEmailIncludeResetPasswordText = NULL, FollettDestinyRoomTeacherType = NULL, FollettDestinyRoomTeacherPeriod = NULL, CourseSubjectIDFollettDestinyRoomTeacher = NULL, ConfigEntityGroupYearIDClonedFrom = NULL, entityId = 1, query = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment())

		body <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper()) %>% purrr::compact()

		modifySkyObject(module = "Student", objectName = "ConfigEntityGroupYear", objectId = ConfigEntityGroupYearID, body = list(DataObject = body), searchFields = append("ConfigEntityGroupYearID", body %>% names()), entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' List TempLockCombinations
	#'
	#' This function returns a dataframe or json object of TempLockCombinations
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given TempLockCombinations. Defaults to FALSE for all return fields which, for convenience, returns all fields for the TempLockCombinations.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object listed as text. Run \code{\link{getSchemaForObjects}}('TempLockCombination') to get more field paths.
	#' @param searchConditionsList A list of search conditions to filter results which are joined by the searchConditionsGroupType. Of the form {FieldName} {ConditionType} {SearchCondition}. For example, c('StudentID LessEqual 500', 'LastName Like Ander\%'). Run \code{\link{listSearchConditionTypes}} for a list of ConditionTypes. Defaults to NULL (unfiltered).
	#' @param searchConditionsGroupType The conjunction which joins multiple searchConditions in the searchConditionsList. Either 'Or' or 'And'. Defaults to 'And'.
	#' @param searchSortFieldNamesList The list of fields sort results by. Defaults to NULL (unsorted).
	#' @param searchSortFieldNamesDescendingList A list of T/F values corresponding to whether to sort each field in searchSortFieldNamesList in descending order. Defaults to F for each FieldName in searchSortFieldNamesList.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param query Queries to be added to the request. Ex. list(SchoolYearID = 1). Defaults to NULL.
	#' @param page Results are paginated. The page of results to return. Default is 1.
	#' @param pageSize Results are paginated. The number of records per page to return. Default is 100,000 (essentially all records for most objects).
	#' @param flatten Whether to flatten results into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Student
	#' @return A list of TempLockCombinations
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	listTempLockCombinations <- function(searchConditionsList = NULL, TempLockCombinationID = F, TempLockerID = F, Combination = F, CombinationNumber = F, FailureReason = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, fieldPaths = NULL, searchConditionsGroupType = "And", searchSortFieldNamesList = NULL, searchSortFieldNamesDescendingList = NULL, entityId = 1, query = NULL, page = 1, pageSize = 100000, flatten = T, returnResponse = F){

		params <- as.list(environment())

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		listSkyObjects(module = "Student", objectName = "TempLockCombination", searchFields = searchFields %>% append(fieldPaths), page = page, pageSize = pageSize, SearchConditionsList = searchConditionsList, SearchConditionsGroupType = searchConditionsGroupType, SearchSortFieldNamesList = searchSortFieldNamesList, SearchSortFieldNamesDescendingList = searchSortFieldNamesDescendingList, entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' Get a TempLockCombination
	#'
	#' This function returns a dataframe or json object of a TempLockCombination
	#' @param TempLockCombinationID The ID of the TempLockCombination to return.
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given TempLockCombination. Defaults to FALSE for all return fields which, for convenience, returns all fields for the TempLockCombination.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object listed as text. Run \code{\link{getSchemaForObjects}}('TempLockCombination') to get more field paths.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param query Queries to be added to the request. Ex. list(SchoolYearID = 1). Defaults to NULL.
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Student
	#' @return A dataframe or of TempLockCombination
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	getTempLockCombination <- function(TempLockCombinationID, TempLockerID = F, Combination = F, CombinationNumber = F, FailureReason = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, fieldPaths = NULL, entityId = 1, query = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment()) %>% purrr::keep(names(.) != "TempLockCombinationID")

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		getSkyObject(module = "Student", objectName = "TempLockCombination", objectId = TempLockCombinationID, searchFields = searchFields, entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' Delete a TempLockCombination
	#'
	#' This function deletes a TempLockCombination
	#' @param TempLockCombinationID The ID of the TempLockCombination to delete
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param query Queries to be added to the request. Ex. list(SchoolYearID = 1). Defaults to NULL.
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Student
	#' @return The TempLockCombinationID of the deleted TempLockCombination.
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	deleteTempLockCombination <- function(TempLockCombinationID, ignoreWarnings = F, entityId = 1, query = NULL, flatten = T, returnResponse = F){

		deleteSkyObject(module = "Student", objectName = "TempLockCombination", objectId = TempLockCombinationID, ignoreWarnings = ignoreWarnings, entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' Create a TempLockCombination
	#'
	#' This function creates a TempLockCombination
	#' @param fieldNames The field values to give the created TempLockCombination. Each defaults to NULL.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param query Queries to be added to the request. Ex. list(SchoolYearID = 1). Defaults to NULL.
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Student
	#' @return A newly created TempLockCombination
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	createTempLockCombination <- function(TempLockerID = NULL, Combination = NULL, CombinationNumber = NULL, FailureReason = NULL, entityId = 1, query = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment())

		body <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper()) %>% purrr::compact()

		createSkyObject(module = "Student", objectName = "TempLockCombination", body = list(DataObject = body), searchFields = append("TempLockCombinationID", body %>% names()), entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' Modify a TempLockCombination
	#'
	#' This function modifies a TempLockCombination
	#' @param fieldNames The field values to give the modified TempLockCombination. Each defaults to NULL.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param query Queries to be added to the request. Ex. list(SchoolYearID = 1). Defaults to NULL.
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Student
	#' @return The modified TempLockCombination
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	modifyTempLockCombination <- function(TempLockCombinationID, TempLockerID = NULL, Combination = NULL, CombinationNumber = NULL, FailureReason = NULL, entityId = 1, query = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment())

		body <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper()) %>% purrr::compact()

		modifySkyObject(module = "Student", objectName = "TempLockCombination", objectId = TempLockCombinationID, body = list(DataObject = body), searchFields = append("TempLockCombinationID", body %>% names()), entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' List TempLockers
	#'
	#' This function returns a dataframe or json object of TempLockers
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given TempLockers. Defaults to FALSE for all return fields which, for convenience, returns all fields for the TempLockers.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object listed as text. Run \code{\link{getSchemaForObjects}}('TempLocker') to get more field paths.
	#' @param searchConditionsList A list of search conditions to filter results which are joined by the searchConditionsGroupType. Of the form {FieldName} {ConditionType} {SearchCondition}. For example, c('StudentID LessEqual 500', 'LastName Like Ander\%'). Run \code{\link{listSearchConditionTypes}} for a list of ConditionTypes. Defaults to NULL (unfiltered).
	#' @param searchConditionsGroupType The conjunction which joins multiple searchConditions in the searchConditionsList. Either 'Or' or 'And'. Defaults to 'And'.
	#' @param searchSortFieldNamesList The list of fields sort results by. Defaults to NULL (unsorted).
	#' @param searchSortFieldNamesDescendingList A list of T/F values corresponding to whether to sort each field in searchSortFieldNamesList in descending order. Defaults to F for each FieldName in searchSortFieldNamesList.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param query Queries to be added to the request. Ex. list(SchoolYearID = 1). Defaults to NULL.
	#' @param page Results are paginated. The page of results to return. Default is 1.
	#' @param pageSize Results are paginated. The number of records per page to return. Default is 100,000 (essentially all records for most objects).
	#' @param flatten Whether to flatten results into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Student
	#' @return A list of TempLockers
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	listTempLockers <- function(searchConditionsList = NULL, TempLockerID = F, LockerNumber = F, BuildingID = F, Building = F, LockerAreaID = F, LockerArea = F, FailureReason = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, Comment = F, LockerNumberDigitLength = F, NewLockerNumber = F, LockerID = F, fieldPaths = NULL, searchConditionsGroupType = "And", searchSortFieldNamesList = NULL, searchSortFieldNamesDescendingList = NULL, entityId = 1, query = NULL, page = 1, pageSize = 100000, flatten = T, returnResponse = F){

		params <- as.list(environment())

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		listSkyObjects(module = "Student", objectName = "TempLocker", searchFields = searchFields %>% append(fieldPaths), page = page, pageSize = pageSize, SearchConditionsList = searchConditionsList, SearchConditionsGroupType = searchConditionsGroupType, SearchSortFieldNamesList = searchSortFieldNamesList, SearchSortFieldNamesDescendingList = searchSortFieldNamesDescendingList, entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' Get a TempLocker
	#'
	#' This function returns a dataframe or json object of a TempLocker
	#' @param TempLockerID The ID of the TempLocker to return.
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given TempLocker. Defaults to FALSE for all return fields which, for convenience, returns all fields for the TempLocker.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object listed as text. Run \code{\link{getSchemaForObjects}}('TempLocker') to get more field paths.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param query Queries to be added to the request. Ex. list(SchoolYearID = 1). Defaults to NULL.
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Student
	#' @return A dataframe or of TempLocker
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	getTempLocker <- function(TempLockerID, LockerNumber = F, BuildingID = F, Building = F, LockerAreaID = F, LockerArea = F, FailureReason = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, Comment = F, LockerNumberDigitLength = F, NewLockerNumber = F, LockerID = F, fieldPaths = NULL, entityId = 1, query = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment()) %>% purrr::keep(names(.) != "TempLockerID")

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		getSkyObject(module = "Student", objectName = "TempLocker", objectId = TempLockerID, searchFields = searchFields, entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' Delete a TempLocker
	#'
	#' This function deletes a TempLocker
	#' @param TempLockerID The ID of the TempLocker to delete
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param query Queries to be added to the request. Ex. list(SchoolYearID = 1). Defaults to NULL.
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Student
	#' @return The TempLockerID of the deleted TempLocker.
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	deleteTempLocker <- function(TempLockerID, ignoreWarnings = F, entityId = 1, query = NULL, flatten = T, returnResponse = F){

		deleteSkyObject(module = "Student", objectName = "TempLocker", objectId = TempLockerID, ignoreWarnings = ignoreWarnings, entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' Create a TempLocker
	#'
	#' This function creates a TempLocker
	#' @param fieldNames The field values to give the created TempLocker. Each defaults to NULL.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param query Queries to be added to the request. Ex. list(SchoolYearID = 1). Defaults to NULL.
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Student
	#' @return A newly created TempLocker
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	createTempLocker <- function(LockerNumber = NULL, BuildingID = NULL, Building = NULL, LockerAreaID = NULL, LockerArea = NULL, FailureReason = NULL, Comment = NULL, LockerNumberDigitLength = NULL, NewLockerNumber = NULL, LockerID = NULL, entityId = 1, query = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment())

		body <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper()) %>% purrr::compact()

		createSkyObject(module = "Student", objectName = "TempLocker", body = list(DataObject = body), searchFields = append("TempLockerID", body %>% names()), entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' Modify a TempLocker
	#'
	#' This function modifies a TempLocker
	#' @param fieldNames The field values to give the modified TempLocker. Each defaults to NULL.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param query Queries to be added to the request. Ex. list(SchoolYearID = 1). Defaults to NULL.
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Student
	#' @return The modified TempLocker
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	modifyTempLocker <- function(TempLockerID, LockerNumber = NULL, BuildingID = NULL, Building = NULL, LockerAreaID = NULL, LockerArea = NULL, FailureReason = NULL, Comment = NULL, LockerNumberDigitLength = NULL, NewLockerNumber = NULL, LockerID = NULL, entityId = 1, query = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment())

		body <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper()) %>% purrr::compact()

		modifySkyObject(module = "Student", objectName = "TempLocker", objectId = TempLockerID, body = list(DataObject = body), searchFields = append("TempLockerID", body %>% names()), entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' List StudentMassUpdates
	#'
	#' This function returns a dataframe or json object of StudentMassUpdates
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given StudentMassUpdates. Defaults to FALSE for all return fields which, for convenience, returns all fields for the StudentMassUpdates.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object listed as text. Run \code{\link{getSchemaForObjects}}('StudentMassUpdate') to get more field paths.
	#' @param searchConditionsList A list of search conditions to filter results which are joined by the searchConditionsGroupType. Of the form {FieldName} {ConditionType} {SearchCondition}. For example, c('StudentID LessEqual 500', 'LastName Like Ander\%'). Run \code{\link{listSearchConditionTypes}} for a list of ConditionTypes. Defaults to NULL (unfiltered).
	#' @param searchConditionsGroupType The conjunction which joins multiple searchConditions in the searchConditionsList. Either 'Or' or 'And'. Defaults to 'And'.
	#' @param searchSortFieldNamesList The list of fields sort results by. Defaults to NULL (unsorted).
	#' @param searchSortFieldNamesDescendingList A list of T/F values corresponding to whether to sort each field in searchSortFieldNamesList in descending order. Defaults to F for each FieldName in searchSortFieldNamesList.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param query Queries to be added to the request. Ex. list(SchoolYearID = 1). Defaults to NULL.
	#' @param page Results are paginated. The page of results to return. Default is 1.
	#' @param pageSize Results are paginated. The number of records per page to return. Default is 100,000 (essentially all records for most objects).
	#' @param flatten Whether to flatten results into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Student
	#' @return A list of StudentMassUpdates
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	listStudentMassUpdates <- function(searchConditionsList = NULL, StudentMassUpdateID = F, UserIDRanBy = F, SchoolYearID = F, AsOfDate = F, RunReason = F, FilterParameters = F, ValueParameters = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, DistrictID = F, fieldPaths = NULL, searchConditionsGroupType = "And", searchSortFieldNamesList = NULL, searchSortFieldNamesDescendingList = NULL, entityId = 1, query = NULL, page = 1, pageSize = 100000, flatten = T, returnResponse = F){

		params <- as.list(environment())

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		listSkyObjects(module = "Student", objectName = "StudentMassUpdate", searchFields = searchFields %>% append(fieldPaths), page = page, pageSize = pageSize, SearchConditionsList = searchConditionsList, SearchConditionsGroupType = searchConditionsGroupType, SearchSortFieldNamesList = searchSortFieldNamesList, SearchSortFieldNamesDescendingList = searchSortFieldNamesDescendingList, entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' Get a StudentMassUpdate
	#'
	#' This function returns a dataframe or json object of a StudentMassUpdate
	#' @param StudentMassUpdateID The ID of the StudentMassUpdate to return.
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given StudentMassUpdate. Defaults to FALSE for all return fields which, for convenience, returns all fields for the StudentMassUpdate.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object listed as text. Run \code{\link{getSchemaForObjects}}('StudentMassUpdate') to get more field paths.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param query Queries to be added to the request. Ex. list(SchoolYearID = 1). Defaults to NULL.
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Student
	#' @return A dataframe or of StudentMassUpdate
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	getStudentMassUpdate <- function(StudentMassUpdateID, UserIDRanBy = F, SchoolYearID = F, AsOfDate = F, RunReason = F, FilterParameters = F, ValueParameters = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, DistrictID = F, fieldPaths = NULL, entityId = 1, query = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment()) %>% purrr::keep(names(.) != "StudentMassUpdateID")

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		getSkyObject(module = "Student", objectName = "StudentMassUpdate", objectId = StudentMassUpdateID, searchFields = searchFields, entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' Delete a StudentMassUpdate
	#'
	#' This function deletes a StudentMassUpdate
	#' @param StudentMassUpdateID The ID of the StudentMassUpdate to delete
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param query Queries to be added to the request. Ex. list(SchoolYearID = 1). Defaults to NULL.
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Student
	#' @return The StudentMassUpdateID of the deleted StudentMassUpdate.
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	deleteStudentMassUpdate <- function(StudentMassUpdateID, ignoreWarnings = F, entityId = 1, query = NULL, flatten = T, returnResponse = F){

		deleteSkyObject(module = "Student", objectName = "StudentMassUpdate", objectId = StudentMassUpdateID, ignoreWarnings = ignoreWarnings, entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' Create a StudentMassUpdate
	#'
	#' This function creates a StudentMassUpdate
	#' @param fieldNames The field values to give the created StudentMassUpdate. Each defaults to NULL.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param query Queries to be added to the request. Ex. list(SchoolYearID = 1). Defaults to NULL.
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Student
	#' @return A newly created StudentMassUpdate
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	createStudentMassUpdate <- function(UserIDRanBy = NULL, SchoolYearID = NULL, AsOfDate = NULL, RunReason = NULL, FilterParameters = NULL, ValueParameters = NULL, DistrictID = NULL, entityId = 1, query = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment())

		body <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper()) %>% purrr::compact()

		createSkyObject(module = "Student", objectName = "StudentMassUpdate", body = list(DataObject = body), searchFields = append("StudentMassUpdateID", body %>% names()), entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' Modify a StudentMassUpdate
	#'
	#' This function modifies a StudentMassUpdate
	#' @param fieldNames The field values to give the modified StudentMassUpdate. Each defaults to NULL.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param query Queries to be added to the request. Ex. list(SchoolYearID = 1). Defaults to NULL.
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Student
	#' @return The modified StudentMassUpdate
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	modifyStudentMassUpdate <- function(StudentMassUpdateID, UserIDRanBy = NULL, SchoolYearID = NULL, AsOfDate = NULL, RunReason = NULL, FilterParameters = NULL, ValueParameters = NULL, DistrictID = NULL, entityId = 1, query = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment())

		body <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper()) %>% purrr::compact()

		modifySkyObject(module = "Student", objectName = "StudentMassUpdate", objectId = StudentMassUpdateID, body = list(DataObject = body), searchFields = append("StudentMassUpdateID", body %>% names()), entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' List TempStudentMassUpdateErrors
	#'
	#' This function returns a dataframe or json object of TempStudentMassUpdateErrors
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given TempStudentMassUpdateErrors. Defaults to FALSE for all return fields which, for convenience, returns all fields for the TempStudentMassUpdateErrors.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object listed as text. Run \code{\link{getSchemaForObjects}}('TempStudentMassUpdateError') to get more field paths.
	#' @param searchConditionsList A list of search conditions to filter results which are joined by the searchConditionsGroupType. Of the form {FieldName} {ConditionType} {SearchCondition}. For example, c('StudentID LessEqual 500', 'LastName Like Ander\%'). Run \code{\link{listSearchConditionTypes}} for a list of ConditionTypes. Defaults to NULL (unfiltered).
	#' @param searchConditionsGroupType The conjunction which joins multiple searchConditions in the searchConditionsList. Either 'Or' or 'And'. Defaults to 'And'.
	#' @param searchSortFieldNamesList The list of fields sort results by. Defaults to NULL (unsorted).
	#' @param searchSortFieldNamesDescendingList A list of T/F values corresponding to whether to sort each field in searchSortFieldNamesList in descending order. Defaults to F for each FieldName in searchSortFieldNamesList.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param query Queries to be added to the request. Ex. list(SchoolYearID = 1). Defaults to NULL.
	#' @param page Results are paginated. The page of results to return. Default is 1.
	#' @param pageSize Results are paginated. The number of records per page to return. Default is 100,000 (essentially all records for most objects).
	#' @param flatten Whether to flatten results into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Student
	#' @return A list of TempStudentMassUpdateErrors
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	listTempStudentMassUpdateErrors <- function(searchConditionsList = NULL, TempStudentMassUpdateErrorID = F, FullNameLFM = F, Type = F, FailureReason = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, fieldPaths = NULL, searchConditionsGroupType = "And", searchSortFieldNamesList = NULL, searchSortFieldNamesDescendingList = NULL, entityId = 1, query = NULL, page = 1, pageSize = 100000, flatten = T, returnResponse = F){

		params <- as.list(environment())

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		listSkyObjects(module = "Student", objectName = "TempStudentMassUpdateError", searchFields = searchFields %>% append(fieldPaths), page = page, pageSize = pageSize, SearchConditionsList = searchConditionsList, SearchConditionsGroupType = searchConditionsGroupType, SearchSortFieldNamesList = searchSortFieldNamesList, SearchSortFieldNamesDescendingList = searchSortFieldNamesDescendingList, entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' Get a TempStudentMassUpdateError
	#'
	#' This function returns a dataframe or json object of a TempStudentMassUpdateError
	#' @param TempStudentMassUpdateErrorID The ID of the TempStudentMassUpdateError to return.
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given TempStudentMassUpdateError. Defaults to FALSE for all return fields which, for convenience, returns all fields for the TempStudentMassUpdateError.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object listed as text. Run \code{\link{getSchemaForObjects}}('TempStudentMassUpdateError') to get more field paths.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param query Queries to be added to the request. Ex. list(SchoolYearID = 1). Defaults to NULL.
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Student
	#' @return A dataframe or of TempStudentMassUpdateError
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	getTempStudentMassUpdateError <- function(TempStudentMassUpdateErrorID, FullNameLFM = F, Type = F, FailureReason = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, fieldPaths = NULL, entityId = 1, query = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment()) %>% purrr::keep(names(.) != "TempStudentMassUpdateErrorID")

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		getSkyObject(module = "Student", objectName = "TempStudentMassUpdateError", objectId = TempStudentMassUpdateErrorID, searchFields = searchFields, entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' Delete a TempStudentMassUpdateError
	#'
	#' This function deletes a TempStudentMassUpdateError
	#' @param TempStudentMassUpdateErrorID The ID of the TempStudentMassUpdateError to delete
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param query Queries to be added to the request. Ex. list(SchoolYearID = 1). Defaults to NULL.
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Student
	#' @return The TempStudentMassUpdateErrorID of the deleted TempStudentMassUpdateError.
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	deleteTempStudentMassUpdateError <- function(TempStudentMassUpdateErrorID, ignoreWarnings = F, entityId = 1, query = NULL, flatten = T, returnResponse = F){

		deleteSkyObject(module = "Student", objectName = "TempStudentMassUpdateError", objectId = TempStudentMassUpdateErrorID, ignoreWarnings = ignoreWarnings, entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' Create a TempStudentMassUpdateError
	#'
	#' This function creates a TempStudentMassUpdateError
	#' @param fieldNames The field values to give the created TempStudentMassUpdateError. Each defaults to NULL.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param query Queries to be added to the request. Ex. list(SchoolYearID = 1). Defaults to NULL.
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Student
	#' @return A newly created TempStudentMassUpdateError
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	createTempStudentMassUpdateError <- function(FullNameLFM = NULL, Type = NULL, FailureReason = NULL, entityId = 1, query = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment())

		body <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper()) %>% purrr::compact()

		createSkyObject(module = "Student", objectName = "TempStudentMassUpdateError", body = list(DataObject = body), searchFields = append("TempStudentMassUpdateErrorID", body %>% names()), entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' Modify a TempStudentMassUpdateError
	#'
	#' This function modifies a TempStudentMassUpdateError
	#' @param fieldNames The field values to give the modified TempStudentMassUpdateError. Each defaults to NULL.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param query Queries to be added to the request. Ex. list(SchoolYearID = 1). Defaults to NULL.
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Student
	#' @return The modified TempStudentMassUpdateError
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	modifyTempStudentMassUpdateError <- function(TempStudentMassUpdateErrorID, FullNameLFM = NULL, Type = NULL, FailureReason = NULL, entityId = 1, query = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment())

		body <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper()) %>% purrr::compact()

		modifySkyObject(module = "Student", objectName = "TempStudentMassUpdateError", objectId = TempStudentMassUpdateErrorID, body = list(DataObject = body), searchFields = append("TempStudentMassUpdateErrorID", body %>% names()), entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' List TempStudentMassUpdateFields
	#'
	#' This function returns a dataframe or json object of TempStudentMassUpdateFields
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given TempStudentMassUpdateFields. Defaults to FALSE for all return fields which, for convenience, returns all fields for the TempStudentMassUpdateFields.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object listed as text. Run \code{\link{getSchemaForObjects}}('TempStudentMassUpdateField') to get more field paths.
	#' @param searchConditionsList A list of search conditions to filter results which are joined by the searchConditionsGroupType. Of the form {FieldName} {ConditionType} {SearchCondition}. For example, c('StudentID LessEqual 500', 'LastName Like Ander\%'). Run \code{\link{listSearchConditionTypes}} for a list of ConditionTypes. Defaults to NULL (unfiltered).
	#' @param searchConditionsGroupType The conjunction which joins multiple searchConditions in the searchConditionsList. Either 'Or' or 'And'. Defaults to 'And'.
	#' @param searchSortFieldNamesList The list of fields sort results by. Defaults to NULL (unsorted).
	#' @param searchSortFieldNamesDescendingList A list of T/F values corresponding to whether to sort each field in searchSortFieldNamesList in descending order. Defaults to F for each FieldName in searchSortFieldNamesList.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param query Queries to be added to the request. Ex. list(SchoolYearID = 1). Defaults to NULL.
	#' @param page Results are paginated. The page of results to return. Default is 1.
	#' @param pageSize Results are paginated. The number of records per page to return. Default is 100,000 (essentially all records for most objects).
	#' @param flatten Whether to flatten results into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Student
	#' @return A list of TempStudentMassUpdateFields
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	listTempStudentMassUpdateFields <- function(searchConditionsList = NULL, TempStudentMassUpdateFieldID = F, StudentID = F, AffectedPrimaryKey = F, FullNameLFM = F, FieldName = F, OriginalValue = F, FriendlyOriginalValue = F, UpdatedValue = F, FriendlyUpdatedValue = F, RelatedType = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, fieldPaths = NULL, searchConditionsGroupType = "And", searchSortFieldNamesList = NULL, searchSortFieldNamesDescendingList = NULL, entityId = 1, query = NULL, page = 1, pageSize = 100000, flatten = T, returnResponse = F){

		params <- as.list(environment())

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		listSkyObjects(module = "Student", objectName = "TempStudentMassUpdateField", searchFields = searchFields %>% append(fieldPaths), page = page, pageSize = pageSize, SearchConditionsList = searchConditionsList, SearchConditionsGroupType = searchConditionsGroupType, SearchSortFieldNamesList = searchSortFieldNamesList, SearchSortFieldNamesDescendingList = searchSortFieldNamesDescendingList, entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' Get a TempStudentMassUpdateField
	#'
	#' This function returns a dataframe or json object of a TempStudentMassUpdateField
	#' @param TempStudentMassUpdateFieldID The ID of the TempStudentMassUpdateField to return.
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given TempStudentMassUpdateField. Defaults to FALSE for all return fields which, for convenience, returns all fields for the TempStudentMassUpdateField.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object listed as text. Run \code{\link{getSchemaForObjects}}('TempStudentMassUpdateField') to get more field paths.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param query Queries to be added to the request. Ex. list(SchoolYearID = 1). Defaults to NULL.
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Student
	#' @return A dataframe or of TempStudentMassUpdateField
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	getTempStudentMassUpdateField <- function(TempStudentMassUpdateFieldID, StudentID = F, AffectedPrimaryKey = F, FullNameLFM = F, FieldName = F, OriginalValue = F, FriendlyOriginalValue = F, UpdatedValue = F, FriendlyUpdatedValue = F, RelatedType = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, fieldPaths = NULL, entityId = 1, query = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment()) %>% purrr::keep(names(.) != "TempStudentMassUpdateFieldID")

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		getSkyObject(module = "Student", objectName = "TempStudentMassUpdateField", objectId = TempStudentMassUpdateFieldID, searchFields = searchFields, entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' Delete a TempStudentMassUpdateField
	#'
	#' This function deletes a TempStudentMassUpdateField
	#' @param TempStudentMassUpdateFieldID The ID of the TempStudentMassUpdateField to delete
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param query Queries to be added to the request. Ex. list(SchoolYearID = 1). Defaults to NULL.
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Student
	#' @return The TempStudentMassUpdateFieldID of the deleted TempStudentMassUpdateField.
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	deleteTempStudentMassUpdateField <- function(TempStudentMassUpdateFieldID, ignoreWarnings = F, entityId = 1, query = NULL, flatten = T, returnResponse = F){

		deleteSkyObject(module = "Student", objectName = "TempStudentMassUpdateField", objectId = TempStudentMassUpdateFieldID, ignoreWarnings = ignoreWarnings, entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' Create a TempStudentMassUpdateField
	#'
	#' This function creates a TempStudentMassUpdateField
	#' @param fieldNames The field values to give the created TempStudentMassUpdateField. Each defaults to NULL.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param query Queries to be added to the request. Ex. list(SchoolYearID = 1). Defaults to NULL.
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Student
	#' @return A newly created TempStudentMassUpdateField
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	createTempStudentMassUpdateField <- function(StudentID = NULL, AffectedPrimaryKey = NULL, FullNameLFM = NULL, FieldName = NULL, OriginalValue = NULL, FriendlyOriginalValue = NULL, UpdatedValue = NULL, FriendlyUpdatedValue = NULL, RelatedType = NULL, entityId = 1, query = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment())

		body <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper()) %>% purrr::compact()

		createSkyObject(module = "Student", objectName = "TempStudentMassUpdateField", body = list(DataObject = body), searchFields = append("TempStudentMassUpdateFieldID", body %>% names()), entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' Modify a TempStudentMassUpdateField
	#'
	#' This function modifies a TempStudentMassUpdateField
	#' @param fieldNames The field values to give the modified TempStudentMassUpdateField. Each defaults to NULL.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param query Queries to be added to the request. Ex. list(SchoolYearID = 1). Defaults to NULL.
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Student
	#' @return The modified TempStudentMassUpdateField
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	modifyTempStudentMassUpdateField <- function(TempStudentMassUpdateFieldID, StudentID = NULL, AffectedPrimaryKey = NULL, FullNameLFM = NULL, FieldName = NULL, OriginalValue = NULL, FriendlyOriginalValue = NULL, UpdatedValue = NULL, FriendlyUpdatedValue = NULL, RelatedType = NULL, entityId = 1, query = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment())

		body <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper()) %>% purrr::compact()

		modifySkyObject(module = "Student", objectName = "TempStudentMassUpdateField", objectId = TempStudentMassUpdateFieldID, body = list(DataObject = body), searchFields = append("TempStudentMassUpdateFieldID", body %>% names()), entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' List TempStudentMergeObjects
	#'
	#' This function returns a dataframe or json object of TempStudentMergeObjects
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given TempStudentMergeObjects. Defaults to FALSE for all return fields which, for convenience, returns all fields for the TempStudentMergeObjects.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object listed as text. Run \code{\link{getSchemaForObjects}}('TempStudentMergeObject') to get more field paths.
	#' @param searchConditionsList A list of search conditions to filter results which are joined by the searchConditionsGroupType. Of the form {FieldName} {ConditionType} {SearchCondition}. For example, c('StudentID LessEqual 500', 'LastName Like Ander\%'). Run \code{\link{listSearchConditionTypes}} for a list of ConditionTypes. Defaults to NULL (unfiltered).
	#' @param searchConditionsGroupType The conjunction which joins multiple searchConditions in the searchConditionsList. Either 'Or' or 'And'. Defaults to 'And'.
	#' @param searchSortFieldNamesList The list of fields sort results by. Defaults to NULL (unsorted).
	#' @param searchSortFieldNamesDescendingList A list of T/F values corresponding to whether to sort each field in searchSortFieldNamesList in descending order. Defaults to F for each FieldName in searchSortFieldNamesList.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param query Queries to be added to the request. Ex. list(SchoolYearID = 1). Defaults to NULL.
	#' @param page Results are paginated. The page of results to return. Default is 1.
	#' @param pageSize Results are paginated. The number of records per page to return. Default is 100,000 (essentially all records for most objects).
	#' @param flatten Whether to flatten results into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Student
	#' @return A list of TempStudentMergeObjects
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	listTempStudentMergeObjects <- function(searchConditionsList = NULL, TempStudentMergeObjectID = F, FieldToShow = F, RecordType = F, HandlingType = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, fieldPaths = NULL, searchConditionsGroupType = "And", searchSortFieldNamesList = NULL, searchSortFieldNamesDescendingList = NULL, entityId = 1, query = NULL, page = 1, pageSize = 100000, flatten = T, returnResponse = F){

		params <- as.list(environment())

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		listSkyObjects(module = "Student", objectName = "TempStudentMergeObject", searchFields = searchFields %>% append(fieldPaths), page = page, pageSize = pageSize, SearchConditionsList = searchConditionsList, SearchConditionsGroupType = searchConditionsGroupType, SearchSortFieldNamesList = searchSortFieldNamesList, SearchSortFieldNamesDescendingList = searchSortFieldNamesDescendingList, entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' Get a TempStudentMergeObject
	#'
	#' This function returns a dataframe or json object of a TempStudentMergeObject
	#' @param TempStudentMergeObjectID The ID of the TempStudentMergeObject to return.
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given TempStudentMergeObject. Defaults to FALSE for all return fields which, for convenience, returns all fields for the TempStudentMergeObject.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object listed as text. Run \code{\link{getSchemaForObjects}}('TempStudentMergeObject') to get more field paths.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param query Queries to be added to the request. Ex. list(SchoolYearID = 1). Defaults to NULL.
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Student
	#' @return A dataframe or of TempStudentMergeObject
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	getTempStudentMergeObject <- function(TempStudentMergeObjectID, FieldToShow = F, RecordType = F, HandlingType = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, fieldPaths = NULL, entityId = 1, query = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment()) %>% purrr::keep(names(.) != "TempStudentMergeObjectID")

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		getSkyObject(module = "Student", objectName = "TempStudentMergeObject", objectId = TempStudentMergeObjectID, searchFields = searchFields, entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' Delete a TempStudentMergeObject
	#'
	#' This function deletes a TempStudentMergeObject
	#' @param TempStudentMergeObjectID The ID of the TempStudentMergeObject to delete
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param query Queries to be added to the request. Ex. list(SchoolYearID = 1). Defaults to NULL.
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Student
	#' @return The TempStudentMergeObjectID of the deleted TempStudentMergeObject.
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	deleteTempStudentMergeObject <- function(TempStudentMergeObjectID, ignoreWarnings = F, entityId = 1, query = NULL, flatten = T, returnResponse = F){

		deleteSkyObject(module = "Student", objectName = "TempStudentMergeObject", objectId = TempStudentMergeObjectID, ignoreWarnings = ignoreWarnings, entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' Create a TempStudentMergeObject
	#'
	#' This function creates a TempStudentMergeObject
	#' @param fieldNames The field values to give the created TempStudentMergeObject. Each defaults to NULL.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param query Queries to be added to the request. Ex. list(SchoolYearID = 1). Defaults to NULL.
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Student
	#' @return A newly created TempStudentMergeObject
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	createTempStudentMergeObject <- function(FieldToShow = NULL, RecordType = NULL, HandlingType = NULL, entityId = 1, query = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment())

		body <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper()) %>% purrr::compact()

		createSkyObject(module = "Student", objectName = "TempStudentMergeObject", body = list(DataObject = body), searchFields = append("TempStudentMergeObjectID", body %>% names()), entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' Modify a TempStudentMergeObject
	#'
	#' This function modifies a TempStudentMergeObject
	#' @param fieldNames The field values to give the modified TempStudentMergeObject. Each defaults to NULL.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param query Queries to be added to the request. Ex. list(SchoolYearID = 1). Defaults to NULL.
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Student
	#' @return The modified TempStudentMergeObject
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	modifyTempStudentMergeObject <- function(TempStudentMergeObjectID, FieldToShow = NULL, RecordType = NULL, HandlingType = NULL, entityId = 1, query = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment())

		body <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper()) %>% purrr::compact()

		modifySkyObject(module = "Student", objectName = "TempStudentMergeObject", objectId = TempStudentMergeObjectID, body = list(DataObject = body), searchFields = append("TempStudentMergeObjectID", body %>% names()), entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' List StudentConfigDistricts
	#'
	#' This function returns a dataframe or json object of StudentConfigDistricts
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given StudentConfigDistricts. Defaults to FALSE for all return fields which, for convenience, returns all fields for the StudentConfigDistricts.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object listed as text. Run \code{\link{getSchemaForObjects}}('StudentConfigDistrict') to get more field paths.
	#' @param searchConditionsList A list of search conditions to filter results which are joined by the searchConditionsGroupType. Of the form {FieldName} {ConditionType} {SearchCondition}. For example, c('StudentID LessEqual 500', 'LastName Like Ander\%'). Run \code{\link{listSearchConditionTypes}} for a list of ConditionTypes. Defaults to NULL (unfiltered).
	#' @param searchConditionsGroupType The conjunction which joins multiple searchConditions in the searchConditionsList. Either 'Or' or 'And'. Defaults to 'And'.
	#' @param searchSortFieldNamesList The list of fields sort results by. Defaults to NULL (unsorted).
	#' @param searchSortFieldNamesDescendingList A list of T/F values corresponding to whether to sort each field in searchSortFieldNamesList in descending order. Defaults to F for each FieldName in searchSortFieldNamesList.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param query Queries to be added to the request. Ex. list(SchoolYearID = 1). Defaults to NULL.
	#' @param page Results are paginated. The page of results to return. Default is 1.
	#' @param pageSize Results are paginated. The number of records per page to return. Default is 100,000 (essentially all records for most objects).
	#' @param flatten Whether to flatten results into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Student
	#' @return A list of StudentConfigDistricts
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	listStudentConfigDistricts <- function(searchConditionsList = NULL, ConfigDistrictID = F, DistrictID = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, EnableIEPWriterDocumentFunctions = F, IEPWriterDocumentSecurityToken = F, ShowIEPWriterIEPLinkOnIndicatorPopup = F, ShowIEPWriterIEP504LinkOnIndicatorPopup = F, FileDestinationIDSchoolMint = F, EmbraceImportFileName = F, FileDestinationIDEmbraceImport = F, fieldPaths = NULL, searchConditionsGroupType = "And", searchSortFieldNamesList = NULL, searchSortFieldNamesDescendingList = NULL, entityId = 1, query = NULL, page = 1, pageSize = 100000, flatten = T, returnResponse = F){

		params <- as.list(environment())

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		listSkyObjects(module = "Student", objectName = "ConfigDistrict", searchFields = searchFields %>% append(fieldPaths), page = page, pageSize = pageSize, SearchConditionsList = searchConditionsList, SearchConditionsGroupType = searchConditionsGroupType, SearchSortFieldNamesList = searchSortFieldNamesList, SearchSortFieldNamesDescendingList = searchSortFieldNamesDescendingList, entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' Get a StudentConfigDistrict
	#'
	#' This function returns a dataframe or json object of a StudentConfigDistrict
	#' @param StudentConfigDistrictID The ID of the StudentConfigDistrict to return.
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given StudentConfigDistrict. Defaults to FALSE for all return fields which, for convenience, returns all fields for the StudentConfigDistrict.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object listed as text. Run \code{\link{getSchemaForObjects}}('StudentConfigDistrict') to get more field paths.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param query Queries to be added to the request. Ex. list(SchoolYearID = 1). Defaults to NULL.
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Student
	#' @return A dataframe or of StudentConfigDistrict
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	getStudentConfigDistrict <- function(StudentConfigDistrictID, ConfigDistrictID = F, DistrictID = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, EnableIEPWriterDocumentFunctions = F, IEPWriterDocumentSecurityToken = F, ShowIEPWriterIEPLinkOnIndicatorPopup = F, ShowIEPWriterIEP504LinkOnIndicatorPopup = F, FileDestinationIDSchoolMint = F, EmbraceImportFileName = F, FileDestinationIDEmbraceImport = F, fieldPaths = NULL, entityId = 1, query = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment()) %>% purrr::keep(names(.) != "StudentConfigDistrictID")

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		getSkyObject(module = "Student", objectName = "ConfigDistrict", objectId = StudentConfigDistrictID, searchFields = searchFields, entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' Delete a StudentConfigDistrict
	#'
	#' This function deletes a StudentConfigDistrict
	#' @param StudentConfigDistrictID The ID of the StudentConfigDistrict to delete
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param query Queries to be added to the request. Ex. list(SchoolYearID = 1). Defaults to NULL.
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Student
	#' @return The StudentConfigDistrictID of the deleted StudentConfigDistrict.
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	deleteStudentConfigDistrict <- function(StudentConfigDistrictID, ignoreWarnings = F, entityId = 1, query = NULL, flatten = T, returnResponse = F){

		deleteSkyObject(module = "Student", objectName = "ConfigDistrict", objectId = StudentConfigDistrictID, ignoreWarnings = ignoreWarnings, entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' Create a StudentConfigDistrict
	#'
	#' This function creates a StudentConfigDistrict
	#' @param fieldNames The field values to give the created StudentConfigDistrict. Each defaults to NULL.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param query Queries to be added to the request. Ex. list(SchoolYearID = 1). Defaults to NULL.
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Student
	#' @return A newly created StudentConfigDistrict
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	createStudentConfigDistrict <- function(DistrictID = NULL, EnableIEPWriterDocumentFunctions = NULL, ShowIEPWriterIEPLinkOnIndicatorPopup = NULL, ShowIEPWriterIEP504LinkOnIndicatorPopup = NULL, FileDestinationIDSchoolMint = NULL, EmbraceImportFileName = NULL, FileDestinationIDEmbraceImport = NULL, entityId = 1, query = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment())

		body <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper()) %>% purrr::compact()

		createSkyObject(module = "Student", objectName = "ConfigDistrict", body = list(DataObject = body), searchFields = append("ConfigDistrictID", body %>% names()), entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' Modify a StudentConfigDistrict
	#'
	#' This function modifies a StudentConfigDistrict
	#' @param fieldNames The field values to give the modified StudentConfigDistrict. Each defaults to NULL.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param query Queries to be added to the request. Ex. list(SchoolYearID = 1). Defaults to NULL.
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Student
	#' @return The modified StudentConfigDistrict
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	modifyStudentConfigDistrict <- function(ConfigDistrictID, DistrictID = NULL, EnableIEPWriterDocumentFunctions = NULL, ShowIEPWriterIEPLinkOnIndicatorPopup = NULL, ShowIEPWriterIEP504LinkOnIndicatorPopup = NULL, FileDestinationIDSchoolMint = NULL, EmbraceImportFileName = NULL, FileDestinationIDEmbraceImport = NULL, entityId = 1, query = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment())

		body <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper()) %>% purrr::compact()

		modifySkyObject(module = "Student", objectName = "ConfigDistrict", objectId = ConfigDistrictID, body = list(DataObject = body), searchFields = append("ConfigDistrictID", body %>% names()), entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' List SkylertAttendanceExportSettingScheduledTasks
	#'
	#' This function returns a dataframe or json object of SkylertAttendanceExportSettingScheduledTasks
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given SkylertAttendanceExportSettingScheduledTasks. Defaults to FALSE for all return fields which, for convenience, returns all fields for the SkylertAttendanceExportSettingScheduledTasks.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object listed as text. Run \code{\link{getSchemaForObjects}}('SkylertAttendanceExportSettingScheduledTask') to get more field paths.
	#' @param searchConditionsList A list of search conditions to filter results which are joined by the searchConditionsGroupType. Of the form {FieldName} {ConditionType} {SearchCondition}. For example, c('StudentID LessEqual 500', 'LastName Like Ander\%'). Run \code{\link{listSearchConditionTypes}} for a list of ConditionTypes. Defaults to NULL (unfiltered).
	#' @param searchConditionsGroupType The conjunction which joins multiple searchConditions in the searchConditionsList. Either 'Or' or 'And'. Defaults to 'And'.
	#' @param searchSortFieldNamesList The list of fields sort results by. Defaults to NULL (unsorted).
	#' @param searchSortFieldNamesDescendingList A list of T/F values corresponding to whether to sort each field in searchSortFieldNamesList in descending order. Defaults to F for each FieldName in searchSortFieldNamesList.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param query Queries to be added to the request. Ex. list(SchoolYearID = 1). Defaults to NULL.
	#' @param page Results are paginated. The page of results to return. Default is 1.
	#' @param pageSize Results are paginated. The number of records per page to return. Default is 100,000 (essentially all records for most objects).
	#' @param flatten Whether to flatten results into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Student
	#' @return A list of SkylertAttendanceExportSettingScheduledTasks
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	listSkylertAttendanceExportSettingScheduledTasks <- function(searchConditionsList = NULL, SkylertAttendanceExportSettingScheduledTaskID = F, SkylertAttendanceExportSettingID = F, ScheduledTaskID = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, fieldPaths = NULL, searchConditionsGroupType = "And", searchSortFieldNamesList = NULL, searchSortFieldNamesDescendingList = NULL, entityId = 1, query = NULL, page = 1, pageSize = 100000, flatten = T, returnResponse = F){

		params <- as.list(environment())

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		listSkyObjects(module = "Student", objectName = "SkylertAttendanceExportSettingScheduledTask", searchFields = searchFields %>% append(fieldPaths), page = page, pageSize = pageSize, SearchConditionsList = searchConditionsList, SearchConditionsGroupType = searchConditionsGroupType, SearchSortFieldNamesList = searchSortFieldNamesList, SearchSortFieldNamesDescendingList = searchSortFieldNamesDescendingList, entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' Get a SkylertAttendanceExportSettingScheduledTask
	#'
	#' This function returns a dataframe or json object of a SkylertAttendanceExportSettingScheduledTask
	#' @param SkylertAttendanceExportSettingScheduledTaskID The ID of the SkylertAttendanceExportSettingScheduledTask to return.
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given SkylertAttendanceExportSettingScheduledTask. Defaults to FALSE for all return fields which, for convenience, returns all fields for the SkylertAttendanceExportSettingScheduledTask.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object listed as text. Run \code{\link{getSchemaForObjects}}('SkylertAttendanceExportSettingScheduledTask') to get more field paths.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param query Queries to be added to the request. Ex. list(SchoolYearID = 1). Defaults to NULL.
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Student
	#' @return A dataframe or of SkylertAttendanceExportSettingScheduledTask
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	getSkylertAttendanceExportSettingScheduledTask <- function(SkylertAttendanceExportSettingScheduledTaskID, SkylertAttendanceExportSettingID = F, ScheduledTaskID = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, fieldPaths = NULL, entityId = 1, query = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment()) %>% purrr::keep(names(.) != "SkylertAttendanceExportSettingScheduledTaskID")

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		getSkyObject(module = "Student", objectName = "SkylertAttendanceExportSettingScheduledTask", objectId = SkylertAttendanceExportSettingScheduledTaskID, searchFields = searchFields, entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' Delete a SkylertAttendanceExportSettingScheduledTask
	#'
	#' This function deletes a SkylertAttendanceExportSettingScheduledTask
	#' @param SkylertAttendanceExportSettingScheduledTaskID The ID of the SkylertAttendanceExportSettingScheduledTask to delete
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param query Queries to be added to the request. Ex. list(SchoolYearID = 1). Defaults to NULL.
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Student
	#' @return The SkylertAttendanceExportSettingScheduledTaskID of the deleted SkylertAttendanceExportSettingScheduledTask.
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	deleteSkylertAttendanceExportSettingScheduledTask <- function(SkylertAttendanceExportSettingScheduledTaskID, ignoreWarnings = F, entityId = 1, query = NULL, flatten = T, returnResponse = F){

		deleteSkyObject(module = "Student", objectName = "SkylertAttendanceExportSettingScheduledTask", objectId = SkylertAttendanceExportSettingScheduledTaskID, ignoreWarnings = ignoreWarnings, entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' Create a SkylertAttendanceExportSettingScheduledTask
	#'
	#' This function creates a SkylertAttendanceExportSettingScheduledTask
	#' @param fieldNames The field values to give the created SkylertAttendanceExportSettingScheduledTask. Each defaults to NULL.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param query Queries to be added to the request. Ex. list(SchoolYearID = 1). Defaults to NULL.
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Student
	#' @return A newly created SkylertAttendanceExportSettingScheduledTask
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	createSkylertAttendanceExportSettingScheduledTask <- function(SkylertAttendanceExportSettingID = NULL, ScheduledTaskID = NULL, entityId = 1, query = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment())

		body <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper()) %>% purrr::compact()

		createSkyObject(module = "Student", objectName = "SkylertAttendanceExportSettingScheduledTask", body = list(DataObject = body), searchFields = append("SkylertAttendanceExportSettingScheduledTaskID", body %>% names()), entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' Modify a SkylertAttendanceExportSettingScheduledTask
	#'
	#' This function modifies a SkylertAttendanceExportSettingScheduledTask
	#' @param fieldNames The field values to give the modified SkylertAttendanceExportSettingScheduledTask. Each defaults to NULL.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param query Queries to be added to the request. Ex. list(SchoolYearID = 1). Defaults to NULL.
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Student
	#' @return The modified SkylertAttendanceExportSettingScheduledTask
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	modifySkylertAttendanceExportSettingScheduledTask <- function(SkylertAttendanceExportSettingScheduledTaskID, SkylertAttendanceExportSettingID = NULL, ScheduledTaskID = NULL, entityId = 1, query = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment())

		body <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper()) %>% purrr::compact()

		modifySkyObject(module = "Student", objectName = "SkylertAttendanceExportSettingScheduledTask", objectId = SkylertAttendanceExportSettingScheduledTaskID, body = list(DataObject = body), searchFields = append("SkylertAttendanceExportSettingScheduledTaskID", body %>% names()), entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' List TempStudentErrorMessages
	#'
	#' This function returns a dataframe or json object of TempStudentErrorMessages
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given TempStudentErrorMessages. Defaults to FALSE for all return fields which, for convenience, returns all fields for the TempStudentErrorMessages.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object listed as text. Run \code{\link{getSchemaForObjects}}('TempStudentErrorMessage') to get more field paths.
	#' @param searchConditionsList A list of search conditions to filter results which are joined by the searchConditionsGroupType. Of the form {FieldName} {ConditionType} {SearchCondition}. For example, c('StudentID LessEqual 500', 'LastName Like Ander\%'). Run \code{\link{listSearchConditionTypes}} for a list of ConditionTypes. Defaults to NULL (unfiltered).
	#' @param searchConditionsGroupType The conjunction which joins multiple searchConditions in the searchConditionsList. Either 'Or' or 'And'. Defaults to 'And'.
	#' @param searchSortFieldNamesList The list of fields sort results by. Defaults to NULL (unsorted).
	#' @param searchSortFieldNamesDescendingList A list of T/F values corresponding to whether to sort each field in searchSortFieldNamesList in descending order. Defaults to F for each FieldName in searchSortFieldNamesList.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param query Queries to be added to the request. Ex. list(SchoolYearID = 1). Defaults to NULL.
	#' @param page Results are paginated. The page of results to return. Default is 1.
	#' @param pageSize Results are paginated. The number of records per page to return. Default is 100,000 (essentially all records for most objects).
	#' @param flatten Whether to flatten results into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Student
	#' @return A list of TempStudentErrorMessages
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	listTempStudentErrorMessages <- function(searchConditionsList = NULL, TempStudentErrorMessageID = F, TempStudentErrorID = F, Error = F, ErrorDetail = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, fieldPaths = NULL, searchConditionsGroupType = "And", searchSortFieldNamesList = NULL, searchSortFieldNamesDescendingList = NULL, entityId = 1, query = NULL, page = 1, pageSize = 100000, flatten = T, returnResponse = F){

		params <- as.list(environment())

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		listSkyObjects(module = "Student", objectName = "TempStudentErrorMessage", searchFields = searchFields %>% append(fieldPaths), page = page, pageSize = pageSize, SearchConditionsList = searchConditionsList, SearchConditionsGroupType = searchConditionsGroupType, SearchSortFieldNamesList = searchSortFieldNamesList, SearchSortFieldNamesDescendingList = searchSortFieldNamesDescendingList, entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' Get a TempStudentErrorMessage
	#'
	#' This function returns a dataframe or json object of a TempStudentErrorMessage
	#' @param TempStudentErrorMessageID The ID of the TempStudentErrorMessage to return.
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given TempStudentErrorMessage. Defaults to FALSE for all return fields which, for convenience, returns all fields for the TempStudentErrorMessage.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object listed as text. Run \code{\link{getSchemaForObjects}}('TempStudentErrorMessage') to get more field paths.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param query Queries to be added to the request. Ex. list(SchoolYearID = 1). Defaults to NULL.
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Student
	#' @return A dataframe or of TempStudentErrorMessage
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	getTempStudentErrorMessage <- function(TempStudentErrorMessageID, TempStudentErrorID = F, Error = F, ErrorDetail = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, fieldPaths = NULL, entityId = 1, query = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment()) %>% purrr::keep(names(.) != "TempStudentErrorMessageID")

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		getSkyObject(module = "Student", objectName = "TempStudentErrorMessage", objectId = TempStudentErrorMessageID, searchFields = searchFields, entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' Delete a TempStudentErrorMessage
	#'
	#' This function deletes a TempStudentErrorMessage
	#' @param TempStudentErrorMessageID The ID of the TempStudentErrorMessage to delete
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param query Queries to be added to the request. Ex. list(SchoolYearID = 1). Defaults to NULL.
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Student
	#' @return The TempStudentErrorMessageID of the deleted TempStudentErrorMessage.
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	deleteTempStudentErrorMessage <- function(TempStudentErrorMessageID, ignoreWarnings = F, entityId = 1, query = NULL, flatten = T, returnResponse = F){

		deleteSkyObject(module = "Student", objectName = "TempStudentErrorMessage", objectId = TempStudentErrorMessageID, ignoreWarnings = ignoreWarnings, entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' Create a TempStudentErrorMessage
	#'
	#' This function creates a TempStudentErrorMessage
	#' @param fieldNames The field values to give the created TempStudentErrorMessage. Each defaults to NULL.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param query Queries to be added to the request. Ex. list(SchoolYearID = 1). Defaults to NULL.
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Student
	#' @return A newly created TempStudentErrorMessage
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	createTempStudentErrorMessage <- function(TempStudentErrorID = NULL, Error = NULL, ErrorDetail = NULL, entityId = 1, query = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment())

		body <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper()) %>% purrr::compact()

		createSkyObject(module = "Student", objectName = "TempStudentErrorMessage", body = list(DataObject = body), searchFields = append("TempStudentErrorMessageID", body %>% names()), entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' Modify a TempStudentErrorMessage
	#'
	#' This function modifies a TempStudentErrorMessage
	#' @param fieldNames The field values to give the modified TempStudentErrorMessage. Each defaults to NULL.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param query Queries to be added to the request. Ex. list(SchoolYearID = 1). Defaults to NULL.
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Student
	#' @return The modified TempStudentErrorMessage
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	modifyTempStudentErrorMessage <- function(TempStudentErrorMessageID, TempStudentErrorID = NULL, Error = NULL, ErrorDetail = NULL, entityId = 1, query = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment())

		body <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper()) %>% purrr::compact()

		modifySkyObject(module = "Student", objectName = "TempStudentErrorMessage", objectId = TempStudentErrorMessageID, body = list(DataObject = body), searchFields = append("TempStudentErrorMessageID", body %>% names()), entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' List FallSports
	#'
	#' This function returns a dataframe or json object of FallSports
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given FallSports. Defaults to FALSE for all return fields which, for convenience, returns all fields for the FallSports.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object listed as text. Run \code{\link{getSchemaForObjects}}('FallSport') to get more field paths.
	#' @param searchConditionsList A list of search conditions to filter results which are joined by the searchConditionsGroupType. Of the form {FieldName} {ConditionType} {SearchCondition}. For example, c('StudentID LessEqual 500', 'LastName Like Ander\%'). Run \code{\link{listSearchConditionTypes}} for a list of ConditionTypes. Defaults to NULL (unfiltered).
	#' @param searchConditionsGroupType The conjunction which joins multiple searchConditions in the searchConditionsList. Either 'Or' or 'And'. Defaults to 'And'.
	#' @param searchSortFieldNamesList The list of fields sort results by. Defaults to NULL (unsorted).
	#' @param searchSortFieldNamesDescendingList A list of T/F values corresponding to whether to sort each field in searchSortFieldNamesList in descending order. Defaults to F for each FieldName in searchSortFieldNamesList.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param query Queries to be added to the request. Ex. list(SchoolYearID = 1). Defaults to NULL.
	#' @param page Results are paginated. The page of results to return. Default is 1.
	#' @param pageSize Results are paginated. The number of records per page to return. Default is 100,000 (essentially all records for most objects).
	#' @param flatten Whether to flatten results into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Student
	#' @return A list of FallSports
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	listFallSports <- function(searchConditionsList = NULL, FallSportID = F, Code = F, FallSport = F, fieldPaths = NULL, searchConditionsGroupType = "And", searchSortFieldNamesList = NULL, searchSortFieldNamesDescendingList = NULL, entityId = 1, query = NULL, page = 1, pageSize = 100000, flatten = T, returnResponse = F){

		params <- as.list(environment())

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		listSkyObjects(module = "Student", objectName = "FallSport", searchFields = searchFields %>% append(fieldPaths), page = page, pageSize = pageSize, SearchConditionsList = searchConditionsList, SearchConditionsGroupType = searchConditionsGroupType, SearchSortFieldNamesList = searchSortFieldNamesList, SearchSortFieldNamesDescendingList = searchSortFieldNamesDescendingList, entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' Get a FallSport
	#'
	#' This function returns a dataframe or json object of a FallSport
	#' @param FallSportID The ID of the FallSport to return.
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given FallSport. Defaults to FALSE for all return fields which, for convenience, returns all fields for the FallSport.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object listed as text. Run \code{\link{getSchemaForObjects}}('FallSport') to get more field paths.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param query Queries to be added to the request. Ex. list(SchoolYearID = 1). Defaults to NULL.
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Student
	#' @return A dataframe or of FallSport
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	getFallSport <- function(FallSportID, Code = F, FallSport = F, fieldPaths = NULL, entityId = 1, query = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment()) %>% purrr::keep(names(.) != "FallSportID")

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		getSkyObject(module = "Student", objectName = "FallSport", objectId = FallSportID, searchFields = searchFields, entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' Delete a FallSport
	#'
	#' This function deletes a FallSport
	#' @param FallSportID The ID of the FallSport to delete
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param query Queries to be added to the request. Ex. list(SchoolYearID = 1). Defaults to NULL.
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Student
	#' @return The FallSportID of the deleted FallSport.
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	deleteFallSport <- function(FallSportID, ignoreWarnings = F, entityId = 1, query = NULL, flatten = T, returnResponse = F){

		deleteSkyObject(module = "Student", objectName = "FallSport", objectId = FallSportID, ignoreWarnings = ignoreWarnings, entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' Create a FallSport
	#'
	#' This function creates a FallSport
	#' @param fieldNames The field values to give the created FallSport. Each defaults to NULL.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param query Queries to be added to the request. Ex. list(SchoolYearID = 1). Defaults to NULL.
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Student
	#' @return A newly created FallSport
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	createFallSport <- function(Code = NULL, FallSport = NULL, entityId = 1, query = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment())

		body <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper()) %>% purrr::compact()

		createSkyObject(module = "Student", objectName = "FallSport", body = list(DataObject = body), searchFields = append("FallSportID", body %>% names()), entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' Modify a FallSport
	#'
	#' This function modifies a FallSport
	#' @param fieldNames The field values to give the modified FallSport. Each defaults to NULL.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param query Queries to be added to the request. Ex. list(SchoolYearID = 1). Defaults to NULL.
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Student
	#' @return The modified FallSport
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	modifyFallSport <- function(FallSportID, Code = NULL, FallSport = NULL, entityId = 1, query = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment())

		body <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper()) %>% purrr::compact()

		modifySkyObject(module = "Student", objectName = "FallSport", objectId = FallSportID, body = list(DataObject = body), searchFields = append("FallSportID", body %>% names()), entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' List WinterSports
	#'
	#' This function returns a dataframe or json object of WinterSports
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given WinterSports. Defaults to FALSE for all return fields which, for convenience, returns all fields for the WinterSports.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object listed as text. Run \code{\link{getSchemaForObjects}}('WinterSport') to get more field paths.
	#' @param searchConditionsList A list of search conditions to filter results which are joined by the searchConditionsGroupType. Of the form {FieldName} {ConditionType} {SearchCondition}. For example, c('StudentID LessEqual 500', 'LastName Like Ander\%'). Run \code{\link{listSearchConditionTypes}} for a list of ConditionTypes. Defaults to NULL (unfiltered).
	#' @param searchConditionsGroupType The conjunction which joins multiple searchConditions in the searchConditionsList. Either 'Or' or 'And'. Defaults to 'And'.
	#' @param searchSortFieldNamesList The list of fields sort results by. Defaults to NULL (unsorted).
	#' @param searchSortFieldNamesDescendingList A list of T/F values corresponding to whether to sort each field in searchSortFieldNamesList in descending order. Defaults to F for each FieldName in searchSortFieldNamesList.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param query Queries to be added to the request. Ex. list(SchoolYearID = 1). Defaults to NULL.
	#' @param page Results are paginated. The page of results to return. Default is 1.
	#' @param pageSize Results are paginated. The number of records per page to return. Default is 100,000 (essentially all records for most objects).
	#' @param flatten Whether to flatten results into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Student
	#' @return A list of WinterSports
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	listWinterSports <- function(searchConditionsList = NULL, WinterSportID = F, Code = F, WinterSport = F, fieldPaths = NULL, searchConditionsGroupType = "And", searchSortFieldNamesList = NULL, searchSortFieldNamesDescendingList = NULL, entityId = 1, query = NULL, page = 1, pageSize = 100000, flatten = T, returnResponse = F){

		params <- as.list(environment())

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		listSkyObjects(module = "Student", objectName = "WinterSport", searchFields = searchFields %>% append(fieldPaths), page = page, pageSize = pageSize, SearchConditionsList = searchConditionsList, SearchConditionsGroupType = searchConditionsGroupType, SearchSortFieldNamesList = searchSortFieldNamesList, SearchSortFieldNamesDescendingList = searchSortFieldNamesDescendingList, entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' Get a WinterSport
	#'
	#' This function returns a dataframe or json object of a WinterSport
	#' @param WinterSportID The ID of the WinterSport to return.
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given WinterSport. Defaults to FALSE for all return fields which, for convenience, returns all fields for the WinterSport.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object listed as text. Run \code{\link{getSchemaForObjects}}('WinterSport') to get more field paths.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param query Queries to be added to the request. Ex. list(SchoolYearID = 1). Defaults to NULL.
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Student
	#' @return A dataframe or of WinterSport
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	getWinterSport <- function(WinterSportID, Code = F, WinterSport = F, fieldPaths = NULL, entityId = 1, query = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment()) %>% purrr::keep(names(.) != "WinterSportID")

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		getSkyObject(module = "Student", objectName = "WinterSport", objectId = WinterSportID, searchFields = searchFields, entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' Delete a WinterSport
	#'
	#' This function deletes a WinterSport
	#' @param WinterSportID The ID of the WinterSport to delete
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param query Queries to be added to the request. Ex. list(SchoolYearID = 1). Defaults to NULL.
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Student
	#' @return The WinterSportID of the deleted WinterSport.
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	deleteWinterSport <- function(WinterSportID, ignoreWarnings = F, entityId = 1, query = NULL, flatten = T, returnResponse = F){

		deleteSkyObject(module = "Student", objectName = "WinterSport", objectId = WinterSportID, ignoreWarnings = ignoreWarnings, entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' Create a WinterSport
	#'
	#' This function creates a WinterSport
	#' @param fieldNames The field values to give the created WinterSport. Each defaults to NULL.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param query Queries to be added to the request. Ex. list(SchoolYearID = 1). Defaults to NULL.
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Student
	#' @return A newly created WinterSport
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	createWinterSport <- function(Code = NULL, WinterSport = NULL, entityId = 1, query = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment())

		body <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper()) %>% purrr::compact()

		createSkyObject(module = "Student", objectName = "WinterSport", body = list(DataObject = body), searchFields = append("WinterSportID", body %>% names()), entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' Modify a WinterSport
	#'
	#' This function modifies a WinterSport
	#' @param fieldNames The field values to give the modified WinterSport. Each defaults to NULL.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param query Queries to be added to the request. Ex. list(SchoolYearID = 1). Defaults to NULL.
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Student
	#' @return The modified WinterSport
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	modifyWinterSport <- function(WinterSportID, Code = NULL, WinterSport = NULL, entityId = 1, query = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment())

		body <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper()) %>% purrr::compact()

		modifySkyObject(module = "Student", objectName = "WinterSport", objectId = WinterSportID, body = list(DataObject = body), searchFields = append("WinterSportID", body %>% names()), entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' List SpringSports
	#'
	#' This function returns a dataframe or json object of SpringSports
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given SpringSports. Defaults to FALSE for all return fields which, for convenience, returns all fields for the SpringSports.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object listed as text. Run \code{\link{getSchemaForObjects}}('SpringSport') to get more field paths.
	#' @param searchConditionsList A list of search conditions to filter results which are joined by the searchConditionsGroupType. Of the form {FieldName} {ConditionType} {SearchCondition}. For example, c('StudentID LessEqual 500', 'LastName Like Ander\%'). Run \code{\link{listSearchConditionTypes}} for a list of ConditionTypes. Defaults to NULL (unfiltered).
	#' @param searchConditionsGroupType The conjunction which joins multiple searchConditions in the searchConditionsList. Either 'Or' or 'And'. Defaults to 'And'.
	#' @param searchSortFieldNamesList The list of fields sort results by. Defaults to NULL (unsorted).
	#' @param searchSortFieldNamesDescendingList A list of T/F values corresponding to whether to sort each field in searchSortFieldNamesList in descending order. Defaults to F for each FieldName in searchSortFieldNamesList.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param query Queries to be added to the request. Ex. list(SchoolYearID = 1). Defaults to NULL.
	#' @param page Results are paginated. The page of results to return. Default is 1.
	#' @param pageSize Results are paginated. The number of records per page to return. Default is 100,000 (essentially all records for most objects).
	#' @param flatten Whether to flatten results into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Student
	#' @return A list of SpringSports
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	listSpringSports <- function(searchConditionsList = NULL, SpringSportID = F, Code = F, SpringSport = F, fieldPaths = NULL, searchConditionsGroupType = "And", searchSortFieldNamesList = NULL, searchSortFieldNamesDescendingList = NULL, entityId = 1, query = NULL, page = 1, pageSize = 100000, flatten = T, returnResponse = F){

		params <- as.list(environment())

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		listSkyObjects(module = "Student", objectName = "SpringSport", searchFields = searchFields %>% append(fieldPaths), page = page, pageSize = pageSize, SearchConditionsList = searchConditionsList, SearchConditionsGroupType = searchConditionsGroupType, SearchSortFieldNamesList = searchSortFieldNamesList, SearchSortFieldNamesDescendingList = searchSortFieldNamesDescendingList, entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' Get a SpringSport
	#'
	#' This function returns a dataframe or json object of a SpringSport
	#' @param SpringSportID The ID of the SpringSport to return.
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given SpringSport. Defaults to FALSE for all return fields which, for convenience, returns all fields for the SpringSport.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object listed as text. Run \code{\link{getSchemaForObjects}}('SpringSport') to get more field paths.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param query Queries to be added to the request. Ex. list(SchoolYearID = 1). Defaults to NULL.
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Student
	#' @return A dataframe or of SpringSport
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	getSpringSport <- function(SpringSportID, Code = F, SpringSport = F, fieldPaths = NULL, entityId = 1, query = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment()) %>% purrr::keep(names(.) != "SpringSportID")

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		getSkyObject(module = "Student", objectName = "SpringSport", objectId = SpringSportID, searchFields = searchFields, entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' Delete a SpringSport
	#'
	#' This function deletes a SpringSport
	#' @param SpringSportID The ID of the SpringSport to delete
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param query Queries to be added to the request. Ex. list(SchoolYearID = 1). Defaults to NULL.
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Student
	#' @return The SpringSportID of the deleted SpringSport.
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	deleteSpringSport <- function(SpringSportID, ignoreWarnings = F, entityId = 1, query = NULL, flatten = T, returnResponse = F){

		deleteSkyObject(module = "Student", objectName = "SpringSport", objectId = SpringSportID, ignoreWarnings = ignoreWarnings, entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' Create a SpringSport
	#'
	#' This function creates a SpringSport
	#' @param fieldNames The field values to give the created SpringSport. Each defaults to NULL.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param query Queries to be added to the request. Ex. list(SchoolYearID = 1). Defaults to NULL.
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Student
	#' @return A newly created SpringSport
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	createSpringSport <- function(Code = NULL, SpringSport = NULL, entityId = 1, query = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment())

		body <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper()) %>% purrr::compact()

		createSkyObject(module = "Student", objectName = "SpringSport", body = list(DataObject = body), searchFields = append("SpringSportID", body %>% names()), entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' Modify a SpringSport
	#'
	#' This function modifies a SpringSport
	#' @param fieldNames The field values to give the modified SpringSport. Each defaults to NULL.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param query Queries to be added to the request. Ex. list(SchoolYearID = 1). Defaults to NULL.
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Student
	#' @return The modified SpringSport
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	modifySpringSport <- function(SpringSportID, Code = NULL, SpringSport = NULL, entityId = 1, query = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment())

		body <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper()) %>% purrr::compact()

		modifySkyObject(module = "Student", objectName = "SpringSport", objectId = SpringSportID, body = list(DataObject = body), searchFields = append("SpringSportID", body %>% names()), entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' List FallSportsTeams
	#'
	#' This function returns a dataframe or json object of FallSportsTeams
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given FallSportsTeams. Defaults to FALSE for all return fields which, for convenience, returns all fields for the FallSportsTeams.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object listed as text. Run \code{\link{getSchemaForObjects}}('FallSportsTeam') to get more field paths.
	#' @param searchConditionsList A list of search conditions to filter results which are joined by the searchConditionsGroupType. Of the form {FieldName} {ConditionType} {SearchCondition}. For example, c('StudentID LessEqual 500', 'LastName Like Ander\%'). Run \code{\link{listSearchConditionTypes}} for a list of ConditionTypes. Defaults to NULL (unfiltered).
	#' @param searchConditionsGroupType The conjunction which joins multiple searchConditions in the searchConditionsList. Either 'Or' or 'And'. Defaults to 'And'.
	#' @param searchSortFieldNamesList The list of fields sort results by. Defaults to NULL (unsorted).
	#' @param searchSortFieldNamesDescendingList A list of T/F values corresponding to whether to sort each field in searchSortFieldNamesList in descending order. Defaults to F for each FieldName in searchSortFieldNamesList.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param query Queries to be added to the request. Ex. list(SchoolYearID = 1). Defaults to NULL.
	#' @param page Results are paginated. The page of results to return. Default is 1.
	#' @param pageSize Results are paginated. The number of records per page to return. Default is 100,000 (essentially all records for most objects).
	#' @param flatten Whether to flatten results into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Student
	#' @return A list of FallSportsTeams
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	listFallSportsTeams <- function(searchConditionsList = NULL, FallSportsTeamID = F, SchoolYearID = F, FallSportID = F, Lettered = F, Captain = F, StudentID = F, fieldPaths = NULL, searchConditionsGroupType = "And", searchSortFieldNamesList = NULL, searchSortFieldNamesDescendingList = NULL, entityId = 1, query = NULL, page = 1, pageSize = 100000, flatten = T, returnResponse = F){

		params <- as.list(environment())

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		listSkyObjects(module = "Student", objectName = "FallSportsTeam", searchFields = searchFields %>% append(fieldPaths), page = page, pageSize = pageSize, SearchConditionsList = searchConditionsList, SearchConditionsGroupType = searchConditionsGroupType, SearchSortFieldNamesList = searchSortFieldNamesList, SearchSortFieldNamesDescendingList = searchSortFieldNamesDescendingList, entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' Get a FallSportsTeam
	#'
	#' This function returns a dataframe or json object of a FallSportsTeam
	#' @param FallSportsTeamID The ID of the FallSportsTeam to return.
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given FallSportsTeam. Defaults to FALSE for all return fields which, for convenience, returns all fields for the FallSportsTeam.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object listed as text. Run \code{\link{getSchemaForObjects}}('FallSportsTeam') to get more field paths.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param query Queries to be added to the request. Ex. list(SchoolYearID = 1). Defaults to NULL.
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Student
	#' @return A dataframe or of FallSportsTeam
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	getFallSportsTeam <- function(FallSportsTeamID, SchoolYearID = F, FallSportID = F, Lettered = F, Captain = F, StudentID = F, fieldPaths = NULL, entityId = 1, query = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment()) %>% purrr::keep(names(.) != "FallSportsTeamID")

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		getSkyObject(module = "Student", objectName = "FallSportsTeam", objectId = FallSportsTeamID, searchFields = searchFields, entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' Delete a FallSportsTeam
	#'
	#' This function deletes a FallSportsTeam
	#' @param FallSportsTeamID The ID of the FallSportsTeam to delete
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param query Queries to be added to the request. Ex. list(SchoolYearID = 1). Defaults to NULL.
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Student
	#' @return The FallSportsTeamID of the deleted FallSportsTeam.
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	deleteFallSportsTeam <- function(FallSportsTeamID, ignoreWarnings = F, entityId = 1, query = NULL, flatten = T, returnResponse = F){

		deleteSkyObject(module = "Student", objectName = "FallSportsTeam", objectId = FallSportsTeamID, ignoreWarnings = ignoreWarnings, entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' Create a FallSportsTeam
	#'
	#' This function creates a FallSportsTeam
	#' @param fieldNames The field values to give the created FallSportsTeam. Each defaults to NULL.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param query Queries to be added to the request. Ex. list(SchoolYearID = 1). Defaults to NULL.
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Student
	#' @return A newly created FallSportsTeam
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	createFallSportsTeam <- function(SchoolYearID = NULL, FallSportID = NULL, Lettered = NULL, Captain = NULL, StudentID = NULL, entityId = 1, query = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment())

		body <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper()) %>% purrr::compact()

		createSkyObject(module = "Student", objectName = "FallSportsTeam", body = list(DataObject = body), searchFields = append("FallSportsTeamID", body %>% names()), entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' Modify a FallSportsTeam
	#'
	#' This function modifies a FallSportsTeam
	#' @param fieldNames The field values to give the modified FallSportsTeam. Each defaults to NULL.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param query Queries to be added to the request. Ex. list(SchoolYearID = 1). Defaults to NULL.
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Student
	#' @return The modified FallSportsTeam
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	modifyFallSportsTeam <- function(FallSportsTeamID, SchoolYearID = NULL, FallSportID = NULL, Lettered = NULL, Captain = NULL, StudentID = NULL, entityId = 1, query = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment())

		body <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper()) %>% purrr::compact()

		modifySkyObject(module = "Student", objectName = "FallSportsTeam", objectId = FallSportsTeamID, body = list(DataObject = body), searchFields = append("FallSportsTeamID", body %>% names()), entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' List WinterSportsTeams
	#'
	#' This function returns a dataframe or json object of WinterSportsTeams
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given WinterSportsTeams. Defaults to FALSE for all return fields which, for convenience, returns all fields for the WinterSportsTeams.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object listed as text. Run \code{\link{getSchemaForObjects}}('WinterSportsTeam') to get more field paths.
	#' @param searchConditionsList A list of search conditions to filter results which are joined by the searchConditionsGroupType. Of the form {FieldName} {ConditionType} {SearchCondition}. For example, c('StudentID LessEqual 500', 'LastName Like Ander\%'). Run \code{\link{listSearchConditionTypes}} for a list of ConditionTypes. Defaults to NULL (unfiltered).
	#' @param searchConditionsGroupType The conjunction which joins multiple searchConditions in the searchConditionsList. Either 'Or' or 'And'. Defaults to 'And'.
	#' @param searchSortFieldNamesList The list of fields sort results by. Defaults to NULL (unsorted).
	#' @param searchSortFieldNamesDescendingList A list of T/F values corresponding to whether to sort each field in searchSortFieldNamesList in descending order. Defaults to F for each FieldName in searchSortFieldNamesList.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param query Queries to be added to the request. Ex. list(SchoolYearID = 1). Defaults to NULL.
	#' @param page Results are paginated. The page of results to return. Default is 1.
	#' @param pageSize Results are paginated. The number of records per page to return. Default is 100,000 (essentially all records for most objects).
	#' @param flatten Whether to flatten results into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Student
	#' @return A list of WinterSportsTeams
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	listWinterSportsTeams <- function(searchConditionsList = NULL, WinterSportsTeamID = F, SchoolYearID = F, WinterSportID = F, Lettered = F, Captain = F, StudentID = F, fieldPaths = NULL, searchConditionsGroupType = "And", searchSortFieldNamesList = NULL, searchSortFieldNamesDescendingList = NULL, entityId = 1, query = NULL, page = 1, pageSize = 100000, flatten = T, returnResponse = F){

		params <- as.list(environment())

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		listSkyObjects(module = "Student", objectName = "WinterSportsTeam", searchFields = searchFields %>% append(fieldPaths), page = page, pageSize = pageSize, SearchConditionsList = searchConditionsList, SearchConditionsGroupType = searchConditionsGroupType, SearchSortFieldNamesList = searchSortFieldNamesList, SearchSortFieldNamesDescendingList = searchSortFieldNamesDescendingList, entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' Get a WinterSportsTeam
	#'
	#' This function returns a dataframe or json object of a WinterSportsTeam
	#' @param WinterSportsTeamID The ID of the WinterSportsTeam to return.
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given WinterSportsTeam. Defaults to FALSE for all return fields which, for convenience, returns all fields for the WinterSportsTeam.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object listed as text. Run \code{\link{getSchemaForObjects}}('WinterSportsTeam') to get more field paths.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param query Queries to be added to the request. Ex. list(SchoolYearID = 1). Defaults to NULL.
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Student
	#' @return A dataframe or of WinterSportsTeam
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	getWinterSportsTeam <- function(WinterSportsTeamID, SchoolYearID = F, WinterSportID = F, Lettered = F, Captain = F, StudentID = F, fieldPaths = NULL, entityId = 1, query = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment()) %>% purrr::keep(names(.) != "WinterSportsTeamID")

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		getSkyObject(module = "Student", objectName = "WinterSportsTeam", objectId = WinterSportsTeamID, searchFields = searchFields, entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' Delete a WinterSportsTeam
	#'
	#' This function deletes a WinterSportsTeam
	#' @param WinterSportsTeamID The ID of the WinterSportsTeam to delete
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param query Queries to be added to the request. Ex. list(SchoolYearID = 1). Defaults to NULL.
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Student
	#' @return The WinterSportsTeamID of the deleted WinterSportsTeam.
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	deleteWinterSportsTeam <- function(WinterSportsTeamID, ignoreWarnings = F, entityId = 1, query = NULL, flatten = T, returnResponse = F){

		deleteSkyObject(module = "Student", objectName = "WinterSportsTeam", objectId = WinterSportsTeamID, ignoreWarnings = ignoreWarnings, entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' Create a WinterSportsTeam
	#'
	#' This function creates a WinterSportsTeam
	#' @param fieldNames The field values to give the created WinterSportsTeam. Each defaults to NULL.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param query Queries to be added to the request. Ex. list(SchoolYearID = 1). Defaults to NULL.
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Student
	#' @return A newly created WinterSportsTeam
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	createWinterSportsTeam <- function(SchoolYearID = NULL, WinterSportID = NULL, Lettered = NULL, Captain = NULL, StudentID = NULL, entityId = 1, query = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment())

		body <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper()) %>% purrr::compact()

		createSkyObject(module = "Student", objectName = "WinterSportsTeam", body = list(DataObject = body), searchFields = append("WinterSportsTeamID", body %>% names()), entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' Modify a WinterSportsTeam
	#'
	#' This function modifies a WinterSportsTeam
	#' @param fieldNames The field values to give the modified WinterSportsTeam. Each defaults to NULL.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param query Queries to be added to the request. Ex. list(SchoolYearID = 1). Defaults to NULL.
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Student
	#' @return The modified WinterSportsTeam
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	modifyWinterSportsTeam <- function(WinterSportsTeamID, SchoolYearID = NULL, WinterSportID = NULL, Lettered = NULL, Captain = NULL, StudentID = NULL, entityId = 1, query = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment())

		body <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper()) %>% purrr::compact()

		modifySkyObject(module = "Student", objectName = "WinterSportsTeam", objectId = WinterSportsTeamID, body = list(DataObject = body), searchFields = append("WinterSportsTeamID", body %>% names()), entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' List SpringSportsTeams
	#'
	#' This function returns a dataframe or json object of SpringSportsTeams
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given SpringSportsTeams. Defaults to FALSE for all return fields which, for convenience, returns all fields for the SpringSportsTeams.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object listed as text. Run \code{\link{getSchemaForObjects}}('SpringSportsTeam') to get more field paths.
	#' @param searchConditionsList A list of search conditions to filter results which are joined by the searchConditionsGroupType. Of the form {FieldName} {ConditionType} {SearchCondition}. For example, c('StudentID LessEqual 500', 'LastName Like Ander\%'). Run \code{\link{listSearchConditionTypes}} for a list of ConditionTypes. Defaults to NULL (unfiltered).
	#' @param searchConditionsGroupType The conjunction which joins multiple searchConditions in the searchConditionsList. Either 'Or' or 'And'. Defaults to 'And'.
	#' @param searchSortFieldNamesList The list of fields sort results by. Defaults to NULL (unsorted).
	#' @param searchSortFieldNamesDescendingList A list of T/F values corresponding to whether to sort each field in searchSortFieldNamesList in descending order. Defaults to F for each FieldName in searchSortFieldNamesList.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param query Queries to be added to the request. Ex. list(SchoolYearID = 1). Defaults to NULL.
	#' @param page Results are paginated. The page of results to return. Default is 1.
	#' @param pageSize Results are paginated. The number of records per page to return. Default is 100,000 (essentially all records for most objects).
	#' @param flatten Whether to flatten results into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Student
	#' @return A list of SpringSportsTeams
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	listSpringSportsTeams <- function(searchConditionsList = NULL, SpringSportsTeamID = F, SpringSportID = F, SchoolYearID = F, Lettered = F, Captain = F, StudentID = F, fieldPaths = NULL, searchConditionsGroupType = "And", searchSortFieldNamesList = NULL, searchSortFieldNamesDescendingList = NULL, entityId = 1, query = NULL, page = 1, pageSize = 100000, flatten = T, returnResponse = F){

		params <- as.list(environment())

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		listSkyObjects(module = "Student", objectName = "SpringSportsTeam", searchFields = searchFields %>% append(fieldPaths), page = page, pageSize = pageSize, SearchConditionsList = searchConditionsList, SearchConditionsGroupType = searchConditionsGroupType, SearchSortFieldNamesList = searchSortFieldNamesList, SearchSortFieldNamesDescendingList = searchSortFieldNamesDescendingList, entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' Get a SpringSportsTeam
	#'
	#' This function returns a dataframe or json object of a SpringSportsTeam
	#' @param SpringSportsTeamID The ID of the SpringSportsTeam to return.
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given SpringSportsTeam. Defaults to FALSE for all return fields which, for convenience, returns all fields for the SpringSportsTeam.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object listed as text. Run \code{\link{getSchemaForObjects}}('SpringSportsTeam') to get more field paths.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param query Queries to be added to the request. Ex. list(SchoolYearID = 1). Defaults to NULL.
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Student
	#' @return A dataframe or of SpringSportsTeam
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	getSpringSportsTeam <- function(SpringSportsTeamID, SpringSportID = F, SchoolYearID = F, Lettered = F, Captain = F, StudentID = F, fieldPaths = NULL, entityId = 1, query = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment()) %>% purrr::keep(names(.) != "SpringSportsTeamID")

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		getSkyObject(module = "Student", objectName = "SpringSportsTeam", objectId = SpringSportsTeamID, searchFields = searchFields, entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' Delete a SpringSportsTeam
	#'
	#' This function deletes a SpringSportsTeam
	#' @param SpringSportsTeamID The ID of the SpringSportsTeam to delete
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param query Queries to be added to the request. Ex. list(SchoolYearID = 1). Defaults to NULL.
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Student
	#' @return The SpringSportsTeamID of the deleted SpringSportsTeam.
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	deleteSpringSportsTeam <- function(SpringSportsTeamID, ignoreWarnings = F, entityId = 1, query = NULL, flatten = T, returnResponse = F){

		deleteSkyObject(module = "Student", objectName = "SpringSportsTeam", objectId = SpringSportsTeamID, ignoreWarnings = ignoreWarnings, entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' Create a SpringSportsTeam
	#'
	#' This function creates a SpringSportsTeam
	#' @param fieldNames The field values to give the created SpringSportsTeam. Each defaults to NULL.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param query Queries to be added to the request. Ex. list(SchoolYearID = 1). Defaults to NULL.
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Student
	#' @return A newly created SpringSportsTeam
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	createSpringSportsTeam <- function(SpringSportID = NULL, SchoolYearID = NULL, Lettered = NULL, Captain = NULL, StudentID = NULL, entityId = 1, query = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment())

		body <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper()) %>% purrr::compact()

		createSkyObject(module = "Student", objectName = "SpringSportsTeam", body = list(DataObject = body), searchFields = append("SpringSportsTeamID", body %>% names()), entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' Modify a SpringSportsTeam
	#'
	#' This function modifies a SpringSportsTeam
	#' @param fieldNames The field values to give the modified SpringSportsTeam. Each defaults to NULL.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param query Queries to be added to the request. Ex. list(SchoolYearID = 1). Defaults to NULL.
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Student
	#' @return The modified SpringSportsTeam
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	modifySpringSportsTeam <- function(SpringSportsTeamID, SpringSportID = NULL, SchoolYearID = NULL, Lettered = NULL, Captain = NULL, StudentID = NULL, entityId = 1, query = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment())

		body <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper()) %>% purrr::compact()

		modifySkyObject(module = "Student", objectName = "SpringSportsTeam", objectId = SpringSportsTeamID, body = list(DataObject = body), searchFields = append("SpringSportsTeamID", body %>% names()), entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' List Religions
	#'
	#' This function returns a dataframe or json object of Religions
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given Religions. Defaults to FALSE for all return fields which, for convenience, returns all fields for the Religions.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object listed as text. Run \code{\link{getSchemaForObjects}}('Religion') to get more field paths.
	#' @param searchConditionsList A list of search conditions to filter results which are joined by the searchConditionsGroupType. Of the form {FieldName} {ConditionType} {SearchCondition}. For example, c('StudentID LessEqual 500', 'LastName Like Ander\%'). Run \code{\link{listSearchConditionTypes}} for a list of ConditionTypes. Defaults to NULL (unfiltered).
	#' @param searchConditionsGroupType The conjunction which joins multiple searchConditions in the searchConditionsList. Either 'Or' or 'And'. Defaults to 'And'.
	#' @param searchSortFieldNamesList The list of fields sort results by. Defaults to NULL (unsorted).
	#' @param searchSortFieldNamesDescendingList A list of T/F values corresponding to whether to sort each field in searchSortFieldNamesList in descending order. Defaults to F for each FieldName in searchSortFieldNamesList.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param query Queries to be added to the request. Ex. list(SchoolYearID = 1). Defaults to NULL.
	#' @param page Results are paginated. The page of results to return. Default is 1.
	#' @param pageSize Results are paginated. The number of records per page to return. Default is 100,000 (essentially all records for most objects).
	#' @param flatten Whether to flatten results into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Student
	#' @return A list of Religions
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	listReligions <- function(searchConditionsList = NULL, ReligionID = F, Code = F, Description = F, fieldPaths = NULL, searchConditionsGroupType = "And", searchSortFieldNamesList = NULL, searchSortFieldNamesDescendingList = NULL, entityId = 1, query = NULL, page = 1, pageSize = 100000, flatten = T, returnResponse = F){

		params <- as.list(environment())

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		listSkyObjects(module = "Student", objectName = "Religion", searchFields = searchFields %>% append(fieldPaths), page = page, pageSize = pageSize, SearchConditionsList = searchConditionsList, SearchConditionsGroupType = searchConditionsGroupType, SearchSortFieldNamesList = searchSortFieldNamesList, SearchSortFieldNamesDescendingList = searchSortFieldNamesDescendingList, entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' Get a Religion
	#'
	#' This function returns a dataframe or json object of a Religion
	#' @param ReligionID The ID of the Religion to return.
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given Religion. Defaults to FALSE for all return fields which, for convenience, returns all fields for the Religion.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object listed as text. Run \code{\link{getSchemaForObjects}}('Religion') to get more field paths.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param query Queries to be added to the request. Ex. list(SchoolYearID = 1). Defaults to NULL.
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Student
	#' @return A dataframe or of Religion
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	getReligion <- function(ReligionID, Code = F, Description = F, fieldPaths = NULL, entityId = 1, query = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment()) %>% purrr::keep(names(.) != "ReligionID")

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		getSkyObject(module = "Student", objectName = "Religion", objectId = ReligionID, searchFields = searchFields, entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' Delete a Religion
	#'
	#' This function deletes a Religion
	#' @param ReligionID The ID of the Religion to delete
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param query Queries to be added to the request. Ex. list(SchoolYearID = 1). Defaults to NULL.
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Student
	#' @return The ReligionID of the deleted Religion.
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	deleteReligion <- function(ReligionID, ignoreWarnings = F, entityId = 1, query = NULL, flatten = T, returnResponse = F){

		deleteSkyObject(module = "Student", objectName = "Religion", objectId = ReligionID, ignoreWarnings = ignoreWarnings, entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' Create a Religion
	#'
	#' This function creates a Religion
	#' @param fieldNames The field values to give the created Religion. Each defaults to NULL.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param query Queries to be added to the request. Ex. list(SchoolYearID = 1). Defaults to NULL.
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Student
	#' @return A newly created Religion
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	createReligion <- function(Code = NULL, Description = NULL, entityId = 1, query = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment())

		body <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper()) %>% purrr::compact()

		createSkyObject(module = "Student", objectName = "Religion", body = list(DataObject = body), searchFields = append("ReligionID", body %>% names()), entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' Modify a Religion
	#'
	#' This function modifies a Religion
	#' @param fieldNames The field values to give the modified Religion. Each defaults to NULL.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param query Queries to be added to the request. Ex. list(SchoolYearID = 1). Defaults to NULL.
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Student
	#' @return The modified Religion
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	modifyReligion <- function(ReligionID, Code = NULL, Description = NULL, entityId = 1, query = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment())

		body <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper()) %>% purrr::compact()

		modifySkyObject(module = "Student", objectName = "Religion", objectId = ReligionID, body = list(DataObject = body), searchFields = append("ReligionID", body %>% names()), entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' List CurrentSportsSelections
	#'
	#' This function returns a dataframe or json object of CurrentSportsSelections
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given CurrentSportsSelections. Defaults to FALSE for all return fields which, for convenience, returns all fields for the CurrentSportsSelections.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object listed as text. Run \code{\link{getSchemaForObjects}}('CurrentSportsSelections') to get more field paths.
	#' @param searchConditionsList A list of search conditions to filter results which are joined by the searchConditionsGroupType. Of the form {FieldName} {ConditionType} {SearchCondition}. For example, c('StudentID LessEqual 500', 'LastName Like Ander\%'). Run \code{\link{listSearchConditionTypes}} for a list of ConditionTypes. Defaults to NULL (unfiltered).
	#' @param searchConditionsGroupType The conjunction which joins multiple searchConditions in the searchConditionsList. Either 'Or' or 'And'. Defaults to 'And'.
	#' @param searchSortFieldNamesList The list of fields sort results by. Defaults to NULL (unsorted).
	#' @param searchSortFieldNamesDescendingList A list of T/F values corresponding to whether to sort each field in searchSortFieldNamesList in descending order. Defaults to F for each FieldName in searchSortFieldNamesList.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param query Queries to be added to the request. Ex. list(SchoolYearID = 1). Defaults to NULL.
	#' @param page Results are paginated. The page of results to return. Default is 1.
	#' @param pageSize Results are paginated. The number of records per page to return. Default is 100,000 (essentially all records for most objects).
	#' @param flatten Whether to flatten results into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Student
	#' @return A list of CurrentSportsSelections
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	listCurrentSportsSelections <- function(searchConditionsList = NULL, CurrentSportsSelectionsID = F, FallSportID = F, WinterSportID = F, SpringSportID = F, StudentID = F, fieldPaths = NULL, searchConditionsGroupType = "And", searchSortFieldNamesList = NULL, searchSortFieldNamesDescendingList = NULL, entityId = 1, query = NULL, page = 1, pageSize = 100000, flatten = T, returnResponse = F){

		params <- as.list(environment())

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		listSkyObjects(module = "Student", objectName = "CurrentSportsSelections", searchFields = searchFields %>% append(fieldPaths), page = page, pageSize = pageSize, SearchConditionsList = searchConditionsList, SearchConditionsGroupType = searchConditionsGroupType, SearchSortFieldNamesList = searchSortFieldNamesList, SearchSortFieldNamesDescendingList = searchSortFieldNamesDescendingList, entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' Get a CurrentSportsSelections
	#'
	#' This function returns a dataframe or json object of a CurrentSportsSelections
	#' @param CurrentSportsSelectionsID The ID of the CurrentSportsSelections to return.
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given CurrentSportsSelections. Defaults to FALSE for all return fields which, for convenience, returns all fields for the CurrentSportsSelections.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object listed as text. Run \code{\link{getSchemaForObjects}}('CurrentSportsSelections') to get more field paths.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param query Queries to be added to the request. Ex. list(SchoolYearID = 1). Defaults to NULL.
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Student
	#' @return A dataframe or of CurrentSportsSelections
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	getCurrentSportsSelections <- function(CurrentSportsSelectionsID, FallSportID = F, WinterSportID = F, SpringSportID = F, StudentID = F, fieldPaths = NULL, entityId = 1, query = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment()) %>% purrr::keep(names(.) != "CurrentSportsSelectionsID")

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		getSkyObject(module = "Student", objectName = "CurrentSportsSelections", objectId = CurrentSportsSelectionsID, searchFields = searchFields, entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' Delete a CurrentSportsSelections
	#'
	#' This function deletes a CurrentSportsSelections
	#' @param CurrentSportsSelectionsID The ID of the CurrentSportsSelections to delete
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param query Queries to be added to the request. Ex. list(SchoolYearID = 1). Defaults to NULL.
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Student
	#' @return The CurrentSportsSelectionsID of the deleted CurrentSportsSelections.
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	deleteCurrentSportsSelections <- function(CurrentSportsSelectionsID, ignoreWarnings = F, entityId = 1, query = NULL, flatten = T, returnResponse = F){

		deleteSkyObject(module = "Student", objectName = "CurrentSportsSelections", objectId = CurrentSportsSelectionsID, ignoreWarnings = ignoreWarnings, entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' Create a CurrentSportsSelections
	#'
	#' This function creates a CurrentSportsSelections
	#' @param fieldNames The field values to give the created CurrentSportsSelections. Each defaults to NULL.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param query Queries to be added to the request. Ex. list(SchoolYearID = 1). Defaults to NULL.
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Student
	#' @return A newly created CurrentSportsSelections
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	createCurrentSportsSelections <- function(FallSportID = NULL, WinterSportID = NULL, SpringSportID = NULL, StudentID = NULL, entityId = 1, query = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment())

		body <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper()) %>% purrr::compact()

		createSkyObject(module = "Student", objectName = "CurrentSportsSelections", body = list(DataObject = body), searchFields = append("CurrentSportsSelectionsID", body %>% names()), entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' Modify a CurrentSportsSelections
	#'
	#' This function modifies a CurrentSportsSelections
	#' @param fieldNames The field values to give the modified CurrentSportsSelections. Each defaults to NULL.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param query Queries to be added to the request. Ex. list(SchoolYearID = 1). Defaults to NULL.
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Student
	#' @return The modified CurrentSportsSelections
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	modifyCurrentSportsSelections <- function(CurrentSportsSelectionsID, FallSportID = NULL, WinterSportID = NULL, SpringSportID = NULL, StudentID = NULL, entityId = 1, query = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment())

		body <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper()) %>% purrr::compact()

		modifySkyObject(module = "Student", objectName = "CurrentSportsSelections", objectId = CurrentSportsSelectionsID, body = list(DataObject = body), searchFields = append("CurrentSportsSelectionsID", body %>% names()), entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' List EthnicityMAS
	#'
	#' This function returns a dataframe or json object of EthnicityMAS
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given EthnicityMAS. Defaults to FALSE for all return fields which, for convenience, returns all fields for the EthnicityMAS.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object listed as text. Run \code{\link{getSchemaForObjects}}('EthnicityMA') to get more field paths.
	#' @param searchConditionsList A list of search conditions to filter results which are joined by the searchConditionsGroupType. Of the form {FieldName} {ConditionType} {SearchCondition}. For example, c('StudentID LessEqual 500', 'LastName Like Ander\%'). Run \code{\link{listSearchConditionTypes}} for a list of ConditionTypes. Defaults to NULL (unfiltered).
	#' @param searchConditionsGroupType The conjunction which joins multiple searchConditions in the searchConditionsList. Either 'Or' or 'And'. Defaults to 'And'.
	#' @param searchSortFieldNamesList The list of fields sort results by. Defaults to NULL (unsorted).
	#' @param searchSortFieldNamesDescendingList A list of T/F values corresponding to whether to sort each field in searchSortFieldNamesList in descending order. Defaults to F for each FieldName in searchSortFieldNamesList.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param query Queries to be added to the request. Ex. list(SchoolYearID = 1). Defaults to NULL.
	#' @param page Results are paginated. The page of results to return. Default is 1.
	#' @param pageSize Results are paginated. The number of records per page to return. Default is 100,000 (essentially all records for most objects).
	#' @param flatten Whether to flatten results into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Student
	#' @return A list of EthnicityMAS
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	listEthnicityMAS <- function(searchConditionsList = NULL, EthnicityMAID = F, Code = F, Ethnicity = F, fieldPaths = NULL, searchConditionsGroupType = "And", searchSortFieldNamesList = NULL, searchSortFieldNamesDescendingList = NULL, entityId = 1, query = NULL, page = 1, pageSize = 100000, flatten = T, returnResponse = F){

		params <- as.list(environment())

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		listSkyObjects(module = "Student", objectName = "EthnicityMA", searchFields = searchFields %>% append(fieldPaths), page = page, pageSize = pageSize, SearchConditionsList = searchConditionsList, SearchConditionsGroupType = searchConditionsGroupType, SearchSortFieldNamesList = searchSortFieldNamesList, SearchSortFieldNamesDescendingList = searchSortFieldNamesDescendingList, entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' Get an EthnicityMA
	#'
	#' This function returns a dataframe or json object of an EthnicityMA
	#' @param EthnicityMAID The ID of the EthnicityMA to return.
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given EthnicityMA. Defaults to FALSE for all return fields which, for convenience, returns all fields for the EthnicityMA.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object listed as text. Run \code{\link{getSchemaForObjects}}('EthnicityMA') to get more field paths.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param query Queries to be added to the request. Ex. list(SchoolYearID = 1). Defaults to NULL.
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Student
	#' @return A dataframe or of EthnicityMA
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	getEthnicityMA <- function(EthnicityMAID, Code = F, Ethnicity = F, fieldPaths = NULL, entityId = 1, query = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment()) %>% purrr::keep(names(.) != "EthnicityMAID")

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		getSkyObject(module = "Student", objectName = "EthnicityMA", objectId = EthnicityMAID, searchFields = searchFields, entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' Delete an EthnicityMA
	#'
	#' This function deletes an EthnicityMA
	#' @param EthnicityMAID The ID of the EthnicityMA to delete
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param query Queries to be added to the request. Ex. list(SchoolYearID = 1). Defaults to NULL.
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Student
	#' @return The EthnicityMAID of the deleted EthnicityMA.
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	deleteEthnicityMA <- function(EthnicityMAID, ignoreWarnings = F, entityId = 1, query = NULL, flatten = T, returnResponse = F){

		deleteSkyObject(module = "Student", objectName = "EthnicityMA", objectId = EthnicityMAID, ignoreWarnings = ignoreWarnings, entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' Create an EthnicityMA
	#'
	#' This function creates an EthnicityMA
	#' @param fieldNames The field values to give the created EthnicityMA. Each defaults to NULL.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param query Queries to be added to the request. Ex. list(SchoolYearID = 1). Defaults to NULL.
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Student
	#' @return A newly created EthnicityMA
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	createEthnicityMA <- function(Code = NULL, Ethnicity = NULL, entityId = 1, query = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment())

		body <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper()) %>% purrr::compact()

		createSkyObject(module = "Student", objectName = "EthnicityMA", body = list(DataObject = body), searchFields = append("EthnicityMAID", body %>% names()), entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' Modify an EthnicityMA
	#'
	#' This function modifies an EthnicityMA
	#' @param fieldNames The field values to give the modified EthnicityMA. Each defaults to NULL.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param query Queries to be added to the request. Ex. list(SchoolYearID = 1). Defaults to NULL.
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Student
	#' @return The modified EthnicityMA
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	modifyEthnicityMA <- function(EthnicityMAID, Code = NULL, Ethnicity = NULL, entityId = 1, query = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment())

		body <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper()) %>% purrr::compact()

		modifySkyObject(module = "Student", objectName = "EthnicityMA", objectId = EthnicityMAID, body = list(DataObject = body), searchFields = append("EthnicityMAID", body %>% names()), entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' List StudentEthnicityMAS
	#'
	#' This function returns a dataframe or json object of StudentEthnicityMAS
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given StudentEthnicityMAS. Defaults to FALSE for all return fields which, for convenience, returns all fields for the StudentEthnicityMAS.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object listed as text. Run \code{\link{getSchemaForObjects}}('StudentEthnicityMA') to get more field paths.
	#' @param searchConditionsList A list of search conditions to filter results which are joined by the searchConditionsGroupType. Of the form {FieldName} {ConditionType} {SearchCondition}. For example, c('StudentID LessEqual 500', 'LastName Like Ander\%'). Run \code{\link{listSearchConditionTypes}} for a list of ConditionTypes. Defaults to NULL (unfiltered).
	#' @param searchConditionsGroupType The conjunction which joins multiple searchConditions in the searchConditionsList. Either 'Or' or 'And'. Defaults to 'And'.
	#' @param searchSortFieldNamesList The list of fields sort results by. Defaults to NULL (unsorted).
	#' @param searchSortFieldNamesDescendingList A list of T/F values corresponding to whether to sort each field in searchSortFieldNamesList in descending order. Defaults to F for each FieldName in searchSortFieldNamesList.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param query Queries to be added to the request. Ex. list(SchoolYearID = 1). Defaults to NULL.
	#' @param page Results are paginated. The page of results to return. Default is 1.
	#' @param pageSize Results are paginated. The number of records per page to return. Default is 100,000 (essentially all records for most objects).
	#' @param flatten Whether to flatten results into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Student
	#' @return A list of StudentEthnicityMAS
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	listStudentEthnicityMAS <- function(searchConditionsList = NULL, StudentEthnicityMAID = F, StudentID = F, EthnicityMAID = F, fieldPaths = NULL, searchConditionsGroupType = "And", searchSortFieldNamesList = NULL, searchSortFieldNamesDescendingList = NULL, entityId = 1, query = NULL, page = 1, pageSize = 100000, flatten = T, returnResponse = F){

		params <- as.list(environment())

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		listSkyObjects(module = "Student", objectName = "StudentEthnicityMA", searchFields = searchFields %>% append(fieldPaths), page = page, pageSize = pageSize, SearchConditionsList = searchConditionsList, SearchConditionsGroupType = searchConditionsGroupType, SearchSortFieldNamesList = searchSortFieldNamesList, SearchSortFieldNamesDescendingList = searchSortFieldNamesDescendingList, entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' Get a StudentEthnicityMA
	#'
	#' This function returns a dataframe or json object of a StudentEthnicityMA
	#' @param StudentEthnicityMAID The ID of the StudentEthnicityMA to return.
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given StudentEthnicityMA. Defaults to FALSE for all return fields which, for convenience, returns all fields for the StudentEthnicityMA.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object listed as text. Run \code{\link{getSchemaForObjects}}('StudentEthnicityMA') to get more field paths.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param query Queries to be added to the request. Ex. list(SchoolYearID = 1). Defaults to NULL.
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Student
	#' @return A dataframe or of StudentEthnicityMA
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	getStudentEthnicityMA <- function(StudentEthnicityMAID, StudentID = F, EthnicityMAID = F, fieldPaths = NULL, entityId = 1, query = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment()) %>% purrr::keep(names(.) != "StudentEthnicityMAID")

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		getSkyObject(module = "Student", objectName = "StudentEthnicityMA", objectId = StudentEthnicityMAID, searchFields = searchFields, entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' Delete a StudentEthnicityMA
	#'
	#' This function deletes a StudentEthnicityMA
	#' @param StudentEthnicityMAID The ID of the StudentEthnicityMA to delete
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param query Queries to be added to the request. Ex. list(SchoolYearID = 1). Defaults to NULL.
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Student
	#' @return The StudentEthnicityMAID of the deleted StudentEthnicityMA.
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	deleteStudentEthnicityMA <- function(StudentEthnicityMAID, ignoreWarnings = F, entityId = 1, query = NULL, flatten = T, returnResponse = F){

		deleteSkyObject(module = "Student", objectName = "StudentEthnicityMA", objectId = StudentEthnicityMAID, ignoreWarnings = ignoreWarnings, entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' Create a StudentEthnicityMA
	#'
	#' This function creates a StudentEthnicityMA
	#' @param fieldNames The field values to give the created StudentEthnicityMA. Each defaults to NULL.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param query Queries to be added to the request. Ex. list(SchoolYearID = 1). Defaults to NULL.
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Student
	#' @return A newly created StudentEthnicityMA
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	createStudentEthnicityMA <- function(StudentID = NULL, EthnicityMAID = NULL, entityId = 1, query = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment())

		body <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper()) %>% purrr::compact()

		createSkyObject(module = "Student", objectName = "StudentEthnicityMA", body = list(DataObject = body), searchFields = append("StudentEthnicityMAID", body %>% names()), entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' Modify a StudentEthnicityMA
	#'
	#' This function modifies a StudentEthnicityMA
	#' @param fieldNames The field values to give the modified StudentEthnicityMA. Each defaults to NULL.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param query Queries to be added to the request. Ex. list(SchoolYearID = 1). Defaults to NULL.
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Student
	#' @return The modified StudentEthnicityMA
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	modifyStudentEthnicityMA <- function(StudentEthnicityMAID, StudentID = NULL, EthnicityMAID = NULL, entityId = 1, query = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment())

		body <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper()) %>% purrr::compact()

		modifySkyObject(module = "Student", objectName = "StudentEthnicityMA", objectId = StudentEthnicityMAID, body = list(DataObject = body), searchFields = append("StudentEthnicityMAID", body %>% names()), entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' List AwardCategories
	#'
	#' This function returns a dataframe or json object of AwardCategories
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given AwardCategories. Defaults to FALSE for all return fields which, for convenience, returns all fields for the AwardCategories.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object listed as text. Run \code{\link{getSchemaForObjects}}('AwardCategory') to get more field paths.
	#' @param searchConditionsList A list of search conditions to filter results which are joined by the searchConditionsGroupType. Of the form {FieldName} {ConditionType} {SearchCondition}. For example, c('StudentID LessEqual 500', 'LastName Like Ander\%'). Run \code{\link{listSearchConditionTypes}} for a list of ConditionTypes. Defaults to NULL (unfiltered).
	#' @param searchConditionsGroupType The conjunction which joins multiple searchConditions in the searchConditionsList. Either 'Or' or 'And'. Defaults to 'And'.
	#' @param searchSortFieldNamesList The list of fields sort results by. Defaults to NULL (unsorted).
	#' @param searchSortFieldNamesDescendingList A list of T/F values corresponding to whether to sort each field in searchSortFieldNamesList in descending order. Defaults to F for each FieldName in searchSortFieldNamesList.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param query Queries to be added to the request. Ex. list(SchoolYearID = 1). Defaults to NULL.
	#' @param page Results are paginated. The page of results to return. Default is 1.
	#' @param pageSize Results are paginated. The number of records per page to return. Default is 100,000 (essentially all records for most objects).
	#' @param flatten Whether to flatten results into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Student
	#' @return A list of AwardCategories
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	listAwardCategories <- function(searchConditionsList = NULL, AwardCategoryID = F, Code = F, Description = F, fieldPaths = NULL, searchConditionsGroupType = "And", searchSortFieldNamesList = NULL, searchSortFieldNamesDescendingList = NULL, entityId = 1, query = NULL, page = 1, pageSize = 100000, flatten = T, returnResponse = F){

		params <- as.list(environment())

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		listSkyObjects(module = "Student", objectName = "AwardCategory", searchFields = searchFields %>% append(fieldPaths), page = page, pageSize = pageSize, SearchConditionsList = searchConditionsList, SearchConditionsGroupType = searchConditionsGroupType, SearchSortFieldNamesList = searchSortFieldNamesList, SearchSortFieldNamesDescendingList = searchSortFieldNamesDescendingList, entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' Get an AwardCategory
	#'
	#' This function returns a dataframe or json object of an AwardCategory
	#' @param AwardCategoryID The ID of the AwardCategory to return.
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given AwardCategory. Defaults to FALSE for all return fields which, for convenience, returns all fields for the AwardCategory.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object listed as text. Run \code{\link{getSchemaForObjects}}('AwardCategory') to get more field paths.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param query Queries to be added to the request. Ex. list(SchoolYearID = 1). Defaults to NULL.
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Student
	#' @return A dataframe or of AwardCategory
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	getAwardCategory <- function(AwardCategoryID, Code = F, Description = F, fieldPaths = NULL, entityId = 1, query = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment()) %>% purrr::keep(names(.) != "AwardCategoryID")

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		getSkyObject(module = "Student", objectName = "AwardCategory", objectId = AwardCategoryID, searchFields = searchFields, entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' Delete an AwardCategory
	#'
	#' This function deletes an AwardCategory
	#' @param AwardCategoryID The ID of the AwardCategory to delete
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param query Queries to be added to the request. Ex. list(SchoolYearID = 1). Defaults to NULL.
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Student
	#' @return The AwardCategoryID of the deleted AwardCategory.
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	deleteAwardCategory <- function(AwardCategoryID, ignoreWarnings = F, entityId = 1, query = NULL, flatten = T, returnResponse = F){

		deleteSkyObject(module = "Student", objectName = "AwardCategory", objectId = AwardCategoryID, ignoreWarnings = ignoreWarnings, entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' Create an AwardCategory
	#'
	#' This function creates an AwardCategory
	#' @param fieldNames The field values to give the created AwardCategory. Each defaults to NULL.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param query Queries to be added to the request. Ex. list(SchoolYearID = 1). Defaults to NULL.
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Student
	#' @return A newly created AwardCategory
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	createAwardCategory <- function(Code = NULL, Description = NULL, entityId = 1, query = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment())

		body <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper()) %>% purrr::compact()

		createSkyObject(module = "Student", objectName = "AwardCategory", body = list(DataObject = body), searchFields = append("AwardCategoryID", body %>% names()), entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' Modify an AwardCategory
	#'
	#' This function modifies an AwardCategory
	#' @param fieldNames The field values to give the modified AwardCategory. Each defaults to NULL.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param query Queries to be added to the request. Ex. list(SchoolYearID = 1). Defaults to NULL.
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Student
	#' @return The modified AwardCategory
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	modifyAwardCategory <- function(AwardCategoryID, Code = NULL, Description = NULL, entityId = 1, query = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment())

		body <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper()) %>% purrr::compact()

		modifySkyObject(module = "Student", objectName = "AwardCategory", objectId = AwardCategoryID, body = list(DataObject = body), searchFields = append("AwardCategoryID", body %>% names()), entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' List AwardTypes
	#'
	#' This function returns a dataframe or json object of AwardTypes
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given AwardTypes. Defaults to FALSE for all return fields which, for convenience, returns all fields for the AwardTypes.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object listed as text. Run \code{\link{getSchemaForObjects}}('AwardType') to get more field paths.
	#' @param searchConditionsList A list of search conditions to filter results which are joined by the searchConditionsGroupType. Of the form {FieldName} {ConditionType} {SearchCondition}. For example, c('StudentID LessEqual 500', 'LastName Like Ander\%'). Run \code{\link{listSearchConditionTypes}} for a list of ConditionTypes. Defaults to NULL (unfiltered).
	#' @param searchConditionsGroupType The conjunction which joins multiple searchConditions in the searchConditionsList. Either 'Or' or 'And'. Defaults to 'And'.
	#' @param searchSortFieldNamesList The list of fields sort results by. Defaults to NULL (unsorted).
	#' @param searchSortFieldNamesDescendingList A list of T/F values corresponding to whether to sort each field in searchSortFieldNamesList in descending order. Defaults to F for each FieldName in searchSortFieldNamesList.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param query Queries to be added to the request. Ex. list(SchoolYearID = 1). Defaults to NULL.
	#' @param page Results are paginated. The page of results to return. Default is 1.
	#' @param pageSize Results are paginated. The number of records per page to return. Default is 100,000 (essentially all records for most objects).
	#' @param flatten Whether to flatten results into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Student
	#' @return A list of AwardTypes
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	listAwardTypes <- function(searchConditionsList = NULL, AwardTypeID = F, Code = F, Description = F, fieldPaths = NULL, searchConditionsGroupType = "And", searchSortFieldNamesList = NULL, searchSortFieldNamesDescendingList = NULL, entityId = 1, query = NULL, page = 1, pageSize = 100000, flatten = T, returnResponse = F){

		params <- as.list(environment())

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		listSkyObjects(module = "Student", objectName = "AwardType", searchFields = searchFields %>% append(fieldPaths), page = page, pageSize = pageSize, SearchConditionsList = searchConditionsList, SearchConditionsGroupType = searchConditionsGroupType, SearchSortFieldNamesList = searchSortFieldNamesList, SearchSortFieldNamesDescendingList = searchSortFieldNamesDescendingList, entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' Get an AwardType
	#'
	#' This function returns a dataframe or json object of an AwardType
	#' @param AwardTypeID The ID of the AwardType to return.
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given AwardType. Defaults to FALSE for all return fields which, for convenience, returns all fields for the AwardType.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object listed as text. Run \code{\link{getSchemaForObjects}}('AwardType') to get more field paths.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param query Queries to be added to the request. Ex. list(SchoolYearID = 1). Defaults to NULL.
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Student
	#' @return A dataframe or of AwardType
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	getAwardType <- function(AwardTypeID, Code = F, Description = F, fieldPaths = NULL, entityId = 1, query = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment()) %>% purrr::keep(names(.) != "AwardTypeID")

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		getSkyObject(module = "Student", objectName = "AwardType", objectId = AwardTypeID, searchFields = searchFields, entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' Delete an AwardType
	#'
	#' This function deletes an AwardType
	#' @param AwardTypeID The ID of the AwardType to delete
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param query Queries to be added to the request. Ex. list(SchoolYearID = 1). Defaults to NULL.
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Student
	#' @return The AwardTypeID of the deleted AwardType.
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	deleteAwardType <- function(AwardTypeID, ignoreWarnings = F, entityId = 1, query = NULL, flatten = T, returnResponse = F){

		deleteSkyObject(module = "Student", objectName = "AwardType", objectId = AwardTypeID, ignoreWarnings = ignoreWarnings, entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' Create an AwardType
	#'
	#' This function creates an AwardType
	#' @param fieldNames The field values to give the created AwardType. Each defaults to NULL.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param query Queries to be added to the request. Ex. list(SchoolYearID = 1). Defaults to NULL.
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Student
	#' @return A newly created AwardType
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	createAwardType <- function(Code = NULL, Description = NULL, entityId = 1, query = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment())

		body <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper()) %>% purrr::compact()

		createSkyObject(module = "Student", objectName = "AwardType", body = list(DataObject = body), searchFields = append("AwardTypeID", body %>% names()), entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' Modify an AwardType
	#'
	#' This function modifies an AwardType
	#' @param fieldNames The field values to give the modified AwardType. Each defaults to NULL.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param query Queries to be added to the request. Ex. list(SchoolYearID = 1). Defaults to NULL.
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Student
	#' @return The modified AwardType
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	modifyAwardType <- function(AwardTypeID, Code = NULL, Description = NULL, entityId = 1, query = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment())

		body <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper()) %>% purrr::compact()

		modifySkyObject(module = "Student", objectName = "AwardType", objectId = AwardTypeID, body = list(DataObject = body), searchFields = append("AwardTypeID", body %>% names()), entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' List AwardHardwares
	#'
	#' This function returns a dataframe or json object of AwardHardwares
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given AwardHardwares. Defaults to FALSE for all return fields which, for convenience, returns all fields for the AwardHardwares.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object listed as text. Run \code{\link{getSchemaForObjects}}('AwardHardware') to get more field paths.
	#' @param searchConditionsList A list of search conditions to filter results which are joined by the searchConditionsGroupType. Of the form {FieldName} {ConditionType} {SearchCondition}. For example, c('StudentID LessEqual 500', 'LastName Like Ander\%'). Run \code{\link{listSearchConditionTypes}} for a list of ConditionTypes. Defaults to NULL (unfiltered).
	#' @param searchConditionsGroupType The conjunction which joins multiple searchConditions in the searchConditionsList. Either 'Or' or 'And'. Defaults to 'And'.
	#' @param searchSortFieldNamesList The list of fields sort results by. Defaults to NULL (unsorted).
	#' @param searchSortFieldNamesDescendingList A list of T/F values corresponding to whether to sort each field in searchSortFieldNamesList in descending order. Defaults to F for each FieldName in searchSortFieldNamesList.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param query Queries to be added to the request. Ex. list(SchoolYearID = 1). Defaults to NULL.
	#' @param page Results are paginated. The page of results to return. Default is 1.
	#' @param pageSize Results are paginated. The number of records per page to return. Default is 100,000 (essentially all records for most objects).
	#' @param flatten Whether to flatten results into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Student
	#' @return A list of AwardHardwares
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	listAwardHardwares <- function(searchConditionsList = NULL, AwardHardwareID = F, Code = F, Description = F, fieldPaths = NULL, searchConditionsGroupType = "And", searchSortFieldNamesList = NULL, searchSortFieldNamesDescendingList = NULL, entityId = 1, query = NULL, page = 1, pageSize = 100000, flatten = T, returnResponse = F){

		params <- as.list(environment())

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		listSkyObjects(module = "Student", objectName = "AwardHardware", searchFields = searchFields %>% append(fieldPaths), page = page, pageSize = pageSize, SearchConditionsList = searchConditionsList, SearchConditionsGroupType = searchConditionsGroupType, SearchSortFieldNamesList = searchSortFieldNamesList, SearchSortFieldNamesDescendingList = searchSortFieldNamesDescendingList, entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' Get an AwardHardware
	#'
	#' This function returns a dataframe or json object of an AwardHardware
	#' @param AwardHardwareID The ID of the AwardHardware to return.
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given AwardHardware. Defaults to FALSE for all return fields which, for convenience, returns all fields for the AwardHardware.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object listed as text. Run \code{\link{getSchemaForObjects}}('AwardHardware') to get more field paths.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param query Queries to be added to the request. Ex. list(SchoolYearID = 1). Defaults to NULL.
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Student
	#' @return A dataframe or of AwardHardware
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	getAwardHardware <- function(AwardHardwareID, Code = F, Description = F, fieldPaths = NULL, entityId = 1, query = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment()) %>% purrr::keep(names(.) != "AwardHardwareID")

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		getSkyObject(module = "Student", objectName = "AwardHardware", objectId = AwardHardwareID, searchFields = searchFields, entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' Delete an AwardHardware
	#'
	#' This function deletes an AwardHardware
	#' @param AwardHardwareID The ID of the AwardHardware to delete
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param query Queries to be added to the request. Ex. list(SchoolYearID = 1). Defaults to NULL.
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Student
	#' @return The AwardHardwareID of the deleted AwardHardware.
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	deleteAwardHardware <- function(AwardHardwareID, ignoreWarnings = F, entityId = 1, query = NULL, flatten = T, returnResponse = F){

		deleteSkyObject(module = "Student", objectName = "AwardHardware", objectId = AwardHardwareID, ignoreWarnings = ignoreWarnings, entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' Create an AwardHardware
	#'
	#' This function creates an AwardHardware
	#' @param fieldNames The field values to give the created AwardHardware. Each defaults to NULL.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param query Queries to be added to the request. Ex. list(SchoolYearID = 1). Defaults to NULL.
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Student
	#' @return A newly created AwardHardware
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	createAwardHardware <- function(Code = NULL, Description = NULL, entityId = 1, query = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment())

		body <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper()) %>% purrr::compact()

		createSkyObject(module = "Student", objectName = "AwardHardware", body = list(DataObject = body), searchFields = append("AwardHardwareID", body %>% names()), entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' Modify an AwardHardware
	#'
	#' This function modifies an AwardHardware
	#' @param fieldNames The field values to give the modified AwardHardware. Each defaults to NULL.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param query Queries to be added to the request. Ex. list(SchoolYearID = 1). Defaults to NULL.
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Student
	#' @return The modified AwardHardware
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	modifyAwardHardware <- function(AwardHardwareID, Code = NULL, Description = NULL, entityId = 1, query = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment())

		body <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper()) %>% purrr::compact()

		modifySkyObject(module = "Student", objectName = "AwardHardware", objectId = AwardHardwareID, body = list(DataObject = body), searchFields = append("AwardHardwareID", body %>% names()), entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' List StudentAwards
	#'
	#' This function returns a dataframe or json object of StudentAwards
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given StudentAwards. Defaults to FALSE for all return fields which, for convenience, returns all fields for the StudentAwards.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object listed as text. Run \code{\link{getSchemaForObjects}}('StudentAward') to get more field paths.
	#' @param searchConditionsList A list of search conditions to filter results which are joined by the searchConditionsGroupType. Of the form {FieldName} {ConditionType} {SearchCondition}. For example, c('StudentID LessEqual 500', 'LastName Like Ander\%'). Run \code{\link{listSearchConditionTypes}} for a list of ConditionTypes. Defaults to NULL (unfiltered).
	#' @param searchConditionsGroupType The conjunction which joins multiple searchConditions in the searchConditionsList. Either 'Or' or 'And'. Defaults to 'And'.
	#' @param searchSortFieldNamesList The list of fields sort results by. Defaults to NULL (unsorted).
	#' @param searchSortFieldNamesDescendingList A list of T/F values corresponding to whether to sort each field in searchSortFieldNamesList in descending order. Defaults to F for each FieldName in searchSortFieldNamesList.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param query Queries to be added to the request. Ex. list(SchoolYearID = 1). Defaults to NULL.
	#' @param page Results are paginated. The page of results to return. Default is 1.
	#' @param pageSize Results are paginated. The number of records per page to return. Default is 100,000 (essentially all records for most objects).
	#' @param flatten Whether to flatten results into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Student
	#' @return A list of StudentAwards
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	listStudentAwards <- function(searchConditionsList = NULL, StudentAwardID = F, AwardCategoryID = F, AwardTypeID = F, AwardHardwareID = F, ActivityID = F, SchoolYearID = F, StudentID = F, fieldPaths = NULL, searchConditionsGroupType = "And", searchSortFieldNamesList = NULL, searchSortFieldNamesDescendingList = NULL, entityId = 1, query = NULL, page = 1, pageSize = 100000, flatten = T, returnResponse = F){

		params <- as.list(environment())

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		listSkyObjects(module = "Student", objectName = "StudentAward", searchFields = searchFields %>% append(fieldPaths), page = page, pageSize = pageSize, SearchConditionsList = searchConditionsList, SearchConditionsGroupType = searchConditionsGroupType, SearchSortFieldNamesList = searchSortFieldNamesList, SearchSortFieldNamesDescendingList = searchSortFieldNamesDescendingList, entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' Get a StudentAward
	#'
	#' This function returns a dataframe or json object of a StudentAward
	#' @param StudentAwardID The ID of the StudentAward to return.
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given StudentAward. Defaults to FALSE for all return fields which, for convenience, returns all fields for the StudentAward.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object listed as text. Run \code{\link{getSchemaForObjects}}('StudentAward') to get more field paths.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param query Queries to be added to the request. Ex. list(SchoolYearID = 1). Defaults to NULL.
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Student
	#' @return A dataframe or of StudentAward
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	getStudentAward <- function(StudentAwardID, AwardCategoryID = F, AwardTypeID = F, AwardHardwareID = F, ActivityID = F, SchoolYearID = F, StudentID = F, fieldPaths = NULL, entityId = 1, query = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment()) %>% purrr::keep(names(.) != "StudentAwardID")

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		getSkyObject(module = "Student", objectName = "StudentAward", objectId = StudentAwardID, searchFields = searchFields, entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' Delete a StudentAward
	#'
	#' This function deletes a StudentAward
	#' @param StudentAwardID The ID of the StudentAward to delete
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param query Queries to be added to the request. Ex. list(SchoolYearID = 1). Defaults to NULL.
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Student
	#' @return The StudentAwardID of the deleted StudentAward.
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	deleteStudentAward <- function(StudentAwardID, ignoreWarnings = F, entityId = 1, query = NULL, flatten = T, returnResponse = F){

		deleteSkyObject(module = "Student", objectName = "StudentAward", objectId = StudentAwardID, ignoreWarnings = ignoreWarnings, entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' Create a StudentAward
	#'
	#' This function creates a StudentAward
	#' @param fieldNames The field values to give the created StudentAward. Each defaults to NULL.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param query Queries to be added to the request. Ex. list(SchoolYearID = 1). Defaults to NULL.
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Student
	#' @return A newly created StudentAward
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	createStudentAward <- function(AwardCategoryID = NULL, AwardTypeID = NULL, AwardHardwareID = NULL, ActivityID = NULL, SchoolYearID = NULL, StudentID = NULL, entityId = 1, query = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment())

		body <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper()) %>% purrr::compact()

		createSkyObject(module = "Student", objectName = "StudentAward", body = list(DataObject = body), searchFields = append("StudentAwardID", body %>% names()), entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' Modify a StudentAward
	#'
	#' This function modifies a StudentAward
	#' @param fieldNames The field values to give the modified StudentAward. Each defaults to NULL.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param query Queries to be added to the request. Ex. list(SchoolYearID = 1). Defaults to NULL.
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Student
	#' @return The modified StudentAward
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	modifyStudentAward <- function(StudentAwardID, AwardCategoryID = NULL, AwardTypeID = NULL, AwardHardwareID = NULL, ActivityID = NULL, SchoolYearID = NULL, StudentID = NULL, entityId = 1, query = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment())

		body <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper()) %>% purrr::compact()

		modifySkyObject(module = "Student", objectName = "StudentAward", objectId = StudentAwardID, body = list(DataObject = body), searchFields = append("StudentAwardID", body %>% names()), entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' List TempMassAssignCTEIndicatorTXExceptions
	#'
	#' This function returns a dataframe or json object of TempMassAssignCTEIndicatorTXExceptions
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given TempMassAssignCTEIndicatorTXExceptions. Defaults to FALSE for all return fields which, for convenience, returns all fields for the TempMassAssignCTEIndicatorTXExceptions.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object listed as text. Run \code{\link{getSchemaForObjects}}('TempMassAssignCTEIndicatorTXException') to get more field paths.
	#' @param searchConditionsList A list of search conditions to filter results which are joined by the searchConditionsGroupType. Of the form {FieldName} {ConditionType} {SearchCondition}. For example, c('StudentID LessEqual 500', 'LastName Like Ander\%'). Run \code{\link{listSearchConditionTypes}} for a list of ConditionTypes. Defaults to NULL (unfiltered).
	#' @param searchConditionsGroupType The conjunction which joins multiple searchConditions in the searchConditionsList. Either 'Or' or 'And'. Defaults to 'And'.
	#' @param searchSortFieldNamesList The list of fields sort results by. Defaults to NULL (unsorted).
	#' @param searchSortFieldNamesDescendingList A list of T/F values corresponding to whether to sort each field in searchSortFieldNamesList in descending order. Defaults to F for each FieldName in searchSortFieldNamesList.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param query Queries to be added to the request. Ex. list(SchoolYearID = 1). Defaults to NULL.
	#' @param page Results are paginated. The page of results to return. Default is 1.
	#' @param pageSize Results are paginated. The number of records per page to return. Default is 100,000 (essentially all records for most objects).
	#' @param flatten Whether to flatten results into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Student
	#' @return A list of TempMassAssignCTEIndicatorTXExceptions
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	listTempMassAssignCTEIndicatorTXExceptions <- function(searchConditionsList = NULL, TempMassAssignCTEIndicatorTXExceptionID = F, TempMassAssignCTEIndicatorTXID = F, StudentID = F, Note = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, fieldPaths = NULL, searchConditionsGroupType = "And", searchSortFieldNamesList = NULL, searchSortFieldNamesDescendingList = NULL, entityId = 1, query = NULL, page = 1, pageSize = 100000, flatten = T, returnResponse = F){

		params <- as.list(environment())

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		listSkyObjects(module = "Student", objectName = "TempMassAssignCTEIndicatorTXException", searchFields = searchFields %>% append(fieldPaths), page = page, pageSize = pageSize, SearchConditionsList = searchConditionsList, SearchConditionsGroupType = searchConditionsGroupType, SearchSortFieldNamesList = searchSortFieldNamesList, SearchSortFieldNamesDescendingList = searchSortFieldNamesDescendingList, entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' Get a TempMassAssignCTEIndicatorTXException
	#'
	#' This function returns a dataframe or json object of a TempMassAssignCTEIndicatorTXException
	#' @param TempMassAssignCTEIndicatorTXExceptionID The ID of the TempMassAssignCTEIndicatorTXException to return.
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given TempMassAssignCTEIndicatorTXException. Defaults to FALSE for all return fields which, for convenience, returns all fields for the TempMassAssignCTEIndicatorTXException.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object listed as text. Run \code{\link{getSchemaForObjects}}('TempMassAssignCTEIndicatorTXException') to get more field paths.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param query Queries to be added to the request. Ex. list(SchoolYearID = 1). Defaults to NULL.
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Student
	#' @return A dataframe or of TempMassAssignCTEIndicatorTXException
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	getTempMassAssignCTEIndicatorTXException <- function(TempMassAssignCTEIndicatorTXExceptionID, TempMassAssignCTEIndicatorTXID = F, StudentID = F, Note = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, fieldPaths = NULL, entityId = 1, query = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment()) %>% purrr::keep(names(.) != "TempMassAssignCTEIndicatorTXExceptionID")

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		getSkyObject(module = "Student", objectName = "TempMassAssignCTEIndicatorTXException", objectId = TempMassAssignCTEIndicatorTXExceptionID, searchFields = searchFields, entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' Delete a TempMassAssignCTEIndicatorTXException
	#'
	#' This function deletes a TempMassAssignCTEIndicatorTXException
	#' @param TempMassAssignCTEIndicatorTXExceptionID The ID of the TempMassAssignCTEIndicatorTXException to delete
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param query Queries to be added to the request. Ex. list(SchoolYearID = 1). Defaults to NULL.
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Student
	#' @return The TempMassAssignCTEIndicatorTXExceptionID of the deleted TempMassAssignCTEIndicatorTXException.
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	deleteTempMassAssignCTEIndicatorTXException <- function(TempMassAssignCTEIndicatorTXExceptionID, ignoreWarnings = F, entityId = 1, query = NULL, flatten = T, returnResponse = F){

		deleteSkyObject(module = "Student", objectName = "TempMassAssignCTEIndicatorTXException", objectId = TempMassAssignCTEIndicatorTXExceptionID, ignoreWarnings = ignoreWarnings, entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' Create a TempMassAssignCTEIndicatorTXException
	#'
	#' This function creates a TempMassAssignCTEIndicatorTXException
	#' @param fieldNames The field values to give the created TempMassAssignCTEIndicatorTXException. Each defaults to NULL.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param query Queries to be added to the request. Ex. list(SchoolYearID = 1). Defaults to NULL.
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Student
	#' @return A newly created TempMassAssignCTEIndicatorTXException
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	createTempMassAssignCTEIndicatorTXException <- function(TempMassAssignCTEIndicatorTXID = NULL, StudentID = NULL, Note = NULL, entityId = 1, query = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment())

		body <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper()) %>% purrr::compact()

		createSkyObject(module = "Student", objectName = "TempMassAssignCTEIndicatorTXException", body = list(DataObject = body), searchFields = append("TempMassAssignCTEIndicatorTXExceptionID", body %>% names()), entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' Modify a TempMassAssignCTEIndicatorTXException
	#'
	#' This function modifies a TempMassAssignCTEIndicatorTXException
	#' @param fieldNames The field values to give the modified TempMassAssignCTEIndicatorTXException. Each defaults to NULL.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param query Queries to be added to the request. Ex. list(SchoolYearID = 1). Defaults to NULL.
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Student
	#' @return The modified TempMassAssignCTEIndicatorTXException
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	modifyTempMassAssignCTEIndicatorTXException <- function(TempMassAssignCTEIndicatorTXExceptionID, TempMassAssignCTEIndicatorTXID = NULL, StudentID = NULL, Note = NULL, entityId = 1, query = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment())

		body <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper()) %>% purrr::compact()

		modifySkyObject(module = "Student", objectName = "TempMassAssignCTEIndicatorTXException", objectId = TempMassAssignCTEIndicatorTXExceptionID, body = list(DataObject = body), searchFields = append("TempMassAssignCTEIndicatorTXExceptionID", body %>% names()), entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' List SchoolMintRunHistoryStudentErrors
	#'
	#' This function returns a dataframe or json object of SchoolMintRunHistoryStudentErrors
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given SchoolMintRunHistoryStudentErrors. Defaults to FALSE for all return fields which, for convenience, returns all fields for the SchoolMintRunHistoryStudentErrors.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object listed as text. Run \code{\link{getSchemaForObjects}}('SchoolMintRunHistoryStudentError') to get more field paths.
	#' @param searchConditionsList A list of search conditions to filter results which are joined by the searchConditionsGroupType. Of the form {FieldName} {ConditionType} {SearchCondition}. For example, c('StudentID LessEqual 500', 'LastName Like Ander\%'). Run \code{\link{listSearchConditionTypes}} for a list of ConditionTypes. Defaults to NULL (unfiltered).
	#' @param searchConditionsGroupType The conjunction which joins multiple searchConditions in the searchConditionsList. Either 'Or' or 'And'. Defaults to 'And'.
	#' @param searchSortFieldNamesList The list of fields sort results by. Defaults to NULL (unsorted).
	#' @param searchSortFieldNamesDescendingList A list of T/F values corresponding to whether to sort each field in searchSortFieldNamesList in descending order. Defaults to F for each FieldName in searchSortFieldNamesList.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param query Queries to be added to the request. Ex. list(SchoolYearID = 1). Defaults to NULL.
	#' @param page Results are paginated. The page of results to return. Default is 1.
	#' @param pageSize Results are paginated. The number of records per page to return. Default is 100,000 (essentially all records for most objects).
	#' @param flatten Whether to flatten results into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Student
	#' @return A list of SchoolMintRunHistoryStudentErrors
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	listSchoolMintRunHistoryStudentErrors <- function(searchConditionsList = NULL, SchoolMintRunHistoryStudentErrorID = F, SchoolMintRunHistoryID = F, StudentIdentifier = F, StudentNameLFM = F, SchoolName = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, ContainsNonStudentInfoDetails = F, fieldPaths = NULL, searchConditionsGroupType = "And", searchSortFieldNamesList = NULL, searchSortFieldNamesDescendingList = NULL, entityId = 1, query = NULL, page = 1, pageSize = 100000, flatten = T, returnResponse = F){

		params <- as.list(environment())

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		listSkyObjects(module = "Student", objectName = "SchoolMintRunHistoryStudentError", searchFields = searchFields %>% append(fieldPaths), page = page, pageSize = pageSize, SearchConditionsList = searchConditionsList, SearchConditionsGroupType = searchConditionsGroupType, SearchSortFieldNamesList = searchSortFieldNamesList, SearchSortFieldNamesDescendingList = searchSortFieldNamesDescendingList, entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' Get a SchoolMintRunHistoryStudentError
	#'
	#' This function returns a dataframe or json object of a SchoolMintRunHistoryStudentError
	#' @param SchoolMintRunHistoryStudentErrorID The ID of the SchoolMintRunHistoryStudentError to return.
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given SchoolMintRunHistoryStudentError. Defaults to FALSE for all return fields which, for convenience, returns all fields for the SchoolMintRunHistoryStudentError.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object listed as text. Run \code{\link{getSchemaForObjects}}('SchoolMintRunHistoryStudentError') to get more field paths.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param query Queries to be added to the request. Ex. list(SchoolYearID = 1). Defaults to NULL.
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Student
	#' @return A dataframe or of SchoolMintRunHistoryStudentError
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	getSchoolMintRunHistoryStudentError <- function(SchoolMintRunHistoryStudentErrorID, SchoolMintRunHistoryID = F, StudentIdentifier = F, StudentNameLFM = F, SchoolName = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, ContainsNonStudentInfoDetails = F, fieldPaths = NULL, entityId = 1, query = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment()) %>% purrr::keep(names(.) != "SchoolMintRunHistoryStudentErrorID")

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		getSkyObject(module = "Student", objectName = "SchoolMintRunHistoryStudentError", objectId = SchoolMintRunHistoryStudentErrorID, searchFields = searchFields, entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' Delete a SchoolMintRunHistoryStudentError
	#'
	#' This function deletes a SchoolMintRunHistoryStudentError
	#' @param SchoolMintRunHistoryStudentErrorID The ID of the SchoolMintRunHistoryStudentError to delete
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param query Queries to be added to the request. Ex. list(SchoolYearID = 1). Defaults to NULL.
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Student
	#' @return The SchoolMintRunHistoryStudentErrorID of the deleted SchoolMintRunHistoryStudentError.
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	deleteSchoolMintRunHistoryStudentError <- function(SchoolMintRunHistoryStudentErrorID, ignoreWarnings = F, entityId = 1, query = NULL, flatten = T, returnResponse = F){

		deleteSkyObject(module = "Student", objectName = "SchoolMintRunHistoryStudentError", objectId = SchoolMintRunHistoryStudentErrorID, ignoreWarnings = ignoreWarnings, entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' Create a SchoolMintRunHistoryStudentError
	#'
	#' This function creates a SchoolMintRunHistoryStudentError
	#' @param fieldNames The field values to give the created SchoolMintRunHistoryStudentError. Each defaults to NULL.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param query Queries to be added to the request. Ex. list(SchoolYearID = 1). Defaults to NULL.
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Student
	#' @return A newly created SchoolMintRunHistoryStudentError
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	createSchoolMintRunHistoryStudentError <- function(SchoolMintRunHistoryID = NULL, StudentIdentifier = NULL, StudentNameLFM = NULL, SchoolName = NULL, entityId = 1, query = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment())

		body <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper()) %>% purrr::compact()

		createSkyObject(module = "Student", objectName = "SchoolMintRunHistoryStudentError", body = list(DataObject = body), searchFields = append("SchoolMintRunHistoryStudentErrorID", body %>% names()), entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' Modify a SchoolMintRunHistoryStudentError
	#'
	#' This function modifies a SchoolMintRunHistoryStudentError
	#' @param fieldNames The field values to give the modified SchoolMintRunHistoryStudentError. Each defaults to NULL.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param query Queries to be added to the request. Ex. list(SchoolYearID = 1). Defaults to NULL.
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Student
	#' @return The modified SchoolMintRunHistoryStudentError
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	modifySchoolMintRunHistoryStudentError <- function(SchoolMintRunHistoryStudentErrorID, SchoolMintRunHistoryID = NULL, StudentIdentifier = NULL, StudentNameLFM = NULL, SchoolName = NULL, entityId = 1, query = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment())

		body <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper()) %>% purrr::compact()

		modifySkyObject(module = "Student", objectName = "SchoolMintRunHistoryStudentError", objectId = SchoolMintRunHistoryStudentErrorID, body = list(DataObject = body), searchFields = append("SchoolMintRunHistoryStudentErrorID", body %>% names()), entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' List SchoolMintRunHistories
	#'
	#' This function returns a dataframe or json object of SchoolMintRunHistories
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given SchoolMintRunHistories. Defaults to FALSE for all return fields which, for convenience, returns all fields for the SchoolMintRunHistories.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object listed as text. Run \code{\link{getSchemaForObjects}}('SchoolMintRunHistory') to get more field paths.
	#' @param searchConditionsList A list of search conditions to filter results which are joined by the searchConditionsGroupType. Of the form {FieldName} {ConditionType} {SearchCondition}. For example, c('StudentID LessEqual 500', 'LastName Like Ander\%'). Run \code{\link{listSearchConditionTypes}} for a list of ConditionTypes. Defaults to NULL (unfiltered).
	#' @param searchConditionsGroupType The conjunction which joins multiple searchConditions in the searchConditionsList. Either 'Or' or 'And'. Defaults to 'And'.
	#' @param searchSortFieldNamesList The list of fields sort results by. Defaults to NULL (unsorted).
	#' @param searchSortFieldNamesDescendingList A list of T/F values corresponding to whether to sort each field in searchSortFieldNamesList in descending order. Defaults to F for each FieldName in searchSortFieldNamesList.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param query Queries to be added to the request. Ex. list(SchoolYearID = 1). Defaults to NULL.
	#' @param page Results are paginated. The page of results to return. Default is 1.
	#' @param pageSize Results are paginated. The number of records per page to return. Default is 100,000 (essentially all records for most objects).
	#' @param flatten Whether to flatten results into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Student
	#' @return A list of SchoolMintRunHistories
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	listSchoolMintRunHistories <- function(searchConditionsList = NULL, SchoolMintRunHistoryID = F, SchoolYearID = F, RunTime = F, SuccessCount = F, StudentSuccessCount = F, AddressSuccessCount = F, GuardianSuccessCount = F, FoodServiceSuccessCount = F, FailCount = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, fieldPaths = NULL, searchConditionsGroupType = "And", searchSortFieldNamesList = NULL, searchSortFieldNamesDescendingList = NULL, entityId = 1, query = NULL, page = 1, pageSize = 100000, flatten = T, returnResponse = F){

		params <- as.list(environment())

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		listSkyObjects(module = "Student", objectName = "SchoolMintRunHistory", searchFields = searchFields %>% append(fieldPaths), page = page, pageSize = pageSize, SearchConditionsList = searchConditionsList, SearchConditionsGroupType = searchConditionsGroupType, SearchSortFieldNamesList = searchSortFieldNamesList, SearchSortFieldNamesDescendingList = searchSortFieldNamesDescendingList, entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' Get a SchoolMintRunHistory
	#'
	#' This function returns a dataframe or json object of a SchoolMintRunHistory
	#' @param SchoolMintRunHistoryID The ID of the SchoolMintRunHistory to return.
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given SchoolMintRunHistory. Defaults to FALSE for all return fields which, for convenience, returns all fields for the SchoolMintRunHistory.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object listed as text. Run \code{\link{getSchemaForObjects}}('SchoolMintRunHistory') to get more field paths.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param query Queries to be added to the request. Ex. list(SchoolYearID = 1). Defaults to NULL.
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Student
	#' @return A dataframe or of SchoolMintRunHistory
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	getSchoolMintRunHistory <- function(SchoolMintRunHistoryID, SchoolYearID = F, RunTime = F, SuccessCount = F, StudentSuccessCount = F, AddressSuccessCount = F, GuardianSuccessCount = F, FoodServiceSuccessCount = F, FailCount = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, fieldPaths = NULL, entityId = 1, query = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment()) %>% purrr::keep(names(.) != "SchoolMintRunHistoryID")

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		getSkyObject(module = "Student", objectName = "SchoolMintRunHistory", objectId = SchoolMintRunHistoryID, searchFields = searchFields, entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' Delete a SchoolMintRunHistory
	#'
	#' This function deletes a SchoolMintRunHistory
	#' @param SchoolMintRunHistoryID The ID of the SchoolMintRunHistory to delete
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param query Queries to be added to the request. Ex. list(SchoolYearID = 1). Defaults to NULL.
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Student
	#' @return The SchoolMintRunHistoryID of the deleted SchoolMintRunHistory.
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	deleteSchoolMintRunHistory <- function(SchoolMintRunHistoryID, ignoreWarnings = F, entityId = 1, query = NULL, flatten = T, returnResponse = F){

		deleteSkyObject(module = "Student", objectName = "SchoolMintRunHistory", objectId = SchoolMintRunHistoryID, ignoreWarnings = ignoreWarnings, entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' Create a SchoolMintRunHistory
	#'
	#' This function creates a SchoolMintRunHistory
	#' @param fieldNames The field values to give the created SchoolMintRunHistory. Each defaults to NULL.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param query Queries to be added to the request. Ex. list(SchoolYearID = 1). Defaults to NULL.
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Student
	#' @return A newly created SchoolMintRunHistory
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	createSchoolMintRunHistory <- function(SchoolYearID = NULL, RunTime = NULL, SuccessCount = NULL, StudentSuccessCount = NULL, AddressSuccessCount = NULL, GuardianSuccessCount = NULL, FoodServiceSuccessCount = NULL, FailCount = NULL, entityId = 1, query = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment())

		body <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper()) %>% purrr::compact()

		createSkyObject(module = "Student", objectName = "SchoolMintRunHistory", body = list(DataObject = body), searchFields = append("SchoolMintRunHistoryID", body %>% names()), entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' Modify a SchoolMintRunHistory
	#'
	#' This function modifies a SchoolMintRunHistory
	#' @param fieldNames The field values to give the modified SchoolMintRunHistory. Each defaults to NULL.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param query Queries to be added to the request. Ex. list(SchoolYearID = 1). Defaults to NULL.
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Student
	#' @return The modified SchoolMintRunHistory
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	modifySchoolMintRunHistory <- function(SchoolMintRunHistoryID, SchoolYearID = NULL, RunTime = NULL, SuccessCount = NULL, StudentSuccessCount = NULL, AddressSuccessCount = NULL, GuardianSuccessCount = NULL, FoodServiceSuccessCount = NULL, FailCount = NULL, entityId = 1, query = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment())

		body <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper()) %>% purrr::compact()

		modifySkyObject(module = "Student", objectName = "SchoolMintRunHistory", objectId = SchoolMintRunHistoryID, body = list(DataObject = body), searchFields = append("SchoolMintRunHistoryID", body %>% names()), entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' List TempEmbraces
	#'
	#' This function returns a dataframe or json object of TempEmbraces
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given TempEmbraces. Defaults to FALSE for all return fields which, for convenience, returns all fields for the TempEmbraces.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object listed as text. Run \code{\link{getSchemaForObjects}}('TempEmbrace') to get more field paths.
	#' @param searchConditionsList A list of search conditions to filter results which are joined by the searchConditionsGroupType. Of the form {FieldName} {ConditionType} {SearchCondition}. For example, c('StudentID LessEqual 500', 'LastName Like Ander\%'). Run \code{\link{listSearchConditionTypes}} for a list of ConditionTypes. Defaults to NULL (unfiltered).
	#' @param searchConditionsGroupType The conjunction which joins multiple searchConditions in the searchConditionsList. Either 'Or' or 'And'. Defaults to 'And'.
	#' @param searchSortFieldNamesList The list of fields sort results by. Defaults to NULL (unsorted).
	#' @param searchSortFieldNamesDescendingList A list of T/F values corresponding to whether to sort each field in searchSortFieldNamesList in descending order. Defaults to F for each FieldName in searchSortFieldNamesList.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param query Queries to be added to the request. Ex. list(SchoolYearID = 1). Defaults to NULL.
	#' @param page Results are paginated. The page of results to return. Default is 1.
	#' @param pageSize Results are paginated. The number of records per page to return. Default is 100,000 (essentially all records for most objects).
	#' @param flatten Whether to flatten results into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Student
	#' @return A list of TempEmbraces
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	listTempEmbraces <- function(searchConditionsList = NULL, TempEmbraceID = F, HasExceptions = F, Exceptions = F, StudentNumber = F, StudentStateNumber = F, FirstName = F, MiddleName = F, LastName = F, HasIEP = F, IEPStartDate = F, PrimaryEligibility = F, SecondaryEligibility = F, HasSection504 = F, Section504StartDate = F, Section504EndDate = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, fieldPaths = NULL, searchConditionsGroupType = "And", searchSortFieldNamesList = NULL, searchSortFieldNamesDescendingList = NULL, entityId = 1, query = NULL, page = 1, pageSize = 100000, flatten = T, returnResponse = F){

		params <- as.list(environment())

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		listSkyObjects(module = "Student", objectName = "TempEmbrace", searchFields = searchFields %>% append(fieldPaths), page = page, pageSize = pageSize, SearchConditionsList = searchConditionsList, SearchConditionsGroupType = searchConditionsGroupType, SearchSortFieldNamesList = searchSortFieldNamesList, SearchSortFieldNamesDescendingList = searchSortFieldNamesDescendingList, entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' Get a TempEmbrace
	#'
	#' This function returns a dataframe or json object of a TempEmbrace
	#' @param TempEmbraceID The ID of the TempEmbrace to return.
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given TempEmbrace. Defaults to FALSE for all return fields which, for convenience, returns all fields for the TempEmbrace.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object listed as text. Run \code{\link{getSchemaForObjects}}('TempEmbrace') to get more field paths.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param query Queries to be added to the request. Ex. list(SchoolYearID = 1). Defaults to NULL.
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Student
	#' @return A dataframe or of TempEmbrace
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	getTempEmbrace <- function(TempEmbraceID, HasExceptions = F, Exceptions = F, StudentNumber = F, StudentStateNumber = F, FirstName = F, MiddleName = F, LastName = F, HasIEP = F, IEPStartDate = F, PrimaryEligibility = F, SecondaryEligibility = F, HasSection504 = F, Section504StartDate = F, Section504EndDate = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, fieldPaths = NULL, entityId = 1, query = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment()) %>% purrr::keep(names(.) != "TempEmbraceID")

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		getSkyObject(module = "Student", objectName = "TempEmbrace", objectId = TempEmbraceID, searchFields = searchFields, entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' Delete a TempEmbrace
	#'
	#' This function deletes a TempEmbrace
	#' @param TempEmbraceID The ID of the TempEmbrace to delete
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param query Queries to be added to the request. Ex. list(SchoolYearID = 1). Defaults to NULL.
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Student
	#' @return The TempEmbraceID of the deleted TempEmbrace.
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	deleteTempEmbrace <- function(TempEmbraceID, ignoreWarnings = F, entityId = 1, query = NULL, flatten = T, returnResponse = F){

		deleteSkyObject(module = "Student", objectName = "TempEmbrace", objectId = TempEmbraceID, ignoreWarnings = ignoreWarnings, entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' Create a TempEmbrace
	#'
	#' This function creates a TempEmbrace
	#' @param fieldNames The field values to give the created TempEmbrace. Each defaults to NULL.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param query Queries to be added to the request. Ex. list(SchoolYearID = 1). Defaults to NULL.
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Student
	#' @return A newly created TempEmbrace
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	createTempEmbrace <- function(HasExceptions = NULL, Exceptions = NULL, StudentNumber = NULL, StudentStateNumber = NULL, FirstName = NULL, MiddleName = NULL, LastName = NULL, HasIEP = NULL, IEPStartDate = NULL, PrimaryEligibility = NULL, SecondaryEligibility = NULL, HasSection504 = NULL, Section504StartDate = NULL, Section504EndDate = NULL, entityId = 1, query = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment())

		body <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper()) %>% purrr::compact()

		createSkyObject(module = "Student", objectName = "TempEmbrace", body = list(DataObject = body), searchFields = append("TempEmbraceID", body %>% names()), entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' Modify a TempEmbrace
	#'
	#' This function modifies a TempEmbrace
	#' @param fieldNames The field values to give the modified TempEmbrace. Each defaults to NULL.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param query Queries to be added to the request. Ex. list(SchoolYearID = 1). Defaults to NULL.
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Student
	#' @return The modified TempEmbrace
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	modifyTempEmbrace <- function(TempEmbraceID, HasExceptions = NULL, Exceptions = NULL, StudentNumber = NULL, StudentStateNumber = NULL, FirstName = NULL, MiddleName = NULL, LastName = NULL, HasIEP = NULL, IEPStartDate = NULL, PrimaryEligibility = NULL, SecondaryEligibility = NULL, HasSection504 = NULL, Section504StartDate = NULL, Section504EndDate = NULL, entityId = 1, query = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment())

		body <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper()) %>% purrr::compact()

		modifySkyObject(module = "Student", objectName = "TempEmbrace", objectId = TempEmbraceID, body = list(DataObject = body), searchFields = append("TempEmbraceID", body %>% names()), entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' List SchoolMintRunHistoryStudentErrorDetails
	#'
	#' This function returns a dataframe or json object of SchoolMintRunHistoryStudentErrorDetails
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given SchoolMintRunHistoryStudentErrorDetails. Defaults to FALSE for all return fields which, for convenience, returns all fields for the SchoolMintRunHistoryStudentErrorDetails.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object listed as text. Run \code{\link{getSchemaForObjects}}('SchoolMintRunHistoryStudentErrorDetail') to get more field paths.
	#' @param searchConditionsList A list of search conditions to filter results which are joined by the searchConditionsGroupType. Of the form {FieldName} {ConditionType} {SearchCondition}. For example, c('StudentID LessEqual 500', 'LastName Like Ander\%'). Run \code{\link{listSearchConditionTypes}} for a list of ConditionTypes. Defaults to NULL (unfiltered).
	#' @param searchConditionsGroupType The conjunction which joins multiple searchConditions in the searchConditionsList. Either 'Or' or 'And'. Defaults to 'And'.
	#' @param searchSortFieldNamesList The list of fields sort results by. Defaults to NULL (unsorted).
	#' @param searchSortFieldNamesDescendingList A list of T/F values corresponding to whether to sort each field in searchSortFieldNamesList in descending order. Defaults to F for each FieldName in searchSortFieldNamesList.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param query Queries to be added to the request. Ex. list(SchoolYearID = 1). Defaults to NULL.
	#' @param page Results are paginated. The page of results to return. Default is 1.
	#' @param pageSize Results are paginated. The number of records per page to return. Default is 100,000 (essentially all records for most objects).
	#' @param flatten Whether to flatten results into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Student
	#' @return A list of SchoolMintRunHistoryStudentErrorDetails
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	listSchoolMintRunHistoryStudentErrorDetails <- function(searchConditionsList = NULL, SchoolMintRunHistoryStudentErrorDetailID = F, SchoolMintRunHistoryStudentErrorID = F, ObjectName = F, FieldValue = F, FieldName = F, Message = F, Rank = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, fieldPaths = NULL, searchConditionsGroupType = "And", searchSortFieldNamesList = NULL, searchSortFieldNamesDescendingList = NULL, entityId = 1, query = NULL, page = 1, pageSize = 100000, flatten = T, returnResponse = F){

		params <- as.list(environment())

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		listSkyObjects(module = "Student", objectName = "SchoolMintRunHistoryStudentErrorDetail", searchFields = searchFields %>% append(fieldPaths), page = page, pageSize = pageSize, SearchConditionsList = searchConditionsList, SearchConditionsGroupType = searchConditionsGroupType, SearchSortFieldNamesList = searchSortFieldNamesList, SearchSortFieldNamesDescendingList = searchSortFieldNamesDescendingList, entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' Get a SchoolMintRunHistoryStudentErrorDetail
	#'
	#' This function returns a dataframe or json object of a SchoolMintRunHistoryStudentErrorDetail
	#' @param SchoolMintRunHistoryStudentErrorDetailID The ID of the SchoolMintRunHistoryStudentErrorDetail to return.
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given SchoolMintRunHistoryStudentErrorDetail. Defaults to FALSE for all return fields which, for convenience, returns all fields for the SchoolMintRunHistoryStudentErrorDetail.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object listed as text. Run \code{\link{getSchemaForObjects}}('SchoolMintRunHistoryStudentErrorDetail') to get more field paths.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param query Queries to be added to the request. Ex. list(SchoolYearID = 1). Defaults to NULL.
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Student
	#' @return A dataframe or of SchoolMintRunHistoryStudentErrorDetail
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	getSchoolMintRunHistoryStudentErrorDetail <- function(SchoolMintRunHistoryStudentErrorDetailID, SchoolMintRunHistoryStudentErrorID = F, ObjectName = F, FieldValue = F, FieldName = F, Message = F, Rank = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, fieldPaths = NULL, entityId = 1, query = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment()) %>% purrr::keep(names(.) != "SchoolMintRunHistoryStudentErrorDetailID")

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		getSkyObject(module = "Student", objectName = "SchoolMintRunHistoryStudentErrorDetail", objectId = SchoolMintRunHistoryStudentErrorDetailID, searchFields = searchFields, entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' Delete a SchoolMintRunHistoryStudentErrorDetail
	#'
	#' This function deletes a SchoolMintRunHistoryStudentErrorDetail
	#' @param SchoolMintRunHistoryStudentErrorDetailID The ID of the SchoolMintRunHistoryStudentErrorDetail to delete
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param query Queries to be added to the request. Ex. list(SchoolYearID = 1). Defaults to NULL.
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Student
	#' @return The SchoolMintRunHistoryStudentErrorDetailID of the deleted SchoolMintRunHistoryStudentErrorDetail.
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	deleteSchoolMintRunHistoryStudentErrorDetail <- function(SchoolMintRunHistoryStudentErrorDetailID, ignoreWarnings = F, entityId = 1, query = NULL, flatten = T, returnResponse = F){

		deleteSkyObject(module = "Student", objectName = "SchoolMintRunHistoryStudentErrorDetail", objectId = SchoolMintRunHistoryStudentErrorDetailID, ignoreWarnings = ignoreWarnings, entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' Create a SchoolMintRunHistoryStudentErrorDetail
	#'
	#' This function creates a SchoolMintRunHistoryStudentErrorDetail
	#' @param fieldNames The field values to give the created SchoolMintRunHistoryStudentErrorDetail. Each defaults to NULL.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param query Queries to be added to the request. Ex. list(SchoolYearID = 1). Defaults to NULL.
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Student
	#' @return A newly created SchoolMintRunHistoryStudentErrorDetail
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	createSchoolMintRunHistoryStudentErrorDetail <- function(SchoolMintRunHistoryStudentErrorID = NULL, ObjectName = NULL, FieldValue = NULL, FieldName = NULL, Message = NULL, entityId = 1, query = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment())

		body <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper()) %>% purrr::compact()

		createSkyObject(module = "Student", objectName = "SchoolMintRunHistoryStudentErrorDetail", body = list(DataObject = body), searchFields = append("SchoolMintRunHistoryStudentErrorDetailID", body %>% names()), entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' Modify a SchoolMintRunHistoryStudentErrorDetail
	#'
	#' This function modifies a SchoolMintRunHistoryStudentErrorDetail
	#' @param fieldNames The field values to give the modified SchoolMintRunHistoryStudentErrorDetail. Each defaults to NULL.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param query Queries to be added to the request. Ex. list(SchoolYearID = 1). Defaults to NULL.
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Student
	#' @return The modified SchoolMintRunHistoryStudentErrorDetail
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	modifySchoolMintRunHistoryStudentErrorDetail <- function(SchoolMintRunHistoryStudentErrorDetailID, SchoolMintRunHistoryStudentErrorID = NULL, ObjectName = NULL, FieldValue = NULL, FieldName = NULL, Message = NULL, entityId = 1, query = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment())

		body <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper()) %>% purrr::compact()

		modifySkyObject(module = "Student", objectName = "SchoolMintRunHistoryStudentErrorDetail", objectId = SchoolMintRunHistoryStudentErrorDetailID, body = list(DataObject = body), searchFields = append("SchoolMintRunHistoryStudentErrorDetailID", body %>% names()), entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' List TechnologyAtHomes
	#'
	#' This function returns a dataframe or json object of TechnologyAtHomes
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given TechnologyAtHomes. Defaults to FALSE for all return fields which, for convenience, returns all fields for the TechnologyAtHomes.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object listed as text. Run \code{\link{getSchemaForObjects}}('TechnologyAtHome') to get more field paths.
	#' @param searchConditionsList A list of search conditions to filter results which are joined by the searchConditionsGroupType. Of the form {FieldName} {ConditionType} {SearchCondition}. For example, c('StudentID LessEqual 500', 'LastName Like Ander\%'). Run \code{\link{listSearchConditionTypes}} for a list of ConditionTypes. Defaults to NULL (unfiltered).
	#' @param searchConditionsGroupType The conjunction which joins multiple searchConditions in the searchConditionsList. Either 'Or' or 'And'. Defaults to 'And'.
	#' @param searchSortFieldNamesList The list of fields sort results by. Defaults to NULL (unsorted).
	#' @param searchSortFieldNamesDescendingList A list of T/F values corresponding to whether to sort each field in searchSortFieldNamesList in descending order. Defaults to F for each FieldName in searchSortFieldNamesList.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param query Queries to be added to the request. Ex. list(SchoolYearID = 1). Defaults to NULL.
	#' @param page Results are paginated. The page of results to return. Default is 1.
	#' @param pageSize Results are paginated. The number of records per page to return. Default is 100,000 (essentially all records for most objects).
	#' @param flatten Whether to flatten results into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Student
	#' @return A list of TechnologyAtHomes
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	listTechnologyAtHomes <- function(searchConditionsList = NULL, TechnologyAtHomeID = F, StudentFamilyID = F, SurveyedDate = F, InternetAccessID = F, InternetAccessTypeID = F, InternetPerformanceID = F, DeviceID = F, DeviceAccessID = F, DeviceAccessSourceID = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, fieldPaths = NULL, searchConditionsGroupType = "And", searchSortFieldNamesList = NULL, searchSortFieldNamesDescendingList = NULL, entityId = 1, query = NULL, page = 1, pageSize = 100000, flatten = T, returnResponse = F){

		params <- as.list(environment())

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		listSkyObjects(module = "Student", objectName = "TechnologyAtHome", searchFields = searchFields %>% append(fieldPaths), page = page, pageSize = pageSize, SearchConditionsList = searchConditionsList, SearchConditionsGroupType = searchConditionsGroupType, SearchSortFieldNamesList = searchSortFieldNamesList, SearchSortFieldNamesDescendingList = searchSortFieldNamesDescendingList, entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' Get a TechnologyAtHome
	#'
	#' This function returns a dataframe or json object of a TechnologyAtHome
	#' @param TechnologyAtHomeID The ID of the TechnologyAtHome to return.
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given TechnologyAtHome. Defaults to FALSE for all return fields which, for convenience, returns all fields for the TechnologyAtHome.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object listed as text. Run \code{\link{getSchemaForObjects}}('TechnologyAtHome') to get more field paths.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param query Queries to be added to the request. Ex. list(SchoolYearID = 1). Defaults to NULL.
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Student
	#' @return A dataframe or of TechnologyAtHome
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	getTechnologyAtHome <- function(TechnologyAtHomeID, StudentFamilyID = F, SurveyedDate = F, InternetAccessID = F, InternetAccessTypeID = F, InternetPerformanceID = F, DeviceID = F, DeviceAccessID = F, DeviceAccessSourceID = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, fieldPaths = NULL, entityId = 1, query = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment()) %>% purrr::keep(names(.) != "TechnologyAtHomeID")

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		getSkyObject(module = "Student", objectName = "TechnologyAtHome", objectId = TechnologyAtHomeID, searchFields = searchFields, entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' Delete a TechnologyAtHome
	#'
	#' This function deletes a TechnologyAtHome
	#' @param TechnologyAtHomeID The ID of the TechnologyAtHome to delete
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param query Queries to be added to the request. Ex. list(SchoolYearID = 1). Defaults to NULL.
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Student
	#' @return The TechnologyAtHomeID of the deleted TechnologyAtHome.
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	deleteTechnologyAtHome <- function(TechnologyAtHomeID, ignoreWarnings = F, entityId = 1, query = NULL, flatten = T, returnResponse = F){

		deleteSkyObject(module = "Student", objectName = "TechnologyAtHome", objectId = TechnologyAtHomeID, ignoreWarnings = ignoreWarnings, entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' Create a TechnologyAtHome
	#'
	#' This function creates a TechnologyAtHome
	#' @param fieldNames The field values to give the created TechnologyAtHome. Each defaults to NULL.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param query Queries to be added to the request. Ex. list(SchoolYearID = 1). Defaults to NULL.
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Student
	#' @return A newly created TechnologyAtHome
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	createTechnologyAtHome <- function(StudentFamilyID = NULL, SurveyedDate = NULL, InternetAccessID = NULL, InternetAccessTypeID = NULL, InternetPerformanceID = NULL, DeviceID = NULL, DeviceAccessID = NULL, DeviceAccessSourceID = NULL, entityId = 1, query = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment())

		body <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper()) %>% purrr::compact()

		createSkyObject(module = "Student", objectName = "TechnologyAtHome", body = list(DataObject = body), searchFields = append("TechnologyAtHomeID", body %>% names()), entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' Modify a TechnologyAtHome
	#'
	#' This function modifies a TechnologyAtHome
	#' @param fieldNames The field values to give the modified TechnologyAtHome. Each defaults to NULL.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param query Queries to be added to the request. Ex. list(SchoolYearID = 1). Defaults to NULL.
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Student
	#' @return The modified TechnologyAtHome
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	modifyTechnologyAtHome <- function(TechnologyAtHomeID, StudentFamilyID = NULL, SurveyedDate = NULL, InternetAccessID = NULL, InternetAccessTypeID = NULL, InternetPerformanceID = NULL, DeviceID = NULL, DeviceAccessID = NULL, DeviceAccessSourceID = NULL, entityId = 1, query = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment())

		body <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper()) %>% purrr::compact()

		modifySkyObject(module = "Student", objectName = "TechnologyAtHome", objectId = TechnologyAtHomeID, body = list(DataObject = body), searchFields = append("TechnologyAtHomeID", body %>% names()), entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' List DeviceAccesses
	#'
	#' This function returns a dataframe or json object of DeviceAccesses
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given DeviceAccesses. Defaults to FALSE for all return fields which, for convenience, returns all fields for the DeviceAccesses.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object listed as text. Run \code{\link{getSchemaForObjects}}('DeviceAccess') to get more field paths.
	#' @param searchConditionsList A list of search conditions to filter results which are joined by the searchConditionsGroupType. Of the form {FieldName} {ConditionType} {SearchCondition}. For example, c('StudentID LessEqual 500', 'LastName Like Ander\%'). Run \code{\link{listSearchConditionTypes}} for a list of ConditionTypes. Defaults to NULL (unfiltered).
	#' @param searchConditionsGroupType The conjunction which joins multiple searchConditions in the searchConditionsList. Either 'Or' or 'And'. Defaults to 'And'.
	#' @param searchSortFieldNamesList The list of fields sort results by. Defaults to NULL (unsorted).
	#' @param searchSortFieldNamesDescendingList A list of T/F values corresponding to whether to sort each field in searchSortFieldNamesList in descending order. Defaults to F for each FieldName in searchSortFieldNamesList.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param query Queries to be added to the request. Ex. list(SchoolYearID = 1). Defaults to NULL.
	#' @param page Results are paginated. The page of results to return. Default is 1.
	#' @param pageSize Results are paginated. The number of records per page to return. Default is 100,000 (essentially all records for most objects).
	#' @param flatten Whether to flatten results into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Student
	#' @return A list of DeviceAccesses
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	listDeviceAccesses <- function(searchConditionsList = NULL, DeviceAccessID = F, DistrictID = F, SchoolYearID = F, Code = F, StateDeviceAccessID = F, DeviceAccessIDClonedFrom = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, fieldPaths = NULL, searchConditionsGroupType = "And", searchSortFieldNamesList = NULL, searchSortFieldNamesDescendingList = NULL, entityId = 1, query = NULL, page = 1, pageSize = 100000, flatten = T, returnResponse = F){

		params <- as.list(environment())

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		listSkyObjects(module = "Student", objectName = "DeviceAccess", searchFields = searchFields %>% append(fieldPaths), page = page, pageSize = pageSize, SearchConditionsList = searchConditionsList, SearchConditionsGroupType = searchConditionsGroupType, SearchSortFieldNamesList = searchSortFieldNamesList, SearchSortFieldNamesDescendingList = searchSortFieldNamesDescendingList, entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' Get a DeviceAccess
	#'
	#' This function returns a dataframe or json object of a DeviceAccess
	#' @param DeviceAccessID The ID of the DeviceAccess to return.
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given DeviceAccess. Defaults to FALSE for all return fields which, for convenience, returns all fields for the DeviceAccess.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object listed as text. Run \code{\link{getSchemaForObjects}}('DeviceAccess') to get more field paths.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param query Queries to be added to the request. Ex. list(SchoolYearID = 1). Defaults to NULL.
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Student
	#' @return A dataframe or of DeviceAccess
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	getDeviceAccess <- function(DeviceAccessID, DistrictID = F, SchoolYearID = F, Code = F, StateDeviceAccessID = F, DeviceAccessIDClonedFrom = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, fieldPaths = NULL, entityId = 1, query = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment()) %>% purrr::keep(names(.) != "DeviceAccessID")

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		getSkyObject(module = "Student", objectName = "DeviceAccess", objectId = DeviceAccessID, searchFields = searchFields, entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' Delete a DeviceAccess
	#'
	#' This function deletes a DeviceAccess
	#' @param DeviceAccessID The ID of the DeviceAccess to delete
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param query Queries to be added to the request. Ex. list(SchoolYearID = 1). Defaults to NULL.
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Student
	#' @return The DeviceAccessID of the deleted DeviceAccess.
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	deleteDeviceAccess <- function(DeviceAccessID, ignoreWarnings = F, entityId = 1, query = NULL, flatten = T, returnResponse = F){

		deleteSkyObject(module = "Student", objectName = "DeviceAccess", objectId = DeviceAccessID, ignoreWarnings = ignoreWarnings, entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' Create a DeviceAccess
	#'
	#' This function creates a DeviceAccess
	#' @param fieldNames The field values to give the created DeviceAccess. Each defaults to NULL.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param query Queries to be added to the request. Ex. list(SchoolYearID = 1). Defaults to NULL.
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Student
	#' @return A newly created DeviceAccess
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	createDeviceAccess <- function(DistrictID = NULL, SchoolYearID = NULL, Code = NULL, StateDeviceAccessID = NULL, DeviceAccessIDClonedFrom = NULL, entityId = 1, query = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment())

		body <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper()) %>% purrr::compact()

		createSkyObject(module = "Student", objectName = "DeviceAccess", body = list(DataObject = body), searchFields = append("DeviceAccessID", body %>% names()), entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' Modify a DeviceAccess
	#'
	#' This function modifies a DeviceAccess
	#' @param fieldNames The field values to give the modified DeviceAccess. Each defaults to NULL.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param query Queries to be added to the request. Ex. list(SchoolYearID = 1). Defaults to NULL.
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Student
	#' @return The modified DeviceAccess
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	modifyDeviceAccess <- function(DeviceAccessID, DistrictID = NULL, SchoolYearID = NULL, Code = NULL, StateDeviceAccessID = NULL, DeviceAccessIDClonedFrom = NULL, entityId = 1, query = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment())

		body <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper()) %>% purrr::compact()

		modifySkyObject(module = "Student", objectName = "DeviceAccess", objectId = DeviceAccessID, body = list(DataObject = body), searchFields = append("DeviceAccessID", body %>% names()), entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' List DeviceAccessSources
	#'
	#' This function returns a dataframe or json object of DeviceAccessSources
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given DeviceAccessSources. Defaults to FALSE for all return fields which, for convenience, returns all fields for the DeviceAccessSources.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object listed as text. Run \code{\link{getSchemaForObjects}}('DeviceAccessSource') to get more field paths.
	#' @param searchConditionsList A list of search conditions to filter results which are joined by the searchConditionsGroupType. Of the form {FieldName} {ConditionType} {SearchCondition}. For example, c('StudentID LessEqual 500', 'LastName Like Ander\%'). Run \code{\link{listSearchConditionTypes}} for a list of ConditionTypes. Defaults to NULL (unfiltered).
	#' @param searchConditionsGroupType The conjunction which joins multiple searchConditions in the searchConditionsList. Either 'Or' or 'And'. Defaults to 'And'.
	#' @param searchSortFieldNamesList The list of fields sort results by. Defaults to NULL (unsorted).
	#' @param searchSortFieldNamesDescendingList A list of T/F values corresponding to whether to sort each field in searchSortFieldNamesList in descending order. Defaults to F for each FieldName in searchSortFieldNamesList.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param query Queries to be added to the request. Ex. list(SchoolYearID = 1). Defaults to NULL.
	#' @param page Results are paginated. The page of results to return. Default is 1.
	#' @param pageSize Results are paginated. The number of records per page to return. Default is 100,000 (essentially all records for most objects).
	#' @param flatten Whether to flatten results into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Student
	#' @return A list of DeviceAccessSources
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	listDeviceAccessSources <- function(searchConditionsList = NULL, DeviceAccessSourceID = F, DistrictID = F, SchoolYearID = F, Code = F, StateDeviceAccessSourceID = F, DeviceAccessSourceIDClonedFrom = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, fieldPaths = NULL, searchConditionsGroupType = "And", searchSortFieldNamesList = NULL, searchSortFieldNamesDescendingList = NULL, entityId = 1, query = NULL, page = 1, pageSize = 100000, flatten = T, returnResponse = F){

		params <- as.list(environment())

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		listSkyObjects(module = "Student", objectName = "DeviceAccessSource", searchFields = searchFields %>% append(fieldPaths), page = page, pageSize = pageSize, SearchConditionsList = searchConditionsList, SearchConditionsGroupType = searchConditionsGroupType, SearchSortFieldNamesList = searchSortFieldNamesList, SearchSortFieldNamesDescendingList = searchSortFieldNamesDescendingList, entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' Get a DeviceAccessSource
	#'
	#' This function returns a dataframe or json object of a DeviceAccessSource
	#' @param DeviceAccessSourceID The ID of the DeviceAccessSource to return.
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given DeviceAccessSource. Defaults to FALSE for all return fields which, for convenience, returns all fields for the DeviceAccessSource.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object listed as text. Run \code{\link{getSchemaForObjects}}('DeviceAccessSource') to get more field paths.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param query Queries to be added to the request. Ex. list(SchoolYearID = 1). Defaults to NULL.
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Student
	#' @return A dataframe or of DeviceAccessSource
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	getDeviceAccessSource <- function(DeviceAccessSourceID, DistrictID = F, SchoolYearID = F, Code = F, StateDeviceAccessSourceID = F, DeviceAccessSourceIDClonedFrom = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, fieldPaths = NULL, entityId = 1, query = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment()) %>% purrr::keep(names(.) != "DeviceAccessSourceID")

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		getSkyObject(module = "Student", objectName = "DeviceAccessSource", objectId = DeviceAccessSourceID, searchFields = searchFields, entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' Delete a DeviceAccessSource
	#'
	#' This function deletes a DeviceAccessSource
	#' @param DeviceAccessSourceID The ID of the DeviceAccessSource to delete
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param query Queries to be added to the request. Ex. list(SchoolYearID = 1). Defaults to NULL.
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Student
	#' @return The DeviceAccessSourceID of the deleted DeviceAccessSource.
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	deleteDeviceAccessSource <- function(DeviceAccessSourceID, ignoreWarnings = F, entityId = 1, query = NULL, flatten = T, returnResponse = F){

		deleteSkyObject(module = "Student", objectName = "DeviceAccessSource", objectId = DeviceAccessSourceID, ignoreWarnings = ignoreWarnings, entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' Create a DeviceAccessSource
	#'
	#' This function creates a DeviceAccessSource
	#' @param fieldNames The field values to give the created DeviceAccessSource. Each defaults to NULL.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param query Queries to be added to the request. Ex. list(SchoolYearID = 1). Defaults to NULL.
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Student
	#' @return A newly created DeviceAccessSource
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	createDeviceAccessSource <- function(DistrictID = NULL, SchoolYearID = NULL, Code = NULL, StateDeviceAccessSourceID = NULL, DeviceAccessSourceIDClonedFrom = NULL, entityId = 1, query = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment())

		body <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper()) %>% purrr::compact()

		createSkyObject(module = "Student", objectName = "DeviceAccessSource", body = list(DataObject = body), searchFields = append("DeviceAccessSourceID", body %>% names()), entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' Modify a DeviceAccessSource
	#'
	#' This function modifies a DeviceAccessSource
	#' @param fieldNames The field values to give the modified DeviceAccessSource. Each defaults to NULL.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param query Queries to be added to the request. Ex. list(SchoolYearID = 1). Defaults to NULL.
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Student
	#' @return The modified DeviceAccessSource
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	modifyDeviceAccessSource <- function(DeviceAccessSourceID, DistrictID = NULL, SchoolYearID = NULL, Code = NULL, StateDeviceAccessSourceID = NULL, DeviceAccessSourceIDClonedFrom = NULL, entityId = 1, query = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment())

		body <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper()) %>% purrr::compact()

		modifySkyObject(module = "Student", objectName = "DeviceAccessSource", objectId = DeviceAccessSourceID, body = list(DataObject = body), searchFields = append("DeviceAccessSourceID", body %>% names()), entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' List Devices
	#'
	#' This function returns a dataframe or json object of Devices
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given Devices. Defaults to FALSE for all return fields which, for convenience, returns all fields for the Devices.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object listed as text. Run \code{\link{getSchemaForObjects}}('Device') to get more field paths.
	#' @param searchConditionsList A list of search conditions to filter results which are joined by the searchConditionsGroupType. Of the form {FieldName} {ConditionType} {SearchCondition}. For example, c('StudentID LessEqual 500', 'LastName Like Ander\%'). Run \code{\link{listSearchConditionTypes}} for a list of ConditionTypes. Defaults to NULL (unfiltered).
	#' @param searchConditionsGroupType The conjunction which joins multiple searchConditions in the searchConditionsList. Either 'Or' or 'And'. Defaults to 'And'.
	#' @param searchSortFieldNamesList The list of fields sort results by. Defaults to NULL (unsorted).
	#' @param searchSortFieldNamesDescendingList A list of T/F values corresponding to whether to sort each field in searchSortFieldNamesList in descending order. Defaults to F for each FieldName in searchSortFieldNamesList.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param query Queries to be added to the request. Ex. list(SchoolYearID = 1). Defaults to NULL.
	#' @param page Results are paginated. The page of results to return. Default is 1.
	#' @param pageSize Results are paginated. The number of records per page to return. Default is 100,000 (essentially all records for most objects).
	#' @param flatten Whether to flatten results into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Student
	#' @return A list of Devices
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	listDevices <- function(searchConditionsList = NULL, DeviceID = F, DistrictID = F, SchoolYearID = F, Code = F, StateDeviceID = F, DeviceIDClonedFrom = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, fieldPaths = NULL, searchConditionsGroupType = "And", searchSortFieldNamesList = NULL, searchSortFieldNamesDescendingList = NULL, entityId = 1, query = NULL, page = 1, pageSize = 100000, flatten = T, returnResponse = F){

		params <- as.list(environment())

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		listSkyObjects(module = "Student", objectName = "Device", searchFields = searchFields %>% append(fieldPaths), page = page, pageSize = pageSize, SearchConditionsList = searchConditionsList, SearchConditionsGroupType = searchConditionsGroupType, SearchSortFieldNamesList = searchSortFieldNamesList, SearchSortFieldNamesDescendingList = searchSortFieldNamesDescendingList, entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' Get a Device
	#'
	#' This function returns a dataframe or json object of a Device
	#' @param DeviceID The ID of the Device to return.
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given Device. Defaults to FALSE for all return fields which, for convenience, returns all fields for the Device.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object listed as text. Run \code{\link{getSchemaForObjects}}('Device') to get more field paths.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param query Queries to be added to the request. Ex. list(SchoolYearID = 1). Defaults to NULL.
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Student
	#' @return A dataframe or of Device
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	getDevice <- function(DeviceID, DistrictID = F, SchoolYearID = F, Code = F, StateDeviceID = F, DeviceIDClonedFrom = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, fieldPaths = NULL, entityId = 1, query = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment()) %>% purrr::keep(names(.) != "DeviceID")

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		getSkyObject(module = "Student", objectName = "Device", objectId = DeviceID, searchFields = searchFields, entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' Delete a Device
	#'
	#' This function deletes a Device
	#' @param DeviceID The ID of the Device to delete
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param query Queries to be added to the request. Ex. list(SchoolYearID = 1). Defaults to NULL.
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Student
	#' @return The DeviceID of the deleted Device.
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	deleteDevice <- function(DeviceID, ignoreWarnings = F, entityId = 1, query = NULL, flatten = T, returnResponse = F){

		deleteSkyObject(module = "Student", objectName = "Device", objectId = DeviceID, ignoreWarnings = ignoreWarnings, entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' Create a Device
	#'
	#' This function creates a Device
	#' @param fieldNames The field values to give the created Device. Each defaults to NULL.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param query Queries to be added to the request. Ex. list(SchoolYearID = 1). Defaults to NULL.
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Student
	#' @return A newly created Device
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	createDevice <- function(DistrictID = NULL, SchoolYearID = NULL, Code = NULL, StateDeviceID = NULL, DeviceIDClonedFrom = NULL, entityId = 1, query = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment())

		body <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper()) %>% purrr::compact()

		createSkyObject(module = "Student", objectName = "Device", body = list(DataObject = body), searchFields = append("DeviceID", body %>% names()), entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' Modify a Device
	#'
	#' This function modifies a Device
	#' @param fieldNames The field values to give the modified Device. Each defaults to NULL.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param query Queries to be added to the request. Ex. list(SchoolYearID = 1). Defaults to NULL.
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Student
	#' @return The modified Device
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	modifyDevice <- function(DeviceID, DistrictID = NULL, SchoolYearID = NULL, Code = NULL, StateDeviceID = NULL, DeviceIDClonedFrom = NULL, entityId = 1, query = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment())

		body <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper()) %>% purrr::compact()

		modifySkyObject(module = "Student", objectName = "Device", objectId = DeviceID, body = list(DataObject = body), searchFields = append("DeviceID", body %>% names()), entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' List InternetAccesses
	#'
	#' This function returns a dataframe or json object of InternetAccesses
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given InternetAccesses. Defaults to FALSE for all return fields which, for convenience, returns all fields for the InternetAccesses.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object listed as text. Run \code{\link{getSchemaForObjects}}('InternetAccess') to get more field paths.
	#' @param searchConditionsList A list of search conditions to filter results which are joined by the searchConditionsGroupType. Of the form {FieldName} {ConditionType} {SearchCondition}. For example, c('StudentID LessEqual 500', 'LastName Like Ander\%'). Run \code{\link{listSearchConditionTypes}} for a list of ConditionTypes. Defaults to NULL (unfiltered).
	#' @param searchConditionsGroupType The conjunction which joins multiple searchConditions in the searchConditionsList. Either 'Or' or 'And'. Defaults to 'And'.
	#' @param searchSortFieldNamesList The list of fields sort results by. Defaults to NULL (unsorted).
	#' @param searchSortFieldNamesDescendingList A list of T/F values corresponding to whether to sort each field in searchSortFieldNamesList in descending order. Defaults to F for each FieldName in searchSortFieldNamesList.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param query Queries to be added to the request. Ex. list(SchoolYearID = 1). Defaults to NULL.
	#' @param page Results are paginated. The page of results to return. Default is 1.
	#' @param pageSize Results are paginated. The number of records per page to return. Default is 100,000 (essentially all records for most objects).
	#' @param flatten Whether to flatten results into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Student
	#' @return A list of InternetAccesses
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	listInternetAccesses <- function(searchConditionsList = NULL, InternetAccessID = F, DistrictID = F, SchoolYearID = F, Code = F, StateInternetAccessID = F, InternetAccessIDClonedFrom = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, fieldPaths = NULL, searchConditionsGroupType = "And", searchSortFieldNamesList = NULL, searchSortFieldNamesDescendingList = NULL, entityId = 1, query = NULL, page = 1, pageSize = 100000, flatten = T, returnResponse = F){

		params <- as.list(environment())

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		listSkyObjects(module = "Student", objectName = "InternetAccess", searchFields = searchFields %>% append(fieldPaths), page = page, pageSize = pageSize, SearchConditionsList = searchConditionsList, SearchConditionsGroupType = searchConditionsGroupType, SearchSortFieldNamesList = searchSortFieldNamesList, SearchSortFieldNamesDescendingList = searchSortFieldNamesDescendingList, entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' Get an InternetAccess
	#'
	#' This function returns a dataframe or json object of an InternetAccess
	#' @param InternetAccessID The ID of the InternetAccess to return.
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given InternetAccess. Defaults to FALSE for all return fields which, for convenience, returns all fields for the InternetAccess.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object listed as text. Run \code{\link{getSchemaForObjects}}('InternetAccess') to get more field paths.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param query Queries to be added to the request. Ex. list(SchoolYearID = 1). Defaults to NULL.
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Student
	#' @return A dataframe or of InternetAccess
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	getInternetAccess <- function(InternetAccessID, DistrictID = F, SchoolYearID = F, Code = F, StateInternetAccessID = F, InternetAccessIDClonedFrom = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, fieldPaths = NULL, entityId = 1, query = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment()) %>% purrr::keep(names(.) != "InternetAccessID")

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		getSkyObject(module = "Student", objectName = "InternetAccess", objectId = InternetAccessID, searchFields = searchFields, entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' Delete an InternetAccess
	#'
	#' This function deletes an InternetAccess
	#' @param InternetAccessID The ID of the InternetAccess to delete
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param query Queries to be added to the request. Ex. list(SchoolYearID = 1). Defaults to NULL.
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Student
	#' @return The InternetAccessID of the deleted InternetAccess.
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	deleteInternetAccess <- function(InternetAccessID, ignoreWarnings = F, entityId = 1, query = NULL, flatten = T, returnResponse = F){

		deleteSkyObject(module = "Student", objectName = "InternetAccess", objectId = InternetAccessID, ignoreWarnings = ignoreWarnings, entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' Create an InternetAccess
	#'
	#' This function creates an InternetAccess
	#' @param fieldNames The field values to give the created InternetAccess. Each defaults to NULL.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param query Queries to be added to the request. Ex. list(SchoolYearID = 1). Defaults to NULL.
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Student
	#' @return A newly created InternetAccess
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	createInternetAccess <- function(DistrictID = NULL, SchoolYearID = NULL, Code = NULL, StateInternetAccessID = NULL, InternetAccessIDClonedFrom = NULL, entityId = 1, query = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment())

		body <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper()) %>% purrr::compact()

		createSkyObject(module = "Student", objectName = "InternetAccess", body = list(DataObject = body), searchFields = append("InternetAccessID", body %>% names()), entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' Modify an InternetAccess
	#'
	#' This function modifies an InternetAccess
	#' @param fieldNames The field values to give the modified InternetAccess. Each defaults to NULL.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param query Queries to be added to the request. Ex. list(SchoolYearID = 1). Defaults to NULL.
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Student
	#' @return The modified InternetAccess
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	modifyInternetAccess <- function(InternetAccessID, DistrictID = NULL, SchoolYearID = NULL, Code = NULL, StateInternetAccessID = NULL, InternetAccessIDClonedFrom = NULL, entityId = 1, query = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment())

		body <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper()) %>% purrr::compact()

		modifySkyObject(module = "Student", objectName = "InternetAccess", objectId = InternetAccessID, body = list(DataObject = body), searchFields = append("InternetAccessID", body %>% names()), entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' List InternetAccessTypes
	#'
	#' This function returns a dataframe or json object of InternetAccessTypes
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given InternetAccessTypes. Defaults to FALSE for all return fields which, for convenience, returns all fields for the InternetAccessTypes.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object listed as text. Run \code{\link{getSchemaForObjects}}('InternetAccessType') to get more field paths.
	#' @param searchConditionsList A list of search conditions to filter results which are joined by the searchConditionsGroupType. Of the form {FieldName} {ConditionType} {SearchCondition}. For example, c('StudentID LessEqual 500', 'LastName Like Ander\%'). Run \code{\link{listSearchConditionTypes}} for a list of ConditionTypes. Defaults to NULL (unfiltered).
	#' @param searchConditionsGroupType The conjunction which joins multiple searchConditions in the searchConditionsList. Either 'Or' or 'And'. Defaults to 'And'.
	#' @param searchSortFieldNamesList The list of fields sort results by. Defaults to NULL (unsorted).
	#' @param searchSortFieldNamesDescendingList A list of T/F values corresponding to whether to sort each field in searchSortFieldNamesList in descending order. Defaults to F for each FieldName in searchSortFieldNamesList.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param query Queries to be added to the request. Ex. list(SchoolYearID = 1). Defaults to NULL.
	#' @param page Results are paginated. The page of results to return. Default is 1.
	#' @param pageSize Results are paginated. The number of records per page to return. Default is 100,000 (essentially all records for most objects).
	#' @param flatten Whether to flatten results into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Student
	#' @return A list of InternetAccessTypes
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	listInternetAccessTypes <- function(searchConditionsList = NULL, InternetAccessTypeID = F, DistrictID = F, SchoolYearID = F, Code = F, StateInternetAccessTypeID = F, InternetAccessTypeIDClonedFrom = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, fieldPaths = NULL, searchConditionsGroupType = "And", searchSortFieldNamesList = NULL, searchSortFieldNamesDescendingList = NULL, entityId = 1, query = NULL, page = 1, pageSize = 100000, flatten = T, returnResponse = F){

		params <- as.list(environment())

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		listSkyObjects(module = "Student", objectName = "InternetAccessType", searchFields = searchFields %>% append(fieldPaths), page = page, pageSize = pageSize, SearchConditionsList = searchConditionsList, SearchConditionsGroupType = searchConditionsGroupType, SearchSortFieldNamesList = searchSortFieldNamesList, SearchSortFieldNamesDescendingList = searchSortFieldNamesDescendingList, entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' Get an InternetAccessType
	#'
	#' This function returns a dataframe or json object of an InternetAccessType
	#' @param InternetAccessTypeID The ID of the InternetAccessType to return.
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given InternetAccessType. Defaults to FALSE for all return fields which, for convenience, returns all fields for the InternetAccessType.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object listed as text. Run \code{\link{getSchemaForObjects}}('InternetAccessType') to get more field paths.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param query Queries to be added to the request. Ex. list(SchoolYearID = 1). Defaults to NULL.
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Student
	#' @return A dataframe or of InternetAccessType
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	getInternetAccessType <- function(InternetAccessTypeID, DistrictID = F, SchoolYearID = F, Code = F, StateInternetAccessTypeID = F, InternetAccessTypeIDClonedFrom = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, fieldPaths = NULL, entityId = 1, query = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment()) %>% purrr::keep(names(.) != "InternetAccessTypeID")

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		getSkyObject(module = "Student", objectName = "InternetAccessType", objectId = InternetAccessTypeID, searchFields = searchFields, entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' Delete an InternetAccessType
	#'
	#' This function deletes an InternetAccessType
	#' @param InternetAccessTypeID The ID of the InternetAccessType to delete
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param query Queries to be added to the request. Ex. list(SchoolYearID = 1). Defaults to NULL.
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Student
	#' @return The InternetAccessTypeID of the deleted InternetAccessType.
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	deleteInternetAccessType <- function(InternetAccessTypeID, ignoreWarnings = F, entityId = 1, query = NULL, flatten = T, returnResponse = F){

		deleteSkyObject(module = "Student", objectName = "InternetAccessType", objectId = InternetAccessTypeID, ignoreWarnings = ignoreWarnings, entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' Create an InternetAccessType
	#'
	#' This function creates an InternetAccessType
	#' @param fieldNames The field values to give the created InternetAccessType. Each defaults to NULL.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param query Queries to be added to the request. Ex. list(SchoolYearID = 1). Defaults to NULL.
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Student
	#' @return A newly created InternetAccessType
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	createInternetAccessType <- function(DistrictID = NULL, SchoolYearID = NULL, Code = NULL, StateInternetAccessTypeID = NULL, InternetAccessTypeIDClonedFrom = NULL, entityId = 1, query = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment())

		body <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper()) %>% purrr::compact()

		createSkyObject(module = "Student", objectName = "InternetAccessType", body = list(DataObject = body), searchFields = append("InternetAccessTypeID", body %>% names()), entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' Modify an InternetAccessType
	#'
	#' This function modifies an InternetAccessType
	#' @param fieldNames The field values to give the modified InternetAccessType. Each defaults to NULL.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param query Queries to be added to the request. Ex. list(SchoolYearID = 1). Defaults to NULL.
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Student
	#' @return The modified InternetAccessType
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	modifyInternetAccessType <- function(InternetAccessTypeID, DistrictID = NULL, SchoolYearID = NULL, Code = NULL, StateInternetAccessTypeID = NULL, InternetAccessTypeIDClonedFrom = NULL, entityId = 1, query = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment())

		body <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper()) %>% purrr::compact()

		modifySkyObject(module = "Student", objectName = "InternetAccessType", objectId = InternetAccessTypeID, body = list(DataObject = body), searchFields = append("InternetAccessTypeID", body %>% names()), entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' List InternetPerformances
	#'
	#' This function returns a dataframe or json object of InternetPerformances
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given InternetPerformances. Defaults to FALSE for all return fields which, for convenience, returns all fields for the InternetPerformances.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object listed as text. Run \code{\link{getSchemaForObjects}}('InternetPerformance') to get more field paths.
	#' @param searchConditionsList A list of search conditions to filter results which are joined by the searchConditionsGroupType. Of the form {FieldName} {ConditionType} {SearchCondition}. For example, c('StudentID LessEqual 500', 'LastName Like Ander\%'). Run \code{\link{listSearchConditionTypes}} for a list of ConditionTypes. Defaults to NULL (unfiltered).
	#' @param searchConditionsGroupType The conjunction which joins multiple searchConditions in the searchConditionsList. Either 'Or' or 'And'. Defaults to 'And'.
	#' @param searchSortFieldNamesList The list of fields sort results by. Defaults to NULL (unsorted).
	#' @param searchSortFieldNamesDescendingList A list of T/F values corresponding to whether to sort each field in searchSortFieldNamesList in descending order. Defaults to F for each FieldName in searchSortFieldNamesList.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param query Queries to be added to the request. Ex. list(SchoolYearID = 1). Defaults to NULL.
	#' @param page Results are paginated. The page of results to return. Default is 1.
	#' @param pageSize Results are paginated. The number of records per page to return. Default is 100,000 (essentially all records for most objects).
	#' @param flatten Whether to flatten results into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Student
	#' @return A list of InternetPerformances
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	listInternetPerformances <- function(searchConditionsList = NULL, InternetPerformanceID = F, DistrictID = F, SchoolYearID = F, Code = F, StateInternetPerformanceID = F, InternetPerformanceIDClonedFrom = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, fieldPaths = NULL, searchConditionsGroupType = "And", searchSortFieldNamesList = NULL, searchSortFieldNamesDescendingList = NULL, entityId = 1, query = NULL, page = 1, pageSize = 100000, flatten = T, returnResponse = F){

		params <- as.list(environment())

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		listSkyObjects(module = "Student", objectName = "InternetPerformance", searchFields = searchFields %>% append(fieldPaths), page = page, pageSize = pageSize, SearchConditionsList = searchConditionsList, SearchConditionsGroupType = searchConditionsGroupType, SearchSortFieldNamesList = searchSortFieldNamesList, SearchSortFieldNamesDescendingList = searchSortFieldNamesDescendingList, entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' Get an InternetPerformance
	#'
	#' This function returns a dataframe or json object of an InternetPerformance
	#' @param InternetPerformanceID The ID of the InternetPerformance to return.
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given InternetPerformance. Defaults to FALSE for all return fields which, for convenience, returns all fields for the InternetPerformance.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object listed as text. Run \code{\link{getSchemaForObjects}}('InternetPerformance') to get more field paths.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param query Queries to be added to the request. Ex. list(SchoolYearID = 1). Defaults to NULL.
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Student
	#' @return A dataframe or of InternetPerformance
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	getInternetPerformance <- function(InternetPerformanceID, DistrictID = F, SchoolYearID = F, Code = F, StateInternetPerformanceID = F, InternetPerformanceIDClonedFrom = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, fieldPaths = NULL, entityId = 1, query = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment()) %>% purrr::keep(names(.) != "InternetPerformanceID")

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		getSkyObject(module = "Student", objectName = "InternetPerformance", objectId = InternetPerformanceID, searchFields = searchFields, entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' Delete an InternetPerformance
	#'
	#' This function deletes an InternetPerformance
	#' @param InternetPerformanceID The ID of the InternetPerformance to delete
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param query Queries to be added to the request. Ex. list(SchoolYearID = 1). Defaults to NULL.
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Student
	#' @return The InternetPerformanceID of the deleted InternetPerformance.
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	deleteInternetPerformance <- function(InternetPerformanceID, ignoreWarnings = F, entityId = 1, query = NULL, flatten = T, returnResponse = F){

		deleteSkyObject(module = "Student", objectName = "InternetPerformance", objectId = InternetPerformanceID, ignoreWarnings = ignoreWarnings, entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' Create an InternetPerformance
	#'
	#' This function creates an InternetPerformance
	#' @param fieldNames The field values to give the created InternetPerformance. Each defaults to NULL.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param query Queries to be added to the request. Ex. list(SchoolYearID = 1). Defaults to NULL.
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Student
	#' @return A newly created InternetPerformance
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	createInternetPerformance <- function(DistrictID = NULL, SchoolYearID = NULL, Code = NULL, StateInternetPerformanceID = NULL, InternetPerformanceIDClonedFrom = NULL, entityId = 1, query = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment())

		body <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper()) %>% purrr::compact()

		createSkyObject(module = "Student", objectName = "InternetPerformance", body = list(DataObject = body), searchFields = append("InternetPerformanceID", body %>% names()), entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' Modify an InternetPerformance
	#'
	#' This function modifies an InternetPerformance
	#' @param fieldNames The field values to give the modified InternetPerformance. Each defaults to NULL.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param query Queries to be added to the request. Ex. list(SchoolYearID = 1). Defaults to NULL.
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Student
	#' @return The modified InternetPerformance
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	modifyInternetPerformance <- function(InternetPerformanceID, DistrictID = NULL, SchoolYearID = NULL, Code = NULL, StateInternetPerformanceID = NULL, InternetPerformanceIDClonedFrom = NULL, entityId = 1, query = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment())

		body <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper()) %>% purrr::compact()

		modifySkyObject(module = "Student", objectName = "InternetPerformance", objectId = InternetPerformanceID, body = list(DataObject = body), searchFields = append("InternetPerformanceID", body %>% names()), entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}
