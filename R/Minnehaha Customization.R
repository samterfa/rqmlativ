

	listResponseTables <- function(searchConditionsList = NULL, ResponseTableID = F, Code = F, Description = F, searchConditionsGroupType = "And", searchSortFieldNamesList = NULL, searchSortFieldNamesDescendingList = NULL, entityID = 1, schoolYearID = NULL, page = 1, pageSize = 100000, flatten = T, ...){

		params <- as.list(environment()) %>% append(list(...))

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- "all", searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		listSkyObjects(module = "MinnehahaCustomization", objectName = "ResponseTable", schoolYearID = schoolYearID, searchFields = searchFields, page = page, pageSize = pageSize, SearchConditionsList = searchConditionsList, SearchConditionsGroupType = searchConditionsGroupType, SearchSortFieldNamesList = searchSortFieldNamesList, SearchSortFieldNamesDescendingList = searchSortFieldNamesDescendingList, entityId = entityID, flatten = flatten)
	}


	listHealthConditionResponses <- function(searchConditionsList = NULL, HealthConditionResponsesID = F, SevereLifeThreateningAllergicReactionID = F, EpiPenNeededatSchoolID = F, AllergicTo = F, OtherTreatments = F, ADHDADDID = F, MedicationatHomeID = F, MedicationandDosage = F, AsthmaID = F, AsthmaMDID = F, InhaleratSchoolID = F, NubulizeratSchool = F, InhalerforSportsID = F, BladderBowelProblemsID = F, BladderProblemandTreatment = F, DiabetesID = F, DiabetesNotesandInstructions = F, HeartProblemsID = F, HeartProblemandTreatment = F, SeizuresID = F, SeizureProblemandTreatment = F, DateofLastSeizure = F, LearningHealthConcernsID = F, LearningHealthConcernsandTreatment = F, VisionorHearingProblemsID = F, GlassesNecessaryID = F, HearingAidsNecessaryID = F, VisionorHearingProblemorAccomodation = F, ActivityRestrictionsID = F, Restrictions = F, RecentSignificantHealthProblemsID = F, DateofOccurrence = F, DescribeProblemandTreatment = F, DailyMedicationsID = F, Medication1Name = F, Medication1Dose = F, Medication1Time = F, Medication1Purpose = F, Medication2Name = F, Medication2Dose = F, Medication2Time = F, Medication2Purpose = F, Medication3Name = F, Medication3Dose = F, Medication3Time = F, Medication3Purpose = F, Medication4Name = F, Medication4Dose = F, Medication4Time = F, OtherHealthProblemsID = F, OtherHeathProblemsandTreatments = F, StudentID = F, MedicationatSchoolID = F, RecentSignificantHealthCondition = F, Medication4Purpose = F, searchConditionsGroupType = "And", searchSortFieldNamesList = NULL, searchSortFieldNamesDescendingList = NULL, entityID = 1, schoolYearID = NULL, page = 1, pageSize = 100000, flatten = T, ...){

		params <- as.list(environment()) %>% append(list(...))

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- "all", searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		listSkyObjects(module = "MinnehahaCustomization", objectName = "HealthConditionResponses", schoolYearID = schoolYearID, searchFields = searchFields, page = page, pageSize = pageSize, SearchConditionsList = searchConditionsList, SearchConditionsGroupType = searchConditionsGroupType, SearchSortFieldNamesList = searchSortFieldNamesList, SearchSortFieldNamesDescendingList = searchSortFieldNamesDescendingList, entityId = entityID, flatten = flatten)
	}


	listOTCMedications <- function(searchConditionsList = NULL, OTCMedicationID = F, IpubrofenorAdvilID = F, CoughDropsID = F, AntiItchCremeID = F, SelfCarryIbuprofenID = F, StudentID = F, SubTylenolForIbuprofenID = F, searchConditionsGroupType = "And", searchSortFieldNamesList = NULL, searchSortFieldNamesDescendingList = NULL, entityID = 1, schoolYearID = NULL, page = 1, pageSize = 100000, flatten = T, ...){

		params <- as.list(environment()) %>% append(list(...))

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- "all", searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		listSkyObjects(module = "MinnehahaCustomization", objectName = "OTCMedication", schoolYearID = schoolYearID, searchFields = searchFields, page = page, pageSize = pageSize, SearchConditionsList = searchConditionsList, SearchConditionsGroupType = searchConditionsGroupType, SearchSortFieldNamesList = searchSortFieldNamesList, SearchSortFieldNamesDescendingList = searchSortFieldNamesDescendingList, entityId = entityID, flatten = flatten)
	}


	listGrandPersonMAS <- function(searchConditionsList = NULL, GrandPersonMAID = F, FamilyID = F, NameID = F, RelationshipID = F, searchConditionsGroupType = "And", searchSortFieldNamesList = NULL, searchSortFieldNamesDescendingList = NULL, entityID = 1, schoolYearID = NULL, page = 1, pageSize = 100000, flatten = T, ...){

		params <- as.list(environment()) %>% append(list(...))

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- "all", searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		listSkyObjects(module = "MinnehahaCustomization", objectName = "GrandPersonMA", schoolYearID = schoolYearID, searchFields = searchFields, page = page, pageSize = pageSize, SearchConditionsList = searchConditionsList, SearchConditionsGroupType = searchConditionsGroupType, SearchSortFieldNamesList = searchSortFieldNamesList, SearchSortFieldNamesDescendingList = searchSortFieldNamesDescendingList, entityId = entityID, flatten = flatten)
	}


	listParentalConsents <- function(searchConditionsList = NULL, ParentalConsentID = F, InstructionalMaterialsEducationalAids = F, HealthServicesEducationalAids = F, CounselingServicesEducationalAids = F, FamilyID = F, COPPAConsent = F, ContactInfoInDirectory = F, WalkingFieldTrip = F, TechnologyAgreement = F, TechnologyAgreement2 = F, searchConditionsGroupType = "And", searchSortFieldNamesList = NULL, searchSortFieldNamesDescendingList = NULL, entityID = 1, schoolYearID = NULL, page = 1, pageSize = 100000, flatten = T, ...){

		params <- as.list(environment()) %>% append(list(...))

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- "all", searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		listSkyObjects(module = "MinnehahaCustomization", objectName = "ParentalConsent", schoolYearID = schoolYearID, searchFields = searchFields, page = page, pageSize = pageSize, SearchConditionsList = searchConditionsList, SearchConditionsGroupType = searchConditionsGroupType, SearchSortFieldNamesList = searchSortFieldNamesList, SearchSortFieldNamesDescendingList = searchSortFieldNamesDescendingList, entityId = entityID, flatten = flatten)
	}


	listPreschools <- function(searchConditionsList = NULL, PreschoolID = F, EmergencyContact1Address = F, EmergencyContact2Address = F, ActInAnEmergency = F, EarlyChildhoodScreening = F, FoodPolicy = F, ProvidedLotions = F, Toileting = F, StudentID = F, searchConditionsGroupType = "And", searchSortFieldNamesList = NULL, searchSortFieldNamesDescendingList = NULL, entityID = 1, schoolYearID = NULL, page = 1, pageSize = 100000, flatten = T, ...){

		params <- as.list(environment()) %>% append(list(...))

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- "all", searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		listSkyObjects(module = "MinnehahaCustomization", objectName = "Preschool", schoolYearID = schoolYearID, searchFields = searchFields, page = page, pageSize = pageSize, SearchConditionsList = searchConditionsList, SearchConditionsGroupType = searchConditionsGroupType, SearchSortFieldNamesList = searchSortFieldNamesList, SearchSortFieldNamesDescendingList = searchSortFieldNamesDescendingList, entityId = entityID, flatten = flatten)
	}
