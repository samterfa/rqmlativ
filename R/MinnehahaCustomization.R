
	#' List ResponseTables
	#'
	#' This function returns a dataframe or json object of ResponseTables
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given ResponseTables. Defaults to FALSE for all return fields which, for convenience, returns all fields for the ResponseTables.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object listed as text. Run \code{\link{getSchemaForObjects}}('ResponseTable') to get more field paths.
	#' @param searchConditionsList A list of search conditions to filter results which are joined by the searchConditionsGroupType. Of the form {FieldName} {ConditionType} {SearchCondition}. For example, c('StudentID LessEqual 500', 'LastName Like Ander\%'). Run \code{\link{listSearchConditionTypes}} for a list of ConditionTypes. Defaults to NULL (unfiltered).
	#' @param searchConditionsGroupType The conjunction which joins multiple searchConditions in the searchConditionsList. Either 'Or' or 'And'. Defaults to 'And'.
	#' @param searchSortFieldNamesList The list of fields sort results by. Defaults to NULL (unsorted).
	#' @param searchSortFieldNamesDescendingList A list of T/F values corresponding to whether to sort each field in searchSortFieldNamesList in descending order. Defaults to F for each FieldName in searchSortFieldNamesList.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{listSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param page Results are paginated. The page of results to return. Default is 1.
	#' @param pageSize Results are paginated. The number of records per page to return. Default is 100,000 (essentially all records for most objects).
	#' @param flatten Whether to flatten results into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Minnehaha Customization
	#' @return A list of ResponseTables
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	listResponseTables <- function(searchConditionsList = NULL, ResponseTableID = F, Code = F, Description = F, fieldPaths = NULL, searchConditionsGroupType = "And", searchSortFieldNamesList = NULL, searchSortFieldNamesDescendingList = NULL, entityId = 1, schoolYearId = NULL, page = 1, pageSize = 100000, flatten = T, returnResponse = F){

		params <- as.list(environment())

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		listSkyObjects(module = "MinnehahaCustomization", objectName = "ResponseTable", searchFields = searchFields %>% append(fieldPaths), page = page, pageSize = pageSize, SearchConditionsList = searchConditionsList, SearchConditionsGroupType = searchConditionsGroupType, SearchSortFieldNamesList = searchSortFieldNamesList, SearchSortFieldNamesDescendingList = searchSortFieldNamesDescendingList, entityId = entityId, schoolYearId = schoolYearId, flatten = flatten, returnResponse = returnResponse)
	}

	#' Get a ResponseTable
	#'
	#' This function returns a dataframe or json object of a ResponseTable
	#' @param ResponseTableID The ID of the ResponseTable to return.
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given ResponseTable. Defaults to FALSE for all return fields which, for convenience, returns all fields for the ResponseTable.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object listed as text. Run \code{\link{getSchemaForObjects}}('ResponseTable') to get more field paths.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{listSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Minnehaha Customization
	#' @return A dataframe or of ResponseTable
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	getResponseTable <- function(ResponseTableID, Code = F, Description = F, fieldPaths = NULL, entityId = 1, schoolYearId = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment()) %>% purrr::keep(names(.) != "ResponseTableID")

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		getSkyObject(module = "MinnehahaCustomization", objectName = "ResponseTable", objectId = ResponseTableID, searchFields = searchFields, entityId = entityId, flatten = flatten, returnResponse = returnResponse)
	}

	#' Delete a ResponseTable
	#'
	#' This function deletes a ResponseTable
	#' @param ResponseTableID The ID of the ResponseTable to delete
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{listSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Minnehaha Customization
	#' @return The ResponseTableID of the deleted ResponseTable.
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	deleteResponseTable <- function(ResponseTableID, ignoreWarnings = F, entityId = 1, schoolYearId = NULL, flatten = T, returnResponse = F){

		deleteSkyObject(module = "MinnehahaCustomization", objectName = "ResponseTable", objectId = ResponseTableID, ignoreWarnings = ignoreWarnings, entityId = entityId, flatten = flatten, returnResponse = returnResponse)
	}

	#' Create a ResponseTable
	#'
	#' This function creates a ResponseTable
	#' @param fieldNames The field values to give the created ResponseTable. Each defaults to NULL.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{listSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Minnehaha Customization
	#' @return A newly created ResponseTable
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	createResponseTable <- function(Code = NULL, Description = NULL, entityId = 1, schoolYearId = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment())

		body <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper()) %>% purrr::compact()

		createSkyObject(module = "MinnehahaCustomization", objectName = "ResponseTable", body = list(DataObject = body), searchFields = append("ResponseTableID", body %>% names()), entityId = entityId, schoolYearId = schoolYearId, flatten = flatten, returnResponse = returnResponse)
	}

	#' Modify a ResponseTable
	#'
	#' This function modifies a ResponseTable
	#' @param fieldNames The field values to give the modified ResponseTable. Each defaults to NULL.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{listSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Minnehaha Customization
	#' @return The modified ResponseTable
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	modifyResponseTable <- function(ResponseTableID, Code = NULL, Description = NULL, entityId = 1, schoolYearId = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment())

		body <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper()) %>% purrr::compact()

		modifySkyObject(module = "MinnehahaCustomization", objectName = "ResponseTable", objectId = ResponseTableID, body = list(DataObject = body), searchFields = append("ResponseTableID", body %>% names()), entityId = entityId, schoolYearId = schoolYearId, flatten = flatten, returnResponse = returnResponse)
	}

	#' List HealthConditionResponses
	#'
	#' This function returns a dataframe or json object of HealthConditionResponses
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given HealthConditionResponses. Defaults to FALSE for all return fields which, for convenience, returns all fields for the HealthConditionResponses.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object listed as text. Run \code{\link{getSchemaForObjects}}('HealthConditionResponses') to get more field paths.
	#' @param searchConditionsList A list of search conditions to filter results which are joined by the searchConditionsGroupType. Of the form {FieldName} {ConditionType} {SearchCondition}. For example, c('StudentID LessEqual 500', 'LastName Like Ander\%'). Run \code{\link{listSearchConditionTypes}} for a list of ConditionTypes. Defaults to NULL (unfiltered).
	#' @param searchConditionsGroupType The conjunction which joins multiple searchConditions in the searchConditionsList. Either 'Or' or 'And'. Defaults to 'And'.
	#' @param searchSortFieldNamesList The list of fields sort results by. Defaults to NULL (unsorted).
	#' @param searchSortFieldNamesDescendingList A list of T/F values corresponding to whether to sort each field in searchSortFieldNamesList in descending order. Defaults to F for each FieldName in searchSortFieldNamesList.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{listSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param page Results are paginated. The page of results to return. Default is 1.
	#' @param pageSize Results are paginated. The number of records per page to return. Default is 100,000 (essentially all records for most objects).
	#' @param flatten Whether to flatten results into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Minnehaha Customization
	#' @return A list of HealthConditionResponses
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	listHealthConditionResponses <- function(searchConditionsList = NULL, HealthConditionResponsesID = F, SevereLifeThreateningAllergicReactionID = F, EpiPenNeededatSchoolID = F, AllergicTo = F, OtherTreatments = F, ADHDADDID = F, MedicationatHomeID = F, MedicationandDosage = F, AsthmaID = F, AsthmaMDID = F, InhaleratSchoolID = F, NubulizeratSchool = F, InhalerforSportsID = F, BladderBowelProblemsID = F, BladderProblemandTreatment = F, DiabetesID = F, DiabetesNotesandInstructions = F, HeartProblemsID = F, HeartProblemandTreatment = F, SeizuresID = F, SeizureProblemandTreatment = F, DateofLastSeizure = F, LearningHealthConcernsID = F, LearningHealthConcernsandTreatment = F, VisionorHearingProblemsID = F, GlassesNecessaryID = F, HearingAidsNecessaryID = F, VisionorHearingProblemorAccomodation = F, ActivityRestrictionsID = F, Restrictions = F, RecentSignificantHealthProblemsID = F, DateofOccurrence = F, DescribeProblemandTreatment = F, DailyMedicationsID = F, Medication1Name = F, Medication1Dose = F, Medication1Time = F, Medication1Purpose = F, Medication2Name = F, Medication2Dose = F, Medication2Time = F, Medication2Purpose = F, Medication3Name = F, Medication3Dose = F, Medication3Time = F, Medication3Purpose = F, Medication4Name = F, Medication4Dose = F, Medication4Time = F, OtherHealthProblemsID = F, OtherHeathProblemsandTreatments = F, StudentID = F, MedicationatSchoolID = F, RecentSignificantHealthCondition = F, Medication4Purpose = F, fieldPaths = NULL, searchConditionsGroupType = "And", searchSortFieldNamesList = NULL, searchSortFieldNamesDescendingList = NULL, entityId = 1, schoolYearId = NULL, page = 1, pageSize = 100000, flatten = T, returnResponse = F){

		params <- as.list(environment())

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		listSkyObjects(module = "MinnehahaCustomization", objectName = "HealthConditionResponses", searchFields = searchFields %>% append(fieldPaths), page = page, pageSize = pageSize, SearchConditionsList = searchConditionsList, SearchConditionsGroupType = searchConditionsGroupType, SearchSortFieldNamesList = searchSortFieldNamesList, SearchSortFieldNamesDescendingList = searchSortFieldNamesDescendingList, entityId = entityId, schoolYearId = schoolYearId, flatten = flatten, returnResponse = returnResponse)
	}

	#' Get a HealthConditionResponses
	#'
	#' This function returns a dataframe or json object of a HealthConditionResponses
	#' @param HealthConditionResponsesID The ID of the HealthConditionResponses to return.
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given HealthConditionResponses. Defaults to FALSE for all return fields which, for convenience, returns all fields for the HealthConditionResponses.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object listed as text. Run \code{\link{getSchemaForObjects}}('HealthConditionResponses') to get more field paths.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{listSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Minnehaha Customization
	#' @return A dataframe or of HealthConditionResponses
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	getHealthConditionResponses <- function(HealthConditionResponsesID, SevereLifeThreateningAllergicReactionID = F, EpiPenNeededatSchoolID = F, AllergicTo = F, OtherTreatments = F, ADHDADDID = F, MedicationatHomeID = F, MedicationandDosage = F, AsthmaID = F, AsthmaMDID = F, InhaleratSchoolID = F, NubulizeratSchool = F, InhalerforSportsID = F, BladderBowelProblemsID = F, BladderProblemandTreatment = F, DiabetesID = F, DiabetesNotesandInstructions = F, HeartProblemsID = F, HeartProblemandTreatment = F, SeizuresID = F, SeizureProblemandTreatment = F, DateofLastSeizure = F, LearningHealthConcernsID = F, LearningHealthConcernsandTreatment = F, VisionorHearingProblemsID = F, GlassesNecessaryID = F, HearingAidsNecessaryID = F, VisionorHearingProblemorAccomodation = F, ActivityRestrictionsID = F, Restrictions = F, RecentSignificantHealthProblemsID = F, DateofOccurrence = F, DescribeProblemandTreatment = F, DailyMedicationsID = F, Medication1Name = F, Medication1Dose = F, Medication1Time = F, Medication1Purpose = F, Medication2Name = F, Medication2Dose = F, Medication2Time = F, Medication2Purpose = F, Medication3Name = F, Medication3Dose = F, Medication3Time = F, Medication3Purpose = F, Medication4Name = F, Medication4Dose = F, Medication4Time = F, OtherHealthProblemsID = F, OtherHeathProblemsandTreatments = F, StudentID = F, MedicationatSchoolID = F, RecentSignificantHealthCondition = F, Medication4Purpose = F, fieldPaths = NULL, entityId = 1, schoolYearId = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment()) %>% purrr::keep(names(.) != "HealthConditionResponsesID")

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		getSkyObject(module = "MinnehahaCustomization", objectName = "HealthConditionResponses", objectId = HealthConditionResponsesID, searchFields = searchFields, entityId = entityId, flatten = flatten, returnResponse = returnResponse)
	}

	#' Delete a HealthConditionResponses
	#'
	#' This function deletes a HealthConditionResponses
	#' @param HealthConditionResponsesID The ID of the HealthConditionResponses to delete
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{listSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Minnehaha Customization
	#' @return The HealthConditionResponsesID of the deleted HealthConditionResponses.
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	deleteHealthConditionResponses <- function(HealthConditionResponsesID, ignoreWarnings = F, entityId = 1, schoolYearId = NULL, flatten = T, returnResponse = F){

		deleteSkyObject(module = "MinnehahaCustomization", objectName = "HealthConditionResponses", objectId = HealthConditionResponsesID, ignoreWarnings = ignoreWarnings, entityId = entityId, flatten = flatten, returnResponse = returnResponse)
	}

	#' Create a HealthConditionResponses
	#'
	#' This function creates a HealthConditionResponses
	#' @param fieldNames The field values to give the created HealthConditionResponses. Each defaults to NULL.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{listSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Minnehaha Customization
	#' @return A newly created HealthConditionResponses
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	createHealthConditionResponses <- function(SevereLifeThreateningAllergicReactionID = NULL, EpiPenNeededatSchoolID = NULL, AllergicTo = NULL, OtherTreatments = NULL, ADHDADDID = NULL, MedicationatHomeID = NULL, MedicationandDosage = NULL, AsthmaID = NULL, AsthmaMDID = NULL, InhaleratSchoolID = NULL, NubulizeratSchool = NULL, InhalerforSportsID = NULL, BladderBowelProblemsID = NULL, BladderProblemandTreatment = NULL, DiabetesID = NULL, DiabetesNotesandInstructions = NULL, HeartProblemsID = NULL, HeartProblemandTreatment = NULL, SeizuresID = NULL, SeizureProblemandTreatment = NULL, DateofLastSeizure = NULL, LearningHealthConcernsID = NULL, LearningHealthConcernsandTreatment = NULL, VisionorHearingProblemsID = NULL, GlassesNecessaryID = NULL, HearingAidsNecessaryID = NULL, VisionorHearingProblemorAccomodation = NULL, ActivityRestrictionsID = NULL, Restrictions = NULL, RecentSignificantHealthProblemsID = NULL, DateofOccurrence = NULL, DescribeProblemandTreatment = NULL, DailyMedicationsID = NULL, Medication1Name = NULL, Medication1Dose = NULL, Medication1Time = NULL, Medication1Purpose = NULL, Medication2Name = NULL, Medication2Dose = NULL, Medication2Time = NULL, Medication2Purpose = NULL, Medication3Name = NULL, Medication3Dose = NULL, Medication3Time = NULL, Medication3Purpose = NULL, Medication4Name = NULL, Medication4Dose = NULL, Medication4Time = NULL, OtherHealthProblemsID = NULL, OtherHeathProblemsandTreatments = NULL, StudentID = NULL, MedicationatSchoolID = NULL, RecentSignificantHealthCondition = NULL, Medication4Purpose = NULL, entityId = 1, schoolYearId = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment())

		body <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper()) %>% purrr::compact()

		createSkyObject(module = "MinnehahaCustomization", objectName = "HealthConditionResponses", body = list(DataObject = body), searchFields = append("HealthConditionResponsesID", body %>% names()), entityId = entityId, schoolYearId = schoolYearId, flatten = flatten, returnResponse = returnResponse)
	}

	#' Modify a HealthConditionResponses
	#'
	#' This function modifies a HealthConditionResponses
	#' @param fieldNames The field values to give the modified HealthConditionResponses. Each defaults to NULL.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{listSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Minnehaha Customization
	#' @return The modified HealthConditionResponses
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	modifyHealthConditionResponses <- function(HealthConditionResponsesID, SevereLifeThreateningAllergicReactionID = NULL, EpiPenNeededatSchoolID = NULL, AllergicTo = NULL, OtherTreatments = NULL, ADHDADDID = NULL, MedicationatHomeID = NULL, MedicationandDosage = NULL, AsthmaID = NULL, AsthmaMDID = NULL, InhaleratSchoolID = NULL, NubulizeratSchool = NULL, InhalerforSportsID = NULL, BladderBowelProblemsID = NULL, BladderProblemandTreatment = NULL, DiabetesID = NULL, DiabetesNotesandInstructions = NULL, HeartProblemsID = NULL, HeartProblemandTreatment = NULL, SeizuresID = NULL, SeizureProblemandTreatment = NULL, DateofLastSeizure = NULL, LearningHealthConcernsID = NULL, LearningHealthConcernsandTreatment = NULL, VisionorHearingProblemsID = NULL, GlassesNecessaryID = NULL, HearingAidsNecessaryID = NULL, VisionorHearingProblemorAccomodation = NULL, ActivityRestrictionsID = NULL, Restrictions = NULL, RecentSignificantHealthProblemsID = NULL, DateofOccurrence = NULL, DescribeProblemandTreatment = NULL, DailyMedicationsID = NULL, Medication1Name = NULL, Medication1Dose = NULL, Medication1Time = NULL, Medication1Purpose = NULL, Medication2Name = NULL, Medication2Dose = NULL, Medication2Time = NULL, Medication2Purpose = NULL, Medication3Name = NULL, Medication3Dose = NULL, Medication3Time = NULL, Medication3Purpose = NULL, Medication4Name = NULL, Medication4Dose = NULL, Medication4Time = NULL, OtherHealthProblemsID = NULL, OtherHeathProblemsandTreatments = NULL, StudentID = NULL, MedicationatSchoolID = NULL, RecentSignificantHealthCondition = NULL, Medication4Purpose = NULL, entityId = 1, schoolYearId = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment())

		body <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper()) %>% purrr::compact()

		modifySkyObject(module = "MinnehahaCustomization", objectName = "HealthConditionResponses", objectId = HealthConditionResponsesID, body = list(DataObject = body), searchFields = append("HealthConditionResponsesID", body %>% names()), entityId = entityId, schoolYearId = schoolYearId, flatten = flatten, returnResponse = returnResponse)
	}

	#' List OTCMedications
	#'
	#' This function returns a dataframe or json object of OTCMedications
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given OTCMedications. Defaults to FALSE for all return fields which, for convenience, returns all fields for the OTCMedications.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object listed as text. Run \code{\link{getSchemaForObjects}}('OTCMedication') to get more field paths.
	#' @param searchConditionsList A list of search conditions to filter results which are joined by the searchConditionsGroupType. Of the form {FieldName} {ConditionType} {SearchCondition}. For example, c('StudentID LessEqual 500', 'LastName Like Ander\%'). Run \code{\link{listSearchConditionTypes}} for a list of ConditionTypes. Defaults to NULL (unfiltered).
	#' @param searchConditionsGroupType The conjunction which joins multiple searchConditions in the searchConditionsList. Either 'Or' or 'And'. Defaults to 'And'.
	#' @param searchSortFieldNamesList The list of fields sort results by. Defaults to NULL (unsorted).
	#' @param searchSortFieldNamesDescendingList A list of T/F values corresponding to whether to sort each field in searchSortFieldNamesList in descending order. Defaults to F for each FieldName in searchSortFieldNamesList.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{listSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param page Results are paginated. The page of results to return. Default is 1.
	#' @param pageSize Results are paginated. The number of records per page to return. Default is 100,000 (essentially all records for most objects).
	#' @param flatten Whether to flatten results into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Minnehaha Customization
	#' @return A list of OTCMedications
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	listOTCMedications <- function(searchConditionsList = NULL, OTCMedicationID = F, IpubrofenorAdvilID = F, CoughDropsID = F, AntiItchCremeID = F, SelfCarryIbuprofenID = F, StudentID = F, SubTylenolForIbuprofenID = F, fieldPaths = NULL, searchConditionsGroupType = "And", searchSortFieldNamesList = NULL, searchSortFieldNamesDescendingList = NULL, entityId = 1, schoolYearId = NULL, page = 1, pageSize = 100000, flatten = T, returnResponse = F){

		params <- as.list(environment())

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		listSkyObjects(module = "MinnehahaCustomization", objectName = "OTCMedication", searchFields = searchFields %>% append(fieldPaths), page = page, pageSize = pageSize, SearchConditionsList = searchConditionsList, SearchConditionsGroupType = searchConditionsGroupType, SearchSortFieldNamesList = searchSortFieldNamesList, SearchSortFieldNamesDescendingList = searchSortFieldNamesDescendingList, entityId = entityId, schoolYearId = schoolYearId, flatten = flatten, returnResponse = returnResponse)
	}

	#' Get an OTCMedication
	#'
	#' This function returns a dataframe or json object of an OTCMedication
	#' @param OTCMedicationID The ID of the OTCMedication to return.
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given OTCMedication. Defaults to FALSE for all return fields which, for convenience, returns all fields for the OTCMedication.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object listed as text. Run \code{\link{getSchemaForObjects}}('OTCMedication') to get more field paths.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{listSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Minnehaha Customization
	#' @return A dataframe or of OTCMedication
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	getOTCMedication <- function(OTCMedicationID, IpubrofenorAdvilID = F, CoughDropsID = F, AntiItchCremeID = F, SelfCarryIbuprofenID = F, StudentID = F, SubTylenolForIbuprofenID = F, fieldPaths = NULL, entityId = 1, schoolYearId = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment()) %>% purrr::keep(names(.) != "OTCMedicationID")

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		getSkyObject(module = "MinnehahaCustomization", objectName = "OTCMedication", objectId = OTCMedicationID, searchFields = searchFields, entityId = entityId, flatten = flatten, returnResponse = returnResponse)
	}

	#' Delete an OTCMedication
	#'
	#' This function deletes an OTCMedication
	#' @param OTCMedicationID The ID of the OTCMedication to delete
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{listSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Minnehaha Customization
	#' @return The OTCMedicationID of the deleted OTCMedication.
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	deleteOTCMedication <- function(OTCMedicationID, ignoreWarnings = F, entityId = 1, schoolYearId = NULL, flatten = T, returnResponse = F){

		deleteSkyObject(module = "MinnehahaCustomization", objectName = "OTCMedication", objectId = OTCMedicationID, ignoreWarnings = ignoreWarnings, entityId = entityId, flatten = flatten, returnResponse = returnResponse)
	}

	#' Create an OTCMedication
	#'
	#' This function creates an OTCMedication
	#' @param fieldNames The field values to give the created OTCMedication. Each defaults to NULL.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{listSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Minnehaha Customization
	#' @return A newly created OTCMedication
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	createOTCMedication <- function(IpubrofenorAdvilID = NULL, CoughDropsID = NULL, AntiItchCremeID = NULL, SelfCarryIbuprofenID = NULL, StudentID = NULL, SubTylenolForIbuprofenID = NULL, entityId = 1, schoolYearId = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment())

		body <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper()) %>% purrr::compact()

		createSkyObject(module = "MinnehahaCustomization", objectName = "OTCMedication", body = list(DataObject = body), searchFields = append("OTCMedicationID", body %>% names()), entityId = entityId, schoolYearId = schoolYearId, flatten = flatten, returnResponse = returnResponse)
	}

	#' Modify an OTCMedication
	#'
	#' This function modifies an OTCMedication
	#' @param fieldNames The field values to give the modified OTCMedication. Each defaults to NULL.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{listSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Minnehaha Customization
	#' @return The modified OTCMedication
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	modifyOTCMedication <- function(OTCMedicationID, IpubrofenorAdvilID = NULL, CoughDropsID = NULL, AntiItchCremeID = NULL, SelfCarryIbuprofenID = NULL, StudentID = NULL, SubTylenolForIbuprofenID = NULL, entityId = 1, schoolYearId = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment())

		body <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper()) %>% purrr::compact()

		modifySkyObject(module = "MinnehahaCustomization", objectName = "OTCMedication", objectId = OTCMedicationID, body = list(DataObject = body), searchFields = append("OTCMedicationID", body %>% names()), entityId = entityId, schoolYearId = schoolYearId, flatten = flatten, returnResponse = returnResponse)
	}

	#' List GrandPersonMAS
	#'
	#' This function returns a dataframe or json object of GrandPersonMAS
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given GrandPersonMAS. Defaults to FALSE for all return fields which, for convenience, returns all fields for the GrandPersonMAS.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object listed as text. Run \code{\link{getSchemaForObjects}}('GrandPersonMA') to get more field paths.
	#' @param searchConditionsList A list of search conditions to filter results which are joined by the searchConditionsGroupType. Of the form {FieldName} {ConditionType} {SearchCondition}. For example, c('StudentID LessEqual 500', 'LastName Like Ander\%'). Run \code{\link{listSearchConditionTypes}} for a list of ConditionTypes. Defaults to NULL (unfiltered).
	#' @param searchConditionsGroupType The conjunction which joins multiple searchConditions in the searchConditionsList. Either 'Or' or 'And'. Defaults to 'And'.
	#' @param searchSortFieldNamesList The list of fields sort results by. Defaults to NULL (unsorted).
	#' @param searchSortFieldNamesDescendingList A list of T/F values corresponding to whether to sort each field in searchSortFieldNamesList in descending order. Defaults to F for each FieldName in searchSortFieldNamesList.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{listSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param page Results are paginated. The page of results to return. Default is 1.
	#' @param pageSize Results are paginated. The number of records per page to return. Default is 100,000 (essentially all records for most objects).
	#' @param flatten Whether to flatten results into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Minnehaha Customization
	#' @return A list of GrandPersonMAS
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	listGrandPersonMAS <- function(searchConditionsList = NULL, GrandPersonMAID = F, FamilyID = F, NameID = F, RelationshipID = F, fieldPaths = NULL, searchConditionsGroupType = "And", searchSortFieldNamesList = NULL, searchSortFieldNamesDescendingList = NULL, entityId = 1, schoolYearId = NULL, page = 1, pageSize = 100000, flatten = T, returnResponse = F){

		params <- as.list(environment())

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		listSkyObjects(module = "MinnehahaCustomization", objectName = "GrandPersonMA", searchFields = searchFields %>% append(fieldPaths), page = page, pageSize = pageSize, SearchConditionsList = searchConditionsList, SearchConditionsGroupType = searchConditionsGroupType, SearchSortFieldNamesList = searchSortFieldNamesList, SearchSortFieldNamesDescendingList = searchSortFieldNamesDescendingList, entityId = entityId, schoolYearId = schoolYearId, flatten = flatten, returnResponse = returnResponse)
	}

	#' Get a GrandPersonMA
	#'
	#' This function returns a dataframe or json object of a GrandPersonMA
	#' @param GrandPersonMAID The ID of the GrandPersonMA to return.
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given GrandPersonMA. Defaults to FALSE for all return fields which, for convenience, returns all fields for the GrandPersonMA.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object listed as text. Run \code{\link{getSchemaForObjects}}('GrandPersonMA') to get more field paths.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{listSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Minnehaha Customization
	#' @return A dataframe or of GrandPersonMA
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	getGrandPersonMA <- function(GrandPersonMAID, FamilyID = F, NameID = F, RelationshipID = F, fieldPaths = NULL, entityId = 1, schoolYearId = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment()) %>% purrr::keep(names(.) != "GrandPersonMAID")

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		getSkyObject(module = "MinnehahaCustomization", objectName = "GrandPersonMA", objectId = GrandPersonMAID, searchFields = searchFields, entityId = entityId, flatten = flatten, returnResponse = returnResponse)
	}

	#' Delete a GrandPersonMA
	#'
	#' This function deletes a GrandPersonMA
	#' @param GrandPersonMAID The ID of the GrandPersonMA to delete
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{listSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Minnehaha Customization
	#' @return The GrandPersonMAID of the deleted GrandPersonMA.
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	deleteGrandPersonMA <- function(GrandPersonMAID, ignoreWarnings = F, entityId = 1, schoolYearId = NULL, flatten = T, returnResponse = F){

		deleteSkyObject(module = "MinnehahaCustomization", objectName = "GrandPersonMA", objectId = GrandPersonMAID, ignoreWarnings = ignoreWarnings, entityId = entityId, flatten = flatten, returnResponse = returnResponse)
	}

	#' Create a GrandPersonMA
	#'
	#' This function creates a GrandPersonMA
	#' @param fieldNames The field values to give the created GrandPersonMA. Each defaults to NULL.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{listSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Minnehaha Customization
	#' @return A newly created GrandPersonMA
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	createGrandPersonMA <- function(FamilyID = NULL, NameID = NULL, RelationshipID = NULL, entityId = 1, schoolYearId = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment())

		body <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper()) %>% purrr::compact()

		createSkyObject(module = "MinnehahaCustomization", objectName = "GrandPersonMA", body = list(DataObject = body), searchFields = append("GrandPersonMAID", body %>% names()), entityId = entityId, schoolYearId = schoolYearId, flatten = flatten, returnResponse = returnResponse)
	}

	#' Modify a GrandPersonMA
	#'
	#' This function modifies a GrandPersonMA
	#' @param fieldNames The field values to give the modified GrandPersonMA. Each defaults to NULL.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{listSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Minnehaha Customization
	#' @return The modified GrandPersonMA
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	modifyGrandPersonMA <- function(GrandPersonMAID, FamilyID = NULL, NameID = NULL, RelationshipID = NULL, entityId = 1, schoolYearId = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment())

		body <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper()) %>% purrr::compact()

		modifySkyObject(module = "MinnehahaCustomization", objectName = "GrandPersonMA", objectId = GrandPersonMAID, body = list(DataObject = body), searchFields = append("GrandPersonMAID", body %>% names()), entityId = entityId, schoolYearId = schoolYearId, flatten = flatten, returnResponse = returnResponse)
	}

	#' List ParentalConsents
	#'
	#' This function returns a dataframe or json object of ParentalConsents
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given ParentalConsents. Defaults to FALSE for all return fields which, for convenience, returns all fields for the ParentalConsents.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object listed as text. Run \code{\link{getSchemaForObjects}}('ParentalConsent') to get more field paths.
	#' @param searchConditionsList A list of search conditions to filter results which are joined by the searchConditionsGroupType. Of the form {FieldName} {ConditionType} {SearchCondition}. For example, c('StudentID LessEqual 500', 'LastName Like Ander\%'). Run \code{\link{listSearchConditionTypes}} for a list of ConditionTypes. Defaults to NULL (unfiltered).
	#' @param searchConditionsGroupType The conjunction which joins multiple searchConditions in the searchConditionsList. Either 'Or' or 'And'. Defaults to 'And'.
	#' @param searchSortFieldNamesList The list of fields sort results by. Defaults to NULL (unsorted).
	#' @param searchSortFieldNamesDescendingList A list of T/F values corresponding to whether to sort each field in searchSortFieldNamesList in descending order. Defaults to F for each FieldName in searchSortFieldNamesList.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{listSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param page Results are paginated. The page of results to return. Default is 1.
	#' @param pageSize Results are paginated. The number of records per page to return. Default is 100,000 (essentially all records for most objects).
	#' @param flatten Whether to flatten results into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Minnehaha Customization
	#' @return A list of ParentalConsents
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	listParentalConsents <- function(searchConditionsList = NULL, ParentalConsentID = F, InstructionalMaterialsEducationalAids = F, HealthServicesEducationalAids = F, CounselingServicesEducationalAids = F, FamilyID = F, COPPAConsent = F, ContactInfoInDirectory = F, WalkingFieldTrip = F, TechnologyAgreement = F, TechnologyAgreement2 = F, fieldPaths = NULL, searchConditionsGroupType = "And", searchSortFieldNamesList = NULL, searchSortFieldNamesDescendingList = NULL, entityId = 1, schoolYearId = NULL, page = 1, pageSize = 100000, flatten = T, returnResponse = F){

		params <- as.list(environment())

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		listSkyObjects(module = "MinnehahaCustomization", objectName = "ParentalConsent", searchFields = searchFields %>% append(fieldPaths), page = page, pageSize = pageSize, SearchConditionsList = searchConditionsList, SearchConditionsGroupType = searchConditionsGroupType, SearchSortFieldNamesList = searchSortFieldNamesList, SearchSortFieldNamesDescendingList = searchSortFieldNamesDescendingList, entityId = entityId, schoolYearId = schoolYearId, flatten = flatten, returnResponse = returnResponse)
	}

	#' Get a ParentalConsent
	#'
	#' This function returns a dataframe or json object of a ParentalConsent
	#' @param ParentalConsentID The ID of the ParentalConsent to return.
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given ParentalConsent. Defaults to FALSE for all return fields which, for convenience, returns all fields for the ParentalConsent.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object listed as text. Run \code{\link{getSchemaForObjects}}('ParentalConsent') to get more field paths.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{listSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Minnehaha Customization
	#' @return A dataframe or of ParentalConsent
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	getParentalConsent <- function(ParentalConsentID, InstructionalMaterialsEducationalAids = F, HealthServicesEducationalAids = F, CounselingServicesEducationalAids = F, FamilyID = F, COPPAConsent = F, ContactInfoInDirectory = F, WalkingFieldTrip = F, TechnologyAgreement = F, TechnologyAgreement2 = F, fieldPaths = NULL, entityId = 1, schoolYearId = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment()) %>% purrr::keep(names(.) != "ParentalConsentID")

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		getSkyObject(module = "MinnehahaCustomization", objectName = "ParentalConsent", objectId = ParentalConsentID, searchFields = searchFields, entityId = entityId, flatten = flatten, returnResponse = returnResponse)
	}

	#' Delete a ParentalConsent
	#'
	#' This function deletes a ParentalConsent
	#' @param ParentalConsentID The ID of the ParentalConsent to delete
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{listSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Minnehaha Customization
	#' @return The ParentalConsentID of the deleted ParentalConsent.
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	deleteParentalConsent <- function(ParentalConsentID, ignoreWarnings = F, entityId = 1, schoolYearId = NULL, flatten = T, returnResponse = F){

		deleteSkyObject(module = "MinnehahaCustomization", objectName = "ParentalConsent", objectId = ParentalConsentID, ignoreWarnings = ignoreWarnings, entityId = entityId, flatten = flatten, returnResponse = returnResponse)
	}

	#' Create a ParentalConsent
	#'
	#' This function creates a ParentalConsent
	#' @param fieldNames The field values to give the created ParentalConsent. Each defaults to NULL.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{listSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Minnehaha Customization
	#' @return A newly created ParentalConsent
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	createParentalConsent <- function(InstructionalMaterialsEducationalAids = NULL, HealthServicesEducationalAids = NULL, CounselingServicesEducationalAids = NULL, FamilyID = NULL, COPPAConsent = NULL, ContactInfoInDirectory = NULL, WalkingFieldTrip = NULL, TechnologyAgreement = NULL, TechnologyAgreement2 = NULL, entityId = 1, schoolYearId = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment())

		body <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper()) %>% purrr::compact()

		createSkyObject(module = "MinnehahaCustomization", objectName = "ParentalConsent", body = list(DataObject = body), searchFields = append("ParentalConsentID", body %>% names()), entityId = entityId, schoolYearId = schoolYearId, flatten = flatten, returnResponse = returnResponse)
	}

	#' Modify a ParentalConsent
	#'
	#' This function modifies a ParentalConsent
	#' @param fieldNames The field values to give the modified ParentalConsent. Each defaults to NULL.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{listSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Minnehaha Customization
	#' @return The modified ParentalConsent
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	modifyParentalConsent <- function(ParentalConsentID, InstructionalMaterialsEducationalAids = NULL, HealthServicesEducationalAids = NULL, CounselingServicesEducationalAids = NULL, FamilyID = NULL, COPPAConsent = NULL, ContactInfoInDirectory = NULL, WalkingFieldTrip = NULL, TechnologyAgreement = NULL, TechnologyAgreement2 = NULL, entityId = 1, schoolYearId = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment())

		body <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper()) %>% purrr::compact()

		modifySkyObject(module = "MinnehahaCustomization", objectName = "ParentalConsent", objectId = ParentalConsentID, body = list(DataObject = body), searchFields = append("ParentalConsentID", body %>% names()), entityId = entityId, schoolYearId = schoolYearId, flatten = flatten, returnResponse = returnResponse)
	}

	#' List Preschools
	#'
	#' This function returns a dataframe or json object of Preschools
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given Preschools. Defaults to FALSE for all return fields which, for convenience, returns all fields for the Preschools.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object listed as text. Run \code{\link{getSchemaForObjects}}('Preschool') to get more field paths.
	#' @param searchConditionsList A list of search conditions to filter results which are joined by the searchConditionsGroupType. Of the form {FieldName} {ConditionType} {SearchCondition}. For example, c('StudentID LessEqual 500', 'LastName Like Ander\%'). Run \code{\link{listSearchConditionTypes}} for a list of ConditionTypes. Defaults to NULL (unfiltered).
	#' @param searchConditionsGroupType The conjunction which joins multiple searchConditions in the searchConditionsList. Either 'Or' or 'And'. Defaults to 'And'.
	#' @param searchSortFieldNamesList The list of fields sort results by. Defaults to NULL (unsorted).
	#' @param searchSortFieldNamesDescendingList A list of T/F values corresponding to whether to sort each field in searchSortFieldNamesList in descending order. Defaults to F for each FieldName in searchSortFieldNamesList.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{listSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param page Results are paginated. The page of results to return. Default is 1.
	#' @param pageSize Results are paginated. The number of records per page to return. Default is 100,000 (essentially all records for most objects).
	#' @param flatten Whether to flatten results into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Minnehaha Customization
	#' @return A list of Preschools
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	listPreschools <- function(searchConditionsList = NULL, PreschoolID = F, EmergencyContact1Address = F, EmergencyContact2Address = F, ActInAnEmergency = F, EarlyChildhoodScreening = F, FoodPolicy = F, ProvidedLotions = F, Toileting = F, StudentID = F, fieldPaths = NULL, searchConditionsGroupType = "And", searchSortFieldNamesList = NULL, searchSortFieldNamesDescendingList = NULL, entityId = 1, schoolYearId = NULL, page = 1, pageSize = 100000, flatten = T, returnResponse = F){

		params <- as.list(environment())

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		listSkyObjects(module = "MinnehahaCustomization", objectName = "Preschool", searchFields = searchFields %>% append(fieldPaths), page = page, pageSize = pageSize, SearchConditionsList = searchConditionsList, SearchConditionsGroupType = searchConditionsGroupType, SearchSortFieldNamesList = searchSortFieldNamesList, SearchSortFieldNamesDescendingList = searchSortFieldNamesDescendingList, entityId = entityId, schoolYearId = schoolYearId, flatten = flatten, returnResponse = returnResponse)
	}

	#' Get a Preschool
	#'
	#' This function returns a dataframe or json object of a Preschool
	#' @param PreschoolID The ID of the Preschool to return.
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given Preschool. Defaults to FALSE for all return fields which, for convenience, returns all fields for the Preschool.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object listed as text. Run \code{\link{getSchemaForObjects}}('Preschool') to get more field paths.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{listSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Minnehaha Customization
	#' @return A dataframe or of Preschool
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	getPreschool <- function(PreschoolID, EmergencyContact1Address = F, EmergencyContact2Address = F, ActInAnEmergency = F, EarlyChildhoodScreening = F, FoodPolicy = F, ProvidedLotions = F, Toileting = F, StudentID = F, fieldPaths = NULL, entityId = 1, schoolYearId = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment()) %>% purrr::keep(names(.) != "PreschoolID")

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		getSkyObject(module = "MinnehahaCustomization", objectName = "Preschool", objectId = PreschoolID, searchFields = searchFields, entityId = entityId, flatten = flatten, returnResponse = returnResponse)
	}

	#' Delete a Preschool
	#'
	#' This function deletes a Preschool
	#' @param PreschoolID The ID of the Preschool to delete
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{listSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Minnehaha Customization
	#' @return The PreschoolID of the deleted Preschool.
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	deletePreschool <- function(PreschoolID, ignoreWarnings = F, entityId = 1, schoolYearId = NULL, flatten = T, returnResponse = F){

		deleteSkyObject(module = "MinnehahaCustomization", objectName = "Preschool", objectId = PreschoolID, ignoreWarnings = ignoreWarnings, entityId = entityId, flatten = flatten, returnResponse = returnResponse)
	}

	#' Create a Preschool
	#'
	#' This function creates a Preschool
	#' @param fieldNames The field values to give the created Preschool. Each defaults to NULL.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{listSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Minnehaha Customization
	#' @return A newly created Preschool
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	createPreschool <- function(EmergencyContact1Address = NULL, EmergencyContact2Address = NULL, ActInAnEmergency = NULL, EarlyChildhoodScreening = NULL, FoodPolicy = NULL, ProvidedLotions = NULL, Toileting = NULL, StudentID = NULL, entityId = 1, schoolYearId = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment())

		body <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper()) %>% purrr::compact()

		createSkyObject(module = "MinnehahaCustomization", objectName = "Preschool", body = list(DataObject = body), searchFields = append("PreschoolID", body %>% names()), entityId = entityId, schoolYearId = schoolYearId, flatten = flatten, returnResponse = returnResponse)
	}

	#' Modify a Preschool
	#'
	#' This function modifies a Preschool
	#' @param fieldNames The field values to give the modified Preschool. Each defaults to NULL.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param schoolYearId The id of the schoolYear. Run \code{\link{listSchoolYears}} for a list of school years. Defaults to NULL (all school years).
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Minnehaha Customization
	#' @return The modified Preschool
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	modifyPreschool <- function(PreschoolID, EmergencyContact1Address = NULL, EmergencyContact2Address = NULL, ActInAnEmergency = NULL, EarlyChildhoodScreening = NULL, FoodPolicy = NULL, ProvidedLotions = NULL, Toileting = NULL, StudentID = NULL, entityId = 1, schoolYearId = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment())

		body <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper()) %>% purrr::compact()

		modifySkyObject(module = "MinnehahaCustomization", objectName = "Preschool", objectId = PreschoolID, body = list(DataObject = body), searchFields = append("PreschoolID", body %>% names()), entityId = entityId, schoolYearId = schoolYearId, flatten = flatten, returnResponse = returnResponse)
	}
