
	#' List AreaOfConcerns
	#'
	#' This function returns a dataframe or json object of AreaOfConcerns
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given AreaOfConcerns. Defaults to FALSE for all return fields which, for convenience, returns all fields for the AreaOfConcerns.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object listed as text. Run \code{\link{getSchemaForObjects}}('AreaOfConcern') to get more field paths.
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
	#' @concept MTSS
	#' @return A list of AreaOfConcerns
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	listAreaOfConcerns <- function(searchConditionsList = NULL, AreaOfConcernID = F, Code = F, Description = F, DistrictID = F, CodeDescription = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, IsActive = F, fieldPaths = NULL, searchConditionsGroupType = "And", searchSortFieldNamesList = NULL, searchSortFieldNamesDescendingList = NULL, entityId = 1, query = NULL, page = 1, pageSize = 100000, flatten = T, returnResponse = F){

		params <- as.list(environment())

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		listSkyObjects(module = "MTSS", objectName = "AreaOfConcern", searchFields = searchFields %>% append(fieldPaths), page = page, pageSize = pageSize, SearchConditionsList = searchConditionsList, SearchConditionsGroupType = searchConditionsGroupType, SearchSortFieldNamesList = searchSortFieldNamesList, SearchSortFieldNamesDescendingList = searchSortFieldNamesDescendingList, entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' Get an AreaOfConcern
	#'
	#' This function returns a dataframe or json object of an AreaOfConcern
	#' @param AreaOfConcernID The ID of the AreaOfConcern to return.
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given AreaOfConcern. Defaults to FALSE for all return fields which, for convenience, returns all fields for the AreaOfConcern.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object listed as text. Run \code{\link{getSchemaForObjects}}('AreaOfConcern') to get more field paths.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param query Queries to be added to the request. Ex. list(SchoolYearID = 1). Defaults to NULL.
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept MTSS
	#' @return A dataframe or of AreaOfConcern
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	getAreaOfConcern <- function(AreaOfConcernID, Code = F, Description = F, DistrictID = F, CodeDescription = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, IsActive = F, fieldPaths = NULL, entityId = 1, query = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment()) %>% purrr::keep(names(.) != "AreaOfConcernID")

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		getSkyObject(module = "MTSS", objectName = "AreaOfConcern", objectId = AreaOfConcernID, searchFields = searchFields, entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' Delete an AreaOfConcern
	#'
	#' This function deletes an AreaOfConcern
	#' @param AreaOfConcernID The ID of the AreaOfConcern to delete
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param query Queries to be added to the request. Ex. list(SchoolYearID = 1). Defaults to NULL.
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept MTSS
	#' @return The AreaOfConcernID of the deleted AreaOfConcern.
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	deleteAreaOfConcern <- function(AreaOfConcernID, ignoreWarnings = F, entityId = 1, query = NULL, flatten = T, returnResponse = F){

		deleteSkyObject(module = "MTSS", objectName = "AreaOfConcern", objectId = AreaOfConcernID, ignoreWarnings = ignoreWarnings, entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' Create an AreaOfConcern
	#'
	#' This function creates an AreaOfConcern
	#' @param fieldNames The field values to give the created AreaOfConcern. Each defaults to NULL.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param query Queries to be added to the request. Ex. list(SchoolYearID = 1). Defaults to NULL.
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept MTSS
	#' @return A newly created AreaOfConcern
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	createAreaOfConcern <- function(Code = NULL, Description = NULL, DistrictID = NULL, IsActive = NULL, entityId = 1, query = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment())

		body <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper()) %>% purrr::compact()

		createSkyObject(module = "MTSS", objectName = "AreaOfConcern", body = list(DataObject = body), searchFields = append("AreaOfConcernID", body %>% names()), entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' Modify an AreaOfConcern
	#'
	#' This function modifies an AreaOfConcern
	#' @param fieldNames The field values to give the modified AreaOfConcern. Each defaults to NULL.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param query Queries to be added to the request. Ex. list(SchoolYearID = 1). Defaults to NULL.
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept MTSS
	#' @return The modified AreaOfConcern
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	modifyAreaOfConcern <- function(AreaOfConcernID, Code = NULL, Description = NULL, DistrictID = NULL, IsActive = NULL, entityId = 1, query = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment())

		body <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper()) %>% purrr::compact()

		modifySkyObject(module = "MTSS", objectName = "AreaOfConcern", objectId = AreaOfConcernID, body = list(DataObject = body), searchFields = append("AreaOfConcernID", body %>% names()), entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' List Outcomes
	#'
	#' This function returns a dataframe or json object of Outcomes
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given Outcomes. Defaults to FALSE for all return fields which, for convenience, returns all fields for the Outcomes.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object listed as text. Run \code{\link{getSchemaForObjects}}('Outcome') to get more field paths.
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
	#' @concept MTSS
	#' @return A list of Outcomes
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	listOutcomes <- function(searchConditionsList = NULL, OutcomeID = F, Code = F, Description = F, DistrictID = F, CodeDescription = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, UseForObservation = F, UseForIntervention = F, IsActive = F, fieldPaths = NULL, searchConditionsGroupType = "And", searchSortFieldNamesList = NULL, searchSortFieldNamesDescendingList = NULL, entityId = 1, query = NULL, page = 1, pageSize = 100000, flatten = T, returnResponse = F){

		params <- as.list(environment())

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		listSkyObjects(module = "MTSS", objectName = "Outcome", searchFields = searchFields %>% append(fieldPaths), page = page, pageSize = pageSize, SearchConditionsList = searchConditionsList, SearchConditionsGroupType = searchConditionsGroupType, SearchSortFieldNamesList = searchSortFieldNamesList, SearchSortFieldNamesDescendingList = searchSortFieldNamesDescendingList, entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' Get an Outcome
	#'
	#' This function returns a dataframe or json object of an Outcome
	#' @param OutcomeID The ID of the Outcome to return.
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given Outcome. Defaults to FALSE for all return fields which, for convenience, returns all fields for the Outcome.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object listed as text. Run \code{\link{getSchemaForObjects}}('Outcome') to get more field paths.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param query Queries to be added to the request. Ex. list(SchoolYearID = 1). Defaults to NULL.
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept MTSS
	#' @return A dataframe or of Outcome
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	getOutcome <- function(OutcomeID, Code = F, Description = F, DistrictID = F, CodeDescription = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, UseForObservation = F, UseForIntervention = F, IsActive = F, fieldPaths = NULL, entityId = 1, query = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment()) %>% purrr::keep(names(.) != "OutcomeID")

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		getSkyObject(module = "MTSS", objectName = "Outcome", objectId = OutcomeID, searchFields = searchFields, entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' Delete an Outcome
	#'
	#' This function deletes an Outcome
	#' @param OutcomeID The ID of the Outcome to delete
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param query Queries to be added to the request. Ex. list(SchoolYearID = 1). Defaults to NULL.
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept MTSS
	#' @return The OutcomeID of the deleted Outcome.
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	deleteOutcome <- function(OutcomeID, ignoreWarnings = F, entityId = 1, query = NULL, flatten = T, returnResponse = F){

		deleteSkyObject(module = "MTSS", objectName = "Outcome", objectId = OutcomeID, ignoreWarnings = ignoreWarnings, entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' Create an Outcome
	#'
	#' This function creates an Outcome
	#' @param fieldNames The field values to give the created Outcome. Each defaults to NULL.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param query Queries to be added to the request. Ex. list(SchoolYearID = 1). Defaults to NULL.
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept MTSS
	#' @return A newly created Outcome
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	createOutcome <- function(Code = NULL, Description = NULL, DistrictID = NULL, UseForObservation = NULL, UseForIntervention = NULL, IsActive = NULL, entityId = 1, query = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment())

		body <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper()) %>% purrr::compact()

		createSkyObject(module = "MTSS", objectName = "Outcome", body = list(DataObject = body), searchFields = append("OutcomeID", body %>% names()), entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' Modify an Outcome
	#'
	#' This function modifies an Outcome
	#' @param fieldNames The field values to give the modified Outcome. Each defaults to NULL.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param query Queries to be added to the request. Ex. list(SchoolYearID = 1). Defaults to NULL.
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept MTSS
	#' @return The modified Outcome
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	modifyOutcome <- function(OutcomeID, Code = NULL, Description = NULL, DistrictID = NULL, UseForObservation = NULL, UseForIntervention = NULL, IsActive = NULL, entityId = 1, query = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment())

		body <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper()) %>% purrr::compact()

		modifySkyObject(module = "MTSS", objectName = "Outcome", objectId = OutcomeID, body = list(DataObject = body), searchFields = append("OutcomeID", body %>% names()), entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' List Observations
	#'
	#' This function returns a dataframe or json object of Observations
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given Observations. Defaults to FALSE for all return fields which, for convenience, returns all fields for the Observations.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object listed as text. Run \code{\link{getSchemaForObjects}}('Observation') to get more field paths.
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
	#' @concept MTSS
	#' @return A list of Observations
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	listObservations <- function(searchConditionsList = NULL, ObservationID = F, StudentID = F, AreaOfConcernID = F, Note = F, StartDate = F, EndDate = F, AttachmentCount = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, DateMethod = F, fieldPaths = NULL, searchConditionsGroupType = "And", searchSortFieldNamesList = NULL, searchSortFieldNamesDescendingList = NULL, entityId = 1, query = NULL, page = 1, pageSize = 100000, flatten = T, returnResponse = F){

		params <- as.list(environment())

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		listSkyObjects(module = "MTSS", objectName = "Observation", searchFields = searchFields %>% append(fieldPaths), page = page, pageSize = pageSize, SearchConditionsList = searchConditionsList, SearchConditionsGroupType = searchConditionsGroupType, SearchSortFieldNamesList = searchSortFieldNamesList, SearchSortFieldNamesDescendingList = searchSortFieldNamesDescendingList, entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' Get an Observation
	#'
	#' This function returns a dataframe or json object of an Observation
	#' @param ObservationID The ID of the Observation to return.
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given Observation. Defaults to FALSE for all return fields which, for convenience, returns all fields for the Observation.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object listed as text. Run \code{\link{getSchemaForObjects}}('Observation') to get more field paths.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param query Queries to be added to the request. Ex. list(SchoolYearID = 1). Defaults to NULL.
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept MTSS
	#' @return A dataframe or of Observation
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	getObservation <- function(ObservationID, StudentID = F, AreaOfConcernID = F, Note = F, StartDate = F, EndDate = F, AttachmentCount = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, DateMethod = F, fieldPaths = NULL, entityId = 1, query = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment()) %>% purrr::keep(names(.) != "ObservationID")

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		getSkyObject(module = "MTSS", objectName = "Observation", objectId = ObservationID, searchFields = searchFields, entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' Delete an Observation
	#'
	#' This function deletes an Observation
	#' @param ObservationID The ID of the Observation to delete
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param query Queries to be added to the request. Ex. list(SchoolYearID = 1). Defaults to NULL.
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept MTSS
	#' @return The ObservationID of the deleted Observation.
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	deleteObservation <- function(ObservationID, ignoreWarnings = F, entityId = 1, query = NULL, flatten = T, returnResponse = F){

		deleteSkyObject(module = "MTSS", objectName = "Observation", objectId = ObservationID, ignoreWarnings = ignoreWarnings, entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' Create an Observation
	#'
	#' This function creates an Observation
	#' @param fieldNames The field values to give the created Observation. Each defaults to NULL.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param query Queries to be added to the request. Ex. list(SchoolYearID = 1). Defaults to NULL.
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept MTSS
	#' @return A newly created Observation
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	createObservation <- function(StudentID = NULL, AreaOfConcernID = NULL, Note = NULL, StartDate = NULL, EndDate = NULL, DateMethod = NULL, entityId = 1, query = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment())

		body <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper()) %>% purrr::compact()

		createSkyObject(module = "MTSS", objectName = "Observation", body = list(DataObject = body), searchFields = append("ObservationID", body %>% names()), entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' Modify an Observation
	#'
	#' This function modifies an Observation
	#' @param fieldNames The field values to give the modified Observation. Each defaults to NULL.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param query Queries to be added to the request. Ex. list(SchoolYearID = 1). Defaults to NULL.
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept MTSS
	#' @return The modified Observation
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	modifyObservation <- function(ObservationID, StudentID = NULL, AreaOfConcernID = NULL, Note = NULL, StartDate = NULL, EndDate = NULL, DateMethod = NULL, entityId = 1, query = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment())

		body <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper()) %>% purrr::compact()

		modifySkyObject(module = "MTSS", objectName = "Observation", objectId = ObservationID, body = list(DataObject = body), searchFields = append("ObservationID", body %>% names()), entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' List ObservationActions
	#'
	#' This function returns a dataframe or json object of ObservationActions
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given ObservationActions. Defaults to FALSE for all return fields which, for convenience, returns all fields for the ObservationActions.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object listed as text. Run \code{\link{getSchemaForObjects}}('ObservationAction') to get more field paths.
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
	#' @concept MTSS
	#' @return A list of ObservationActions
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	listObservationActions <- function(searchConditionsList = NULL, ObservationActionID = F, ObservationID = F, OutcomeID = F, OutcomeNote = F, StartDate = F, EndDate = F, Description = F, AttachmentCount = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, DateMethod = F, fieldPaths = NULL, searchConditionsGroupType = "And", searchSortFieldNamesList = NULL, searchSortFieldNamesDescendingList = NULL, entityId = 1, query = NULL, page = 1, pageSize = 100000, flatten = T, returnResponse = F){

		params <- as.list(environment())

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		listSkyObjects(module = "MTSS", objectName = "ObservationAction", searchFields = searchFields %>% append(fieldPaths), page = page, pageSize = pageSize, SearchConditionsList = searchConditionsList, SearchConditionsGroupType = searchConditionsGroupType, SearchSortFieldNamesList = searchSortFieldNamesList, SearchSortFieldNamesDescendingList = searchSortFieldNamesDescendingList, entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' Get an ObservationAction
	#'
	#' This function returns a dataframe or json object of an ObservationAction
	#' @param ObservationActionID The ID of the ObservationAction to return.
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given ObservationAction. Defaults to FALSE for all return fields which, for convenience, returns all fields for the ObservationAction.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object listed as text. Run \code{\link{getSchemaForObjects}}('ObservationAction') to get more field paths.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param query Queries to be added to the request. Ex. list(SchoolYearID = 1). Defaults to NULL.
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept MTSS
	#' @return A dataframe or of ObservationAction
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	getObservationAction <- function(ObservationActionID, ObservationID = F, OutcomeID = F, OutcomeNote = F, StartDate = F, EndDate = F, Description = F, AttachmentCount = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, DateMethod = F, fieldPaths = NULL, entityId = 1, query = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment()) %>% purrr::keep(names(.) != "ObservationActionID")

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		getSkyObject(module = "MTSS", objectName = "ObservationAction", objectId = ObservationActionID, searchFields = searchFields, entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' Delete an ObservationAction
	#'
	#' This function deletes an ObservationAction
	#' @param ObservationActionID The ID of the ObservationAction to delete
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param query Queries to be added to the request. Ex. list(SchoolYearID = 1). Defaults to NULL.
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept MTSS
	#' @return The ObservationActionID of the deleted ObservationAction.
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	deleteObservationAction <- function(ObservationActionID, ignoreWarnings = F, entityId = 1, query = NULL, flatten = T, returnResponse = F){

		deleteSkyObject(module = "MTSS", objectName = "ObservationAction", objectId = ObservationActionID, ignoreWarnings = ignoreWarnings, entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' Create an ObservationAction
	#'
	#' This function creates an ObservationAction
	#' @param fieldNames The field values to give the created ObservationAction. Each defaults to NULL.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param query Queries to be added to the request. Ex. list(SchoolYearID = 1). Defaults to NULL.
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept MTSS
	#' @return A newly created ObservationAction
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	createObservationAction <- function(ObservationID = NULL, OutcomeID = NULL, OutcomeNote = NULL, StartDate = NULL, EndDate = NULL, Description = NULL, DateMethod = NULL, entityId = 1, query = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment())

		body <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper()) %>% purrr::compact()

		createSkyObject(module = "MTSS", objectName = "ObservationAction", body = list(DataObject = body), searchFields = append("ObservationActionID", body %>% names()), entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' Modify an ObservationAction
	#'
	#' This function modifies an ObservationAction
	#' @param fieldNames The field values to give the modified ObservationAction. Each defaults to NULL.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param query Queries to be added to the request. Ex. list(SchoolYearID = 1). Defaults to NULL.
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept MTSS
	#' @return The modified ObservationAction
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	modifyObservationAction <- function(ObservationActionID, ObservationID = NULL, OutcomeID = NULL, OutcomeNote = NULL, StartDate = NULL, EndDate = NULL, Description = NULL, DateMethod = NULL, entityId = 1, query = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment())

		body <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper()) %>% purrr::compact()

		modifySkyObject(module = "MTSS", objectName = "ObservationAction", objectId = ObservationActionID, body = list(DataObject = body), searchFields = append("ObservationActionID", body %>% names()), entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' List Tiers
	#'
	#' This function returns a dataframe or json object of Tiers
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given Tiers. Defaults to FALSE for all return fields which, for convenience, returns all fields for the Tiers.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object listed as text. Run \code{\link{getSchemaForObjects}}('Tier') to get more field paths.
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
	#' @concept MTSS
	#' @return A list of Tiers
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	listTiers <- function(searchConditionsList = NULL, TierID = F, Code = F, Description = F, Level = F, DistrictID = F, CodeDescription = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, fieldPaths = NULL, searchConditionsGroupType = "And", searchSortFieldNamesList = NULL, searchSortFieldNamesDescendingList = NULL, entityId = 1, query = NULL, page = 1, pageSize = 100000, flatten = T, returnResponse = F){

		params <- as.list(environment())

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		listSkyObjects(module = "MTSS", objectName = "Tier", searchFields = searchFields %>% append(fieldPaths), page = page, pageSize = pageSize, SearchConditionsList = searchConditionsList, SearchConditionsGroupType = searchConditionsGroupType, SearchSortFieldNamesList = searchSortFieldNamesList, SearchSortFieldNamesDescendingList = searchSortFieldNamesDescendingList, entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' Get a Tier
	#'
	#' This function returns a dataframe or json object of a Tier
	#' @param TierID The ID of the Tier to return.
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given Tier. Defaults to FALSE for all return fields which, for convenience, returns all fields for the Tier.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object listed as text. Run \code{\link{getSchemaForObjects}}('Tier') to get more field paths.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param query Queries to be added to the request. Ex. list(SchoolYearID = 1). Defaults to NULL.
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept MTSS
	#' @return A dataframe or of Tier
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	getTier <- function(TierID, Code = F, Description = F, Level = F, DistrictID = F, CodeDescription = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, fieldPaths = NULL, entityId = 1, query = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment()) %>% purrr::keep(names(.) != "TierID")

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		getSkyObject(module = "MTSS", objectName = "Tier", objectId = TierID, searchFields = searchFields, entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' Delete a Tier
	#'
	#' This function deletes a Tier
	#' @param TierID The ID of the Tier to delete
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param query Queries to be added to the request. Ex. list(SchoolYearID = 1). Defaults to NULL.
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept MTSS
	#' @return The TierID of the deleted Tier.
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	deleteTier <- function(TierID, ignoreWarnings = F, entityId = 1, query = NULL, flatten = T, returnResponse = F){

		deleteSkyObject(module = "MTSS", objectName = "Tier", objectId = TierID, ignoreWarnings = ignoreWarnings, entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' Create a Tier
	#'
	#' This function creates a Tier
	#' @param fieldNames The field values to give the created Tier. Each defaults to NULL.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param query Queries to be added to the request. Ex. list(SchoolYearID = 1). Defaults to NULL.
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept MTSS
	#' @return A newly created Tier
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	createTier <- function(Code = NULL, Description = NULL, Level = NULL, DistrictID = NULL, entityId = 1, query = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment())

		body <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper()) %>% purrr::compact()

		createSkyObject(module = "MTSS", objectName = "Tier", body = list(DataObject = body), searchFields = append("TierID", body %>% names()), entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' Modify a Tier
	#'
	#' This function modifies a Tier
	#' @param fieldNames The field values to give the modified Tier. Each defaults to NULL.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param query Queries to be added to the request. Ex. list(SchoolYearID = 1). Defaults to NULL.
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept MTSS
	#' @return The modified Tier
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	modifyTier <- function(TierID, Code = NULL, Description = NULL, Level = NULL, DistrictID = NULL, entityId = 1, query = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment())

		body <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper()) %>% purrr::compact()

		modifySkyObject(module = "MTSS", objectName = "Tier", objectId = TierID, body = list(DataObject = body), searchFields = append("TierID", body %>% names()), entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' List StudentInterventionReferrals
	#'
	#' This function returns a dataframe or json object of StudentInterventionReferrals
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given StudentInterventionReferrals. Defaults to FALSE for all return fields which, for convenience, returns all fields for the StudentInterventionReferrals.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object listed as text. Run \code{\link{getSchemaForObjects}}('StudentInterventionReferral') to get more field paths.
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
	#' @concept MTSS
	#' @return A list of StudentInterventionReferrals
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	listStudentInterventionReferrals <- function(searchConditionsList = NULL, StudentInterventionReferralID = F, StudentID = F, AreaOfConcernID = F, Note = F, ReferralDate = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, AttachmentCount = F, WatchlistID = F, NameIDAssignedTo = F, ApprovedBeforeLastEffectiveRun = F, StatusChangedBeforeLastEffectiveRun = F, CreatedBeforeLastEffectiveRun = F, fieldPaths = NULL, searchConditionsGroupType = "And", searchSortFieldNamesList = NULL, searchSortFieldNamesDescendingList = NULL, entityId = 1, query = NULL, page = 1, pageSize = 100000, flatten = T, returnResponse = F){

		params <- as.list(environment())

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		listSkyObjects(module = "MTSS", objectName = "StudentInterventionReferral", searchFields = searchFields %>% append(fieldPaths), page = page, pageSize = pageSize, SearchConditionsList = searchConditionsList, SearchConditionsGroupType = searchConditionsGroupType, SearchSortFieldNamesList = searchSortFieldNamesList, SearchSortFieldNamesDescendingList = searchSortFieldNamesDescendingList, entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' Get a StudentInterventionReferral
	#'
	#' This function returns a dataframe or json object of a StudentInterventionReferral
	#' @param StudentInterventionReferralID The ID of the StudentInterventionReferral to return.
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given StudentInterventionReferral. Defaults to FALSE for all return fields which, for convenience, returns all fields for the StudentInterventionReferral.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object listed as text. Run \code{\link{getSchemaForObjects}}('StudentInterventionReferral') to get more field paths.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param query Queries to be added to the request. Ex. list(SchoolYearID = 1). Defaults to NULL.
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept MTSS
	#' @return A dataframe or of StudentInterventionReferral
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	getStudentInterventionReferral <- function(StudentInterventionReferralID, StudentID = F, AreaOfConcernID = F, Note = F, ReferralDate = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, AttachmentCount = F, WatchlistID = F, NameIDAssignedTo = F, ApprovedBeforeLastEffectiveRun = F, StatusChangedBeforeLastEffectiveRun = F, CreatedBeforeLastEffectiveRun = F, fieldPaths = NULL, entityId = 1, query = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment()) %>% purrr::keep(names(.) != "StudentInterventionReferralID")

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		getSkyObject(module = "MTSS", objectName = "StudentInterventionReferral", objectId = StudentInterventionReferralID, searchFields = searchFields, entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' Delete a StudentInterventionReferral
	#'
	#' This function deletes a StudentInterventionReferral
	#' @param StudentInterventionReferralID The ID of the StudentInterventionReferral to delete
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param query Queries to be added to the request. Ex. list(SchoolYearID = 1). Defaults to NULL.
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept MTSS
	#' @return The StudentInterventionReferralID of the deleted StudentInterventionReferral.
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	deleteStudentInterventionReferral <- function(StudentInterventionReferralID, ignoreWarnings = F, entityId = 1, query = NULL, flatten = T, returnResponse = F){

		deleteSkyObject(module = "MTSS", objectName = "StudentInterventionReferral", objectId = StudentInterventionReferralID, ignoreWarnings = ignoreWarnings, entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' Create a StudentInterventionReferral
	#'
	#' This function creates a StudentInterventionReferral
	#' @param fieldNames The field values to give the created StudentInterventionReferral. Each defaults to NULL.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param query Queries to be added to the request. Ex. list(SchoolYearID = 1). Defaults to NULL.
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept MTSS
	#' @return A newly created StudentInterventionReferral
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	createStudentInterventionReferral <- function(StudentID = NULL, AreaOfConcernID = NULL, Note = NULL, ReferralDate = NULL, WatchlistID = NULL, NameIDAssignedTo = NULL, entityId = 1, query = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment())

		body <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper()) %>% purrr::compact()

		createSkyObject(module = "MTSS", objectName = "StudentInterventionReferral", body = list(DataObject = body), searchFields = append("StudentInterventionReferralID", body %>% names()), entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' Modify a StudentInterventionReferral
	#'
	#' This function modifies a StudentInterventionReferral
	#' @param fieldNames The field values to give the modified StudentInterventionReferral. Each defaults to NULL.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param query Queries to be added to the request. Ex. list(SchoolYearID = 1). Defaults to NULL.
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept MTSS
	#' @return The modified StudentInterventionReferral
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	modifyStudentInterventionReferral <- function(StudentInterventionReferralID, StudentID = NULL, AreaOfConcernID = NULL, Note = NULL, ReferralDate = NULL, WatchlistID = NULL, NameIDAssignedTo = NULL, entityId = 1, query = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment())

		body <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper()) %>% purrr::compact()

		modifySkyObject(module = "MTSS", objectName = "StudentInterventionReferral", objectId = StudentInterventionReferralID, body = list(DataObject = body), searchFields = append("StudentInterventionReferralID", body %>% names()), entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' List StudentInterventionReferralObservations
	#'
	#' This function returns a dataframe or json object of StudentInterventionReferralObservations
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given StudentInterventionReferralObservations. Defaults to FALSE for all return fields which, for convenience, returns all fields for the StudentInterventionReferralObservations.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object listed as text. Run \code{\link{getSchemaForObjects}}('StudentInterventionReferralObservation') to get more field paths.
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
	#' @concept MTSS
	#' @return A list of StudentInterventionReferralObservations
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	listStudentInterventionReferralObservations <- function(searchConditionsList = NULL, StudentInterventionReferralObservationID = F, StudentInterventionReferralID = F, ObservationID = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, fieldPaths = NULL, searchConditionsGroupType = "And", searchSortFieldNamesList = NULL, searchSortFieldNamesDescendingList = NULL, entityId = 1, query = NULL, page = 1, pageSize = 100000, flatten = T, returnResponse = F){

		params <- as.list(environment())

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		listSkyObjects(module = "MTSS", objectName = "StudentInterventionReferralObservation", searchFields = searchFields %>% append(fieldPaths), page = page, pageSize = pageSize, SearchConditionsList = searchConditionsList, SearchConditionsGroupType = searchConditionsGroupType, SearchSortFieldNamesList = searchSortFieldNamesList, SearchSortFieldNamesDescendingList = searchSortFieldNamesDescendingList, entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' Get a StudentInterventionReferralObservation
	#'
	#' This function returns a dataframe or json object of a StudentInterventionReferralObservation
	#' @param StudentInterventionReferralObservationID The ID of the StudentInterventionReferralObservation to return.
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given StudentInterventionReferralObservation. Defaults to FALSE for all return fields which, for convenience, returns all fields for the StudentInterventionReferralObservation.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object listed as text. Run \code{\link{getSchemaForObjects}}('StudentInterventionReferralObservation') to get more field paths.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param query Queries to be added to the request. Ex. list(SchoolYearID = 1). Defaults to NULL.
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept MTSS
	#' @return A dataframe or of StudentInterventionReferralObservation
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	getStudentInterventionReferralObservation <- function(StudentInterventionReferralObservationID, StudentInterventionReferralID = F, ObservationID = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, fieldPaths = NULL, entityId = 1, query = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment()) %>% purrr::keep(names(.) != "StudentInterventionReferralObservationID")

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		getSkyObject(module = "MTSS", objectName = "StudentInterventionReferralObservation", objectId = StudentInterventionReferralObservationID, searchFields = searchFields, entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' Delete a StudentInterventionReferralObservation
	#'
	#' This function deletes a StudentInterventionReferralObservation
	#' @param StudentInterventionReferralObservationID The ID of the StudentInterventionReferralObservation to delete
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param query Queries to be added to the request. Ex. list(SchoolYearID = 1). Defaults to NULL.
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept MTSS
	#' @return The StudentInterventionReferralObservationID of the deleted StudentInterventionReferralObservation.
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	deleteStudentInterventionReferralObservation <- function(StudentInterventionReferralObservationID, ignoreWarnings = F, entityId = 1, query = NULL, flatten = T, returnResponse = F){

		deleteSkyObject(module = "MTSS", objectName = "StudentInterventionReferralObservation", objectId = StudentInterventionReferralObservationID, ignoreWarnings = ignoreWarnings, entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' Create a StudentInterventionReferralObservation
	#'
	#' This function creates a StudentInterventionReferralObservation
	#' @param fieldNames The field values to give the created StudentInterventionReferralObservation. Each defaults to NULL.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param query Queries to be added to the request. Ex. list(SchoolYearID = 1). Defaults to NULL.
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept MTSS
	#' @return A newly created StudentInterventionReferralObservation
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	createStudentInterventionReferralObservation <- function(StudentInterventionReferralID = NULL, ObservationID = NULL, entityId = 1, query = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment())

		body <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper()) %>% purrr::compact()

		createSkyObject(module = "MTSS", objectName = "StudentInterventionReferralObservation", body = list(DataObject = body), searchFields = append("StudentInterventionReferralObservationID", body %>% names()), entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' Modify a StudentInterventionReferralObservation
	#'
	#' This function modifies a StudentInterventionReferralObservation
	#' @param fieldNames The field values to give the modified StudentInterventionReferralObservation. Each defaults to NULL.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param query Queries to be added to the request. Ex. list(SchoolYearID = 1). Defaults to NULL.
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept MTSS
	#' @return The modified StudentInterventionReferralObservation
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	modifyStudentInterventionReferralObservation <- function(StudentInterventionReferralObservationID, StudentInterventionReferralID = NULL, ObservationID = NULL, entityId = 1, query = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment())

		body <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper()) %>% purrr::compact()

		modifySkyObject(module = "MTSS", objectName = "StudentInterventionReferralObservation", objectId = StudentInterventionReferralObservationID, body = list(DataObject = body), searchFields = append("StudentInterventionReferralObservationID", body %>% names()), entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' List StudentInterventionReferralStatuses
	#'
	#' This function returns a dataframe or json object of StudentInterventionReferralStatuses
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given StudentInterventionReferralStatuses. Defaults to FALSE for all return fields which, for convenience, returns all fields for the StudentInterventionReferralStatuses.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object listed as text. Run \code{\link{getSchemaForObjects}}('StudentInterventionReferralStatus') to get more field paths.
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
	#' @concept MTSS
	#' @return A list of StudentInterventionReferralStatuses
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	listStudentInterventionReferralStatuses <- function(searchConditionsList = NULL, StudentInterventionReferralStatusID = F, StudentInterventionReferralID = F, Note = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, ReferralStatus = F, ReferralDateTime = F, fieldPaths = NULL, searchConditionsGroupType = "And", searchSortFieldNamesList = NULL, searchSortFieldNamesDescendingList = NULL, entityId = 1, query = NULL, page = 1, pageSize = 100000, flatten = T, returnResponse = F){

		params <- as.list(environment())

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		listSkyObjects(module = "MTSS", objectName = "StudentInterventionReferralStatus", searchFields = searchFields %>% append(fieldPaths), page = page, pageSize = pageSize, SearchConditionsList = searchConditionsList, SearchConditionsGroupType = searchConditionsGroupType, SearchSortFieldNamesList = searchSortFieldNamesList, SearchSortFieldNamesDescendingList = searchSortFieldNamesDescendingList, entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' Get a StudentInterventionReferralStatus
	#'
	#' This function returns a dataframe or json object of a StudentInterventionReferralStatus
	#' @param StudentInterventionReferralStatusID The ID of the StudentInterventionReferralStatus to return.
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given StudentInterventionReferralStatus. Defaults to FALSE for all return fields which, for convenience, returns all fields for the StudentInterventionReferralStatus.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object listed as text. Run \code{\link{getSchemaForObjects}}('StudentInterventionReferralStatus') to get more field paths.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param query Queries to be added to the request. Ex. list(SchoolYearID = 1). Defaults to NULL.
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept MTSS
	#' @return A dataframe or of StudentInterventionReferralStatus
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	getStudentInterventionReferralStatus <- function(StudentInterventionReferralStatusID, StudentInterventionReferralID = F, Note = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, ReferralStatus = F, ReferralDateTime = F, fieldPaths = NULL, entityId = 1, query = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment()) %>% purrr::keep(names(.) != "StudentInterventionReferralStatusID")

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		getSkyObject(module = "MTSS", objectName = "StudentInterventionReferralStatus", objectId = StudentInterventionReferralStatusID, searchFields = searchFields, entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' Delete a StudentInterventionReferralStatus
	#'
	#' This function deletes a StudentInterventionReferralStatus
	#' @param StudentInterventionReferralStatusID The ID of the StudentInterventionReferralStatus to delete
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param query Queries to be added to the request. Ex. list(SchoolYearID = 1). Defaults to NULL.
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept MTSS
	#' @return The StudentInterventionReferralStatusID of the deleted StudentInterventionReferralStatus.
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	deleteStudentInterventionReferralStatus <- function(StudentInterventionReferralStatusID, ignoreWarnings = F, entityId = 1, query = NULL, flatten = T, returnResponse = F){

		deleteSkyObject(module = "MTSS", objectName = "StudentInterventionReferralStatus", objectId = StudentInterventionReferralStatusID, ignoreWarnings = ignoreWarnings, entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' Create a StudentInterventionReferralStatus
	#'
	#' This function creates a StudentInterventionReferralStatus
	#' @param fieldNames The field values to give the created StudentInterventionReferralStatus. Each defaults to NULL.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param query Queries to be added to the request. Ex. list(SchoolYearID = 1). Defaults to NULL.
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept MTSS
	#' @return A newly created StudentInterventionReferralStatus
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	createStudentInterventionReferralStatus <- function(StudentInterventionReferralID = NULL, Note = NULL, ReferralStatus = NULL, ReferralDateTime = NULL, entityId = 1, query = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment())

		body <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper()) %>% purrr::compact()

		createSkyObject(module = "MTSS", objectName = "StudentInterventionReferralStatus", body = list(DataObject = body), searchFields = append("StudentInterventionReferralStatusID", body %>% names()), entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' Modify a StudentInterventionReferralStatus
	#'
	#' This function modifies a StudentInterventionReferralStatus
	#' @param fieldNames The field values to give the modified StudentInterventionReferralStatus. Each defaults to NULL.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param query Queries to be added to the request. Ex. list(SchoolYearID = 1). Defaults to NULL.
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept MTSS
	#' @return The modified StudentInterventionReferralStatus
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	modifyStudentInterventionReferralStatus <- function(StudentInterventionReferralStatusID, StudentInterventionReferralID = NULL, Note = NULL, ReferralStatus = NULL, ReferralDateTime = NULL, entityId = 1, query = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment())

		body <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper()) %>% purrr::compact()

		modifySkyObject(module = "MTSS", objectName = "StudentInterventionReferralStatus", objectId = StudentInterventionReferralStatusID, body = list(DataObject = body), searchFields = append("StudentInterventionReferralStatusID", body %>% names()), entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' List InterventionTeams
	#'
	#' This function returns a dataframe or json object of InterventionTeams
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given InterventionTeams. Defaults to FALSE for all return fields which, for convenience, returns all fields for the InterventionTeams.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object listed as text. Run \code{\link{getSchemaForObjects}}('InterventionTeam') to get more field paths.
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
	#' @concept MTSS
	#' @return A list of InterventionTeams
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	listInterventionTeams <- function(searchConditionsList = NULL, InterventionTeamID = F, Name = F, Description = F, IsActive = F, DistrictID = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, fieldPaths = NULL, searchConditionsGroupType = "And", searchSortFieldNamesList = NULL, searchSortFieldNamesDescendingList = NULL, entityId = 1, query = NULL, page = 1, pageSize = 100000, flatten = T, returnResponse = F){

		params <- as.list(environment())

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		listSkyObjects(module = "MTSS", objectName = "InterventionTeam", searchFields = searchFields %>% append(fieldPaths), page = page, pageSize = pageSize, SearchConditionsList = searchConditionsList, SearchConditionsGroupType = searchConditionsGroupType, SearchSortFieldNamesList = searchSortFieldNamesList, SearchSortFieldNamesDescendingList = searchSortFieldNamesDescendingList, entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' Get an InterventionTeam
	#'
	#' This function returns a dataframe or json object of an InterventionTeam
	#' @param InterventionTeamID The ID of the InterventionTeam to return.
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given InterventionTeam. Defaults to FALSE for all return fields which, for convenience, returns all fields for the InterventionTeam.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object listed as text. Run \code{\link{getSchemaForObjects}}('InterventionTeam') to get more field paths.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param query Queries to be added to the request. Ex. list(SchoolYearID = 1). Defaults to NULL.
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept MTSS
	#' @return A dataframe or of InterventionTeam
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	getInterventionTeam <- function(InterventionTeamID, Name = F, Description = F, IsActive = F, DistrictID = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, fieldPaths = NULL, entityId = 1, query = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment()) %>% purrr::keep(names(.) != "InterventionTeamID")

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		getSkyObject(module = "MTSS", objectName = "InterventionTeam", objectId = InterventionTeamID, searchFields = searchFields, entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' Delete an InterventionTeam
	#'
	#' This function deletes an InterventionTeam
	#' @param InterventionTeamID The ID of the InterventionTeam to delete
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param query Queries to be added to the request. Ex. list(SchoolYearID = 1). Defaults to NULL.
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept MTSS
	#' @return The InterventionTeamID of the deleted InterventionTeam.
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	deleteInterventionTeam <- function(InterventionTeamID, ignoreWarnings = F, entityId = 1, query = NULL, flatten = T, returnResponse = F){

		deleteSkyObject(module = "MTSS", objectName = "InterventionTeam", objectId = InterventionTeamID, ignoreWarnings = ignoreWarnings, entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' Create an InterventionTeam
	#'
	#' This function creates an InterventionTeam
	#' @param fieldNames The field values to give the created InterventionTeam. Each defaults to NULL.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param query Queries to be added to the request. Ex. list(SchoolYearID = 1). Defaults to NULL.
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept MTSS
	#' @return A newly created InterventionTeam
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	createInterventionTeam <- function(Name = NULL, Description = NULL, IsActive = NULL, DistrictID = NULL, entityId = 1, query = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment())

		body <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper()) %>% purrr::compact()

		createSkyObject(module = "MTSS", objectName = "InterventionTeam", body = list(DataObject = body), searchFields = append("InterventionTeamID", body %>% names()), entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' Modify an InterventionTeam
	#'
	#' This function modifies an InterventionTeam
	#' @param fieldNames The field values to give the modified InterventionTeam. Each defaults to NULL.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param query Queries to be added to the request. Ex. list(SchoolYearID = 1). Defaults to NULL.
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept MTSS
	#' @return The modified InterventionTeam
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	modifyInterventionTeam <- function(InterventionTeamID, Name = NULL, Description = NULL, IsActive = NULL, DistrictID = NULL, entityId = 1, query = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment())

		body <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper()) %>% purrr::compact()

		modifySkyObject(module = "MTSS", objectName = "InterventionTeam", objectId = InterventionTeamID, body = list(DataObject = body), searchFields = append("InterventionTeamID", body %>% names()), entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' List InterventionTeamStaffs
	#'
	#' This function returns a dataframe or json object of InterventionTeamStaffs
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given InterventionTeamStaffs. Defaults to FALSE for all return fields which, for convenience, returns all fields for the InterventionTeamStaffs.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object listed as text. Run \code{\link{getSchemaForObjects}}('InterventionTeamStaff') to get more field paths.
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
	#' @concept MTSS
	#' @return A list of InterventionTeamStaffs
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	listInterventionTeamStaffs <- function(searchConditionsList = NULL, InterventionTeamStaffID = F, InterventionTeamID = F, StaffID = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, IsActive = F, fieldPaths = NULL, searchConditionsGroupType = "And", searchSortFieldNamesList = NULL, searchSortFieldNamesDescendingList = NULL, entityId = 1, query = NULL, page = 1, pageSize = 100000, flatten = T, returnResponse = F){

		params <- as.list(environment())

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		listSkyObjects(module = "MTSS", objectName = "InterventionTeamStaff", searchFields = searchFields %>% append(fieldPaths), page = page, pageSize = pageSize, SearchConditionsList = searchConditionsList, SearchConditionsGroupType = searchConditionsGroupType, SearchSortFieldNamesList = searchSortFieldNamesList, SearchSortFieldNamesDescendingList = searchSortFieldNamesDescendingList, entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' Get an InterventionTeamStaff
	#'
	#' This function returns a dataframe or json object of an InterventionTeamStaff
	#' @param InterventionTeamStaffID The ID of the InterventionTeamStaff to return.
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given InterventionTeamStaff. Defaults to FALSE for all return fields which, for convenience, returns all fields for the InterventionTeamStaff.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object listed as text. Run \code{\link{getSchemaForObjects}}('InterventionTeamStaff') to get more field paths.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param query Queries to be added to the request. Ex. list(SchoolYearID = 1). Defaults to NULL.
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept MTSS
	#' @return A dataframe or of InterventionTeamStaff
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	getInterventionTeamStaff <- function(InterventionTeamStaffID, InterventionTeamID = F, StaffID = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, IsActive = F, fieldPaths = NULL, entityId = 1, query = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment()) %>% purrr::keep(names(.) != "InterventionTeamStaffID")

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		getSkyObject(module = "MTSS", objectName = "InterventionTeamStaff", objectId = InterventionTeamStaffID, searchFields = searchFields, entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' Delete an InterventionTeamStaff
	#'
	#' This function deletes an InterventionTeamStaff
	#' @param InterventionTeamStaffID The ID of the InterventionTeamStaff to delete
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param query Queries to be added to the request. Ex. list(SchoolYearID = 1). Defaults to NULL.
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept MTSS
	#' @return The InterventionTeamStaffID of the deleted InterventionTeamStaff.
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	deleteInterventionTeamStaff <- function(InterventionTeamStaffID, ignoreWarnings = F, entityId = 1, query = NULL, flatten = T, returnResponse = F){

		deleteSkyObject(module = "MTSS", objectName = "InterventionTeamStaff", objectId = InterventionTeamStaffID, ignoreWarnings = ignoreWarnings, entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' Create an InterventionTeamStaff
	#'
	#' This function creates an InterventionTeamStaff
	#' @param fieldNames The field values to give the created InterventionTeamStaff. Each defaults to NULL.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param query Queries to be added to the request. Ex. list(SchoolYearID = 1). Defaults to NULL.
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept MTSS
	#' @return A newly created InterventionTeamStaff
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	createInterventionTeamStaff <- function(InterventionTeamID = NULL, StaffID = NULL, IsActive = NULL, entityId = 1, query = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment())

		body <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper()) %>% purrr::compact()

		createSkyObject(module = "MTSS", objectName = "InterventionTeamStaff", body = list(DataObject = body), searchFields = append("InterventionTeamStaffID", body %>% names()), entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' Modify an InterventionTeamStaff
	#'
	#' This function modifies an InterventionTeamStaff
	#' @param fieldNames The field values to give the modified InterventionTeamStaff. Each defaults to NULL.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param query Queries to be added to the request. Ex. list(SchoolYearID = 1). Defaults to NULL.
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept MTSS
	#' @return The modified InterventionTeamStaff
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	modifyInterventionTeamStaff <- function(InterventionTeamStaffID, InterventionTeamID = NULL, StaffID = NULL, IsActive = NULL, entityId = 1, query = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment())

		body <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper()) %>% purrr::compact()

		modifySkyObject(module = "MTSS", objectName = "InterventionTeamStaff", objectId = InterventionTeamStaffID, body = list(DataObject = body), searchFields = append("InterventionTeamStaffID", body %>% names()), entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' List InterventionPlans
	#'
	#' This function returns a dataframe or json object of InterventionPlans
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given InterventionPlans. Defaults to FALSE for all return fields which, for convenience, returns all fields for the InterventionPlans.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object listed as text. Run \code{\link{getSchemaForObjects}}('InterventionPlan') to get more field paths.
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
	#' @concept MTSS
	#' @return A list of InterventionPlans
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	listInterventionPlans <- function(searchConditionsList = NULL, InterventionPlanID = F, StudentID = F, TierID = F, OutcomeID = F, GoalFreeform = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, StartDate = F, EndDate = F, AreaOfConcernID = F, ObjectiveID = F, StartingGoal = F, MeasurementID = F, StartingGoalDisplay = F, TargetGoal = F, TargetGoalDisplay = F, StudentInterventionReferralID = F, CurrentUserHasPermission = F, AttachmentCount = F, ReviewDate = F, QualitativeScaleMappingIDStarting = F, QualitativeScaleMappingIDTarget = F, WatchlistID = F, fieldPaths = NULL, searchConditionsGroupType = "And", searchSortFieldNamesList = NULL, searchSortFieldNamesDescendingList = NULL, entityId = 1, query = NULL, page = 1, pageSize = 100000, flatten = T, returnResponse = F){

		params <- as.list(environment())

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		listSkyObjects(module = "MTSS", objectName = "InterventionPlan", searchFields = searchFields %>% append(fieldPaths), page = page, pageSize = pageSize, SearchConditionsList = searchConditionsList, SearchConditionsGroupType = searchConditionsGroupType, SearchSortFieldNamesList = searchSortFieldNamesList, SearchSortFieldNamesDescendingList = searchSortFieldNamesDescendingList, entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' Get an InterventionPlan
	#'
	#' This function returns a dataframe or json object of an InterventionPlan
	#' @param InterventionPlanID The ID of the InterventionPlan to return.
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given InterventionPlan. Defaults to FALSE for all return fields which, for convenience, returns all fields for the InterventionPlan.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object listed as text. Run \code{\link{getSchemaForObjects}}('InterventionPlan') to get more field paths.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param query Queries to be added to the request. Ex. list(SchoolYearID = 1). Defaults to NULL.
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept MTSS
	#' @return A dataframe or of InterventionPlan
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	getInterventionPlan <- function(InterventionPlanID, StudentID = F, TierID = F, OutcomeID = F, GoalFreeform = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, StartDate = F, EndDate = F, AreaOfConcernID = F, ObjectiveID = F, StartingGoal = F, MeasurementID = F, StartingGoalDisplay = F, TargetGoal = F, TargetGoalDisplay = F, StudentInterventionReferralID = F, CurrentUserHasPermission = F, AttachmentCount = F, ReviewDate = F, QualitativeScaleMappingIDStarting = F, QualitativeScaleMappingIDTarget = F, WatchlistID = F, fieldPaths = NULL, entityId = 1, query = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment()) %>% purrr::keep(names(.) != "InterventionPlanID")

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		getSkyObject(module = "MTSS", objectName = "InterventionPlan", objectId = InterventionPlanID, searchFields = searchFields, entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' Delete an InterventionPlan
	#'
	#' This function deletes an InterventionPlan
	#' @param InterventionPlanID The ID of the InterventionPlan to delete
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param query Queries to be added to the request. Ex. list(SchoolYearID = 1). Defaults to NULL.
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept MTSS
	#' @return The InterventionPlanID of the deleted InterventionPlan.
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	deleteInterventionPlan <- function(InterventionPlanID, ignoreWarnings = F, entityId = 1, query = NULL, flatten = T, returnResponse = F){

		deleteSkyObject(module = "MTSS", objectName = "InterventionPlan", objectId = InterventionPlanID, ignoreWarnings = ignoreWarnings, entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' Create an InterventionPlan
	#'
	#' This function creates an InterventionPlan
	#' @param fieldNames The field values to give the created InterventionPlan. Each defaults to NULL.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param query Queries to be added to the request. Ex. list(SchoolYearID = 1). Defaults to NULL.
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept MTSS
	#' @return A newly created InterventionPlan
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	createInterventionPlan <- function(StudentID = NULL, TierID = NULL, OutcomeID = NULL, GoalFreeform = NULL, StartDate = NULL, EndDate = NULL, AreaOfConcernID = NULL, ObjectiveID = NULL, StartingGoal = NULL, MeasurementID = NULL, TargetGoal = NULL, StudentInterventionReferralID = NULL, ReviewDate = NULL, QualitativeScaleMappingIDStarting = NULL, QualitativeScaleMappingIDTarget = NULL, WatchlistID = NULL, entityId = 1, query = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment())

		body <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper()) %>% purrr::compact()

		createSkyObject(module = "MTSS", objectName = "InterventionPlan", body = list(DataObject = body), searchFields = append("InterventionPlanID", body %>% names()), entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' Modify an InterventionPlan
	#'
	#' This function modifies an InterventionPlan
	#' @param fieldNames The field values to give the modified InterventionPlan. Each defaults to NULL.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param query Queries to be added to the request. Ex. list(SchoolYearID = 1). Defaults to NULL.
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept MTSS
	#' @return The modified InterventionPlan
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	modifyInterventionPlan <- function(InterventionPlanID, StudentID = NULL, TierID = NULL, OutcomeID = NULL, GoalFreeform = NULL, StartDate = NULL, EndDate = NULL, AreaOfConcernID = NULL, ObjectiveID = NULL, StartingGoal = NULL, MeasurementID = NULL, TargetGoal = NULL, StudentInterventionReferralID = NULL, ReviewDate = NULL, QualitativeScaleMappingIDStarting = NULL, QualitativeScaleMappingIDTarget = NULL, WatchlistID = NULL, entityId = 1, query = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment())

		body <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper()) %>% purrr::compact()

		modifySkyObject(module = "MTSS", objectName = "InterventionPlan", objectId = InterventionPlanID, body = list(DataObject = body), searchFields = append("InterventionPlanID", body %>% names()), entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' List InterventionPlanStaffs
	#'
	#' This function returns a dataframe or json object of InterventionPlanStaffs
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given InterventionPlanStaffs. Defaults to FALSE for all return fields which, for convenience, returns all fields for the InterventionPlanStaffs.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object listed as text. Run \code{\link{getSchemaForObjects}}('InterventionPlanStaff') to get more field paths.
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
	#' @concept MTSS
	#' @return A list of InterventionPlanStaffs
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	listInterventionPlanStaffs <- function(searchConditionsList = NULL, InterventionPlanStaffID = F, InterventionPlanID = F, StaffID = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, InterventionTeamStaffID = F, IsActive = F, IsProvidedBy = F, StaffAddedBeforeLastEffectiveRun = F, UserAddedSelf = F, fieldPaths = NULL, searchConditionsGroupType = "And", searchSortFieldNamesList = NULL, searchSortFieldNamesDescendingList = NULL, entityId = 1, query = NULL, page = 1, pageSize = 100000, flatten = T, returnResponse = F){

		params <- as.list(environment())

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		listSkyObjects(module = "MTSS", objectName = "InterventionPlanStaff", searchFields = searchFields %>% append(fieldPaths), page = page, pageSize = pageSize, SearchConditionsList = searchConditionsList, SearchConditionsGroupType = searchConditionsGroupType, SearchSortFieldNamesList = searchSortFieldNamesList, SearchSortFieldNamesDescendingList = searchSortFieldNamesDescendingList, entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' Get an InterventionPlanStaff
	#'
	#' This function returns a dataframe or json object of an InterventionPlanStaff
	#' @param InterventionPlanStaffID The ID of the InterventionPlanStaff to return.
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given InterventionPlanStaff. Defaults to FALSE for all return fields which, for convenience, returns all fields for the InterventionPlanStaff.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object listed as text. Run \code{\link{getSchemaForObjects}}('InterventionPlanStaff') to get more field paths.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param query Queries to be added to the request. Ex. list(SchoolYearID = 1). Defaults to NULL.
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept MTSS
	#' @return A dataframe or of InterventionPlanStaff
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	getInterventionPlanStaff <- function(InterventionPlanStaffID, InterventionPlanID = F, StaffID = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, InterventionTeamStaffID = F, IsActive = F, IsProvidedBy = F, StaffAddedBeforeLastEffectiveRun = F, UserAddedSelf = F, fieldPaths = NULL, entityId = 1, query = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment()) %>% purrr::keep(names(.) != "InterventionPlanStaffID")

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		getSkyObject(module = "MTSS", objectName = "InterventionPlanStaff", objectId = InterventionPlanStaffID, searchFields = searchFields, entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' Delete an InterventionPlanStaff
	#'
	#' This function deletes an InterventionPlanStaff
	#' @param InterventionPlanStaffID The ID of the InterventionPlanStaff to delete
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param query Queries to be added to the request. Ex. list(SchoolYearID = 1). Defaults to NULL.
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept MTSS
	#' @return The InterventionPlanStaffID of the deleted InterventionPlanStaff.
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	deleteInterventionPlanStaff <- function(InterventionPlanStaffID, ignoreWarnings = F, entityId = 1, query = NULL, flatten = T, returnResponse = F){

		deleteSkyObject(module = "MTSS", objectName = "InterventionPlanStaff", objectId = InterventionPlanStaffID, ignoreWarnings = ignoreWarnings, entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' Create an InterventionPlanStaff
	#'
	#' This function creates an InterventionPlanStaff
	#' @param fieldNames The field values to give the created InterventionPlanStaff. Each defaults to NULL.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param query Queries to be added to the request. Ex. list(SchoolYearID = 1). Defaults to NULL.
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept MTSS
	#' @return A newly created InterventionPlanStaff
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	createInterventionPlanStaff <- function(InterventionPlanID = NULL, StaffID = NULL, InterventionTeamStaffID = NULL, IsActive = NULL, IsProvidedBy = NULL, entityId = 1, query = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment())

		body <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper()) %>% purrr::compact()

		createSkyObject(module = "MTSS", objectName = "InterventionPlanStaff", body = list(DataObject = body), searchFields = append("InterventionPlanStaffID", body %>% names()), entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' Modify an InterventionPlanStaff
	#'
	#' This function modifies an InterventionPlanStaff
	#' @param fieldNames The field values to give the modified InterventionPlanStaff. Each defaults to NULL.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param query Queries to be added to the request. Ex. list(SchoolYearID = 1). Defaults to NULL.
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept MTSS
	#' @return The modified InterventionPlanStaff
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	modifyInterventionPlanStaff <- function(InterventionPlanStaffID, InterventionPlanID = NULL, StaffID = NULL, InterventionTeamStaffID = NULL, IsActive = NULL, IsProvidedBy = NULL, entityId = 1, query = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment())

		body <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper()) %>% purrr::compact()

		modifySkyObject(module = "MTSS", objectName = "InterventionPlanStaff", objectId = InterventionPlanStaffID, body = list(DataObject = body), searchFields = append("InterventionPlanStaffID", body %>% names()), entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' List InterventionPlanStrategies
	#'
	#' This function returns a dataframe or json object of InterventionPlanStrategies
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given InterventionPlanStrategies. Defaults to FALSE for all return fields which, for convenience, returns all fields for the InterventionPlanStrategies.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object listed as text. Run \code{\link{getSchemaForObjects}}('InterventionPlanStrategy') to get more field paths.
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
	#' @concept MTSS
	#' @return A list of InterventionPlanStrategies
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	listInterventionPlanStrategies <- function(searchConditionsList = NULL, InterventionPlanStrategyID = F, InterventionPlanID = F, GoalFreeform = F, Duration = F, MeetingTime = F, StartDate = F, EndDate = F, Sunday = F, Monday = F, Tuesday = F, Wednesday = F, Thursday = F, Friday = F, Saturday = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, StrategyID = F, OutcomeID = F, StrategyGoal = F, StrategyGoalDisplay = F, Days = F, IsAdministrableAsOfSpecifiedDate = F, InterventionStaffIDByCurrentUser = F, MeetingTimeDisplay = F, Frequency = F, TimesToMeet = F, DayAbbreviations = F, QualitativeScaleMappingIDStrategy = F, fieldPaths = NULL, searchConditionsGroupType = "And", searchSortFieldNamesList = NULL, searchSortFieldNamesDescendingList = NULL, entityId = 1, query = NULL, page = 1, pageSize = 100000, flatten = T, returnResponse = F){

		params <- as.list(environment())

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		listSkyObjects(module = "MTSS", objectName = "InterventionPlanStrategy", searchFields = searchFields %>% append(fieldPaths), page = page, pageSize = pageSize, SearchConditionsList = searchConditionsList, SearchConditionsGroupType = searchConditionsGroupType, SearchSortFieldNamesList = searchSortFieldNamesList, SearchSortFieldNamesDescendingList = searchSortFieldNamesDescendingList, entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' Get an InterventionPlanStrategy
	#'
	#' This function returns a dataframe or json object of an InterventionPlanStrategy
	#' @param InterventionPlanStrategyID The ID of the InterventionPlanStrategy to return.
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given InterventionPlanStrategy. Defaults to FALSE for all return fields which, for convenience, returns all fields for the InterventionPlanStrategy.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object listed as text. Run \code{\link{getSchemaForObjects}}('InterventionPlanStrategy') to get more field paths.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param query Queries to be added to the request. Ex. list(SchoolYearID = 1). Defaults to NULL.
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept MTSS
	#' @return A dataframe or of InterventionPlanStrategy
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	getInterventionPlanStrategy <- function(InterventionPlanStrategyID, InterventionPlanID = F, GoalFreeform = F, Duration = F, MeetingTime = F, StartDate = F, EndDate = F, Sunday = F, Monday = F, Tuesday = F, Wednesday = F, Thursday = F, Friday = F, Saturday = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, StrategyID = F, OutcomeID = F, StrategyGoal = F, StrategyGoalDisplay = F, Days = F, IsAdministrableAsOfSpecifiedDate = F, InterventionStaffIDByCurrentUser = F, MeetingTimeDisplay = F, Frequency = F, TimesToMeet = F, DayAbbreviations = F, QualitativeScaleMappingIDStrategy = F, fieldPaths = NULL, entityId = 1, query = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment()) %>% purrr::keep(names(.) != "InterventionPlanStrategyID")

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		getSkyObject(module = "MTSS", objectName = "InterventionPlanStrategy", objectId = InterventionPlanStrategyID, searchFields = searchFields, entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' Delete an InterventionPlanStrategy
	#'
	#' This function deletes an InterventionPlanStrategy
	#' @param InterventionPlanStrategyID The ID of the InterventionPlanStrategy to delete
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param query Queries to be added to the request. Ex. list(SchoolYearID = 1). Defaults to NULL.
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept MTSS
	#' @return The InterventionPlanStrategyID of the deleted InterventionPlanStrategy.
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	deleteInterventionPlanStrategy <- function(InterventionPlanStrategyID, ignoreWarnings = F, entityId = 1, query = NULL, flatten = T, returnResponse = F){

		deleteSkyObject(module = "MTSS", objectName = "InterventionPlanStrategy", objectId = InterventionPlanStrategyID, ignoreWarnings = ignoreWarnings, entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' Create an InterventionPlanStrategy
	#'
	#' This function creates an InterventionPlanStrategy
	#' @param fieldNames The field values to give the created InterventionPlanStrategy. Each defaults to NULL.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param query Queries to be added to the request. Ex. list(SchoolYearID = 1). Defaults to NULL.
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept MTSS
	#' @return A newly created InterventionPlanStrategy
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	createInterventionPlanStrategy <- function(InterventionPlanID = NULL, GoalFreeform = NULL, Duration = NULL, MeetingTime = NULL, StartDate = NULL, EndDate = NULL, Sunday = NULL, Monday = NULL, Tuesday = NULL, Wednesday = NULL, Thursday = NULL, Friday = NULL, Saturday = NULL, StrategyID = NULL, OutcomeID = NULL, StrategyGoal = NULL, Frequency = NULL, TimesToMeet = NULL, QualitativeScaleMappingIDStrategy = NULL, entityId = 1, query = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment())

		body <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper()) %>% purrr::compact()

		createSkyObject(module = "MTSS", objectName = "InterventionPlanStrategy", body = list(DataObject = body), searchFields = append("InterventionPlanStrategyID", body %>% names()), entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' Modify an InterventionPlanStrategy
	#'
	#' This function modifies an InterventionPlanStrategy
	#' @param fieldNames The field values to give the modified InterventionPlanStrategy. Each defaults to NULL.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param query Queries to be added to the request. Ex. list(SchoolYearID = 1). Defaults to NULL.
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept MTSS
	#' @return The modified InterventionPlanStrategy
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	modifyInterventionPlanStrategy <- function(InterventionPlanStrategyID, InterventionPlanID = NULL, GoalFreeform = NULL, Duration = NULL, MeetingTime = NULL, StartDate = NULL, EndDate = NULL, Sunday = NULL, Monday = NULL, Tuesday = NULL, Wednesday = NULL, Thursday = NULL, Friday = NULL, Saturday = NULL, StrategyID = NULL, OutcomeID = NULL, StrategyGoal = NULL, Frequency = NULL, TimesToMeet = NULL, QualitativeScaleMappingIDStrategy = NULL, entityId = 1, query = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment())

		body <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper()) %>% purrr::compact()

		modifySkyObject(module = "MTSS", objectName = "InterventionPlanStrategy", objectId = InterventionPlanStrategyID, body = list(DataObject = body), searchFields = append("InterventionPlanStrategyID", body %>% names()), entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' List StrategyTiers
	#'
	#' This function returns a dataframe or json object of StrategyTiers
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given StrategyTiers. Defaults to FALSE for all return fields which, for convenience, returns all fields for the StrategyTiers.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object listed as text. Run \code{\link{getSchemaForObjects}}('StrategyTier') to get more field paths.
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
	#' @concept MTSS
	#' @return A list of StrategyTiers
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	listStrategyTiers <- function(searchConditionsList = NULL, StrategyTierID = F, StrategyID = F, TierID = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, fieldPaths = NULL, searchConditionsGroupType = "And", searchSortFieldNamesList = NULL, searchSortFieldNamesDescendingList = NULL, entityId = 1, query = NULL, page = 1, pageSize = 100000, flatten = T, returnResponse = F){

		params <- as.list(environment())

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		listSkyObjects(module = "MTSS", objectName = "StrategyTier", searchFields = searchFields %>% append(fieldPaths), page = page, pageSize = pageSize, SearchConditionsList = searchConditionsList, SearchConditionsGroupType = searchConditionsGroupType, SearchSortFieldNamesList = searchSortFieldNamesList, SearchSortFieldNamesDescendingList = searchSortFieldNamesDescendingList, entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' Get a StrategyTier
	#'
	#' This function returns a dataframe or json object of a StrategyTier
	#' @param StrategyTierID The ID of the StrategyTier to return.
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given StrategyTier. Defaults to FALSE for all return fields which, for convenience, returns all fields for the StrategyTier.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object listed as text. Run \code{\link{getSchemaForObjects}}('StrategyTier') to get more field paths.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param query Queries to be added to the request. Ex. list(SchoolYearID = 1). Defaults to NULL.
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept MTSS
	#' @return A dataframe or of StrategyTier
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	getStrategyTier <- function(StrategyTierID, StrategyID = F, TierID = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, fieldPaths = NULL, entityId = 1, query = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment()) %>% purrr::keep(names(.) != "StrategyTierID")

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		getSkyObject(module = "MTSS", objectName = "StrategyTier", objectId = StrategyTierID, searchFields = searchFields, entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' Delete a StrategyTier
	#'
	#' This function deletes a StrategyTier
	#' @param StrategyTierID The ID of the StrategyTier to delete
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param query Queries to be added to the request. Ex. list(SchoolYearID = 1). Defaults to NULL.
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept MTSS
	#' @return The StrategyTierID of the deleted StrategyTier.
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	deleteStrategyTier <- function(StrategyTierID, ignoreWarnings = F, entityId = 1, query = NULL, flatten = T, returnResponse = F){

		deleteSkyObject(module = "MTSS", objectName = "StrategyTier", objectId = StrategyTierID, ignoreWarnings = ignoreWarnings, entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' Create a StrategyTier
	#'
	#' This function creates a StrategyTier
	#' @param fieldNames The field values to give the created StrategyTier. Each defaults to NULL.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param query Queries to be added to the request. Ex. list(SchoolYearID = 1). Defaults to NULL.
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept MTSS
	#' @return A newly created StrategyTier
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	createStrategyTier <- function(StrategyID = NULL, TierID = NULL, entityId = 1, query = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment())

		body <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper()) %>% purrr::compact()

		createSkyObject(module = "MTSS", objectName = "StrategyTier", body = list(DataObject = body), searchFields = append("StrategyTierID", body %>% names()), entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' Modify a StrategyTier
	#'
	#' This function modifies a StrategyTier
	#' @param fieldNames The field values to give the modified StrategyTier. Each defaults to NULL.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param query Queries to be added to the request. Ex. list(SchoolYearID = 1). Defaults to NULL.
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept MTSS
	#' @return The modified StrategyTier
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	modifyStrategyTier <- function(StrategyTierID, StrategyID = NULL, TierID = NULL, entityId = 1, query = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment())

		body <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper()) %>% purrr::compact()

		modifySkyObject(module = "MTSS", objectName = "StrategyTier", objectId = StrategyTierID, body = list(DataObject = body), searchFields = append("StrategyTierID", body %>% names()), entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' List Strategies
	#'
	#' This function returns a dataframe or json object of Strategies
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given Strategies. Defaults to FALSE for all return fields which, for convenience, returns all fields for the Strategies.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object listed as text. Run \code{\link{getSchemaForObjects}}('Strategy') to get more field paths.
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
	#' @concept MTSS
	#' @return A list of Strategies
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	listStrategies <- function(searchConditionsList = NULL, StrategyID = F, Description = F, AreaOfConcernID = F, DistrictID = F, TierLabels = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, Note = F, AttachmentCount = F, IsActive = F, EntityLabels = F, fieldPaths = NULL, searchConditionsGroupType = "And", searchSortFieldNamesList = NULL, searchSortFieldNamesDescendingList = NULL, entityId = 1, query = NULL, page = 1, pageSize = 100000, flatten = T, returnResponse = F){

		params <- as.list(environment())

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		listSkyObjects(module = "MTSS", objectName = "Strategy", searchFields = searchFields %>% append(fieldPaths), page = page, pageSize = pageSize, SearchConditionsList = searchConditionsList, SearchConditionsGroupType = searchConditionsGroupType, SearchSortFieldNamesList = searchSortFieldNamesList, SearchSortFieldNamesDescendingList = searchSortFieldNamesDescendingList, entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' Get a Strategy
	#'
	#' This function returns a dataframe or json object of a Strategy
	#' @param StrategyID The ID of the Strategy to return.
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given Strategy. Defaults to FALSE for all return fields which, for convenience, returns all fields for the Strategy.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object listed as text. Run \code{\link{getSchemaForObjects}}('Strategy') to get more field paths.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param query Queries to be added to the request. Ex. list(SchoolYearID = 1). Defaults to NULL.
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept MTSS
	#' @return A dataframe or of Strategy
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	getStrategy <- function(StrategyID, Description = F, AreaOfConcernID = F, DistrictID = F, TierLabels = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, Note = F, AttachmentCount = F, IsActive = F, EntityLabels = F, fieldPaths = NULL, entityId = 1, query = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment()) %>% purrr::keep(names(.) != "StrategyID")

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		getSkyObject(module = "MTSS", objectName = "Strategy", objectId = StrategyID, searchFields = searchFields, entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' Delete a Strategy
	#'
	#' This function deletes a Strategy
	#' @param StrategyID The ID of the Strategy to delete
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param query Queries to be added to the request. Ex. list(SchoolYearID = 1). Defaults to NULL.
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept MTSS
	#' @return The StrategyID of the deleted Strategy.
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	deleteStrategy <- function(StrategyID, ignoreWarnings = F, entityId = 1, query = NULL, flatten = T, returnResponse = F){

		deleteSkyObject(module = "MTSS", objectName = "Strategy", objectId = StrategyID, ignoreWarnings = ignoreWarnings, entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' Create a Strategy
	#'
	#' This function creates a Strategy
	#' @param fieldNames The field values to give the created Strategy. Each defaults to NULL.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param query Queries to be added to the request. Ex. list(SchoolYearID = 1). Defaults to NULL.
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept MTSS
	#' @return A newly created Strategy
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	createStrategy <- function(Description = NULL, AreaOfConcernID = NULL, DistrictID = NULL, Note = NULL, IsActive = NULL, entityId = 1, query = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment())

		body <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper()) %>% purrr::compact()

		createSkyObject(module = "MTSS", objectName = "Strategy", body = list(DataObject = body), searchFields = append("StrategyID", body %>% names()), entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' Modify a Strategy
	#'
	#' This function modifies a Strategy
	#' @param fieldNames The field values to give the modified Strategy. Each defaults to NULL.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param query Queries to be added to the request. Ex. list(SchoolYearID = 1). Defaults to NULL.
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept MTSS
	#' @return The modified Strategy
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	modifyStrategy <- function(StrategyID, Description = NULL, AreaOfConcernID = NULL, DistrictID = NULL, Note = NULL, IsActive = NULL, entityId = 1, query = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment())

		body <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper()) %>% purrr::compact()

		modifySkyObject(module = "MTSS", objectName = "Strategy", objectId = StrategyID, body = list(DataObject = body), searchFields = append("StrategyID", body %>% names()), entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' List ObjectiveMeasurements
	#'
	#' This function returns a dataframe or json object of ObjectiveMeasurements
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given ObjectiveMeasurements. Defaults to FALSE for all return fields which, for convenience, returns all fields for the ObjectiveMeasurements.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object listed as text. Run \code{\link{getSchemaForObjects}}('ObjectiveMeasurement') to get more field paths.
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
	#' @concept MTSS
	#' @return A list of ObjectiveMeasurements
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	listObjectiveMeasurements <- function(searchConditionsList = NULL, ObjectiveMeasurementID = F, ObjectiveID = F, MeasurementID = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, fieldPaths = NULL, searchConditionsGroupType = "And", searchSortFieldNamesList = NULL, searchSortFieldNamesDescendingList = NULL, entityId = 1, query = NULL, page = 1, pageSize = 100000, flatten = T, returnResponse = F){

		params <- as.list(environment())

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		listSkyObjects(module = "MTSS", objectName = "ObjectiveMeasurement", searchFields = searchFields %>% append(fieldPaths), page = page, pageSize = pageSize, SearchConditionsList = searchConditionsList, SearchConditionsGroupType = searchConditionsGroupType, SearchSortFieldNamesList = searchSortFieldNamesList, SearchSortFieldNamesDescendingList = searchSortFieldNamesDescendingList, entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' Get an ObjectiveMeasurement
	#'
	#' This function returns a dataframe or json object of an ObjectiveMeasurement
	#' @param ObjectiveMeasurementID The ID of the ObjectiveMeasurement to return.
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given ObjectiveMeasurement. Defaults to FALSE for all return fields which, for convenience, returns all fields for the ObjectiveMeasurement.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object listed as text. Run \code{\link{getSchemaForObjects}}('ObjectiveMeasurement') to get more field paths.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param query Queries to be added to the request. Ex. list(SchoolYearID = 1). Defaults to NULL.
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept MTSS
	#' @return A dataframe or of ObjectiveMeasurement
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	getObjectiveMeasurement <- function(ObjectiveMeasurementID, ObjectiveID = F, MeasurementID = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, fieldPaths = NULL, entityId = 1, query = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment()) %>% purrr::keep(names(.) != "ObjectiveMeasurementID")

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		getSkyObject(module = "MTSS", objectName = "ObjectiveMeasurement", objectId = ObjectiveMeasurementID, searchFields = searchFields, entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' Delete an ObjectiveMeasurement
	#'
	#' This function deletes an ObjectiveMeasurement
	#' @param ObjectiveMeasurementID The ID of the ObjectiveMeasurement to delete
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param query Queries to be added to the request. Ex. list(SchoolYearID = 1). Defaults to NULL.
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept MTSS
	#' @return The ObjectiveMeasurementID of the deleted ObjectiveMeasurement.
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	deleteObjectiveMeasurement <- function(ObjectiveMeasurementID, ignoreWarnings = F, entityId = 1, query = NULL, flatten = T, returnResponse = F){

		deleteSkyObject(module = "MTSS", objectName = "ObjectiveMeasurement", objectId = ObjectiveMeasurementID, ignoreWarnings = ignoreWarnings, entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' Create an ObjectiveMeasurement
	#'
	#' This function creates an ObjectiveMeasurement
	#' @param fieldNames The field values to give the created ObjectiveMeasurement. Each defaults to NULL.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param query Queries to be added to the request. Ex. list(SchoolYearID = 1). Defaults to NULL.
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept MTSS
	#' @return A newly created ObjectiveMeasurement
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	createObjectiveMeasurement <- function(ObjectiveID = NULL, MeasurementID = NULL, entityId = 1, query = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment())

		body <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper()) %>% purrr::compact()

		createSkyObject(module = "MTSS", objectName = "ObjectiveMeasurement", body = list(DataObject = body), searchFields = append("ObjectiveMeasurementID", body %>% names()), entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' Modify an ObjectiveMeasurement
	#'
	#' This function modifies an ObjectiveMeasurement
	#' @param fieldNames The field values to give the modified ObjectiveMeasurement. Each defaults to NULL.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param query Queries to be added to the request. Ex. list(SchoolYearID = 1). Defaults to NULL.
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept MTSS
	#' @return The modified ObjectiveMeasurement
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	modifyObjectiveMeasurement <- function(ObjectiveMeasurementID, ObjectiveID = NULL, MeasurementID = NULL, entityId = 1, query = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment())

		body <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper()) %>% purrr::compact()

		modifySkyObject(module = "MTSS", objectName = "ObjectiveMeasurement", objectId = ObjectiveMeasurementID, body = list(DataObject = body), searchFields = append("ObjectiveMeasurementID", body %>% names()), entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' List Measurements
	#'
	#' This function returns a dataframe or json object of Measurements
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given Measurements. Defaults to FALSE for all return fields which, for convenience, returns all fields for the Measurements.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object listed as text. Run \code{\link{getSchemaForObjects}}('Measurement') to get more field paths.
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
	#' @concept MTSS
	#' @return A list of Measurements
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	listMeasurements <- function(searchConditionsList = NULL, MeasurementID = F, Code = F, Description = F, CodeDescription = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, UseQualitativeMeasurement = F, QualitativeScaleID = F, IsActive = F, fieldPaths = NULL, searchConditionsGroupType = "And", searchSortFieldNamesList = NULL, searchSortFieldNamesDescendingList = NULL, entityId = 1, query = NULL, page = 1, pageSize = 100000, flatten = T, returnResponse = F){

		params <- as.list(environment())

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		listSkyObjects(module = "MTSS", objectName = "Measurement", searchFields = searchFields %>% append(fieldPaths), page = page, pageSize = pageSize, SearchConditionsList = searchConditionsList, SearchConditionsGroupType = searchConditionsGroupType, SearchSortFieldNamesList = searchSortFieldNamesList, SearchSortFieldNamesDescendingList = searchSortFieldNamesDescendingList, entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' Get a Measurement
	#'
	#' This function returns a dataframe or json object of a Measurement
	#' @param MeasurementID The ID of the Measurement to return.
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given Measurement. Defaults to FALSE for all return fields which, for convenience, returns all fields for the Measurement.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object listed as text. Run \code{\link{getSchemaForObjects}}('Measurement') to get more field paths.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param query Queries to be added to the request. Ex. list(SchoolYearID = 1). Defaults to NULL.
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept MTSS
	#' @return A dataframe or of Measurement
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	getMeasurement <- function(MeasurementID, Code = F, Description = F, CodeDescription = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, UseQualitativeMeasurement = F, QualitativeScaleID = F, IsActive = F, fieldPaths = NULL, entityId = 1, query = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment()) %>% purrr::keep(names(.) != "MeasurementID")

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		getSkyObject(module = "MTSS", objectName = "Measurement", objectId = MeasurementID, searchFields = searchFields, entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' Delete a Measurement
	#'
	#' This function deletes a Measurement
	#' @param MeasurementID The ID of the Measurement to delete
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param query Queries to be added to the request. Ex. list(SchoolYearID = 1). Defaults to NULL.
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept MTSS
	#' @return The MeasurementID of the deleted Measurement.
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	deleteMeasurement <- function(MeasurementID, ignoreWarnings = F, entityId = 1, query = NULL, flatten = T, returnResponse = F){

		deleteSkyObject(module = "MTSS", objectName = "Measurement", objectId = MeasurementID, ignoreWarnings = ignoreWarnings, entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' Create a Measurement
	#'
	#' This function creates a Measurement
	#' @param fieldNames The field values to give the created Measurement. Each defaults to NULL.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param query Queries to be added to the request. Ex. list(SchoolYearID = 1). Defaults to NULL.
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept MTSS
	#' @return A newly created Measurement
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	createMeasurement <- function(Code = NULL, Description = NULL, UseQualitativeMeasurement = NULL, QualitativeScaleID = NULL, IsActive = NULL, entityId = 1, query = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment())

		body <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper()) %>% purrr::compact()

		createSkyObject(module = "MTSS", objectName = "Measurement", body = list(DataObject = body), searchFields = append("MeasurementID", body %>% names()), entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' Modify a Measurement
	#'
	#' This function modifies a Measurement
	#' @param fieldNames The field values to give the modified Measurement. Each defaults to NULL.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param query Queries to be added to the request. Ex. list(SchoolYearID = 1). Defaults to NULL.
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept MTSS
	#' @return The modified Measurement
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	modifyMeasurement <- function(MeasurementID, Code = NULL, Description = NULL, UseQualitativeMeasurement = NULL, QualitativeScaleID = NULL, IsActive = NULL, entityId = 1, query = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment())

		body <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper()) %>% purrr::compact()

		modifySkyObject(module = "MTSS", objectName = "Measurement", objectId = MeasurementID, body = list(DataObject = body), searchFields = append("MeasurementID", body %>% names()), entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' List Objectives
	#'
	#' This function returns a dataframe or json object of Objectives
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given Objectives. Defaults to FALSE for all return fields which, for convenience, returns all fields for the Objectives.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object listed as text. Run \code{\link{getSchemaForObjects}}('Objective') to get more field paths.
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
	#' @concept MTSS
	#' @return A list of Objectives
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	listObjectives <- function(searchConditionsList = NULL, ObjectiveID = F, Code = F, Description = F, ObjectiveType = F, AreaOfConcernID = F, DistrictID = F, CodeDescription = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, IsActive = F, fieldPaths = NULL, searchConditionsGroupType = "And", searchSortFieldNamesList = NULL, searchSortFieldNamesDescendingList = NULL, entityId = 1, query = NULL, page = 1, pageSize = 100000, flatten = T, returnResponse = F){

		params <- as.list(environment())

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		listSkyObjects(module = "MTSS", objectName = "Objective", searchFields = searchFields %>% append(fieldPaths), page = page, pageSize = pageSize, SearchConditionsList = searchConditionsList, SearchConditionsGroupType = searchConditionsGroupType, SearchSortFieldNamesList = searchSortFieldNamesList, SearchSortFieldNamesDescendingList = searchSortFieldNamesDescendingList, entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' Get an Objective
	#'
	#' This function returns a dataframe or json object of an Objective
	#' @param ObjectiveID The ID of the Objective to return.
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given Objective. Defaults to FALSE for all return fields which, for convenience, returns all fields for the Objective.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object listed as text. Run \code{\link{getSchemaForObjects}}('Objective') to get more field paths.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param query Queries to be added to the request. Ex. list(SchoolYearID = 1). Defaults to NULL.
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept MTSS
	#' @return A dataframe or of Objective
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	getObjective <- function(ObjectiveID, Code = F, Description = F, ObjectiveType = F, AreaOfConcernID = F, DistrictID = F, CodeDescription = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, IsActive = F, fieldPaths = NULL, entityId = 1, query = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment()) %>% purrr::keep(names(.) != "ObjectiveID")

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		getSkyObject(module = "MTSS", objectName = "Objective", objectId = ObjectiveID, searchFields = searchFields, entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' Delete an Objective
	#'
	#' This function deletes an Objective
	#' @param ObjectiveID The ID of the Objective to delete
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param query Queries to be added to the request. Ex. list(SchoolYearID = 1). Defaults to NULL.
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept MTSS
	#' @return The ObjectiveID of the deleted Objective.
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	deleteObjective <- function(ObjectiveID, ignoreWarnings = F, entityId = 1, query = NULL, flatten = T, returnResponse = F){

		deleteSkyObject(module = "MTSS", objectName = "Objective", objectId = ObjectiveID, ignoreWarnings = ignoreWarnings, entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' Create an Objective
	#'
	#' This function creates an Objective
	#' @param fieldNames The field values to give the created Objective. Each defaults to NULL.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param query Queries to be added to the request. Ex. list(SchoolYearID = 1). Defaults to NULL.
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept MTSS
	#' @return A newly created Objective
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	createObjective <- function(Code = NULL, Description = NULL, ObjectiveType = NULL, AreaOfConcernID = NULL, DistrictID = NULL, IsActive = NULL, entityId = 1, query = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment())

		body <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper()) %>% purrr::compact()

		createSkyObject(module = "MTSS", objectName = "Objective", body = list(DataObject = body), searchFields = append("ObjectiveID", body %>% names()), entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' Modify an Objective
	#'
	#' This function modifies an Objective
	#' @param fieldNames The field values to give the modified Objective. Each defaults to NULL.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param query Queries to be added to the request. Ex. list(SchoolYearID = 1). Defaults to NULL.
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept MTSS
	#' @return The modified Objective
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	modifyObjective <- function(ObjectiveID, Code = NULL, Description = NULL, ObjectiveType = NULL, AreaOfConcernID = NULL, DistrictID = NULL, IsActive = NULL, entityId = 1, query = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment())

		body <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper()) %>% purrr::compact()

		modifySkyObject(module = "MTSS", objectName = "Objective", objectId = ObjectiveID, body = list(DataObject = body), searchFields = append("ObjectiveID", body %>% names()), entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' List InterventionPlanMeetings
	#'
	#' This function returns a dataframe or json object of InterventionPlanMeetings
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given InterventionPlanMeetings. Defaults to FALSE for all return fields which, for convenience, returns all fields for the InterventionPlanMeetings.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object listed as text. Run \code{\link{getSchemaForObjects}}('InterventionPlanMeeting') to get more field paths.
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
	#' @concept MTSS
	#' @return A list of InterventionPlanMeetings
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	listInterventionPlanMeetings <- function(searchConditionsList = NULL, InterventionPlanMeetingID = F, InterventionPlanStrategyID = F, Date = F, Time = F, Measurement = F, InterventionPlanStaffID = F, Note = F, MeetingType = F, MeasurementDisplay = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, DidNotMeet = F, AttachmentCount = F, QualitativeScaleMappingID = F, fieldPaths = NULL, searchConditionsGroupType = "And", searchSortFieldNamesList = NULL, searchSortFieldNamesDescendingList = NULL, entityId = 1, query = NULL, page = 1, pageSize = 100000, flatten = T, returnResponse = F){

		params <- as.list(environment())

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		listSkyObjects(module = "MTSS", objectName = "InterventionPlanMeeting", searchFields = searchFields %>% append(fieldPaths), page = page, pageSize = pageSize, SearchConditionsList = searchConditionsList, SearchConditionsGroupType = searchConditionsGroupType, SearchSortFieldNamesList = searchSortFieldNamesList, SearchSortFieldNamesDescendingList = searchSortFieldNamesDescendingList, entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' Get an InterventionPlanMeeting
	#'
	#' This function returns a dataframe or json object of an InterventionPlanMeeting
	#' @param InterventionPlanMeetingID The ID of the InterventionPlanMeeting to return.
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given InterventionPlanMeeting. Defaults to FALSE for all return fields which, for convenience, returns all fields for the InterventionPlanMeeting.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object listed as text. Run \code{\link{getSchemaForObjects}}('InterventionPlanMeeting') to get more field paths.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param query Queries to be added to the request. Ex. list(SchoolYearID = 1). Defaults to NULL.
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept MTSS
	#' @return A dataframe or of InterventionPlanMeeting
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	getInterventionPlanMeeting <- function(InterventionPlanMeetingID, InterventionPlanStrategyID = F, Date = F, Time = F, Measurement = F, InterventionPlanStaffID = F, Note = F, MeetingType = F, MeasurementDisplay = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, DidNotMeet = F, AttachmentCount = F, QualitativeScaleMappingID = F, fieldPaths = NULL, entityId = 1, query = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment()) %>% purrr::keep(names(.) != "InterventionPlanMeetingID")

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		getSkyObject(module = "MTSS", objectName = "InterventionPlanMeeting", objectId = InterventionPlanMeetingID, searchFields = searchFields, entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' Delete an InterventionPlanMeeting
	#'
	#' This function deletes an InterventionPlanMeeting
	#' @param InterventionPlanMeetingID The ID of the InterventionPlanMeeting to delete
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param query Queries to be added to the request. Ex. list(SchoolYearID = 1). Defaults to NULL.
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept MTSS
	#' @return The InterventionPlanMeetingID of the deleted InterventionPlanMeeting.
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	deleteInterventionPlanMeeting <- function(InterventionPlanMeetingID, ignoreWarnings = F, entityId = 1, query = NULL, flatten = T, returnResponse = F){

		deleteSkyObject(module = "MTSS", objectName = "InterventionPlanMeeting", objectId = InterventionPlanMeetingID, ignoreWarnings = ignoreWarnings, entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' Create an InterventionPlanMeeting
	#'
	#' This function creates an InterventionPlanMeeting
	#' @param fieldNames The field values to give the created InterventionPlanMeeting. Each defaults to NULL.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param query Queries to be added to the request. Ex. list(SchoolYearID = 1). Defaults to NULL.
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept MTSS
	#' @return A newly created InterventionPlanMeeting
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	createInterventionPlanMeeting <- function(InterventionPlanStrategyID = NULL, Date = NULL, Time = NULL, Measurement = NULL, InterventionPlanStaffID = NULL, Note = NULL, MeetingType = NULL, DidNotMeet = NULL, QualitativeScaleMappingID = NULL, entityId = 1, query = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment())

		body <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper()) %>% purrr::compact()

		createSkyObject(module = "MTSS", objectName = "InterventionPlanMeeting", body = list(DataObject = body), searchFields = append("InterventionPlanMeetingID", body %>% names()), entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' Modify an InterventionPlanMeeting
	#'
	#' This function modifies an InterventionPlanMeeting
	#' @param fieldNames The field values to give the modified InterventionPlanMeeting. Each defaults to NULL.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param query Queries to be added to the request. Ex. list(SchoolYearID = 1). Defaults to NULL.
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept MTSS
	#' @return The modified InterventionPlanMeeting
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	modifyInterventionPlanMeeting <- function(InterventionPlanMeetingID, InterventionPlanStrategyID = NULL, Date = NULL, Time = NULL, Measurement = NULL, InterventionPlanStaffID = NULL, Note = NULL, MeetingType = NULL, DidNotMeet = NULL, QualitativeScaleMappingID = NULL, entityId = 1, query = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment())

		body <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper()) %>% purrr::compact()

		modifySkyObject(module = "MTSS", objectName = "InterventionPlanMeeting", objectId = InterventionPlanMeetingID, body = list(DataObject = body), searchFields = append("InterventionPlanMeetingID", body %>% names()), entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' List InterventionPlanNotes
	#'
	#' This function returns a dataframe or json object of InterventionPlanNotes
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given InterventionPlanNotes. Defaults to FALSE for all return fields which, for convenience, returns all fields for the InterventionPlanNotes.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object listed as text. Run \code{\link{getSchemaForObjects}}('InterventionPlanNote') to get more field paths.
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
	#' @concept MTSS
	#' @return A list of InterventionPlanNotes
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	listInterventionPlanNotes <- function(searchConditionsList = NULL, InterventionPlanNoteID = F, InterventionPlanID = F, Note = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, fieldPaths = NULL, searchConditionsGroupType = "And", searchSortFieldNamesList = NULL, searchSortFieldNamesDescendingList = NULL, entityId = 1, query = NULL, page = 1, pageSize = 100000, flatten = T, returnResponse = F){

		params <- as.list(environment())

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		listSkyObjects(module = "MTSS", objectName = "InterventionPlanNote", searchFields = searchFields %>% append(fieldPaths), page = page, pageSize = pageSize, SearchConditionsList = searchConditionsList, SearchConditionsGroupType = searchConditionsGroupType, SearchSortFieldNamesList = searchSortFieldNamesList, SearchSortFieldNamesDescendingList = searchSortFieldNamesDescendingList, entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' Get an InterventionPlanNote
	#'
	#' This function returns a dataframe or json object of an InterventionPlanNote
	#' @param InterventionPlanNoteID The ID of the InterventionPlanNote to return.
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given InterventionPlanNote. Defaults to FALSE for all return fields which, for convenience, returns all fields for the InterventionPlanNote.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object listed as text. Run \code{\link{getSchemaForObjects}}('InterventionPlanNote') to get more field paths.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param query Queries to be added to the request. Ex. list(SchoolYearID = 1). Defaults to NULL.
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept MTSS
	#' @return A dataframe or of InterventionPlanNote
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	getInterventionPlanNote <- function(InterventionPlanNoteID, InterventionPlanID = F, Note = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, fieldPaths = NULL, entityId = 1, query = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment()) %>% purrr::keep(names(.) != "InterventionPlanNoteID")

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		getSkyObject(module = "MTSS", objectName = "InterventionPlanNote", objectId = InterventionPlanNoteID, searchFields = searchFields, entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' Delete an InterventionPlanNote
	#'
	#' This function deletes an InterventionPlanNote
	#' @param InterventionPlanNoteID The ID of the InterventionPlanNote to delete
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param query Queries to be added to the request. Ex. list(SchoolYearID = 1). Defaults to NULL.
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept MTSS
	#' @return The InterventionPlanNoteID of the deleted InterventionPlanNote.
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	deleteInterventionPlanNote <- function(InterventionPlanNoteID, ignoreWarnings = F, entityId = 1, query = NULL, flatten = T, returnResponse = F){

		deleteSkyObject(module = "MTSS", objectName = "InterventionPlanNote", objectId = InterventionPlanNoteID, ignoreWarnings = ignoreWarnings, entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' Create an InterventionPlanNote
	#'
	#' This function creates an InterventionPlanNote
	#' @param fieldNames The field values to give the created InterventionPlanNote. Each defaults to NULL.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param query Queries to be added to the request. Ex. list(SchoolYearID = 1). Defaults to NULL.
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept MTSS
	#' @return A newly created InterventionPlanNote
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	createInterventionPlanNote <- function(InterventionPlanID = NULL, Note = NULL, entityId = 1, query = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment())

		body <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper()) %>% purrr::compact()

		createSkyObject(module = "MTSS", objectName = "InterventionPlanNote", body = list(DataObject = body), searchFields = append("InterventionPlanNoteID", body %>% names()), entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' Modify an InterventionPlanNote
	#'
	#' This function modifies an InterventionPlanNote
	#' @param fieldNames The field values to give the modified InterventionPlanNote. Each defaults to NULL.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param query Queries to be added to the request. Ex. list(SchoolYearID = 1). Defaults to NULL.
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept MTSS
	#' @return The modified InterventionPlanNote
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	modifyInterventionPlanNote <- function(InterventionPlanNoteID, InterventionPlanID = NULL, Note = NULL, entityId = 1, query = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment())

		body <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper()) %>% purrr::compact()

		modifySkyObject(module = "MTSS", objectName = "InterventionPlanNote", objectId = InterventionPlanNoteID, body = list(DataObject = body), searchFields = append("InterventionPlanNoteID", body %>% names()), entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' List QualitativeScales
	#'
	#' This function returns a dataframe or json object of QualitativeScales
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given QualitativeScales. Defaults to FALSE for all return fields which, for convenience, returns all fields for the QualitativeScales.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object listed as text. Run \code{\link{getSchemaForObjects}}('QualitativeScale') to get more field paths.
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
	#' @concept MTSS
	#' @return A list of QualitativeScales
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	listQualitativeScales <- function(searchConditionsList = NULL, QualitativeScaleID = F, Code = F, Description = F, DistrictID = F, CodeDescription = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, IsActive = F, fieldPaths = NULL, searchConditionsGroupType = "And", searchSortFieldNamesList = NULL, searchSortFieldNamesDescendingList = NULL, entityId = 1, query = NULL, page = 1, pageSize = 100000, flatten = T, returnResponse = F){

		params <- as.list(environment())

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		listSkyObjects(module = "MTSS", objectName = "QualitativeScale", searchFields = searchFields %>% append(fieldPaths), page = page, pageSize = pageSize, SearchConditionsList = searchConditionsList, SearchConditionsGroupType = searchConditionsGroupType, SearchSortFieldNamesList = searchSortFieldNamesList, SearchSortFieldNamesDescendingList = searchSortFieldNamesDescendingList, entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' Get a QualitativeScale
	#'
	#' This function returns a dataframe or json object of a QualitativeScale
	#' @param QualitativeScaleID The ID of the QualitativeScale to return.
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given QualitativeScale. Defaults to FALSE for all return fields which, for convenience, returns all fields for the QualitativeScale.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object listed as text. Run \code{\link{getSchemaForObjects}}('QualitativeScale') to get more field paths.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param query Queries to be added to the request. Ex. list(SchoolYearID = 1). Defaults to NULL.
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept MTSS
	#' @return A dataframe or of QualitativeScale
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	getQualitativeScale <- function(QualitativeScaleID, Code = F, Description = F, DistrictID = F, CodeDescription = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, IsActive = F, fieldPaths = NULL, entityId = 1, query = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment()) %>% purrr::keep(names(.) != "QualitativeScaleID")

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		getSkyObject(module = "MTSS", objectName = "QualitativeScale", objectId = QualitativeScaleID, searchFields = searchFields, entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' Delete a QualitativeScale
	#'
	#' This function deletes a QualitativeScale
	#' @param QualitativeScaleID The ID of the QualitativeScale to delete
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param query Queries to be added to the request. Ex. list(SchoolYearID = 1). Defaults to NULL.
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept MTSS
	#' @return The QualitativeScaleID of the deleted QualitativeScale.
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	deleteQualitativeScale <- function(QualitativeScaleID, ignoreWarnings = F, entityId = 1, query = NULL, flatten = T, returnResponse = F){

		deleteSkyObject(module = "MTSS", objectName = "QualitativeScale", objectId = QualitativeScaleID, ignoreWarnings = ignoreWarnings, entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' Create a QualitativeScale
	#'
	#' This function creates a QualitativeScale
	#' @param fieldNames The field values to give the created QualitativeScale. Each defaults to NULL.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param query Queries to be added to the request. Ex. list(SchoolYearID = 1). Defaults to NULL.
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept MTSS
	#' @return A newly created QualitativeScale
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	createQualitativeScale <- function(Code = NULL, Description = NULL, DistrictID = NULL, IsActive = NULL, entityId = 1, query = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment())

		body <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper()) %>% purrr::compact()

		createSkyObject(module = "MTSS", objectName = "QualitativeScale", body = list(DataObject = body), searchFields = append("QualitativeScaleID", body %>% names()), entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' Modify a QualitativeScale
	#'
	#' This function modifies a QualitativeScale
	#' @param fieldNames The field values to give the modified QualitativeScale. Each defaults to NULL.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param query Queries to be added to the request. Ex. list(SchoolYearID = 1). Defaults to NULL.
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept MTSS
	#' @return The modified QualitativeScale
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	modifyQualitativeScale <- function(QualitativeScaleID, Code = NULL, Description = NULL, DistrictID = NULL, IsActive = NULL, entityId = 1, query = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment())

		body <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper()) %>% purrr::compact()

		modifySkyObject(module = "MTSS", objectName = "QualitativeScale", objectId = QualitativeScaleID, body = list(DataObject = body), searchFields = append("QualitativeScaleID", body %>% names()), entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' List QualitativeScaleMappings
	#'
	#' This function returns a dataframe or json object of QualitativeScaleMappings
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given QualitativeScaleMappings. Defaults to FALSE for all return fields which, for convenience, returns all fields for the QualitativeScaleMappings.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object listed as text. Run \code{\link{getSchemaForObjects}}('QualitativeScaleMapping') to get more field paths.
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
	#' @concept MTSS
	#' @return A list of QualitativeScaleMappings
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	listQualitativeScaleMappings <- function(searchConditionsList = NULL, QualitativeScaleMappingID = F, QualitativeScaleID = F, Value = F, NumericValue = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, fieldPaths = NULL, searchConditionsGroupType = "And", searchSortFieldNamesList = NULL, searchSortFieldNamesDescendingList = NULL, entityId = 1, query = NULL, page = 1, pageSize = 100000, flatten = T, returnResponse = F){

		params <- as.list(environment())

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		listSkyObjects(module = "MTSS", objectName = "QualitativeScaleMapping", searchFields = searchFields %>% append(fieldPaths), page = page, pageSize = pageSize, SearchConditionsList = searchConditionsList, SearchConditionsGroupType = searchConditionsGroupType, SearchSortFieldNamesList = searchSortFieldNamesList, SearchSortFieldNamesDescendingList = searchSortFieldNamesDescendingList, entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' Get a QualitativeScaleMapping
	#'
	#' This function returns a dataframe or json object of a QualitativeScaleMapping
	#' @param QualitativeScaleMappingID The ID of the QualitativeScaleMapping to return.
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given QualitativeScaleMapping. Defaults to FALSE for all return fields which, for convenience, returns all fields for the QualitativeScaleMapping.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object listed as text. Run \code{\link{getSchemaForObjects}}('QualitativeScaleMapping') to get more field paths.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param query Queries to be added to the request. Ex. list(SchoolYearID = 1). Defaults to NULL.
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept MTSS
	#' @return A dataframe or of QualitativeScaleMapping
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	getQualitativeScaleMapping <- function(QualitativeScaleMappingID, QualitativeScaleID = F, Value = F, NumericValue = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, fieldPaths = NULL, entityId = 1, query = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment()) %>% purrr::keep(names(.) != "QualitativeScaleMappingID")

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		getSkyObject(module = "MTSS", objectName = "QualitativeScaleMapping", objectId = QualitativeScaleMappingID, searchFields = searchFields, entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' Delete a QualitativeScaleMapping
	#'
	#' This function deletes a QualitativeScaleMapping
	#' @param QualitativeScaleMappingID The ID of the QualitativeScaleMapping to delete
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param query Queries to be added to the request. Ex. list(SchoolYearID = 1). Defaults to NULL.
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept MTSS
	#' @return The QualitativeScaleMappingID of the deleted QualitativeScaleMapping.
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	deleteQualitativeScaleMapping <- function(QualitativeScaleMappingID, ignoreWarnings = F, entityId = 1, query = NULL, flatten = T, returnResponse = F){

		deleteSkyObject(module = "MTSS", objectName = "QualitativeScaleMapping", objectId = QualitativeScaleMappingID, ignoreWarnings = ignoreWarnings, entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' Create a QualitativeScaleMapping
	#'
	#' This function creates a QualitativeScaleMapping
	#' @param fieldNames The field values to give the created QualitativeScaleMapping. Each defaults to NULL.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param query Queries to be added to the request. Ex. list(SchoolYearID = 1). Defaults to NULL.
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept MTSS
	#' @return A newly created QualitativeScaleMapping
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	createQualitativeScaleMapping <- function(QualitativeScaleID = NULL, Value = NULL, NumericValue = NULL, entityId = 1, query = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment())

		body <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper()) %>% purrr::compact()

		createSkyObject(module = "MTSS", objectName = "QualitativeScaleMapping", body = list(DataObject = body), searchFields = append("QualitativeScaleMappingID", body %>% names()), entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' Modify a QualitativeScaleMapping
	#'
	#' This function modifies a QualitativeScaleMapping
	#' @param fieldNames The field values to give the modified QualitativeScaleMapping. Each defaults to NULL.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param query Queries to be added to the request. Ex. list(SchoolYearID = 1). Defaults to NULL.
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept MTSS
	#' @return The modified QualitativeScaleMapping
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	modifyQualitativeScaleMapping <- function(QualitativeScaleMappingID, QualitativeScaleID = NULL, Value = NULL, NumericValue = NULL, entityId = 1, query = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment())

		body <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper()) %>% purrr::compact()

		modifySkyObject(module = "MTSS", objectName = "QualitativeScaleMapping", objectId = QualitativeScaleMappingID, body = list(DataObject = body), searchFields = append("QualitativeScaleMappingID", body %>% names()), entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' List WatchlistConditions
	#'
	#' This function returns a dataframe or json object of WatchlistConditions
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given WatchlistConditions. Defaults to FALSE for all return fields which, for convenience, returns all fields for the WatchlistConditions.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object listed as text. Run \code{\link{getSchemaForObjects}}('WatchlistCondition') to get more field paths.
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
	#' @concept MTSS
	#' @return A list of WatchlistConditions
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	listWatchlistConditions <- function(searchConditionsList = NULL, WatchlistConditionID = F, SkywardID = F, SkywardHash = F, FieldPath = F, DisplayName = F, MultiSelectBrowseSkywardID = F, Order = F, DependentSkywardID = F, DependentFriendlyNames = F, RuleType = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, fieldPaths = NULL, searchConditionsGroupType = "And", searchSortFieldNamesList = NULL, searchSortFieldNamesDescendingList = NULL, entityId = 1, query = NULL, page = 1, pageSize = 100000, flatten = T, returnResponse = F){

		params <- as.list(environment())

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		listSkyObjects(module = "MTSS", objectName = "WatchlistCondition", searchFields = searchFields %>% append(fieldPaths), page = page, pageSize = pageSize, SearchConditionsList = searchConditionsList, SearchConditionsGroupType = searchConditionsGroupType, SearchSortFieldNamesList = searchSortFieldNamesList, SearchSortFieldNamesDescendingList = searchSortFieldNamesDescendingList, entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' Get a WatchlistCondition
	#'
	#' This function returns a dataframe or json object of a WatchlistCondition
	#' @param WatchlistConditionID The ID of the WatchlistCondition to return.
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given WatchlistCondition. Defaults to FALSE for all return fields which, for convenience, returns all fields for the WatchlistCondition.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object listed as text. Run \code{\link{getSchemaForObjects}}('WatchlistCondition') to get more field paths.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param query Queries to be added to the request. Ex. list(SchoolYearID = 1). Defaults to NULL.
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept MTSS
	#' @return A dataframe or of WatchlistCondition
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	getWatchlistCondition <- function(WatchlistConditionID, SkywardID = F, SkywardHash = F, FieldPath = F, DisplayName = F, MultiSelectBrowseSkywardID = F, Order = F, DependentSkywardID = F, DependentFriendlyNames = F, RuleType = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, fieldPaths = NULL, entityId = 1, query = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment()) %>% purrr::keep(names(.) != "WatchlistConditionID")

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		getSkyObject(module = "MTSS", objectName = "WatchlistCondition", objectId = WatchlistConditionID, searchFields = searchFields, entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' Delete a WatchlistCondition
	#'
	#' This function deletes a WatchlistCondition
	#' @param WatchlistConditionID The ID of the WatchlistCondition to delete
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param query Queries to be added to the request. Ex. list(SchoolYearID = 1). Defaults to NULL.
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept MTSS
	#' @return The WatchlistConditionID of the deleted WatchlistCondition.
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	deleteWatchlistCondition <- function(WatchlistConditionID, ignoreWarnings = F, entityId = 1, query = NULL, flatten = T, returnResponse = F){

		deleteSkyObject(module = "MTSS", objectName = "WatchlistCondition", objectId = WatchlistConditionID, ignoreWarnings = ignoreWarnings, entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' Create a WatchlistCondition
	#'
	#' This function creates a WatchlistCondition
	#' @param fieldNames The field values to give the created WatchlistCondition. Each defaults to NULL.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param query Queries to be added to the request. Ex. list(SchoolYearID = 1). Defaults to NULL.
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept MTSS
	#' @return A newly created WatchlistCondition
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	createWatchlistCondition <- function(RuleType = NULL, entityId = 1, query = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment())

		body <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper()) %>% purrr::compact()

		createSkyObject(module = "MTSS", objectName = "WatchlistCondition", body = list(DataObject = body), searchFields = append("WatchlistConditionID", body %>% names()), entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' Modify a WatchlistCondition
	#'
	#' This function modifies a WatchlistCondition
	#' @param fieldNames The field values to give the modified WatchlistCondition. Each defaults to NULL.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param query Queries to be added to the request. Ex. list(SchoolYearID = 1). Defaults to NULL.
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept MTSS
	#' @return The modified WatchlistCondition
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	modifyWatchlistCondition <- function(WatchlistConditionID, RuleType = NULL, entityId = 1, query = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment())

		body <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper()) %>% purrr::compact()

		modifySkyObject(module = "MTSS", objectName = "WatchlistCondition", objectId = WatchlistConditionID, body = list(DataObject = body), searchFields = append("WatchlistConditionID", body %>% names()), entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' List WatchlistThresholds
	#'
	#' This function returns a dataframe or json object of WatchlistThresholds
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given WatchlistThresholds. Defaults to FALSE for all return fields which, for convenience, returns all fields for the WatchlistThresholds.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object listed as text. Run \code{\link{getSchemaForObjects}}('WatchlistThreshold') to get more field paths.
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
	#' @concept MTSS
	#' @return A list of WatchlistThresholds
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	listWatchlistThresholds <- function(searchConditionsList = NULL, WatchlistThresholdID = F, EntityID = F, SchoolYearID = F, AreaOfConcernID = F, Name = F, Description = F, RunStartDate = F, RunEndDate = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, IsActive = F, WatchlistThresholdIDClonedFrom = F, WatchlistThresholdIDClonedTo = F, GradeLevelLabels = F, fieldPaths = NULL, searchConditionsGroupType = "And", searchSortFieldNamesList = NULL, searchSortFieldNamesDescendingList = NULL, entityId = 1, query = NULL, page = 1, pageSize = 100000, flatten = T, returnResponse = F){

		params <- as.list(environment())

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		listSkyObjects(module = "MTSS", objectName = "WatchlistThreshold", searchFields = searchFields %>% append(fieldPaths), page = page, pageSize = pageSize, SearchConditionsList = searchConditionsList, SearchConditionsGroupType = searchConditionsGroupType, SearchSortFieldNamesList = searchSortFieldNamesList, SearchSortFieldNamesDescendingList = searchSortFieldNamesDescendingList, entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' Get a WatchlistThreshold
	#'
	#' This function returns a dataframe or json object of a WatchlistThreshold
	#' @param WatchlistThresholdID The ID of the WatchlistThreshold to return.
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given WatchlistThreshold. Defaults to FALSE for all return fields which, for convenience, returns all fields for the WatchlistThreshold.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object listed as text. Run \code{\link{getSchemaForObjects}}('WatchlistThreshold') to get more field paths.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param query Queries to be added to the request. Ex. list(SchoolYearID = 1). Defaults to NULL.
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept MTSS
	#' @return A dataframe or of WatchlistThreshold
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	getWatchlistThreshold <- function(WatchlistThresholdID, EntityID = F, SchoolYearID = F, AreaOfConcernID = F, Name = F, Description = F, RunStartDate = F, RunEndDate = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, IsActive = F, WatchlistThresholdIDClonedFrom = F, WatchlistThresholdIDClonedTo = F, GradeLevelLabels = F, fieldPaths = NULL, entityId = 1, query = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment()) %>% purrr::keep(names(.) != "WatchlistThresholdID")

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		getSkyObject(module = "MTSS", objectName = "WatchlistThreshold", objectId = WatchlistThresholdID, searchFields = searchFields, entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' Delete a WatchlistThreshold
	#'
	#' This function deletes a WatchlistThreshold
	#' @param WatchlistThresholdID The ID of the WatchlistThreshold to delete
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param query Queries to be added to the request. Ex. list(SchoolYearID = 1). Defaults to NULL.
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept MTSS
	#' @return The WatchlistThresholdID of the deleted WatchlistThreshold.
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	deleteWatchlistThreshold <- function(WatchlistThresholdID, ignoreWarnings = F, entityId = 1, query = NULL, flatten = T, returnResponse = F){

		deleteSkyObject(module = "MTSS", objectName = "WatchlistThreshold", objectId = WatchlistThresholdID, ignoreWarnings = ignoreWarnings, entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' Create a WatchlistThreshold
	#'
	#' This function creates a WatchlistThreshold
	#' @param fieldNames The field values to give the created WatchlistThreshold. Each defaults to NULL.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param query Queries to be added to the request. Ex. list(SchoolYearID = 1). Defaults to NULL.
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept MTSS
	#' @return A newly created WatchlistThreshold
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	createWatchlistThreshold <- function(EntityID = NULL, SchoolYearID = NULL, AreaOfConcernID = NULL, Name = NULL, Description = NULL, RunStartDate = NULL, RunEndDate = NULL, IsActive = NULL, WatchlistThresholdIDClonedFrom = NULL, entityId = 1, query = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment())

		body <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper()) %>% purrr::compact()

		createSkyObject(module = "MTSS", objectName = "WatchlistThreshold", body = list(DataObject = body), searchFields = append("WatchlistThresholdID", body %>% names()), entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' Modify a WatchlistThreshold
	#'
	#' This function modifies a WatchlistThreshold
	#' @param fieldNames The field values to give the modified WatchlistThreshold. Each defaults to NULL.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param query Queries to be added to the request. Ex. list(SchoolYearID = 1). Defaults to NULL.
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept MTSS
	#' @return The modified WatchlistThreshold
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	modifyWatchlistThreshold <- function(WatchlistThresholdID, EntityID = NULL, SchoolYearID = NULL, AreaOfConcernID = NULL, Name = NULL, Description = NULL, RunStartDate = NULL, RunEndDate = NULL, IsActive = NULL, WatchlistThresholdIDClonedFrom = NULL, entityId = 1, query = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment())

		body <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper()) %>% purrr::compact()

		modifySkyObject(module = "MTSS", objectName = "WatchlistThreshold", objectId = WatchlistThresholdID, body = list(DataObject = body), searchFields = append("WatchlistThresholdID", body %>% names()), entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' List ThresholdRules
	#'
	#' This function returns a dataframe or json object of ThresholdRules
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given ThresholdRules. Defaults to FALSE for all return fields which, for convenience, returns all fields for the ThresholdRules.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object listed as text. Run \code{\link{getSchemaForObjects}}('ThresholdRule') to get more field paths.
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
	#' @concept MTSS
	#' @return A list of ThresholdRules
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	listThresholdRules <- function(searchConditionsList = NULL, ThresholdRuleID = F, ThresholdID = F, ThresholdRuleGroupID = F, NumberOfOccurrences = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, ThresholdRuleIDClonedFrom = F, ThresholdRuleIDClonedTo = F, fieldPaths = NULL, searchConditionsGroupType = "And", searchSortFieldNamesList = NULL, searchSortFieldNamesDescendingList = NULL, entityId = 1, query = NULL, page = 1, pageSize = 100000, flatten = T, returnResponse = F){

		params <- as.list(environment())

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		listSkyObjects(module = "MTSS", objectName = "ThresholdRule", searchFields = searchFields %>% append(fieldPaths), page = page, pageSize = pageSize, SearchConditionsList = searchConditionsList, SearchConditionsGroupType = searchConditionsGroupType, SearchSortFieldNamesList = searchSortFieldNamesList, SearchSortFieldNamesDescendingList = searchSortFieldNamesDescendingList, entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' Get a ThresholdRule
	#'
	#' This function returns a dataframe or json object of a ThresholdRule
	#' @param ThresholdRuleID The ID of the ThresholdRule to return.
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given ThresholdRule. Defaults to FALSE for all return fields which, for convenience, returns all fields for the ThresholdRule.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object listed as text. Run \code{\link{getSchemaForObjects}}('ThresholdRule') to get more field paths.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param query Queries to be added to the request. Ex. list(SchoolYearID = 1). Defaults to NULL.
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept MTSS
	#' @return A dataframe or of ThresholdRule
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	getThresholdRule <- function(ThresholdRuleID, ThresholdID = F, ThresholdRuleGroupID = F, NumberOfOccurrences = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, ThresholdRuleIDClonedFrom = F, ThresholdRuleIDClonedTo = F, fieldPaths = NULL, entityId = 1, query = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment()) %>% purrr::keep(names(.) != "ThresholdRuleID")

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		getSkyObject(module = "MTSS", objectName = "ThresholdRule", objectId = ThresholdRuleID, searchFields = searchFields, entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' Delete a ThresholdRule
	#'
	#' This function deletes a ThresholdRule
	#' @param ThresholdRuleID The ID of the ThresholdRule to delete
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param query Queries to be added to the request. Ex. list(SchoolYearID = 1). Defaults to NULL.
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept MTSS
	#' @return The ThresholdRuleID of the deleted ThresholdRule.
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	deleteThresholdRule <- function(ThresholdRuleID, ignoreWarnings = F, entityId = 1, query = NULL, flatten = T, returnResponse = F){

		deleteSkyObject(module = "MTSS", objectName = "ThresholdRule", objectId = ThresholdRuleID, ignoreWarnings = ignoreWarnings, entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' Create a ThresholdRule
	#'
	#' This function creates a ThresholdRule
	#' @param fieldNames The field values to give the created ThresholdRule. Each defaults to NULL.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param query Queries to be added to the request. Ex. list(SchoolYearID = 1). Defaults to NULL.
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept MTSS
	#' @return A newly created ThresholdRule
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	createThresholdRule <- function(ThresholdID = NULL, ThresholdRuleGroupID = NULL, NumberOfOccurrences = NULL, ThresholdRuleIDClonedFrom = NULL, entityId = 1, query = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment())

		body <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper()) %>% purrr::compact()

		createSkyObject(module = "MTSS", objectName = "ThresholdRule", body = list(DataObject = body), searchFields = append("ThresholdRuleID", body %>% names()), entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' Modify a ThresholdRule
	#'
	#' This function modifies a ThresholdRule
	#' @param fieldNames The field values to give the modified ThresholdRule. Each defaults to NULL.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param query Queries to be added to the request. Ex. list(SchoolYearID = 1). Defaults to NULL.
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept MTSS
	#' @return The modified ThresholdRule
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	modifyThresholdRule <- function(ThresholdRuleID, ThresholdID = NULL, ThresholdRuleGroupID = NULL, NumberOfOccurrences = NULL, ThresholdRuleIDClonedFrom = NULL, entityId = 1, query = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment())

		body <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper()) %>% purrr::compact()

		modifySkyObject(module = "MTSS", objectName = "ThresholdRule", objectId = ThresholdRuleID, body = list(DataObject = body), searchFields = append("ThresholdRuleID", body %>% names()), entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' List RuleConditions
	#'
	#' This function returns a dataframe or json object of RuleConditions
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given RuleConditions. Defaults to FALSE for all return fields which, for convenience, returns all fields for the RuleConditions.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object listed as text. Run \code{\link{getSchemaForObjects}}('RuleCondition') to get more field paths.
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
	#' @concept MTSS
	#' @return A list of RuleConditions
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	listRuleConditions <- function(searchConditionsList = NULL, RuleConditionID = F, ThresholdRuleGroupID = F, WatchlistConditionID = F, Value = F, OperationType = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, RuleConditionIDClonedFrom = F, RuleConditionIDClonedTo = F, fieldPaths = NULL, searchConditionsGroupType = "And", searchSortFieldNamesList = NULL, searchSortFieldNamesDescendingList = NULL, entityId = 1, query = NULL, page = 1, pageSize = 100000, flatten = T, returnResponse = F){

		params <- as.list(environment())

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		listSkyObjects(module = "MTSS", objectName = "RuleCondition", searchFields = searchFields %>% append(fieldPaths), page = page, pageSize = pageSize, SearchConditionsList = searchConditionsList, SearchConditionsGroupType = searchConditionsGroupType, SearchSortFieldNamesList = searchSortFieldNamesList, SearchSortFieldNamesDescendingList = searchSortFieldNamesDescendingList, entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' Get a RuleCondition
	#'
	#' This function returns a dataframe or json object of a RuleCondition
	#' @param RuleConditionID The ID of the RuleCondition to return.
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given RuleCondition. Defaults to FALSE for all return fields which, for convenience, returns all fields for the RuleCondition.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object listed as text. Run \code{\link{getSchemaForObjects}}('RuleCondition') to get more field paths.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param query Queries to be added to the request. Ex. list(SchoolYearID = 1). Defaults to NULL.
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept MTSS
	#' @return A dataframe or of RuleCondition
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	getRuleCondition <- function(RuleConditionID, ThresholdRuleGroupID = F, WatchlistConditionID = F, Value = F, OperationType = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, RuleConditionIDClonedFrom = F, RuleConditionIDClonedTo = F, fieldPaths = NULL, entityId = 1, query = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment()) %>% purrr::keep(names(.) != "RuleConditionID")

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		getSkyObject(module = "MTSS", objectName = "RuleCondition", objectId = RuleConditionID, searchFields = searchFields, entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' Delete a RuleCondition
	#'
	#' This function deletes a RuleCondition
	#' @param RuleConditionID The ID of the RuleCondition to delete
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param query Queries to be added to the request. Ex. list(SchoolYearID = 1). Defaults to NULL.
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept MTSS
	#' @return The RuleConditionID of the deleted RuleCondition.
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	deleteRuleCondition <- function(RuleConditionID, ignoreWarnings = F, entityId = 1, query = NULL, flatten = T, returnResponse = F){

		deleteSkyObject(module = "MTSS", objectName = "RuleCondition", objectId = RuleConditionID, ignoreWarnings = ignoreWarnings, entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' Create a RuleCondition
	#'
	#' This function creates a RuleCondition
	#' @param fieldNames The field values to give the created RuleCondition. Each defaults to NULL.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param query Queries to be added to the request. Ex. list(SchoolYearID = 1). Defaults to NULL.
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept MTSS
	#' @return A newly created RuleCondition
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	createRuleCondition <- function(ThresholdRuleGroupID = NULL, WatchlistConditionID = NULL, Value = NULL, OperationType = NULL, RuleConditionIDClonedFrom = NULL, entityId = 1, query = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment())

		body <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper()) %>% purrr::compact()

		createSkyObject(module = "MTSS", objectName = "RuleCondition", body = list(DataObject = body), searchFields = append("RuleConditionID", body %>% names()), entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' Modify a RuleCondition
	#'
	#' This function modifies a RuleCondition
	#' @param fieldNames The field values to give the modified RuleCondition. Each defaults to NULL.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param query Queries to be added to the request. Ex. list(SchoolYearID = 1). Defaults to NULL.
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept MTSS
	#' @return The modified RuleCondition
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	modifyRuleCondition <- function(RuleConditionID, ThresholdRuleGroupID = NULL, WatchlistConditionID = NULL, Value = NULL, OperationType = NULL, RuleConditionIDClonedFrom = NULL, entityId = 1, query = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment())

		body <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper()) %>% purrr::compact()

		modifySkyObject(module = "MTSS", objectName = "RuleCondition", objectId = RuleConditionID, body = list(DataObject = body), searchFields = append("RuleConditionID", body %>% names()), entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' List ThresholdRuleGroups
	#'
	#' This function returns a dataframe or json object of ThresholdRuleGroups
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given ThresholdRuleGroups. Defaults to FALSE for all return fields which, for convenience, returns all fields for the ThresholdRuleGroups.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object listed as text. Run \code{\link{getSchemaForObjects}}('ThresholdRuleGroup') to get more field paths.
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
	#' @concept MTSS
	#' @return A list of ThresholdRuleGroups
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	listThresholdRuleGroups <- function(searchConditionsList = NULL, ThresholdRuleGroupID = F, Name = F, EntityID = F, SchoolYearID = F, RuleType = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, ThresholdRuleGroupIDClonedFrom = F, ThresholdRuleGroupIDClonedTo = F, fieldPaths = NULL, searchConditionsGroupType = "And", searchSortFieldNamesList = NULL, searchSortFieldNamesDescendingList = NULL, entityId = 1, query = NULL, page = 1, pageSize = 100000, flatten = T, returnResponse = F){

		params <- as.list(environment())

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		listSkyObjects(module = "MTSS", objectName = "ThresholdRuleGroup", searchFields = searchFields %>% append(fieldPaths), page = page, pageSize = pageSize, SearchConditionsList = searchConditionsList, SearchConditionsGroupType = searchConditionsGroupType, SearchSortFieldNamesList = searchSortFieldNamesList, SearchSortFieldNamesDescendingList = searchSortFieldNamesDescendingList, entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' Get a ThresholdRuleGroup
	#'
	#' This function returns a dataframe or json object of a ThresholdRuleGroup
	#' @param ThresholdRuleGroupID The ID of the ThresholdRuleGroup to return.
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given ThresholdRuleGroup. Defaults to FALSE for all return fields which, for convenience, returns all fields for the ThresholdRuleGroup.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object listed as text. Run \code{\link{getSchemaForObjects}}('ThresholdRuleGroup') to get more field paths.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param query Queries to be added to the request. Ex. list(SchoolYearID = 1). Defaults to NULL.
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept MTSS
	#' @return A dataframe or of ThresholdRuleGroup
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	getThresholdRuleGroup <- function(ThresholdRuleGroupID, Name = F, EntityID = F, SchoolYearID = F, RuleType = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, ThresholdRuleGroupIDClonedFrom = F, ThresholdRuleGroupIDClonedTo = F, fieldPaths = NULL, entityId = 1, query = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment()) %>% purrr::keep(names(.) != "ThresholdRuleGroupID")

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		getSkyObject(module = "MTSS", objectName = "ThresholdRuleGroup", objectId = ThresholdRuleGroupID, searchFields = searchFields, entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' Delete a ThresholdRuleGroup
	#'
	#' This function deletes a ThresholdRuleGroup
	#' @param ThresholdRuleGroupID The ID of the ThresholdRuleGroup to delete
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param query Queries to be added to the request. Ex. list(SchoolYearID = 1). Defaults to NULL.
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept MTSS
	#' @return The ThresholdRuleGroupID of the deleted ThresholdRuleGroup.
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	deleteThresholdRuleGroup <- function(ThresholdRuleGroupID, ignoreWarnings = F, entityId = 1, query = NULL, flatten = T, returnResponse = F){

		deleteSkyObject(module = "MTSS", objectName = "ThresholdRuleGroup", objectId = ThresholdRuleGroupID, ignoreWarnings = ignoreWarnings, entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' Create a ThresholdRuleGroup
	#'
	#' This function creates a ThresholdRuleGroup
	#' @param fieldNames The field values to give the created ThresholdRuleGroup. Each defaults to NULL.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param query Queries to be added to the request. Ex. list(SchoolYearID = 1). Defaults to NULL.
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept MTSS
	#' @return A newly created ThresholdRuleGroup
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	createThresholdRuleGroup <- function(Name = NULL, EntityID = NULL, SchoolYearID = NULL, RuleType = NULL, ThresholdRuleGroupIDClonedFrom = NULL, entityId = 1, query = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment())

		body <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper()) %>% purrr::compact()

		createSkyObject(module = "MTSS", objectName = "ThresholdRuleGroup", body = list(DataObject = body), searchFields = append("ThresholdRuleGroupID", body %>% names()), entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' Modify a ThresholdRuleGroup
	#'
	#' This function modifies a ThresholdRuleGroup
	#' @param fieldNames The field values to give the modified ThresholdRuleGroup. Each defaults to NULL.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param query Queries to be added to the request. Ex. list(SchoolYearID = 1). Defaults to NULL.
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept MTSS
	#' @return The modified ThresholdRuleGroup
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	modifyThresholdRuleGroup <- function(ThresholdRuleGroupID, Name = NULL, EntityID = NULL, SchoolYearID = NULL, RuleType = NULL, ThresholdRuleGroupIDClonedFrom = NULL, entityId = 1, query = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment())

		body <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper()) %>% purrr::compact()

		modifySkyObject(module = "MTSS", objectName = "ThresholdRuleGroup", objectId = ThresholdRuleGroupID, body = list(DataObject = body), searchFields = append("ThresholdRuleGroupID", body %>% names()), entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' List Thresholds
	#'
	#' This function returns a dataframe or json object of Thresholds
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given Thresholds. Defaults to FALSE for all return fields which, for convenience, returns all fields for the Thresholds.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object listed as text. Run \code{\link{getSchemaForObjects}}('Threshold') to get more field paths.
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
	#' @concept MTSS
	#' @return A list of Thresholds
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	listThresholds <- function(searchConditionsList = NULL, ThresholdID = F, WatchlistThresholdID = F, TierID = F, Name = F, Description = F, NumberOfRulesToMeet = F, Rank = F, RulesToMeet = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, ThresholdIDClonedFrom = F, ThresholdIDClonedTo = F, fieldPaths = NULL, searchConditionsGroupType = "And", searchSortFieldNamesList = NULL, searchSortFieldNamesDescendingList = NULL, entityId = 1, query = NULL, page = 1, pageSize = 100000, flatten = T, returnResponse = F){

		params <- as.list(environment())

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		listSkyObjects(module = "MTSS", objectName = "Threshold", searchFields = searchFields %>% append(fieldPaths), page = page, pageSize = pageSize, SearchConditionsList = searchConditionsList, SearchConditionsGroupType = searchConditionsGroupType, SearchSortFieldNamesList = searchSortFieldNamesList, SearchSortFieldNamesDescendingList = searchSortFieldNamesDescendingList, entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' Get a Threshold
	#'
	#' This function returns a dataframe or json object of a Threshold
	#' @param ThresholdID The ID of the Threshold to return.
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given Threshold. Defaults to FALSE for all return fields which, for convenience, returns all fields for the Threshold.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object listed as text. Run \code{\link{getSchemaForObjects}}('Threshold') to get more field paths.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param query Queries to be added to the request. Ex. list(SchoolYearID = 1). Defaults to NULL.
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept MTSS
	#' @return A dataframe or of Threshold
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	getThreshold <- function(ThresholdID, WatchlistThresholdID = F, TierID = F, Name = F, Description = F, NumberOfRulesToMeet = F, Rank = F, RulesToMeet = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, ThresholdIDClonedFrom = F, ThresholdIDClonedTo = F, fieldPaths = NULL, entityId = 1, query = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment()) %>% purrr::keep(names(.) != "ThresholdID")

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		getSkyObject(module = "MTSS", objectName = "Threshold", objectId = ThresholdID, searchFields = searchFields, entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' Delete a Threshold
	#'
	#' This function deletes a Threshold
	#' @param ThresholdID The ID of the Threshold to delete
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param query Queries to be added to the request. Ex. list(SchoolYearID = 1). Defaults to NULL.
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept MTSS
	#' @return The ThresholdID of the deleted Threshold.
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	deleteThreshold <- function(ThresholdID, ignoreWarnings = F, entityId = 1, query = NULL, flatten = T, returnResponse = F){

		deleteSkyObject(module = "MTSS", objectName = "Threshold", objectId = ThresholdID, ignoreWarnings = ignoreWarnings, entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' Create a Threshold
	#'
	#' This function creates a Threshold
	#' @param fieldNames The field values to give the created Threshold. Each defaults to NULL.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param query Queries to be added to the request. Ex. list(SchoolYearID = 1). Defaults to NULL.
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept MTSS
	#' @return A newly created Threshold
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	createThreshold <- function(WatchlistThresholdID = NULL, TierID = NULL, Name = NULL, Description = NULL, NumberOfRulesToMeet = NULL, Rank = NULL, RulesToMeet = NULL, ThresholdIDClonedFrom = NULL, entityId = 1, query = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment())

		body <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper()) %>% purrr::compact()

		createSkyObject(module = "MTSS", objectName = "Threshold", body = list(DataObject = body), searchFields = append("ThresholdID", body %>% names()), entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' Modify a Threshold
	#'
	#' This function modifies a Threshold
	#' @param fieldNames The field values to give the modified Threshold. Each defaults to NULL.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param query Queries to be added to the request. Ex. list(SchoolYearID = 1). Defaults to NULL.
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept MTSS
	#' @return The modified Threshold
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	modifyThreshold <- function(ThresholdID, WatchlistThresholdID = NULL, TierID = NULL, Name = NULL, Description = NULL, NumberOfRulesToMeet = NULL, Rank = NULL, RulesToMeet = NULL, ThresholdIDClonedFrom = NULL, entityId = 1, query = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment())

		body <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper()) %>% purrr::compact()

		modifySkyObject(module = "MTSS", objectName = "Threshold", objectId = ThresholdID, body = list(DataObject = body), searchFields = append("ThresholdID", body %>% names()), entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' List Watchlists
	#'
	#' This function returns a dataframe or json object of Watchlists
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given Watchlists. Defaults to FALSE for all return fields which, for convenience, returns all fields for the Watchlists.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object listed as text. Run \code{\link{getSchemaForObjects}}('Watchlist') to get more field paths.
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
	#' @concept MTSS
	#' @return A list of Watchlists
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	listWatchlists <- function(searchConditionsList = NULL, WatchlistID = F, StudentID = F, ThresholdID = F, LastActiveDate = F, IsActive = F, PlanCreated = F, ReferralCreated = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, fieldPaths = NULL, searchConditionsGroupType = "And", searchSortFieldNamesList = NULL, searchSortFieldNamesDescendingList = NULL, entityId = 1, query = NULL, page = 1, pageSize = 100000, flatten = T, returnResponse = F){

		params <- as.list(environment())

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		listSkyObjects(module = "MTSS", objectName = "Watchlist", searchFields = searchFields %>% append(fieldPaths), page = page, pageSize = pageSize, SearchConditionsList = searchConditionsList, SearchConditionsGroupType = searchConditionsGroupType, SearchSortFieldNamesList = searchSortFieldNamesList, SearchSortFieldNamesDescendingList = searchSortFieldNamesDescendingList, entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' Get a Watchlist
	#'
	#' This function returns a dataframe or json object of a Watchlist
	#' @param WatchlistID The ID of the Watchlist to return.
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given Watchlist. Defaults to FALSE for all return fields which, for convenience, returns all fields for the Watchlist.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object listed as text. Run \code{\link{getSchemaForObjects}}('Watchlist') to get more field paths.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param query Queries to be added to the request. Ex. list(SchoolYearID = 1). Defaults to NULL.
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept MTSS
	#' @return A dataframe or of Watchlist
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	getWatchlist <- function(WatchlistID, StudentID = F, ThresholdID = F, LastActiveDate = F, IsActive = F, PlanCreated = F, ReferralCreated = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, fieldPaths = NULL, entityId = 1, query = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment()) %>% purrr::keep(names(.) != "WatchlistID")

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		getSkyObject(module = "MTSS", objectName = "Watchlist", objectId = WatchlistID, searchFields = searchFields, entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' Delete a Watchlist
	#'
	#' This function deletes a Watchlist
	#' @param WatchlistID The ID of the Watchlist to delete
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param query Queries to be added to the request. Ex. list(SchoolYearID = 1). Defaults to NULL.
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept MTSS
	#' @return The WatchlistID of the deleted Watchlist.
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	deleteWatchlist <- function(WatchlistID, ignoreWarnings = F, entityId = 1, query = NULL, flatten = T, returnResponse = F){

		deleteSkyObject(module = "MTSS", objectName = "Watchlist", objectId = WatchlistID, ignoreWarnings = ignoreWarnings, entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' Create a Watchlist
	#'
	#' This function creates a Watchlist
	#' @param fieldNames The field values to give the created Watchlist. Each defaults to NULL.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param query Queries to be added to the request. Ex. list(SchoolYearID = 1). Defaults to NULL.
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept MTSS
	#' @return A newly created Watchlist
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	createWatchlist <- function(StudentID = NULL, ThresholdID = NULL, LastActiveDate = NULL, IsActive = NULL, entityId = 1, query = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment())

		body <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper()) %>% purrr::compact()

		createSkyObject(module = "MTSS", objectName = "Watchlist", body = list(DataObject = body), searchFields = append("WatchlistID", body %>% names()), entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' Modify a Watchlist
	#'
	#' This function modifies a Watchlist
	#' @param fieldNames The field values to give the modified Watchlist. Each defaults to NULL.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param query Queries to be added to the request. Ex. list(SchoolYearID = 1). Defaults to NULL.
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept MTSS
	#' @return The modified Watchlist
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	modifyWatchlist <- function(WatchlistID, StudentID = NULL, ThresholdID = NULL, LastActiveDate = NULL, IsActive = NULL, entityId = 1, query = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment())

		body <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper()) %>% purrr::compact()

		modifySkyObject(module = "MTSS", objectName = "Watchlist", objectId = WatchlistID, body = list(DataObject = body), searchFields = append("WatchlistID", body %>% names()), entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' List TempInterventionPlans
	#'
	#' This function returns a dataframe or json object of TempInterventionPlans
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given TempInterventionPlans. Defaults to FALSE for all return fields which, for convenience, returns all fields for the TempInterventionPlans.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object listed as text. Run \code{\link{getSchemaForObjects}}('TempInterventionPlan') to get more field paths.
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
	#' @concept MTSS
	#' @return A list of TempInterventionPlans
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	listTempInterventionPlans <- function(searchConditionsList = NULL, TempInterventionPlanID = F, InterventionPlanID = F, StudentNameLFM = F, StudentNumber = F, AreaOfConcernDescription = F, InterventionPlanStartDate = F, TierDescription = F, ObjectiveDescription = F, OutcomeDescription = F, HasExceptions = F, Exceptions = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, StudentID = F, Threshold = F, Duration = F, MeetingTime = F, StartingGoal = F, QualitativeScaleMappingIDStarting = F, TargetGoal = F, QualitativeScaleMappingIDTarget = F, WatchlistID = F, IsSelected = F, ErrorCount = F, fieldPaths = NULL, searchConditionsGroupType = "And", searchSortFieldNamesList = NULL, searchSortFieldNamesDescendingList = NULL, entityId = 1, query = NULL, page = 1, pageSize = 100000, flatten = T, returnResponse = F){

		params <- as.list(environment())

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		listSkyObjects(module = "MTSS", objectName = "TempInterventionPlan", searchFields = searchFields %>% append(fieldPaths), page = page, pageSize = pageSize, SearchConditionsList = searchConditionsList, SearchConditionsGroupType = searchConditionsGroupType, SearchSortFieldNamesList = searchSortFieldNamesList, SearchSortFieldNamesDescendingList = searchSortFieldNamesDescendingList, entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' Get a TempInterventionPlan
	#'
	#' This function returns a dataframe or json object of a TempInterventionPlan
	#' @param TempInterventionPlanID The ID of the TempInterventionPlan to return.
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given TempInterventionPlan. Defaults to FALSE for all return fields which, for convenience, returns all fields for the TempInterventionPlan.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object listed as text. Run \code{\link{getSchemaForObjects}}('TempInterventionPlan') to get more field paths.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param query Queries to be added to the request. Ex. list(SchoolYearID = 1). Defaults to NULL.
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept MTSS
	#' @return A dataframe or of TempInterventionPlan
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	getTempInterventionPlan <- function(TempInterventionPlanID, InterventionPlanID = F, StudentNameLFM = F, StudentNumber = F, AreaOfConcernDescription = F, InterventionPlanStartDate = F, TierDescription = F, ObjectiveDescription = F, OutcomeDescription = F, HasExceptions = F, Exceptions = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, StudentID = F, Threshold = F, Duration = F, MeetingTime = F, StartingGoal = F, QualitativeScaleMappingIDStarting = F, TargetGoal = F, QualitativeScaleMappingIDTarget = F, WatchlistID = F, IsSelected = F, ErrorCount = F, fieldPaths = NULL, entityId = 1, query = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment()) %>% purrr::keep(names(.) != "TempInterventionPlanID")

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		getSkyObject(module = "MTSS", objectName = "TempInterventionPlan", objectId = TempInterventionPlanID, searchFields = searchFields, entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' Delete a TempInterventionPlan
	#'
	#' This function deletes a TempInterventionPlan
	#' @param TempInterventionPlanID The ID of the TempInterventionPlan to delete
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param query Queries to be added to the request. Ex. list(SchoolYearID = 1). Defaults to NULL.
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept MTSS
	#' @return The TempInterventionPlanID of the deleted TempInterventionPlan.
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	deleteTempInterventionPlan <- function(TempInterventionPlanID, ignoreWarnings = F, entityId = 1, query = NULL, flatten = T, returnResponse = F){

		deleteSkyObject(module = "MTSS", objectName = "TempInterventionPlan", objectId = TempInterventionPlanID, ignoreWarnings = ignoreWarnings, entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' Create a TempInterventionPlan
	#'
	#' This function creates a TempInterventionPlan
	#' @param fieldNames The field values to give the created TempInterventionPlan. Each defaults to NULL.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param query Queries to be added to the request. Ex. list(SchoolYearID = 1). Defaults to NULL.
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept MTSS
	#' @return A newly created TempInterventionPlan
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	createTempInterventionPlan <- function(StudentNameLFM = NULL, StudentNumber = NULL, AreaOfConcernDescription = NULL, InterventionPlanStartDate = NULL, TierDescription = NULL, ObjectiveDescription = NULL, OutcomeDescription = NULL, Exceptions = NULL, Threshold = NULL, Duration = NULL, MeetingTime = NULL, StartingGoal = NULL, QualitativeScaleMappingIDStarting = NULL, TargetGoal = NULL, QualitativeScaleMappingIDTarget = NULL, IsSelected = NULL, ErrorCount = NULL, entityId = 1, query = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment())

		body <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper()) %>% purrr::compact()

		createSkyObject(module = "MTSS", objectName = "TempInterventionPlan", body = list(DataObject = body), searchFields = append("TempInterventionPlanID", body %>% names()), entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' Modify a TempInterventionPlan
	#'
	#' This function modifies a TempInterventionPlan
	#' @param fieldNames The field values to give the modified TempInterventionPlan. Each defaults to NULL.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param query Queries to be added to the request. Ex. list(SchoolYearID = 1). Defaults to NULL.
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept MTSS
	#' @return The modified TempInterventionPlan
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	modifyTempInterventionPlan <- function(TempInterventionPlanID, StudentNameLFM = NULL, StudentNumber = NULL, AreaOfConcernDescription = NULL, InterventionPlanStartDate = NULL, TierDescription = NULL, ObjectiveDescription = NULL, OutcomeDescription = NULL, Exceptions = NULL, Threshold = NULL, Duration = NULL, MeetingTime = NULL, StartingGoal = NULL, QualitativeScaleMappingIDStarting = NULL, TargetGoal = NULL, QualitativeScaleMappingIDTarget = NULL, IsSelected = NULL, ErrorCount = NULL, entityId = 1, query = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment())

		body <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper()) %>% purrr::compact()

		modifySkyObject(module = "MTSS", objectName = "TempInterventionPlan", objectId = TempInterventionPlanID, body = list(DataObject = body), searchFields = append("TempInterventionPlanID", body %>% names()), entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' List TempInterventionTeamStaffErrors
	#'
	#' This function returns a dataframe or json object of TempInterventionTeamStaffErrors
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given TempInterventionTeamStaffErrors. Defaults to FALSE for all return fields which, for convenience, returns all fields for the TempInterventionTeamStaffErrors.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object listed as text. Run \code{\link{getSchemaForObjects}}('TempInterventionTeamStaffError') to get more field paths.
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
	#' @concept MTSS
	#' @return A list of TempInterventionTeamStaffErrors
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	listTempInterventionTeamStaffErrors <- function(searchConditionsList = NULL, TempInterventionTeamStaffErrorID = F, StaffNameLFM = F, ErrorMessage = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, fieldPaths = NULL, searchConditionsGroupType = "And", searchSortFieldNamesList = NULL, searchSortFieldNamesDescendingList = NULL, entityId = 1, query = NULL, page = 1, pageSize = 100000, flatten = T, returnResponse = F){

		params <- as.list(environment())

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		listSkyObjects(module = "MTSS", objectName = "TempInterventionTeamStaffError", searchFields = searchFields %>% append(fieldPaths), page = page, pageSize = pageSize, SearchConditionsList = searchConditionsList, SearchConditionsGroupType = searchConditionsGroupType, SearchSortFieldNamesList = searchSortFieldNamesList, SearchSortFieldNamesDescendingList = searchSortFieldNamesDescendingList, entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' Get a TempInterventionTeamStaffError
	#'
	#' This function returns a dataframe or json object of a TempInterventionTeamStaffError
	#' @param TempInterventionTeamStaffErrorID The ID of the TempInterventionTeamStaffError to return.
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given TempInterventionTeamStaffError. Defaults to FALSE for all return fields which, for convenience, returns all fields for the TempInterventionTeamStaffError.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object listed as text. Run \code{\link{getSchemaForObjects}}('TempInterventionTeamStaffError') to get more field paths.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param query Queries to be added to the request. Ex. list(SchoolYearID = 1). Defaults to NULL.
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept MTSS
	#' @return A dataframe or of TempInterventionTeamStaffError
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	getTempInterventionTeamStaffError <- function(TempInterventionTeamStaffErrorID, StaffNameLFM = F, ErrorMessage = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, fieldPaths = NULL, entityId = 1, query = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment()) %>% purrr::keep(names(.) != "TempInterventionTeamStaffErrorID")

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		getSkyObject(module = "MTSS", objectName = "TempInterventionTeamStaffError", objectId = TempInterventionTeamStaffErrorID, searchFields = searchFields, entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' Delete a TempInterventionTeamStaffError
	#'
	#' This function deletes a TempInterventionTeamStaffError
	#' @param TempInterventionTeamStaffErrorID The ID of the TempInterventionTeamStaffError to delete
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param query Queries to be added to the request. Ex. list(SchoolYearID = 1). Defaults to NULL.
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept MTSS
	#' @return The TempInterventionTeamStaffErrorID of the deleted TempInterventionTeamStaffError.
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	deleteTempInterventionTeamStaffError <- function(TempInterventionTeamStaffErrorID, ignoreWarnings = F, entityId = 1, query = NULL, flatten = T, returnResponse = F){

		deleteSkyObject(module = "MTSS", objectName = "TempInterventionTeamStaffError", objectId = TempInterventionTeamStaffErrorID, ignoreWarnings = ignoreWarnings, entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' Create a TempInterventionTeamStaffError
	#'
	#' This function creates a TempInterventionTeamStaffError
	#' @param fieldNames The field values to give the created TempInterventionTeamStaffError. Each defaults to NULL.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param query Queries to be added to the request. Ex. list(SchoolYearID = 1). Defaults to NULL.
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept MTSS
	#' @return A newly created TempInterventionTeamStaffError
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	createTempInterventionTeamStaffError <- function(StaffNameLFM = NULL, ErrorMessage = NULL, entityId = 1, query = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment())

		body <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper()) %>% purrr::compact()

		createSkyObject(module = "MTSS", objectName = "TempInterventionTeamStaffError", body = list(DataObject = body), searchFields = append("TempInterventionTeamStaffErrorID", body %>% names()), entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' Modify a TempInterventionTeamStaffError
	#'
	#' This function modifies a TempInterventionTeamStaffError
	#' @param fieldNames The field values to give the modified TempInterventionTeamStaffError. Each defaults to NULL.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param query Queries to be added to the request. Ex. list(SchoolYearID = 1). Defaults to NULL.
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept MTSS
	#' @return The modified TempInterventionTeamStaffError
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	modifyTempInterventionTeamStaffError <- function(TempInterventionTeamStaffErrorID, StaffNameLFM = NULL, ErrorMessage = NULL, entityId = 1, query = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment())

		body <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper()) %>% purrr::compact()

		modifySkyObject(module = "MTSS", objectName = "TempInterventionTeamStaffError", objectId = TempInterventionTeamStaffErrorID, body = list(DataObject = body), searchFields = append("TempInterventionTeamStaffErrorID", body %>% names()), entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' List TempInterventionPlanStaffs
	#'
	#' This function returns a dataframe or json object of TempInterventionPlanStaffs
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given TempInterventionPlanStaffs. Defaults to FALSE for all return fields which, for convenience, returns all fields for the TempInterventionPlanStaffs.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object listed as text. Run \code{\link{getSchemaForObjects}}('TempInterventionPlanStaff') to get more field paths.
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
	#' @concept MTSS
	#' @return A list of TempInterventionPlanStaffs
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	listTempInterventionPlanStaffs <- function(searchConditionsList = NULL, TempInterventionPlanStaffID = F, InterventionPlanStaffID = F, InterventionTeamStaffID = F, InterventionPlanID = F, StaffID = F, StaffNameLFM = F, NewIsProvidedBy = F, NewIsActive = F, OldIsProvidedBy = F, OldIsActive = F, StudentNameLFM = F, StudentNumber = F, AreaOfConcern = F, ErrorMessage = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, fieldPaths = NULL, searchConditionsGroupType = "And", searchSortFieldNamesList = NULL, searchSortFieldNamesDescendingList = NULL, entityId = 1, query = NULL, page = 1, pageSize = 100000, flatten = T, returnResponse = F){

		params <- as.list(environment())

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		listSkyObjects(module = "MTSS", objectName = "TempInterventionPlanStaff", searchFields = searchFields %>% append(fieldPaths), page = page, pageSize = pageSize, SearchConditionsList = searchConditionsList, SearchConditionsGroupType = searchConditionsGroupType, SearchSortFieldNamesList = searchSortFieldNamesList, SearchSortFieldNamesDescendingList = searchSortFieldNamesDescendingList, entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' Get a TempInterventionPlanStaff
	#'
	#' This function returns a dataframe or json object of a TempInterventionPlanStaff
	#' @param TempInterventionPlanStaffID The ID of the TempInterventionPlanStaff to return.
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given TempInterventionPlanStaff. Defaults to FALSE for all return fields which, for convenience, returns all fields for the TempInterventionPlanStaff.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object listed as text. Run \code{\link{getSchemaForObjects}}('TempInterventionPlanStaff') to get more field paths.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param query Queries to be added to the request. Ex. list(SchoolYearID = 1). Defaults to NULL.
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept MTSS
	#' @return A dataframe or of TempInterventionPlanStaff
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	getTempInterventionPlanStaff <- function(TempInterventionPlanStaffID, InterventionPlanStaffID = F, InterventionTeamStaffID = F, InterventionPlanID = F, StaffID = F, StaffNameLFM = F, NewIsProvidedBy = F, NewIsActive = F, OldIsProvidedBy = F, OldIsActive = F, StudentNameLFM = F, StudentNumber = F, AreaOfConcern = F, ErrorMessage = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, fieldPaths = NULL, entityId = 1, query = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment()) %>% purrr::keep(names(.) != "TempInterventionPlanStaffID")

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		getSkyObject(module = "MTSS", objectName = "TempInterventionPlanStaff", objectId = TempInterventionPlanStaffID, searchFields = searchFields, entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' Delete a TempInterventionPlanStaff
	#'
	#' This function deletes a TempInterventionPlanStaff
	#' @param TempInterventionPlanStaffID The ID of the TempInterventionPlanStaff to delete
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param query Queries to be added to the request. Ex. list(SchoolYearID = 1). Defaults to NULL.
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept MTSS
	#' @return The TempInterventionPlanStaffID of the deleted TempInterventionPlanStaff.
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	deleteTempInterventionPlanStaff <- function(TempInterventionPlanStaffID, ignoreWarnings = F, entityId = 1, query = NULL, flatten = T, returnResponse = F){

		deleteSkyObject(module = "MTSS", objectName = "TempInterventionPlanStaff", objectId = TempInterventionPlanStaffID, ignoreWarnings = ignoreWarnings, entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' Create a TempInterventionPlanStaff
	#'
	#' This function creates a TempInterventionPlanStaff
	#' @param fieldNames The field values to give the created TempInterventionPlanStaff. Each defaults to NULL.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param query Queries to be added to the request. Ex. list(SchoolYearID = 1). Defaults to NULL.
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept MTSS
	#' @return A newly created TempInterventionPlanStaff
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	createTempInterventionPlanStaff <- function(StaffNameLFM = NULL, NewIsProvidedBy = NULL, NewIsActive = NULL, OldIsProvidedBy = NULL, OldIsActive = NULL, StudentNameLFM = NULL, StudentNumber = NULL, AreaOfConcern = NULL, ErrorMessage = NULL, entityId = 1, query = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment())

		body <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper()) %>% purrr::compact()

		createSkyObject(module = "MTSS", objectName = "TempInterventionPlanStaff", body = list(DataObject = body), searchFields = append("TempInterventionPlanStaffID", body %>% names()), entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' Modify a TempInterventionPlanStaff
	#'
	#' This function modifies a TempInterventionPlanStaff
	#' @param fieldNames The field values to give the modified TempInterventionPlanStaff. Each defaults to NULL.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param query Queries to be added to the request. Ex. list(SchoolYearID = 1). Defaults to NULL.
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept MTSS
	#' @return The modified TempInterventionPlanStaff
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	modifyTempInterventionPlanStaff <- function(TempInterventionPlanStaffID, StaffNameLFM = NULL, NewIsProvidedBy = NULL, NewIsActive = NULL, OldIsProvidedBy = NULL, OldIsActive = NULL, StudentNameLFM = NULL, StudentNumber = NULL, AreaOfConcern = NULL, ErrorMessage = NULL, entityId = 1, query = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment())

		body <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper()) %>% purrr::compact()

		modifySkyObject(module = "MTSS", objectName = "TempInterventionPlanStaff", objectId = TempInterventionPlanStaffID, body = list(DataObject = body), searchFields = append("TempInterventionPlanStaffID", body %>% names()), entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' List StrategyEntities
	#'
	#' This function returns a dataframe or json object of StrategyEntities
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given StrategyEntities. Defaults to FALSE for all return fields which, for convenience, returns all fields for the StrategyEntities.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object listed as text. Run \code{\link{getSchemaForObjects}}('StrategyEntity') to get more field paths.
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
	#' @concept MTSS
	#' @return A list of StrategyEntities
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	listStrategyEntities <- function(searchConditionsList = NULL, StrategyEntityID = F, StrategyID = F, EntityID = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, fieldPaths = NULL, searchConditionsGroupType = "And", searchSortFieldNamesList = NULL, searchSortFieldNamesDescendingList = NULL, entityId = 1, query = NULL, page = 1, pageSize = 100000, flatten = T, returnResponse = F){

		params <- as.list(environment())

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		listSkyObjects(module = "MTSS", objectName = "StrategyEntity", searchFields = searchFields %>% append(fieldPaths), page = page, pageSize = pageSize, SearchConditionsList = searchConditionsList, SearchConditionsGroupType = searchConditionsGroupType, SearchSortFieldNamesList = searchSortFieldNamesList, SearchSortFieldNamesDescendingList = searchSortFieldNamesDescendingList, entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' Get a StrategyEntity
	#'
	#' This function returns a dataframe or json object of a StrategyEntity
	#' @param StrategyEntityID The ID of the StrategyEntity to return.
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given StrategyEntity. Defaults to FALSE for all return fields which, for convenience, returns all fields for the StrategyEntity.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object listed as text. Run \code{\link{getSchemaForObjects}}('StrategyEntity') to get more field paths.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param query Queries to be added to the request. Ex. list(SchoolYearID = 1). Defaults to NULL.
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept MTSS
	#' @return A dataframe or of StrategyEntity
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	getStrategyEntity <- function(StrategyEntityID, StrategyID = F, EntityID = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, fieldPaths = NULL, entityId = 1, query = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment()) %>% purrr::keep(names(.) != "StrategyEntityID")

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		getSkyObject(module = "MTSS", objectName = "StrategyEntity", objectId = StrategyEntityID, searchFields = searchFields, entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' Delete a StrategyEntity
	#'
	#' This function deletes a StrategyEntity
	#' @param StrategyEntityID The ID of the StrategyEntity to delete
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param query Queries to be added to the request. Ex. list(SchoolYearID = 1). Defaults to NULL.
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept MTSS
	#' @return The StrategyEntityID of the deleted StrategyEntity.
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	deleteStrategyEntity <- function(StrategyEntityID, ignoreWarnings = F, entityId = 1, query = NULL, flatten = T, returnResponse = F){

		deleteSkyObject(module = "MTSS", objectName = "StrategyEntity", objectId = StrategyEntityID, ignoreWarnings = ignoreWarnings, entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' Create a StrategyEntity
	#'
	#' This function creates a StrategyEntity
	#' @param fieldNames The field values to give the created StrategyEntity. Each defaults to NULL.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param query Queries to be added to the request. Ex. list(SchoolYearID = 1). Defaults to NULL.
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept MTSS
	#' @return A newly created StrategyEntity
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	createStrategyEntity <- function(StrategyID = NULL, EntityID = NULL, entityId = 1, query = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment())

		body <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper()) %>% purrr::compact()

		createSkyObject(module = "MTSS", objectName = "StrategyEntity", body = list(DataObject = body), searchFields = append("StrategyEntityID", body %>% names()), entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' Modify a StrategyEntity
	#'
	#' This function modifies a StrategyEntity
	#' @param fieldNames The field values to give the modified StrategyEntity. Each defaults to NULL.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param query Queries to be added to the request. Ex. list(SchoolYearID = 1). Defaults to NULL.
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept MTSS
	#' @return The modified StrategyEntity
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	modifyStrategyEntity <- function(StrategyEntityID, StrategyID = NULL, EntityID = NULL, entityId = 1, query = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment())

		body <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper()) %>% purrr::compact()

		modifySkyObject(module = "MTSS", objectName = "StrategyEntity", objectId = StrategyEntityID, body = list(DataObject = body), searchFields = append("StrategyEntityID", body %>% names()), entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' List MTSSConfigEntities
	#'
	#' This function returns a dataframe or json object of MTSSConfigEntities
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given MTSSConfigEntities. Defaults to FALSE for all return fields which, for convenience, returns all fields for the MTSSConfigEntities.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object listed as text. Run \code{\link{getSchemaForObjects}}('MTSSConfigEntity') to get more field paths.
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
	#' @concept MTSS
	#' @return A list of MTSSConfigEntities
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	listMTSSConfigEntities <- function(searchConditionsList = NULL, ConfigEntityID = F, EntityID = F, DisplayReferrals = F, DisplayInterventionPlans = F, DisplayWatchlist = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, fieldPaths = NULL, searchConditionsGroupType = "And", searchSortFieldNamesList = NULL, searchSortFieldNamesDescendingList = NULL, entityId = 1, query = NULL, page = 1, pageSize = 100000, flatten = T, returnResponse = F){

		params <- as.list(environment())

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		listSkyObjects(module = "MTSS", objectName = "ConfigEntity", searchFields = searchFields %>% append(fieldPaths), page = page, pageSize = pageSize, SearchConditionsList = searchConditionsList, SearchConditionsGroupType = searchConditionsGroupType, SearchSortFieldNamesList = searchSortFieldNamesList, SearchSortFieldNamesDescendingList = searchSortFieldNamesDescendingList, entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' Get a MTSSConfigEntity
	#'
	#' This function returns a dataframe or json object of a MTSSConfigEntity
	#' @param MTSSConfigEntityID The ID of the MTSSConfigEntity to return.
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given MTSSConfigEntity. Defaults to FALSE for all return fields which, for convenience, returns all fields for the MTSSConfigEntity.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object listed as text. Run \code{\link{getSchemaForObjects}}('MTSSConfigEntity') to get more field paths.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param query Queries to be added to the request. Ex. list(SchoolYearID = 1). Defaults to NULL.
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept MTSS
	#' @return A dataframe or of MTSSConfigEntity
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	getMTSSConfigEntity <- function(MTSSConfigEntityID, ConfigEntityID = F, EntityID = F, DisplayReferrals = F, DisplayInterventionPlans = F, DisplayWatchlist = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, fieldPaths = NULL, entityId = 1, query = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment()) %>% purrr::keep(names(.) != "MTSSConfigEntityID")

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		getSkyObject(module = "MTSS", objectName = "ConfigEntity", objectId = MTSSConfigEntityID, searchFields = searchFields, entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' Delete a MTSSConfigEntity
	#'
	#' This function deletes a MTSSConfigEntity
	#' @param MTSSConfigEntityID The ID of the MTSSConfigEntity to delete
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param query Queries to be added to the request. Ex. list(SchoolYearID = 1). Defaults to NULL.
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept MTSS
	#' @return The MTSSConfigEntityID of the deleted MTSSConfigEntity.
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	deleteMTSSConfigEntity <- function(MTSSConfigEntityID, ignoreWarnings = F, entityId = 1, query = NULL, flatten = T, returnResponse = F){

		deleteSkyObject(module = "MTSS", objectName = "ConfigEntity", objectId = MTSSConfigEntityID, ignoreWarnings = ignoreWarnings, entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' Create a MTSSConfigEntity
	#'
	#' This function creates a MTSSConfigEntity
	#' @param fieldNames The field values to give the created MTSSConfigEntity. Each defaults to NULL.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param query Queries to be added to the request. Ex. list(SchoolYearID = 1). Defaults to NULL.
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept MTSS
	#' @return A newly created MTSSConfigEntity
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	createMTSSConfigEntity <- function(EntityID = NULL, DisplayReferrals = NULL, DisplayInterventionPlans = NULL, DisplayWatchlist = NULL, entityId = 1, query = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment())

		body <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper()) %>% purrr::compact()

		createSkyObject(module = "MTSS", objectName = "ConfigEntity", body = list(DataObject = body), searchFields = append("ConfigEntityID", body %>% names()), entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' Modify a MTSSConfigEntity
	#'
	#' This function modifies a MTSSConfigEntity
	#' @param fieldNames The field values to give the modified MTSSConfigEntity. Each defaults to NULL.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param query Queries to be added to the request. Ex. list(SchoolYearID = 1). Defaults to NULL.
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept MTSS
	#' @return The modified MTSSConfigEntity
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	modifyMTSSConfigEntity <- function(ConfigEntityID, EntityID = NULL, DisplayReferrals = NULL, DisplayInterventionPlans = NULL, DisplayWatchlist = NULL, entityId = 1, query = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment())

		body <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper()) %>% purrr::compact()

		modifySkyObject(module = "MTSS", objectName = "ConfigEntity", objectId = ConfigEntityID, body = list(DataObject = body), searchFields = append("ConfigEntityID", body %>% names()), entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' List TempStudentInterventionReferralErrors
	#'
	#' This function returns a dataframe or json object of TempStudentInterventionReferralErrors
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given TempStudentInterventionReferralErrors. Defaults to FALSE for all return fields which, for convenience, returns all fields for the TempStudentInterventionReferralErrors.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object listed as text. Run \code{\link{getSchemaForObjects}}('TempStudentInterventionReferralError') to get more field paths.
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
	#' @concept MTSS
	#' @return A list of TempStudentInterventionReferralErrors
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	listTempStudentInterventionReferralErrors <- function(searchConditionsList = NULL, TempStudentInterventionReferralErrorID = F, TempStudentInterventionReferralID = F, Error = F, ErrorDetail = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, fieldPaths = NULL, searchConditionsGroupType = "And", searchSortFieldNamesList = NULL, searchSortFieldNamesDescendingList = NULL, entityId = 1, query = NULL, page = 1, pageSize = 100000, flatten = T, returnResponse = F){

		params <- as.list(environment())

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		listSkyObjects(module = "MTSS", objectName = "TempStudentInterventionReferralError", searchFields = searchFields %>% append(fieldPaths), page = page, pageSize = pageSize, SearchConditionsList = searchConditionsList, SearchConditionsGroupType = searchConditionsGroupType, SearchSortFieldNamesList = searchSortFieldNamesList, SearchSortFieldNamesDescendingList = searchSortFieldNamesDescendingList, entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' Get a TempStudentInterventionReferralError
	#'
	#' This function returns a dataframe or json object of a TempStudentInterventionReferralError
	#' @param TempStudentInterventionReferralErrorID The ID of the TempStudentInterventionReferralError to return.
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given TempStudentInterventionReferralError. Defaults to FALSE for all return fields which, for convenience, returns all fields for the TempStudentInterventionReferralError.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object listed as text. Run \code{\link{getSchemaForObjects}}('TempStudentInterventionReferralError') to get more field paths.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param query Queries to be added to the request. Ex. list(SchoolYearID = 1). Defaults to NULL.
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept MTSS
	#' @return A dataframe or of TempStudentInterventionReferralError
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	getTempStudentInterventionReferralError <- function(TempStudentInterventionReferralErrorID, TempStudentInterventionReferralID = F, Error = F, ErrorDetail = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, fieldPaths = NULL, entityId = 1, query = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment()) %>% purrr::keep(names(.) != "TempStudentInterventionReferralErrorID")

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		getSkyObject(module = "MTSS", objectName = "TempStudentInterventionReferralError", objectId = TempStudentInterventionReferralErrorID, searchFields = searchFields, entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' Delete a TempStudentInterventionReferralError
	#'
	#' This function deletes a TempStudentInterventionReferralError
	#' @param TempStudentInterventionReferralErrorID The ID of the TempStudentInterventionReferralError to delete
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param query Queries to be added to the request. Ex. list(SchoolYearID = 1). Defaults to NULL.
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept MTSS
	#' @return The TempStudentInterventionReferralErrorID of the deleted TempStudentInterventionReferralError.
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	deleteTempStudentInterventionReferralError <- function(TempStudentInterventionReferralErrorID, ignoreWarnings = F, entityId = 1, query = NULL, flatten = T, returnResponse = F){

		deleteSkyObject(module = "MTSS", objectName = "TempStudentInterventionReferralError", objectId = TempStudentInterventionReferralErrorID, ignoreWarnings = ignoreWarnings, entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' Create a TempStudentInterventionReferralError
	#'
	#' This function creates a TempStudentInterventionReferralError
	#' @param fieldNames The field values to give the created TempStudentInterventionReferralError. Each defaults to NULL.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param query Queries to be added to the request. Ex. list(SchoolYearID = 1). Defaults to NULL.
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept MTSS
	#' @return A newly created TempStudentInterventionReferralError
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	createTempStudentInterventionReferralError <- function(TempStudentInterventionReferralID = NULL, Error = NULL, ErrorDetail = NULL, entityId = 1, query = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment())

		body <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper()) %>% purrr::compact()

		createSkyObject(module = "MTSS", objectName = "TempStudentInterventionReferralError", body = list(DataObject = body), searchFields = append("TempStudentInterventionReferralErrorID", body %>% names()), entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' Modify a TempStudentInterventionReferralError
	#'
	#' This function modifies a TempStudentInterventionReferralError
	#' @param fieldNames The field values to give the modified TempStudentInterventionReferralError. Each defaults to NULL.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param query Queries to be added to the request. Ex. list(SchoolYearID = 1). Defaults to NULL.
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept MTSS
	#' @return The modified TempStudentInterventionReferralError
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	modifyTempStudentInterventionReferralError <- function(TempStudentInterventionReferralErrorID, TempStudentInterventionReferralID = NULL, Error = NULL, ErrorDetail = NULL, entityId = 1, query = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment())

		body <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper()) %>% purrr::compact()

		modifySkyObject(module = "MTSS", objectName = "TempStudentInterventionReferralError", objectId = TempStudentInterventionReferralErrorID, body = list(DataObject = body), searchFields = append("TempStudentInterventionReferralErrorID", body %>% names()), entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' List TempStudentInterventionReferrals
	#'
	#' This function returns a dataframe or json object of TempStudentInterventionReferrals
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given TempStudentInterventionReferrals. Defaults to FALSE for all return fields which, for convenience, returns all fields for the TempStudentInterventionReferrals.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object listed as text. Run \code{\link{getSchemaForObjects}}('TempStudentInterventionReferral') to get more field paths.
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
	#' @concept MTSS
	#' @return A list of TempStudentInterventionReferrals
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	listTempStudentInterventionReferrals <- function(searchConditionsList = NULL, TempStudentInterventionReferralID = F, WatchlistID = F, StudentID = F, StudentNameLFM = F, StudentNumber = F, NameIDAssignedTo = F, NameAssignedToLFM = F, AreaOfConcernID = F, AreaOfConcernDescription = F, ReferralDate = F, Note = F, ErrorCount = F, Exceptions = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, StudentInterventionReferralID = F, Threshold = F, fieldPaths = NULL, searchConditionsGroupType = "And", searchSortFieldNamesList = NULL, searchSortFieldNamesDescendingList = NULL, entityId = 1, query = NULL, page = 1, pageSize = 100000, flatten = T, returnResponse = F){

		params <- as.list(environment())

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		listSkyObjects(module = "MTSS", objectName = "TempStudentInterventionReferral", searchFields = searchFields %>% append(fieldPaths), page = page, pageSize = pageSize, SearchConditionsList = searchConditionsList, SearchConditionsGroupType = searchConditionsGroupType, SearchSortFieldNamesList = searchSortFieldNamesList, SearchSortFieldNamesDescendingList = searchSortFieldNamesDescendingList, entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' Get a TempStudentInterventionReferral
	#'
	#' This function returns a dataframe or json object of a TempStudentInterventionReferral
	#' @param TempStudentInterventionReferralID The ID of the TempStudentInterventionReferral to return.
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given TempStudentInterventionReferral. Defaults to FALSE for all return fields which, for convenience, returns all fields for the TempStudentInterventionReferral.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object listed as text. Run \code{\link{getSchemaForObjects}}('TempStudentInterventionReferral') to get more field paths.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param query Queries to be added to the request. Ex. list(SchoolYearID = 1). Defaults to NULL.
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept MTSS
	#' @return A dataframe or of TempStudentInterventionReferral
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	getTempStudentInterventionReferral <- function(TempStudentInterventionReferralID, WatchlistID = F, StudentID = F, StudentNameLFM = F, StudentNumber = F, NameIDAssignedTo = F, NameAssignedToLFM = F, AreaOfConcernID = F, AreaOfConcernDescription = F, ReferralDate = F, Note = F, ErrorCount = F, Exceptions = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, StudentInterventionReferralID = F, Threshold = F, fieldPaths = NULL, entityId = 1, query = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment()) %>% purrr::keep(names(.) != "TempStudentInterventionReferralID")

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		getSkyObject(module = "MTSS", objectName = "TempStudentInterventionReferral", objectId = TempStudentInterventionReferralID, searchFields = searchFields, entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' Delete a TempStudentInterventionReferral
	#'
	#' This function deletes a TempStudentInterventionReferral
	#' @param TempStudentInterventionReferralID The ID of the TempStudentInterventionReferral to delete
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param query Queries to be added to the request. Ex. list(SchoolYearID = 1). Defaults to NULL.
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept MTSS
	#' @return The TempStudentInterventionReferralID of the deleted TempStudentInterventionReferral.
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	deleteTempStudentInterventionReferral <- function(TempStudentInterventionReferralID, ignoreWarnings = F, entityId = 1, query = NULL, flatten = T, returnResponse = F){

		deleteSkyObject(module = "MTSS", objectName = "TempStudentInterventionReferral", objectId = TempStudentInterventionReferralID, ignoreWarnings = ignoreWarnings, entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' Create a TempStudentInterventionReferral
	#'
	#' This function creates a TempStudentInterventionReferral
	#' @param fieldNames The field values to give the created TempStudentInterventionReferral. Each defaults to NULL.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param query Queries to be added to the request. Ex. list(SchoolYearID = 1). Defaults to NULL.
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept MTSS
	#' @return A newly created TempStudentInterventionReferral
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	createTempStudentInterventionReferral <- function(StudentNameLFM = NULL, StudentNumber = NULL, NameAssignedToLFM = NULL, AreaOfConcernDescription = NULL, ReferralDate = NULL, Note = NULL, ErrorCount = NULL, Exceptions = NULL, Threshold = NULL, entityId = 1, query = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment())

		body <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper()) %>% purrr::compact()

		createSkyObject(module = "MTSS", objectName = "TempStudentInterventionReferral", body = list(DataObject = body), searchFields = append("TempStudentInterventionReferralID", body %>% names()), entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' Modify a TempStudentInterventionReferral
	#'
	#' This function modifies a TempStudentInterventionReferral
	#' @param fieldNames The field values to give the modified TempStudentInterventionReferral. Each defaults to NULL.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param query Queries to be added to the request. Ex. list(SchoolYearID = 1). Defaults to NULL.
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept MTSS
	#' @return The modified TempStudentInterventionReferral
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	modifyTempStudentInterventionReferral <- function(TempStudentInterventionReferralID, StudentNameLFM = NULL, StudentNumber = NULL, NameAssignedToLFM = NULL, AreaOfConcernDescription = NULL, ReferralDate = NULL, Note = NULL, ErrorCount = NULL, Exceptions = NULL, Threshold = NULL, entityId = 1, query = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment())

		body <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper()) %>% purrr::compact()

		modifySkyObject(module = "MTSS", objectName = "TempStudentInterventionReferral", objectId = TempStudentInterventionReferralID, body = list(DataObject = body), searchFields = append("TempStudentInterventionReferralID", body %>% names()), entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' List WatchlistThresholdGradeLevels
	#'
	#' This function returns a dataframe or json object of WatchlistThresholdGradeLevels
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given WatchlistThresholdGradeLevels. Defaults to FALSE for all return fields which, for convenience, returns all fields for the WatchlistThresholdGradeLevels.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object listed as text. Run \code{\link{getSchemaForObjects}}('WatchlistThresholdGradeLevel') to get more field paths.
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
	#' @concept MTSS
	#' @return A list of WatchlistThresholdGradeLevels
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	listWatchlistThresholdGradeLevels <- function(searchConditionsList = NULL, WatchlistThresholdGradeLevelID = F, WatchlistThresholdID = F, GradeLevelID = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, fieldPaths = NULL, searchConditionsGroupType = "And", searchSortFieldNamesList = NULL, searchSortFieldNamesDescendingList = NULL, entityId = 1, query = NULL, page = 1, pageSize = 100000, flatten = T, returnResponse = F){

		params <- as.list(environment())

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		listSkyObjects(module = "MTSS", objectName = "WatchlistThresholdGradeLevel", searchFields = searchFields %>% append(fieldPaths), page = page, pageSize = pageSize, SearchConditionsList = searchConditionsList, SearchConditionsGroupType = searchConditionsGroupType, SearchSortFieldNamesList = searchSortFieldNamesList, SearchSortFieldNamesDescendingList = searchSortFieldNamesDescendingList, entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' Get a WatchlistThresholdGradeLevel
	#'
	#' This function returns a dataframe or json object of a WatchlistThresholdGradeLevel
	#' @param WatchlistThresholdGradeLevelID The ID of the WatchlistThresholdGradeLevel to return.
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given WatchlistThresholdGradeLevel. Defaults to FALSE for all return fields which, for convenience, returns all fields for the WatchlistThresholdGradeLevel.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object listed as text. Run \code{\link{getSchemaForObjects}}('WatchlistThresholdGradeLevel') to get more field paths.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param query Queries to be added to the request. Ex. list(SchoolYearID = 1). Defaults to NULL.
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept MTSS
	#' @return A dataframe or of WatchlistThresholdGradeLevel
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	getWatchlistThresholdGradeLevel <- function(WatchlistThresholdGradeLevelID, WatchlistThresholdID = F, GradeLevelID = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, fieldPaths = NULL, entityId = 1, query = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment()) %>% purrr::keep(names(.) != "WatchlistThresholdGradeLevelID")

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		getSkyObject(module = "MTSS", objectName = "WatchlistThresholdGradeLevel", objectId = WatchlistThresholdGradeLevelID, searchFields = searchFields, entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' Delete a WatchlistThresholdGradeLevel
	#'
	#' This function deletes a WatchlistThresholdGradeLevel
	#' @param WatchlistThresholdGradeLevelID The ID of the WatchlistThresholdGradeLevel to delete
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param query Queries to be added to the request. Ex. list(SchoolYearID = 1). Defaults to NULL.
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept MTSS
	#' @return The WatchlistThresholdGradeLevelID of the deleted WatchlistThresholdGradeLevel.
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	deleteWatchlistThresholdGradeLevel <- function(WatchlistThresholdGradeLevelID, ignoreWarnings = F, entityId = 1, query = NULL, flatten = T, returnResponse = F){

		deleteSkyObject(module = "MTSS", objectName = "WatchlistThresholdGradeLevel", objectId = WatchlistThresholdGradeLevelID, ignoreWarnings = ignoreWarnings, entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' Create a WatchlistThresholdGradeLevel
	#'
	#' This function creates a WatchlistThresholdGradeLevel
	#' @param fieldNames The field values to give the created WatchlistThresholdGradeLevel. Each defaults to NULL.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param query Queries to be added to the request. Ex. list(SchoolYearID = 1). Defaults to NULL.
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept MTSS
	#' @return A newly created WatchlistThresholdGradeLevel
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	createWatchlistThresholdGradeLevel <- function(WatchlistThresholdID = NULL, GradeLevelID = NULL, entityId = 1, query = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment())

		body <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper()) %>% purrr::compact()

		createSkyObject(module = "MTSS", objectName = "WatchlistThresholdGradeLevel", body = list(DataObject = body), searchFields = append("WatchlistThresholdGradeLevelID", body %>% names()), entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' Modify a WatchlistThresholdGradeLevel
	#'
	#' This function modifies a WatchlistThresholdGradeLevel
	#' @param fieldNames The field values to give the modified WatchlistThresholdGradeLevel. Each defaults to NULL.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param query Queries to be added to the request. Ex. list(SchoolYearID = 1). Defaults to NULL.
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept MTSS
	#' @return The modified WatchlistThresholdGradeLevel
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	modifyWatchlistThresholdGradeLevel <- function(WatchlistThresholdGradeLevelID, WatchlistThresholdID = NULL, GradeLevelID = NULL, entityId = 1, query = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment())

		body <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper()) %>% purrr::compact()

		modifySkyObject(module = "MTSS", objectName = "WatchlistThresholdGradeLevel", objectId = WatchlistThresholdGradeLevelID, body = list(DataObject = body), searchFields = append("WatchlistThresholdGradeLevelID", body %>% names()), entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' List TempMTSSYearEndTypeSummaries
	#'
	#' This function returns a dataframe or json object of TempMTSSYearEndTypeSummaries
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given TempMTSSYearEndTypeSummaries. Defaults to FALSE for all return fields which, for convenience, returns all fields for the TempMTSSYearEndTypeSummaries.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object listed as text. Run \code{\link{getSchemaForObjects}}('TempMTSSYearEndTypeSummary') to get more field paths.
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
	#' @concept MTSS
	#' @return A list of TempMTSSYearEndTypeSummaries
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	listTempMTSSYearEndTypeSummaries <- function(searchConditionsList = NULL, TempMTSSYearEndTypeSummaryID = F, CloneWarning = F, DisplayName = F, SkywardIDObject = F, SourceRecordsFound = F, TargetRecordsFound = F, EstimatedRecordsToProcess = F, RecordsProcessedCount = F, SuccessCount = F, FailureCount = F, CloneUnsuccessful = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, fieldPaths = NULL, searchConditionsGroupType = "And", searchSortFieldNamesList = NULL, searchSortFieldNamesDescendingList = NULL, entityId = 1, query = NULL, page = 1, pageSize = 100000, flatten = T, returnResponse = F){

		params <- as.list(environment())

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		listSkyObjects(module = "MTSS", objectName = "TempMTSSYearEndTypeSummary", searchFields = searchFields %>% append(fieldPaths), page = page, pageSize = pageSize, SearchConditionsList = searchConditionsList, SearchConditionsGroupType = searchConditionsGroupType, SearchSortFieldNamesList = searchSortFieldNamesList, SearchSortFieldNamesDescendingList = searchSortFieldNamesDescendingList, entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' Get a TempMTSSYearEndTypeSummary
	#'
	#' This function returns a dataframe or json object of a TempMTSSYearEndTypeSummary
	#' @param TempMTSSYearEndTypeSummaryID The ID of the TempMTSSYearEndTypeSummary to return.
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given TempMTSSYearEndTypeSummary. Defaults to FALSE for all return fields which, for convenience, returns all fields for the TempMTSSYearEndTypeSummary.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object listed as text. Run \code{\link{getSchemaForObjects}}('TempMTSSYearEndTypeSummary') to get more field paths.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param query Queries to be added to the request. Ex. list(SchoolYearID = 1). Defaults to NULL.
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept MTSS
	#' @return A dataframe or of TempMTSSYearEndTypeSummary
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	getTempMTSSYearEndTypeSummary <- function(TempMTSSYearEndTypeSummaryID, CloneWarning = F, DisplayName = F, SkywardIDObject = F, SourceRecordsFound = F, TargetRecordsFound = F, EstimatedRecordsToProcess = F, RecordsProcessedCount = F, SuccessCount = F, FailureCount = F, CloneUnsuccessful = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, fieldPaths = NULL, entityId = 1, query = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment()) %>% purrr::keep(names(.) != "TempMTSSYearEndTypeSummaryID")

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		getSkyObject(module = "MTSS", objectName = "TempMTSSYearEndTypeSummary", objectId = TempMTSSYearEndTypeSummaryID, searchFields = searchFields, entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' Delete a TempMTSSYearEndTypeSummary
	#'
	#' This function deletes a TempMTSSYearEndTypeSummary
	#' @param TempMTSSYearEndTypeSummaryID The ID of the TempMTSSYearEndTypeSummary to delete
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param query Queries to be added to the request. Ex. list(SchoolYearID = 1). Defaults to NULL.
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept MTSS
	#' @return The TempMTSSYearEndTypeSummaryID of the deleted TempMTSSYearEndTypeSummary.
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	deleteTempMTSSYearEndTypeSummary <- function(TempMTSSYearEndTypeSummaryID, ignoreWarnings = F, entityId = 1, query = NULL, flatten = T, returnResponse = F){

		deleteSkyObject(module = "MTSS", objectName = "TempMTSSYearEndTypeSummary", objectId = TempMTSSYearEndTypeSummaryID, ignoreWarnings = ignoreWarnings, entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' Create a TempMTSSYearEndTypeSummary
	#'
	#' This function creates a TempMTSSYearEndTypeSummary
	#' @param fieldNames The field values to give the created TempMTSSYearEndTypeSummary. Each defaults to NULL.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param query Queries to be added to the request. Ex. list(SchoolYearID = 1). Defaults to NULL.
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept MTSS
	#' @return A newly created TempMTSSYearEndTypeSummary
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	createTempMTSSYearEndTypeSummary <- function(DisplayName = NULL, SkywardIDObject = NULL, SourceRecordsFound = NULL, TargetRecordsFound = NULL, EstimatedRecordsToProcess = NULL, RecordsProcessedCount = NULL, SuccessCount = NULL, FailureCount = NULL, entityId = 1, query = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment())

		body <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper()) %>% purrr::compact()

		createSkyObject(module = "MTSS", objectName = "TempMTSSYearEndTypeSummary", body = list(DataObject = body), searchFields = append("TempMTSSYearEndTypeSummaryID", body %>% names()), entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' Modify a TempMTSSYearEndTypeSummary
	#'
	#' This function modifies a TempMTSSYearEndTypeSummary
	#' @param fieldNames The field values to give the modified TempMTSSYearEndTypeSummary. Each defaults to NULL.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param query Queries to be added to the request. Ex. list(SchoolYearID = 1). Defaults to NULL.
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept MTSS
	#' @return The modified TempMTSSYearEndTypeSummary
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	modifyTempMTSSYearEndTypeSummary <- function(TempMTSSYearEndTypeSummaryID, DisplayName = NULL, SkywardIDObject = NULL, SourceRecordsFound = NULL, TargetRecordsFound = NULL, EstimatedRecordsToProcess = NULL, RecordsProcessedCount = NULL, SuccessCount = NULL, FailureCount = NULL, entityId = 1, query = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment())

		body <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper()) %>% purrr::compact()

		modifySkyObject(module = "MTSS", objectName = "TempMTSSYearEndTypeSummary", objectId = TempMTSSYearEndTypeSummaryID, body = list(DataObject = body), searchFields = append("TempMTSSYearEndTypeSummaryID", body %>% names()), entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' List InterventionPlanMeetingAttendees
	#'
	#' This function returns a dataframe or json object of InterventionPlanMeetingAttendees
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given InterventionPlanMeetingAttendees. Defaults to FALSE for all return fields which, for convenience, returns all fields for the InterventionPlanMeetingAttendees.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object listed as text. Run \code{\link{getSchemaForObjects}}('InterventionPlanMeetingAttendee') to get more field paths.
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
	#' @concept MTSS
	#' @return A list of InterventionPlanMeetingAttendees
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	listInterventionPlanMeetingAttendees <- function(searchConditionsList = NULL, InterventionPlanMeetingAttendeeID = F, NameID = F, InterventionPlanMeetingID = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, fieldPaths = NULL, searchConditionsGroupType = "And", searchSortFieldNamesList = NULL, searchSortFieldNamesDescendingList = NULL, entityId = 1, query = NULL, page = 1, pageSize = 100000, flatten = T, returnResponse = F){

		params <- as.list(environment())

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		listSkyObjects(module = "MTSS", objectName = "InterventionPlanMeetingAttendee", searchFields = searchFields %>% append(fieldPaths), page = page, pageSize = pageSize, SearchConditionsList = searchConditionsList, SearchConditionsGroupType = searchConditionsGroupType, SearchSortFieldNamesList = searchSortFieldNamesList, SearchSortFieldNamesDescendingList = searchSortFieldNamesDescendingList, entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' Get an InterventionPlanMeetingAttendee
	#'
	#' This function returns a dataframe or json object of an InterventionPlanMeetingAttendee
	#' @param InterventionPlanMeetingAttendeeID The ID of the InterventionPlanMeetingAttendee to return.
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given InterventionPlanMeetingAttendee. Defaults to FALSE for all return fields which, for convenience, returns all fields for the InterventionPlanMeetingAttendee.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object listed as text. Run \code{\link{getSchemaForObjects}}('InterventionPlanMeetingAttendee') to get more field paths.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param query Queries to be added to the request. Ex. list(SchoolYearID = 1). Defaults to NULL.
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept MTSS
	#' @return A dataframe or of InterventionPlanMeetingAttendee
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	getInterventionPlanMeetingAttendee <- function(InterventionPlanMeetingAttendeeID, NameID = F, InterventionPlanMeetingID = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, fieldPaths = NULL, entityId = 1, query = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment()) %>% purrr::keep(names(.) != "InterventionPlanMeetingAttendeeID")

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		getSkyObject(module = "MTSS", objectName = "InterventionPlanMeetingAttendee", objectId = InterventionPlanMeetingAttendeeID, searchFields = searchFields, entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' Delete an InterventionPlanMeetingAttendee
	#'
	#' This function deletes an InterventionPlanMeetingAttendee
	#' @param InterventionPlanMeetingAttendeeID The ID of the InterventionPlanMeetingAttendee to delete
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param query Queries to be added to the request. Ex. list(SchoolYearID = 1). Defaults to NULL.
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept MTSS
	#' @return The InterventionPlanMeetingAttendeeID of the deleted InterventionPlanMeetingAttendee.
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	deleteInterventionPlanMeetingAttendee <- function(InterventionPlanMeetingAttendeeID, ignoreWarnings = F, entityId = 1, query = NULL, flatten = T, returnResponse = F){

		deleteSkyObject(module = "MTSS", objectName = "InterventionPlanMeetingAttendee", objectId = InterventionPlanMeetingAttendeeID, ignoreWarnings = ignoreWarnings, entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' Create an InterventionPlanMeetingAttendee
	#'
	#' This function creates an InterventionPlanMeetingAttendee
	#' @param fieldNames The field values to give the created InterventionPlanMeetingAttendee. Each defaults to NULL.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param query Queries to be added to the request. Ex. list(SchoolYearID = 1). Defaults to NULL.
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept MTSS
	#' @return A newly created InterventionPlanMeetingAttendee
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	createInterventionPlanMeetingAttendee <- function(NameID = NULL, InterventionPlanMeetingID = NULL, entityId = 1, query = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment())

		body <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper()) %>% purrr::compact()

		createSkyObject(module = "MTSS", objectName = "InterventionPlanMeetingAttendee", body = list(DataObject = body), searchFields = append("InterventionPlanMeetingAttendeeID", body %>% names()), entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' Modify an InterventionPlanMeetingAttendee
	#'
	#' This function modifies an InterventionPlanMeetingAttendee
	#' @param fieldNames The field values to give the modified InterventionPlanMeetingAttendee. Each defaults to NULL.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param query Queries to be added to the request. Ex. list(SchoolYearID = 1). Defaults to NULL.
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept MTSS
	#' @return The modified InterventionPlanMeetingAttendee
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	modifyInterventionPlanMeetingAttendee <- function(InterventionPlanMeetingAttendeeID, NameID = NULL, InterventionPlanMeetingID = NULL, entityId = 1, query = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment())

		body <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper()) %>% purrr::compact()

		modifySkyObject(module = "MTSS", objectName = "InterventionPlanMeetingAttendee", objectId = InterventionPlanMeetingAttendeeID, body = list(DataObject = body), searchFields = append("InterventionPlanMeetingAttendeeID", body %>% names()), entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' List TempInterventionPlanErrors
	#'
	#' This function returns a dataframe or json object of TempInterventionPlanErrors
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given TempInterventionPlanErrors. Defaults to FALSE for all return fields which, for convenience, returns all fields for the TempInterventionPlanErrors.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object listed as text. Run \code{\link{getSchemaForObjects}}('TempInterventionPlanError') to get more field paths.
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
	#' @concept MTSS
	#' @return A list of TempInterventionPlanErrors
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	listTempInterventionPlanErrors <- function(searchConditionsList = NULL, TempInterventionPlanErrorID = F, TempInterventionPlanID = F, Error = F, ErrorDetail = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, fieldPaths = NULL, searchConditionsGroupType = "And", searchSortFieldNamesList = NULL, searchSortFieldNamesDescendingList = NULL, entityId = 1, query = NULL, page = 1, pageSize = 100000, flatten = T, returnResponse = F){

		params <- as.list(environment())

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		listSkyObjects(module = "MTSS", objectName = "TempInterventionPlanError", searchFields = searchFields %>% append(fieldPaths), page = page, pageSize = pageSize, SearchConditionsList = searchConditionsList, SearchConditionsGroupType = searchConditionsGroupType, SearchSortFieldNamesList = searchSortFieldNamesList, SearchSortFieldNamesDescendingList = searchSortFieldNamesDescendingList, entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' Get a TempInterventionPlanError
	#'
	#' This function returns a dataframe or json object of a TempInterventionPlanError
	#' @param TempInterventionPlanErrorID The ID of the TempInterventionPlanError to return.
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given TempInterventionPlanError. Defaults to FALSE for all return fields which, for convenience, returns all fields for the TempInterventionPlanError.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object listed as text. Run \code{\link{getSchemaForObjects}}('TempInterventionPlanError') to get more field paths.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param query Queries to be added to the request. Ex. list(SchoolYearID = 1). Defaults to NULL.
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept MTSS
	#' @return A dataframe or of TempInterventionPlanError
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	getTempInterventionPlanError <- function(TempInterventionPlanErrorID, TempInterventionPlanID = F, Error = F, ErrorDetail = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, fieldPaths = NULL, entityId = 1, query = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment()) %>% purrr::keep(names(.) != "TempInterventionPlanErrorID")

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		getSkyObject(module = "MTSS", objectName = "TempInterventionPlanError", objectId = TempInterventionPlanErrorID, searchFields = searchFields, entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' Delete a TempInterventionPlanError
	#'
	#' This function deletes a TempInterventionPlanError
	#' @param TempInterventionPlanErrorID The ID of the TempInterventionPlanError to delete
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param query Queries to be added to the request. Ex. list(SchoolYearID = 1). Defaults to NULL.
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept MTSS
	#' @return The TempInterventionPlanErrorID of the deleted TempInterventionPlanError.
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	deleteTempInterventionPlanError <- function(TempInterventionPlanErrorID, ignoreWarnings = F, entityId = 1, query = NULL, flatten = T, returnResponse = F){

		deleteSkyObject(module = "MTSS", objectName = "TempInterventionPlanError", objectId = TempInterventionPlanErrorID, ignoreWarnings = ignoreWarnings, entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' Create a TempInterventionPlanError
	#'
	#' This function creates a TempInterventionPlanError
	#' @param fieldNames The field values to give the created TempInterventionPlanError. Each defaults to NULL.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param query Queries to be added to the request. Ex. list(SchoolYearID = 1). Defaults to NULL.
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept MTSS
	#' @return A newly created TempInterventionPlanError
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	createTempInterventionPlanError <- function(TempInterventionPlanID = NULL, Error = NULL, ErrorDetail = NULL, entityId = 1, query = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment())

		body <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper()) %>% purrr::compact()

		createSkyObject(module = "MTSS", objectName = "TempInterventionPlanError", body = list(DataObject = body), searchFields = append("TempInterventionPlanErrorID", body %>% names()), entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' Modify a TempInterventionPlanError
	#'
	#' This function modifies a TempInterventionPlanError
	#' @param fieldNames The field values to give the modified TempInterventionPlanError. Each defaults to NULL.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param query Queries to be added to the request. Ex. list(SchoolYearID = 1). Defaults to NULL.
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept MTSS
	#' @return The modified TempInterventionPlanError
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	modifyTempInterventionPlanError <- function(TempInterventionPlanErrorID, TempInterventionPlanID = NULL, Error = NULL, ErrorDetail = NULL, entityId = 1, query = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment())

		body <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper()) %>% purrr::compact()

		modifySkyObject(module = "MTSS", objectName = "TempInterventionPlanError", objectId = TempInterventionPlanErrorID, body = list(DataObject = body), searchFields = append("TempInterventionPlanErrorID", body %>% names()), entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}
