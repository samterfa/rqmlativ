
	#' List DisabilityMNS
	#'
	#' This function returns a dataframe or json object of DisabilityMNS
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given DisabilityMNS. Defaults to FALSE for all return fields which, for convenience, returns all fields for the DisabilityMNS.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object listed as text. Run \code{\link{getSchemaForObjects}}('DisabilityMN') to get more field paths.
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
	#' @concept SpecialEducation
	#' @return A list of DisabilityMNS
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	listDisabilityMNS <- function(searchConditionsList = NULL, DisabilityMNID = F, EnrollmentMNID = F, Rank = F, StateDisabilityCodeMNID = F, DisabilityRank = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, fieldPaths = NULL, searchConditionsGroupType = "And", searchSortFieldNamesList = NULL, searchSortFieldNamesDescendingList = NULL, entityId = 1, query = NULL, page = 1, pageSize = 100000, flatten = T, returnResponse = F){

		params <- as.list(environment())

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		listSkyObjects(module = "SpecialEducation", objectName = "DisabilityMN", searchFields = searchFields %>% append(fieldPaths), page = page, pageSize = pageSize, SearchConditionsList = searchConditionsList, SearchConditionsGroupType = searchConditionsGroupType, SearchSortFieldNamesList = searchSortFieldNamesList, SearchSortFieldNamesDescendingList = searchSortFieldNamesDescendingList, entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' Get a DisabilityMN
	#'
	#' This function returns a dataframe or json object of a DisabilityMN
	#' @param DisabilityMNID The ID of the DisabilityMN to return.
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given DisabilityMN. Defaults to FALSE for all return fields which, for convenience, returns all fields for the DisabilityMN.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object listed as text. Run \code{\link{getSchemaForObjects}}('DisabilityMN') to get more field paths.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param query Queries to be added to the request. Ex. list(SchoolYearID = 1). Defaults to NULL.
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept SpecialEducation
	#' @return A dataframe or of DisabilityMN
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	getDisabilityMN <- function(DisabilityMNID, EnrollmentMNID = F, Rank = F, StateDisabilityCodeMNID = F, DisabilityRank = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, fieldPaths = NULL, entityId = 1, query = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment()) %>% purrr::keep(names(.) != "DisabilityMNID")

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		getSkyObject(module = "SpecialEducation", objectName = "DisabilityMN", objectId = DisabilityMNID, searchFields = searchFields, entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' Delete a DisabilityMN
	#'
	#' This function deletes a DisabilityMN
	#' @param DisabilityMNID The ID of the DisabilityMN to delete
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param query Queries to be added to the request. Ex. list(SchoolYearID = 1). Defaults to NULL.
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept SpecialEducation
	#' @return The DisabilityMNID of the deleted DisabilityMN.
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	deleteDisabilityMN <- function(DisabilityMNID, ignoreWarnings = F, entityId = 1, query = NULL, flatten = T, returnResponse = F){

		deleteSkyObject(module = "SpecialEducation", objectName = "DisabilityMN", objectId = DisabilityMNID, ignoreWarnings = ignoreWarnings, entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' Create a DisabilityMN
	#'
	#' This function creates a DisabilityMN
	#' @param fieldNames The field values to give the created DisabilityMN. Each defaults to NULL.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param query Queries to be added to the request. Ex. list(SchoolYearID = 1). Defaults to NULL.
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept SpecialEducation
	#' @return A newly created DisabilityMN
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	createDisabilityMN <- function(EnrollmentMNID = NULL, Rank = NULL, StateDisabilityCodeMNID = NULL, entityId = 1, query = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment())

		body <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper()) %>% purrr::compact()

		createSkyObject(module = "SpecialEducation", objectName = "DisabilityMN", body = list(DataObject = body), searchFields = append("DisabilityMNID", body %>% names()), entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' Modify a DisabilityMN
	#'
	#' This function modifies a DisabilityMN
	#' @param fieldNames The field values to give the modified DisabilityMN. Each defaults to NULL.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param query Queries to be added to the request. Ex. list(SchoolYearID = 1). Defaults to NULL.
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept SpecialEducation
	#' @return The modified DisabilityMN
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	modifyDisabilityMN <- function(DisabilityMNID, EnrollmentMNID = NULL, Rank = NULL, StateDisabilityCodeMNID = NULL, entityId = 1, query = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment())

		body <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper()) %>% purrr::compact()

		modifySkyObject(module = "SpecialEducation", objectName = "DisabilityMN", objectId = DisabilityMNID, body = list(DataObject = body), searchFields = append("DisabilityMNID", body %>% names()), entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' List EnrollmentMNS
	#'
	#' This function returns a dataframe or json object of EnrollmentMNS
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given EnrollmentMNS. Defaults to FALSE for all return fields which, for convenience, returns all fields for the EnrollmentMNS.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object listed as text. Run \code{\link{getSchemaForObjects}}('EnrollmentMN') to get more field paths.
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
	#' @concept SpecialEducation
	#' @return A list of EnrollmentMNS
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	listEnrollmentMNS <- function(searchConditionsList = NULL, EnrollmentMNID = F, DistrictID = F, StateEvaluationStatusCodeMNIDEvaluation = F, StateSpecEdInstructionalSettingCodeMNID = F, IsConsideredSpecialEd = F, IsExitingSpecialEd = F, StateEvaluationStatusCodeMNIDMARSSExit = F, EntryComment = F, ExitComment = F, StartDate = F, EndDate = F, IsActive = F, StudentID = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, ReceivesServicesInDistrict = F, fieldPaths = NULL, searchConditionsGroupType = "And", searchSortFieldNamesList = NULL, searchSortFieldNamesDescendingList = NULL, entityId = 1, query = NULL, page = 1, pageSize = 100000, flatten = T, returnResponse = F){

		params <- as.list(environment())

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		listSkyObjects(module = "SpecialEducation", objectName = "EnrollmentMN", searchFields = searchFields %>% append(fieldPaths), page = page, pageSize = pageSize, SearchConditionsList = searchConditionsList, SearchConditionsGroupType = searchConditionsGroupType, SearchSortFieldNamesList = searchSortFieldNamesList, SearchSortFieldNamesDescendingList = searchSortFieldNamesDescendingList, entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' Get an EnrollmentMN
	#'
	#' This function returns a dataframe or json object of an EnrollmentMN
	#' @param EnrollmentMNID The ID of the EnrollmentMN to return.
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given EnrollmentMN. Defaults to FALSE for all return fields which, for convenience, returns all fields for the EnrollmentMN.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object listed as text. Run \code{\link{getSchemaForObjects}}('EnrollmentMN') to get more field paths.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param query Queries to be added to the request. Ex. list(SchoolYearID = 1). Defaults to NULL.
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept SpecialEducation
	#' @return A dataframe or of EnrollmentMN
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	getEnrollmentMN <- function(EnrollmentMNID, DistrictID = F, StateEvaluationStatusCodeMNIDEvaluation = F, StateSpecEdInstructionalSettingCodeMNID = F, IsConsideredSpecialEd = F, IsExitingSpecialEd = F, StateEvaluationStatusCodeMNIDMARSSExit = F, EntryComment = F, ExitComment = F, StartDate = F, EndDate = F, IsActive = F, StudentID = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, ReceivesServicesInDistrict = F, fieldPaths = NULL, entityId = 1, query = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment()) %>% purrr::keep(names(.) != "EnrollmentMNID")

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		getSkyObject(module = "SpecialEducation", objectName = "EnrollmentMN", objectId = EnrollmentMNID, searchFields = searchFields, entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' Delete an EnrollmentMN
	#'
	#' This function deletes an EnrollmentMN
	#' @param EnrollmentMNID The ID of the EnrollmentMN to delete
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param query Queries to be added to the request. Ex. list(SchoolYearID = 1). Defaults to NULL.
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept SpecialEducation
	#' @return The EnrollmentMNID of the deleted EnrollmentMN.
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	deleteEnrollmentMN <- function(EnrollmentMNID, ignoreWarnings = F, entityId = 1, query = NULL, flatten = T, returnResponse = F){

		deleteSkyObject(module = "SpecialEducation", objectName = "EnrollmentMN", objectId = EnrollmentMNID, ignoreWarnings = ignoreWarnings, entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' Create an EnrollmentMN
	#'
	#' This function creates an EnrollmentMN
	#' @param fieldNames The field values to give the created EnrollmentMN. Each defaults to NULL.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param query Queries to be added to the request. Ex. list(SchoolYearID = 1). Defaults to NULL.
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept SpecialEducation
	#' @return A newly created EnrollmentMN
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	createEnrollmentMN <- function(DistrictID = NULL, StateEvaluationStatusCodeMNIDEvaluation = NULL, StateSpecEdInstructionalSettingCodeMNID = NULL, IsConsideredSpecialEd = NULL, IsExitingSpecialEd = NULL, StateEvaluationStatusCodeMNIDMARSSExit = NULL, EntryComment = NULL, ExitComment = NULL, StartDate = NULL, EndDate = NULL, StudentID = NULL, ReceivesServicesInDistrict = NULL, entityId = 1, query = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment())

		body <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper()) %>% purrr::compact()

		createSkyObject(module = "SpecialEducation", objectName = "EnrollmentMN", body = list(DataObject = body), searchFields = append("EnrollmentMNID", body %>% names()), entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' Modify an EnrollmentMN
	#'
	#' This function modifies an EnrollmentMN
	#' @param fieldNames The field values to give the modified EnrollmentMN. Each defaults to NULL.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param query Queries to be added to the request. Ex. list(SchoolYearID = 1). Defaults to NULL.
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept SpecialEducation
	#' @return The modified EnrollmentMN
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	modifyEnrollmentMN <- function(EnrollmentMNID, DistrictID = NULL, StateEvaluationStatusCodeMNIDEvaluation = NULL, StateSpecEdInstructionalSettingCodeMNID = NULL, IsConsideredSpecialEd = NULL, IsExitingSpecialEd = NULL, StateEvaluationStatusCodeMNIDMARSSExit = NULL, EntryComment = NULL, ExitComment = NULL, StartDate = NULL, EndDate = NULL, StudentID = NULL, ReceivesServicesInDistrict = NULL, entityId = 1, query = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment())

		body <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper()) %>% purrr::compact()

		modifySkyObject(module = "SpecialEducation", objectName = "EnrollmentMN", objectId = EnrollmentMNID, body = list(DataObject = body), searchFields = append("EnrollmentMNID", body %>% names()), entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' List NextRestraintInstanceNumberTXES
	#'
	#' This function returns a dataframe or json object of NextRestraintInstanceNumberTXES
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given NextRestraintInstanceNumberTXES. Defaults to FALSE for all return fields which, for convenience, returns all fields for the NextRestraintInstanceNumberTXES.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object listed as text. Run \code{\link{getSchemaForObjects}}('NextRestraintInstanceNumberTX') to get more field paths.
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
	#' @concept SpecialEducation
	#' @return A list of NextRestraintInstanceNumberTXES
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	listNextRestraintInstanceNumberTXES <- function(searchConditionsList = NULL, NextRestraintInstanceNumberTXID = F, DistrictID = F, SchoolYearID = F, SequenceNumber = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, fieldPaths = NULL, searchConditionsGroupType = "And", searchSortFieldNamesList = NULL, searchSortFieldNamesDescendingList = NULL, entityId = 1, query = NULL, page = 1, pageSize = 100000, flatten = T, returnResponse = F){

		params <- as.list(environment())

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		listSkyObjects(module = "SpecialEducation", objectName = "NextRestraintInstanceNumberTX", searchFields = searchFields %>% append(fieldPaths), page = page, pageSize = pageSize, SearchConditionsList = searchConditionsList, SearchConditionsGroupType = searchConditionsGroupType, SearchSortFieldNamesList = searchSortFieldNamesList, SearchSortFieldNamesDescendingList = searchSortFieldNamesDescendingList, entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' Get a NextRestraintInstanceNumberTX
	#'
	#' This function returns a dataframe or json object of a NextRestraintInstanceNumberTX
	#' @param NextRestraintInstanceNumberTXID The ID of the NextRestraintInstanceNumberTX to return.
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given NextRestraintInstanceNumberTX. Defaults to FALSE for all return fields which, for convenience, returns all fields for the NextRestraintInstanceNumberTX.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object listed as text. Run \code{\link{getSchemaForObjects}}('NextRestraintInstanceNumberTX') to get more field paths.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param query Queries to be added to the request. Ex. list(SchoolYearID = 1). Defaults to NULL.
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept SpecialEducation
	#' @return A dataframe or of NextRestraintInstanceNumberTX
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	getNextRestraintInstanceNumberTX <- function(NextRestraintInstanceNumberTXID, DistrictID = F, SchoolYearID = F, SequenceNumber = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, fieldPaths = NULL, entityId = 1, query = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment()) %>% purrr::keep(names(.) != "NextRestraintInstanceNumberTXID")

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		getSkyObject(module = "SpecialEducation", objectName = "NextRestraintInstanceNumberTX", objectId = NextRestraintInstanceNumberTXID, searchFields = searchFields, entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' Delete a NextRestraintInstanceNumberTX
	#'
	#' This function deletes a NextRestraintInstanceNumberTX
	#' @param NextRestraintInstanceNumberTXID The ID of the NextRestraintInstanceNumberTX to delete
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param query Queries to be added to the request. Ex. list(SchoolYearID = 1). Defaults to NULL.
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept SpecialEducation
	#' @return The NextRestraintInstanceNumberTXID of the deleted NextRestraintInstanceNumberTX.
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	deleteNextRestraintInstanceNumberTX <- function(NextRestraintInstanceNumberTXID, ignoreWarnings = F, entityId = 1, query = NULL, flatten = T, returnResponse = F){

		deleteSkyObject(module = "SpecialEducation", objectName = "NextRestraintInstanceNumberTX", objectId = NextRestraintInstanceNumberTXID, ignoreWarnings = ignoreWarnings, entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' Create a NextRestraintInstanceNumberTX
	#'
	#' This function creates a NextRestraintInstanceNumberTX
	#' @param fieldNames The field values to give the created NextRestraintInstanceNumberTX. Each defaults to NULL.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param query Queries to be added to the request. Ex. list(SchoolYearID = 1). Defaults to NULL.
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept SpecialEducation
	#' @return A newly created NextRestraintInstanceNumberTX
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	createNextRestraintInstanceNumberTX <- function(DistrictID = NULL, SchoolYearID = NULL, SequenceNumber = NULL, entityId = 1, query = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment())

		body <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper()) %>% purrr::compact()

		createSkyObject(module = "SpecialEducation", objectName = "NextRestraintInstanceNumberTX", body = list(DataObject = body), searchFields = append("NextRestraintInstanceNumberTXID", body %>% names()), entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' Modify a NextRestraintInstanceNumberTX
	#'
	#' This function modifies a NextRestraintInstanceNumberTX
	#' @param fieldNames The field values to give the modified NextRestraintInstanceNumberTX. Each defaults to NULL.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param query Queries to be added to the request. Ex. list(SchoolYearID = 1). Defaults to NULL.
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept SpecialEducation
	#' @return The modified NextRestraintInstanceNumberTX
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	modifyNextRestraintInstanceNumberTX <- function(NextRestraintInstanceNumberTXID, DistrictID = NULL, SchoolYearID = NULL, SequenceNumber = NULL, entityId = 1, query = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment())

		body <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper()) %>% purrr::compact()

		modifySkyObject(module = "SpecialEducation", objectName = "NextRestraintInstanceNumberTX", objectId = NextRestraintInstanceNumberTXID, body = list(DataObject = body), searchFields = append("NextRestraintInstanceNumberTXID", body %>% names()), entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' List EasyIEPExceptions
	#'
	#' This function returns a dataframe or json object of EasyIEPExceptions
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given EasyIEPExceptions. Defaults to FALSE for all return fields which, for convenience, returns all fields for the EasyIEPExceptions.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object listed as text. Run \code{\link{getSchemaForObjects}}('EasyIEPException') to get more field paths.
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
	#' @concept SpecialEducation
	#' @return A list of EasyIEPExceptions
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	listEasyIEPExceptions <- function(searchConditionsList = NULL, EasyIEPExceptionID = F, EasyIEPRunHistoryID = F, StudentID = F, RecordType = F, StartDate = F, EndDate = F, Message = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, fieldPaths = NULL, searchConditionsGroupType = "And", searchSortFieldNamesList = NULL, searchSortFieldNamesDescendingList = NULL, entityId = 1, query = NULL, page = 1, pageSize = 100000, flatten = T, returnResponse = F){

		params <- as.list(environment())

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		listSkyObjects(module = "SpecialEducation", objectName = "EasyIEPException", searchFields = searchFields %>% append(fieldPaths), page = page, pageSize = pageSize, SearchConditionsList = searchConditionsList, SearchConditionsGroupType = searchConditionsGroupType, SearchSortFieldNamesList = searchSortFieldNamesList, SearchSortFieldNamesDescendingList = searchSortFieldNamesDescendingList, entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' Get an EasyIEPException
	#'
	#' This function returns a dataframe or json object of an EasyIEPException
	#' @param EasyIEPExceptionID The ID of the EasyIEPException to return.
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given EasyIEPException. Defaults to FALSE for all return fields which, for convenience, returns all fields for the EasyIEPException.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object listed as text. Run \code{\link{getSchemaForObjects}}('EasyIEPException') to get more field paths.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param query Queries to be added to the request. Ex. list(SchoolYearID = 1). Defaults to NULL.
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept SpecialEducation
	#' @return A dataframe or of EasyIEPException
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	getEasyIEPException <- function(EasyIEPExceptionID, EasyIEPRunHistoryID = F, StudentID = F, RecordType = F, StartDate = F, EndDate = F, Message = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, fieldPaths = NULL, entityId = 1, query = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment()) %>% purrr::keep(names(.) != "EasyIEPExceptionID")

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		getSkyObject(module = "SpecialEducation", objectName = "EasyIEPException", objectId = EasyIEPExceptionID, searchFields = searchFields, entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' Delete an EasyIEPException
	#'
	#' This function deletes an EasyIEPException
	#' @param EasyIEPExceptionID The ID of the EasyIEPException to delete
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param query Queries to be added to the request. Ex. list(SchoolYearID = 1). Defaults to NULL.
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept SpecialEducation
	#' @return The EasyIEPExceptionID of the deleted EasyIEPException.
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	deleteEasyIEPException <- function(EasyIEPExceptionID, ignoreWarnings = F, entityId = 1, query = NULL, flatten = T, returnResponse = F){

		deleteSkyObject(module = "SpecialEducation", objectName = "EasyIEPException", objectId = EasyIEPExceptionID, ignoreWarnings = ignoreWarnings, entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' Create an EasyIEPException
	#'
	#' This function creates an EasyIEPException
	#' @param fieldNames The field values to give the created EasyIEPException. Each defaults to NULL.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param query Queries to be added to the request. Ex. list(SchoolYearID = 1). Defaults to NULL.
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept SpecialEducation
	#' @return A newly created EasyIEPException
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	createEasyIEPException <- function(EasyIEPRunHistoryID = NULL, StudentID = NULL, RecordType = NULL, StartDate = NULL, EndDate = NULL, Message = NULL, entityId = 1, query = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment())

		body <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper()) %>% purrr::compact()

		createSkyObject(module = "SpecialEducation", objectName = "EasyIEPException", body = list(DataObject = body), searchFields = append("EasyIEPExceptionID", body %>% names()), entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' Modify an EasyIEPException
	#'
	#' This function modifies an EasyIEPException
	#' @param fieldNames The field values to give the modified EasyIEPException. Each defaults to NULL.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param query Queries to be added to the request. Ex. list(SchoolYearID = 1). Defaults to NULL.
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept SpecialEducation
	#' @return The modified EasyIEPException
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	modifyEasyIEPException <- function(EasyIEPExceptionID, EasyIEPRunHistoryID = NULL, StudentID = NULL, RecordType = NULL, StartDate = NULL, EndDate = NULL, Message = NULL, entityId = 1, query = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment())

		body <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper()) %>% purrr::compact()

		modifySkyObject(module = "SpecialEducation", objectName = "EasyIEPException", objectId = EasyIEPExceptionID, body = list(DataObject = body), searchFields = append("EasyIEPExceptionID", body %>% names()), entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' List EasyIEPImportedRecords
	#'
	#' This function returns a dataframe or json object of EasyIEPImportedRecords
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given EasyIEPImportedRecords. Defaults to FALSE for all return fields which, for convenience, returns all fields for the EasyIEPImportedRecords.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object listed as text. Run \code{\link{getSchemaForObjects}}('EasyIEPImportedRecord') to get more field paths.
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
	#' @concept SpecialEducation
	#' @return A list of EasyIEPImportedRecords
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	listEasyIEPImportedRecords <- function(searchConditionsList = NULL, EasyIEPImportedRecordID = F, EasyIEPRunHistoryID = F, StudentID = F, StartDate = F, EndDate = F, RecordType = F, UpdateType = F, Message = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, fieldPaths = NULL, searchConditionsGroupType = "And", searchSortFieldNamesList = NULL, searchSortFieldNamesDescendingList = NULL, entityId = 1, query = NULL, page = 1, pageSize = 100000, flatten = T, returnResponse = F){

		params <- as.list(environment())

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		listSkyObjects(module = "SpecialEducation", objectName = "EasyIEPImportedRecord", searchFields = searchFields %>% append(fieldPaths), page = page, pageSize = pageSize, SearchConditionsList = searchConditionsList, SearchConditionsGroupType = searchConditionsGroupType, SearchSortFieldNamesList = searchSortFieldNamesList, SearchSortFieldNamesDescendingList = searchSortFieldNamesDescendingList, entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' Get an EasyIEPImportedRecord
	#'
	#' This function returns a dataframe or json object of an EasyIEPImportedRecord
	#' @param EasyIEPImportedRecordID The ID of the EasyIEPImportedRecord to return.
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given EasyIEPImportedRecord. Defaults to FALSE for all return fields which, for convenience, returns all fields for the EasyIEPImportedRecord.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object listed as text. Run \code{\link{getSchemaForObjects}}('EasyIEPImportedRecord') to get more field paths.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param query Queries to be added to the request. Ex. list(SchoolYearID = 1). Defaults to NULL.
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept SpecialEducation
	#' @return A dataframe or of EasyIEPImportedRecord
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	getEasyIEPImportedRecord <- function(EasyIEPImportedRecordID, EasyIEPRunHistoryID = F, StudentID = F, StartDate = F, EndDate = F, RecordType = F, UpdateType = F, Message = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, fieldPaths = NULL, entityId = 1, query = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment()) %>% purrr::keep(names(.) != "EasyIEPImportedRecordID")

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		getSkyObject(module = "SpecialEducation", objectName = "EasyIEPImportedRecord", objectId = EasyIEPImportedRecordID, searchFields = searchFields, entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' Delete an EasyIEPImportedRecord
	#'
	#' This function deletes an EasyIEPImportedRecord
	#' @param EasyIEPImportedRecordID The ID of the EasyIEPImportedRecord to delete
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param query Queries to be added to the request. Ex. list(SchoolYearID = 1). Defaults to NULL.
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept SpecialEducation
	#' @return The EasyIEPImportedRecordID of the deleted EasyIEPImportedRecord.
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	deleteEasyIEPImportedRecord <- function(EasyIEPImportedRecordID, ignoreWarnings = F, entityId = 1, query = NULL, flatten = T, returnResponse = F){

		deleteSkyObject(module = "SpecialEducation", objectName = "EasyIEPImportedRecord", objectId = EasyIEPImportedRecordID, ignoreWarnings = ignoreWarnings, entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' Create an EasyIEPImportedRecord
	#'
	#' This function creates an EasyIEPImportedRecord
	#' @param fieldNames The field values to give the created EasyIEPImportedRecord. Each defaults to NULL.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param query Queries to be added to the request. Ex. list(SchoolYearID = 1). Defaults to NULL.
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept SpecialEducation
	#' @return A newly created EasyIEPImportedRecord
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	createEasyIEPImportedRecord <- function(EasyIEPRunHistoryID = NULL, StudentID = NULL, StartDate = NULL, EndDate = NULL, RecordType = NULL, UpdateType = NULL, Message = NULL, entityId = 1, query = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment())

		body <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper()) %>% purrr::compact()

		createSkyObject(module = "SpecialEducation", objectName = "EasyIEPImportedRecord", body = list(DataObject = body), searchFields = append("EasyIEPImportedRecordID", body %>% names()), entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' Modify an EasyIEPImportedRecord
	#'
	#' This function modifies an EasyIEPImportedRecord
	#' @param fieldNames The field values to give the modified EasyIEPImportedRecord. Each defaults to NULL.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param query Queries to be added to the request. Ex. list(SchoolYearID = 1). Defaults to NULL.
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept SpecialEducation
	#' @return The modified EasyIEPImportedRecord
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	modifyEasyIEPImportedRecord <- function(EasyIEPImportedRecordID, EasyIEPRunHistoryID = NULL, StudentID = NULL, StartDate = NULL, EndDate = NULL, RecordType = NULL, UpdateType = NULL, Message = NULL, entityId = 1, query = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment())

		body <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper()) %>% purrr::compact()

		modifySkyObject(module = "SpecialEducation", objectName = "EasyIEPImportedRecord", objectId = EasyIEPImportedRecordID, body = list(DataObject = body), searchFields = append("EasyIEPImportedRecordID", body %>% names()), entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' List EasyIEPRunHistories
	#'
	#' This function returns a dataframe or json object of EasyIEPRunHistories
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given EasyIEPRunHistories. Defaults to FALSE for all return fields which, for convenience, returns all fields for the EasyIEPRunHistories.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object listed as text. Run \code{\link{getSchemaForObjects}}('EasyIEPRunHistory') to get more field paths.
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
	#' @concept SpecialEducation
	#' @return A list of EasyIEPRunHistories
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	listEasyIEPRunHistories <- function(searchConditionsList = NULL, EasyIEPRunHistoryID = F, SchoolYearID = F, DistrictID = F, StartTime = F, EndTime = F, ProcessType = F, ImportType = F, IsLocked = F, ExceptionCount = F, AddCount = F, UpdateCount = F, DeleteCount = F, Status = F, RunData = F, UserIDCanceledBy = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, RunParameters = F, fieldPaths = NULL, searchConditionsGroupType = "And", searchSortFieldNamesList = NULL, searchSortFieldNamesDescendingList = NULL, entityId = 1, query = NULL, page = 1, pageSize = 100000, flatten = T, returnResponse = F){

		params <- as.list(environment())

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		listSkyObjects(module = "SpecialEducation", objectName = "EasyIEPRunHistory", searchFields = searchFields %>% append(fieldPaths), page = page, pageSize = pageSize, SearchConditionsList = searchConditionsList, SearchConditionsGroupType = searchConditionsGroupType, SearchSortFieldNamesList = searchSortFieldNamesList, SearchSortFieldNamesDescendingList = searchSortFieldNamesDescendingList, entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' Get an EasyIEPRunHistory
	#'
	#' This function returns a dataframe or json object of an EasyIEPRunHistory
	#' @param EasyIEPRunHistoryID The ID of the EasyIEPRunHistory to return.
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given EasyIEPRunHistory. Defaults to FALSE for all return fields which, for convenience, returns all fields for the EasyIEPRunHistory.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object listed as text. Run \code{\link{getSchemaForObjects}}('EasyIEPRunHistory') to get more field paths.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param query Queries to be added to the request. Ex. list(SchoolYearID = 1). Defaults to NULL.
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept SpecialEducation
	#' @return A dataframe or of EasyIEPRunHistory
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	getEasyIEPRunHistory <- function(EasyIEPRunHistoryID, SchoolYearID = F, DistrictID = F, StartTime = F, EndTime = F, ProcessType = F, ImportType = F, IsLocked = F, ExceptionCount = F, AddCount = F, UpdateCount = F, DeleteCount = F, Status = F, RunData = F, UserIDCanceledBy = F, UserIDCreatedBy = F, CreatedTime = F, UserIDModifiedBy = F, ModifiedTime = F, RunParameters = F, fieldPaths = NULL, entityId = 1, query = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment()) %>% purrr::keep(names(.) != "EasyIEPRunHistoryID")

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		getSkyObject(module = "SpecialEducation", objectName = "EasyIEPRunHistory", objectId = EasyIEPRunHistoryID, searchFields = searchFields, entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' Delete an EasyIEPRunHistory
	#'
	#' This function deletes an EasyIEPRunHistory
	#' @param EasyIEPRunHistoryID The ID of the EasyIEPRunHistory to delete
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param query Queries to be added to the request. Ex. list(SchoolYearID = 1). Defaults to NULL.
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept SpecialEducation
	#' @return The EasyIEPRunHistoryID of the deleted EasyIEPRunHistory.
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	deleteEasyIEPRunHistory <- function(EasyIEPRunHistoryID, ignoreWarnings = F, entityId = 1, query = NULL, flatten = T, returnResponse = F){

		deleteSkyObject(module = "SpecialEducation", objectName = "EasyIEPRunHistory", objectId = EasyIEPRunHistoryID, ignoreWarnings = ignoreWarnings, entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' Create an EasyIEPRunHistory
	#'
	#' This function creates an EasyIEPRunHistory
	#' @param fieldNames The field values to give the created EasyIEPRunHistory. Each defaults to NULL.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param query Queries to be added to the request. Ex. list(SchoolYearID = 1). Defaults to NULL.
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept SpecialEducation
	#' @return A newly created EasyIEPRunHistory
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	createEasyIEPRunHistory <- function(SchoolYearID = NULL, DistrictID = NULL, StartTime = NULL, EndTime = NULL, ProcessType = NULL, ImportType = NULL, Status = NULL, UserIDCanceledBy = NULL, entityId = 1, query = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment())

		body <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper()) %>% purrr::compact()

		createSkyObject(module = "SpecialEducation", objectName = "EasyIEPRunHistory", body = list(DataObject = body), searchFields = append("EasyIEPRunHistoryID", body %>% names()), entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' Modify an EasyIEPRunHistory
	#'
	#' This function modifies an EasyIEPRunHistory
	#' @param fieldNames The field values to give the modified EasyIEPRunHistory. Each defaults to NULL.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param query Queries to be added to the request. Ex. list(SchoolYearID = 1). Defaults to NULL.
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept SpecialEducation
	#' @return The modified EasyIEPRunHistory
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	modifyEasyIEPRunHistory <- function(EasyIEPRunHistoryID, SchoolYearID = NULL, DistrictID = NULL, StartTime = NULL, EndTime = NULL, ProcessType = NULL, ImportType = NULL, Status = NULL, UserIDCanceledBy = NULL, entityId = 1, query = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment())

		body <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper()) %>% purrr::compact()

		modifySkyObject(module = "SpecialEducation", objectName = "EasyIEPRunHistory", objectId = EasyIEPRunHistoryID, body = list(DataObject = body), searchFields = append("EasyIEPRunHistoryID", body %>% names()), entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}
