
	#' List TempGradeRangeCopyResults
	#'
	#' This function returns a dataframe or json object of TempGradeRangeCopyResults
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given TempGradeRangeCopyResults. Defaults to FALSE for all return fields which, for convenience, returns all fields for the TempGradeRangeCopyResults.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object listed as text. Run \code{\link{getSchemaForObjects}}('TempGradeRangeCopyResult') to get more field paths.
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
	#' @concept Curriculum
	#' @return A list of TempGradeRangeCopyResults
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	listTempGradeRangeCopyResults <- function(searchConditionsList = NULL, AcademicStandardSubjectCode = F, AcademicStandardSubjectID = F, Code = F, CreatedTime = F, Description = F, DistrictGroupKey = F, EnteredByDistrict = F, ErrorCount = F, GradeRangeHigh = F, GradeRangeLow = F, Guid = F, IsSelected = F, Key = F, ModifiedTime = F, Sequence = F, TempGradeRangeCopyResultID = F, UserIDCreatedBy = F, UserIDModifiedBy = F, fieldPaths = NULL, searchConditionsGroupType = "And", searchSortFieldNamesList = NULL, searchSortFieldNamesDescendingList = NULL, entityId = 1, query = NULL, page = 1, pageSize = 100000, flatten = T, returnResponse = F){

		params <- as.list(environment())

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		listSkyObjects(module = "Curriculum", objectName = "TempGradeRangeCopyResult", searchFields = searchFields %>% append(fieldPaths), page = page, pageSize = pageSize, SearchConditionsList = searchConditionsList, SearchConditionsGroupType = searchConditionsGroupType, SearchSortFieldNamesList = searchSortFieldNamesList, SearchSortFieldNamesDescendingList = searchSortFieldNamesDescendingList, entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' Get a TempGradeRangeCopyResult
	#'
	#' This function returns a dataframe or json object of a TempGradeRangeCopyResult
	#' @param TempGradeRangeCopyResultID The ID of the TempGradeRangeCopyResult to return.
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given TempGradeRangeCopyResult. Defaults to FALSE for all return fields which, for convenience, returns all fields for the TempGradeRangeCopyResult.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object listed as text. Run \code{\link{getSchemaForObjects}}('TempGradeRangeCopyResult') to get more field paths.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param query Queries to be added to the request. Ex. list(SchoolYearID = 1). Defaults to NULL.
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Curriculum
	#' @return A dataframe or of TempGradeRangeCopyResult
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	getTempGradeRangeCopyResult <- function(TempGradeRangeCopyResultID, AcademicStandardSubjectCode = F, AcademicStandardSubjectID = F, Code = F, CreatedTime = F, Description = F, DistrictGroupKey = F, EnteredByDistrict = F, ErrorCount = F, GradeRangeHigh = F, GradeRangeLow = F, Guid = F, IsSelected = F, Key = F, ModifiedTime = F, Sequence = F, UserIDCreatedBy = F, UserIDModifiedBy = F, fieldPaths = NULL, entityId = 1, query = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment()) %>% purrr::keep(names(.) != "TempGradeRangeCopyResultID")

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		getSkyObject(module = "Curriculum", objectName = "TempGradeRangeCopyResult", objectId = TempGradeRangeCopyResultID, searchFields = searchFields, entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' Delete a TempGradeRangeCopyResult
	#'
	#' This function deletes a TempGradeRangeCopyResult
	#' @param TempGradeRangeCopyResultID The ID of the TempGradeRangeCopyResult to delete
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param query Queries to be added to the request. Ex. list(SchoolYearID = 1). Defaults to NULL.
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Curriculum
	#' @return The TempGradeRangeCopyResultID of the deleted TempGradeRangeCopyResult.
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	deleteTempGradeRangeCopyResult <- function(TempGradeRangeCopyResultID, ignoreWarnings = F, entityId = 1, query = NULL, flatten = T, returnResponse = F){

		deleteSkyObject(module = "Curriculum", objectName = "TempGradeRangeCopyResult", objectId = TempGradeRangeCopyResultID, ignoreWarnings = ignoreWarnings, entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' Create a TempGradeRangeCopyResult
	#'
	#' This function creates a TempGradeRangeCopyResult
	#' @param fieldNames The field values to give the created TempGradeRangeCopyResult. Each defaults to NULL.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param query Queries to be added to the request. Ex. list(SchoolYearID = 1). Defaults to NULL.
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Curriculum
	#' @return A newly created TempGradeRangeCopyResult
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	createTempGradeRangeCopyResult <- function(AcademicStandardSubjectCode = NULL, AcademicStandardSubjectID = NULL, Code = NULL, Description = NULL, DistrictGroupKey = NULL, EnteredByDistrict = NULL, ErrorCount = NULL, GradeRangeHigh = NULL, GradeRangeLow = NULL, Guid = NULL, IsSelected = NULL, Key = NULL, Sequence = NULL, entityId = 1, query = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment())

		body <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper()) %>% purrr::compact()

		createSkyObject(module = "Curriculum", objectName = "TempGradeRangeCopyResult", body = list(DataObject = body), searchFields = append("TempGradeRangeCopyResultID", body %>% names()), entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' Modify a TempGradeRangeCopyResult
	#'
	#' This function modifies a TempGradeRangeCopyResult
	#' @param fieldNames The field values to give the modified TempGradeRangeCopyResult. Each defaults to NULL.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param query Queries to be added to the request. Ex. list(SchoolYearID = 1). Defaults to NULL.
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Curriculum
	#' @return The modified TempGradeRangeCopyResult
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	modifyTempGradeRangeCopyResult <- function(TempGradeRangeCopyResultID, AcademicStandardSubjectCode = NULL, AcademicStandardSubjectID = NULL, Code = NULL, Description = NULL, DistrictGroupKey = NULL, EnteredByDistrict = NULL, ErrorCount = NULL, GradeRangeHigh = NULL, GradeRangeLow = NULL, Guid = NULL, IsSelected = NULL, Key = NULL, Sequence = NULL, entityId = 1, query = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment())

		body <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper()) %>% purrr::compact()

		modifySkyObject(module = "Curriculum", objectName = "TempGradeRangeCopyResult", objectId = TempGradeRangeCopyResultID, body = list(DataObject = body), searchFields = append("TempGradeRangeCopyResultID", body %>% names()), entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' List StandardPlacementMNS
	#'
	#' This function returns a dataframe or json object of StandardPlacementMNS
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given StandardPlacementMNS. Defaults to FALSE for all return fields which, for convenience, returns all fields for the StandardPlacementMNS.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object listed as text. Run \code{\link{getSchemaForObjects}}('StandardPlacementMN') to get more field paths.
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
	#' @concept Curriculum
	#' @return A list of StandardPlacementMNS
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	listStandardPlacementMNS <- function(searchConditionsList = NULL, CreatedTime = F, CurriculumYearID = F, ModifiedTime = F, StandardPlacementMNID = F, StateStandardCodeMNID = F, UserIDCreatedBy = F, UserIDModifiedBy = F, fieldPaths = NULL, searchConditionsGroupType = "And", searchSortFieldNamesList = NULL, searchSortFieldNamesDescendingList = NULL, entityId = 1, query = NULL, page = 1, pageSize = 100000, flatten = T, returnResponse = F){

		params <- as.list(environment())

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		listSkyObjects(module = "Curriculum", objectName = "StandardPlacementMN", searchFields = searchFields %>% append(fieldPaths), page = page, pageSize = pageSize, SearchConditionsList = searchConditionsList, SearchConditionsGroupType = searchConditionsGroupType, SearchSortFieldNamesList = searchSortFieldNamesList, SearchSortFieldNamesDescendingList = searchSortFieldNamesDescendingList, entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' Get a StandardPlacementMN
	#'
	#' This function returns a dataframe or json object of a StandardPlacementMN
	#' @param StandardPlacementMNID The ID of the StandardPlacementMN to return.
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given StandardPlacementMN. Defaults to FALSE for all return fields which, for convenience, returns all fields for the StandardPlacementMN.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object listed as text. Run \code{\link{getSchemaForObjects}}('StandardPlacementMN') to get more field paths.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param query Queries to be added to the request. Ex. list(SchoolYearID = 1). Defaults to NULL.
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Curriculum
	#' @return A dataframe or of StandardPlacementMN
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	getStandardPlacementMN <- function(StandardPlacementMNID, CreatedTime = F, CurriculumYearID = F, ModifiedTime = F, StateStandardCodeMNID = F, UserIDCreatedBy = F, UserIDModifiedBy = F, fieldPaths = NULL, entityId = 1, query = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment()) %>% purrr::keep(names(.) != "StandardPlacementMNID")

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		getSkyObject(module = "Curriculum", objectName = "StandardPlacementMN", objectId = StandardPlacementMNID, searchFields = searchFields, entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' Delete a StandardPlacementMN
	#'
	#' This function deletes a StandardPlacementMN
	#' @param StandardPlacementMNID The ID of the StandardPlacementMN to delete
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param query Queries to be added to the request. Ex. list(SchoolYearID = 1). Defaults to NULL.
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Curriculum
	#' @return The StandardPlacementMNID of the deleted StandardPlacementMN.
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	deleteStandardPlacementMN <- function(StandardPlacementMNID, ignoreWarnings = F, entityId = 1, query = NULL, flatten = T, returnResponse = F){

		deleteSkyObject(module = "Curriculum", objectName = "StandardPlacementMN", objectId = StandardPlacementMNID, ignoreWarnings = ignoreWarnings, entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' Create a StandardPlacementMN
	#'
	#' This function creates a StandardPlacementMN
	#' @param fieldNames The field values to give the created StandardPlacementMN. Each defaults to NULL.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param query Queries to be added to the request. Ex. list(SchoolYearID = 1). Defaults to NULL.
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Curriculum
	#' @return A newly created StandardPlacementMN
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	createStandardPlacementMN <- function(CurriculumYearID = NULL, StateStandardCodeMNID = NULL, entityId = 1, query = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment())

		body <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper()) %>% purrr::compact()

		createSkyObject(module = "Curriculum", objectName = "StandardPlacementMN", body = list(DataObject = body), searchFields = append("StandardPlacementMNID", body %>% names()), entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' Modify a StandardPlacementMN
	#'
	#' This function modifies a StandardPlacementMN
	#' @param fieldNames The field values to give the modified StandardPlacementMN. Each defaults to NULL.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param query Queries to be added to the request. Ex. list(SchoolYearID = 1). Defaults to NULL.
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Curriculum
	#' @return The modified StandardPlacementMN
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	modifyStandardPlacementMN <- function(StandardPlacementMNID, CurriculumYearID = NULL, StateStandardCodeMNID = NULL, entityId = 1, query = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment())

		body <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper()) %>% purrr::compact()

		modifySkyObject(module = "Curriculum", objectName = "StandardPlacementMN", objectId = StandardPlacementMNID, body = list(DataObject = body), searchFields = append("StandardPlacementMNID", body %>% names()), entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' List CurriculumYears
	#'
	#' This function returns a dataframe or json object of CurriculumYears
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given CurriculumYears. Defaults to FALSE for all return fields which, for convenience, returns all fields for the CurriculumYears.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object listed as text. Run \code{\link{getSchemaForObjects}}('CurriculumYear') to get more field paths.
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
	#' @concept Curriculum
	#' @return A list of CurriculumYears
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	listCurriculumYears <- function(searchConditionsList = NULL, CreatedTime = F, CurriculumID = F, CurriculumYearID = F, CurriculumYearIDClonedFrom = F, CurriculumYearIDClonedTo = F, CurriculumYearMNID = F, Description = F, DistrictGroupKey = F, FederalAdvancedPlacementCourseTypeID = F, FederalSubjectTypeID = F, HasCourseLevels = F, IsAdultBasicEducation = F, IsEndOfCourse = F, IsFederalDistanceEducation = F, IsFederalDualEnrollment = F, IsFederalProgram = F, IsGraduationRequirement = F, IsStateProgram = F, ModifiedTime = F, ReportToFitnessGram = F, SchoolYearID = F, StateCourseCodeMNID = F, StateEarlyEducationLocationMNID = F, StateStandardAddressedCodeMNID = F, StateSTARAssignmentCodeMNID = F, StateSubjectAreaCodeMNID = F, UserIDCreatedBy = F, UserIDModifiedBy = F, fieldPaths = NULL, searchConditionsGroupType = "And", searchSortFieldNamesList = NULL, searchSortFieldNamesDescendingList = NULL, entityId = 1, query = NULL, page = 1, pageSize = 100000, flatten = T, returnResponse = F){

		params <- as.list(environment())

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		listSkyObjects(module = "Curriculum", objectName = "CurriculumYear", searchFields = searchFields %>% append(fieldPaths), page = page, pageSize = pageSize, SearchConditionsList = searchConditionsList, SearchConditionsGroupType = searchConditionsGroupType, SearchSortFieldNamesList = searchSortFieldNamesList, SearchSortFieldNamesDescendingList = searchSortFieldNamesDescendingList, entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' Get a CurriculumYear
	#'
	#' This function returns a dataframe or json object of a CurriculumYear
	#' @param CurriculumYearID The ID of the CurriculumYear to return.
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given CurriculumYear. Defaults to FALSE for all return fields which, for convenience, returns all fields for the CurriculumYear.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object listed as text. Run \code{\link{getSchemaForObjects}}('CurriculumYear') to get more field paths.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param query Queries to be added to the request. Ex. list(SchoolYearID = 1). Defaults to NULL.
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Curriculum
	#' @return A dataframe or of CurriculumYear
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	getCurriculumYear <- function(CurriculumYearID, CreatedTime = F, CurriculumID = F, CurriculumYearIDClonedFrom = F, CurriculumYearIDClonedTo = F, CurriculumYearMNID = F, Description = F, DistrictGroupKey = F, FederalAdvancedPlacementCourseTypeID = F, FederalSubjectTypeID = F, HasCourseLevels = F, IsAdultBasicEducation = F, IsEndOfCourse = F, IsFederalDistanceEducation = F, IsFederalDualEnrollment = F, IsFederalProgram = F, IsGraduationRequirement = F, IsStateProgram = F, ModifiedTime = F, ReportToFitnessGram = F, SchoolYearID = F, StateCourseCodeMNID = F, StateEarlyEducationLocationMNID = F, StateStandardAddressedCodeMNID = F, StateSTARAssignmentCodeMNID = F, StateSubjectAreaCodeMNID = F, UserIDCreatedBy = F, UserIDModifiedBy = F, fieldPaths = NULL, entityId = 1, query = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment()) %>% purrr::keep(names(.) != "CurriculumYearID")

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		getSkyObject(module = "Curriculum", objectName = "CurriculumYear", objectId = CurriculumYearID, searchFields = searchFields, entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' Delete a CurriculumYear
	#'
	#' This function deletes a CurriculumYear
	#' @param CurriculumYearID The ID of the CurriculumYear to delete
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param query Queries to be added to the request. Ex. list(SchoolYearID = 1). Defaults to NULL.
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Curriculum
	#' @return The CurriculumYearID of the deleted CurriculumYear.
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	deleteCurriculumYear <- function(CurriculumYearID, ignoreWarnings = F, entityId = 1, query = NULL, flatten = T, returnResponse = F){

		deleteSkyObject(module = "Curriculum", objectName = "CurriculumYear", objectId = CurriculumYearID, ignoreWarnings = ignoreWarnings, entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' Create a CurriculumYear
	#'
	#' This function creates a CurriculumYear
	#' @param fieldNames The field values to give the created CurriculumYear. Each defaults to NULL.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param query Queries to be added to the request. Ex. list(SchoolYearID = 1). Defaults to NULL.
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Curriculum
	#' @return A newly created CurriculumYear
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	createCurriculumYear <- function(CurriculumID = NULL, CurriculumYearIDClonedFrom = NULL, Description = NULL, DistrictGroupKey = NULL, FederalAdvancedPlacementCourseTypeID = NULL, FederalSubjectTypeID = NULL, IsAdultBasicEducation = NULL, IsEndOfCourse = NULL, IsFederalDistanceEducation = NULL, IsFederalDualEnrollment = NULL, IsFederalProgram = NULL, IsGraduationRequirement = NULL, IsStateProgram = NULL, ReportToFitnessGram = NULL, SchoolYearID = NULL, StateCourseCodeMNID = NULL, StateEarlyEducationLocationMNID = NULL, StateStandardAddressedCodeMNID = NULL, StateSTARAssignmentCodeMNID = NULL, StateSubjectAreaCodeMNID = NULL, entityId = 1, query = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment())

		body <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper()) %>% purrr::compact()

		createSkyObject(module = "Curriculum", objectName = "CurriculumYear", body = list(DataObject = body), searchFields = append("CurriculumYearID", body %>% names()), entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' Modify a CurriculumYear
	#'
	#' This function modifies a CurriculumYear
	#' @param fieldNames The field values to give the modified CurriculumYear. Each defaults to NULL.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param query Queries to be added to the request. Ex. list(SchoolYearID = 1). Defaults to NULL.
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Curriculum
	#' @return The modified CurriculumYear
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	modifyCurriculumYear <- function(CurriculumYearID, CurriculumID = NULL, CurriculumYearIDClonedFrom = NULL, Description = NULL, DistrictGroupKey = NULL, FederalAdvancedPlacementCourseTypeID = NULL, FederalSubjectTypeID = NULL, IsAdultBasicEducation = NULL, IsEndOfCourse = NULL, IsFederalDistanceEducation = NULL, IsFederalDualEnrollment = NULL, IsFederalProgram = NULL, IsGraduationRequirement = NULL, IsStateProgram = NULL, ReportToFitnessGram = NULL, SchoolYearID = NULL, StateCourseCodeMNID = NULL, StateEarlyEducationLocationMNID = NULL, StateStandardAddressedCodeMNID = NULL, StateSTARAssignmentCodeMNID = NULL, StateSubjectAreaCodeMNID = NULL, entityId = 1, query = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment())

		body <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper()) %>% purrr::compact()

		modifySkyObject(module = "Curriculum", objectName = "CurriculumYear", objectId = CurriculumYearID, body = list(DataObject = body), searchFields = append("CurriculumYearID", body %>% names()), entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' List AcademicStandardHierarchyDepths
	#'
	#' This function returns a dataframe or json object of AcademicStandardHierarchyDepths
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given AcademicStandardHierarchyDepths. Defaults to FALSE for all return fields which, for convenience, returns all fields for the AcademicStandardHierarchyDepths.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object listed as text. Run \code{\link{getSchemaForObjects}}('AcademicStandardHierarchyDepth') to get more field paths.
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
	#' @concept Curriculum
	#' @return A list of AcademicStandardHierarchyDepths
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	listAcademicStandardHierarchyDepths <- function(searchConditionsList = NULL, AcademicStandardHierarchyDepthID = F, AcademicStandardID = F, AcademicStandardIDAtLevel = F, CreatedTime = F, DistrictGroupKey = F, HierarchyDepthID = F, ModifiedTime = F, UserIDCreatedBy = F, UserIDModifiedBy = F, fieldPaths = NULL, searchConditionsGroupType = "And", searchSortFieldNamesList = NULL, searchSortFieldNamesDescendingList = NULL, entityId = 1, query = NULL, page = 1, pageSize = 100000, flatten = T, returnResponse = F){

		params <- as.list(environment())

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		listSkyObjects(module = "Curriculum", objectName = "AcademicStandardHierarchyDepth", searchFields = searchFields %>% append(fieldPaths), page = page, pageSize = pageSize, SearchConditionsList = searchConditionsList, SearchConditionsGroupType = searchConditionsGroupType, SearchSortFieldNamesList = searchSortFieldNamesList, SearchSortFieldNamesDescendingList = searchSortFieldNamesDescendingList, entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' Get an AcademicStandardHierarchyDepth
	#'
	#' This function returns a dataframe or json object of an AcademicStandardHierarchyDepth
	#' @param AcademicStandardHierarchyDepthID The ID of the AcademicStandardHierarchyDepth to return.
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given AcademicStandardHierarchyDepth. Defaults to FALSE for all return fields which, for convenience, returns all fields for the AcademicStandardHierarchyDepth.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object listed as text. Run \code{\link{getSchemaForObjects}}('AcademicStandardHierarchyDepth') to get more field paths.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param query Queries to be added to the request. Ex. list(SchoolYearID = 1). Defaults to NULL.
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Curriculum
	#' @return A dataframe or of AcademicStandardHierarchyDepth
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	getAcademicStandardHierarchyDepth <- function(AcademicStandardHierarchyDepthID, AcademicStandardID = F, AcademicStandardIDAtLevel = F, CreatedTime = F, DistrictGroupKey = F, HierarchyDepthID = F, ModifiedTime = F, UserIDCreatedBy = F, UserIDModifiedBy = F, fieldPaths = NULL, entityId = 1, query = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment()) %>% purrr::keep(names(.) != "AcademicStandardHierarchyDepthID")

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		getSkyObject(module = "Curriculum", objectName = "AcademicStandardHierarchyDepth", objectId = AcademicStandardHierarchyDepthID, searchFields = searchFields, entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' Delete an AcademicStandardHierarchyDepth
	#'
	#' This function deletes an AcademicStandardHierarchyDepth
	#' @param AcademicStandardHierarchyDepthID The ID of the AcademicStandardHierarchyDepth to delete
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param query Queries to be added to the request. Ex. list(SchoolYearID = 1). Defaults to NULL.
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Curriculum
	#' @return The AcademicStandardHierarchyDepthID of the deleted AcademicStandardHierarchyDepth.
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	deleteAcademicStandardHierarchyDepth <- function(AcademicStandardHierarchyDepthID, ignoreWarnings = F, entityId = 1, query = NULL, flatten = T, returnResponse = F){

		deleteSkyObject(module = "Curriculum", objectName = "AcademicStandardHierarchyDepth", objectId = AcademicStandardHierarchyDepthID, ignoreWarnings = ignoreWarnings, entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' Create an AcademicStandardHierarchyDepth
	#'
	#' This function creates an AcademicStandardHierarchyDepth
	#' @param fieldNames The field values to give the created AcademicStandardHierarchyDepth. Each defaults to NULL.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param query Queries to be added to the request. Ex. list(SchoolYearID = 1). Defaults to NULL.
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Curriculum
	#' @return A newly created AcademicStandardHierarchyDepth
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	createAcademicStandardHierarchyDepth <- function(AcademicStandardID = NULL, AcademicStandardIDAtLevel = NULL, DistrictGroupKey = NULL, HierarchyDepthID = NULL, entityId = 1, query = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment())

		body <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper()) %>% purrr::compact()

		createSkyObject(module = "Curriculum", objectName = "AcademicStandardHierarchyDepth", body = list(DataObject = body), searchFields = append("AcademicStandardHierarchyDepthID", body %>% names()), entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' Modify an AcademicStandardHierarchyDepth
	#'
	#' This function modifies an AcademicStandardHierarchyDepth
	#' @param fieldNames The field values to give the modified AcademicStandardHierarchyDepth. Each defaults to NULL.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param query Queries to be added to the request. Ex. list(SchoolYearID = 1). Defaults to NULL.
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Curriculum
	#' @return The modified AcademicStandardHierarchyDepth
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	modifyAcademicStandardHierarchyDepth <- function(AcademicStandardHierarchyDepthID, AcademicStandardID = NULL, AcademicStandardIDAtLevel = NULL, DistrictGroupKey = NULL, HierarchyDepthID = NULL, entityId = 1, query = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment())

		body <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper()) %>% purrr::compact()

		modifySkyObject(module = "Curriculum", objectName = "AcademicStandardHierarchyDepth", objectId = AcademicStandardHierarchyDepthID, body = list(DataObject = body), searchFields = append("AcademicStandardHierarchyDepthID", body %>% names()), entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' List CurriculumCurriculumSubjects
	#'
	#' This function returns a dataframe or json object of CurriculumCurriculumSubjects
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given CurriculumCurriculumSubjects. Defaults to FALSE for all return fields which, for convenience, returns all fields for the CurriculumCurriculumSubjects.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object listed as text. Run \code{\link{getSchemaForObjects}}('CurriculumCurriculumSubject') to get more field paths.
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
	#' @concept Curriculum
	#' @return A list of CurriculumCurriculumSubjects
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	listCurriculumCurriculumSubjects <- function(searchConditionsList = NULL, CreatedTime = F, CurriculumID = F, CurriculumSubjectID = F, CurriculumSubjectIDClonedFrom = F, CurriculumSubjectIDClonedTo = F, DistrictGroupKey = F, IsDefault = F, ModifiedTime = F, NumberOfAttachedCurriculumAcademicStandards = F, SubjectID = F, UserIDCreatedBy = F, UserIDModifiedBy = F, fieldPaths = NULL, searchConditionsGroupType = "And", searchSortFieldNamesList = NULL, searchSortFieldNamesDescendingList = NULL, entityId = 1, query = NULL, page = 1, pageSize = 100000, flatten = T, returnResponse = F){

		params <- as.list(environment())

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		listSkyObjects(module = "Curriculum", objectName = "CurriculumSubject", searchFields = searchFields %>% append(fieldPaths), page = page, pageSize = pageSize, SearchConditionsList = searchConditionsList, SearchConditionsGroupType = searchConditionsGroupType, SearchSortFieldNamesList = searchSortFieldNamesList, SearchSortFieldNamesDescendingList = searchSortFieldNamesDescendingList, entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' Get a CurriculumCurriculumSubject
	#'
	#' This function returns a dataframe or json object of a CurriculumCurriculumSubject
	#' @param CurriculumCurriculumSubjectID The ID of the CurriculumCurriculumSubject to return.
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given CurriculumCurriculumSubject. Defaults to FALSE for all return fields which, for convenience, returns all fields for the CurriculumCurriculumSubject.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object listed as text. Run \code{\link{getSchemaForObjects}}('CurriculumCurriculumSubject') to get more field paths.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param query Queries to be added to the request. Ex. list(SchoolYearID = 1). Defaults to NULL.
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Curriculum
	#' @return A dataframe or of CurriculumCurriculumSubject
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	getCurriculumCurriculumSubject <- function(CurriculumCurriculumSubjectID, CreatedTime = F, CurriculumID = F, CurriculumSubjectID = F, CurriculumSubjectIDClonedFrom = F, CurriculumSubjectIDClonedTo = F, DistrictGroupKey = F, IsDefault = F, ModifiedTime = F, NumberOfAttachedCurriculumAcademicStandards = F, SubjectID = F, UserIDCreatedBy = F, UserIDModifiedBy = F, fieldPaths = NULL, entityId = 1, query = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment()) %>% purrr::keep(names(.) != "CurriculumCurriculumSubjectID")

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		getSkyObject(module = "Curriculum", objectName = "CurriculumSubject", objectId = CurriculumCurriculumSubjectID, searchFields = searchFields, entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' Delete a CurriculumCurriculumSubject
	#'
	#' This function deletes a CurriculumCurriculumSubject
	#' @param CurriculumCurriculumSubjectID The ID of the CurriculumCurriculumSubject to delete
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param query Queries to be added to the request. Ex. list(SchoolYearID = 1). Defaults to NULL.
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Curriculum
	#' @return The CurriculumCurriculumSubjectID of the deleted CurriculumCurriculumSubject.
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	deleteCurriculumCurriculumSubject <- function(CurriculumCurriculumSubjectID, ignoreWarnings = F, entityId = 1, query = NULL, flatten = T, returnResponse = F){

		deleteSkyObject(module = "Curriculum", objectName = "CurriculumSubject", objectId = CurriculumCurriculumSubjectID, ignoreWarnings = ignoreWarnings, entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' Create a CurriculumCurriculumSubject
	#'
	#' This function creates a CurriculumCurriculumSubject
	#' @param fieldNames The field values to give the created CurriculumCurriculumSubject. Each defaults to NULL.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param query Queries to be added to the request. Ex. list(SchoolYearID = 1). Defaults to NULL.
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Curriculum
	#' @return A newly created CurriculumCurriculumSubject
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	createCurriculumCurriculumSubject <- function(CurriculumID = NULL, CurriculumSubjectIDClonedFrom = NULL, DistrictGroupKey = NULL, IsDefault = NULL, SubjectID = NULL, entityId = 1, query = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment())

		body <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper()) %>% purrr::compact()

		createSkyObject(module = "Curriculum", objectName = "CurriculumSubject", body = list(DataObject = body), searchFields = append("CurriculumSubjectID", body %>% names()), entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' Modify a CurriculumCurriculumSubject
	#'
	#' This function modifies a CurriculumCurriculumSubject
	#' @param fieldNames The field values to give the modified CurriculumCurriculumSubject. Each defaults to NULL.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param query Queries to be added to the request. Ex. list(SchoolYearID = 1). Defaults to NULL.
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Curriculum
	#' @return The modified CurriculumCurriculumSubject
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	modifyCurriculumCurriculumSubject <- function(CurriculumSubjectID, CurriculumID = NULL, CurriculumSubjectIDClonedFrom = NULL, DistrictGroupKey = NULL, IsDefault = NULL, SubjectID = NULL, entityId = 1, query = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment())

		body <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper()) %>% purrr::compact()

		modifySkyObject(module = "Curriculum", objectName = "CurriculumSubject", objectId = CurriculumSubjectID, body = list(DataObject = body), searchFields = append("CurriculumSubjectID", body %>% names()), entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' List CurriculumSubjectAcademicStandards
	#'
	#' This function returns a dataframe or json object of CurriculumSubjectAcademicStandards
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given CurriculumSubjectAcademicStandards. Defaults to FALSE for all return fields which, for convenience, returns all fields for the CurriculumSubjectAcademicStandards.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object listed as text. Run \code{\link{getSchemaForObjects}}('CurriculumSubjectAcademicStandard') to get more field paths.
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
	#' @concept Curriculum
	#' @return A list of CurriculumSubjectAcademicStandards
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	listCurriculumSubjectAcademicStandards <- function(searchConditionsList = NULL, CreatedTime = F, CurriculumAcademicStandardID = F, CurriculumSubjectAcademicStandardID = F, CurriculumSubjectID = F, ModifiedTime = F, UserIDCreatedBy = F, UserIDModifiedBy = F, fieldPaths = NULL, searchConditionsGroupType = "And", searchSortFieldNamesList = NULL, searchSortFieldNamesDescendingList = NULL, entityId = 1, query = NULL, page = 1, pageSize = 100000, flatten = T, returnResponse = F){

		params <- as.list(environment())

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		listSkyObjects(module = "Curriculum", objectName = "CurriculumSubjectAcademicStandard", searchFields = searchFields %>% append(fieldPaths), page = page, pageSize = pageSize, SearchConditionsList = searchConditionsList, SearchConditionsGroupType = searchConditionsGroupType, SearchSortFieldNamesList = searchSortFieldNamesList, SearchSortFieldNamesDescendingList = searchSortFieldNamesDescendingList, entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' Get a CurriculumSubjectAcademicStandard
	#'
	#' This function returns a dataframe or json object of a CurriculumSubjectAcademicStandard
	#' @param CurriculumSubjectAcademicStandardID The ID of the CurriculumSubjectAcademicStandard to return.
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given CurriculumSubjectAcademicStandard. Defaults to FALSE for all return fields which, for convenience, returns all fields for the CurriculumSubjectAcademicStandard.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object listed as text. Run \code{\link{getSchemaForObjects}}('CurriculumSubjectAcademicStandard') to get more field paths.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param query Queries to be added to the request. Ex. list(SchoolYearID = 1). Defaults to NULL.
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Curriculum
	#' @return A dataframe or of CurriculumSubjectAcademicStandard
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	getCurriculumSubjectAcademicStandard <- function(CurriculumSubjectAcademicStandardID, CreatedTime = F, CurriculumAcademicStandardID = F, CurriculumSubjectID = F, ModifiedTime = F, UserIDCreatedBy = F, UserIDModifiedBy = F, fieldPaths = NULL, entityId = 1, query = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment()) %>% purrr::keep(names(.) != "CurriculumSubjectAcademicStandardID")

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		getSkyObject(module = "Curriculum", objectName = "CurriculumSubjectAcademicStandard", objectId = CurriculumSubjectAcademicStandardID, searchFields = searchFields, entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' Delete a CurriculumSubjectAcademicStandard
	#'
	#' This function deletes a CurriculumSubjectAcademicStandard
	#' @param CurriculumSubjectAcademicStandardID The ID of the CurriculumSubjectAcademicStandard to delete
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param query Queries to be added to the request. Ex. list(SchoolYearID = 1). Defaults to NULL.
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Curriculum
	#' @return The CurriculumSubjectAcademicStandardID of the deleted CurriculumSubjectAcademicStandard.
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	deleteCurriculumSubjectAcademicStandard <- function(CurriculumSubjectAcademicStandardID, ignoreWarnings = F, entityId = 1, query = NULL, flatten = T, returnResponse = F){

		deleteSkyObject(module = "Curriculum", objectName = "CurriculumSubjectAcademicStandard", objectId = CurriculumSubjectAcademicStandardID, ignoreWarnings = ignoreWarnings, entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' Create a CurriculumSubjectAcademicStandard
	#'
	#' This function creates a CurriculumSubjectAcademicStandard
	#' @param fieldNames The field values to give the created CurriculumSubjectAcademicStandard. Each defaults to NULL.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param query Queries to be added to the request. Ex. list(SchoolYearID = 1). Defaults to NULL.
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Curriculum
	#' @return A newly created CurriculumSubjectAcademicStandard
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	createCurriculumSubjectAcademicStandard <- function(CurriculumAcademicStandardID = NULL, CurriculumSubjectID = NULL, entityId = 1, query = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment())

		body <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper()) %>% purrr::compact()

		createSkyObject(module = "Curriculum", objectName = "CurriculumSubjectAcademicStandard", body = list(DataObject = body), searchFields = append("CurriculumSubjectAcademicStandardID", body %>% names()), entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' Modify a CurriculumSubjectAcademicStandard
	#'
	#' This function modifies a CurriculumSubjectAcademicStandard
	#' @param fieldNames The field values to give the modified CurriculumSubjectAcademicStandard. Each defaults to NULL.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param query Queries to be added to the request. Ex. list(SchoolYearID = 1). Defaults to NULL.
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Curriculum
	#' @return The modified CurriculumSubjectAcademicStandard
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	modifyCurriculumSubjectAcademicStandard <- function(CurriculumSubjectAcademicStandardID, CurriculumAcademicStandardID = NULL, CurriculumSubjectID = NULL, entityId = 1, query = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment())

		body <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper()) %>% purrr::compact()

		modifySkyObject(module = "Curriculum", objectName = "CurriculumSubjectAcademicStandard", objectId = CurriculumSubjectAcademicStandardID, body = list(DataObject = body), searchFields = append("CurriculumSubjectAcademicStandardID", body %>% names()), entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' List HierarchyDepths
	#'
	#' This function returns a dataframe or json object of HierarchyDepths
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given HierarchyDepths. Defaults to FALSE for all return fields which, for convenience, returns all fields for the HierarchyDepths.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object listed as text. Run \code{\link{getSchemaForObjects}}('HierarchyDepth') to get more field paths.
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
	#' @concept Curriculum
	#' @return A list of HierarchyDepths
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	listHierarchyDepths <- function(searchConditionsList = NULL, AcademicStandardSetID = F, Code = F, CreatedTime = F, DepthLevel = F, Description = F, DistrictGroupKey = F, DynamicRelationshipID = F, HierarchyDepthID = F, ModifiedTime = F, UserIDCreatedBy = F, UserIDModifiedBy = F, fieldPaths = NULL, searchConditionsGroupType = "And", searchSortFieldNamesList = NULL, searchSortFieldNamesDescendingList = NULL, entityId = 1, query = NULL, page = 1, pageSize = 100000, flatten = T, returnResponse = F){

		params <- as.list(environment())

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		listSkyObjects(module = "Curriculum", objectName = "HierarchyDepth", searchFields = searchFields %>% append(fieldPaths), page = page, pageSize = pageSize, SearchConditionsList = searchConditionsList, SearchConditionsGroupType = searchConditionsGroupType, SearchSortFieldNamesList = searchSortFieldNamesList, SearchSortFieldNamesDescendingList = searchSortFieldNamesDescendingList, entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' Get a HierarchyDepth
	#'
	#' This function returns a dataframe or json object of a HierarchyDepth
	#' @param HierarchyDepthID The ID of the HierarchyDepth to return.
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given HierarchyDepth. Defaults to FALSE for all return fields which, for convenience, returns all fields for the HierarchyDepth.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object listed as text. Run \code{\link{getSchemaForObjects}}('HierarchyDepth') to get more field paths.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param query Queries to be added to the request. Ex. list(SchoolYearID = 1). Defaults to NULL.
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Curriculum
	#' @return A dataframe or of HierarchyDepth
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	getHierarchyDepth <- function(HierarchyDepthID, AcademicStandardSetID = F, Code = F, CreatedTime = F, DepthLevel = F, Description = F, DistrictGroupKey = F, DynamicRelationshipID = F, ModifiedTime = F, UserIDCreatedBy = F, UserIDModifiedBy = F, fieldPaths = NULL, entityId = 1, query = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment()) %>% purrr::keep(names(.) != "HierarchyDepthID")

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		getSkyObject(module = "Curriculum", objectName = "HierarchyDepth", objectId = HierarchyDepthID, searchFields = searchFields, entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' Delete a HierarchyDepth
	#'
	#' This function deletes a HierarchyDepth
	#' @param HierarchyDepthID The ID of the HierarchyDepth to delete
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param query Queries to be added to the request. Ex. list(SchoolYearID = 1). Defaults to NULL.
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Curriculum
	#' @return The HierarchyDepthID of the deleted HierarchyDepth.
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	deleteHierarchyDepth <- function(HierarchyDepthID, ignoreWarnings = F, entityId = 1, query = NULL, flatten = T, returnResponse = F){

		deleteSkyObject(module = "Curriculum", objectName = "HierarchyDepth", objectId = HierarchyDepthID, ignoreWarnings = ignoreWarnings, entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' Create a HierarchyDepth
	#'
	#' This function creates a HierarchyDepth
	#' @param fieldNames The field values to give the created HierarchyDepth. Each defaults to NULL.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param query Queries to be added to the request. Ex. list(SchoolYearID = 1). Defaults to NULL.
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Curriculum
	#' @return A newly created HierarchyDepth
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	createHierarchyDepth <- function(AcademicStandardSetID = NULL, Code = NULL, DepthLevel = NULL, Description = NULL, DistrictGroupKey = NULL, entityId = 1, query = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment())

		body <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper()) %>% purrr::compact()

		createSkyObject(module = "Curriculum", objectName = "HierarchyDepth", body = list(DataObject = body), searchFields = append("HierarchyDepthID", body %>% names()), entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' Modify a HierarchyDepth
	#'
	#' This function modifies a HierarchyDepth
	#' @param fieldNames The field values to give the modified HierarchyDepth. Each defaults to NULL.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param query Queries to be added to the request. Ex. list(SchoolYearID = 1). Defaults to NULL.
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Curriculum
	#' @return The modified HierarchyDepth
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	modifyHierarchyDepth <- function(HierarchyDepthID, AcademicStandardSetID = NULL, Code = NULL, DepthLevel = NULL, Description = NULL, DistrictGroupKey = NULL, entityId = 1, query = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment())

		body <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper()) %>% purrr::compact()

		modifySkyObject(module = "Curriculum", objectName = "HierarchyDepth", objectId = HierarchyDepthID, body = list(DataObject = body), searchFields = append("HierarchyDepthID", body %>% names()), entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' List TempHierarchyDepths
	#'
	#' This function returns a dataframe or json object of TempHierarchyDepths
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given TempHierarchyDepths. Defaults to FALSE for all return fields which, for convenience, returns all fields for the TempHierarchyDepths.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object listed as text. Run \code{\link{getSchemaForObjects}}('TempHierarchyDepth') to get more field paths.
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
	#' @concept Curriculum
	#' @return A list of TempHierarchyDepths
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	listTempHierarchyDepths <- function(searchConditionsList = NULL, AcademicStandardSetCode = F, AcademicStandardSetDescription = F, Code = F, CreatedTime = F, DepthLevel = F, Description = F, ModifiedTime = F, TempHierarchyDepthID = F, UserIDCreatedBy = F, UserIDModifiedBy = F, fieldPaths = NULL, searchConditionsGroupType = "And", searchSortFieldNamesList = NULL, searchSortFieldNamesDescendingList = NULL, entityId = 1, query = NULL, page = 1, pageSize = 100000, flatten = T, returnResponse = F){

		params <- as.list(environment())

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		listSkyObjects(module = "Curriculum", objectName = "TempHierarchyDepth", searchFields = searchFields %>% append(fieldPaths), page = page, pageSize = pageSize, SearchConditionsList = searchConditionsList, SearchConditionsGroupType = searchConditionsGroupType, SearchSortFieldNamesList = searchSortFieldNamesList, SearchSortFieldNamesDescendingList = searchSortFieldNamesDescendingList, entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' Get a TempHierarchyDepth
	#'
	#' This function returns a dataframe or json object of a TempHierarchyDepth
	#' @param TempHierarchyDepthID The ID of the TempHierarchyDepth to return.
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given TempHierarchyDepth. Defaults to FALSE for all return fields which, for convenience, returns all fields for the TempHierarchyDepth.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object listed as text. Run \code{\link{getSchemaForObjects}}('TempHierarchyDepth') to get more field paths.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param query Queries to be added to the request. Ex. list(SchoolYearID = 1). Defaults to NULL.
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Curriculum
	#' @return A dataframe or of TempHierarchyDepth
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	getTempHierarchyDepth <- function(TempHierarchyDepthID, AcademicStandardSetCode = F, AcademicStandardSetDescription = F, Code = F, CreatedTime = F, DepthLevel = F, Description = F, ModifiedTime = F, UserIDCreatedBy = F, UserIDModifiedBy = F, fieldPaths = NULL, entityId = 1, query = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment()) %>% purrr::keep(names(.) != "TempHierarchyDepthID")

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		getSkyObject(module = "Curriculum", objectName = "TempHierarchyDepth", objectId = TempHierarchyDepthID, searchFields = searchFields, entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' Delete a TempHierarchyDepth
	#'
	#' This function deletes a TempHierarchyDepth
	#' @param TempHierarchyDepthID The ID of the TempHierarchyDepth to delete
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param query Queries to be added to the request. Ex. list(SchoolYearID = 1). Defaults to NULL.
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Curriculum
	#' @return The TempHierarchyDepthID of the deleted TempHierarchyDepth.
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	deleteTempHierarchyDepth <- function(TempHierarchyDepthID, ignoreWarnings = F, entityId = 1, query = NULL, flatten = T, returnResponse = F){

		deleteSkyObject(module = "Curriculum", objectName = "TempHierarchyDepth", objectId = TempHierarchyDepthID, ignoreWarnings = ignoreWarnings, entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' Create a TempHierarchyDepth
	#'
	#' This function creates a TempHierarchyDepth
	#' @param fieldNames The field values to give the created TempHierarchyDepth. Each defaults to NULL.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param query Queries to be added to the request. Ex. list(SchoolYearID = 1). Defaults to NULL.
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Curriculum
	#' @return A newly created TempHierarchyDepth
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	createTempHierarchyDepth <- function(AcademicStandardSetCode = NULL, AcademicStandardSetDescription = NULL, Code = NULL, DepthLevel = NULL, Description = NULL, entityId = 1, query = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment())

		body <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper()) %>% purrr::compact()

		createSkyObject(module = "Curriculum", objectName = "TempHierarchyDepth", body = list(DataObject = body), searchFields = append("TempHierarchyDepthID", body %>% names()), entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' Modify a TempHierarchyDepth
	#'
	#' This function modifies a TempHierarchyDepth
	#' @param fieldNames The field values to give the modified TempHierarchyDepth. Each defaults to NULL.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param query Queries to be added to the request. Ex. list(SchoolYearID = 1). Defaults to NULL.
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Curriculum
	#' @return The modified TempHierarchyDepth
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	modifyTempHierarchyDepth <- function(TempHierarchyDepthID, AcademicStandardSetCode = NULL, AcademicStandardSetDescription = NULL, Code = NULL, DepthLevel = NULL, Description = NULL, entityId = 1, query = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment())

		body <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper()) %>% purrr::compact()

		modifySkyObject(module = "Curriculum", objectName = "TempHierarchyDepth", objectId = TempHierarchyDepthID, body = list(DataObject = body), searchFields = append("TempHierarchyDepthID", body %>% names()), entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' List PrerequisiteCurricula
	#'
	#' This function returns a dataframe or json object of PrerequisiteCurricula
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given PrerequisiteCurricula. Defaults to FALSE for all return fields which, for convenience, returns all fields for the PrerequisiteCurricula.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object listed as text. Run \code{\link{getSchemaForObjects}}('PrerequisiteCurriculum') to get more field paths.
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
	#' @concept Curriculum
	#' @return A list of PrerequisiteCurricula
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	listPrerequisiteCurricula <- function(searchConditionsList = NULL, CreatedTime = F, CurriculumIDRequired = F, ModifiedTime = F, PrerequisiteCurriculumID = F, PrerequisiteID = F, UserIDCreatedBy = F, UserIDModifiedBy = F, fieldPaths = NULL, searchConditionsGroupType = "And", searchSortFieldNamesList = NULL, searchSortFieldNamesDescendingList = NULL, entityId = 1, query = NULL, page = 1, pageSize = 100000, flatten = T, returnResponse = F){

		params <- as.list(environment())

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		listSkyObjects(module = "Curriculum", objectName = "PrerequisiteCurriculum", searchFields = searchFields %>% append(fieldPaths), page = page, pageSize = pageSize, SearchConditionsList = searchConditionsList, SearchConditionsGroupType = searchConditionsGroupType, SearchSortFieldNamesList = searchSortFieldNamesList, SearchSortFieldNamesDescendingList = searchSortFieldNamesDescendingList, entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' Get a PrerequisiteCurriculum
	#'
	#' This function returns a dataframe or json object of a PrerequisiteCurriculum
	#' @param PrerequisiteCurriculumID The ID of the PrerequisiteCurriculum to return.
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given PrerequisiteCurriculum. Defaults to FALSE for all return fields which, for convenience, returns all fields for the PrerequisiteCurriculum.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object listed as text. Run \code{\link{getSchemaForObjects}}('PrerequisiteCurriculum') to get more field paths.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param query Queries to be added to the request. Ex. list(SchoolYearID = 1). Defaults to NULL.
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Curriculum
	#' @return A dataframe or of PrerequisiteCurriculum
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	getPrerequisiteCurriculum <- function(PrerequisiteCurriculumID, CreatedTime = F, CurriculumIDRequired = F, ModifiedTime = F, PrerequisiteID = F, UserIDCreatedBy = F, UserIDModifiedBy = F, fieldPaths = NULL, entityId = 1, query = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment()) %>% purrr::keep(names(.) != "PrerequisiteCurriculumID")

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		getSkyObject(module = "Curriculum", objectName = "PrerequisiteCurriculum", objectId = PrerequisiteCurriculumID, searchFields = searchFields, entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' Delete a PrerequisiteCurriculum
	#'
	#' This function deletes a PrerequisiteCurriculum
	#' @param PrerequisiteCurriculumID The ID of the PrerequisiteCurriculum to delete
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param query Queries to be added to the request. Ex. list(SchoolYearID = 1). Defaults to NULL.
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Curriculum
	#' @return The PrerequisiteCurriculumID of the deleted PrerequisiteCurriculum.
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	deletePrerequisiteCurriculum <- function(PrerequisiteCurriculumID, ignoreWarnings = F, entityId = 1, query = NULL, flatten = T, returnResponse = F){

		deleteSkyObject(module = "Curriculum", objectName = "PrerequisiteCurriculum", objectId = PrerequisiteCurriculumID, ignoreWarnings = ignoreWarnings, entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' Create a PrerequisiteCurriculum
	#'
	#' This function creates a PrerequisiteCurriculum
	#' @param fieldNames The field values to give the created PrerequisiteCurriculum. Each defaults to NULL.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param query Queries to be added to the request. Ex. list(SchoolYearID = 1). Defaults to NULL.
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Curriculum
	#' @return A newly created PrerequisiteCurriculum
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	createPrerequisiteCurriculum <- function(CurriculumIDRequired = NULL, PrerequisiteID = NULL, entityId = 1, query = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment())

		body <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper()) %>% purrr::compact()

		createSkyObject(module = "Curriculum", objectName = "PrerequisiteCurriculum", body = list(DataObject = body), searchFields = append("PrerequisiteCurriculumID", body %>% names()), entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' Modify a PrerequisiteCurriculum
	#'
	#' This function modifies a PrerequisiteCurriculum
	#' @param fieldNames The field values to give the modified PrerequisiteCurriculum. Each defaults to NULL.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param query Queries to be added to the request. Ex. list(SchoolYearID = 1). Defaults to NULL.
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Curriculum
	#' @return The modified PrerequisiteCurriculum
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	modifyPrerequisiteCurriculum <- function(PrerequisiteCurriculumID, CurriculumIDRequired = NULL, PrerequisiteID = NULL, entityId = 1, query = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment())

		body <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper()) %>% purrr::compact()

		modifySkyObject(module = "Curriculum", objectName = "PrerequisiteCurriculum", objectId = PrerequisiteCurriculumID, body = list(DataObject = body), searchFields = append("PrerequisiteCurriculumID", body %>% names()), entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' List TempAcademicStandards
	#'
	#' This function returns a dataframe or json object of TempAcademicStandards
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given TempAcademicStandards. Defaults to FALSE for all return fields which, for convenience, returns all fields for the TempAcademicStandards.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object listed as text. Run \code{\link{getSchemaForObjects}}('TempAcademicStandard') to get more field paths.
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
	#' @concept Curriculum
	#' @return A list of TempAcademicStandards
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	listTempAcademicStandards <- function(searchConditionsList = NULL, CreatedTime = F, Description = F, EnteredByDistrict = F, ExtendedDescription = F, Guid = F, ImportedFrom = F, IsPlaceHolder = F, Key = F, Label = F, Level = F, ModifiedTime = F, ParentGuid = F, Sequence = F, StateNumber = F, TempAcademicStandardGradeRangeID = F, TempAcademicStandardID = F, TempAcademicStandardIDParent = F, TempAcademicStandardSetID = F, UserIDCreatedBy = F, UserIDModifiedBy = F, fieldPaths = NULL, searchConditionsGroupType = "And", searchSortFieldNamesList = NULL, searchSortFieldNamesDescendingList = NULL, entityId = 1, query = NULL, page = 1, pageSize = 100000, flatten = T, returnResponse = F){

		params <- as.list(environment())

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		listSkyObjects(module = "Curriculum", objectName = "TempAcademicStandard", searchFields = searchFields %>% append(fieldPaths), page = page, pageSize = pageSize, SearchConditionsList = searchConditionsList, SearchConditionsGroupType = searchConditionsGroupType, SearchSortFieldNamesList = searchSortFieldNamesList, SearchSortFieldNamesDescendingList = searchSortFieldNamesDescendingList, entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' Get a TempAcademicStandard
	#'
	#' This function returns a dataframe or json object of a TempAcademicStandard
	#' @param TempAcademicStandardID The ID of the TempAcademicStandard to return.
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given TempAcademicStandard. Defaults to FALSE for all return fields which, for convenience, returns all fields for the TempAcademicStandard.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object listed as text. Run \code{\link{getSchemaForObjects}}('TempAcademicStandard') to get more field paths.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param query Queries to be added to the request. Ex. list(SchoolYearID = 1). Defaults to NULL.
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Curriculum
	#' @return A dataframe or of TempAcademicStandard
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	getTempAcademicStandard <- function(TempAcademicStandardID, CreatedTime = F, Description = F, EnteredByDistrict = F, ExtendedDescription = F, Guid = F, ImportedFrom = F, IsPlaceHolder = F, Key = F, Label = F, Level = F, ModifiedTime = F, ParentGuid = F, Sequence = F, StateNumber = F, TempAcademicStandardGradeRangeID = F, TempAcademicStandardIDParent = F, TempAcademicStandardSetID = F, UserIDCreatedBy = F, UserIDModifiedBy = F, fieldPaths = NULL, entityId = 1, query = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment()) %>% purrr::keep(names(.) != "TempAcademicStandardID")

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		getSkyObject(module = "Curriculum", objectName = "TempAcademicStandard", objectId = TempAcademicStandardID, searchFields = searchFields, entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' Delete a TempAcademicStandard
	#'
	#' This function deletes a TempAcademicStandard
	#' @param TempAcademicStandardID The ID of the TempAcademicStandard to delete
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param query Queries to be added to the request. Ex. list(SchoolYearID = 1). Defaults to NULL.
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Curriculum
	#' @return The TempAcademicStandardID of the deleted TempAcademicStandard.
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	deleteTempAcademicStandard <- function(TempAcademicStandardID, ignoreWarnings = F, entityId = 1, query = NULL, flatten = T, returnResponse = F){

		deleteSkyObject(module = "Curriculum", objectName = "TempAcademicStandard", objectId = TempAcademicStandardID, ignoreWarnings = ignoreWarnings, entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' Create a TempAcademicStandard
	#'
	#' This function creates a TempAcademicStandard
	#' @param fieldNames The field values to give the created TempAcademicStandard. Each defaults to NULL.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param query Queries to be added to the request. Ex. list(SchoolYearID = 1). Defaults to NULL.
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Curriculum
	#' @return A newly created TempAcademicStandard
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	createTempAcademicStandard <- function(Description = NULL, EnteredByDistrict = NULL, ExtendedDescription = NULL, Guid = NULL, ImportedFrom = NULL, IsPlaceHolder = NULL, Key = NULL, Label = NULL, Level = NULL, ParentGuid = NULL, Sequence = NULL, StateNumber = NULL, TempAcademicStandardGradeRangeID = NULL, TempAcademicStandardIDParent = NULL, TempAcademicStandardSetID = NULL, entityId = 1, query = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment())

		body <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper()) %>% purrr::compact()

		createSkyObject(module = "Curriculum", objectName = "TempAcademicStandard", body = list(DataObject = body), searchFields = append("TempAcademicStandardID", body %>% names()), entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' Modify a TempAcademicStandard
	#'
	#' This function modifies a TempAcademicStandard
	#' @param fieldNames The field values to give the modified TempAcademicStandard. Each defaults to NULL.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param query Queries to be added to the request. Ex. list(SchoolYearID = 1). Defaults to NULL.
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Curriculum
	#' @return The modified TempAcademicStandard
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	modifyTempAcademicStandard <- function(TempAcademicStandardID, Description = NULL, EnteredByDistrict = NULL, ExtendedDescription = NULL, Guid = NULL, ImportedFrom = NULL, IsPlaceHolder = NULL, Key = NULL, Label = NULL, Level = NULL, ParentGuid = NULL, Sequence = NULL, StateNumber = NULL, TempAcademicStandardGradeRangeID = NULL, TempAcademicStandardIDParent = NULL, TempAcademicStandardSetID = NULL, entityId = 1, query = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment())

		body <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper()) %>% purrr::compact()

		modifySkyObject(module = "Curriculum", objectName = "TempAcademicStandard", objectId = TempAcademicStandardID, body = list(DataObject = body), searchFields = append("TempAcademicStandardID", body %>% names()), entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' List TempAcademicStandardGradeRanges
	#'
	#' This function returns a dataframe or json object of TempAcademicStandardGradeRanges
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given TempAcademicStandardGradeRanges. Defaults to FALSE for all return fields which, for convenience, returns all fields for the TempAcademicStandardGradeRanges.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object listed as text. Run \code{\link{getSchemaForObjects}}('TempAcademicStandardGradeRange') to get more field paths.
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
	#' @concept Curriculum
	#' @return A list of TempAcademicStandardGradeRanges
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	listTempAcademicStandardGradeRanges <- function(searchConditionsList = NULL, Code = F, CreatedTime = F, Description = F, EnteredByDistrict = F, GradeRangeHigh = F, GradeRangeLow = F, Guid = F, ImportedFrom = F, Key = F, ModifiedTime = F, Sequence = F, TempAcademicStandardGradeRangeID = F, TempAcademicStandardSubjectID = F, UserIDCreatedBy = F, UserIDModifiedBy = F, fieldPaths = NULL, searchConditionsGroupType = "And", searchSortFieldNamesList = NULL, searchSortFieldNamesDescendingList = NULL, entityId = 1, query = NULL, page = 1, pageSize = 100000, flatten = T, returnResponse = F){

		params <- as.list(environment())

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		listSkyObjects(module = "Curriculum", objectName = "TempAcademicStandardGradeRange", searchFields = searchFields %>% append(fieldPaths), page = page, pageSize = pageSize, SearchConditionsList = searchConditionsList, SearchConditionsGroupType = searchConditionsGroupType, SearchSortFieldNamesList = searchSortFieldNamesList, SearchSortFieldNamesDescendingList = searchSortFieldNamesDescendingList, entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' Get a TempAcademicStandardGradeRange
	#'
	#' This function returns a dataframe or json object of a TempAcademicStandardGradeRange
	#' @param TempAcademicStandardGradeRangeID The ID of the TempAcademicStandardGradeRange to return.
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given TempAcademicStandardGradeRange. Defaults to FALSE for all return fields which, for convenience, returns all fields for the TempAcademicStandardGradeRange.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object listed as text. Run \code{\link{getSchemaForObjects}}('TempAcademicStandardGradeRange') to get more field paths.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param query Queries to be added to the request. Ex. list(SchoolYearID = 1). Defaults to NULL.
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Curriculum
	#' @return A dataframe or of TempAcademicStandardGradeRange
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	getTempAcademicStandardGradeRange <- function(TempAcademicStandardGradeRangeID, Code = F, CreatedTime = F, Description = F, EnteredByDistrict = F, GradeRangeHigh = F, GradeRangeLow = F, Guid = F, ImportedFrom = F, Key = F, ModifiedTime = F, Sequence = F, TempAcademicStandardSubjectID = F, UserIDCreatedBy = F, UserIDModifiedBy = F, fieldPaths = NULL, entityId = 1, query = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment()) %>% purrr::keep(names(.) != "TempAcademicStandardGradeRangeID")

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		getSkyObject(module = "Curriculum", objectName = "TempAcademicStandardGradeRange", objectId = TempAcademicStandardGradeRangeID, searchFields = searchFields, entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' Delete a TempAcademicStandardGradeRange
	#'
	#' This function deletes a TempAcademicStandardGradeRange
	#' @param TempAcademicStandardGradeRangeID The ID of the TempAcademicStandardGradeRange to delete
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param query Queries to be added to the request. Ex. list(SchoolYearID = 1). Defaults to NULL.
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Curriculum
	#' @return The TempAcademicStandardGradeRangeID of the deleted TempAcademicStandardGradeRange.
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	deleteTempAcademicStandardGradeRange <- function(TempAcademicStandardGradeRangeID, ignoreWarnings = F, entityId = 1, query = NULL, flatten = T, returnResponse = F){

		deleteSkyObject(module = "Curriculum", objectName = "TempAcademicStandardGradeRange", objectId = TempAcademicStandardGradeRangeID, ignoreWarnings = ignoreWarnings, entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' Create a TempAcademicStandardGradeRange
	#'
	#' This function creates a TempAcademicStandardGradeRange
	#' @param fieldNames The field values to give the created TempAcademicStandardGradeRange. Each defaults to NULL.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param query Queries to be added to the request. Ex. list(SchoolYearID = 1). Defaults to NULL.
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Curriculum
	#' @return A newly created TempAcademicStandardGradeRange
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	createTempAcademicStandardGradeRange <- function(Code = NULL, Description = NULL, EnteredByDistrict = NULL, GradeRangeHigh = NULL, GradeRangeLow = NULL, Guid = NULL, ImportedFrom = NULL, Key = NULL, Sequence = NULL, TempAcademicStandardSubjectID = NULL, entityId = 1, query = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment())

		body <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper()) %>% purrr::compact()

		createSkyObject(module = "Curriculum", objectName = "TempAcademicStandardGradeRange", body = list(DataObject = body), searchFields = append("TempAcademicStandardGradeRangeID", body %>% names()), entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' Modify a TempAcademicStandardGradeRange
	#'
	#' This function modifies a TempAcademicStandardGradeRange
	#' @param fieldNames The field values to give the modified TempAcademicStandardGradeRange. Each defaults to NULL.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param query Queries to be added to the request. Ex. list(SchoolYearID = 1). Defaults to NULL.
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Curriculum
	#' @return The modified TempAcademicStandardGradeRange
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	modifyTempAcademicStandardGradeRange <- function(TempAcademicStandardGradeRangeID, Code = NULL, Description = NULL, EnteredByDistrict = NULL, GradeRangeHigh = NULL, GradeRangeLow = NULL, Guid = NULL, ImportedFrom = NULL, Key = NULL, Sequence = NULL, TempAcademicStandardSubjectID = NULL, entityId = 1, query = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment())

		body <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper()) %>% purrr::compact()

		modifySkyObject(module = "Curriculum", objectName = "TempAcademicStandardGradeRange", objectId = TempAcademicStandardGradeRangeID, body = list(DataObject = body), searchFields = append("TempAcademicStandardGradeRangeID", body %>% names()), entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' List TempAcademicStandardSets
	#'
	#' This function returns a dataframe or json object of TempAcademicStandardSets
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given TempAcademicStandardSets. Defaults to FALSE for all return fields which, for convenience, returns all fields for the TempAcademicStandardSets.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object listed as text. Run \code{\link{getSchemaForObjects}}('TempAcademicStandardSet') to get more field paths.
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
	#' @concept Curriculum
	#' @return A list of TempAcademicStandardSets
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	listTempAcademicStandardSets <- function(searchConditionsList = NULL, Code = F, CreatedTime = F, Description = F, DistrictID = F, EnteredByDistrict = F, ImportedFrom = F, IsActive = F, Key = F, ModifiedTime = F, StateCode = F, TempAcademicStandardSetID = F, UserIDCreatedBy = F, UserIDModifiedBy = F, fieldPaths = NULL, searchConditionsGroupType = "And", searchSortFieldNamesList = NULL, searchSortFieldNamesDescendingList = NULL, entityId = 1, query = NULL, page = 1, pageSize = 100000, flatten = T, returnResponse = F){

		params <- as.list(environment())

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		listSkyObjects(module = "Curriculum", objectName = "TempAcademicStandardSet", searchFields = searchFields %>% append(fieldPaths), page = page, pageSize = pageSize, SearchConditionsList = searchConditionsList, SearchConditionsGroupType = searchConditionsGroupType, SearchSortFieldNamesList = searchSortFieldNamesList, SearchSortFieldNamesDescendingList = searchSortFieldNamesDescendingList, entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' Get a TempAcademicStandardSet
	#'
	#' This function returns a dataframe or json object of a TempAcademicStandardSet
	#' @param TempAcademicStandardSetID The ID of the TempAcademicStandardSet to return.
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given TempAcademicStandardSet. Defaults to FALSE for all return fields which, for convenience, returns all fields for the TempAcademicStandardSet.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object listed as text. Run \code{\link{getSchemaForObjects}}('TempAcademicStandardSet') to get more field paths.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param query Queries to be added to the request. Ex. list(SchoolYearID = 1). Defaults to NULL.
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Curriculum
	#' @return A dataframe or of TempAcademicStandardSet
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	getTempAcademicStandardSet <- function(TempAcademicStandardSetID, Code = F, CreatedTime = F, Description = F, DistrictID = F, EnteredByDistrict = F, ImportedFrom = F, IsActive = F, Key = F, ModifiedTime = F, StateCode = F, UserIDCreatedBy = F, UserIDModifiedBy = F, fieldPaths = NULL, entityId = 1, query = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment()) %>% purrr::keep(names(.) != "TempAcademicStandardSetID")

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		getSkyObject(module = "Curriculum", objectName = "TempAcademicStandardSet", objectId = TempAcademicStandardSetID, searchFields = searchFields, entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' Delete a TempAcademicStandardSet
	#'
	#' This function deletes a TempAcademicStandardSet
	#' @param TempAcademicStandardSetID The ID of the TempAcademicStandardSet to delete
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param query Queries to be added to the request. Ex. list(SchoolYearID = 1). Defaults to NULL.
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Curriculum
	#' @return The TempAcademicStandardSetID of the deleted TempAcademicStandardSet.
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	deleteTempAcademicStandardSet <- function(TempAcademicStandardSetID, ignoreWarnings = F, entityId = 1, query = NULL, flatten = T, returnResponse = F){

		deleteSkyObject(module = "Curriculum", objectName = "TempAcademicStandardSet", objectId = TempAcademicStandardSetID, ignoreWarnings = ignoreWarnings, entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' Create a TempAcademicStandardSet
	#'
	#' This function creates a TempAcademicStandardSet
	#' @param fieldNames The field values to give the created TempAcademicStandardSet. Each defaults to NULL.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param query Queries to be added to the request. Ex. list(SchoolYearID = 1). Defaults to NULL.
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Curriculum
	#' @return A newly created TempAcademicStandardSet
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	createTempAcademicStandardSet <- function(Code = NULL, Description = NULL, DistrictID = NULL, EnteredByDistrict = NULL, ImportedFrom = NULL, IsActive = NULL, Key = NULL, StateCode = NULL, entityId = 1, query = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment())

		body <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper()) %>% purrr::compact()

		createSkyObject(module = "Curriculum", objectName = "TempAcademicStandardSet", body = list(DataObject = body), searchFields = append("TempAcademicStandardSetID", body %>% names()), entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' Modify a TempAcademicStandardSet
	#'
	#' This function modifies a TempAcademicStandardSet
	#' @param fieldNames The field values to give the modified TempAcademicStandardSet. Each defaults to NULL.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param query Queries to be added to the request. Ex. list(SchoolYearID = 1). Defaults to NULL.
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Curriculum
	#' @return The modified TempAcademicStandardSet
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	modifyTempAcademicStandardSet <- function(TempAcademicStandardSetID, Code = NULL, Description = NULL, DistrictID = NULL, EnteredByDistrict = NULL, ImportedFrom = NULL, IsActive = NULL, Key = NULL, StateCode = NULL, entityId = 1, query = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment())

		body <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper()) %>% purrr::compact()

		modifySkyObject(module = "Curriculum", objectName = "TempAcademicStandardSet", objectId = TempAcademicStandardSetID, body = list(DataObject = body), searchFields = append("TempAcademicStandardSetID", body %>% names()), entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' List TempAcademicStandardSubjects
	#'
	#' This function returns a dataframe or json object of TempAcademicStandardSubjects
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given TempAcademicStandardSubjects. Defaults to FALSE for all return fields which, for convenience, returns all fields for the TempAcademicStandardSubjects.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object listed as text. Run \code{\link{getSchemaForObjects}}('TempAcademicStandardSubject') to get more field paths.
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
	#' @concept Curriculum
	#' @return A list of TempAcademicStandardSubjects
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	listTempAcademicStandardSubjects <- function(searchConditionsList = NULL, Code = F, CreatedTime = F, Description = F, EnteredByDistrict = F, ImportedFrom = F, Key = F, ModifiedTime = F, Sequence = F, TempAcademicStandardSetID = F, TempAcademicStandardSubjectID = F, UserIDCreatedBy = F, UserIDModifiedBy = F, Year = F, fieldPaths = NULL, searchConditionsGroupType = "And", searchSortFieldNamesList = NULL, searchSortFieldNamesDescendingList = NULL, entityId = 1, query = NULL, page = 1, pageSize = 100000, flatten = T, returnResponse = F){

		params <- as.list(environment())

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		listSkyObjects(module = "Curriculum", objectName = "TempAcademicStandardSubject", searchFields = searchFields %>% append(fieldPaths), page = page, pageSize = pageSize, SearchConditionsList = searchConditionsList, SearchConditionsGroupType = searchConditionsGroupType, SearchSortFieldNamesList = searchSortFieldNamesList, SearchSortFieldNamesDescendingList = searchSortFieldNamesDescendingList, entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' Get a TempAcademicStandardSubject
	#'
	#' This function returns a dataframe or json object of a TempAcademicStandardSubject
	#' @param TempAcademicStandardSubjectID The ID of the TempAcademicStandardSubject to return.
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given TempAcademicStandardSubject. Defaults to FALSE for all return fields which, for convenience, returns all fields for the TempAcademicStandardSubject.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object listed as text. Run \code{\link{getSchemaForObjects}}('TempAcademicStandardSubject') to get more field paths.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param query Queries to be added to the request. Ex. list(SchoolYearID = 1). Defaults to NULL.
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Curriculum
	#' @return A dataframe or of TempAcademicStandardSubject
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	getTempAcademicStandardSubject <- function(TempAcademicStandardSubjectID, Code = F, CreatedTime = F, Description = F, EnteredByDistrict = F, ImportedFrom = F, Key = F, ModifiedTime = F, Sequence = F, TempAcademicStandardSetID = F, UserIDCreatedBy = F, UserIDModifiedBy = F, Year = F, fieldPaths = NULL, entityId = 1, query = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment()) %>% purrr::keep(names(.) != "TempAcademicStandardSubjectID")

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		getSkyObject(module = "Curriculum", objectName = "TempAcademicStandardSubject", objectId = TempAcademicStandardSubjectID, searchFields = searchFields, entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' Delete a TempAcademicStandardSubject
	#'
	#' This function deletes a TempAcademicStandardSubject
	#' @param TempAcademicStandardSubjectID The ID of the TempAcademicStandardSubject to delete
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param query Queries to be added to the request. Ex. list(SchoolYearID = 1). Defaults to NULL.
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Curriculum
	#' @return The TempAcademicStandardSubjectID of the deleted TempAcademicStandardSubject.
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	deleteTempAcademicStandardSubject <- function(TempAcademicStandardSubjectID, ignoreWarnings = F, entityId = 1, query = NULL, flatten = T, returnResponse = F){

		deleteSkyObject(module = "Curriculum", objectName = "TempAcademicStandardSubject", objectId = TempAcademicStandardSubjectID, ignoreWarnings = ignoreWarnings, entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' Create a TempAcademicStandardSubject
	#'
	#' This function creates a TempAcademicStandardSubject
	#' @param fieldNames The field values to give the created TempAcademicStandardSubject. Each defaults to NULL.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param query Queries to be added to the request. Ex. list(SchoolYearID = 1). Defaults to NULL.
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Curriculum
	#' @return A newly created TempAcademicStandardSubject
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	createTempAcademicStandardSubject <- function(Code = NULL, Description = NULL, EnteredByDistrict = NULL, ImportedFrom = NULL, Key = NULL, Sequence = NULL, TempAcademicStandardSetID = NULL, Year = NULL, entityId = 1, query = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment())

		body <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper()) %>% purrr::compact()

		createSkyObject(module = "Curriculum", objectName = "TempAcademicStandardSubject", body = list(DataObject = body), searchFields = append("TempAcademicStandardSubjectID", body %>% names()), entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' Modify a TempAcademicStandardSubject
	#'
	#' This function modifies a TempAcademicStandardSubject
	#' @param fieldNames The field values to give the modified TempAcademicStandardSubject. Each defaults to NULL.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param query Queries to be added to the request. Ex. list(SchoolYearID = 1). Defaults to NULL.
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Curriculum
	#' @return The modified TempAcademicStandardSubject
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	modifyTempAcademicStandardSubject <- function(TempAcademicStandardSubjectID, Code = NULL, Description = NULL, EnteredByDistrict = NULL, ImportedFrom = NULL, Key = NULL, Sequence = NULL, TempAcademicStandardSetID = NULL, Year = NULL, entityId = 1, query = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment())

		body <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper()) %>% purrr::compact()

		modifySkyObject(module = "Curriculum", objectName = "TempAcademicStandardSubject", objectId = TempAcademicStandardSubjectID, body = list(DataObject = body), searchFields = append("TempAcademicStandardSubjectID", body %>% names()), entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' List CurriculumGradeLevels
	#'
	#' This function returns a dataframe or json object of CurriculumGradeLevels
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given CurriculumGradeLevels. Defaults to FALSE for all return fields which, for convenience, returns all fields for the CurriculumGradeLevels.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object listed as text. Run \code{\link{getSchemaForObjects}}('CurriculumGradeLevel') to get more field paths.
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
	#' @concept Curriculum
	#' @return A list of CurriculumGradeLevels
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	listCurriculumGradeLevels <- function(searchConditionsList = NULL, CreatedTime = F, CurriculumGradeLevelID = F, CurriculumID = F, DistrictGroupKey = F, GradeLevelID = F, ModifiedTime = F, UserIDCreatedBy = F, UserIDModifiedBy = F, fieldPaths = NULL, searchConditionsGroupType = "And", searchSortFieldNamesList = NULL, searchSortFieldNamesDescendingList = NULL, entityId = 1, query = NULL, page = 1, pageSize = 100000, flatten = T, returnResponse = F){

		params <- as.list(environment())

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		listSkyObjects(module = "Curriculum", objectName = "CurriculumGradeLevel", searchFields = searchFields %>% append(fieldPaths), page = page, pageSize = pageSize, SearchConditionsList = searchConditionsList, SearchConditionsGroupType = searchConditionsGroupType, SearchSortFieldNamesList = searchSortFieldNamesList, SearchSortFieldNamesDescendingList = searchSortFieldNamesDescendingList, entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' Get a CurriculumGradeLevel
	#'
	#' This function returns a dataframe or json object of a CurriculumGradeLevel
	#' @param CurriculumGradeLevelID The ID of the CurriculumGradeLevel to return.
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given CurriculumGradeLevel. Defaults to FALSE for all return fields which, for convenience, returns all fields for the CurriculumGradeLevel.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object listed as text. Run \code{\link{getSchemaForObjects}}('CurriculumGradeLevel') to get more field paths.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param query Queries to be added to the request. Ex. list(SchoolYearID = 1). Defaults to NULL.
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Curriculum
	#' @return A dataframe or of CurriculumGradeLevel
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	getCurriculumGradeLevel <- function(CurriculumGradeLevelID, CreatedTime = F, CurriculumID = F, DistrictGroupKey = F, GradeLevelID = F, ModifiedTime = F, UserIDCreatedBy = F, UserIDModifiedBy = F, fieldPaths = NULL, entityId = 1, query = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment()) %>% purrr::keep(names(.) != "CurriculumGradeLevelID")

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		getSkyObject(module = "Curriculum", objectName = "CurriculumGradeLevel", objectId = CurriculumGradeLevelID, searchFields = searchFields, entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' Delete a CurriculumGradeLevel
	#'
	#' This function deletes a CurriculumGradeLevel
	#' @param CurriculumGradeLevelID The ID of the CurriculumGradeLevel to delete
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param query Queries to be added to the request. Ex. list(SchoolYearID = 1). Defaults to NULL.
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Curriculum
	#' @return The CurriculumGradeLevelID of the deleted CurriculumGradeLevel.
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	deleteCurriculumGradeLevel <- function(CurriculumGradeLevelID, ignoreWarnings = F, entityId = 1, query = NULL, flatten = T, returnResponse = F){

		deleteSkyObject(module = "Curriculum", objectName = "CurriculumGradeLevel", objectId = CurriculumGradeLevelID, ignoreWarnings = ignoreWarnings, entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' Create a CurriculumGradeLevel
	#'
	#' This function creates a CurriculumGradeLevel
	#' @param fieldNames The field values to give the created CurriculumGradeLevel. Each defaults to NULL.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param query Queries to be added to the request. Ex. list(SchoolYearID = 1). Defaults to NULL.
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Curriculum
	#' @return A newly created CurriculumGradeLevel
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	createCurriculumGradeLevel <- function(CurriculumID = NULL, DistrictGroupKey = NULL, GradeLevelID = NULL, entityId = 1, query = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment())

		body <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper()) %>% purrr::compact()

		createSkyObject(module = "Curriculum", objectName = "CurriculumGradeLevel", body = list(DataObject = body), searchFields = append("CurriculumGradeLevelID", body %>% names()), entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' Modify a CurriculumGradeLevel
	#'
	#' This function modifies a CurriculumGradeLevel
	#' @param fieldNames The field values to give the modified CurriculumGradeLevel. Each defaults to NULL.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param query Queries to be added to the request. Ex. list(SchoolYearID = 1). Defaults to NULL.
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Curriculum
	#' @return The modified CurriculumGradeLevel
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	modifyCurriculumGradeLevel <- function(CurriculumGradeLevelID, CurriculumID = NULL, DistrictGroupKey = NULL, GradeLevelID = NULL, entityId = 1, query = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment())

		body <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper()) %>% purrr::compact()

		modifySkyObject(module = "Curriculum", objectName = "CurriculumGradeLevel", objectId = CurriculumGradeLevelID, body = list(DataObject = body), searchFields = append("CurriculumGradeLevelID", body %>% names()), entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' List AcademicStandardSets
	#'
	#' This function returns a dataframe or json object of AcademicStandardSets
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given AcademicStandardSets. Defaults to FALSE for all return fields which, for convenience, returns all fields for the AcademicStandardSets.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object listed as text. Run \code{\link{getSchemaForObjects}}('AcademicStandardSet') to get more field paths.
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
	#' @concept Curriculum
	#' @return A list of AcademicStandardSets
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	listAcademicStandardSets <- function(searchConditionsList = NULL, AcademicStandardSetDefaultID = F, AcademicStandardSetID = F, Code = F, CodeDescription = F, CreatedTime = F, Description = F, DistrictGroupKey = F, DistrictID = F, EnteredByDistrict = F, IsActive = F, Key = F, ModifiedTime = F, StateCode = F, UserIDCreatedBy = F, UserIDModifiedBy = F, fieldPaths = NULL, searchConditionsGroupType = "And", searchSortFieldNamesList = NULL, searchSortFieldNamesDescendingList = NULL, entityId = 1, query = NULL, page = 1, pageSize = 100000, flatten = T, returnResponse = F){

		params <- as.list(environment())

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		listSkyObjects(module = "Curriculum", objectName = "AcademicStandardSet", searchFields = searchFields %>% append(fieldPaths), page = page, pageSize = pageSize, SearchConditionsList = searchConditionsList, SearchConditionsGroupType = searchConditionsGroupType, SearchSortFieldNamesList = searchSortFieldNamesList, SearchSortFieldNamesDescendingList = searchSortFieldNamesDescendingList, entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' Get an AcademicStandardSet
	#'
	#' This function returns a dataframe or json object of an AcademicStandardSet
	#' @param AcademicStandardSetID The ID of the AcademicStandardSet to return.
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given AcademicStandardSet. Defaults to FALSE for all return fields which, for convenience, returns all fields for the AcademicStandardSet.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object listed as text. Run \code{\link{getSchemaForObjects}}('AcademicStandardSet') to get more field paths.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param query Queries to be added to the request. Ex. list(SchoolYearID = 1). Defaults to NULL.
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Curriculum
	#' @return A dataframe or of AcademicStandardSet
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	getAcademicStandardSet <- function(AcademicStandardSetID, AcademicStandardSetDefaultID = F, Code = F, CodeDescription = F, CreatedTime = F, Description = F, DistrictGroupKey = F, DistrictID = F, EnteredByDistrict = F, IsActive = F, Key = F, ModifiedTime = F, StateCode = F, UserIDCreatedBy = F, UserIDModifiedBy = F, fieldPaths = NULL, entityId = 1, query = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment()) %>% purrr::keep(names(.) != "AcademicStandardSetID")

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		getSkyObject(module = "Curriculum", objectName = "AcademicStandardSet", objectId = AcademicStandardSetID, searchFields = searchFields, entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' Delete an AcademicStandardSet
	#'
	#' This function deletes an AcademicStandardSet
	#' @param AcademicStandardSetID The ID of the AcademicStandardSet to delete
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param query Queries to be added to the request. Ex. list(SchoolYearID = 1). Defaults to NULL.
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Curriculum
	#' @return The AcademicStandardSetID of the deleted AcademicStandardSet.
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	deleteAcademicStandardSet <- function(AcademicStandardSetID, ignoreWarnings = F, entityId = 1, query = NULL, flatten = T, returnResponse = F){

		deleteSkyObject(module = "Curriculum", objectName = "AcademicStandardSet", objectId = AcademicStandardSetID, ignoreWarnings = ignoreWarnings, entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' Create an AcademicStandardSet
	#'
	#' This function creates an AcademicStandardSet
	#' @param fieldNames The field values to give the created AcademicStandardSet. Each defaults to NULL.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param query Queries to be added to the request. Ex. list(SchoolYearID = 1). Defaults to NULL.
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Curriculum
	#' @return A newly created AcademicStandardSet
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	createAcademicStandardSet <- function(AcademicStandardSetDefaultID = NULL, Code = NULL, Description = NULL, DistrictGroupKey = NULL, DistrictID = NULL, EnteredByDistrict = NULL, IsActive = NULL, Key = NULL, StateCode = NULL, entityId = 1, query = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment())

		body <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper()) %>% purrr::compact()

		createSkyObject(module = "Curriculum", objectName = "AcademicStandardSet", body = list(DataObject = body), searchFields = append("AcademicStandardSetID", body %>% names()), entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' Modify an AcademicStandardSet
	#'
	#' This function modifies an AcademicStandardSet
	#' @param fieldNames The field values to give the modified AcademicStandardSet. Each defaults to NULL.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param query Queries to be added to the request. Ex. list(SchoolYearID = 1). Defaults to NULL.
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Curriculum
	#' @return The modified AcademicStandardSet
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	modifyAcademicStandardSet <- function(AcademicStandardSetID, AcademicStandardSetDefaultID = NULL, Code = NULL, Description = NULL, DistrictGroupKey = NULL, DistrictID = NULL, EnteredByDistrict = NULL, IsActive = NULL, Key = NULL, StateCode = NULL, entityId = 1, query = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment())

		body <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper()) %>% purrr::compact()

		modifySkyObject(module = "Curriculum", objectName = "AcademicStandardSet", objectId = AcademicStandardSetID, body = list(DataObject = body), searchFields = append("AcademicStandardSetID", body %>% names()), entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' List AcademicStandardSubjects
	#'
	#' This function returns a dataframe or json object of AcademicStandardSubjects
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given AcademicStandardSubjects. Defaults to FALSE for all return fields which, for convenience, returns all fields for the AcademicStandardSubjects.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object listed as text. Run \code{\link{getSchemaForObjects}}('AcademicStandardSubject') to get more field paths.
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
	#' @concept Curriculum
	#' @return A list of AcademicStandardSubjects
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	listAcademicStandardSubjects <- function(searchConditionsList = NULL, AcademicStandardSetID = F, AcademicStandardSubjectDefaultID = F, AcademicStandardSubjectID = F, Code = F, CreatedTime = F, Description = F, DistrictGroupKey = F, EnteredByDistrict = F, FullKey = F, FullKeyPrefix = F, Key = F, ModifiedTime = F, Sequence = F, UserIDCreatedBy = F, UserIDModifiedBy = F, Year = F, fieldPaths = NULL, searchConditionsGroupType = "And", searchSortFieldNamesList = NULL, searchSortFieldNamesDescendingList = NULL, entityId = 1, query = NULL, page = 1, pageSize = 100000, flatten = T, returnResponse = F){

		params <- as.list(environment())

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		listSkyObjects(module = "Curriculum", objectName = "AcademicStandardSubject", searchFields = searchFields %>% append(fieldPaths), page = page, pageSize = pageSize, SearchConditionsList = searchConditionsList, SearchConditionsGroupType = searchConditionsGroupType, SearchSortFieldNamesList = searchSortFieldNamesList, SearchSortFieldNamesDescendingList = searchSortFieldNamesDescendingList, entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' Get an AcademicStandardSubject
	#'
	#' This function returns a dataframe or json object of an AcademicStandardSubject
	#' @param AcademicStandardSubjectID The ID of the AcademicStandardSubject to return.
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given AcademicStandardSubject. Defaults to FALSE for all return fields which, for convenience, returns all fields for the AcademicStandardSubject.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object listed as text. Run \code{\link{getSchemaForObjects}}('AcademicStandardSubject') to get more field paths.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param query Queries to be added to the request. Ex. list(SchoolYearID = 1). Defaults to NULL.
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Curriculum
	#' @return A dataframe or of AcademicStandardSubject
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	getAcademicStandardSubject <- function(AcademicStandardSubjectID, AcademicStandardSetID = F, AcademicStandardSubjectDefaultID = F, Code = F, CreatedTime = F, Description = F, DistrictGroupKey = F, EnteredByDistrict = F, FullKey = F, FullKeyPrefix = F, Key = F, ModifiedTime = F, Sequence = F, UserIDCreatedBy = F, UserIDModifiedBy = F, Year = F, fieldPaths = NULL, entityId = 1, query = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment()) %>% purrr::keep(names(.) != "AcademicStandardSubjectID")

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		getSkyObject(module = "Curriculum", objectName = "AcademicStandardSubject", objectId = AcademicStandardSubjectID, searchFields = searchFields, entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' Delete an AcademicStandardSubject
	#'
	#' This function deletes an AcademicStandardSubject
	#' @param AcademicStandardSubjectID The ID of the AcademicStandardSubject to delete
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param query Queries to be added to the request. Ex. list(SchoolYearID = 1). Defaults to NULL.
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Curriculum
	#' @return The AcademicStandardSubjectID of the deleted AcademicStandardSubject.
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	deleteAcademicStandardSubject <- function(AcademicStandardSubjectID, ignoreWarnings = F, entityId = 1, query = NULL, flatten = T, returnResponse = F){

		deleteSkyObject(module = "Curriculum", objectName = "AcademicStandardSubject", objectId = AcademicStandardSubjectID, ignoreWarnings = ignoreWarnings, entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' Create an AcademicStandardSubject
	#'
	#' This function creates an AcademicStandardSubject
	#' @param fieldNames The field values to give the created AcademicStandardSubject. Each defaults to NULL.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param query Queries to be added to the request. Ex. list(SchoolYearID = 1). Defaults to NULL.
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Curriculum
	#' @return A newly created AcademicStandardSubject
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	createAcademicStandardSubject <- function(AcademicStandardSetID = NULL, AcademicStandardSubjectDefaultID = NULL, Code = NULL, Description = NULL, DistrictGroupKey = NULL, EnteredByDistrict = NULL, Key = NULL, Sequence = NULL, Year = NULL, entityId = 1, query = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment())

		body <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper()) %>% purrr::compact()

		createSkyObject(module = "Curriculum", objectName = "AcademicStandardSubject", body = list(DataObject = body), searchFields = append("AcademicStandardSubjectID", body %>% names()), entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' Modify an AcademicStandardSubject
	#'
	#' This function modifies an AcademicStandardSubject
	#' @param fieldNames The field values to give the modified AcademicStandardSubject. Each defaults to NULL.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param query Queries to be added to the request. Ex. list(SchoolYearID = 1). Defaults to NULL.
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Curriculum
	#' @return The modified AcademicStandardSubject
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	modifyAcademicStandardSubject <- function(AcademicStandardSubjectID, AcademicStandardSetID = NULL, AcademicStandardSubjectDefaultID = NULL, Code = NULL, Description = NULL, DistrictGroupKey = NULL, EnteredByDistrict = NULL, Key = NULL, Sequence = NULL, Year = NULL, entityId = 1, query = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment())

		body <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper()) %>% purrr::compact()

		modifySkyObject(module = "Curriculum", objectName = "AcademicStandardSubject", objectId = AcademicStandardSubjectID, body = list(DataObject = body), searchFields = append("AcademicStandardSubjectID", body %>% names()), entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' List AcademicStandardGradeRanges
	#'
	#' This function returns a dataframe or json object of AcademicStandardGradeRanges
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given AcademicStandardGradeRanges. Defaults to FALSE for all return fields which, for convenience, returns all fields for the AcademicStandardGradeRanges.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object listed as text. Run \code{\link{getSchemaForObjects}}('AcademicStandardGradeRange') to get more field paths.
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
	#' @concept Curriculum
	#' @return A list of AcademicStandardGradeRanges
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	listAcademicStandardGradeRanges <- function(searchConditionsList = NULL, AcademicStandardGradeRangeDefaultID = F, AcademicStandardGradeRangeID = F, AcademicStandardSubjectID = F, Code = F, CreatedTime = F, Description = F, DistrictGroupKey = F, EnteredByDistrict = F, FullKey = F, FullKeyPrefix = F, GradeRangeHigh = F, GradeRangeLow = F, Guid = F, Key = F, ModifiedTime = F, Sequence = F, UserIDCreatedBy = F, UserIDModifiedBy = F, fieldPaths = NULL, searchConditionsGroupType = "And", searchSortFieldNamesList = NULL, searchSortFieldNamesDescendingList = NULL, entityId = 1, query = NULL, page = 1, pageSize = 100000, flatten = T, returnResponse = F){

		params <- as.list(environment())

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		listSkyObjects(module = "Curriculum", objectName = "AcademicStandardGradeRange", searchFields = searchFields %>% append(fieldPaths), page = page, pageSize = pageSize, SearchConditionsList = searchConditionsList, SearchConditionsGroupType = searchConditionsGroupType, SearchSortFieldNamesList = searchSortFieldNamesList, SearchSortFieldNamesDescendingList = searchSortFieldNamesDescendingList, entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' Get an AcademicStandardGradeRange
	#'
	#' This function returns a dataframe or json object of an AcademicStandardGradeRange
	#' @param AcademicStandardGradeRangeID The ID of the AcademicStandardGradeRange to return.
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given AcademicStandardGradeRange. Defaults to FALSE for all return fields which, for convenience, returns all fields for the AcademicStandardGradeRange.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object listed as text. Run \code{\link{getSchemaForObjects}}('AcademicStandardGradeRange') to get more field paths.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param query Queries to be added to the request. Ex. list(SchoolYearID = 1). Defaults to NULL.
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Curriculum
	#' @return A dataframe or of AcademicStandardGradeRange
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	getAcademicStandardGradeRange <- function(AcademicStandardGradeRangeID, AcademicStandardGradeRangeDefaultID = F, AcademicStandardSubjectID = F, Code = F, CreatedTime = F, Description = F, DistrictGroupKey = F, EnteredByDistrict = F, FullKey = F, FullKeyPrefix = F, GradeRangeHigh = F, GradeRangeLow = F, Guid = F, Key = F, ModifiedTime = F, Sequence = F, UserIDCreatedBy = F, UserIDModifiedBy = F, fieldPaths = NULL, entityId = 1, query = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment()) %>% purrr::keep(names(.) != "AcademicStandardGradeRangeID")

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		getSkyObject(module = "Curriculum", objectName = "AcademicStandardGradeRange", objectId = AcademicStandardGradeRangeID, searchFields = searchFields, entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' Delete an AcademicStandardGradeRange
	#'
	#' This function deletes an AcademicStandardGradeRange
	#' @param AcademicStandardGradeRangeID The ID of the AcademicStandardGradeRange to delete
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param query Queries to be added to the request. Ex. list(SchoolYearID = 1). Defaults to NULL.
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Curriculum
	#' @return The AcademicStandardGradeRangeID of the deleted AcademicStandardGradeRange.
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	deleteAcademicStandardGradeRange <- function(AcademicStandardGradeRangeID, ignoreWarnings = F, entityId = 1, query = NULL, flatten = T, returnResponse = F){

		deleteSkyObject(module = "Curriculum", objectName = "AcademicStandardGradeRange", objectId = AcademicStandardGradeRangeID, ignoreWarnings = ignoreWarnings, entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' Create an AcademicStandardGradeRange
	#'
	#' This function creates an AcademicStandardGradeRange
	#' @param fieldNames The field values to give the created AcademicStandardGradeRange. Each defaults to NULL.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param query Queries to be added to the request. Ex. list(SchoolYearID = 1). Defaults to NULL.
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Curriculum
	#' @return A newly created AcademicStandardGradeRange
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	createAcademicStandardGradeRange <- function(AcademicStandardGradeRangeDefaultID = NULL, AcademicStandardSubjectID = NULL, Code = NULL, Description = NULL, DistrictGroupKey = NULL, EnteredByDistrict = NULL, GradeRangeHigh = NULL, GradeRangeLow = NULL, Guid = NULL, Key = NULL, Sequence = NULL, entityId = 1, query = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment())

		body <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper()) %>% purrr::compact()

		createSkyObject(module = "Curriculum", objectName = "AcademicStandardGradeRange", body = list(DataObject = body), searchFields = append("AcademicStandardGradeRangeID", body %>% names()), entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' Modify an AcademicStandardGradeRange
	#'
	#' This function modifies an AcademicStandardGradeRange
	#' @param fieldNames The field values to give the modified AcademicStandardGradeRange. Each defaults to NULL.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param query Queries to be added to the request. Ex. list(SchoolYearID = 1). Defaults to NULL.
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Curriculum
	#' @return The modified AcademicStandardGradeRange
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	modifyAcademicStandardGradeRange <- function(AcademicStandardGradeRangeID, AcademicStandardGradeRangeDefaultID = NULL, AcademicStandardSubjectID = NULL, Code = NULL, Description = NULL, DistrictGroupKey = NULL, EnteredByDistrict = NULL, GradeRangeHigh = NULL, GradeRangeLow = NULL, Guid = NULL, Key = NULL, Sequence = NULL, entityId = 1, query = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment())

		body <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper()) %>% purrr::compact()

		modifySkyObject(module = "Curriculum", objectName = "AcademicStandardGradeRange", objectId = AcademicStandardGradeRangeID, body = list(DataObject = body), searchFields = append("AcademicStandardGradeRangeID", body %>% names()), entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' List AcademicStandards
	#'
	#' This function returns a dataframe or json object of AcademicStandards
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given AcademicStandards. Defaults to FALSE for all return fields which, for convenience, returns all fields for the AcademicStandards.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object listed as text. Run \code{\link{getSchemaForObjects}}('AcademicStandard') to get more field paths.
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
	#' @concept Curriculum
	#' @return A list of AcademicStandards
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	listAcademicStandards <- function(searchConditionsList = NULL, AcademicStandardDefaultID = F, AcademicStandardGradeRangeID = F, AcademicStandardID = F, AcademicStandardIDParent = F, BackgroundColor = F, ChildAcademicStandardCount = F, CreatedTime = F, Description = F, DescriptionToUse = F, DisplayAs = F, DistrictGroupKey = F, EnteredByDistrict = F, ExtendedDescription = F, FullKey = F, FullKeyPrefix = F, GrandChildLevelHierarchyDepthDescription = F, Guid = F, HierarchyDepthDescription = F, IsAttachedToASubject = F, IsHighFrequencyWord = F, IsLettersAndSounds = F, IsPlaceHolder = F, Key = F, Label = F, LetterAndSoundType = F, LetterType = F, Level = F, ModifiedTime = F, NextLevelHierarchyDepthDescription = F, ParentGuid = F, Sequence = F, StateNumber = F, TextColor = F, UserIDCreatedBy = F, UserIDModifiedBy = F, fieldPaths = NULL, searchConditionsGroupType = "And", searchSortFieldNamesList = NULL, searchSortFieldNamesDescendingList = NULL, entityId = 1, query = NULL, page = 1, pageSize = 100000, flatten = T, returnResponse = F){

		params <- as.list(environment())

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		listSkyObjects(module = "Curriculum", objectName = "AcademicStandard", searchFields = searchFields %>% append(fieldPaths), page = page, pageSize = pageSize, SearchConditionsList = searchConditionsList, SearchConditionsGroupType = searchConditionsGroupType, SearchSortFieldNamesList = searchSortFieldNamesList, SearchSortFieldNamesDescendingList = searchSortFieldNamesDescendingList, entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' Get an AcademicStandard
	#'
	#' This function returns a dataframe or json object of an AcademicStandard
	#' @param AcademicStandardID The ID of the AcademicStandard to return.
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given AcademicStandard. Defaults to FALSE for all return fields which, for convenience, returns all fields for the AcademicStandard.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object listed as text. Run \code{\link{getSchemaForObjects}}('AcademicStandard') to get more field paths.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param query Queries to be added to the request. Ex. list(SchoolYearID = 1). Defaults to NULL.
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Curriculum
	#' @return A dataframe or of AcademicStandard
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	getAcademicStandard <- function(AcademicStandardID, AcademicStandardDefaultID = F, AcademicStandardGradeRangeID = F, AcademicStandardIDParent = F, BackgroundColor = F, ChildAcademicStandardCount = F, CreatedTime = F, Description = F, DescriptionToUse = F, DisplayAs = F, DistrictGroupKey = F, EnteredByDistrict = F, ExtendedDescription = F, FullKey = F, FullKeyPrefix = F, GrandChildLevelHierarchyDepthDescription = F, Guid = F, HierarchyDepthDescription = F, IsAttachedToASubject = F, IsHighFrequencyWord = F, IsLettersAndSounds = F, IsPlaceHolder = F, Key = F, Label = F, LetterAndSoundType = F, LetterType = F, Level = F, ModifiedTime = F, NextLevelHierarchyDepthDescription = F, ParentGuid = F, Sequence = F, StateNumber = F, TextColor = F, UserIDCreatedBy = F, UserIDModifiedBy = F, fieldPaths = NULL, entityId = 1, query = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment()) %>% purrr::keep(names(.) != "AcademicStandardID")

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		getSkyObject(module = "Curriculum", objectName = "AcademicStandard", objectId = AcademicStandardID, searchFields = searchFields, entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' Delete an AcademicStandard
	#'
	#' This function deletes an AcademicStandard
	#' @param AcademicStandardID The ID of the AcademicStandard to delete
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param query Queries to be added to the request. Ex. list(SchoolYearID = 1). Defaults to NULL.
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Curriculum
	#' @return The AcademicStandardID of the deleted AcademicStandard.
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	deleteAcademicStandard <- function(AcademicStandardID, ignoreWarnings = F, entityId = 1, query = NULL, flatten = T, returnResponse = F){

		deleteSkyObject(module = "Curriculum", objectName = "AcademicStandard", objectId = AcademicStandardID, ignoreWarnings = ignoreWarnings, entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' Create an AcademicStandard
	#'
	#' This function creates an AcademicStandard
	#' @param fieldNames The field values to give the created AcademicStandard. Each defaults to NULL.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param query Queries to be added to the request. Ex. list(SchoolYearID = 1). Defaults to NULL.
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Curriculum
	#' @return A newly created AcademicStandard
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	createAcademicStandard <- function(AcademicStandardDefaultID = NULL, AcademicStandardGradeRangeID = NULL, AcademicStandardIDParent = NULL, BackgroundColor = NULL, Description = NULL, DisplayAs = NULL, DistrictGroupKey = NULL, EnteredByDistrict = NULL, ExtendedDescription = NULL, Guid = NULL, IsHighFrequencyWord = NULL, IsPlaceHolder = NULL, Key = NULL, Label = NULL, LetterAndSoundType = NULL, LetterType = NULL, Level = NULL, ParentGuid = NULL, Sequence = NULL, StateNumber = NULL, TextColor = NULL, entityId = 1, query = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment())

		body <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper()) %>% purrr::compact()

		createSkyObject(module = "Curriculum", objectName = "AcademicStandard", body = list(DataObject = body), searchFields = append("AcademicStandardID", body %>% names()), entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' Modify an AcademicStandard
	#'
	#' This function modifies an AcademicStandard
	#' @param fieldNames The field values to give the modified AcademicStandard. Each defaults to NULL.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param query Queries to be added to the request. Ex. list(SchoolYearID = 1). Defaults to NULL.
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Curriculum
	#' @return The modified AcademicStandard
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	modifyAcademicStandard <- function(AcademicStandardID, AcademicStandardDefaultID = NULL, AcademicStandardGradeRangeID = NULL, AcademicStandardIDParent = NULL, BackgroundColor = NULL, Description = NULL, DisplayAs = NULL, DistrictGroupKey = NULL, EnteredByDistrict = NULL, ExtendedDescription = NULL, Guid = NULL, IsHighFrequencyWord = NULL, IsPlaceHolder = NULL, Key = NULL, Label = NULL, LetterAndSoundType = NULL, LetterType = NULL, Level = NULL, ParentGuid = NULL, Sequence = NULL, StateNumber = NULL, TextColor = NULL, entityId = 1, query = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment())

		body <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper()) %>% purrr::compact()

		modifySkyObject(module = "Curriculum", objectName = "AcademicStandard", objectId = AcademicStandardID, body = list(DataObject = body), searchFields = append("AcademicStandardID", body %>% names()), entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' List CurriculumAcademicStandards
	#'
	#' This function returns a dataframe or json object of CurriculumAcademicStandards
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given CurriculumAcademicStandards. Defaults to FALSE for all return fields which, for convenience, returns all fields for the CurriculumAcademicStandards.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object listed as text. Run \code{\link{getSchemaForObjects}}('CurriculumAcademicStandard') to get more field paths.
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
	#' @concept Curriculum
	#' @return A list of CurriculumAcademicStandards
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	listCurriculumAcademicStandards <- function(searchConditionsList = NULL, AcademicStandardID = F, CreatedTime = F, CurriculumAcademicStandardID = F, CurriculumID = F, DistrictGroupKey = F, IsGraded = F, ModifiedTime = F, UserIDCreatedBy = F, UserIDModifiedBy = F, fieldPaths = NULL, searchConditionsGroupType = "And", searchSortFieldNamesList = NULL, searchSortFieldNamesDescendingList = NULL, entityId = 1, query = NULL, page = 1, pageSize = 100000, flatten = T, returnResponse = F){

		params <- as.list(environment())

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		listSkyObjects(module = "Curriculum", objectName = "CurriculumAcademicStandard", searchFields = searchFields %>% append(fieldPaths), page = page, pageSize = pageSize, SearchConditionsList = searchConditionsList, SearchConditionsGroupType = searchConditionsGroupType, SearchSortFieldNamesList = searchSortFieldNamesList, SearchSortFieldNamesDescendingList = searchSortFieldNamesDescendingList, entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' Get a CurriculumAcademicStandard
	#'
	#' This function returns a dataframe or json object of a CurriculumAcademicStandard
	#' @param CurriculumAcademicStandardID The ID of the CurriculumAcademicStandard to return.
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given CurriculumAcademicStandard. Defaults to FALSE for all return fields which, for convenience, returns all fields for the CurriculumAcademicStandard.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object listed as text. Run \code{\link{getSchemaForObjects}}('CurriculumAcademicStandard') to get more field paths.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param query Queries to be added to the request. Ex. list(SchoolYearID = 1). Defaults to NULL.
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Curriculum
	#' @return A dataframe or of CurriculumAcademicStandard
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	getCurriculumAcademicStandard <- function(CurriculumAcademicStandardID, AcademicStandardID = F, CreatedTime = F, CurriculumID = F, DistrictGroupKey = F, IsGraded = F, ModifiedTime = F, UserIDCreatedBy = F, UserIDModifiedBy = F, fieldPaths = NULL, entityId = 1, query = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment()) %>% purrr::keep(names(.) != "CurriculumAcademicStandardID")

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		getSkyObject(module = "Curriculum", objectName = "CurriculumAcademicStandard", objectId = CurriculumAcademicStandardID, searchFields = searchFields, entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' Delete a CurriculumAcademicStandard
	#'
	#' This function deletes a CurriculumAcademicStandard
	#' @param CurriculumAcademicStandardID The ID of the CurriculumAcademicStandard to delete
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param query Queries to be added to the request. Ex. list(SchoolYearID = 1). Defaults to NULL.
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Curriculum
	#' @return The CurriculumAcademicStandardID of the deleted CurriculumAcademicStandard.
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	deleteCurriculumAcademicStandard <- function(CurriculumAcademicStandardID, ignoreWarnings = F, entityId = 1, query = NULL, flatten = T, returnResponse = F){

		deleteSkyObject(module = "Curriculum", objectName = "CurriculumAcademicStandard", objectId = CurriculumAcademicStandardID, ignoreWarnings = ignoreWarnings, entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' Create a CurriculumAcademicStandard
	#'
	#' This function creates a CurriculumAcademicStandard
	#' @param fieldNames The field values to give the created CurriculumAcademicStandard. Each defaults to NULL.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param query Queries to be added to the request. Ex. list(SchoolYearID = 1). Defaults to NULL.
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Curriculum
	#' @return A newly created CurriculumAcademicStandard
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	createCurriculumAcademicStandard <- function(AcademicStandardID = NULL, CurriculumID = NULL, DistrictGroupKey = NULL, IsGraded = NULL, entityId = 1, query = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment())

		body <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper()) %>% purrr::compact()

		createSkyObject(module = "Curriculum", objectName = "CurriculumAcademicStandard", body = list(DataObject = body), searchFields = append("CurriculumAcademicStandardID", body %>% names()), entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' Modify a CurriculumAcademicStandard
	#'
	#' This function modifies a CurriculumAcademicStandard
	#' @param fieldNames The field values to give the modified CurriculumAcademicStandard. Each defaults to NULL.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param query Queries to be added to the request. Ex. list(SchoolYearID = 1). Defaults to NULL.
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Curriculum
	#' @return The modified CurriculumAcademicStandard
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	modifyCurriculumAcademicStandard <- function(CurriculumAcademicStandardID, AcademicStandardID = NULL, CurriculumID = NULL, DistrictGroupKey = NULL, IsGraded = NULL, entityId = 1, query = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment())

		body <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper()) %>% purrr::compact()

		modifySkyObject(module = "Curriculum", objectName = "CurriculumAcademicStandard", objectId = CurriculumAcademicStandardID, body = list(DataObject = body), searchFields = append("CurriculumAcademicStandardID", body %>% names()), entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' List CurriculumCustomRequirements
	#'
	#' This function returns a dataframe or json object of CurriculumCustomRequirements
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given CurriculumCustomRequirements. Defaults to FALSE for all return fields which, for convenience, returns all fields for the CurriculumCustomRequirements.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object listed as text. Run \code{\link{getSchemaForObjects}}('CurriculumCustomRequirement') to get more field paths.
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
	#' @concept Curriculum
	#' @return A list of CurriculumCustomRequirements
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	listCurriculumCustomRequirements <- function(searchConditionsList = NULL, CreatedTime = F, CurriculumCustomRequirementID = F, CurriculumID = F, CustomRequirementID = F, DistrictGroupKey = F, ModifiedTime = F, SchoolYearHigh = F, SchoolYearLow = F, UserIDCreatedBy = F, UserIDModifiedBy = F, fieldPaths = NULL, searchConditionsGroupType = "And", searchSortFieldNamesList = NULL, searchSortFieldNamesDescendingList = NULL, entityId = 1, query = NULL, page = 1, pageSize = 100000, flatten = T, returnResponse = F){

		params <- as.list(environment())

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		listSkyObjects(module = "Curriculum", objectName = "CurriculumCustomRequirement", searchFields = searchFields %>% append(fieldPaths), page = page, pageSize = pageSize, SearchConditionsList = searchConditionsList, SearchConditionsGroupType = searchConditionsGroupType, SearchSortFieldNamesList = searchSortFieldNamesList, SearchSortFieldNamesDescendingList = searchSortFieldNamesDescendingList, entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' Get a CurriculumCustomRequirement
	#'
	#' This function returns a dataframe or json object of a CurriculumCustomRequirement
	#' @param CurriculumCustomRequirementID The ID of the CurriculumCustomRequirement to return.
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given CurriculumCustomRequirement. Defaults to FALSE for all return fields which, for convenience, returns all fields for the CurriculumCustomRequirement.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object listed as text. Run \code{\link{getSchemaForObjects}}('CurriculumCustomRequirement') to get more field paths.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param query Queries to be added to the request. Ex. list(SchoolYearID = 1). Defaults to NULL.
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Curriculum
	#' @return A dataframe or of CurriculumCustomRequirement
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	getCurriculumCustomRequirement <- function(CurriculumCustomRequirementID, CreatedTime = F, CurriculumID = F, CustomRequirementID = F, DistrictGroupKey = F, ModifiedTime = F, SchoolYearHigh = F, SchoolYearLow = F, UserIDCreatedBy = F, UserIDModifiedBy = F, fieldPaths = NULL, entityId = 1, query = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment()) %>% purrr::keep(names(.) != "CurriculumCustomRequirementID")

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		getSkyObject(module = "Curriculum", objectName = "CurriculumCustomRequirement", objectId = CurriculumCustomRequirementID, searchFields = searchFields, entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' Delete a CurriculumCustomRequirement
	#'
	#' This function deletes a CurriculumCustomRequirement
	#' @param CurriculumCustomRequirementID The ID of the CurriculumCustomRequirement to delete
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param query Queries to be added to the request. Ex. list(SchoolYearID = 1). Defaults to NULL.
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Curriculum
	#' @return The CurriculumCustomRequirementID of the deleted CurriculumCustomRequirement.
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	deleteCurriculumCustomRequirement <- function(CurriculumCustomRequirementID, ignoreWarnings = F, entityId = 1, query = NULL, flatten = T, returnResponse = F){

		deleteSkyObject(module = "Curriculum", objectName = "CurriculumCustomRequirement", objectId = CurriculumCustomRequirementID, ignoreWarnings = ignoreWarnings, entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' Create a CurriculumCustomRequirement
	#'
	#' This function creates a CurriculumCustomRequirement
	#' @param fieldNames The field values to give the created CurriculumCustomRequirement. Each defaults to NULL.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param query Queries to be added to the request. Ex. list(SchoolYearID = 1). Defaults to NULL.
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Curriculum
	#' @return A newly created CurriculumCustomRequirement
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	createCurriculumCustomRequirement <- function(CurriculumID = NULL, CustomRequirementID = NULL, DistrictGroupKey = NULL, SchoolYearHigh = NULL, SchoolYearLow = NULL, entityId = 1, query = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment())

		body <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper()) %>% purrr::compact()

		createSkyObject(module = "Curriculum", objectName = "CurriculumCustomRequirement", body = list(DataObject = body), searchFields = append("CurriculumCustomRequirementID", body %>% names()), entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' Modify a CurriculumCustomRequirement
	#'
	#' This function modifies a CurriculumCustomRequirement
	#' @param fieldNames The field values to give the modified CurriculumCustomRequirement. Each defaults to NULL.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param query Queries to be added to the request. Ex. list(SchoolYearID = 1). Defaults to NULL.
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Curriculum
	#' @return The modified CurriculumCustomRequirement
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	modifyCurriculumCustomRequirement <- function(CurriculumCustomRequirementID, CurriculumID = NULL, CustomRequirementID = NULL, DistrictGroupKey = NULL, SchoolYearHigh = NULL, SchoolYearLow = NULL, entityId = 1, query = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment())

		body <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper()) %>% purrr::compact()

		modifySkyObject(module = "Curriculum", objectName = "CurriculumCustomRequirement", objectId = CurriculumCustomRequirementID, body = list(DataObject = body), searchFields = append("CurriculumCustomRequirementID", body %>% names()), entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' List Prerequisites
	#'
	#' This function returns a dataframe or json object of Prerequisites
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given Prerequisites. Defaults to FALSE for all return fields which, for convenience, returns all fields for the Prerequisites.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object listed as text. Run \code{\link{getSchemaForObjects}}('Prerequisite') to get more field paths.
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
	#' @concept Curriculum
	#' @return A list of Prerequisites
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	listPrerequisites <- function(searchConditionsList = NULL, Code = F, CodeDescription = F, CreatedTime = F, CurriculumID = F, Description = F, DistrictGroupKey = F, EarnedCredits = F, HasPrerequisiteCurriculums = F, ModifiedTime = F, PrerequisiteID = F, SchoolYearHigh = F, SchoolYearLow = F, UserIDCreatedBy = F, UserIDModifiedBy = F, fieldPaths = NULL, searchConditionsGroupType = "And", searchSortFieldNamesList = NULL, searchSortFieldNamesDescendingList = NULL, entityId = 1, query = NULL, page = 1, pageSize = 100000, flatten = T, returnResponse = F){

		params <- as.list(environment())

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		listSkyObjects(module = "Curriculum", objectName = "Prerequisite", searchFields = searchFields %>% append(fieldPaths), page = page, pageSize = pageSize, SearchConditionsList = searchConditionsList, SearchConditionsGroupType = searchConditionsGroupType, SearchSortFieldNamesList = searchSortFieldNamesList, SearchSortFieldNamesDescendingList = searchSortFieldNamesDescendingList, entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' Get a Prerequisite
	#'
	#' This function returns a dataframe or json object of a Prerequisite
	#' @param PrerequisiteID The ID of the Prerequisite to return.
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given Prerequisite. Defaults to FALSE for all return fields which, for convenience, returns all fields for the Prerequisite.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object listed as text. Run \code{\link{getSchemaForObjects}}('Prerequisite') to get more field paths.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param query Queries to be added to the request. Ex. list(SchoolYearID = 1). Defaults to NULL.
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Curriculum
	#' @return A dataframe or of Prerequisite
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	getPrerequisite <- function(PrerequisiteID, Code = F, CodeDescription = F, CreatedTime = F, CurriculumID = F, Description = F, DistrictGroupKey = F, EarnedCredits = F, HasPrerequisiteCurriculums = F, ModifiedTime = F, SchoolYearHigh = F, SchoolYearLow = F, UserIDCreatedBy = F, UserIDModifiedBy = F, fieldPaths = NULL, entityId = 1, query = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment()) %>% purrr::keep(names(.) != "PrerequisiteID")

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		getSkyObject(module = "Curriculum", objectName = "Prerequisite", objectId = PrerequisiteID, searchFields = searchFields, entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' Delete a Prerequisite
	#'
	#' This function deletes a Prerequisite
	#' @param PrerequisiteID The ID of the Prerequisite to delete
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param query Queries to be added to the request. Ex. list(SchoolYearID = 1). Defaults to NULL.
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Curriculum
	#' @return The PrerequisiteID of the deleted Prerequisite.
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	deletePrerequisite <- function(PrerequisiteID, ignoreWarnings = F, entityId = 1, query = NULL, flatten = T, returnResponse = F){

		deleteSkyObject(module = "Curriculum", objectName = "Prerequisite", objectId = PrerequisiteID, ignoreWarnings = ignoreWarnings, entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' Create a Prerequisite
	#'
	#' This function creates a Prerequisite
	#' @param fieldNames The field values to give the created Prerequisite. Each defaults to NULL.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param query Queries to be added to the request. Ex. list(SchoolYearID = 1). Defaults to NULL.
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Curriculum
	#' @return A newly created Prerequisite
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	createPrerequisite <- function(Code = NULL, CurriculumID = NULL, Description = NULL, DistrictGroupKey = NULL, EarnedCredits = NULL, SchoolYearHigh = NULL, SchoolYearLow = NULL, entityId = 1, query = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment())

		body <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper()) %>% purrr::compact()

		createSkyObject(module = "Curriculum", objectName = "Prerequisite", body = list(DataObject = body), searchFields = append("PrerequisiteID", body %>% names()), entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' Modify a Prerequisite
	#'
	#' This function modifies a Prerequisite
	#' @param fieldNames The field values to give the modified Prerequisite. Each defaults to NULL.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param query Queries to be added to the request. Ex. list(SchoolYearID = 1). Defaults to NULL.
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Curriculum
	#' @return The modified Prerequisite
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	modifyPrerequisite <- function(PrerequisiteID, Code = NULL, CurriculumID = NULL, Description = NULL, DistrictGroupKey = NULL, EarnedCredits = NULL, SchoolYearHigh = NULL, SchoolYearLow = NULL, entityId = 1, query = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment())

		body <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper()) %>% purrr::compact()

		modifySkyObject(module = "Curriculum", objectName = "Prerequisite", objectId = PrerequisiteID, body = list(DataObject = body), searchFields = append("PrerequisiteID", body %>% names()), entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' List Curricula
	#'
	#' This function returns a dataframe or json object of Curricula
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given Curricula. Defaults to FALSE for all return fields which, for convenience, returns all fields for the Curricula.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object listed as text. Run \code{\link{getSchemaForObjects}}('Curriculum') to get more field paths.
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
	#' @concept Curriculum
	#' @return A list of Curricula
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	listCurricula <- function(searchConditionsList = NULL, Code = F, CodeDescription = F, CreatedTime = F, CurriculumID = F, CurriculumSubAreaExistsForStudentAndSubArea = F, CurriculumSubAreaExistsForSubAreaWithoutStudent = F, CurriculumSubjectSummary = F, Description = F, DistrictGroupKey = F, DistrictID = F, EarnedCredits = F, GradeLevelSummary = F, GradReqRankGPAIgnoreDuplicateCheck = F, GradReqSubjectTypeID = F, HasPrerequisiteCurriculums = F, HasPrerequisites = F, IsActive = F, IsAllowedToBeSelectedInCareerPlan = F, IsFederalDistanceEducation = F, IsFederalDualEnrollment = F, MaximumCompletionsForCredit = F, ModifiedTime = F, NumberOfActiveCurrentOrFutureCourses = F, NumberOfAttachedSubjects = F, PrerequisiteCurriculumExistsForPrerequisite = F, UserIDCreatedBy = F, UserIDModifiedBy = F, fieldPaths = NULL, searchConditionsGroupType = "And", searchSortFieldNamesList = NULL, searchSortFieldNamesDescendingList = NULL, entityId = 1, query = NULL, page = 1, pageSize = 100000, flatten = T, returnResponse = F){

		params <- as.list(environment())

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		listSkyObjects(module = "Curriculum", objectName = "Curriculum", searchFields = searchFields %>% append(fieldPaths), page = page, pageSize = pageSize, SearchConditionsList = searchConditionsList, SearchConditionsGroupType = searchConditionsGroupType, SearchSortFieldNamesList = searchSortFieldNamesList, SearchSortFieldNamesDescendingList = searchSortFieldNamesDescendingList, entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' Get a Curriculum
	#'
	#' This function returns a dataframe or json object of a Curriculum
	#' @param CurriculumID The ID of the Curriculum to return.
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given Curriculum. Defaults to FALSE for all return fields which, for convenience, returns all fields for the Curriculum.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object listed as text. Run \code{\link{getSchemaForObjects}}('Curriculum') to get more field paths.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param query Queries to be added to the request. Ex. list(SchoolYearID = 1). Defaults to NULL.
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Curriculum
	#' @return A dataframe or of Curriculum
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	getCurriculum <- function(CurriculumID, Code = F, CodeDescription = F, CreatedTime = F, CurriculumSubAreaExistsForStudentAndSubArea = F, CurriculumSubAreaExistsForSubAreaWithoutStudent = F, CurriculumSubjectSummary = F, Description = F, DistrictGroupKey = F, DistrictID = F, EarnedCredits = F, GradeLevelSummary = F, GradReqRankGPAIgnoreDuplicateCheck = F, GradReqSubjectTypeID = F, HasPrerequisiteCurriculums = F, HasPrerequisites = F, IsActive = F, IsAllowedToBeSelectedInCareerPlan = F, IsFederalDistanceEducation = F, IsFederalDualEnrollment = F, MaximumCompletionsForCredit = F, ModifiedTime = F, NumberOfActiveCurrentOrFutureCourses = F, NumberOfAttachedSubjects = F, PrerequisiteCurriculumExistsForPrerequisite = F, UserIDCreatedBy = F, UserIDModifiedBy = F, fieldPaths = NULL, entityId = 1, query = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment()) %>% purrr::keep(names(.) != "CurriculumID")

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		getSkyObject(module = "Curriculum", objectName = "Curriculum", objectId = CurriculumID, searchFields = searchFields, entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' Delete a Curriculum
	#'
	#' This function deletes a Curriculum
	#' @param CurriculumID The ID of the Curriculum to delete
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param query Queries to be added to the request. Ex. list(SchoolYearID = 1). Defaults to NULL.
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Curriculum
	#' @return The CurriculumID of the deleted Curriculum.
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	deleteCurriculum <- function(CurriculumID, ignoreWarnings = F, entityId = 1, query = NULL, flatten = T, returnResponse = F){

		deleteSkyObject(module = "Curriculum", objectName = "Curriculum", objectId = CurriculumID, ignoreWarnings = ignoreWarnings, entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' Create a Curriculum
	#'
	#' This function creates a Curriculum
	#' @param fieldNames The field values to give the created Curriculum. Each defaults to NULL.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param query Queries to be added to the request. Ex. list(SchoolYearID = 1). Defaults to NULL.
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Curriculum
	#' @return A newly created Curriculum
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	createCurriculum <- function(Code = NULL, Description = NULL, DistrictGroupKey = NULL, DistrictID = NULL, EarnedCredits = NULL, GradReqRankGPAIgnoreDuplicateCheck = NULL, GradReqSubjectTypeID = NULL, IsActive = NULL, IsAllowedToBeSelectedInCareerPlan = NULL, MaximumCompletionsForCredit = NULL, entityId = 1, query = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment())

		body <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper()) %>% purrr::compact()

		createSkyObject(module = "Curriculum", objectName = "Curriculum", body = list(DataObject = body), searchFields = append("CurriculumID", body %>% names()), entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' Modify a Curriculum
	#'
	#' This function modifies a Curriculum
	#' @param fieldNames The field values to give the modified Curriculum. Each defaults to NULL.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param query Queries to be added to the request. Ex. list(SchoolYearID = 1). Defaults to NULL.
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Curriculum
	#' @return The modified Curriculum
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	modifyCurriculum <- function(CurriculumID, Code = NULL, Description = NULL, DistrictGroupKey = NULL, DistrictID = NULL, EarnedCredits = NULL, GradReqRankGPAIgnoreDuplicateCheck = NULL, GradReqSubjectTypeID = NULL, IsActive = NULL, IsAllowedToBeSelectedInCareerPlan = NULL, MaximumCompletionsForCredit = NULL, entityId = 1, query = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment())

		body <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper()) %>% purrr::compact()

		modifySkyObject(module = "Curriculum", objectName = "Curriculum", objectId = CurriculumID, body = list(DataObject = body), searchFields = append("CurriculumID", body %>% names()), entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' List CurriculumSubjects
	#'
	#' This function returns a dataframe or json object of CurriculumSubjects
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given CurriculumSubjects. Defaults to FALSE for all return fields which, for convenience, returns all fields for the CurriculumSubjects.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object listed as text. Run \code{\link{getSchemaForObjects}}('CurriculumSubject') to get more field paths.
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
	#' @concept Curriculum
	#' @return A list of CurriculumSubjects
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	listCurriculumSubjects <- function(searchConditionsList = NULL, BackgroundColor = F, Code = F, CodeDescription = F, CreatedTime = F, Description = F, DistrictGroupKey = F, DistrictID = F, EdFiAcademicSubjectDescriptorID = F, IsPrimaryForSelectedCurriculum = F, ModifiedTime = F, NumberOfAttachedCurriculums = F, SchoolYearID = F, SubjectID = F, SubjectIDClonedFrom = F, SubjectIDClonedTo = F, TextColor = F, UserIDCreatedBy = F, UserIDModifiedBy = F, fieldPaths = NULL, searchConditionsGroupType = "And", searchSortFieldNamesList = NULL, searchSortFieldNamesDescendingList = NULL, entityId = 1, query = NULL, page = 1, pageSize = 100000, flatten = T, returnResponse = F){

		params <- as.list(environment())

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		listSkyObjects(module = "Curriculum", objectName = "Subject", searchFields = searchFields %>% append(fieldPaths), page = page, pageSize = pageSize, SearchConditionsList = searchConditionsList, SearchConditionsGroupType = searchConditionsGroupType, SearchSortFieldNamesList = searchSortFieldNamesList, SearchSortFieldNamesDescendingList = searchSortFieldNamesDescendingList, entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' Get a CurriculumSubject
	#'
	#' This function returns a dataframe or json object of a CurriculumSubject
	#' @param CurriculumSubjectID The ID of the CurriculumSubject to return.
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given CurriculumSubject. Defaults to FALSE for all return fields which, for convenience, returns all fields for the CurriculumSubject.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object listed as text. Run \code{\link{getSchemaForObjects}}('CurriculumSubject') to get more field paths.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param query Queries to be added to the request. Ex. list(SchoolYearID = 1). Defaults to NULL.
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Curriculum
	#' @return A dataframe or of CurriculumSubject
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	getCurriculumSubject <- function(CurriculumSubjectID, BackgroundColor = F, Code = F, CodeDescription = F, CreatedTime = F, Description = F, DistrictGroupKey = F, DistrictID = F, EdFiAcademicSubjectDescriptorID = F, IsPrimaryForSelectedCurriculum = F, ModifiedTime = F, NumberOfAttachedCurriculums = F, SchoolYearID = F, SubjectID = F, SubjectIDClonedFrom = F, SubjectIDClonedTo = F, TextColor = F, UserIDCreatedBy = F, UserIDModifiedBy = F, fieldPaths = NULL, entityId = 1, query = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment()) %>% purrr::keep(names(.) != "CurriculumSubjectID")

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		getSkyObject(module = "Curriculum", objectName = "Subject", objectId = CurriculumSubjectID, searchFields = searchFields, entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' Delete a CurriculumSubject
	#'
	#' This function deletes a CurriculumSubject
	#' @param CurriculumSubjectID The ID of the CurriculumSubject to delete
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param query Queries to be added to the request. Ex. list(SchoolYearID = 1). Defaults to NULL.
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Curriculum
	#' @return The CurriculumSubjectID of the deleted CurriculumSubject.
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	deleteCurriculumSubject <- function(CurriculumSubjectID, ignoreWarnings = F, entityId = 1, query = NULL, flatten = T, returnResponse = F){

		deleteSkyObject(module = "Curriculum", objectName = "Subject", objectId = CurriculumSubjectID, ignoreWarnings = ignoreWarnings, entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' Create a CurriculumSubject
	#'
	#' This function creates a CurriculumSubject
	#' @param fieldNames The field values to give the created CurriculumSubject. Each defaults to NULL.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param query Queries to be added to the request. Ex. list(SchoolYearID = 1). Defaults to NULL.
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Curriculum
	#' @return A newly created CurriculumSubject
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	createCurriculumSubject <- function(BackgroundColor = NULL, Code = NULL, Description = NULL, DistrictGroupKey = NULL, DistrictID = NULL, EdFiAcademicSubjectDescriptorID = NULL, SchoolYearID = NULL, SubjectIDClonedFrom = NULL, TextColor = NULL, entityId = 1, query = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment())

		body <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper()) %>% purrr::compact()

		createSkyObject(module = "Curriculum", objectName = "Subject", body = list(DataObject = body), searchFields = append("SubjectID", body %>% names()), entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' Modify a CurriculumSubject
	#'
	#' This function modifies a CurriculumSubject
	#' @param fieldNames The field values to give the modified CurriculumSubject. Each defaults to NULL.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param query Queries to be added to the request. Ex. list(SchoolYearID = 1). Defaults to NULL.
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Curriculum
	#' @return The modified CurriculumSubject
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	modifyCurriculumSubject <- function(SubjectID, BackgroundColor = NULL, Code = NULL, Description = NULL, DistrictGroupKey = NULL, DistrictID = NULL, EdFiAcademicSubjectDescriptorID = NULL, SchoolYearID = NULL, SubjectIDClonedFrom = NULL, TextColor = NULL, entityId = 1, query = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment())

		body <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper()) %>% purrr::compact()

		modifySkyObject(module = "Curriculum", objectName = "Subject", objectId = SubjectID, body = list(DataObject = body), searchFields = append("SubjectID", body %>% names()), entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' List AssessmentToolMNS
	#'
	#' This function returns a dataframe or json object of AssessmentToolMNS
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given AssessmentToolMNS. Defaults to FALSE for all return fields which, for convenience, returns all fields for the AssessmentToolMNS.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object listed as text. Run \code{\link{getSchemaForObjects}}('AssessmentToolMN') to get more field paths.
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
	#' @concept Curriculum
	#' @return A list of AssessmentToolMNS
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	listAssessmentToolMNS <- function(searchConditionsList = NULL, AssessmentToolMNID = F, AssessmentToolMNIDClonedFrom = F, CreatedTime = F, CurriculumYearID = F, ModifiedTime = F, StateAssessmentToolMNID = F, StateImplementationStatusMNID = F, UserIDCreatedBy = F, UserIDModifiedBy = F, fieldPaths = NULL, searchConditionsGroupType = "And", searchSortFieldNamesList = NULL, searchSortFieldNamesDescendingList = NULL, entityId = 1, query = NULL, page = 1, pageSize = 100000, flatten = T, returnResponse = F){

		params <- as.list(environment())

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		listSkyObjects(module = "Curriculum", objectName = "AssessmentToolMN", searchFields = searchFields %>% append(fieldPaths), page = page, pageSize = pageSize, SearchConditionsList = searchConditionsList, SearchConditionsGroupType = searchConditionsGroupType, SearchSortFieldNamesList = searchSortFieldNamesList, SearchSortFieldNamesDescendingList = searchSortFieldNamesDescendingList, entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' Get an AssessmentToolMN
	#'
	#' This function returns a dataframe or json object of an AssessmentToolMN
	#' @param AssessmentToolMNID The ID of the AssessmentToolMN to return.
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given AssessmentToolMN. Defaults to FALSE for all return fields which, for convenience, returns all fields for the AssessmentToolMN.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object listed as text. Run \code{\link{getSchemaForObjects}}('AssessmentToolMN') to get more field paths.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param query Queries to be added to the request. Ex. list(SchoolYearID = 1). Defaults to NULL.
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Curriculum
	#' @return A dataframe or of AssessmentToolMN
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	getAssessmentToolMN <- function(AssessmentToolMNID, AssessmentToolMNIDClonedFrom = F, CreatedTime = F, CurriculumYearID = F, ModifiedTime = F, StateAssessmentToolMNID = F, StateImplementationStatusMNID = F, UserIDCreatedBy = F, UserIDModifiedBy = F, fieldPaths = NULL, entityId = 1, query = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment()) %>% purrr::keep(names(.) != "AssessmentToolMNID")

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		getSkyObject(module = "Curriculum", objectName = "AssessmentToolMN", objectId = AssessmentToolMNID, searchFields = searchFields, entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' Delete an AssessmentToolMN
	#'
	#' This function deletes an AssessmentToolMN
	#' @param AssessmentToolMNID The ID of the AssessmentToolMN to delete
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param query Queries to be added to the request. Ex. list(SchoolYearID = 1). Defaults to NULL.
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Curriculum
	#' @return The AssessmentToolMNID of the deleted AssessmentToolMN.
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	deleteAssessmentToolMN <- function(AssessmentToolMNID, ignoreWarnings = F, entityId = 1, query = NULL, flatten = T, returnResponse = F){

		deleteSkyObject(module = "Curriculum", objectName = "AssessmentToolMN", objectId = AssessmentToolMNID, ignoreWarnings = ignoreWarnings, entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' Create an AssessmentToolMN
	#'
	#' This function creates an AssessmentToolMN
	#' @param fieldNames The field values to give the created AssessmentToolMN. Each defaults to NULL.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param query Queries to be added to the request. Ex. list(SchoolYearID = 1). Defaults to NULL.
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Curriculum
	#' @return A newly created AssessmentToolMN
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	createAssessmentToolMN <- function(AssessmentToolMNIDClonedFrom = NULL, CurriculumYearID = NULL, StateAssessmentToolMNID = NULL, StateImplementationStatusMNID = NULL, entityId = 1, query = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment())

		body <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper()) %>% purrr::compact()

		createSkyObject(module = "Curriculum", objectName = "AssessmentToolMN", body = list(DataObject = body), searchFields = append("AssessmentToolMNID", body %>% names()), entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' Modify an AssessmentToolMN
	#'
	#' This function modifies an AssessmentToolMN
	#' @param fieldNames The field values to give the modified AssessmentToolMN. Each defaults to NULL.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param query Queries to be added to the request. Ex. list(SchoolYearID = 1). Defaults to NULL.
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Curriculum
	#' @return The modified AssessmentToolMN
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	modifyAssessmentToolMN <- function(AssessmentToolMNID, AssessmentToolMNIDClonedFrom = NULL, CurriculumYearID = NULL, StateAssessmentToolMNID = NULL, StateImplementationStatusMNID = NULL, entityId = 1, query = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment())

		body <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper()) %>% purrr::compact()

		modifySkyObject(module = "Curriculum", objectName = "AssessmentToolMN", objectId = AssessmentToolMNID, body = list(DataObject = body), searchFields = append("AssessmentToolMNID", body %>% names()), entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' List EarlyEducationInstructionalApproachMNS
	#'
	#' This function returns a dataframe or json object of EarlyEducationInstructionalApproachMNS
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given EarlyEducationInstructionalApproachMNS. Defaults to FALSE for all return fields which, for convenience, returns all fields for the EarlyEducationInstructionalApproachMNS.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object listed as text. Run \code{\link{getSchemaForObjects}}('EarlyEducationInstructionalApproachMN') to get more field paths.
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
	#' @concept Curriculum
	#' @return A list of EarlyEducationInstructionalApproachMNS
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	listEarlyEducationInstructionalApproachMNS <- function(searchConditionsList = NULL, CreatedTime = F, CurriculumYearID = F, EarlyEducationInstructionalApproachMNID = F, EarlyEducationInstructionalApproachMNIDClonedFrom = F, ModifiedTime = F, StateEarlyEducationInstructionalApproachMNID = F, StateImplementationStatusMNID = F, UserIDCreatedBy = F, UserIDModifiedBy = F, fieldPaths = NULL, searchConditionsGroupType = "And", searchSortFieldNamesList = NULL, searchSortFieldNamesDescendingList = NULL, entityId = 1, query = NULL, page = 1, pageSize = 100000, flatten = T, returnResponse = F){

		params <- as.list(environment())

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		listSkyObjects(module = "Curriculum", objectName = "EarlyEducationInstructionalApproachMN", searchFields = searchFields %>% append(fieldPaths), page = page, pageSize = pageSize, SearchConditionsList = searchConditionsList, SearchConditionsGroupType = searchConditionsGroupType, SearchSortFieldNamesList = searchSortFieldNamesList, SearchSortFieldNamesDescendingList = searchSortFieldNamesDescendingList, entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' Get an EarlyEducationInstructionalApproachMN
	#'
	#' This function returns a dataframe or json object of an EarlyEducationInstructionalApproachMN
	#' @param EarlyEducationInstructionalApproachMNID The ID of the EarlyEducationInstructionalApproachMN to return.
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given EarlyEducationInstructionalApproachMN. Defaults to FALSE for all return fields which, for convenience, returns all fields for the EarlyEducationInstructionalApproachMN.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object listed as text. Run \code{\link{getSchemaForObjects}}('EarlyEducationInstructionalApproachMN') to get more field paths.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param query Queries to be added to the request. Ex. list(SchoolYearID = 1). Defaults to NULL.
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Curriculum
	#' @return A dataframe or of EarlyEducationInstructionalApproachMN
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	getEarlyEducationInstructionalApproachMN <- function(EarlyEducationInstructionalApproachMNID, CreatedTime = F, CurriculumYearID = F, EarlyEducationInstructionalApproachMNIDClonedFrom = F, ModifiedTime = F, StateEarlyEducationInstructionalApproachMNID = F, StateImplementationStatusMNID = F, UserIDCreatedBy = F, UserIDModifiedBy = F, fieldPaths = NULL, entityId = 1, query = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment()) %>% purrr::keep(names(.) != "EarlyEducationInstructionalApproachMNID")

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		getSkyObject(module = "Curriculum", objectName = "EarlyEducationInstructionalApproachMN", objectId = EarlyEducationInstructionalApproachMNID, searchFields = searchFields, entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' Delete an EarlyEducationInstructionalApproachMN
	#'
	#' This function deletes an EarlyEducationInstructionalApproachMN
	#' @param EarlyEducationInstructionalApproachMNID The ID of the EarlyEducationInstructionalApproachMN to delete
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param query Queries to be added to the request. Ex. list(SchoolYearID = 1). Defaults to NULL.
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Curriculum
	#' @return The EarlyEducationInstructionalApproachMNID of the deleted EarlyEducationInstructionalApproachMN.
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	deleteEarlyEducationInstructionalApproachMN <- function(EarlyEducationInstructionalApproachMNID, ignoreWarnings = F, entityId = 1, query = NULL, flatten = T, returnResponse = F){

		deleteSkyObject(module = "Curriculum", objectName = "EarlyEducationInstructionalApproachMN", objectId = EarlyEducationInstructionalApproachMNID, ignoreWarnings = ignoreWarnings, entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' Create an EarlyEducationInstructionalApproachMN
	#'
	#' This function creates an EarlyEducationInstructionalApproachMN
	#' @param fieldNames The field values to give the created EarlyEducationInstructionalApproachMN. Each defaults to NULL.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param query Queries to be added to the request. Ex. list(SchoolYearID = 1). Defaults to NULL.
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Curriculum
	#' @return A newly created EarlyEducationInstructionalApproachMN
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	createEarlyEducationInstructionalApproachMN <- function(CurriculumYearID = NULL, EarlyEducationInstructionalApproachMNIDClonedFrom = NULL, StateEarlyEducationInstructionalApproachMNID = NULL, StateImplementationStatusMNID = NULL, entityId = 1, query = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment())

		body <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper()) %>% purrr::compact()

		createSkyObject(module = "Curriculum", objectName = "EarlyEducationInstructionalApproachMN", body = list(DataObject = body), searchFields = append("EarlyEducationInstructionalApproachMNID", body %>% names()), entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' Modify an EarlyEducationInstructionalApproachMN
	#'
	#' This function modifies an EarlyEducationInstructionalApproachMN
	#' @param fieldNames The field values to give the modified EarlyEducationInstructionalApproachMN. Each defaults to NULL.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param query Queries to be added to the request. Ex. list(SchoolYearID = 1). Defaults to NULL.
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Curriculum
	#' @return The modified EarlyEducationInstructionalApproachMN
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	modifyEarlyEducationInstructionalApproachMN <- function(EarlyEducationInstructionalApproachMNID, CurriculumYearID = NULL, EarlyEducationInstructionalApproachMNIDClonedFrom = NULL, StateEarlyEducationInstructionalApproachMNID = NULL, StateImplementationStatusMNID = NULL, entityId = 1, query = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment())

		body <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper()) %>% purrr::compact()

		modifySkyObject(module = "Curriculum", objectName = "EarlyEducationInstructionalApproachMN", objectId = EarlyEducationInstructionalApproachMNID, body = list(DataObject = body), searchFields = append("EarlyEducationInstructionalApproachMNID", body %>% names()), entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' List SiteBasedInitiativeMNS
	#'
	#' This function returns a dataframe or json object of SiteBasedInitiativeMNS
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given SiteBasedInitiativeMNS. Defaults to FALSE for all return fields which, for convenience, returns all fields for the SiteBasedInitiativeMNS.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object listed as text. Run \code{\link{getSchemaForObjects}}('SiteBasedInitiativeMN') to get more field paths.
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
	#' @concept Curriculum
	#' @return A list of SiteBasedInitiativeMNS
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	listSiteBasedInitiativeMNS <- function(searchConditionsList = NULL, CreatedTime = F, CurriculumYearID = F, ModifiedTime = F, SiteBasedInitiativeMNID = F, SiteBasedInitiativeMNIDClonedFrom = F, StateImplementationStatusMNID = F, StateSiteBasedInitiativeMNID = F, UserIDCreatedBy = F, UserIDModifiedBy = F, fieldPaths = NULL, searchConditionsGroupType = "And", searchSortFieldNamesList = NULL, searchSortFieldNamesDescendingList = NULL, entityId = 1, query = NULL, page = 1, pageSize = 100000, flatten = T, returnResponse = F){

		params <- as.list(environment())

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		listSkyObjects(module = "Curriculum", objectName = "SiteBasedInitiativeMN", searchFields = searchFields %>% append(fieldPaths), page = page, pageSize = pageSize, SearchConditionsList = searchConditionsList, SearchConditionsGroupType = searchConditionsGroupType, SearchSortFieldNamesList = searchSortFieldNamesList, SearchSortFieldNamesDescendingList = searchSortFieldNamesDescendingList, entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' Get a SiteBasedInitiativeMN
	#'
	#' This function returns a dataframe or json object of a SiteBasedInitiativeMN
	#' @param SiteBasedInitiativeMNID The ID of the SiteBasedInitiativeMN to return.
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given SiteBasedInitiativeMN. Defaults to FALSE for all return fields which, for convenience, returns all fields for the SiteBasedInitiativeMN.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object listed as text. Run \code{\link{getSchemaForObjects}}('SiteBasedInitiativeMN') to get more field paths.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param query Queries to be added to the request. Ex. list(SchoolYearID = 1). Defaults to NULL.
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Curriculum
	#' @return A dataframe or of SiteBasedInitiativeMN
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	getSiteBasedInitiativeMN <- function(SiteBasedInitiativeMNID, CreatedTime = F, CurriculumYearID = F, ModifiedTime = F, SiteBasedInitiativeMNIDClonedFrom = F, StateImplementationStatusMNID = F, StateSiteBasedInitiativeMNID = F, UserIDCreatedBy = F, UserIDModifiedBy = F, fieldPaths = NULL, entityId = 1, query = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment()) %>% purrr::keep(names(.) != "SiteBasedInitiativeMNID")

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		getSkyObject(module = "Curriculum", objectName = "SiteBasedInitiativeMN", objectId = SiteBasedInitiativeMNID, searchFields = searchFields, entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' Delete a SiteBasedInitiativeMN
	#'
	#' This function deletes a SiteBasedInitiativeMN
	#' @param SiteBasedInitiativeMNID The ID of the SiteBasedInitiativeMN to delete
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param query Queries to be added to the request. Ex. list(SchoolYearID = 1). Defaults to NULL.
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Curriculum
	#' @return The SiteBasedInitiativeMNID of the deleted SiteBasedInitiativeMN.
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	deleteSiteBasedInitiativeMN <- function(SiteBasedInitiativeMNID, ignoreWarnings = F, entityId = 1, query = NULL, flatten = T, returnResponse = F){

		deleteSkyObject(module = "Curriculum", objectName = "SiteBasedInitiativeMN", objectId = SiteBasedInitiativeMNID, ignoreWarnings = ignoreWarnings, entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' Create a SiteBasedInitiativeMN
	#'
	#' This function creates a SiteBasedInitiativeMN
	#' @param fieldNames The field values to give the created SiteBasedInitiativeMN. Each defaults to NULL.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param query Queries to be added to the request. Ex. list(SchoolYearID = 1). Defaults to NULL.
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Curriculum
	#' @return A newly created SiteBasedInitiativeMN
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	createSiteBasedInitiativeMN <- function(CurriculumYearID = NULL, SiteBasedInitiativeMNIDClonedFrom = NULL, StateImplementationStatusMNID = NULL, StateSiteBasedInitiativeMNID = NULL, entityId = 1, query = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment())

		body <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper()) %>% purrr::compact()

		createSkyObject(module = "Curriculum", objectName = "SiteBasedInitiativeMN", body = list(DataObject = body), searchFields = append("SiteBasedInitiativeMNID", body %>% names()), entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' Modify a SiteBasedInitiativeMN
	#'
	#' This function modifies a SiteBasedInitiativeMN
	#' @param fieldNames The field values to give the modified SiteBasedInitiativeMN. Each defaults to NULL.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param query Queries to be added to the request. Ex. list(SchoolYearID = 1). Defaults to NULL.
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Curriculum
	#' @return The modified SiteBasedInitiativeMN
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	modifySiteBasedInitiativeMN <- function(SiteBasedInitiativeMNID, CurriculumYearID = NULL, SiteBasedInitiativeMNIDClonedFrom = NULL, StateImplementationStatusMNID = NULL, StateSiteBasedInitiativeMNID = NULL, entityId = 1, query = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment())

		body <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper()) %>% purrr::compact()

		modifySkyObject(module = "Curriculum", objectName = "SiteBasedInitiativeMN", objectId = SiteBasedInitiativeMNID, body = list(DataObject = body), searchFields = append("SiteBasedInitiativeMNID", body %>% names()), entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' List CurriculumProgramMNS
	#'
	#' This function returns a dataframe or json object of CurriculumProgramMNS
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given CurriculumProgramMNS. Defaults to FALSE for all return fields which, for convenience, returns all fields for the CurriculumProgramMNS.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object listed as text. Run \code{\link{getSchemaForObjects}}('CurriculumProgramMN') to get more field paths.
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
	#' @concept Curriculum
	#' @return A list of CurriculumProgramMNS
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	listCurriculumProgramMNS <- function(searchConditionsList = NULL, CreatedTime = F, CurriculumProgramMNID = F, CurriculumProgramMNIDClonedFrom = F, CurriculumYearID = F, ModifiedTime = F, StateCurriculumProgramMNID = F, StateImplementationStatusMNID = F, UserIDCreatedBy = F, UserIDModifiedBy = F, fieldPaths = NULL, searchConditionsGroupType = "And", searchSortFieldNamesList = NULL, searchSortFieldNamesDescendingList = NULL, entityId = 1, query = NULL, page = 1, pageSize = 100000, flatten = T, returnResponse = F){

		params <- as.list(environment())

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		listSkyObjects(module = "Curriculum", objectName = "CurriculumProgramMN", searchFields = searchFields %>% append(fieldPaths), page = page, pageSize = pageSize, SearchConditionsList = searchConditionsList, SearchConditionsGroupType = searchConditionsGroupType, SearchSortFieldNamesList = searchSortFieldNamesList, SearchSortFieldNamesDescendingList = searchSortFieldNamesDescendingList, entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' Get a CurriculumProgramMN
	#'
	#' This function returns a dataframe or json object of a CurriculumProgramMN
	#' @param CurriculumProgramMNID The ID of the CurriculumProgramMN to return.
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given CurriculumProgramMN. Defaults to FALSE for all return fields which, for convenience, returns all fields for the CurriculumProgramMN.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object listed as text. Run \code{\link{getSchemaForObjects}}('CurriculumProgramMN') to get more field paths.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param query Queries to be added to the request. Ex. list(SchoolYearID = 1). Defaults to NULL.
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Curriculum
	#' @return A dataframe or of CurriculumProgramMN
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	getCurriculumProgramMN <- function(CurriculumProgramMNID, CreatedTime = F, CurriculumProgramMNIDClonedFrom = F, CurriculumYearID = F, ModifiedTime = F, StateCurriculumProgramMNID = F, StateImplementationStatusMNID = F, UserIDCreatedBy = F, UserIDModifiedBy = F, fieldPaths = NULL, entityId = 1, query = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment()) %>% purrr::keep(names(.) != "CurriculumProgramMNID")

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		getSkyObject(module = "Curriculum", objectName = "CurriculumProgramMN", objectId = CurriculumProgramMNID, searchFields = searchFields, entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' Delete a CurriculumProgramMN
	#'
	#' This function deletes a CurriculumProgramMN
	#' @param CurriculumProgramMNID The ID of the CurriculumProgramMN to delete
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param query Queries to be added to the request. Ex. list(SchoolYearID = 1). Defaults to NULL.
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Curriculum
	#' @return The CurriculumProgramMNID of the deleted CurriculumProgramMN.
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	deleteCurriculumProgramMN <- function(CurriculumProgramMNID, ignoreWarnings = F, entityId = 1, query = NULL, flatten = T, returnResponse = F){

		deleteSkyObject(module = "Curriculum", objectName = "CurriculumProgramMN", objectId = CurriculumProgramMNID, ignoreWarnings = ignoreWarnings, entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' Create a CurriculumProgramMN
	#'
	#' This function creates a CurriculumProgramMN
	#' @param fieldNames The field values to give the created CurriculumProgramMN. Each defaults to NULL.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param query Queries to be added to the request. Ex. list(SchoolYearID = 1). Defaults to NULL.
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Curriculum
	#' @return A newly created CurriculumProgramMN
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	createCurriculumProgramMN <- function(CurriculumProgramMNIDClonedFrom = NULL, CurriculumYearID = NULL, StateCurriculumProgramMNID = NULL, StateImplementationStatusMNID = NULL, entityId = 1, query = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment())

		body <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper()) %>% purrr::compact()

		createSkyObject(module = "Curriculum", objectName = "CurriculumProgramMN", body = list(DataObject = body), searchFields = append("CurriculumProgramMNID", body %>% names()), entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' Modify a CurriculumProgramMN
	#'
	#' This function modifies a CurriculumProgramMN
	#' @param fieldNames The field values to give the modified CurriculumProgramMN. Each defaults to NULL.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param query Queries to be added to the request. Ex. list(SchoolYearID = 1). Defaults to NULL.
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Curriculum
	#' @return The modified CurriculumProgramMN
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	modifyCurriculumProgramMN <- function(CurriculumProgramMNID, CurriculumProgramMNIDClonedFrom = NULL, CurriculumYearID = NULL, StateCurriculumProgramMNID = NULL, StateImplementationStatusMNID = NULL, entityId = 1, query = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment())

		body <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper()) %>% purrr::compact()

		modifySkyObject(module = "Curriculum", objectName = "CurriculumProgramMN", objectId = CurriculumProgramMNID, body = list(DataObject = body), searchFields = append("CurriculumProgramMNID", body %>% names()), entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' List EarlyEducationProgramMNS
	#'
	#' This function returns a dataframe or json object of EarlyEducationProgramMNS
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given EarlyEducationProgramMNS. Defaults to FALSE for all return fields which, for convenience, returns all fields for the EarlyEducationProgramMNS.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object listed as text. Run \code{\link{getSchemaForObjects}}('EarlyEducationProgramMN') to get more field paths.
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
	#' @concept Curriculum
	#' @return A list of EarlyEducationProgramMNS
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	listEarlyEducationProgramMNS <- function(searchConditionsList = NULL, CreatedTime = F, CurriculumYearID = F, EarlyEducationProgramMNID = F, EarlyEducationProgramMNIDClonedFrom = F, ModifiedTime = F, StateEarlyEducationProgramMNID = F, UserIDCreatedBy = F, UserIDModifiedBy = F, fieldPaths = NULL, searchConditionsGroupType = "And", searchSortFieldNamesList = NULL, searchSortFieldNamesDescendingList = NULL, entityId = 1, query = NULL, page = 1, pageSize = 100000, flatten = T, returnResponse = F){

		params <- as.list(environment())

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		listSkyObjects(module = "Curriculum", objectName = "EarlyEducationProgramMN", searchFields = searchFields %>% append(fieldPaths), page = page, pageSize = pageSize, SearchConditionsList = searchConditionsList, SearchConditionsGroupType = searchConditionsGroupType, SearchSortFieldNamesList = searchSortFieldNamesList, SearchSortFieldNamesDescendingList = searchSortFieldNamesDescendingList, entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' Get an EarlyEducationProgramMN
	#'
	#' This function returns a dataframe or json object of an EarlyEducationProgramMN
	#' @param EarlyEducationProgramMNID The ID of the EarlyEducationProgramMN to return.
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given EarlyEducationProgramMN. Defaults to FALSE for all return fields which, for convenience, returns all fields for the EarlyEducationProgramMN.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object listed as text. Run \code{\link{getSchemaForObjects}}('EarlyEducationProgramMN') to get more field paths.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param query Queries to be added to the request. Ex. list(SchoolYearID = 1). Defaults to NULL.
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Curriculum
	#' @return A dataframe or of EarlyEducationProgramMN
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	getEarlyEducationProgramMN <- function(EarlyEducationProgramMNID, CreatedTime = F, CurriculumYearID = F, EarlyEducationProgramMNIDClonedFrom = F, ModifiedTime = F, StateEarlyEducationProgramMNID = F, UserIDCreatedBy = F, UserIDModifiedBy = F, fieldPaths = NULL, entityId = 1, query = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment()) %>% purrr::keep(names(.) != "EarlyEducationProgramMNID")

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		getSkyObject(module = "Curriculum", objectName = "EarlyEducationProgramMN", objectId = EarlyEducationProgramMNID, searchFields = searchFields, entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' Delete an EarlyEducationProgramMN
	#'
	#' This function deletes an EarlyEducationProgramMN
	#' @param EarlyEducationProgramMNID The ID of the EarlyEducationProgramMN to delete
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param query Queries to be added to the request. Ex. list(SchoolYearID = 1). Defaults to NULL.
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Curriculum
	#' @return The EarlyEducationProgramMNID of the deleted EarlyEducationProgramMN.
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	deleteEarlyEducationProgramMN <- function(EarlyEducationProgramMNID, ignoreWarnings = F, entityId = 1, query = NULL, flatten = T, returnResponse = F){

		deleteSkyObject(module = "Curriculum", objectName = "EarlyEducationProgramMN", objectId = EarlyEducationProgramMNID, ignoreWarnings = ignoreWarnings, entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' Create an EarlyEducationProgramMN
	#'
	#' This function creates an EarlyEducationProgramMN
	#' @param fieldNames The field values to give the created EarlyEducationProgramMN. Each defaults to NULL.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param query Queries to be added to the request. Ex. list(SchoolYearID = 1). Defaults to NULL.
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Curriculum
	#' @return A newly created EarlyEducationProgramMN
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	createEarlyEducationProgramMN <- function(CurriculumYearID = NULL, EarlyEducationProgramMNIDClonedFrom = NULL, StateEarlyEducationProgramMNID = NULL, entityId = 1, query = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment())

		body <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper()) %>% purrr::compact()

		createSkyObject(module = "Curriculum", objectName = "EarlyEducationProgramMN", body = list(DataObject = body), searchFields = append("EarlyEducationProgramMNID", body %>% names()), entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' Modify an EarlyEducationProgramMN
	#'
	#' This function modifies an EarlyEducationProgramMN
	#' @param fieldNames The field values to give the modified EarlyEducationProgramMN. Each defaults to NULL.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param query Queries to be added to the request. Ex. list(SchoolYearID = 1). Defaults to NULL.
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Curriculum
	#' @return The modified EarlyEducationProgramMN
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	modifyEarlyEducationProgramMN <- function(EarlyEducationProgramMNID, CurriculumYearID = NULL, EarlyEducationProgramMNIDClonedFrom = NULL, StateEarlyEducationProgramMNID = NULL, entityId = 1, query = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment())

		body <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper()) %>% purrr::compact()

		modifySkyObject(module = "Curriculum", objectName = "EarlyEducationProgramMN", objectId = EarlyEducationProgramMNID, body = list(DataObject = body), searchFields = append("EarlyEducationProgramMNID", body %>% names()), entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' List AcademicStandardDefaults
	#'
	#' This function returns a dataframe or json object of AcademicStandardDefaults
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given AcademicStandardDefaults. Defaults to FALSE for all return fields which, for convenience, returns all fields for the AcademicStandardDefaults.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object listed as text. Run \code{\link{getSchemaForObjects}}('AcademicStandardDefault') to get more field paths.
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
	#' @concept Curriculum
	#' @return A list of AcademicStandardDefaults
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	listAcademicStandardDefaults <- function(searchConditionsList = NULL, AcademicStandardDefaultID = F, AcademicStandardDefaultIDParent = F, AcademicStandardGradeRangeDefaultID = F, CreatedTime = F, Description = F, IsHighFrequencyWord = F, Key = F, Language = F, LetterAndSoundType = F, LetterType = F, Level = F, ModifiedTime = F, ParentGuid = F, SkywardHash = F, SkywardID = F, UserIDCreatedBy = F, UserIDModifiedBy = F, fieldPaths = NULL, searchConditionsGroupType = "And", searchSortFieldNamesList = NULL, searchSortFieldNamesDescendingList = NULL, entityId = 1, query = NULL, page = 1, pageSize = 100000, flatten = T, returnResponse = F){

		params <- as.list(environment())

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		listSkyObjects(module = "Curriculum", objectName = "AcademicStandardDefault", searchFields = searchFields %>% append(fieldPaths), page = page, pageSize = pageSize, SearchConditionsList = searchConditionsList, SearchConditionsGroupType = searchConditionsGroupType, SearchSortFieldNamesList = searchSortFieldNamesList, SearchSortFieldNamesDescendingList = searchSortFieldNamesDescendingList, entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' Get an AcademicStandardDefault
	#'
	#' This function returns a dataframe or json object of an AcademicStandardDefault
	#' @param AcademicStandardDefaultID The ID of the AcademicStandardDefault to return.
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given AcademicStandardDefault. Defaults to FALSE for all return fields which, for convenience, returns all fields for the AcademicStandardDefault.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object listed as text. Run \code{\link{getSchemaForObjects}}('AcademicStandardDefault') to get more field paths.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param query Queries to be added to the request. Ex. list(SchoolYearID = 1). Defaults to NULL.
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Curriculum
	#' @return A dataframe or of AcademicStandardDefault
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	getAcademicStandardDefault <- function(AcademicStandardDefaultID, AcademicStandardDefaultIDParent = F, AcademicStandardGradeRangeDefaultID = F, CreatedTime = F, Description = F, IsHighFrequencyWord = F, Key = F, Language = F, LetterAndSoundType = F, LetterType = F, Level = F, ModifiedTime = F, ParentGuid = F, SkywardHash = F, SkywardID = F, UserIDCreatedBy = F, UserIDModifiedBy = F, fieldPaths = NULL, entityId = 1, query = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment()) %>% purrr::keep(names(.) != "AcademicStandardDefaultID")

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		getSkyObject(module = "Curriculum", objectName = "AcademicStandardDefault", objectId = AcademicStandardDefaultID, searchFields = searchFields, entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' Delete an AcademicStandardDefault
	#'
	#' This function deletes an AcademicStandardDefault
	#' @param AcademicStandardDefaultID The ID of the AcademicStandardDefault to delete
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param query Queries to be added to the request. Ex. list(SchoolYearID = 1). Defaults to NULL.
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Curriculum
	#' @return The AcademicStandardDefaultID of the deleted AcademicStandardDefault.
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	deleteAcademicStandardDefault <- function(AcademicStandardDefaultID, ignoreWarnings = F, entityId = 1, query = NULL, flatten = T, returnResponse = F){

		deleteSkyObject(module = "Curriculum", objectName = "AcademicStandardDefault", objectId = AcademicStandardDefaultID, ignoreWarnings = ignoreWarnings, entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' Create an AcademicStandardDefault
	#'
	#' This function creates an AcademicStandardDefault
	#' @param fieldNames The field values to give the created AcademicStandardDefault. Each defaults to NULL.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param query Queries to be added to the request. Ex. list(SchoolYearID = 1). Defaults to NULL.
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Curriculum
	#' @return A newly created AcademicStandardDefault
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	createAcademicStandardDefault <- function(AcademicStandardDefaultIDParent = NULL, AcademicStandardGradeRangeDefaultID = NULL, Description = NULL, IsHighFrequencyWord = NULL, Key = NULL, Language = NULL, LetterAndSoundType = NULL, LetterType = NULL, Level = NULL, ParentGuid = NULL, entityId = 1, query = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment())

		body <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper()) %>% purrr::compact()

		createSkyObject(module = "Curriculum", objectName = "AcademicStandardDefault", body = list(DataObject = body), searchFields = append("AcademicStandardDefaultID", body %>% names()), entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' Modify an AcademicStandardDefault
	#'
	#' This function modifies an AcademicStandardDefault
	#' @param fieldNames The field values to give the modified AcademicStandardDefault. Each defaults to NULL.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param query Queries to be added to the request. Ex. list(SchoolYearID = 1). Defaults to NULL.
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Curriculum
	#' @return The modified AcademicStandardDefault
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	modifyAcademicStandardDefault <- function(AcademicStandardDefaultID, AcademicStandardDefaultIDParent = NULL, AcademicStandardGradeRangeDefaultID = NULL, Description = NULL, IsHighFrequencyWord = NULL, Key = NULL, Language = NULL, LetterAndSoundType = NULL, LetterType = NULL, Level = NULL, ParentGuid = NULL, entityId = 1, query = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment())

		body <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper()) %>% purrr::compact()

		modifySkyObject(module = "Curriculum", objectName = "AcademicStandardDefault", objectId = AcademicStandardDefaultID, body = list(DataObject = body), searchFields = append("AcademicStandardDefaultID", body %>% names()), entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' List AcademicStandardGradeRangeDefaults
	#'
	#' This function returns a dataframe or json object of AcademicStandardGradeRangeDefaults
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given AcademicStandardGradeRangeDefaults. Defaults to FALSE for all return fields which, for convenience, returns all fields for the AcademicStandardGradeRangeDefaults.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object listed as text. Run \code{\link{getSchemaForObjects}}('AcademicStandardGradeRangeDefault') to get more field paths.
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
	#' @concept Curriculum
	#' @return A list of AcademicStandardGradeRangeDefaults
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	listAcademicStandardGradeRangeDefaults <- function(searchConditionsList = NULL, AcademicStandardGradeRangeDefaultID = F, AcademicStandardSubjectDefaultID = F, Code = F, CreatedTime = F, Description = F, Key = F, ModifiedTime = F, SkywardHash = F, SkywardID = F, UserIDCreatedBy = F, UserIDModifiedBy = F, fieldPaths = NULL, searchConditionsGroupType = "And", searchSortFieldNamesList = NULL, searchSortFieldNamesDescendingList = NULL, entityId = 1, query = NULL, page = 1, pageSize = 100000, flatten = T, returnResponse = F){

		params <- as.list(environment())

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		listSkyObjects(module = "Curriculum", objectName = "AcademicStandardGradeRangeDefault", searchFields = searchFields %>% append(fieldPaths), page = page, pageSize = pageSize, SearchConditionsList = searchConditionsList, SearchConditionsGroupType = searchConditionsGroupType, SearchSortFieldNamesList = searchSortFieldNamesList, SearchSortFieldNamesDescendingList = searchSortFieldNamesDescendingList, entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' Get an AcademicStandardGradeRangeDefault
	#'
	#' This function returns a dataframe or json object of an AcademicStandardGradeRangeDefault
	#' @param AcademicStandardGradeRangeDefaultID The ID of the AcademicStandardGradeRangeDefault to return.
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given AcademicStandardGradeRangeDefault. Defaults to FALSE for all return fields which, for convenience, returns all fields for the AcademicStandardGradeRangeDefault.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object listed as text. Run \code{\link{getSchemaForObjects}}('AcademicStandardGradeRangeDefault') to get more field paths.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param query Queries to be added to the request. Ex. list(SchoolYearID = 1). Defaults to NULL.
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Curriculum
	#' @return A dataframe or of AcademicStandardGradeRangeDefault
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	getAcademicStandardGradeRangeDefault <- function(AcademicStandardGradeRangeDefaultID, AcademicStandardSubjectDefaultID = F, Code = F, CreatedTime = F, Description = F, Key = F, ModifiedTime = F, SkywardHash = F, SkywardID = F, UserIDCreatedBy = F, UserIDModifiedBy = F, fieldPaths = NULL, entityId = 1, query = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment()) %>% purrr::keep(names(.) != "AcademicStandardGradeRangeDefaultID")

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		getSkyObject(module = "Curriculum", objectName = "AcademicStandardGradeRangeDefault", objectId = AcademicStandardGradeRangeDefaultID, searchFields = searchFields, entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' Delete an AcademicStandardGradeRangeDefault
	#'
	#' This function deletes an AcademicStandardGradeRangeDefault
	#' @param AcademicStandardGradeRangeDefaultID The ID of the AcademicStandardGradeRangeDefault to delete
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param query Queries to be added to the request. Ex. list(SchoolYearID = 1). Defaults to NULL.
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Curriculum
	#' @return The AcademicStandardGradeRangeDefaultID of the deleted AcademicStandardGradeRangeDefault.
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	deleteAcademicStandardGradeRangeDefault <- function(AcademicStandardGradeRangeDefaultID, ignoreWarnings = F, entityId = 1, query = NULL, flatten = T, returnResponse = F){

		deleteSkyObject(module = "Curriculum", objectName = "AcademicStandardGradeRangeDefault", objectId = AcademicStandardGradeRangeDefaultID, ignoreWarnings = ignoreWarnings, entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' Create an AcademicStandardGradeRangeDefault
	#'
	#' This function creates an AcademicStandardGradeRangeDefault
	#' @param fieldNames The field values to give the created AcademicStandardGradeRangeDefault. Each defaults to NULL.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param query Queries to be added to the request. Ex. list(SchoolYearID = 1). Defaults to NULL.
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Curriculum
	#' @return A newly created AcademicStandardGradeRangeDefault
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	createAcademicStandardGradeRangeDefault <- function(AcademicStandardSubjectDefaultID = NULL, Code = NULL, Description = NULL, Key = NULL, entityId = 1, query = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment())

		body <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper()) %>% purrr::compact()

		createSkyObject(module = "Curriculum", objectName = "AcademicStandardGradeRangeDefault", body = list(DataObject = body), searchFields = append("AcademicStandardGradeRangeDefaultID", body %>% names()), entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' Modify an AcademicStandardGradeRangeDefault
	#'
	#' This function modifies an AcademicStandardGradeRangeDefault
	#' @param fieldNames The field values to give the modified AcademicStandardGradeRangeDefault. Each defaults to NULL.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param query Queries to be added to the request. Ex. list(SchoolYearID = 1). Defaults to NULL.
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Curriculum
	#' @return The modified AcademicStandardGradeRangeDefault
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	modifyAcademicStandardGradeRangeDefault <- function(AcademicStandardGradeRangeDefaultID, AcademicStandardSubjectDefaultID = NULL, Code = NULL, Description = NULL, Key = NULL, entityId = 1, query = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment())

		body <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper()) %>% purrr::compact()

		modifySkyObject(module = "Curriculum", objectName = "AcademicStandardGradeRangeDefault", objectId = AcademicStandardGradeRangeDefaultID, body = list(DataObject = body), searchFields = append("AcademicStandardGradeRangeDefaultID", body %>% names()), entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' List AcademicStandardSetDefaults
	#'
	#' This function returns a dataframe or json object of AcademicStandardSetDefaults
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given AcademicStandardSetDefaults. Defaults to FALSE for all return fields which, for convenience, returns all fields for the AcademicStandardSetDefaults.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object listed as text. Run \code{\link{getSchemaForObjects}}('AcademicStandardSetDefault') to get more field paths.
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
	#' @concept Curriculum
	#' @return A list of AcademicStandardSetDefaults
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	listAcademicStandardSetDefaults <- function(searchConditionsList = NULL, AcademicStandardSetDefaultID = F, Code = F, CreatedTime = F, Description = F, Key = F, ModifiedTime = F, SkywardHash = F, SkywardID = F, UserIDCreatedBy = F, UserIDModifiedBy = F, fieldPaths = NULL, searchConditionsGroupType = "And", searchSortFieldNamesList = NULL, searchSortFieldNamesDescendingList = NULL, entityId = 1, query = NULL, page = 1, pageSize = 100000, flatten = T, returnResponse = F){

		params <- as.list(environment())

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		listSkyObjects(module = "Curriculum", objectName = "AcademicStandardSetDefault", searchFields = searchFields %>% append(fieldPaths), page = page, pageSize = pageSize, SearchConditionsList = searchConditionsList, SearchConditionsGroupType = searchConditionsGroupType, SearchSortFieldNamesList = searchSortFieldNamesList, SearchSortFieldNamesDescendingList = searchSortFieldNamesDescendingList, entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' Get an AcademicStandardSetDefault
	#'
	#' This function returns a dataframe or json object of an AcademicStandardSetDefault
	#' @param AcademicStandardSetDefaultID The ID of the AcademicStandardSetDefault to return.
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given AcademicStandardSetDefault. Defaults to FALSE for all return fields which, for convenience, returns all fields for the AcademicStandardSetDefault.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object listed as text. Run \code{\link{getSchemaForObjects}}('AcademicStandardSetDefault') to get more field paths.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param query Queries to be added to the request. Ex. list(SchoolYearID = 1). Defaults to NULL.
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Curriculum
	#' @return A dataframe or of AcademicStandardSetDefault
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	getAcademicStandardSetDefault <- function(AcademicStandardSetDefaultID, Code = F, CreatedTime = F, Description = F, Key = F, ModifiedTime = F, SkywardHash = F, SkywardID = F, UserIDCreatedBy = F, UserIDModifiedBy = F, fieldPaths = NULL, entityId = 1, query = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment()) %>% purrr::keep(names(.) != "AcademicStandardSetDefaultID")

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		getSkyObject(module = "Curriculum", objectName = "AcademicStandardSetDefault", objectId = AcademicStandardSetDefaultID, searchFields = searchFields, entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' Delete an AcademicStandardSetDefault
	#'
	#' This function deletes an AcademicStandardSetDefault
	#' @param AcademicStandardSetDefaultID The ID of the AcademicStandardSetDefault to delete
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param query Queries to be added to the request. Ex. list(SchoolYearID = 1). Defaults to NULL.
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Curriculum
	#' @return The AcademicStandardSetDefaultID of the deleted AcademicStandardSetDefault.
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	deleteAcademicStandardSetDefault <- function(AcademicStandardSetDefaultID, ignoreWarnings = F, entityId = 1, query = NULL, flatten = T, returnResponse = F){

		deleteSkyObject(module = "Curriculum", objectName = "AcademicStandardSetDefault", objectId = AcademicStandardSetDefaultID, ignoreWarnings = ignoreWarnings, entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' Create an AcademicStandardSetDefault
	#'
	#' This function creates an AcademicStandardSetDefault
	#' @param fieldNames The field values to give the created AcademicStandardSetDefault. Each defaults to NULL.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param query Queries to be added to the request. Ex. list(SchoolYearID = 1). Defaults to NULL.
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Curriculum
	#' @return A newly created AcademicStandardSetDefault
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	createAcademicStandardSetDefault <- function(Code = NULL, Description = NULL, Key = NULL, entityId = 1, query = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment())

		body <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper()) %>% purrr::compact()

		createSkyObject(module = "Curriculum", objectName = "AcademicStandardSetDefault", body = list(DataObject = body), searchFields = append("AcademicStandardSetDefaultID", body %>% names()), entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' Modify an AcademicStandardSetDefault
	#'
	#' This function modifies an AcademicStandardSetDefault
	#' @param fieldNames The field values to give the modified AcademicStandardSetDefault. Each defaults to NULL.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param query Queries to be added to the request. Ex. list(SchoolYearID = 1). Defaults to NULL.
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Curriculum
	#' @return The modified AcademicStandardSetDefault
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	modifyAcademicStandardSetDefault <- function(AcademicStandardSetDefaultID, Code = NULL, Description = NULL, Key = NULL, entityId = 1, query = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment())

		body <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper()) %>% purrr::compact()

		modifySkyObject(module = "Curriculum", objectName = "AcademicStandardSetDefault", objectId = AcademicStandardSetDefaultID, body = list(DataObject = body), searchFields = append("AcademicStandardSetDefaultID", body %>% names()), entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' List AcademicStandardSubjectDefaults
	#'
	#' This function returns a dataframe or json object of AcademicStandardSubjectDefaults
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given AcademicStandardSubjectDefaults. Defaults to FALSE for all return fields which, for convenience, returns all fields for the AcademicStandardSubjectDefaults.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object listed as text. Run \code{\link{getSchemaForObjects}}('AcademicStandardSubjectDefault') to get more field paths.
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
	#' @concept Curriculum
	#' @return A list of AcademicStandardSubjectDefaults
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	listAcademicStandardSubjectDefaults <- function(searchConditionsList = NULL, AcademicStandardSetDefaultID = F, AcademicStandardSubjectDefaultID = F, Code = F, CreatedTime = F, Description = F, Key = F, ModifiedTime = F, SkywardHash = F, SkywardID = F, UserIDCreatedBy = F, UserIDModifiedBy = F, fieldPaths = NULL, searchConditionsGroupType = "And", searchSortFieldNamesList = NULL, searchSortFieldNamesDescendingList = NULL, entityId = 1, query = NULL, page = 1, pageSize = 100000, flatten = T, returnResponse = F){

		params <- as.list(environment())

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		listSkyObjects(module = "Curriculum", objectName = "AcademicStandardSubjectDefault", searchFields = searchFields %>% append(fieldPaths), page = page, pageSize = pageSize, SearchConditionsList = searchConditionsList, SearchConditionsGroupType = searchConditionsGroupType, SearchSortFieldNamesList = searchSortFieldNamesList, SearchSortFieldNamesDescendingList = searchSortFieldNamesDescendingList, entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' Get an AcademicStandardSubjectDefault
	#'
	#' This function returns a dataframe or json object of an AcademicStandardSubjectDefault
	#' @param AcademicStandardSubjectDefaultID The ID of the AcademicStandardSubjectDefault to return.
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given AcademicStandardSubjectDefault. Defaults to FALSE for all return fields which, for convenience, returns all fields for the AcademicStandardSubjectDefault.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object listed as text. Run \code{\link{getSchemaForObjects}}('AcademicStandardSubjectDefault') to get more field paths.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param query Queries to be added to the request. Ex. list(SchoolYearID = 1). Defaults to NULL.
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Curriculum
	#' @return A dataframe or of AcademicStandardSubjectDefault
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	getAcademicStandardSubjectDefault <- function(AcademicStandardSubjectDefaultID, AcademicStandardSetDefaultID = F, Code = F, CreatedTime = F, Description = F, Key = F, ModifiedTime = F, SkywardHash = F, SkywardID = F, UserIDCreatedBy = F, UserIDModifiedBy = F, fieldPaths = NULL, entityId = 1, query = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment()) %>% purrr::keep(names(.) != "AcademicStandardSubjectDefaultID")

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		getSkyObject(module = "Curriculum", objectName = "AcademicStandardSubjectDefault", objectId = AcademicStandardSubjectDefaultID, searchFields = searchFields, entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' Delete an AcademicStandardSubjectDefault
	#'
	#' This function deletes an AcademicStandardSubjectDefault
	#' @param AcademicStandardSubjectDefaultID The ID of the AcademicStandardSubjectDefault to delete
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param query Queries to be added to the request. Ex. list(SchoolYearID = 1). Defaults to NULL.
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Curriculum
	#' @return The AcademicStandardSubjectDefaultID of the deleted AcademicStandardSubjectDefault.
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	deleteAcademicStandardSubjectDefault <- function(AcademicStandardSubjectDefaultID, ignoreWarnings = F, entityId = 1, query = NULL, flatten = T, returnResponse = F){

		deleteSkyObject(module = "Curriculum", objectName = "AcademicStandardSubjectDefault", objectId = AcademicStandardSubjectDefaultID, ignoreWarnings = ignoreWarnings, entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' Create an AcademicStandardSubjectDefault
	#'
	#' This function creates an AcademicStandardSubjectDefault
	#' @param fieldNames The field values to give the created AcademicStandardSubjectDefault. Each defaults to NULL.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param query Queries to be added to the request. Ex. list(SchoolYearID = 1). Defaults to NULL.
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Curriculum
	#' @return A newly created AcademicStandardSubjectDefault
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	createAcademicStandardSubjectDefault <- function(AcademicStandardSetDefaultID = NULL, Code = NULL, Description = NULL, Key = NULL, entityId = 1, query = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment())

		body <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper()) %>% purrr::compact()

		createSkyObject(module = "Curriculum", objectName = "AcademicStandardSubjectDefault", body = list(DataObject = body), searchFields = append("AcademicStandardSubjectDefaultID", body %>% names()), entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' Modify an AcademicStandardSubjectDefault
	#'
	#' This function modifies an AcademicStandardSubjectDefault
	#' @param fieldNames The field values to give the modified AcademicStandardSubjectDefault. Each defaults to NULL.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param query Queries to be added to the request. Ex. list(SchoolYearID = 1). Defaults to NULL.
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Curriculum
	#' @return The modified AcademicStandardSubjectDefault
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	modifyAcademicStandardSubjectDefault <- function(AcademicStandardSubjectDefaultID, AcademicStandardSetDefaultID = NULL, Code = NULL, Description = NULL, Key = NULL, entityId = 1, query = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment())

		body <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper()) %>% purrr::compact()

		modifySkyObject(module = "Curriculum", objectName = "AcademicStandardSubjectDefault", objectId = AcademicStandardSubjectDefaultID, body = list(DataObject = body), searchFields = append("AcademicStandardSubjectDefaultID", body %>% names()), entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' List CurriculumYearEdFiCourseLevelCharacteristics
	#'
	#' This function returns a dataframe or json object of CurriculumYearEdFiCourseLevelCharacteristics
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given CurriculumYearEdFiCourseLevelCharacteristics. Defaults to FALSE for all return fields which, for convenience, returns all fields for the CurriculumYearEdFiCourseLevelCharacteristics.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object listed as text. Run \code{\link{getSchemaForObjects}}('CurriculumYearEdFiCourseLevelCharacteristic') to get more field paths.
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
	#' @concept Curriculum
	#' @return A list of CurriculumYearEdFiCourseLevelCharacteristics
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	listCurriculumYearEdFiCourseLevelCharacteristics <- function(searchConditionsList = NULL, CreatedTime = F, CurriculumYearEdFiCourseLevelCharacteristicID = F, CurriculumYearEdFiCourseLevelCharacteristicIDClonedFrom = F, CurriculumYearID = F, EdFiCourseLevelCharacteristicID = F, ModifiedTime = F, UserIDCreatedBy = F, UserIDModifiedBy = F, fieldPaths = NULL, searchConditionsGroupType = "And", searchSortFieldNamesList = NULL, searchSortFieldNamesDescendingList = NULL, entityId = 1, query = NULL, page = 1, pageSize = 100000, flatten = T, returnResponse = F){

		params <- as.list(environment())

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		listSkyObjects(module = "Curriculum", objectName = "CurriculumYearEdFiCourseLevelCharacteristic", searchFields = searchFields %>% append(fieldPaths), page = page, pageSize = pageSize, SearchConditionsList = searchConditionsList, SearchConditionsGroupType = searchConditionsGroupType, SearchSortFieldNamesList = searchSortFieldNamesList, SearchSortFieldNamesDescendingList = searchSortFieldNamesDescendingList, entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' Get a CurriculumYearEdFiCourseLevelCharacteristic
	#'
	#' This function returns a dataframe or json object of a CurriculumYearEdFiCourseLevelCharacteristic
	#' @param CurriculumYearEdFiCourseLevelCharacteristicID The ID of the CurriculumYearEdFiCourseLevelCharacteristic to return.
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given CurriculumYearEdFiCourseLevelCharacteristic. Defaults to FALSE for all return fields which, for convenience, returns all fields for the CurriculumYearEdFiCourseLevelCharacteristic.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object listed as text. Run \code{\link{getSchemaForObjects}}('CurriculumYearEdFiCourseLevelCharacteristic') to get more field paths.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param query Queries to be added to the request. Ex. list(SchoolYearID = 1). Defaults to NULL.
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Curriculum
	#' @return A dataframe or of CurriculumYearEdFiCourseLevelCharacteristic
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	getCurriculumYearEdFiCourseLevelCharacteristic <- function(CurriculumYearEdFiCourseLevelCharacteristicID, CreatedTime = F, CurriculumYearEdFiCourseLevelCharacteristicIDClonedFrom = F, CurriculumYearID = F, EdFiCourseLevelCharacteristicID = F, ModifiedTime = F, UserIDCreatedBy = F, UserIDModifiedBy = F, fieldPaths = NULL, entityId = 1, query = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment()) %>% purrr::keep(names(.) != "CurriculumYearEdFiCourseLevelCharacteristicID")

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		getSkyObject(module = "Curriculum", objectName = "CurriculumYearEdFiCourseLevelCharacteristic", objectId = CurriculumYearEdFiCourseLevelCharacteristicID, searchFields = searchFields, entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' Delete a CurriculumYearEdFiCourseLevelCharacteristic
	#'
	#' This function deletes a CurriculumYearEdFiCourseLevelCharacteristic
	#' @param CurriculumYearEdFiCourseLevelCharacteristicID The ID of the CurriculumYearEdFiCourseLevelCharacteristic to delete
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param query Queries to be added to the request. Ex. list(SchoolYearID = 1). Defaults to NULL.
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Curriculum
	#' @return The CurriculumYearEdFiCourseLevelCharacteristicID of the deleted CurriculumYearEdFiCourseLevelCharacteristic.
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	deleteCurriculumYearEdFiCourseLevelCharacteristic <- function(CurriculumYearEdFiCourseLevelCharacteristicID, ignoreWarnings = F, entityId = 1, query = NULL, flatten = T, returnResponse = F){

		deleteSkyObject(module = "Curriculum", objectName = "CurriculumYearEdFiCourseLevelCharacteristic", objectId = CurriculumYearEdFiCourseLevelCharacteristicID, ignoreWarnings = ignoreWarnings, entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' Create a CurriculumYearEdFiCourseLevelCharacteristic
	#'
	#' This function creates a CurriculumYearEdFiCourseLevelCharacteristic
	#' @param fieldNames The field values to give the created CurriculumYearEdFiCourseLevelCharacteristic. Each defaults to NULL.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param query Queries to be added to the request. Ex. list(SchoolYearID = 1). Defaults to NULL.
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Curriculum
	#' @return A newly created CurriculumYearEdFiCourseLevelCharacteristic
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	createCurriculumYearEdFiCourseLevelCharacteristic <- function(CurriculumYearEdFiCourseLevelCharacteristicIDClonedFrom = NULL, CurriculumYearID = NULL, EdFiCourseLevelCharacteristicID = NULL, entityId = 1, query = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment())

		body <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper()) %>% purrr::compact()

		createSkyObject(module = "Curriculum", objectName = "CurriculumYearEdFiCourseLevelCharacteristic", body = list(DataObject = body), searchFields = append("CurriculumYearEdFiCourseLevelCharacteristicID", body %>% names()), entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' Modify a CurriculumYearEdFiCourseLevelCharacteristic
	#'
	#' This function modifies a CurriculumYearEdFiCourseLevelCharacteristic
	#' @param fieldNames The field values to give the modified CurriculumYearEdFiCourseLevelCharacteristic. Each defaults to NULL.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param query Queries to be added to the request. Ex. list(SchoolYearID = 1). Defaults to NULL.
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Curriculum
	#' @return The modified CurriculumYearEdFiCourseLevelCharacteristic
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	modifyCurriculumYearEdFiCourseLevelCharacteristic <- function(CurriculumYearEdFiCourseLevelCharacteristicID, CurriculumYearEdFiCourseLevelCharacteristicIDClonedFrom = NULL, CurriculumYearID = NULL, EdFiCourseLevelCharacteristicID = NULL, entityId = 1, query = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment())

		body <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper()) %>% purrr::compact()

		modifySkyObject(module = "Curriculum", objectName = "CurriculumYearEdFiCourseLevelCharacteristic", objectId = CurriculumYearEdFiCourseLevelCharacteristicID, body = list(DataObject = body), searchFields = append("CurriculumYearEdFiCourseLevelCharacteristicID", body %>% names()), entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' List TempGradeRangeCopyResultMessages
	#'
	#' This function returns a dataframe or json object of TempGradeRangeCopyResultMessages
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given TempGradeRangeCopyResultMessages. Defaults to FALSE for all return fields which, for convenience, returns all fields for the TempGradeRangeCopyResultMessages.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object listed as text. Run \code{\link{getSchemaForObjects}}('TempGradeRangeCopyResultMessage') to get more field paths.
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
	#' @concept Curriculum
	#' @return A list of TempGradeRangeCopyResultMessages
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	listTempGradeRangeCopyResultMessages <- function(searchConditionsList = NULL, CreatedTime = F, ErrorID = F, ErrorMessage = F, ModifiedTime = F, TempGradeRangeCopyResultID = F, TempGradeRangeCopyResultMessageID = F, UserIDCreatedBy = F, UserIDModifiedBy = F, fieldPaths = NULL, searchConditionsGroupType = "And", searchSortFieldNamesList = NULL, searchSortFieldNamesDescendingList = NULL, entityId = 1, query = NULL, page = 1, pageSize = 100000, flatten = T, returnResponse = F){

		params <- as.list(environment())

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		listSkyObjects(module = "Curriculum", objectName = "TempGradeRangeCopyResultMessage", searchFields = searchFields %>% append(fieldPaths), page = page, pageSize = pageSize, SearchConditionsList = searchConditionsList, SearchConditionsGroupType = searchConditionsGroupType, SearchSortFieldNamesList = searchSortFieldNamesList, SearchSortFieldNamesDescendingList = searchSortFieldNamesDescendingList, entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' Get a TempGradeRangeCopyResultMessage
	#'
	#' This function returns a dataframe or json object of a TempGradeRangeCopyResultMessage
	#' @param TempGradeRangeCopyResultMessageID The ID of the TempGradeRangeCopyResultMessage to return.
	#' @param fieldNames A TRUE or FALSE value determining whether or not to return the field for the given TempGradeRangeCopyResultMessage. Defaults to FALSE for all return fields which, for convenience, returns all fields for the TempGradeRangeCopyResultMessage.
	#' @param fieldPaths Fields from other objects with 'Many to One' or 'One to One' relationships to the given object listed as text. Run \code{\link{getSchemaForObjects}}('TempGradeRangeCopyResultMessage') to get more field paths.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param query Queries to be added to the request. Ex. list(SchoolYearID = 1). Defaults to NULL.
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Curriculum
	#' @return A dataframe or of TempGradeRangeCopyResultMessage
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	getTempGradeRangeCopyResultMessage <- function(TempGradeRangeCopyResultMessageID, CreatedTime = F, ErrorID = F, ErrorMessage = F, ModifiedTime = F, TempGradeRangeCopyResultID = F, UserIDCreatedBy = F, UserIDModifiedBy = F, fieldPaths = NULL, entityId = 1, query = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment()) %>% purrr::keep(names(.) != "TempGradeRangeCopyResultMessageID")

		searchFields <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper())

		ifelse(!any(searchFields %>% unlist()), searchFields <- searchFields %>% names(), searchFields <- searchFields %>% purrr::keep(~.x) %>% names())

		getSkyObject(module = "Curriculum", objectName = "TempGradeRangeCopyResultMessage", objectId = TempGradeRangeCopyResultMessageID, searchFields = searchFields, entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' Delete a TempGradeRangeCopyResultMessage
	#'
	#' This function deletes a TempGradeRangeCopyResultMessage
	#' @param TempGradeRangeCopyResultMessageID The ID of the TempGradeRangeCopyResultMessage to delete
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param query Queries to be added to the request. Ex. list(SchoolYearID = 1). Defaults to NULL.
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Curriculum
	#' @return The TempGradeRangeCopyResultMessageID of the deleted TempGradeRangeCopyResultMessage.
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	deleteTempGradeRangeCopyResultMessage <- function(TempGradeRangeCopyResultMessageID, ignoreWarnings = F, entityId = 1, query = NULL, flatten = T, returnResponse = F){

		deleteSkyObject(module = "Curriculum", objectName = "TempGradeRangeCopyResultMessage", objectId = TempGradeRangeCopyResultMessageID, ignoreWarnings = ignoreWarnings, entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' Create a TempGradeRangeCopyResultMessage
	#'
	#' This function creates a TempGradeRangeCopyResultMessage
	#' @param fieldNames The field values to give the created TempGradeRangeCopyResultMessage. Each defaults to NULL.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param query Queries to be added to the request. Ex. list(SchoolYearID = 1). Defaults to NULL.
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Curriculum
	#' @return A newly created TempGradeRangeCopyResultMessage
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	createTempGradeRangeCopyResultMessage <- function(ErrorID = NULL, ErrorMessage = NULL, TempGradeRangeCopyResultID = NULL, entityId = 1, query = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment())

		body <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper()) %>% purrr::compact()

		createSkyObject(module = "Curriculum", objectName = "TempGradeRangeCopyResultMessage", body = list(DataObject = body), searchFields = append("TempGradeRangeCopyResultMessageID", body %>% names()), entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}

	#' Modify a TempGradeRangeCopyResultMessage
	#'
	#' This function modifies a TempGradeRangeCopyResultMessage
	#' @param fieldNames The field values to give the modified TempGradeRangeCopyResultMessage. Each defaults to NULL.
	#' @param entityId The id of the entity (school). Run \code{\link{listEntities}} for a list of entities. Defaults to 1 (district).
	#' @param query Queries to be added to the request. Ex. list(SchoolYearID = 1). Defaults to NULL.
	#' @param flatten Whether to flatten result into a dataframe or return the json object. Default is TRUE (flatten to dataframe).
	#' @param returnResponse Whether to return the server response instead of the results. Useful for debugging. Default is FALSE.
	#' @concept Curriculum
	#' @return The modified TempGradeRangeCopyResultMessage
	#' \href{https://help.skyward.com/}{Skyward's Knowledge Hub}
	#' @export
	modifyTempGradeRangeCopyResultMessage <- function(TempGradeRangeCopyResultMessageID, ErrorID = NULL, ErrorMessage = NULL, TempGradeRangeCopyResultID = NULL, entityId = 1, query = NULL, flatten = T, returnResponse = F){

		params <- as.list(environment())

		body <- params %>% purrr::keep(names(params) %>% stringr::str_sub(1,1) == names(params) %>% stringr::str_sub(1,1) %>% stringr::str_to_upper()) %>% purrr::compact()

		modifySkyObject(module = "Curriculum", objectName = "TempGradeRangeCopyResultMessage", objectId = TempGradeRangeCopyResultMessageID, body = list(DataObject = body), searchFields = append("TempGradeRangeCopyResultMessageID", body %>% names()), entityId = entityId, query = query, flatten = flatten, returnResponse = returnResponse)
	}
